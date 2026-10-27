#####################################################
#
#  011. Diagnóstico de normalidad
#
#  Descripción:
#  Evalúa la normalidad de una variable numérica
#  combinando histograma + densidad, función de
#  distribución acumulada empírica (ECDF), QQ plot
#  y test de Shapiro-Wilk.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráficos en la ventana gráfica de R
#  - resultado del test en consola
#  - tabla resumen en salidas/tablas
#  - (opcional) salidas/figuras/dataset_variable_Diagnostico_Normalidad.png o .pdf
#
#####################################################

# Cargar librerías ====

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

# Archivo de datos ====

ruta_datos <- "datos/procesados/fichero_transf.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))

# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable <- "variable_transf"   # <-- EDITAR AQUÍ
numero_clases <- 13            # <-- EDITAR AQUÍ

titulo_general <- "Diagnóstico de normalidad"   # <-- EDITAR AQUÍ
subtitulo_general <- "Histograma, ECDF, QQ plot y test de Shapiro-Wilk"   # <-- EDITAR AQUÍ

# Guardado de figura
guardar_figura <- TRUE         # <-- TRUE / FALSE
formato_figura <- "pdf"        # <-- "png" o "pdf"

ancho_figura <- 10
alto_figura  <- 8
dpi_figura   <- 300            # solo PNG

# Comprobaciones ====

if (!nombre_variable %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable), "no existe en el fichero de datos."))
}

if (!is.numeric(data[[nombre_variable]])) {
  stop(paste("La variable", shQuote(nombre_variable), "debe ser numérica."))
}

# Eliminar valores ausentes y no finitos
data <- data[
  is.finite(data[[nombre_variable]]),
  ,
  drop = FALSE
]

Parametro <- data[[nombre_variable]]

# Tamaño muestral
n <- length(Parametro)

if (n < 3 || n > 5000) {
  stop("Shapiro-Wilk requiere entre 3 y 5000 observaciones válidas.")
}

# Comprobar variabilidad
if (length(unique(Parametro)) < 2) {
  stop("La variable es constante. No puede aplicarse Shapiro-Wilk.")
}

# Comprobar número de clases
if (!is.numeric(numero_clases) ||
    length(numero_clases) != 1 ||
    is.na(numero_clases) ||
    !is.finite(numero_clases) ||
    numero_clases < 1 ||
    numero_clases != as.integer(numero_clases)) {
  stop("numero_clases debe ser un número entero positivo.")
}

cat("\nVariable seleccionada:", nombre_variable, "\n")
cat("Número de observaciones válidas:", n, "\n")

Parametro <- data[[nombre_variable]]

cat("\nVariable seleccionada:", nombre_variable, "\n")
cat("Número de observaciones válidas:", length(Parametro), "\n")

# Test de Shapiro-Wilk ====

ShapiroResult <- shapiro.test(Parametro)
p_valor <- ShapiroResult$p.value
W_valor <- unname(ShapiroResult$statistic)

# Nivel de significación
alpha <- 0.05

if (p_valor < alpha) {
  conclusion <- "Se rechaza la normalidad"
} else {
  conclusion <- "No se rechaza la normalidad"
}

cat("\n=====================================\n")
cat(" Test de Shapiro-Wilk\n")
cat("=====================================\n")
cat("W:", round(W_valor, 4), "\n")
cat("p-value:", signif(p_valor, 4), "\n")
cat("Conclusión:", conclusion, "\n")


# Gráfico 1: Histograma + densidad ====


p1 <- ggplot(data, aes(x = .data[[nombre_variable]])) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = numero_clases,
    col = "black",
    fill = "orange",
    alpha = 0.25
  ) +
  geom_density(
    color = "blue",
    linewidth = 0.9,
    alpha = 0.8
  ) +
  labs(
    title = "Histograma + densidad",
    x = nombre_variable,
    y = "Densidad"
  ) +
  theme_minimal()

# Gráfico 2: ECDF ====

p2 <- ggplot(data, aes(x = .data[[nombre_variable]])) +
  stat_ecdf(
    color = "blue",
    linewidth = 0.8
  ) +
  labs(
    title = "Función acumulada empírica (ECDF)",
    x = nombre_variable,
    y = "F(x)"
  ) +
  theme_minimal()

# Gráfico 3: QQ Plot ====

p3 <- ggplot(data, aes(sample = .data[[nombre_variable]])) +
  stat_qq(
    color = "blue",
    size = 1.8,
    alpha = 0.6
  ) +
  stat_qq_line(
    color = "black",
    linewidth = 0.6
  ) +
  labs(
    title = "QQ Plot",
    x = "Cuantiles teóricos (Normal)",
    y = "Cuantiles observados"
  ) +
  theme_minimal()

# Gráfico 4: Texto diagnóstico ====

texto_diag <- paste(
  "Variable:", nombre_variable,
  "\nN:", length(Parametro),
  "\nW:", round(W_valor, 4),
  "\np-value:", signif(p_valor, 4),
  "\nConclusión:", conclusion
)

p4 <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = texto_diag, size = 5) +
  xlim(0, 1) +
  ylim(0, 1) +
  labs(title = "Resultado del test") +
  theme_void()

# Panel gráfico ====

# Construir panel sin dibujarlo
panel <- arrangeGrob(
  p1, p2, p3, p4,
  ncol = 2,
  top = titulo_general
)

# Mostrar panel
grid::grid.newpage()
grid::grid.draw(panel)

# Guardar figura ====

if (guardar_figura) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0("011_",
      nombre_dataset, "_",
      nombre_variable,
      "_Diagnostico_Normalidad.",
      formato_figura
    )
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura,
      plot = panel,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura,
      plot = panel,
      width = ancho_figura,
      height = alto_figura,
      device = cairo_pdf
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("\n=====================================\n")
  cat(" Figura guardada\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_figura, "\n")
}

# Guardar tabla resumen ====

nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))

dir_salida <- "salidas/tablas"

if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
}

tabla_diagnostico <- data.frame(
  Variable = nombre_variable,
  N = length(Parametro),
  Shapiro_W = W_valor,
  p_value = p_valor,
  Conclusion = conclusion
)

nombre_salida <- file.path(
  dir_salida,
  paste0("011_",nombre_dataset, "_", nombre_variable, "_Diagnostico_Normalidad.csv")
)

write.csv(tabla_diagnostico, nombre_salida, row.names = FALSE)

cat("\n=====================================\n")
cat(" Tabla guardada\n")
cat("=====================================\n")
cat("Archivo:\n", nombre_salida, "\n\n")
