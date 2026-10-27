#####################################################
#
#  013. Scatter Plot ggplot y regresión lineal
#
#  Descripción:
#  Genera un gráfico de dispersión entre dos variables
#  numéricas, ajusta un modelo de regresión lineal simple
#  y representa la recta de ajuste junto con la ecuación
#  y el coeficiente de determinación R².
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - resumen del modelo en consola
#  - salidas/figuras/dataset_varX_varY_regresion_lineal.png
#
#####################################################



# Cargar librerías ====

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)




# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

# Variables a representar
nombre_variable_x <- "variable1"   # <-- EDITAR AQUÍ
nombre_variable_y <- "variable2"   # <-- EDITAR AQUÍ

# Etiquetas y títulos
etiqueta_x <- "etiqueta del eje x"      # <-- EDITAR AQUÍ
etiqueta_y <- "etiqueta del eje y"      # <-- EDITAR AQUÍ
titulo_grafico <- "título del plot"     # <-- EDITAR AQUÍ
subtitulo_grafico <- "subtítulo"        # <-- EDITAR AQUÍ

# Estética de puntos y recta
color_puntos <- "steelblue"   # <-- EDITAR AQUÍ
alpha_puntos <- 0.7      # <-- EDITAR AQUÍ
tam_puntos   <- 2        # <-- EDITAR AQUÍ

color_recta <- "red"     # <-- EDITAR AQUÍ
grosor_recta <- 1        # <-- EDITAR AQUÍ

# Guardado automático de figura
guardar_figura <- TRUE   # <-- TRUE / FALSE

# Dimensiones de exportación
ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300



# Comprobaciones ====

if (!nombre_variable_x %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_x), "no existe en el dataset"))
}

if (!nombre_variable_y %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_y), "no existe en el dataset"))
}

if (!is.numeric(data[[nombre_variable_x]])) {
  stop(paste("La variable", shQuote(nombre_variable_x), "debe ser numérica"))
}

if (!is.numeric(data[[nombre_variable_y]])) {
  stop(paste("La variable", shQuote(nombre_variable_y), "debe ser numérica"))
}

# Filtrar filas completas para ambas variables
data_plot <- data[complete.cases(data[, c(nombre_variable_x, nombre_variable_y)]), ]

if (nrow(data_plot) == 0) {
  stop("No quedan observaciones válidas tras eliminar NA en las variables seleccionadas.")
}

cat("\nVariable X seleccionada:", nombre_variable_x, "\n")
cat("Variable Y seleccionada:", nombre_variable_y, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")



# Ajuste lineal ====

formula_lm <- as.formula(
  paste(nombre_variable_y, "~", nombre_variable_x)
)

modeloLM <- lm(formula_lm, data = data_plot)

cat("\n=====================================\n")
cat(" Resumen del modelo lineal\n")
cat("=====================================\n")
print(summary(modeloLM))

# Coeficientes
coeficientes <- coef(modeloLM)
intercepto <- unname(coeficientes[1])
pendiente  <- unname(coeficientes[2])

# Coeficientes de determinación
R2 <- summary(modeloLM)$r.squared
R2_adj <- summary(modeloLM)$adj.r.squared

cat("\nCoeficientes del modelo:\n")
cat("Intercepto:", round(intercepto, 4), "\n")
cat("Pendiente :", round(pendiente, 4), "\n")
cat("R²        :", round(R2, 4), "\n")
cat("R² ajust. :", round(R2_adj, 4), "\n")



# Texto de ecuación ====

eq_text <- paste0(
  "y = ", round(pendiente, 3), " x + ", round(intercepto, 3),
  "\nR² = ", round(R2, 3)
)

# Posición de la anotación
x_annot <- min(data_plot[[nombre_variable_x]], na.rm = TRUE)
y_annot <- max(data_plot[[nombre_variable_y]], na.rm = TRUE)



# Gráfico ====

grafico <- ggplot(
  data_plot,
  aes_string(x = nombre_variable_x, y = nombre_variable_y)
) +
  geom_point(
    colour = color_puntos,
    alpha = alpha_puntos,
    size = tam_puntos
  ) +
  geom_abline(
    intercept = intercepto,
    slope = pendiente,
    colour = color_recta,
    linewidth = grosor_recta
  ) +
  annotate(
    "text",
    x = x_annot,
    y = y_annot,
    label = eq_text,
    hjust = 0,
    vjust = 1,
    size = 4,
    fontface = "bold"
  ) +
  labs(
    x = etiqueta_x,
    y = etiqueta_y,
    title = titulo_grafico,
    subtitle = subtitulo_grafico
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    axis.title = element_text(face = "bold")
  )

print(grafico)



# Guardar figura ====

if (guardar_figura) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0("014_",
      nombre_dataset, "_",
      nombre_variable_x, "_",
      nombre_variable_y,
      "_regresion_lineal.png"
    )
  )
  
  ggsave(
    filename = nombre_figura,
    plot = grafico,
    width = ancho_figura,
    height = alto_figura,
    dpi = dpi_figura
  )
  
 cat(" Figura guardada\n")
 cat("Archivo:\n", nombre_figura, "\n\n")
}