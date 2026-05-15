#####################################################
#
#  024. Ajuste polinómico 2D
#
#  Descripción:
#  Genera un gráfico de dispersión entre dos variables
#  numéricas y superpone ajustes de regresión lineal,
#  polinómica de segundo orden y polinómica de tercer
#  orden, útil para comparar tendencias y curvaturas.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/dataset_varX_varY_ajuste_polinomico.png o .pdf
#
#####################################################



# Cargar librerías ====

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)



# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))




# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable_x <- "columna1"   # <-- EDITAR AQUÍ
nombre_variable_y <- "columna2"   # <-- EDITAR AQUÍ

Titulo_graf <- "Título del gráfico"       # <-- EDITAR AQUÍ
Subtitulo_graf <- "Subtítulo del gráfico" # <-- EDITAR AQUÍ
Etiqueta_x <- "Columna1"                  # <-- EDITAR AQUÍ
Etiqueta_y <- "Columna2"                  # <-- EDITAR AQUÍ

# Mostrar u ocultar cada ajuste
mostrar_lineal <- TRUE         # <-- TRUE / FALSE
mostrar_segundo <- TRUE        # <-- TRUE / FALSE
mostrar_tercero <- TRUE        # <-- TRUE / FALSE

# Mostrar banda de confianza
mostrar_banda_confianza <- FALSE   # <-- TRUE / FALSE

# Guardado de figura
guardar_figura <- TRUE         # <-- TRUE / FALSE
formato_figura <- "pdf"        # <-- "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300            # solo PNG




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

# Filtrar filas completas
datos_plot <- data[
  complete.cases(data[, c(nombre_variable_x, nombre_variable_y)]),
  c(nombre_variable_x, nombre_variable_y),
  drop = FALSE
]

if (nrow(datos_plot) == 0) {
  stop("No quedan observaciones válidas tras eliminar NA en las variables seleccionadas.")
}

colnames(datos_plot) <- c("X", "Y")

cat("\nVariable X seleccionada:", nombre_variable_x, "\n")
cat("Variable Y seleccionada:", nombre_variable_y, "\n")
cat("Número de observaciones válidas:", nrow(datos_plot), "\n")



# Gráfico base ====

grafico <- ggplot(datos_plot, aes(x = X, y = Y)) +
  geom_point(colour = "blue", alpha = 0.7) +
  labs(
    x = Etiqueta_x,
    y = Etiqueta_y,
    title = Titulo_graf,
    subtitle = Subtitulo_graf,
    colour = "Tipo de regresión"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )



# Añadir ajustes polinómicos ====

if (mostrar_lineal) {
  grafico <- grafico +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = mostrar_banda_confianza,
      linewidth = 1.2,
      aes(colour = "Lineal")
    )
}

if (mostrar_segundo) {
  grafico <- grafico +
    geom_smooth(
      method = "lm",
      formula = y ~ poly(x, 2, raw = TRUE),
      se = mostrar_banda_confianza,
      linewidth = 1.2,
      aes(colour = "Segundo_orden")
    )
}

if (mostrar_tercero) {
  grafico <- grafico +
    geom_smooth(
      method = "lm",
      formula = y ~ poly(x, 3, raw = TRUE),
      se = mostrar_banda_confianza,
      linewidth = 1.2,
      aes(colour = "Tercer_orden")
    )
}

grafico <- grafico +
  scale_color_manual(
    values = c(
      Lineal = "red",
      Segundo_orden = "blue",
      Tercer_orden = "green4"
    )
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
    paste0(
      nombre_dataset, "_",
      nombre_variable_x, "_",
      nombre_variable_y,
      "_ajuste_polinomico.",
      formato_figura
    )
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico,
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
  cat("Archivo:\n", nombre_figura, "\n\n")
}