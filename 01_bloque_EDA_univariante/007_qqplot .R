#####################################################
#
#  007. QQ Plot
#
#  Descripción:
#  Genera un gráfico QQ para comparar la distribución
#  de una variable con la distribución normal, útil
#  para evaluar normalidad y posibles transformaciones.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/dataset_variable_qqplot.png o .pdf
#
#####################################################


# Cargar librerías ====


if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)


# Archivo de datos ====

ruta_datos <- "datos/brutos/meuse.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta

# Mostrar nombres de columnas
cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable <- "zinc"   # <-- EDITAR AQUÍ

etiqueta_x <- "Cuantiles teóricos (Normal)"   # <-- EDITAR AQUÍ si se desea
etiqueta_y <- "Cuantiles observados"          # <-- EDITAR AQUÍ si se desea
titulo_grafico <- "QQ Plot"                   # <-- EDITAR AQUÍ
subtitulo_grafico <- "Comparación con distribución normal"   # <-- EDITAR AQUÍ

# Estética
color_puntos <- "blue"        # <-- EDITAR AQUÍ
tam_puntos <- 2               # <-- EDITAR AQUÍ
alpha_puntos <- 0.5           # <-- EDITAR AQUÍ

color_linea <- "black"        # <-- EDITAR AQUÍ
grosor_linea <- 0.6           # <-- EDITAR AQUÍ

# Guardado
guardar_figura <- TRUE        # <-- TRUE / FALSE
formato_figura <- "pdf"       # <-- "png" o "pdf"

ancho_figura <- 7
alto_figura  <- 6
dpi_figura   <- 300           # solo PNG


# Comprobaciones ====


if (!nombre_variable %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable), "no existe en el fichero de datos."))
}

if (!is.numeric(data[[nombre_variable]])) {
  stop(paste("La variable", shQuote(nombre_variable), "debe ser numérica."))
}

# Eliminar NA
data_plot <- data[!is.na(data[[nombre_variable]]), , drop = FALSE]

if (nrow(data_plot) == 0) {
  stop("No quedan datos válidos tras eliminar NA.")
}

cat("\nVariable seleccionada:", nombre_variable, "\n")
cat("Número de observaciones:", nrow(data_plot), "\n")


# QQ Plot ====


grafico <- ggplot(data_plot, aes_string(sample = nombre_variable)) +
  stat_qq(
    color = color_puntos,
    size = tam_puntos,
    alpha = alpha_puntos
  ) +
  stat_qq_line(
    color = color_linea,
    linewidth = grosor_linea
  ) +
  labs(
    x = etiqueta_x,
    y = etiqueta_y,
    title = titulo_grafico,
    subtitle = subtitulo_grafico
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
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
    paste0(
      nombre_dataset, "_",
      nombre_variable,
      "_qqplot.",
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