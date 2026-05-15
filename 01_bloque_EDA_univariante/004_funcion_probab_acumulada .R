#####################################################
#
#  004. Función de probabilidad acumulada (ECDF)
#
#  Descripción:
#  Representa la función de distribución acumulada
#  empírica (ECDF) de una variable numérica.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/dataset_variable_ecdf.png o .pdf
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

# Mostrar nombres de columnas para facilitar la elección
cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable <- "columna"   # <-- EDITAR AQUÍ

etiqueta_x <- "etiqueta del eje x"     # <-- EDITAR AQUÍ
etiqueta_y <- "F(x)"                   # <-- EDITAR AQUÍ
titulo_grafico <- "título del plot"    # <-- EDITAR AQUÍ
subtitulo_grafico <- "subtítulo"       # <-- EDITAR AQUÍ

# Estética
color_ecdf <- "blue"           # <-- EDITAR AQUÍ
grosor_linea <- 0.7            # <-- EDITAR AQUÍ

# Guardado
guardar_figura <- TRUE         # <-- TRUE / FALSE
formato_figura <- "pdf"        # <-- "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300            # solo PNG



# Comprobaciones ====

if (!nombre_variable %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable), "no existe en el fichero de datos."))
}

if (!is.numeric(data[[nombre_variable]])) {
  stop(paste("La variable", shQuote(nombre_variable), "debe ser numérica."))
}

# Eliminar NA para evitar problemas
data <- data[!is.na(data[[nombre_variable]]), ]

if (nrow(data) == 0) {
  stop("No quedan datos válidos tras eliminar NA.")
}

cat("\nVariable seleccionada:", nombre_variable, "\n")
cat("Número de observaciones:", nrow(data), "\n")



# Gráfico ECDF ====

grafico <- ggplot(data, aes_string(x = nombre_variable)) +
  stat_ecdf(
    color = color_ecdf,
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
      "_ecdf.",
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