#####################################################
#
#  005. Boxplot de una variable
#
#  Descripción:
#  Representa un diagrama de caja (boxplot) de una
#  variable numérica, útil para visualizar su
#  distribución, mediana y posibles outliers.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/dataset_variable_boxplot.png o .pdf
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

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable <- "zinc"   # <-- EDITAR AQUÍ

EtiquetaX <- "eje x"             # normalmente vacío en boxplot simple
EtiquetaY <- "eje y"         # <-- EDITAR AQUÍ
TituloGrafico <- "Boxplot"  # <-- EDITAR AQUÍ
SubTituloGrafico <- "vete a saber"      # <-- EDITAR AQUÍ

# Estética
color_box <- "salmon"

# Guardado
guardar_figura <- TRUE      # <-- TRUE / FALSE
formato_figura <- "pdf"     # <-- "png" o "pdf"

ancho_figura <- 6
alto_figura  <- 6
dpi_figura   <- 300         # solo PNG



# Comprobaciones ====

if (!nombre_variable %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable), "no existe en el fichero de datos."))
}

if (!is.numeric(data[[nombre_variable]])) {
  stop(paste("La variable", shQuote(nombre_variable), "debe ser numérica."))
}

# Filtrar NA
data_plot <- data[!is.na(data[[nombre_variable]]), ]

if (nrow(data_plot) == 0) {
  stop("No quedan datos válidos tras eliminar NA.")
}

cat("\nVariable seleccionada:", nombre_variable, "\n")
cat("Número de observaciones:", nrow(data_plot), "\n")



# Gráfico ====

grafico <- ggplot(data_plot, aes_string(y = nombre_variable)) +
  geom_boxplot(fill = color_box) +
  labs(
    x = EtiquetaX,
    y = EtiquetaY,
    title = TituloGrafico,
    subtitle = SubTituloGrafico
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
      "_boxplot.",
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