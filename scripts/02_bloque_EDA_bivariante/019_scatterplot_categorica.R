####################################################
#
#  019. Scatter Plot + variable categórica
#
#  Descripción:
#  Genera un gráfico de dispersión entre dos variables
#  numéricas, representando los puntos según una variable
#  categórica mediante colores, útil para explorar grupos,
#  tendencias y posibles diferencias entre categorías.
#
#  Entrada:
#  - datos/procesados/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/019_dataset_varX_varY_categoria.png o .pdf
#
####################################################



# Cargar librerías ====

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")

library(ggplot2)
library(viridis)



# Archivo de datos ====

ruta_datos <- "datos/procesados/meuse_transf.csv"

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable_x   <- "log10_zinc"   # <-- EDITAR AQUÍ
nombre_variable_y   <- "log10_lead"   # <-- EDITAR AQUÍ
nombre_variable_cat <- "soil"   # <-- EDITAR AQUÍ

titulo_grafico    <- "Título del gráfico"     # <-- EDITAR AQUÍ
subtitulo_grafico <- "Subtítulo del gráfico"  # <-- EDITAR AQUÍ
etiqueta_x        <- "Columna1"               # <-- EDITAR AQUÍ
etiqueta_y        <- "Columna2"               # <-- EDITAR AQUÍ
titulo_leyenda    <- "Leyenda"                # <-- EDITAR AQUÍ

tam_puntos   <- 2.5   # <-- EDITAR AQUÍ
alpha_puntos <- 0.8   # <-- EDITAR AQUÍ

guardar_figura <- TRUE    # <-- TRUE / FALSE
formato_figura <- "pdf"   # <-- "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300       # solo para PNG



# Comprobaciones ====

if (!nombre_variable_x %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_x), "no existe en el dataset"))
}

if (!nombre_variable_y %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_y), "no existe en el dataset"))
}

if (!nombre_variable_cat %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_cat), "no existe en el dataset"))
}

if (!is.numeric(data[[nombre_variable_x]])) {
  stop(paste("La variable", shQuote(nombre_variable_x), "debe ser numérica"))
}

if (!is.numeric(data[[nombre_variable_y]])) {
  stop(paste("La variable", shQuote(nombre_variable_y), "debe ser numérica"))
}

# Filtrar filas completas en las tres variables
data_plot <- data[complete.cases(data[, c(nombre_variable_x, nombre_variable_y, nombre_variable_cat)]), ]

if (nrow(data_plot) == 0) {
  stop("No quedan observaciones válidas tras eliminar NA en las variables seleccionadas.")
}

cat("\nVariable X seleccionada:", nombre_variable_x, "\n")
cat("Variable Y seleccionada:", nombre_variable_y, "\n")
cat("Variable categórica:", nombre_variable_cat, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")




# Gráfico ====

grafico <- ggplot(
  data_plot,
  aes(
    x = .data[[nombre_variable_x]],
    y = .data[[nombre_variable_y]]
  )
) +
  geom_point(
    aes(
      color = factor(.data[[nombre_variable_cat]])
    ),
    size = tam_puntos,
    alpha = alpha_puntos
  ) +
  labs(
    x = etiqueta_x,
    y = etiqueta_y,
    title = titulo_grafico,
    subtitle = subtitulo_grafico,
    color = titulo_leyenda
  ) +
  scale_color_viridis_d(
    option = "viridis",
    name = titulo_leyenda
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0.5,
      color = "black"
    ),
    axis.text = element_text(
      colour = "black",
      size = 10,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      face = "italic",
      color = "black"
    ),
    axis.title = element_text(
      face = "bold"
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
    paste0("019_",
      nombre_dataset, "_",
      nombre_variable_x, "_",
      nombre_variable_y, "_",
      nombre_variable_cat,
      "_scatter_categorias.",
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
  
  cat(" Figura guardada\n")
  cat("Archivo:\n", nombre_figura, "\n\n")
}
