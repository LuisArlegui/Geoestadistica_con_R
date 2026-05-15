####################################################
#
#  020. Scatter Plot + 2 variables categóricas
#
#  Descripción:
#  Genera un gráfico de dispersión entre dos variables
#  numéricas, representando simultáneamente dos variables
#  categóricas mediante forma y color, útil para explorar
#  agrupaciones y diferencias entre categorías.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/dataset_varX_varY_cat1_cat2_scatter_categorias.png o .pdf
#
####################################################



# Cargar librerías ====

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")

library(ggplot2)
library(viridis)



# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable_x    <- "Columna1"   # <-- EDITAR AQUÍ
nombre_variable_y    <- "Columna2"   # <-- EDITAR AQUÍ
nombre_variable_cat1 <- "Columna3"   # <-- EDITAR AQUÍ
nombre_variable_cat2 <- "Columna4"   # <-- EDITAR AQUÍ

titulo_grafico    <- "Título del gráfico"      # <-- EDITAR AQUÍ
subtitulo_grafico <- "Subtítulo del gráfico"   # <-- EDITAR AQUÍ
etiqueta_x        <- "Columna1"                # <-- EDITAR AQUÍ
etiqueta_y        <- "Columna2"                # <-- EDITAR AQUÍ

titulo_leyenda_forma <- "Leyenda 1"   # <-- EDITAR AQUÍ
titulo_leyenda_color <- "Leyenda 2"   # <-- EDITAR AQUÍ

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

if (!nombre_variable_cat1 %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_cat1), "no existe en el dataset"))
}

if (!nombre_variable_cat2 %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_cat2), "no existe en el dataset"))
}

if (!is.numeric(data[[nombre_variable_x]])) {
  stop(paste("La variable", shQuote(nombre_variable_x), "debe ser numérica"))
}

if (!is.numeric(data[[nombre_variable_y]])) {
  stop(paste("La variable", shQuote(nombre_variable_y), "debe ser numérica"))
}

# Filtrar filas completas
data_plot <- data[
  complete.cases(
    data[, c(
      nombre_variable_x,
      nombre_variable_y,
      nombre_variable_cat1,
      nombre_variable_cat2
    )]
  ),
]

if (nrow(data_plot) == 0) {
  stop("No quedan observaciones válidas tras eliminar NA en las variables seleccionadas.")
}

cat("\nVariable X seleccionada:", nombre_variable_x, "\n")
cat("Variable Y seleccionada:", nombre_variable_y, "\n")
cat("Variable categórica 1 (forma):", nombre_variable_cat1, "\n")
cat("Variable categórica 2 (color):", nombre_variable_cat2, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")



# Gráfico ====

grafico <- ggplot(
  data_plot,
  aes_string(x = nombre_variable_x, y = nombre_variable_y)
) +
  geom_point(
    aes_string(
      shape = paste0("factor(", nombre_variable_cat1, ")"),
      colour = paste0("factor(", nombre_variable_cat2, ")")
    ),
    size = tam_puntos,
    alpha = alpha_puntos
  ) +
  labs(
    x = etiqueta_x,
    y = etiqueta_y,
    title = titulo_grafico,
    subtitle = subtitulo_grafico,
    shape = titulo_leyenda_forma,
    colour = titulo_leyenda_color
  ) +
  scale_color_viridis_d(option = "viridis", name = titulo_leyenda_color) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black"),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", color = "black"),
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
      nombre_variable_x, "_",
      nombre_variable_y, "_",
      nombre_variable_cat1, "_",
      nombre_variable_cat2,
      "_scatter_2categorias.",
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