#####################################################
#
#  030. Variograma nube omnidireccional
#
#  Descripción:
#  Calcula y representa el variograma nube omnidireccional
#  de una variable regionalizada, mostrando la semivarianza
#  para todos los pares de puntos en función de la distancia.
#  Es útil como primer análisis exploratorio de la estructura
#  espacial y para detectar dispersión, outliers o tendencias.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/dataset_variable_variograma_nube.png o .pdf
#
#####################################################



# Cargar librerías ====

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gstat", quietly = TRUE)) install.packages("gstat")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(ggplot2)
library(gstat)
library(sf)



# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

nombre_x <- "columna1"                 # <-- EDITAR AQUÍ
nombre_y <- "columna2"                 # <-- EDITAR AQUÍ
nombre_variable <- "objetivo"   # <-- EDITAR AQUÍ

epsg_datos <- 12345             # <-- EDITAR AQUÍ

cutoff_variograma <- 123456     # distancia máxima; en metros si CRS proyectado, mirar proyecto GIS

Label_x <- "Distancia"
Label_y <- "Semivarianza"
Title_graph <- "Variograma nube omnidireccional"
Subtitle_graph <- "Datos de ejemplo"

# Estética
color_puntos <- "blue"
tam_puntos <- 1
alpha_puntos <- 0.6

# Guardado
guardar_figura <- TRUE
formato_figura <- "pdf"         # "png" o "pdf"
guardar_tabla <- TRUE

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300             # solo PNG



# Comprobaciones ====

vars <- c(nombre_x, nombre_y, nombre_variable)

faltan <- vars[!vars %in% colnames(data)]

if (length(faltan) > 0) {
  stop(paste("No existen estas columnas en el dataset:", paste(faltan, collapse = ", ")))
}

vars_no_num <- vars[!sapply(data[vars], is.numeric)]

if (length(vars_no_num) > 0) {
  stop(paste("Estas variables deben ser numéricas:", paste(vars_no_num, collapse = ", ")))
}

data_plot <- data[
  complete.cases(data[, vars]),
  vars,
  drop = FALSE
]

if (nrow(data_plot) < 2) {
  stop("Se necesitan al menos dos observaciones válidas para calcular el variograma nube.")
}

cat("\nCoordenada X:", nombre_x, "\n")
cat("Coordenada Y:", nombre_y, "\n")
cat("Variable:", nombre_variable, "\n")
cat("EPSG:", epsg_datos, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")
cat("Cutoff:", cutoff_variograma, "\n")



# Conversión a sf ====

data_sf <- st_as_sf(
  data_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)



# Variograma nube omnidireccional ====

formula_variograma <- as.formula(
  paste(nombre_variable, "~ 1")
)

cloud_variogram <- variogram(
  formula_variograma,
  data = data_sf,
  cutoff = cutoff_variograma,
  cloud = TRUE
)

cat("\nNúmero de pares calculados:", nrow(cloud_variogram), "\n")



# Gráfico ====

grafico <- ggplot(cloud_variogram, aes(x = dist, y = gamma)) +
  geom_point(
    colour = color_puntos,
    size = tam_puntos,
    alpha = alpha_puntos
  ) +
  labs(
    x = Label_x,
    y = Label_y,
    title = Title_graph,
    subtitle = Subtitle_graph
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
      "_variograma_nube.",
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

