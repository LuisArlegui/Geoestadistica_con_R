#####################################################
#
#  031. Variograma nube con boxplots
#
#  Descripción:
#  Calcula el variograma nube omnidireccional de una
#  variable regionalizada y resume la dispersión de las
#  semivarianzas mediante boxplots agrupados por clases
#  de distancia. Es útil para explorar la variabilidad
#  de los pares y orientar la elección de lag y cutoff.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/dataset_variable_variograma_nube_boxplots.png o .pdf
#  - (opcional) salidas/tablas/dataset_variable_variograma_nube_boxplots.csv
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

ruta_datos <- "datos/brutos/wolfcamp.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

nombre_x <- "x"                 # <-- EDITAR AQUÍ
nombre_y <- "y"                 # <-- EDITAR AQUÍ
nombre_variable <- "gw"      # <-- EDITAR AQUÍ

epsg_datos <- 32613             # <-- EDITAR AQUÍ

cutoff_variograma <- 450000       # distancia máxima
lag_cajas <- 12000               # anchura de clase para los boxplots

Label_x <- "Distancia"
Label_y <- "Semivarianza"
Title_graph <- "Variograma nube omnidireccional con boxplots"
Subtitle_graph <- "Datos de ejemplo"

# Estética
color_box <- "skyblue"
color_borde <- "grey30"
alpha_box <- 0.6

# Guardado
guardar_figura <- TRUE
formato_figura <- "pdf"         # "png" o "pdf"
guardar_tabla <- TRUE

ancho_figura <- 9
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

if (cutoff_variograma <= 0) {
  stop("cutoff_variograma debe ser mayor que cero.")
}

if (lag_cajas <= 0) {
  stop("lag_cajas debe ser mayor que cero.")
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
cat("Lag de cajas:", lag_cajas, "\n")



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

if (nrow(cloud_variogram) == 0) {
  stop("El variograma nube no contiene pares. Revisa cutoff_variograma.")
}

cat("\nNúmero de pares calculados:", nrow(cloud_variogram), "\n")



# Crear clases de distancia ====

breaks_dist <- seq(
  from = 0,
  to = ceiling(max(cloud_variogram$dist, na.rm = TRUE) / lag_cajas) * lag_cajas,
  by = lag_cajas
)

cloud_variogram$clase_distancia <- cut(
  cloud_variogram$dist,
  breaks = breaks_dist,
  include.lowest = TRUE,
  right = FALSE
)



# Gráfico ====

grafico <- ggplot(cloud_variogram, aes(x = clase_distancia, y = gamma)) +
  geom_boxplot(
    fill = color_box,
    color = color_borde,
    alpha = alpha_box,
    outlier.alpha = 0.4
  ) +
  labs(
    x = Label_x,
    y = Label_y,
    title = Title_graph,
    subtitle = Subtitle_graph
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text.x = element_text(
      colour = "black",
      size = 8,
      face = "bold",
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(
      colour = "black",
      size = 10,
      face = "bold"
    ),
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
      "_variograma_nube_boxplots.",
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

