#####################################################
#
#  047. Simulación gaussiana 3: promedio de simulaciones
#
#  Descripción:
#  Calcula el promedio celda a celda de todas las
#  simulaciones gaussianas ya revertidas a unidades
#  originales. El resultado representa el valor medio
#  simulado de la variable objetivo.
#
#  Requisito previo:
#  - Ejecutar antes el script 046, que guarda:
#    modelos/dataset_variable_simulaciones_originales.rds
#
#  Entrada:
#  - modelos/dataset_variable_simulaciones_originales.rds
#
#  Salida:
#  - salidas/simulaciones/dataset_variable_promedio_simulaciones.tif
#  - salidas/figuras/dataset_variable_promedio_simulaciones.pdf o .png
#
#####################################################

# ==== Cargar librerías ====

if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")

library(terra)
library(sf)
library(ggplot2)
library(viridis)

# ==== Archivo de datos ====

ruta_datos <- "datos/brutos/meuse.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))

# ==== Parámetros a editar ====

nombre_x <- "x"
nombre_y <- "y"
nombre_variable <- "zinc"

epsg_datos <- 28992

n_filas <- 100
n_columnas <- 100

guardar_raster <- TRUE
guardar_figura <- TRUE
formato_figura <- "pdf"          # "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300              # solo PNG

# ==== Archivos de entrada ====
nombre_dataset <- "meuse"
nombre_variable <- "zinc"
ruta_sim_originales <- file.path(
  "modelos",
  paste0(nombre_dataset, "_", nombre_variable, "_simulaciones_originales.rds")
)

if (!file.exists(ruta_sim_originales)) {
  stop(paste(
    "No se encuentra el fichero:",
    ruta_sim_originales,
    "\nEjecuta antes el script 046."
  ))
}

# ==== Cargar simulaciones ====

simulaciones_sf <- readRDS(ruta_sim_originales)

cat("\nSimulaciones cargadas:\n")
cat(ruta_sim_originales, "\n")

simulaciones_df <- st_drop_geometry(simulaciones_sf)

columnas_sim <- grep("^sim", names(simulaciones_df), value = TRUE)

if (length(columnas_sim) == 0) {
  stop("No se han encontrado columnas de simulación con prefijo 'sim'.")
}

cat("Número de simulaciones:", length(columnas_sim), "\n")
cat("Número de celdas:", nrow(simulaciones_df), "\n")

# ==== Calcular promedio ====

simulaciones_df$promedio <- rowMeans(
  simulaciones_df[, columnas_sim, drop = FALSE],
  na.rm = TRUE
)

cat("\nResumen del promedio de simulaciones:\n")
print(summary(simulaciones_df$promedio))

# ==== Preparar coordenadas ====

if (!all(c("X", "Y") %in% colnames(simulaciones_df))) {
  coords <- st_coordinates(simulaciones_sf)
  simulaciones_df$X <- coords[, 1]
  simulaciones_df$Y <- coords[, 2]
}

# ==== Crear raster ====

bbox <- st_bbox(simulaciones_sf)

r_promedio <- rast(
  xmin = bbox["xmin"],
  xmax = bbox["xmax"],
  ymin = bbox["ymin"],
  ymax = bbox["ymax"],
  nrows = n_filas,
  ncols = n_columnas,
  crs = paste0("EPSG:", epsg_datos)
)

simulaciones_df_ord <- simulaciones_df[
  order(simulaciones_df$Y, decreasing = TRUE, simulaciones_df$X),
]

terra::values(r_promedio) <- simulaciones_df_ord$promedio

names(r_promedio) <- paste0(nombre_variable, "_promedio_simulaciones")

# ==== Visualización ====

grafico_promedio <- ggplot(
  simulaciones_df,
  aes(x = X, y = Y, fill = promedio)
) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(
    title = "Promedio de simulaciones gaussianas",
    subtitle = paste(nombre_variable, "en unidades originales"),
    x = "X",
    y = "Y",
    fill = paste(nombre_variable, "(ppm)")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )

print(grafico_promedio)

# ==== Guardar raster ====

if (guardar_raster) {
  
  dir_simulaciones <- "salidas/simulaciones"
  
  if (!dir.exists(dir_simulaciones)) {
    dir.create(dir_simulaciones, recursive = TRUE)
  }
  
  nombre_raster <- file.path(
    dir_simulaciones,
    paste0(nombre_dataset, "_", nombre_variable, "_promedio_simulaciones.tif")
  )
  
  writeRaster(
    r_promedio,
    nombre_raster,
    overwrite = TRUE
  )
  
  cat("\n=====================================\n")
  cat(" Raster guardado\n")
  cat("=====================================\n")
  cat("Promedio simulaciones:\n", nombre_raster, "\n")
  cat("Existe:", file.exists(nombre_raster), "\n\n")
}

# ==== Guardar figura ====

if (guardar_figura) {
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0(nombre_dataset, "_", nombre_variable, "_promedio_simulaciones.", formato_figura)
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico_promedio,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico_promedio,
      width = ancho_figura,
      height = alto_figura,
      device = "pdf"
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("\n=====================================\n")
  cat(" Figura guardada\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_figura, "\n\n")
}

