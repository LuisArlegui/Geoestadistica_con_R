#####################################################
#
#  048. Simulación gaussiana 4: probabilidad de superar un umbral
#
#  Descripción:
#  Calcula, celda a celda, la proporción de simulaciones
#  que superan un umbral definido. El resultado se interpreta
#  como la probabilidad espacial de excedencia.
#
#  Requisito previo:
#  - Ejecutar antes el script 046, que guarda:
#    modelos/meuse_zinc_simulaciones_originales.rds
#
#  Entrada:
#  - modelos/meuse_zinc_simulaciones_originales.rds
#
#  Salida:
#  - salidas/simulaciones/meuse_zinc_probabilidad_superar_umbral.tif
#  - salidas/simulaciones/meuse_zinc_numero_superaciones_umbral.tif
#  - salidas/figuras/meuse_zinc_probabilidad_superar_umbral.pdf o .png
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

# ==== Parámetros a editar ====

ruta_sim_originales <- "modelos/meuse_zinc_simulaciones_originales.rds"

nombre_dataset <- "meuse"
nombre_variable <- "zinc"

epsg_datos <- 28992

n_filas <- 100
n_columnas <- 100

umbral <- 500                    # ppm

guardar_rasters <- TRUE
guardar_figura <- TRUE
formato_figura <- "pdf"          # "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300              # solo PNG

# ==== Comprobaciones ====

if (!file.exists(ruta_sim_originales)) {
  stop(paste(
    "No se encuentra el fichero:",
    ruta_sim_originales,
    "\nEjecuta antes el script 046."
  ))
}

if (umbral <= 0) {
  warning("El umbral es <= 0. Revisa si tiene sentido para esta variable.")
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
cat("Umbral:", umbral, "\n")

# ==== Preparar coordenadas ====

if (!all(c("X", "Y") %in% colnames(simulaciones_df))) {
  coords <- st_coordinates(simulaciones_sf)
  simulaciones_df$X <- coords[, 1]
  simulaciones_df$Y <- coords[, 2]
}

# ==== Calcular excedencias ====

simulation_binary <- simulaciones_df[, columnas_sim, drop = FALSE] >= umbral

simulaciones_df$numero_superaciones <- rowSums(
  simulation_binary,
  na.rm = TRUE
)

simulaciones_df$probabilidad_superacion <- simulaciones_df$numero_superaciones /
  length(columnas_sim)

cat("\nResumen del número de superaciones:\n")
print(summary(simulaciones_df$numero_superaciones))

cat("\nResumen de la probabilidad de superación:\n")
print(summary(simulaciones_df$probabilidad_superacion))

# ==== Crear rasters ====

bbox <- st_bbox(simulaciones_sf)

r_template <- rast(
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

r_numero <- r_template
r_prob   <- r_template

terra::values(r_numero) <- simulaciones_df_ord$numero_superaciones
terra::values(r_prob)   <- simulaciones_df_ord$probabilidad_superacion

names(r_numero) <- paste0(nombre_variable, "_numero_superaciones_", umbral)
names(r_prob)   <- paste0(nombre_variable, "_prob_superar_", umbral)

# ==== Visualización ====

grafico_prob <- ggplot(
  simulaciones_df,
  aes(x = X, y = Y, fill = probabilidad_superacion)
) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 1)) +
  coord_equal() +
  labs(
    title = "Probabilidad de superar el umbral",
    subtitle = paste(nombre_variable, ">=", umbral, "ppm"),
    x = "X",
    y = "Y",
    fill = "Probabilidad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )

print(grafico_prob)

# ==== Guardar rasters ====

if (guardar_rasters) {
  
  dir_simulaciones <- "salidas/simulaciones"
  
  if (!dir.exists(dir_simulaciones)) {
    dir.create(dir_simulaciones, recursive = TRUE)
  }
  
  nombre_raster_numero <- file.path(
    dir_simulaciones,
    paste0(
      nombre_dataset, "_",
      nombre_variable,
      "_numero_superaciones_umbral_",
      umbral,
      ".tif"
    )
  )
  
  nombre_raster_prob <- file.path(
    dir_simulaciones,
    paste0(
      nombre_dataset, "_",
      nombre_variable,
      "_probabilidad_superar_umbral_",
      umbral,
      ".tif"
    )
  )
  
  writeRaster(
    r_numero,
    nombre_raster_numero,
    overwrite = TRUE
  )
  
  writeRaster(
    r_prob,
    nombre_raster_prob,
    overwrite = TRUE
  )
  
  cat("\n=====================================\n")
  cat(" Rasters guardados\n")
  cat("=====================================\n")
  cat("Número de superaciones:\n", nombre_raster_numero, "\n")
  cat("Existe:", file.exists(nombre_raster_numero), "\n")
  cat("Probabilidad:\n", nombre_raster_prob, "\n")
  cat("Existe:", file.exists(nombre_raster_prob), "\n\n")
}

# ==== Guardar figura ====

if (guardar_figura) {
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0(
      nombre_dataset, "_",
      nombre_variable,
      "_probabilidad_superar_umbral_",
      umbral,
      ".",
      formato_figura
    )
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico_prob,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico_prob,
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

