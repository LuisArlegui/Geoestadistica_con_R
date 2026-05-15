#####################################################
#
#  036. Kriging ordinario (OK)
#
#  Descripción:
#  Realiza una interpolación mediante kriging ordinario
#  a partir de datos puntuales georreferenciados. Calcula
#  el variograma experimental, ajusta un modelo variográfico,
#  genera una rejilla de predicción y exporta predicciones
#  y varianza de kriging como GeoTIFF para QGIS.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - mapa interactivo de puntos en el visor
#  - salidas/rasters/dataset_variable_OK_prediccion.tif
#  - salidas/rasters/dataset_variable_OK_varianza.tif
#  - (opcional) salidas/figuras/dataset_variable_OK_prediccion.png o .pdf
#  - (opcional) salidas/figuras/dataset_variable_OK_varianza.png o .pdf
#
#####################################################

# ==== Cargar librerías ====

if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("gstat", quietly = TRUE)) install.packages("gstat")
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("mapview", quietly = TRUE)) install.packages("mapview")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")

library(sf)
library(gstat)
library(terra)
library(mapview)
library(ggplot2)
library(viridis)

# ==== Archivo de datos ====

ruta_datos <- "datos/procesados/meuse_transf.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))

# ==== Parámetros a editar ====

nombre_x <- "x"                 # <-- EDITAR AQUÍ
nombre_y <- "y"                 # <-- EDITAR AQUÍ
nombre_variable <- "log10_zinc"      # <-- EDITAR AQUÍ

epsg_datos <- 28992             # <-- EDITAR AQUÍ

# Variograma experimental
cutoff_variograma <- 1000
lag_variograma <- NULL          # NULL = gstat decide; o valor numérico

# Modelo inicial de variograma
pSill <- 0.58
modelo_variograma <- "Sph"      # "Exp", "Sph", "Gau", "Bes", "Mat", etc.
rango <- 900
nugget <- 0.05

# Rejilla de predicción
n_filas <- 100
n_columnas <- 100
margen_bbox <- 0.15

# Opciones
mostrar_mapa <- TRUE
ajustar_variograma <- TRUE

# Guardado
guardar_rasters <- TRUE
guardar_figuras <- TRUE
formato_figura <- "pdf"         # "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300             # solo PNG

# ==== Comprobaciones ====

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

if (!is.null(lag_variograma) && lag_variograma <= 0) {
  stop("lag_variograma debe ser NULL o mayor que cero.")
}

data_plot <- data[
  complete.cases(data[, vars]),
  vars,
  drop = FALSE
]

if (nrow(data_plot) < 3) {
  stop("Se necesitan al menos tres observaciones válidas para aplicar kriging.")
}

cat("\nCoordenada X:", nombre_x, "\n")
cat("Coordenada Y:", nombre_y, "\n")
cat("Variable:", nombre_variable, "\n")
cat("EPSG:", epsg_datos, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")

# ==== Conversión a sf ====

data_sf <- st_as_sf(
  data_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)

# ==== Mapa rápido ====

if (mostrar_mapa) {
  mapviewOptions(
    basemaps = "CartoDB.Voyager",
    basemaps.color.shuffle = FALSE
  )
  
  print(
    mapview(
      data_sf,
      zcol = nombre_variable,
      map.types = "CartoDB.Voyager"
    )
  )
}

# ==== Variograma experimental ====

formula_variograma <- as.formula(
  paste(nombre_variable, "~ 1")
)

if (is.null(lag_variograma)) {
  variograma_exp <- variogram(
    formula_variograma,
    data = data_sf,
    cutoff = cutoff_variograma
  )
} else {
  variograma_exp <- variogram(
    formula_variograma,
    data = data_sf,
    cutoff = cutoff_variograma,
    width = lag_variograma
  )
}

if (nrow(variograma_exp) == 0) {
  stop("El variograma experimental no contiene clases. Revisa cutoff y lag.")
}

cat("\nVariograma experimental calculado:\n")
print(variograma_exp)

# ==== Modelo variográfico ====

modelo_inicial <- vgm(
  psill = pSill,
  model = modelo_variograma,
  range = rango,
  nugget = nugget
)

cat("\n=====================================\n")
cat(" Modelo inicial\n")
cat("=====================================\n")
print(modelo_inicial)

if (ajustar_variograma) {
  modelo_final <- fit.variogram(
    object = variograma_exp,
    model = modelo_inicial
  )
} else {
  modelo_final <- modelo_inicial
}

cat("\n=====================================\n")
cat(" Modelo usado en kriging\n")
cat("=====================================\n")
print(modelo_final)

# ==== Rejilla de predicción ====

bbox <- st_bbox(data_sf)

ancho_bbox <- bbox["xmax"] - bbox["xmin"]
alto_bbox  <- bbox["ymax"] - bbox["ymin"]

xmin <- bbox["xmin"] - ancho_bbox * margen_bbox
xmax <- bbox["xmax"] + ancho_bbox * margen_bbox
ymin <- bbox["ymin"] - alto_bbox * margen_bbox
ymax <- bbox["ymax"] + alto_bbox * margen_bbox

x_range <- seq(xmin, xmax, length.out = n_columnas)
y_range <- seq(ymin, ymax, length.out = n_filas)

grid <- expand.grid(
  x = x_range,
  y = y_range
)

grid_sf <- st_as_sf(
  grid,
  coords = c("x", "y"),
  crs = st_crs(data_sf),
  remove = FALSE
)

cat("\nRejilla de predicción creada:\n")
cat("Filas:", n_filas, "\n")
cat("Columnas:", n_columnas, "\n")
cat("Número de celdas:", nrow(grid_sf), "\n")

# ==== Kriging ordinario ====

modelo_ok <- gstat(
  formula = formula_variograma,
  data = data_sf,
  model = modelo_final
)

kpred <- predict(
  modelo_ok,
  newdata = grid_sf
)

cat("\nKriging ordinario completado.\n")

# ==== Preparar resultados ====

kpred_df <- data.frame(
  X = grid$x,
  Y = grid$y,
  prediccion = kpred$var1.pred,
  varianza = kpred$var1.var
)

# ==== Crear rasters ====

r_pred <- rast(
  xmin = xmin,
  xmax = xmax,
  ymin = ymin,
  ymax = ymax,
  nrows = n_filas,
  ncols = n_columnas,
  crs = st_crs(data_sf)$wkt
)

r_var <- r_pred

# Orden necesario para que terra rellene correctamente por filas
kpred_df_ord <- kpred_df[order(kpred_df$Y, decreasing = TRUE, kpred_df$X), ]

values(r_pred) <- kpred_df_ord$prediccion
values(r_var)  <- kpred_df_ord$varianza

names(r_pred) <- paste0(nombre_variable, "_OK_pred")
names(r_var)  <- paste0(nombre_variable, "_OK_var")

# ==== Guardar rasters ====

if (guardar_rasters) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_rasters <- "salidas/kriging"
  
  if (!dir.exists(dir_rasters)) {
    dir.create(dir_rasters, recursive = TRUE)
  }
  
  nombre_raster_pred <- file.path(
    dir_rasters,
    paste0(nombre_dataset, "_", nombre_variable, "_OK_prediccion.tif")
  )
  
  nombre_raster_var <- file.path(
    dir_rasters,
    paste0(nombre_dataset, "_", nombre_variable, "_OK_varianza.tif")
  )
  
  writeRaster(r_pred, nombre_raster_pred, overwrite = TRUE)
  writeRaster(r_var, nombre_raster_var, overwrite = TRUE)
  
  cat("\n=====================================\n")
  cat(" Rasters guardados\n")
  cat("=====================================\n")
  cat("Predicción:\n", nombre_raster_pred, "\n")
  cat("Varianza:\n", nombre_raster_var, "\n\n")
}

# ==== Visualización predicción ====

grafico_pred <- ggplot(kpred_df, aes(x = X, y = Y, fill = prediccion)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Predicciones por kriging ordinario",
    x = nombre_x,
    y = nombre_y,
    fill = "Predicción"
  ) +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

print(grafico_pred)

# ==== Visualización varianza ====

grafico_var <- ggplot(kpred_df, aes(x = X, y = Y, fill = varianza)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Varianza de kriging ordinario",
    x = nombre_x,
    y = nombre_y,
    fill = "Varianza"
  ) +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

print(grafico_var)

# ==== Guardar figuras ====

if (guardar_figuras) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura_pred <- file.path(
    dir_figuras,
    paste0(nombre_dataset, "_", nombre_variable, "_OK_prediccion.", formato_figura)
  )
  
  nombre_figura_var <- file.path(
    dir_figuras,
    paste0(nombre_dataset, "_", nombre_variable, "_OK_varianza.", formato_figura)
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura_pred,
      plot = grafico_pred,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
    ggsave(
      filename = nombre_figura_var,
      plot = grafico_var,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura_pred,
      plot = grafico_pred,
      width = ancho_figura,
      height = alto_figura,
      device = cairo_pdf
    )
    
    ggsave(
      filename = nombre_figura_var,
      plot = grafico_var,
      width = ancho_figura,
      height = alto_figura,
      device = cairo_pdf
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("\n=====================================\n")
  cat(" Figuras guardadas\n")
  cat("=====================================\n")
  cat("Predicción:\n", nombre_figura_pred, "\n")
  cat("Varianza:\n", nombre_figura_var, "\n\n")
}
