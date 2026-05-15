#####################################################
#
#  043. Cokriging 4: cokriging ordinario
#
#  Descripción:
#  Realiza cokriging ordinario usando el modelo LMC
#  ajustado previamente en el script 042. El script carga
#  la variable objetivo y la variable auxiliar, genera una
#  rejilla regular de predicción, aplica cokriging y exporta
#  los resultados principales como GeoTIFF para QGIS.
#
#  Requisito previo:
#  - Ejecutar antes el script 042, que debe guardar:
#    modelos/lmc_model.rds
#
#  Entrada:
#  - datos/brutos/cloruros/LabSamples.csv
#  - datos/procesados/Field_conduc_combi.csv
#  - modelos/lmc_model.rds
#
#  Salida:
#  - salidas/kriging/dataset_objetivo_auxiliar_CK_pred_objetivo.tif
#  - salidas/kriging/dataset_objetivo_auxiliar_CK_var_objetivo.tif
#  - salidas/kriging/dataset_objetivo_auxiliar_CK_pred_auxiliar.tif
#  - salidas/kriging/dataset_objetivo_auxiliar_CK_var_auxiliar.tif
#
#####################################################

# ==== Cargar librerías ====

if (!requireNamespace("gstat", quietly = TRUE)) install.packages("gstat")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("mapview", quietly = TRUE)) install.packages("mapview")

library(gstat)
library(sf)
library(terra)
library(mapview)

# ==== Archivos de datos ====

ruta_objetivo <- "datos/brutos/cloruros/LabSamples.csv"
ruta_auxiliar <- "datos/procesados/Field_conduc_combi.csv"
ruta_modelo_lmc <- "modelos/lmc_model.rds"

if (!file.exists(ruta_objetivo)) {
  stop(paste("No se encuentra el fichero:", ruta_objetivo))
}

if (!file.exists(ruta_auxiliar)) {
  stop(paste("No se encuentra el fichero:", ruta_auxiliar))
}

if (!file.exists(ruta_modelo_lmc)) {
  stop(paste("No se encuentra el modelo LMC:", ruta_modelo_lmc,
             "\nEjecuta antes el script 042."))
}

z1 <- read.csv(ruta_objetivo, sep = ",")
z2 <- read.csv(ruta_auxiliar, sep = ",")

cat("Columnas disponibles en el dataset objetivo:\n")
print(colnames(z1))

cat("\nColumnas disponibles en el dataset auxiliar:\n")
print(colnames(z2))



# ==== Parámetros a editar ====

nombre_x <- "x"
nombre_y <- "y"

nombre_objetivo <- "ClmgKg"
nombre_auxiliar <- "FmSm"

epsg_datos <- 32613

# Deben coincidir con los ids usados en el script 042
id_objetivo <- "Cl"
id_auxiliar <- "FmSm"

# Rejilla de predicción
n_filas <- 100
n_columnas <- 100
margen_bbox <- 0.05

# Opciones
mostrar_mapa <- TRUE
mostrar_rejilla <- FALSE

# Guardado
guardar_rasters <- TRUE

# ==== Comprobaciones ====

vars_obj <- c(nombre_x, nombre_y, nombre_objetivo)
vars_aux <- c(nombre_x, nombre_y, nombre_auxiliar)

faltan_obj <- vars_obj[!vars_obj %in% colnames(z1)]
faltan_aux <- vars_aux[!vars_aux %in% colnames(z2)]

if (length(faltan_obj) > 0) {
  stop(paste("En el dataset objetivo faltan:", paste(faltan_obj, collapse = ", ")))
}

if (length(faltan_aux) > 0) {
  stop(paste("En el dataset auxiliar faltan:", paste(faltan_aux, collapse = ", ")))
}

vars_no_num_obj <- vars_obj[!sapply(z1[vars_obj], is.numeric)]
vars_no_num_aux <- vars_aux[!sapply(z2[vars_aux], is.numeric)]

if (length(vars_no_num_obj) > 0) {
  stop(paste(
    "Estas columnas del dataset objetivo deben ser numéricas:",
    paste(vars_no_num_obj, collapse = ", ")
  ))
}

if (length(vars_no_num_aux) > 0) {
  stop(paste(
    "Estas columnas del dataset auxiliar deben ser numéricas:",
    paste(vars_no_num_aux, collapse = ", ")
  ))
}

if (n_filas <= 1 || n_columnas <= 1) {
  stop("n_filas y n_columnas deben ser mayores que 1.")
}

z1_plot <- z1[
  complete.cases(z1[, vars_obj]),
  vars_obj,
  drop = FALSE
]

z2_plot <- z2[
  complete.cases(z2[, vars_aux]),
  vars_aux,
  drop = FALSE
]

if (nrow(z1_plot) < 2) {
  stop("El dataset objetivo necesita al menos dos observaciones válidas.")
}

if (nrow(z2_plot) < 2) {
  stop("El dataset auxiliar necesita al menos dos observaciones válidas.")
}

cat("\nVariable objetivo:", nombre_objetivo, "\n")
cat("Variable auxiliar:", nombre_auxiliar, "\n")
cat("EPSG:", epsg_datos, "\n")
cat("Observaciones objetivo:", nrow(z1_plot), "\n")
cat("Observaciones auxiliar:", nrow(z2_plot), "\n")

# ==== Conversión a sf ====

z1_sf <- st_as_sf(
  z1_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)

z2_sf <- st_as_sf(
  z2_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)

# ==== Mapa rápido ====

if (mostrar_mapa) {
  print(
    mapview(z1_sf, zcol = nombre_objetivo, map.types = "CartoDB.Voyager") +
      mapview(z2_sf, zcol = nombre_auxiliar)
  )
}

# ==== Cargar modelo LMC ====

crosscorrection <- readRDS(ruta_modelo_lmc)

cat("\n=====================================\n")
cat(" Modelo LMC cargado\n")
cat("=====================================\n")
print(crosscorrection)

# Comprobar que los nombres de salida esperados existen en el modelo
cat("\nVariables del modelo LMC:\n")
print(names(crosscorrection$data))

# ==== Rejilla de predicción ====

bbox <- st_bbox(
  st_union(
    st_geometry(z1_sf),
    st_geometry(z2_sf)
  )
)

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
  crs = st_crs(z1_sf),
  remove = FALSE
)

cat("\nRejilla de predicción creada:\n")
cat("Filas:", n_filas, "\n")
cat("Columnas:", n_columnas, "\n")
cat("Número de celdas:", nrow(grid_sf), "\n")

if (mostrar_rejilla) {
  print(
    mapview(z2_sf, zcol = nombre_auxiliar, map.types = "CartoDB.Voyager") +
      mapview(grid_sf, color = "red")
  )
}

# ==== Cokriging ordinario ====

prediction <- predict(
  crosscorrection,
  newdata = grid_sf
)

cat("\n=====================================\n")
cat(" Cokriging completado\n")
cat("=====================================\n")
cat("Campos generados:\n")
print(names(prediction))

# ==== Preparar resultados ====

prediction_df <- data.frame(
  X = grid$x,
  Y = grid$y
)

campo_pred_obj <- paste0(id_objetivo, ".pred")
campo_var_obj  <- paste0(id_objetivo, ".var")
campo_pred_aux <- paste0(id_auxiliar, ".pred")
campo_var_aux  <- paste0(id_auxiliar, ".var")

campos_necesarios <- c(
  campo_pred_obj,
  campo_var_obj,
  campo_pred_aux,
  campo_var_aux
)

faltan_pred <- campos_necesarios[!campos_necesarios %in% names(prediction)]

if (length(faltan_pred) > 0) {
  stop(
    paste(
      "No se encuentran estos campos en la predicción:",
      paste(faltan_pred, collapse = ", "),
      "\nRevisa que id_objetivo e id_auxiliar coincidan con los usados en el script 042."
    )
  )
}

prediction_df$pred_objetivo <- prediction[[campo_pred_obj]]
prediction_df$var_objetivo  <- prediction[[campo_var_obj]]
prediction_df$pred_auxiliar <- prediction[[campo_pred_aux]]
prediction_df$var_auxiliar  <- prediction[[campo_var_aux]]

# ==== Crear rasters ====

r_template <- rast(
  xmin = xmin,
  xmax = xmax,
  ymin = ymin,
  ymax = ymax,
  nrows = n_filas,
  ncols = n_columnas,
  crs = st_crs(grid_sf)$wkt
)

prediction_df_ord <- prediction_df[
  order(prediction_df$Y, decreasing = TRUE, prediction_df$X),
]

r_pred_obj <- r_template
r_var_obj  <- r_template
r_pred_aux <- r_template
r_var_aux  <- r_template

values(r_pred_obj) <- prediction_df_ord$pred_objetivo
values(r_var_obj)  <- prediction_df_ord$var_objetivo
values(r_pred_aux) <- prediction_df_ord$pred_auxiliar
values(r_var_aux)  <- prediction_df_ord$var_auxiliar

names(r_pred_obj) <- paste0(id_objetivo, "_CK_pred")
names(r_var_obj)  <- paste0(id_objetivo, "_CK_var")
names(r_pred_aux) <- paste0(id_auxiliar, "_CK_pred")
names(r_var_aux)  <- paste0(id_auxiliar, "_CK_var")

# ==== Guardar rasters ====

if (guardar_rasters) {
  
  nombre_dataset_obj <- tools::file_path_sans_ext(basename(ruta_objetivo))
  nombre_dataset_aux <- tools::file_path_sans_ext(basename(ruta_auxiliar))
  
  dir_rasters <- "salidas/kriging"
  
  if (!dir.exists(dir_rasters)) {
    dir.create(dir_rasters, recursive = TRUE)
  }
  
  nombre_pred_obj <- file.path(
    dir_rasters,
    paste0(nombre_dataset_obj, "_", nombre_dataset_aux, "_CK_pred_objetivo.tif")
  )
  
  nombre_var_obj <- file.path(
    dir_rasters,
    paste0(nombre_dataset_obj, "_", nombre_dataset_aux, "_CK_var_objetivo.tif")
  )
  
  nombre_pred_aux <- file.path(
    dir_rasters,
    paste0(nombre_dataset_obj, "_", nombre_dataset_aux, "_CK_pred_auxiliar.tif")
  )
  
  nombre_var_aux <- file.path(
    dir_rasters,
    paste0(nombre_dataset_obj, "_", nombre_dataset_aux, "_CK_var_auxiliar.tif")
  )
  
  writeRaster(r_pred_obj, nombre_pred_obj, overwrite = TRUE)
  writeRaster(r_var_obj, nombre_var_obj, overwrite = TRUE)
  writeRaster(r_pred_aux, nombre_pred_aux, overwrite = TRUE)
  writeRaster(r_var_aux, nombre_var_aux, overwrite = TRUE)
  
  cat("\n=====================================\n")
  cat(" Rasters guardados\n")
  cat("=====================================\n")
  cat("Predicción objetivo:\n", nombre_pred_obj, "\n")
  cat("Existe:", file.exists(nombre_pred_obj), "\n")
  cat("Varianza objetivo:\n", nombre_var_obj, "\n")
  cat("Existe:", file.exists(nombre_var_obj), "\n")
  cat("Predicción auxiliar:\n", nombre_pred_aux, "\n")
  cat("Existe:", file.exists(nombre_pred_aux), "\n")
  cat("Varianza auxiliar:\n", nombre_var_aux, "\n")
  cat("Existe:", file.exists(nombre_var_aux), "\n\n")
}
