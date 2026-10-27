#####################################################
#
#  043. Cokriging 4: cokriging ordinario
#
#  Descripción:
#  Realiza cokriging ordinario usando el modelo LMC
#  ajustado previamente en el script 042. El script carga
#  la variable objetivo y la variable auxiliar, genera una
#  rejilla regular de predicción a partir de una plantilla
#  raster creada con terra, aplica cokriging y exporta los
#  resultados principales como GeoTIFF para QGIS.
#
#  Requisito previo:
#  - Ejecutar antes el script 042, que debe guardar:
#    modelos/042_dataset_objetivo_auxiliar_modelo_LMC.rds
#
#  Entrada:
#  - datos/brutos/cloruros/LabSamples.csv
#  - datos/procesados/Field_conduc_combi.csv
#  - modelos/042_dataset_objetivo_auxiliar_modelo_LMC.rds
#
#  Salida:
#  - salidas/kriging/043_dataset_objetivo_auxiliar_CK_pred_objetivo.tif
#  - salidas/kriging/043_dataset_objetivo_auxiliar_CK_var_objetivo.tif
#  - salidas/kriging/043_dataset_objetivo_auxiliar_CK_pred_auxiliar.tif
#  - salidas/kriging/043_dataset_objetivo_auxiliar_CK_var_auxiliar.tif
#
#####################################################


# ==== Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("gstat", "sf", "terra", "mapview")

instalar_si_falta <- function(pkgs) {
  nuevos <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(nuevos) > 0) {
    message("Instalando paquetes faltantes: ", paste(nuevos, collapse = ", "))
    install.packages(nuevos, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

instalar_si_falta(paquetes_requeridos)


# ==== Archivos de datos ====

ruta_objetivo <- "datos/brutos/cloruros/LabSamples.csv"     # <-- EDITAR AQUÍ
ruta_auxiliar <- "datos/procesados/040_dataset_combinado.csv"  # <-- EDITAR AQUÍ

if (!file.exists(ruta_objetivo)) {
  stop(paste("No se encuentra el fichero:", ruta_objetivo))
}

if (!file.exists(ruta_auxiliar)) {
  stop(paste("No se encuentra el fichero:", ruta_auxiliar))
}


# ==== Nombres de los datasets ====

nombre_dataset_obj <- tools::file_path_sans_ext(basename(ruta_objetivo))
nombre_dataset_aux <- tools::file_path_sans_ext(basename(ruta_auxiliar))


# ==== Ruta dinámica del modelo LMC ====

ruta_modelo_lmc <- file.path(
  "modelos",
  paste0(
    "042_",
    nombre_dataset_obj, "_",
    nombre_dataset_aux,
    "_modelo_LMC.rds"
  )
)

if (!file.exists(ruta_modelo_lmc)) {
  stop(
    paste(
      "No se encuentra el modelo LMC:",
      ruta_modelo_lmc,
      "\nEjecuta antes el script 042."
    )
  )
}


# ==== Leer datos ====

z1 <- read.csv(ruta_objetivo, sep = ",")
z2 <- read.csv(ruta_auxiliar, sep = ",")

cat("Columnas disponibles en el dataset objetivo:\n")
print(colnames(z1))

cat("\nColumnas disponibles en el dataset auxiliar:\n")
print(colnames(z2))

cat("\nModelo LMC que se utilizará:\n")
cat(ruta_modelo_lmc, "\n")


# ==== Parámetros a editar ====

nombre_x <- "x"
nombre_y <- "y"

nombre_objetivo <- "ClmgKg"
nombre_auxiliar <- "FmSm"

epsg_datos <- 32613                        # <-- EDITAR AQUÍ: Código EPSG

# Deben coincidir exactamente con los IDs usados en el script 042
id_objetivo <- "Cl"
id_auxiliar <- "FmSm"

# Rejilla de predicción
tam_pixel   <- 20                          # Resolución espacial en unidades del CRS (metros)
margen_bbox <- 0.05                        # Margen adicional: 0.05 = 5 %

# Opciones de visualización
tipo_mapa_base  <- "OpenStreetMap"         # Mapa base libre de API Key
mostrar_mapa    <- TRUE
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
  stop(paste("Estas columnas del dataset objetivo deben ser numéricas:", paste(vars_no_num_obj, collapse = ", ")))
}

if (length(vars_no_num_aux) > 0) {
  stop(paste("Estas columnas del dataset auxiliar deben ser numéricas:", paste(vars_no_num_aux, collapse = ", ")))
}

if (tam_pixel <= 0) {
  stop("tam_pixel debe ser mayor que cero.")
}

if (margen_bbox < 0) {
  stop("margen_bbox no puede ser negativo.")
}


# ==== Eliminar observaciones incompletas ====

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
cat("Resolución de la rejilla:", tam_pixel, "m\n")


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


# ==== Mapa rápido (Mapview sin API Key) ====

if (mostrar_mapa) {
  mapviewOptions(
    basemaps = tipo_mapa_base,
    basemaps.color.shuffle = FALSE
  )
  
  print(
    mapview(
      z1_sf,
      zcol = nombre_objetivo,
      layer.name = paste0("Objetivo: ", nombre_objetivo)
    ) +
      mapview(
        z2_sf,
        zcol = nombre_auxiliar,
        layer.name = paste0("Auxiliar: ", nombre_auxiliar)
      )
  )
}


# ==== Cargar modelo LMC ====

crosscorrection <- readRDS(ruta_modelo_lmc)

cat("\n=====================================\n")
cat(" Modelo LMC cargado exitosamente:\n")
cat("=====================================\n")
print(crosscorrection)

cat("\nVariables configuradas en el modelo LMC:\n")
print(names(crosscorrection$data))


# ==== Comprobar identificadores del LMC ====

ids_modelo     <- names(crosscorrection$data)
ids_necesarios <- c(id_objetivo, id_auxiliar)

faltan_ids <- ids_necesarios[!ids_necesarios %in% ids_modelo]

if (length(faltan_ids) > 0) {
  stop(
    paste(
      "Estos identificadores no existen en el modelo LMC:",
      paste(faltan_ids, collapse = ", "),
      "\nRevisa que id_objetivo e id_auxiliar coincidan",
      "con los utilizados en el script 042."
    )
  )
}


# ==== Extensión de la rejilla de predicción ====

bbox <- st_bbox(
  st_union(
    st_geometry(z1_sf),
    st_geometry(z2_sf)
  )
)

ancho_bbox <- as.numeric(bbox["xmax"] - bbox["xmin"])
alto_bbox  <- as.numeric(bbox["ymax"] - bbox["ymin"])

xmin <- as.numeric(bbox["xmin"] - ancho_bbox * margen_bbox)
xmax <- as.numeric(bbox["xmax"] + ancho_bbox * margen_bbox)
ymin <- as.numeric(bbox["ymin"] - alto_bbox * margen_bbox)
ymax <- as.numeric(bbox["ymax"] + alto_bbox * margen_bbox)

extension_prediccion <- ext(
  xmin,
  xmax,
  ymin,
  ymax
)


# ==== Plantilla raster ====

r_template <- rast(
  ext = extension_prediccion,
  resolution = tam_pixel,
  crs = st_crs(z1_sf)$wkt
)

cat("\n=====================================\n")
cat(" Plantilla raster creada:\n")
cat("=====================================\n")
cat("Resolución X:", res(r_template)[1], "\n")
cat("Resolución Y:", res(r_template)[2], "\n")
cat("Filas:", nrow(r_template), "\n")
cat("Columnas:", ncol(r_template), "\n")
cat("Número de celdas:", ncell(r_template), "\n")


# ==== Centros de las celdas ====

grid_vect <- as.points(
  r_template,
  values = FALSE,
  na.rm = FALSE
)

grid_sf <- st_as_sf(grid_vect)

cat("\nNúmero de puntos de predicción creados:", nrow(grid_sf), "\n")


# ==== Mostrar rejilla ====

if (mostrar_rejilla) {
  mapviewOptions(basemaps = tipo_mapa_base)
  print(
    mapview(
      z2_sf,
      zcol = nombre_auxiliar,
      layer.name = nombre_auxiliar
    ) +
      mapview(
        grid_sf,
        color = "red",
        layer.name = "Puntos Rejilla"
      )
  )
}


# ==== Cokriging ordinario ====

prediction <- predict(
  crosscorrection,
  newdata = grid_sf
)

cat("\n=====================================\n")
cat(" Cokriging ordinario completado exitosamente\n")
cat("=====================================\n")

cat("Campos generados en el resultado:\n")
print(names(prediction))


# ==== Identificar campos de predicción ====

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
      "\nRevisa que id_objetivo e id_auxiliar coincidan",
      "con los utilizados en el script 042."
    )
  )
}


# ==== Crear rasters ====

# Los puntos de predicción proceden directamente de los
# centros de las celdas de r_template. Por tanto, el orden
# de los resultados coincide con el orden interno del raster
# y no es necesario reordenar manualmente las coordenadas.

r_pred_obj <- r_template
r_var_obj  <- r_template
r_pred_aux <- r_template
r_var_aux  <- r_template

values(r_pred_obj) <- prediction[[campo_pred_obj]]
values(r_var_obj)  <- prediction[[campo_var_obj]]

values(r_pred_aux) <- prediction[[campo_pred_aux]]
values(r_var_aux)  <- prediction[[campo_var_aux]]


# ==== Nombres de las capas raster ====

names(r_pred_obj) <- paste0(id_objetivo, "_CK_pred")
names(r_var_obj)  <- paste0(id_objetivo, "_CK_var")

names(r_pred_aux) <- paste0(id_auxiliar, "_CK_pred")
names(r_var_aux)  <- paste0(id_auxiliar, "_CK_var")


# ==== Resumen de los resultados ====

cat("\n=====================================\n")
cat(" Resumen de los rasters generados\n")
cat("=====================================\n")

cat("\nPredicción de la variable objetivo:\n")
print(global(r_pred_obj, range, na.rm = TRUE))

cat("\nVarianza de la variable objetivo:\n")
print(global(r_var_obj, range, na.rm = TRUE))

cat("\nPredicción de la variable auxiliar:\n")
print(global(r_pred_aux, range, na.rm = TRUE))

cat("\nVarianza de la variable auxiliar:\n")
print(global(r_var_aux, range, na.rm = TRUE))


# ==== Guardar rasters ====

if (guardar_rasters) {
  
  dir_rasters <- "salidas/kriging"
  
  if (!dir.exists(dir_rasters)) {
    dir.create(dir_rasters, recursive = TRUE)
  }
  
  nombre_pred_obj <- file.path(
    dir_rasters,
    paste0("043_", nombre_dataset_obj, "_", nombre_dataset_aux, "_CK_pred_objetivo.tif")
  )
  
  nombre_var_obj <- file.path(
    dir_rasters,
    paste0("043_", nombre_dataset_obj, "_", nombre_dataset_aux, "_CK_var_objetivo.tif")
  )
  
  nombre_pred_aux <- file.path(
    dir_rasters,
    paste0("043_", nombre_dataset_obj, "_", nombre_dataset_aux, "_CK_pred_auxiliar.tif")
  )
  
  nombre_var_aux <- file.path(
    dir_rasters,
    paste0("043_", nombre_dataset_obj, "_", nombre_dataset_aux, "_CK_var_auxiliar.tif")
  )
  
  writeRaster(r_pred_obj, nombre_pred_obj, overwrite = TRUE)
  writeRaster(r_var_obj,  nombre_var_obj,  overwrite = TRUE)
  writeRaster(r_pred_aux, nombre_pred_aux, overwrite = TRUE)
  writeRaster(r_var_aux,  nombre_var_aux,  overwrite = TRUE)
  
  cat("\n=====================================\n")
  cat(" Rasters guardados exitosamente en:\n")
  cat("=====================================\n")
  cat("Predicción objetivo:\n", nombre_pred_obj, "\n")
  cat("Varianza objetivo:\n",   nombre_var_obj,  "\n")
  cat("Predicción auxiliar:\n", nombre_pred_aux, "\n")
  cat("Varianza auxiliar:\n",   nombre_var_aux,  "\n\n")
}