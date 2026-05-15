#####################################################
#
#  029. Superficie polinómica para GIS
#
#  Descripción:
#  Ajusta una superficie polinómica de orden 1, 2 o 3
#  a una variable Z en función de coordenadas X e Y,
#  predice los valores sobre una rejilla regular y exporta
#  el resultado como raster GeoTIFF para su uso en GIS.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - resumen del modelo en consola
#  - salidas/rasters/dataset_varZ_superficie_orden_N.tif
#
#####################################################

# Cargar librerías ====

if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")

library(sf)
library(terra)



# Archivo de datos ====

ruta_datos <- "datos/brutos/wolfcamp.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable_x <- "x"   # coordenada X  <-- EDITAR AQUÍ
nombre_variable_y <- "y"   # coordenada Y  <-- EDITAR AQUÍ
nombre_variable_z <- "gw"   # variable Z    <-- EDITAR AQUÍ

orden_modelo <- 2                 # 1, 2 o 3      <-- EDITAR AQUÍ

crs_epsg <- 32613                 # EPSG proyectado en metros <-- EDITAR AQUÍ

buffer_dist <- 200                # metros        <-- EDITAR AQUÍ
resolucion <- 100                 # tamaño de celda en metros <-- EDITAR AQUÍ

guardar_raster <- TRUE            # TRUE / FALSE



# Comprobaciones ====

vars <- c(nombre_variable_x, nombre_variable_y, nombre_variable_z)

faltan <- vars[!vars %in% names(data)]

if (length(faltan) > 0) {
  stop(paste("No existen estas columnas en el dataset:", paste(faltan, collapse = ", ")))
}

vars_no_num <- vars[!sapply(data[vars], is.numeric)]

if (length(vars_no_num) > 0) {
  stop(paste("Estas variables deben ser numéricas:", paste(vars_no_num, collapse = ", ")))
}

if (!orden_modelo %in% c(1, 2, 3)) {
  stop("orden_modelo debe ser 1, 2 o 3.")
}

if (resolucion <= 0) {
  stop("La resolución debe ser mayor que cero.")
}

if (buffer_dist < 0) {
  stop("El buffer no puede ser negativo.")
}



# Preparar datos del modelo ====

datos <- data[
  complete.cases(data[, vars]),
  vars,
  drop = FALSE
]

if (nrow(datos) == 0) {
  stop("No quedan observaciones válidas tras eliminar NA en X, Y y Z.")
}

colnames(datos) <- c("X", "Y", "Z")

cat("\nVariable X:", nombre_variable_x, "\n")
cat("Variable Y:", nombre_variable_y, "\n")
cat("Variable Z:", nombre_variable_z, "\n")
cat("Orden del modelo:", orden_modelo, "\n")
cat("EPSG:", crs_epsg, "\n")
cat("Resolución:", resolucion, "m\n")
cat("Buffer:", buffer_dist, "m\n")
cat("Número de observaciones válidas:", nrow(datos), "\n")




# Ajuste del modelo ====

if (orden_modelo == 1) {
  formula_modelo <- Z ~ X + Y
}

if (orden_modelo == 2) {
  formula_modelo <- Z ~ X + Y + I(X^2) + I(Y^2) + I(X * Y)
}

if (orden_modelo == 3) {
  formula_modelo <- Z ~ X + Y +
    I(X^2) + I(Y^2) + I(X * Y) +
    I(X^3) + I(Y^3) +
    I(X^2 * Y) + I(X * Y^2)
}

modelo <- lm(formula_modelo, data = datos)

cat("\n=====================================\n")
cat(" Resumen del modelo\n")
cat("=====================================\n")
print(summary(modelo))



# Convertir puntos a sf y preparación del raster ====

data_sf <- st_as_sf(
  datos,
  coords = c("X", "Y"),
  crs = crs_epsg,
  remove = FALSE
)


# Envolvente convexa + buffer

hull <- st_convex_hull(st_union(data_sf))
hull_buffer <- st_buffer(hull, dist = buffer_dist)

hull_vect <- terra::vect(hull_buffer)
ext_buffer <- terra::ext(hull_vect)


# Crear raster vacío

raster_surface <- terra::rast(
  xmin = ext_buffer$xmin,
  xmax = ext_buffer$xmax,
  ymin = ext_buffer$ymin,
  ymax = ext_buffer$ymax,
  resolution = resolucion,
  crs = st_crs(data_sf)$wkt
)


# Predicción sobre centros de celda

coords <- terra::crds(raster_surface, df = TRUE)

newdata_pred <- data.frame(
  X = coords$x,
  Y = coords$y
)

Z_pred <- predict(modelo, newdata = newdata_pred)

terra::values(raster_surface) <- Z_pred

# Enmascarar con la envolvente buffered
raster_mask <- terra::mask(raster_surface, hull_vect)

names(raster_mask) <- paste0(nombre_variable_z, "_orden_", orden_modelo)


# Guardar raster GeoTIFF ====

if (guardar_raster) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_salida <- "salidas/rasters"
  
  if (!dir.exists(dir_salida)) {
    dir.create(dir_salida, recursive = TRUE)
  }
  
  nombre_salida <- file.path(
    dir_salida,
    paste0(
      nombre_dataset, "_",
      nombre_variable_z,
      "_superficie_orden_",
      orden_modelo,
      ".tif"
    )
  )
  
  terra::writeRaster(
    raster_mask,
    nombre_salida,
    overwrite = TRUE
  )
  
  cat("\n=====================================\n")
  cat(" Raster GeoTIFF exportado\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_salida, "\n\n")
}