#####################################################
#
#  037. Indicator kriging
#
#  Descripción:
#  Realiza kriging de indicadores a partir de una
#  variable continua transformada en una variable binaria
#  según un umbral. El resultado se interpreta como una
#  probabilidad espacial asociada al cumplimiento de la
#  condición definida.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - mapa interactivo de puntos en el visor
#  - salidas/rasters/dataset_variable_IK_probabilidad.tif
#  - salidas/rasters/dataset_variable_IK_varianza.tif
#  - salidas/rasters/dataset_variable_IK_clasificacion.tif
#  - (opcional) salidas/figuras/dataset_variable_IK_probabilidad.png o .pdf
#  - (opcional) salidas/figuras/dataset_variable_IK_varianza.png o .pdf
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

nombre_x <- "x"                  # <-- EDITAR AQUÍ
nombre_y <- "y"                  # <-- EDITAR AQUÍ
nombre_variable <- "log10_zinc"       # <-- EDITAR AQUÍ

epsg_datos <- 28992              # <-- EDITAR AQUÍ

# Condición indicadora
umbral <- log10(150)             # <-- EDITAR AQUÍ
tipo_condicion <- "menor"        # "menor" o "mayor"

nombre_indicador <- "indicador"

# Variograma experimental del indicador
cutoff_variograma <- 1500
lag_variograma <- NULL           # NULL = gstat decide

# Modelo inicial de variograma
pSill <- 0.135
modelo_variograma <- "Sph"
rango <- 1300
nugget <- 0.01

# Rejilla de predicción
n_filas <- 100
n_columnas <- 100
buffer_dist <- 200               # en unidades del CRS

# Clasificación probabilística
umbral_probabilidad <- 0.8        # p >= 0.8

# Opciones
mostrar_mapa <- TRUE
mostrar_rejilla <- FALSE
ajustar_variograma <- TRUE

# Guardado
guardar_rasters <- TRUE
guardar_figuras <- TRUE
formato_figura <- "pdf"          # "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300              # solo PNG

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

if (!tipo_condicion %in% c("menor", "mayor")) {
  stop("tipo_condicion debe ser 'menor' o 'mayor'.")
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
cat("Umbral:", umbral, "\n")
cat("Condición:", tipo_condicion, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")

# ==== Conversión a sf ====

data_sf <- st_as_sf(
  data_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)

# ==== Crear variable indicadora ====

if (tipo_condicion == "menor") {
  data_sf[[nombre_indicador]] <- as.numeric(data_sf[[nombre_variable]] < umbral)
  texto_condicion <- paste0(nombre_variable, " < ", round(umbral, 4))
} else {
  data_sf[[nombre_indicador]] <- as.numeric(data_sf[[nombre_variable]] > umbral)
  texto_condicion <- paste0(nombre_variable, " > ", round(umbral, 4))
}

cat("\nResumen de la variable indicadora:\n")
print(summary(data_sf[[nombre_indicador]]))

cat("\nProporción de casos TRUE:\n")
print(mean(data_sf[[nombre_indicador]], na.rm = TRUE))

# ==== Mapa rápido ====

if (mostrar_mapa) {
  print(
    mapview(
      data_sf,
      zcol = nombre_variable,
      map.types = "CartoDB.Voyager"
    )
  )
}

# ==== Variograma experimental del indicador ====

formula_indicador <- as.formula(
  paste(nombre_indicador, "~ 1")
)

if (is.null(lag_variograma)) {
  variograma_ind <- variogram(
    formula_indicador,
    data = data_sf,
    cutoff = cutoff_variograma
  )
} else {
  variograma_ind <- variogram(
    formula_indicador,
    data = data_sf,
    cutoff = cutoff_variograma,
    width = lag_variograma
  )
}

if (nrow(variograma_ind) == 0) {
  stop("El variograma del indicador no contiene clases. Revisa cutoff y lag.")
}

cat("\nVariograma experimental del indicador:\n")
print(variograma_ind)

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
    object = variograma_ind,
    model = modelo_inicial
  )
} else {
  modelo_final <- modelo_inicial
}

cat("\n=====================================\n")
cat(" Modelo usado en indicator kriging\n")
cat("=====================================\n")
print(modelo_final)

# ==== Rejilla de predicción ====

hull <- st_convex_hull(st_union(data_sf))
hull_buffer <- st_buffer(hull, dist = buffer_dist)

bbox <- st_bbox(hull_buffer)

x_range <- seq(bbox["xmin"], bbox["xmax"], length.out = n_columnas)
y_range <- seq(bbox["ymin"], bbox["ymax"], length.out = n_filas)

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

grid_sf <- st_intersection(grid_sf, hull_buffer)

grid_sf$X <- st_coordinates(grid_sf)[, 1]
grid_sf$Y <- st_coordinates(grid_sf)[, 2]

cat("\nRejilla de predicción creada:\n")
cat("Número de puntos:", nrow(grid_sf), "\n")

if (mostrar_rejilla) {
  print(
    mapview(data_sf, zcol = nombre_variable, map.types = "CartoDB.Voyager") +
      mapview(grid_sf, color = "red") +
      mapview(hull_buffer, color = "blue")
  )
}

# ==== Indicator kriging ====

ik <- krige(
  formula = formula_indicador,
  locations = data_sf,
  newdata = grid_sf,
  model = modelo_final
)

cat("\nIndicator kriging completado.\n")

# ==== Preparar resultados ====

kpred_df <- data.frame(
  X = grid_sf$X,
  Y = grid_sf$Y,
  probabilidad = ik$var1.pred,
  varianza = ik$var1.var
)

# Limitar probabilidades al rango [0, 1], si se desea evitar valores ligeramente fuera por ajuste
kpred_df$probabilidad <- pmin(pmax(kpred_df$probabilidad, 0), 1)

kpred_df$clasificacion <- kpred_df$probabilidad >= umbral_probabilidad

cat("\nResumen de probabilidades:\n")
print(summary(kpred_df$probabilidad))

cat("\nResumen de clasificación:\n")
print(table(kpred_df$clasificacion))

# ==== Crear rasters ====

r_template <- rast(
  xmin = bbox["xmin"],
  xmax = bbox["xmax"],
  ymin = bbox["ymin"],
  ymax = bbox["ymax"],
  nrows = n_filas,
  ncols = n_columnas,
  crs = st_crs(data_sf)$wkt
)

r_prob <- rasterize(
  vect(st_as_sf(kpred_df, coords = c("X", "Y"), crs = st_crs(data_sf))),
  r_template,
  field = "probabilidad"
)

r_var <- rasterize(
  vect(st_as_sf(kpred_df, coords = c("X", "Y"), crs = st_crs(data_sf))),
  r_template,
  field = "varianza"
)

r_clas <- rasterize(
  vect(st_as_sf(kpred_df, coords = c("X", "Y"), crs = st_crs(data_sf))),
  r_template,
  field = "clasificacion"
)

names(r_prob) <- paste0(nombre_variable, "_IK_prob")
names(r_var)  <- paste0(nombre_variable, "_IK_var")
names(r_clas) <- paste0(nombre_variable, "_IK_clas")

# ==== Guardar rasters ====

if (guardar_rasters) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_rasters <- "salidas/kriging"
  
  if (!dir.exists(dir_rasters)) {
    dir.create(dir_rasters, recursive = TRUE)
  }
  
  nombre_raster_prob <- file.path(
    dir_rasters,
    paste0(nombre_dataset, "_", nombre_variable, "_IK_probabilidad.tif")
  )
  
  nombre_raster_var <- file.path(
    dir_rasters,
    paste0(nombre_dataset, "_", nombre_variable, "_IK_varianza.tif")
  )
  
  nombre_raster_clas <- file.path(
    dir_rasters,
    paste0(nombre_dataset, "_", nombre_variable, "_IK_clasificacion.tif")
  )
  
  writeRaster(r_prob, nombre_raster_prob, overwrite = TRUE)
  writeRaster(r_var, nombre_raster_var, overwrite = TRUE)
  writeRaster(r_clas, nombre_raster_clas, overwrite = TRUE)
  
  cat("\n=====================================\n")
  cat(" Rasters guardados\n")
  cat("=====================================\n")
  cat("Probabilidad:\n", nombre_raster_prob, "\n")
  cat("Varianza:\n", nombre_raster_var, "\n")
  cat("Clasificación:\n", nombre_raster_clas, "\n\n")
}

# ==== Visualización probabilidad ====

grafico_prob <- ggplot(kpred_df, aes(x = X, y = Y, fill = probabilidad)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, 1)) +
  labs(
    title = "Indicator kriging: probabilidad",
    subtitle = texto_condicion,
    x = nombre_x,
    y = nombre_y,
    fill = "Probabilidad"
  ) +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )

print(grafico_prob)

# ==== Visualización varianza ====

grafico_var <- ggplot(kpred_df, aes(x = X, y = Y, fill = varianza)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Indicator kriging: varianza",
    subtitle = texto_condicion,
    x = nombre_x,
    y = nombre_y,
    fill = "Varianza"
  ) +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
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
  
  nombre_figura_prob <- file.path(
    dir_figuras,
    paste0(nombre_dataset, "_", nombre_variable, "_IK_probabilidad.", formato_figura)
  )
  
  nombre_figura_var <- file.path(
    dir_figuras,
    paste0(nombre_dataset, "_", nombre_variable, "_IK_varianza.", formato_figura)
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura_prob,
      plot = grafico_prob,
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
      filename = nombre_figura_prob,
      plot = grafico_prob,
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
  cat("Probabilidad:\n", nombre_figura_prob, "\n")
  cat("Varianza:\n", nombre_figura_var, "\n\n")
}

