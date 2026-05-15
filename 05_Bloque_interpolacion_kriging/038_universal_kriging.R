#####################################################
#
#  038. Universal kriging
#
#  Descripción:
#  Realiza una interpolación mediante kriging universal,
#  incorporando una tendencia espacial de primer orden
#  en función de las coordenadas X e Y. El variograma se
#  calcula sobre los residuos del modelo de tendencia y
#  se utiliza después para predecir la variable objetivo
#  sobre una rejilla regular.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - salidas/rasters/dataset_variable_UK_prediccion.tif
#  - salidas/rasters/dataset_variable_UK_varianza.tif
#  - (opcional) salidas/figuras/dataset_variable_UK_prediccion.png o .pdf
#  - (opcional) salidas/figuras/dataset_variable_UK_varianza.png o .pdf
#
#####################################################

# ==== Cargar librerías ====

if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("gstat", quietly = TRUE)) install.packages("gstat")
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")

library(sf)
library(gstat)
library(terra)
library(ggplot2)
library(viridis)

# ==== Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))

# ==== Parámetros a editar ====

nombre_x <- "columnax"                  # <-- EDITAR AQUÍ
nombre_y <- "columnay"                  # <-- EDITAR AQUÍ
nombre_variable <- "variable_objetivo"          # <-- EDITAR AQUÍ

epsg_datos <- 12345              # <-- EDITAR AQUÍ

# Variograma residual
usar_maxlag_automatico <- TRUE
cutoff_variograma <- NULL        # si usar_maxlag_automatico = FALSE, poner valor numérico
numero_lags <- 15

# Modelo inicial de variograma
pSill <- 2987
modelo_variograma <- "Sph"
rango <- 97659
nugget <- 886

fit_method <- 2                  # método de ajuste en fit.variogram()

# Rejilla de predicción
n_filas <- 100
n_columnas <- 100
margen_bbox <- 0.15

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

if (numero_lags <= 0) {
  stop("numero_lags debe ser mayor que cero.")
}

data_plot <- data[
  complete.cases(data[, vars]),
  vars,
  drop = FALSE
]

if (nrow(data_plot) < 3) {
  stop("Se necesitan al menos tres observaciones válidas para aplicar kriging universal.")
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

# Añadir coordenadas explícitas para el modelo de tendencia
data_sf$coord_x <- st_coordinates(data_sf)[, 1]
data_sf$coord_y <- st_coordinates(data_sf)[, 2]

# ==== Definir fórmula de tendencia ====

formula_uk <- as.formula(
  paste(nombre_variable, "~ coord_x + coord_y")
)

cat("\nFórmula usada en kriging universal:\n")
print(formula_uk)

# ==== Calcular cutoff y lag ====

if (usar_maxlag_automatico) {
  
  bbox <- st_bbox(data_sf)
  
  maxlag <- 0.5 * sqrt(
    (bbox["xmax"] - bbox["xmin"])^2 +
      (bbox["ymax"] - bbox["ymin"])^2
  )
  
  cutoff_usado <- as.numeric(maxlag)
  
} else {
  
  if (is.null(cutoff_variograma) || cutoff_variograma <= 0) {
    stop("Si usar_maxlag_automatico = FALSE, cutoff_variograma debe ser un número positivo.")
  }
  
  cutoff_usado <- cutoff_variograma
}

lag_usado <- cutoff_usado / numero_lags

cat("\nCutoff usado:", cutoff_usado, "\n")
cat("Lag usado:", lag_usado, "\n")

# ==== Variograma residual ====

variograma_residual <- variogram(
  formula_uk,
  data = data_sf,
  cutoff = cutoff_usado,
  width = lag_usado
)

if (nrow(variograma_residual) == 0) {
  stop("El variograma residual no contiene clases. Revisa cutoff y lag.")
}

cat("\nVariograma residual calculado:\n")
print(variograma_residual)

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

modelo_ajustado <- fit.variogram(
  variograma_residual,
  modelo_inicial,
  fit.method = fit_method
)

cat("\n=====================================\n")
cat(" Modelo ajustado\n")
cat("=====================================\n")
print(modelo_ajustado)

# ==== Rejilla de predicción ====

bbox <- st_bbox(data_sf)

xmin <- bbox["xmin"] - (bbox["xmax"] - bbox["xmin"]) * margen_bbox
xmax <- bbox["xmax"] + (bbox["xmax"] - bbox["xmin"]) * margen_bbox
ymin <- bbox["ymin"] - (bbox["ymax"] - bbox["ymin"]) * margen_bbox
ymax <- bbox["ymax"] + (bbox["ymax"] - bbox["ymin"]) * margen_bbox

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

# Coordenadas necesarias para la tendencia
grid_sf$coord_x <- st_coordinates(grid_sf)[, 1]
grid_sf$coord_y <- st_coordinates(grid_sf)[, 2]

cat("\nRejilla de predicción creada:\n")
cat("Filas:", n_filas, "\n")
cat("Columnas:", n_columnas, "\n")
cat("Número de celdas:", nrow(grid_sf), "\n")

# ==== Universal kriging ====

kpred <- krige(
  formula = formula_uk,
  locations = data_sf,
  model = modelo_ajustado,
  newdata = grid_sf
)

cat("\nUniversal kriging completado.\n")

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

kpred_df_ord <- kpred_df[order(kpred_df$Y, decreasing = TRUE, kpred_df$X), ]

values(r_pred) <- kpred_df_ord$prediccion
values(r_var)  <- kpred_df_ord$varianza

names(r_pred) <- paste0(nombre_variable, "_UK_pred")
names(r_var)  <- paste0(nombre_variable, "_UK_var")

# ==== Guardar rasters ====

if (guardar_rasters) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_rasters <- "salidas/kriging"
  
  if (!dir.exists(dir_rasters)) {
    dir.create(dir_rasters, recursive = TRUE)
  }
  
  nombre_raster_pred <- file.path(
    dir_rasters,
    paste0(nombre_dataset, "_", nombre_variable, "_UK_prediccion.tif")
  )
  
  nombre_raster_var <- file.path(
    dir_rasters,
    paste0(nombre_dataset, "_", nombre_variable, "_UK_varianza.tif")
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
    title = "Predicción por kriging universal",
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
    title = "Varianza de kriging universal",
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
    paste0(nombre_dataset, "_", nombre_variable, "_UK_prediccion.", formato_figura)
  )
  
  nombre_figura_var <- file.path(
    dir_figuras,
    paste0(nombre_dataset, "_", nombre_variable, "_UK_varianza.", formato_figura)
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
      device = "pdf"
    )
    
    ggsave(
      filename = nombre_figura_var,
      plot = grafico_var,
      width = ancho_figura,
      height = alto_figura,
      device = "pdf"
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