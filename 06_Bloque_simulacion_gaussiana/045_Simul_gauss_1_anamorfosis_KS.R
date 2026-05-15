#####################################################
#
#  045. Simulación gaussiana 1: anamorfosis, variograma y SK
#
#  Descripción:
#  Prepara los datos para simulación secuencial gaussiana.
#  Lee datos puntuales, aplica anamorfosis gaussiana mediante
#  transformación por rangos normales, calcula el variograma de
#  la variable transformada y realiza kriging simple sobre una
#  rejilla regular.
#
#  Entrada:
#  - datos/brutos/meuse.csv
#
#  Salida:
#  - modelos/dataset_variable_anamorfosis.rds
#  - modelos/dataset_variable_variograma_gaussiano.rds
#  - modelos/dataset_variable_modelo_variograma_gaussiano.rds
#  - modelos/dataset_variable_rejilla.rds
#  - salidas/kriging/dataset_variable_SK_prediccion_gaussiana.tif
#  - salidas/kriging/dataset_variable_SK_varianza_gaussiana.tif
#  - salidas/figuras/dataset_variable_anamorfosis.pdf
#  - salidas/figuras/dataset_variable_variograma_gaussiano.pdf
#  - salidas/figuras/dataset_variable_SK_prediccion_gaussiana.pdf
#  - salidas/figuras/dataset_variable_SK_varianza_gaussiana.pdf
#
#####################################################

# ==== Cargar librerías ====

if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("gstat", quietly = TRUE)) install.packages("gstat")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")

library(terra)
library(gstat)
library(sf)
library(ggplot2)
library(viridis)

# ==== Archivo de datos ====

ruta_datos <- "datos/brutos/meuse.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",", header = TRUE)

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))

cat("\nDirectorio raíz del proyecto:\n", getwd(), "\n")

# ==== Parámetros a editar ====

nombre_x <- "x"              # <-- EDITAR AQUÍ
nombre_y <- "y"              # <-- EDITAR AQUÍ
nombre_variable <- "zinc"    # <-- EDITAR AQUÍ

epsg_datos <- 28992          # <-- EDITAR AQUÍ

# Rejilla de predicción
n_filas <- 100
n_columnas <- 100
margen_bbox <- 0.00

# Variograma variable gaussianizada
cutoff_variograma <- 1500
lag_variograma <- NULL       # NULL = gstat decide automáticamente

# Modelo inicial de variograma gaussiano
pSill <- 0.1
modelo_variograma <- "Sph"
rango <- 900
nugget <- 0.01

# Kriging simple
media_gaussiana <- 0          # beta para simple kriging

# Gráficos
numero_clases <- 13
formato_figura <- "pdf"      # "png" o "pdf"
guardar_figuras <- TRUE
guardar_rasters <- TRUE
guardar_modelos <- TRUE

ancho_figura <- 8
alto_figura <- 6
dpi_figura <- 300            # solo PNG

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

data_plot <- data[
  complete.cases(data[, vars]),
  vars,
  drop = FALSE
]

if (nrow(data_plot) < 3) {
  stop("Se necesitan al menos tres observaciones válidas.")
}

cat("\nVariable:", nombre_variable, "\n")
cat("EPSG:", epsg_datos, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")

# ==== Conversión a sf ====

data_sf <- st_as_sf(
  data_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)

# ==== Crear rejilla de predicción ====

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

# ==== Anamorfosis gaussiana ====

# Transformación por rangos normales.
# Se usa (rank - 0.5) / n para evitar cuantiles 0 y 1,
# que producirían valores -Inf o Inf en qnorm().

data_sf$Z_gauss <- qnorm(
  (rank(data_sf[[nombre_variable]], ties.method = "average") - 0.5) /
    length(data_sf[[nombre_variable]])
)

# Centrar exactamente en media cero
data_sf$Z_gauss <- data_sf$Z_gauss - mean(data_sf$Z_gauss, na.rm = TRUE)

cat("\nResumen variable original:\n")
print(summary(data_sf[[nombre_variable]]))

cat("\nResumen variable gaussianizada:\n")
print(summary(data_sf$Z_gauss))

# Tabla de anamorfosis para reversión posterior
tabla_anamorfosis <- data.frame(
  valor_original = data_sf[[nombre_variable]],
  valor_gaussiano = data_sf$Z_gauss
)

tabla_anamorfosis <- tabla_anamorfosis[
  order(tabla_anamorfosis$valor_gaussiano),
]

# ==== Gráfico anamorfosis ====

grafico_anamorfosis <- ggplot(data.frame(Z_gauss = data_sf$Z_gauss), aes(x = Z_gauss)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = numero_clases,
    color = "black",
    fill = "orange",
    alpha = 0.25
  ) +
  geom_density(
    color = "blue",
    linewidth = 1,
    alpha = 0.75
  ) +
  labs(
    title = "Variable gaussianizada",
    subtitle = paste("Anamorfosis gaussiana de", nombre_variable),
    x = "Cuantiles normales",
    y = "Densidad"
  ) +
  theme_gray() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )

print(grafico_anamorfosis)

# ==== Variograma nube ====

cloud_gauss <- variogram(
  Z_gauss ~ 1,
  data = data_sf,
  cutoff = cutoff_variograma,
  cloud = TRUE
)

cat("\nVariograma nube calculado.\n")

# ==== Variograma experimental ====

if (is.null(lag_variograma)) {
  variograma_gauss <- variogram(
    Z_gauss ~ 1,
    data = data_sf,
    cutoff = cutoff_variograma
  )
} else {
  variograma_gauss <- variogram(
    Z_gauss ~ 1,
    data = data_sf,
    cutoff = cutoff_variograma,
    width = lag_variograma
  )
}

cat("\nVariograma experimental de la variable gaussianizada:\n")
print(variograma_gauss)

# ==== Ajuste del modelo de variograma ====

modelo_inicial <- vgm(
  psill = pSill,
  model = modelo_variograma,
  range = rango,
  nugget = nugget
)

modelo_gauss <- fit.variogram(
  variograma_gauss,
  model = modelo_inicial
)

cat("\n=====================================\n")
cat(" Modelo de variograma ajustado\n")
cat("=====================================\n")
print(modelo_gauss)

# ==== Gráfico variograma ====

linea_modelo <- variogramLine(
  modelo_gauss,
  maxdist = max(variograma_gauss$dist, na.rm = TRUE)
)

grafico_variograma <- ggplot() +
  geom_point(
    data = as.data.frame(variograma_gauss),
    aes(x = dist, y = gamma),
    color = "blue",
    size = 2
  ) +
  geom_line(
    data = as.data.frame(linea_modelo),
    aes(x = dist, y = gamma),
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Variograma experimental con ajuste",
    subtitle = "Variable gaussianizada",
    x = "Distancia",
    y = "Semivarianza"
  ) +
  theme_gray() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )

print(grafico_variograma)

# ==== Kriging simple ====

simple_kriging <- krige(
  Z_gauss ~ 1,
  locations = data_sf,
  newdata = grid_sf,
  model = modelo_gauss,
  beta = media_gaussiana
)

cat("\nKriging simple completado.\n")
print(names(simple_kriging))

simple_kriging_sf <- st_as_sf(simple_kriging)
simple_kriging_sf <- cbind(simple_kriging_sf, st_coordinates(simple_kriging_sf))

# ==== Visualización predicción SK ====

grafico_SK_pred <- ggplot(simple_kriging_sf, aes(x = X, y = Y, fill = var1.pred)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(
    title = "Predicción por kriging simple",
    subtitle = "Variable gaussianizada",
    x = nombre_x,
    y = nombre_y,
    fill = "Predicción"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )

print(grafico_SK_pred)

# ==== Visualización varianza SK ====

grafico_SK_var <- ggplot(simple_kriging_sf, aes(x = X, y = Y, fill = var1.var)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(
    title = "Varianza de kriging simple",
    subtitle = "Variable gaussianizada",
    x = nombre_x,
    y = nombre_y,
    fill = "Varianza"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )

print(grafico_SK_var)

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

sk_df <- data.frame(
  X = grid$x,
  Y = grid$y,
  prediccion = simple_kriging$var1.pred,
  varianza = simple_kriging$var1.var
)

sk_df_ord <- sk_df[
  order(sk_df$Y, decreasing = TRUE, sk_df$X),
]

r_pred <- r_template
r_var  <- r_template

values(r_pred) <- sk_df_ord$prediccion
values(r_var)  <- sk_df_ord$varianza

names(r_pred) <- paste0(nombre_variable, "_SK_pred_gauss")
names(r_var)  <- paste0(nombre_variable, "_SK_var_gauss")

# ==== Guardar modelos y objetos ====

if (guardar_modelos) {
  
  dir_modelos <- "modelos"
  
  if (!dir.exists(dir_modelos)) {
    dir.create(dir_modelos, recursive = TRUE)
  }
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  ruta_anamorfosis <- file.path(
    dir_modelos,
    paste0(nombre_dataset, "_", nombre_variable, "_anamorfosis.rds")
  )
  
  ruta_variograma <- file.path(
    dir_modelos,
    paste0(nombre_dataset, "_", nombre_variable, "_variograma_gaussiano.rds")
  )
  
  ruta_modelo_variograma <- file.path(
    dir_modelos,
    paste0(nombre_dataset, "_", nombre_variable, "_modelo_variograma_gaussiano.rds")
  )
  
  ruta_rejilla <- file.path(
    dir_modelos,
    paste0(nombre_dataset, "_", nombre_variable, "_rejilla.rds")
  )
  
  saveRDS(tabla_anamorfosis, ruta_anamorfosis)
  saveRDS(variograma_gauss, ruta_variograma)
  saveRDS(modelo_gauss, ruta_modelo_variograma)
  saveRDS(grid_sf, ruta_rejilla)
  
  cat("\n=====================================\n")
  cat(" Objetos guardados\n")
  cat("=====================================\n")
  cat("Anamorfosis:\n", ruta_anamorfosis, "\n")
  cat("Variograma:\n", ruta_variograma, "\n")
  cat("Modelo variograma:\n", ruta_modelo_variograma, "\n")
  cat("Rejilla:\n", ruta_rejilla, "\n\n")
}

# ==== Guardar rasters ====

if (guardar_rasters) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_kriging <- "salidas/kriging"
  
  if (!dir.exists(dir_kriging)) {
    dir.create(dir_kriging, recursive = TRUE)
  }
  
  nombre_raster_pred <- file.path(
    dir_kriging,
    paste0(nombre_dataset, "_", nombre_variable, "_SK_prediccion_gaussiana.tif")
  )
  
  nombre_raster_var <- file.path(
    dir_kriging,
    paste0(nombre_dataset, "_", nombre_variable, "_SK_varianza_gaussiana.tif")
  )
  
  writeRaster(r_pred, nombre_raster_pred, overwrite = TRUE)
  writeRaster(r_var, nombre_raster_var, overwrite = TRUE)
  
  cat("\n=====================================\n")
  cat(" Rasters guardados\n")
  cat("=====================================\n")
  cat("Predicción SK:\n", nombre_raster_pred, "\n")
  cat("Existe:", file.exists(nombre_raster_pred), "\n")
  cat("Varianza SK:\n", nombre_raster_var, "\n")
  cat("Existe:", file.exists(nombre_raster_var), "\n\n")
}

# ==== Guardar figuras ====

if (guardar_figuras) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  figuras <- list(
    anamorfosis = grafico_anamorfosis,
    variograma_gaussiano = grafico_variograma,
    SK_prediccion_gaussiana = grafico_SK_pred,
    SK_varianza_gaussiana = grafico_SK_var
  )
  
  for (nombre_fig in names(figuras)) {
    
    nombre_salida <- file.path(
      dir_figuras,
      paste0(
        nombre_dataset, "_",
        nombre_variable, "_",
        nombre_fig, ".",
        formato_figura
      )
    )
    
    if (formato_figura == "png") {
      
      ggsave(
        filename = nombre_salida,
        plot = figuras[[nombre_fig]],
        width = ancho_figura,
        height = alto_figura,
        dpi = dpi_figura
      )
      
    } else if (formato_figura == "pdf") {
      
      ggsave(
        filename = nombre_salida,
        plot = figuras[[nombre_fig]],
        width = ancho_figura,
        height = alto_figura,
        device = "pdf"
      )
      
    } else {
      stop("Formato no soportado. Usa 'png' o 'pdf'.")
    }
    
    cat("Figura guardada:", nombre_salida, "\n")
  }
}
