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
#  La geometría de la rejilla se define mediante una plantilla
#  raster de terra, que se guarda para ser reutilizada por los
#  scripts posteriores del bloque de simulación (046-049).
#
#  Entrada:
#  - datos/brutos/meuse.csv
#
#  Salida:
#  - modelos/045_dataset_variable_anamorfosis.rds
#  - modelos/045_dataset_variable_variograma_gaussiano.rds
#  - modelos/045_dataset_variable_modelo_variograma_gaussiano.rds
#  - modelos/045_dataset_variable_rejilla.rds
#  - salidas/kriging/045_dataset_variable_SK_prediccion_gaussiana.tif
#  - salidas/kriging/045_dataset_variable_SK_varianza_gaussiana.tif
#  - salidas/figuras/045_dataset_variable_anamorfosis.pdf
#  - salidas/figuras/045_dataset_variable_variograma_gaussiano.pdf
#  - salidas/figuras/045_dataset_variable_SK_prediccion_gaussiana.pdf
#  - salidas/figuras/045_dataset_variable_SK_varianza_gaussiana.pdf
#
#####################################################


# ==== Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("terra", "gstat", "sf", "ggplot2", "viridis")

instalar_si_falta <- function(pkgs) {
  nuevos <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(nuevos) > 0) {
    message("Instalando paquetes faltantes: ", paste(nuevos, collapse = ", "))
    install.packages(nuevos, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

instalar_si_falta(paquetes_requeridos)


# ==== Archivo de datos ====

ruta_datos <- "datos/brutos/meuse.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(
  ruta_datos,
  sep = ",",
  header = TRUE
)

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))

cat("\nDirectorio raíz del proyecto:\n", getwd(), "\n")


# ==== Nombre del dataset ====

nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))


# ==== Parámetros a editar ====

nombre_x        <- "x"               # <-- EDITAR AQUÍ
nombre_y        <- "y"               # <-- EDITAR AQUÍ
nombre_variable <- "zinc"            # <-- EDITAR AQUÍ

epsg_datos <- 28992                  # <-- EDITAR AQUÍ

# Rejilla de predicción
tam_pixel   <- 40                    # Resolución espacial en unidades del CRS (metros)
margen_bbox <- 0.00                  # Proporción añadida a los bordes del bbox

# Variograma de la variable gaussianizada
cutoff_variograma <- 1500
lag_variograma    <- NULL            # NULL = gstat decide automáticamente

# Modelo inicial de variograma gaussiano
pSill             <- 0.80
modelo_variograma <- "Sph"
rango             <- 900
nugget            <- 0.10

fit_method <- 7

# Kriging simple
media_gaussiana <- 0                 # Beta para simple kriging (media teórica = 0)

# Gráficos
numero_clases  <- 13
formato_figura <- "pdf"              # "png" o "pdf"

guardar_figuras <- TRUE
guardar_rasters <- TRUE
guardar_modelos <- TRUE

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300                  # Solo para PNG


# ==== Comprobaciones ====

vars <- c(
  nombre_x,
  nombre_y,
  nombre_variable
)

faltan <- vars[!vars %in% colnames(data)]

if (length(faltan) > 0) {
  stop(paste("No existen estas columnas en el dataset:", paste(faltan, collapse = ", ")))
}

vars_no_num <- vars[!sapply(data[vars], is.numeric)]

if (length(vars_no_num) > 0) {
  stop(paste("Estas variables deben ser numéricas:", paste(vars_no_num, collapse = ", ")))
}

if (tam_pixel <= 0) {
  stop("tam_pixel debe ser mayor que cero.")
}

if (margen_bbox < 0) {
  stop("margen_bbox no puede ser negativo.")
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


# ==== Extensión de la rejilla de predicción ====

bbox <- st_bbox(data_sf)

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


# ==== Crear plantilla raster ====

# Esta plantilla constituye la geometría espacial maestra
# que reutilizarán los scripts posteriores (046-049).

r_template <- rast(
  ext = extension_prediccion,
  resolution = tam_pixel,
  crs = st_crs(data_sf)$wkt
)

cat("\n=====================================\n")
cat(" Plantilla raster creada:\n")
cat("=====================================\n")
cat("Resolución X:", res(r_template)[1], "\n")
cat("Resolución Y:", res(r_template)[2], "\n")
cat("Filas:", nrow(r_template), "\n")
cat("Columnas:", ncol(r_template), "\n")
cat("Número de celdas:", ncell(r_template), "\n")


# ==== Crear puntos de predicción ====

# Los puntos corresponden exactamente a los centros de
# las celdas de la plantilla raster.

grid_vect <- as.points(
  r_template,
  values = FALSE,
  na.rm = FALSE
)

grid_sf <- st_as_sf(grid_vect)

cat("Número de puntos de predicción creados:", nrow(grid_sf), "\n")


# ==== Anamorfosis gaussiana ====

# Transformación por rangos normales.
# Se usa (rank - 0.5) / n para evitar cuantiles 0 y 1 en qnorm().

data_sf$Z_gauss <- qnorm(
  (rank(data_sf[[nombre_variable]], ties.method = "average") - 0.5) /
    length(data_sf[[nombre_variable]])
)

# Centrar exactamente en media cero
data_sf$Z_gauss <- data_sf$Z_gauss - mean(data_sf$Z_gauss, na.rm = TRUE)

cat("\nResumen de la variable original:\n")
print(summary(data_sf[[nombre_variable]]))

cat("\nResumen de la variable gaussianizada:\n")
print(summary(data_sf$Z_gauss))

cat("\nMedia de la variable gaussianizada:", mean(data_sf$Z_gauss, na.rm = TRUE), "\n")
cat("Varianza de la variable gaussianizada:", var(data_sf$Z_gauss, na.rm = TRUE), "\n")


# ==== Tabla de anamorfosis ====

# Conserva la relación empírica entre la variable original y la
# gaussianizada para posibilitar la reversión posterior.

tabla_anamorfosis <- data.frame(
  valor_original  = data_sf[[nombre_variable]],
  valor_gaussiano = data_sf$Z_gauss
)

tabla_anamorfosis <- tabla_anamorfosis[
  order(tabla_anamorfosis$valor_gaussiano),
  ,
  drop = FALSE
]


# ==== Gráfico de anamorfosis ====

grafico_anamorfosis <- ggplot(
  data.frame(Z_gauss = data_sf$Z_gauss),
  aes(x = Z_gauss)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = numero_clases,
    color = "black",
    fill = "orange",
    alpha = 0.3
  ) +
  geom_density(
    color = "blue",
    linewidth = 1,
    alpha = 0.75
  ) +
  labs(
    title = "Variable Gaussianizada",
    subtitle = paste("Anamorfosis gaussiana de", nombre_variable),
    x = "Cuantiles normales N(0,1)",
    y = "Densidad"
  ) +
  theme_gray() +   # Tema gris por defecto
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(colour = "black", size = 10)
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
  model = modelo_inicial,
  fit.method = fit_method
)

cat("\n=====================================\n")
cat(" Modelo de variograma ajustado:\n")
cat("=====================================\n")
print(modelo_gauss)


# ==== Gráfico del variograma ====

linea_modelo <- variogramLine(
  modelo_gauss,
  maxdist = max(variograma_gauss$dist, na.rm = TRUE)
)

grafico_variograma <- ggplot() +
  geom_point(
    data = as.data.frame(variograma_gauss),
    aes(x = dist, y = gamma),
    color = "blue",
    size = 2.5
  ) +
  geom_line(
    data = as.data.frame(linea_modelo),
    aes(x = dist, y = gamma),
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Variograma Experimental con Ajuste",
    subtitle = "Variable gaussianizada",
    x = "Distancia (m)",
    y = "Semivarianza γ(h)"
  ) +
  theme_gray() +   # Tema gris por defecto
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(colour = "black", size = 10)
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

cat("\nKriging simple completado exitosamente.\n")


# ==== Crear rasters de kriging simple ====

r_pred <- r_template
r_var  <- r_template

values(r_pred) <- simple_kriging$var1.pred
values(r_var)  <- simple_kriging$var1.var

names(r_pred) <- paste0(nombre_variable, "_SK_pred_gauss")
names(r_var)  <- paste0(nombre_variable, "_SK_var_gauss")


# ==== Preparar datos para gráficos ====

sk_pred_df <- as.data.frame(r_pred, xy = TRUE, na.rm = TRUE)
sk_var_df  <- as.data.frame(r_var, xy = TRUE, na.rm = TRUE)

nombre_capa_pred <- names(r_pred)
nombre_capa_var  <- names(r_var)


# ==== Visualización predicción SK ====

grafico_SK_pred <- ggplot(
  sk_pred_df,
  aes(
    x = x,
    y = y,
    fill = .data[[nombre_capa_pred]]
  )
) +
  geom_raster() +
  scale_fill_viridis_c(name = "Predicción SK") +
  coord_sf(
    crs = st_crs(data_sf),
    expand = FALSE
  ) +
  labs(
    title = "Predicción por Kriging Simple",
    subtitle = "Variable gaussianizada N(0,1)",
    x = nombre_x,
    y = nombre_y
  ) +
  theme_gray() +   # Tema gris por defecto
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(colour = "black", size = 10)
  )

print(grafico_SK_pred)


# ==== Visualización varianza SK ====

grafico_SK_var <- ggplot(
  sk_var_df,
  aes(
    x = x,
    y = y,
    fill = .data[[nombre_capa_var]]
  )
) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", name = "Varianza SK") +
  coord_sf(
    crs = st_crs(data_sf),
    expand = FALSE
  ) +
  labs(
    title = "Varianza de Kriging Simple",
    subtitle = "Variable gaussianizada N(0,1)",
    x = nombre_x,
    y = nombre_y
  ) +
  theme_gray() +   # Tema gris por defecto
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(colour = "black", size = 10)
  )

print(grafico_SK_var)


# ==== Guardar modelos y objetos ====

if (guardar_modelos) {
  
  dir_modelos <- "modelos"
  
  if (!dir.exists(dir_modelos)) {
    dir.create(dir_modelos, recursive = TRUE)
  }
  
  ruta_anamorfosis        <- file.path(dir_modelos, paste0("045_", nombre_dataset, "_", nombre_variable, "_anamorfosis.rds"))
  ruta_variograma        <- file.path(dir_modelos, paste0("045_", nombre_dataset, "_", nombre_variable, "_variograma_gaussiano.rds"))
  ruta_modelo_variograma <- file.path(dir_modelos, paste0("045_", nombre_dataset, "_", nombre_variable, "_modelo_variograma_gaussiano.rds"))
  ruta_rejilla           <- file.path(dir_modelos, paste0("045_", nombre_dataset, "_", nombre_variable, "_rejilla.rds"))
  ruta_datos_gauss       <- file.path(dir_modelos, paste0("045_", nombre_dataset, "_", nombre_variable, "_datos_gaussianizados.rds"))
  
  saveRDS(data_sf, ruta_datos_gauss)
  saveRDS(tabla_anamorfosis, ruta_anamorfosis)
  saveRDS(variograma_gauss,  ruta_variograma)
  saveRDS(modelo_gauss,      ruta_modelo_variograma)
  saveRDS(r_template,        ruta_rejilla)
  
  cat("\n=====================================\n")
  cat(" Objetos guardados exitosamente en:\n")
  cat("=====================================\n")
  cat("Anamorfosis:\n",        ruta_anamorfosis,        "\n")
  cat("Variograma:\n",         ruta_variograma,         "\n")
  cat("Modelo variograma:\n",  ruta_modelo_variograma, "\n")
  cat("Plantilla raster:\n",   ruta_rejilla,           "\n\n")
}


# ==== Guardar rasters ====

if (guardar_rasters) {
  
  dir_kriging <- "salidas/kriging"
  
  if (!dir.exists(dir_kriging)) {
    dir.create(dir_kriging, recursive = TRUE)
  }
  
  nombre_raster_pred <- file.path(dir_kriging, paste0("045_", nombre_dataset, "_", nombre_variable, "_SK_prediccion_gaussiana.tif"))
  nombre_raster_var  <- file.path(dir_kriging, paste0("045_", nombre_dataset, "_", nombre_variable, "_SK_varianza_gaussiana.tif"))
  
  writeRaster(r_pred, nombre_raster_pred, overwrite = TRUE)
  writeRaster(r_var,  nombre_raster_var,  overwrite = TRUE)
  
  cat("=====================================\n")
  cat(" Rasters guardados exitosamente en:\n")
  cat("=====================================\n")
  cat("Predicción SK:\n", nombre_raster_pred, "\n")
  cat("Varianza SK:\n",   nombre_raster_var,  "\n\n")
}


# ==== Guardar figuras ====

if (guardar_figuras) {
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  figuras <- list(
    anamorfosis             = grafico_anamorfosis,
    variograma_gaussiano    = grafico_variograma,
    SK_prediccion_gaussiana = grafico_SK_pred,
    SK_varianza_gaussiana   = grafico_SK_var
  )
  
  for (nombre_fig in names(figuras)) {
    
    nombre_salida <- file.path(
      dir_figuras,
      paste0("045_", nombre_dataset, "_", nombre_variable, "_", nombre_fig, ".", formato_figura)
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
        height = alto_figura
      )
      
    } else {
      stop("Formato no soportado. Usa 'png' o 'pdf'.")
    }
    
    cat("Figura guardada:", nombre_salida, "\n")
  }
}