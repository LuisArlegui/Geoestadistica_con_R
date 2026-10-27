#####################################################
#
#  036. Kriging ordinario (OK)
#
#  Descripción:
#  Realiza una interpolación mediante kriging ordinario
#  a partir de datos puntuales georreferenciados. Calcula
#  el variograma experimental, ajusta un modelo variográfico
#  y predice la variable sobre los centros de las celdas de
#  una plantilla raster creada con terra.
#
#  La utilización directa de una plantilla SpatRaster evita
#  problemas de orientación, transposición o giro al exportar
#  los resultados a GeoTIFF.
#
#  Entrada:
#  - datos/brutos/wolfcamp.csv o datos/procesados/fichero.csv
#
#  Salida:
#  - mapa interactivo de puntos en el visor
#  - salidas/kriging/036_dataset_variable_OK_prediccion.tif
#  - salidas/kriging/036_dataset_variable_OK_varianza.tif
#  - (opcional) salidas/figuras/036_dataset_variable_OK_prediccion.png o .pdf
#  - (opcional) salidas/figuras/036_dataset_variable_OK_varianza.png o .pdf
#
#####################################################


# ==== Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("sf", "gstat", "terra", "mapview", "ggplot2", "viridis")

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

ruta_datos <- "datos/procesados/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(
  ruta_datos,
  sep = ","
)

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ==== Parámetros a editar ====

nombre_x        <- "coord_X"                     # <-- EDITAR AQUÍ
nombre_y        <- "coord_Y"                     # <-- EDITAR AQUÍ
nombre_variable <- "variable"                    # <-- EDITAR AQUÍ

epsg_datos <- 12345                        # <-- EDITAR AQUÍ


# Variograma experimental
cutoff_variograma <- 125000
lag_variograma    <- 9000                 # NULL = gstat decide; o valor numérico


# Modelo inicial de variograma
pSill             <- 2500
modelo_variograma <- "Sph"                 # "Exp", "Sph", "Gau", "Bes", "Mat", etc.
rango             <- 100000
nugget            <- 1000


# Plantilla raster
tam_pixel   <- 2500                        # Unidades del CRS (metros)
margen_bbox <- 0.10                        # Proporción añadida a cada lado del bbox


# Recorte espacial opcional
usar_envolvente_convexa <- TRUE
buffer_envolvente       <- 100            # Unidades del CRS (metros)


# Opciones de visualización y ajuste
tipo_mapa_base     <- "OpenStreetMap"      # Mapa base libre de API Key
mostrar_mapa       <- TRUE
ajustar_variograma <- TRUE


# Guardado
guardar_rasters <- TRUE
guardar_figuras <- TRUE

formato_figura <- "pdf"                    # "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300                        # Solo para PNG


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

if (cutoff_variograma <= 0) {
  stop("cutoff_variograma debe ser mayor que cero.")
}

if (!is.null(lag_variograma) && lag_variograma <= 0) {
  stop("lag_variograma debe ser NULL o mayor que cero.")
}

if (tam_pixel <= 0) {
  stop("tam_pixel debe ser mayor que cero.")
}

if (margen_bbox < 0) {
  stop("margen_bbox no puede ser negativo.")
}

if (buffer_envolvente < 0) {
  stop("buffer_envolvente no puede ser negativo.")
}


# Eliminar filas incompletas
data_plot <- data[
  complete.cases(data[, vars]),
  vars,
  drop = FALSE
]

if (nrow(data_plot) < 3) {
  stop("Se necesitan al menos tres observaciones válidas para aplicar kriging.")
}


# ==== Conversión a sf ====

data_sf <- st_as_sf(
  data_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)

if (st_is_longlat(data_sf)) {
  stop(
    paste(
      "El CRS indicado es geográfico.",
      "Para calcular distancias y generar el raster debe utilizarse un CRS proyectado."
    )
  )
}


# ==== Mapa rápido (Mapview sin API Key) ====

if (mostrar_mapa) {
  mapviewOptions(
    basemaps = tipo_mapa_base,
    basemaps.color.shuffle = FALSE
  )
  
  print(
    mapview(
      data_sf,
      zcol = nombre_variable,
      layer.name = nombre_variable
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
cat(" Modelo inicial:\n")
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
cat(" Modelo final usado en Kriging:\n")
cat("=====================================\n")
print(modelo_final)


# ==== Extensión de predicción ====

bbox <- st_bbox(data_sf)

ancho_bbox <- as.numeric(bbox["xmax"] - bbox["xmin"])
alto_bbox  <- as.numeric(bbox["ymax"] - bbox["ymin"])

xmin <- as.numeric(bbox["xmin"] - ancho_bbox * margen_bbox)
xmax <- as.numeric(bbox["xmax"] + ancho_bbox * margen_bbox)
ymin <- as.numeric(bbox["ymin"] - alto_bbox * margen_bbox)
ymax <- as.numeric(bbox["ymax"] + alto_bbox * margen_bbox)


# Plantilla raster

extension_prediccion <- ext(
  xmin,
  xmax,
  ymin,
  ymax
)

r_template <- rast(
  ext = extension_prediccion,
  resolution = tam_pixel,
  crs = st_crs(data_sf)$wkt
)


# ==== Centros de las celdas ====

# as.points() devuelve los centros en el mismo orden interno
# en que terra almacena posteriormente los valores del raster.

grid_vect <- as.points(
  r_template,
  values = FALSE,
  na.rm = FALSE
)

grid_sf <- st_as_sf(grid_vect)

coords_grid <- st_coordinates(grid_sf)

grid_sf$coord_x <- coords_grid[, 1]
grid_sf$coord_y <- coords_grid[, 2]

cat("\nRejilla de predicción creada a partir del raster:\n")
cat("Número de puntos:", nrow(grid_sf), "\n")


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

cat("\nKriging ordinario completado exitosamente.\n")


# ==== Crear rasters de resultados ====

# Los resultados de predict() mantienen el mismo orden que
# los puntos de grid_sf y, por tanto, que las celdas de
# r_template. No es necesario ordenar, trasponer ni girar.

r_pred <- r_template
r_var  <- r_template

values(r_pred) <- kpred$var1.pred
values(r_var)  <- kpred$var1.var

names(r_pred) <- paste0(
  nombre_variable,
  "_OK_pred"
)

names(r_var) <- paste0(
  nombre_variable,
  "_OK_var"
)


# ==== Recorte opcional por envolvente convexa ====

if (usar_envolvente_convexa) {
  
  envolvente_sf <- st_convex_hull(
    st_union(data_sf)
  )
  
  if (buffer_envolvente > 0) {
    envolvente_sf <- st_buffer(
      envolvente_sf,
      dist = buffer_envolvente
    )
  }
  
  envolvente_vect <- vect(envolvente_sf)
  
  r_pred <- mask(r_pred, envolvente_vect)
  r_var  <- mask(r_var, envolvente_vect)
}


# ==== Comprobación de orientación ====

cat("\nExtensión del raster de predicción:\n")
print(ext(r_pred))

cat("\nCRS del raster:\n")
print(crs(r_pred, proj = TRUE))


# ==== Preparar datos para los gráficos ====

pred_df <- as.data.frame(
  r_pred,
  xy = TRUE,
  na.rm = TRUE
)

var_df <- as.data.frame(
  r_var,
  xy = TRUE,
  na.rm = TRUE
)

colnames(pred_df)[3] <- "prediccion"
colnames(var_df)[3]  <- "varianza"


# ==== Visualización predicción ====

grafico_pred <- ggplot(
  pred_df,
  aes(
    x = .data[["x"]],
    y = .data[["y"]],
    fill = .data[["prediccion"]]
  )
) +
  geom_raster() +
  scale_fill_viridis_c(name = "Predicción") +
  labs(
    title = "Predicciones por Kriging Ordinario",
    x = nombre_x,
    y = nombre_y
  ) +
  coord_sf(
    crs = st_crs(data_sf),
    expand = FALSE
  ) +
  theme_gray() +   # Tema gris por defecto
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0.5,
      color = "black"
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text = element_text(
      colour = "black",
      size = 10
    )
  )

print(grafico_pred)


# ==== Visualización varianza ====

grafico_var <- ggplot(
  var_df,
  aes(
    x = .data[["x"]],
    y = .data[["y"]],
    fill = .data[["varianza"]]
  )
) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", name = "Varianza") +
  labs(
    title = "Varianza de Kriging Ordinario",
    x = nombre_x,
    y = nombre_y
  ) +
  coord_sf(
    crs = st_crs(data_sf),
    expand = FALSE
  ) +
  theme_gray() +   # Tema gris por defecto
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0.5,
      color = "black"
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text = element_text(
      colour = "black",
      size = 10
    )
  )

print(grafico_var)


# ==== Guardar rasters ====

if (guardar_rasters) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_rasters    <- "salidas/kriging"
  
  if (!dir.exists(dir_rasters)) {
    dir.create(dir_rasters, recursive = TRUE)
  }
  
  nombre_raster_pred <- file.path(
    dir_rasters,
    paste0("036_", nombre_dataset, "_", nombre_variable, "_OK_prediccion.tif")
  )
  
  nombre_raster_var <- file.path(
    dir_rasters,
    paste0("036_", nombre_dataset, "_", nombre_variable, "_OK_varianza.tif")
  )
  
  writeRaster(r_pred, nombre_raster_pred, overwrite = TRUE)
  writeRaster(r_var, nombre_raster_var, overwrite = TRUE)
  
  cat("\n=====================================\n")
  cat(" Rasters guardados exitosamente en:\n")
  cat("=====================================\n")
  cat("Predicción:\n", nombre_raster_pred, "\n")
  cat("Varianza:\n", nombre_raster_var, "\n\n")
}


# ==== Guardar figuras ====

if (guardar_figuras) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_figuras    <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura_pred <- file.path(
    dir_figuras,
    paste0("036_", nombre_dataset, "_", nombre_variable, "_OK_prediccion.", formato_figura)
  )
  
  nombre_figura_var <- file.path(
    dir_figuras,
    paste0("036_", nombre_dataset, "_", nombre_variable, "_OK_varianza.", formato_figura)
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
      height = alto_figura
    )
    
    ggsave(
      filename = nombre_figura_var,
      plot = grafico_var,
      width = ancho_figura,
      height = alto_figura
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("=====================================\n")
  cat(" Figuras guardadas exitosamente en:\n")
  cat("=====================================\n")
  cat("Predicción:\n", nombre_figura_pred, "\n")
  cat("Varianza:\n", nombre_figura_var, "\n\n")
}