#####################################################
#
#  037. Indicator kriging
#
#  Descripción:
#  Realiza kriging de indicadores a partir de una variable
#  continua transformada en una variable binaria según un
#  umbral. El valor interpolado se interpreta como una
#  estimación de la probabilidad espacial de que se cumpla
#  la condición definida.
#
#  El variograma se calcula sobre la variable indicadora.
#  La predicción se realiza sobre los centros de las celdas
#  de una plantilla raster generada con terra, evitando
#  problemas de orientación o transposición al exportar
#  los resultados a GeoTIFF.
#
#  Entrada:
#  - datos/brutos/wolfcamp.csv o
#  - datos/procesados/fichero.csv
#
#  Salidas:
#  - mapa interactivo de los datos
#  - salidas/kriging/037_dataset_variable_IK_probabilidad.tif
#  - salidas/kriging/037_dataset_variable_IK_varianza.tif
#  - salidas/kriging/037_dataset_variable_IK_clasificacion.tif
#  - (opcional) salidas/figuras/037_dataset_variable_IK_probabilidad.png o .pdf
#  - (opcional) salidas/figuras/037_dataset_variable_IK_varianza.png o .pdf
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

ruta_datos <- "datos/procesados/fichero_transf.csv"   # <-- EDITAR AQUÍ

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

nombre_x        <- "coord_x"                     # <-- EDITAR AQUÍ
nombre_y        <- "coord_y"                     # <-- EDITAR AQUÍ
nombre_variable <- "variable"                    # <-- EDITAR AQUÍ

epsg_datos <- 12345                        # <-- EDITAR AQUÍ


# Condición indicadora
umbral         <- 1234                     # <-- EDITAR AQUÍ: Umbral crítico
tipo_condicion <- "menor"                  # "menor" (< umbral) o "mayor" (> umbral)

nombre_indicador <- "indicador"


# Variograma experimental del indicador
cutoff_variograma <- 1300
lag_variograma    <- 60                 # NULL = gstat decide; o valor numérico


# Modelo inicial de variograma
pSill             <- 0.112
modelo_variograma <- "Sph"                 # "Exp", "Sph", "Gau", "Bes", "Mat", etc.
rango             <- 983.3
nugget            <- 0.0126


# Plantilla raster
tam_pixel   <- 10                        # Unidades del CRS (metros)
buffer_dist <- 50                        # Buffer de la envolvente convexa (metros)


# Clasificación probabilística
umbral_probabilidad <- 0.80                # p >= 0.80


# Opciones de visualización y ajuste
tipo_mapa_base     <- "OpenStreetMap"      # Mapa base libre de API Key
mostrar_mapa       <- TRUE
mostrar_rejilla    <- FALSE
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

if (!tipo_condicion %in% c("menor", "mayor")) {
  stop("tipo_condicion debe ser 'menor' o 'mayor'.")
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

if (buffer_dist < 0) {
  stop("buffer_dist no puede ser negativo.")
}

if (umbral_probabilidad < 0 || umbral_probabilidad > 1) {
  stop("umbral_probabilidad debe estar comprendido entre 0 y 1.")
}


# Eliminar filas incompletas
data_plot <- data[
  complete.cases(data[, vars]),
  vars,
  drop = FALSE
]

if (nrow(data_plot) < 3) {
  stop("Se necesitan al menos tres observaciones válidas para aplicar indicator kriging.")
}

cat("\nCoordenada X:", nombre_x, "\n")
cat("Coordenada Y:", nombre_y, "\n")
cat("Variable:", nombre_variable, "\n")
cat("EPSG:", epsg_datos, "\n")
cat("Observaciones válidas:", nrow(data_plot), "\n")
cat("Umbral:", umbral, "\n")
cat("Condición:", tipo_condicion, "\n")
cat("Tamaño de píxel:", tam_pixel, "m\n")


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

cat("\nProporción de observaciones que cumplen la condición:\n")
print(mean(data_sf[[nombre_indicador]], na.rm = TRUE))


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
cat(" Modelo inicial:\n")
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
cat(" Modelo final usado en Indicator Kriging:\n")
cat("=====================================\n")
print(modelo_final)


# ==== Envolvente de predicción ====

hull_sf <- st_convex_hull(
  st_union(data_sf)
)

if (buffer_dist > 0) {
  hull_buffer_sf <- st_buffer(
    hull_sf,
    dist = buffer_dist
  )
} else {
  hull_buffer_sf <- hull_sf
}

hull_buffer_vect <- vect(hull_buffer_sf)
bbox             <- st_bbox(hull_buffer_sf)


# ==== Plantilla raster ====

extension_prediccion <- ext(
  as.numeric(bbox["xmin"]),
  as.numeric(bbox["xmax"]),
  as.numeric(bbox["ymin"]),
  as.numeric(bbox["ymax"])
)

r_template <- rast(
  ext = extension_prediccion,
  resolution = tam_pixel,
  crs = st_crs(data_sf)$wkt
)

cat("\nPlantilla raster creada:\n")
cat("Número de filas:", nrow(r_template), "\n")
cat("Número de columnas:", ncol(r_template), "\n")
cat("Número de celdas:", ncell(r_template), "\n")
cat("Resolución X:", res(r_template)[1], "\n")
cat("Resolución Y:", res(r_template)[2], "\n")


# ==== Centros de las celdas ====

grid_vect <- as.points(
  r_template,
  values = FALSE,
  na.rm = FALSE
)

# Conservar únicamente los centros incluidos en la envolvente convexa + buffer
grid_vect <- grid_vect[hull_buffer_vect, ]
grid_sf   <- st_as_sf(grid_vect)

cat("\nRejilla de predicción creada:\n")
cat("Número de puntos:", nrow(grid_sf), "\n")


# ==== Visualización opcional de la rejilla ====

if (mostrar_rejilla) {
  mapviewOptions(basemaps = tipo_mapa_base)
  print(
    mapview(data_sf, zcol = nombre_variable, layer.name = nombre_variable) +
      mapview(grid_sf, color = "red", layer.name = "Puntos Rejilla") +
      mapview(hull_buffer_sf, color = "blue", alpha.regions = 0.1, layer.name = "Malla Buffer")
  )
}


# ==== Indicator kriging ====

ik <- krige(
  formula = formula_indicador,
  locations = data_sf,
  newdata = grid_sf,
  model = modelo_final
)

cat("\nIndicator kriging completado exitosamente.\n")


# ==== Preparar resultados ====

coords_ik <- st_coordinates(ik)

ik_df <- data.frame(
  X = coords_ik[, 1],
  Y = coords_ik[, 2],
  probabilidad = ik$var1.pred,
  varianza = ik$var1.var
)

# El kriging de indicadores puede generar pequeñas extrapolaciones fuera de [0,1]
# Comprobar predicciones fuera de [0,1]

n_fuera <- sum(
  ik_df$probabilidad < 0 |
    ik_df$probabilidad > 1,
  na.rm = TRUE
)

cat(
  "\nPredicciones fuera del intervalo [0,1]:",
  n_fuera,
  "\n"
)

if (n_fuera > 0) {
  warning(
    "Se han obtenido ", n_fuera,
    " predicciones fuera de [0,1]. ",
    "Se recortarán para generar el mapa de probabilidades."
  )
}

ik_df$probabilidad <- pmin(pmax(ik_df$probabilidad, 0), 1)
ik_df$clasificacion <- as.integer(ik_df$probabilidad >= umbral_probabilidad)

cat("\nResumen de probabilidades:\n")
print(summary(ik_df$probabilidad))

cat("\nResumen de clasificación (Probabilidad >= ", umbral_probabilidad, "):\n", sep = "")
print(table(ik_df$clasificacion))


# ==== Crear objeto espacial de resultados ====

ik_result_sf <- st_as_sf(
  ik_df,
  coords = c("X", "Y"),
  crs = st_crs(data_sf),
  remove = FALSE
)

ik_result_vect <- vect(ik_result_sf)


# ==== Crear rasters ====

r_prob <- rasterize(ik_result_vect, r_template, field = "probabilidad")
r_var  <- rasterize(ik_result_vect, r_template, field = "varianza")
r_clas <- rasterize(ik_result_vect, r_template, field = "clasificacion")

# Recortar exactamente por la envolvente
r_prob <- mask(r_prob, hull_buffer_vect)
r_var  <- mask(r_var, hull_buffer_vect)
r_clas <- mask(r_clas, hull_buffer_vect)

names(r_prob) <- paste0(nombre_variable, "_IK_prob")
names(r_var)  <- paste0(nombre_variable, "_IK_var")
names(r_clas) <- paste0(nombre_variable, "_IK_clas")


# ==== Comprobación espacial ====

cat("\nExtensión del raster de probabilidad:\n")
print(ext(r_prob))

cat("\nCRS del raster:\n")
print(crs(r_prob, proj = TRUE))


# ==== Preparar datos para gráficos ====

prob_df <- as.data.frame(r_prob, xy = TRUE, na.rm = TRUE)
var_df  <- as.data.frame(r_var, xy = TRUE, na.rm = TRUE)

colnames(prob_df)[3] <- "probabilidad"
colnames(var_df)[3]  <- "varianza"


# ==== Visualización probabilidad ====

grafico_prob <- ggplot(
  prob_df,
  aes(
    x = .data[["x"]],
    y = .data[["y"]],
    fill = .data[["probabilidad"]]
  )
) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Probabilidad") +
  labs(
    title = "Indicator Kriging: Probabilidad Espacial",
    subtitle = texto_condicion,
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
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      face = "italic",
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

print(grafico_prob)


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
    title = "Indicator Kriging: Varianza",
    subtitle = texto_condicion,
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
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      face = "italic",
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
  
  nombre_raster_prob <- file.path(dir_rasters, paste0("037_", nombre_dataset, "_", nombre_variable, "_IK_probabilidad.tif"))
  nombre_raster_var  <- file.path(dir_rasters, paste0("037_", nombre_dataset, "_", nombre_variable, "_IK_varianza.tif"))
  nombre_raster_clas <- file.path(dir_rasters, paste0("037_", nombre_dataset, "_", nombre_variable, "_IK_clasificacion.tif"))
  
  writeRaster(r_prob, nombre_raster_prob, overwrite = TRUE)
  writeRaster(r_var, nombre_raster_var, overwrite = TRUE)
  writeRaster(r_clas, nombre_raster_clas, overwrite = TRUE)
  
  cat("\n=====================================\n")
  cat(" Rasters guardados exitosamente en:\n")
  cat("=====================================\n")
  cat("Probabilidad:\n", nombre_raster_prob, "\n")
  cat("Varianza:\n", nombre_raster_var, "\n")
  cat("Clasificación:\n", nombre_raster_clas, "\n\n")
}


# ==== Guardar figuras ====

if (guardar_figuras) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_figuras    <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura_prob <- file.path(dir_figuras, paste0("037_", nombre_dataset, "_", nombre_variable, "_IK_probabilidad.", formato_figura))
  nombre_figura_var  <- file.path(dir_figuras, paste0("037_", nombre_dataset, "_", nombre_variable, "_IK_varianza.", formato_figura))
  
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
  cat("Probabilidad:\n", nombre_figura_prob, "\n")
  cat("Varianza:\n", nombre_figura_var, "\n\n")
}