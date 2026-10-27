#####################################################
#
#  038. Universal kriging
#
#  Descripción:
#  Realiza una interpolación mediante kriging universal,
#  incorporando una tendencia espacial en función de las
#  coordenadas X e Y centradas y escaladas. El variograma
#  se calcula sobre los residuos del modelo de tendencia y
#  se utiliza después para interpolar la variable objetivo.
#
#  La rejilla de predicción se genera a partir de una
#  plantilla SpatRaster de terra, y las predicciones se
#  realizan sobre los centros de sus celdas. Este procedimiento
#  evita problemas de orientación, transposición o giro al
#  exportar los resultados a GeoTIFF.
#
#  Entrada:
#  - datos/brutos/wolfcamp.csv o datos/procesados/fichero.csv
#
#  Salidas:
#  - salidas/kriging/038_dataset_variable_UK_prediccion.tif
#  - salidas/kriging/038_dataset_variable_UK_varianza.tif
#  - (opcional) salidas/figuras/038_dataset_variable_UK_prediccion.png o .pdf
#  - (opcional) salidas/figuras/038_dataset_variable_UK_varianza.png o .pdf
#
#####################################################


# ==== Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("sf", "gstat", "terra", "ggplot2", "viridis")

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

ruta_datos <- "datos/brutos/fichero.csv"   # <-- EDITAR AQUÍ

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

epsg_datos <- 12345                        # <-- EDITAR AQUÍ: Código EPSG (ej. 32613 = UTM zone 13N)

# Tendencia espacial
orden_tendencia <- 2      # 1 = plano, 2 = superficie cuadrática, 3 = superficie cúbica

# Variograma residual
usar_maxlag_automatico <- TRUE

cutoff_variograma <- NULL # Si usar_maxlag_automatico = FALSE, introducir valor numérico
numero_lags       <- 15


# Modelo inicial de variograma residual
pSill             <- 12000
modelo_variograma <- "Sph"
rango             <- 45000
nugget            <- 1500

fit_method <- 2


# Plantilla raster
tam_pixel   <- 2000                        # Unidades del CRS (metros)
margen_bbox <- 0.10                        # Proporción añadida al bbox


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

if (numero_lags <= 0) {
  stop("numero_lags debe ser mayor que cero.")
}

if (!orden_tendencia %in% c(1, 2, 3)) {
  stop("orden_tendencia debe ser 1, 2 o 3.")
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
  stop("Se necesitan al menos tres observaciones válidas para aplicar kriging universal.")
}

cat("\nCoordenada X:", nombre_x, "\n")
cat("Coordenada Y:", nombre_y, "\n")
cat("Variable:", nombre_variable, "\n")
cat("EPSG:", epsg_datos, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")
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


# ==== Coordenadas centradas y escaladas para la tendencia ====

coords_data <- st_coordinates(data_sf)

# Centro de las coordenadas originales
centro_x <- mean(coords_data[, 1])
centro_y <- mean(coords_data[, 2])

# Escala común para mejorar la estabilidad numérica
escala_xy <- max(
  diff(range(coords_data[, 1])),
  diff(range(coords_data[, 2]))
)

if (!is.finite(escala_xy) || escala_xy <= 0) {
  stop("No existe extensión espacial suficiente para calcular la escala.")
}

# Coordenadas centradas y escaladas en data_sf
data_sf$coord_x <- (coords_data[, 1] - centro_x) / escala_xy
data_sf$coord_y <- (coords_data[, 2] - centro_y) / escala_xy


# ==== Definir fórmula de tendencia ====

if (orden_tendencia == 1) {
  formula_uk <- as.formula(
    paste(nombre_variable, "~ coord_x + coord_y")
  )
} else if (orden_tendencia == 2) {
  formula_uk <- as.formula(
    paste(
      nombre_variable,
      "~ coord_x + coord_y + I(coord_x^2) + I(coord_y^2) + I(coord_x * coord_y)"
    )
  )
} else if (orden_tendencia == 3) {
  formula_uk <- as.formula(
    paste(
      nombre_variable,
      "~ coord_x + coord_y + I(coord_x^2) + I(coord_y^2) + I(coord_x * coord_y) +",
      "I(coord_x^3) + I(coord_y^3) + I(coord_x^2 * coord_y) + I(coord_x * coord_y^2)"
    )
  )
}

cat("\nOrden de la tendencia espacial:", orden_tendencia, "\n")
cat("Fórmula utilizada:\n")
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

cat("\nCutoff usado:", cutoff_usado, "m\n")
cat("Lag usado:", lag_usado, "m\n")


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
cat(" Modelo inicial:\n")
cat("=====================================\n")
print(modelo_inicial)

modelo_ajustado <- fit.variogram(
  variograma_residual,
  modelo_inicial,
  fit.method = fit_method
)

cat("\n=====================================\n")
cat(" Modelo ajustado:\n")
cat("=====================================\n")
print(modelo_ajustado)


# ==== Extensión de predicción ====

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


# ==== Plantilla raster ====

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

grid_sf <- st_as_sf(grid_vect)

# Coordenadas de la rejilla aplicándoles EL MISMO CENTRADO Y ESCALADO
coords_grid <- st_coordinates(grid_sf)

grid_sf$coord_x <- (coords_grid[, 1] - centro_x) / escala_xy
grid_sf$coord_y <- (coords_grid[, 2] - centro_y) / escala_xy

cat("\nRejilla de predicción creada (con coordenadas centradas y escaladas):\n")
cat("Número de puntos:", nrow(grid_sf), "\n")


# ==== Universal kriging ====

kpred <- krige(
  formula = formula_uk,
  locations = data_sf,
  model = modelo_ajustado,
  newdata = grid_sf
)

cat("\nUniversal kriging completado exitosamente.\n")


# ==== Crear rasters de resultados ====

r_pred <- r_template
r_var  <- r_template

values(r_pred) <- kpred$var1.pred
values(r_var)  <- kpred$var1.var

names(r_pred) <- paste0(
  nombre_variable,
  "_UK_pred"
)

names(r_var) <- paste0(
  nombre_variable,
  "_UK_var"
)


# ==== Comprobación espacial ====

cat("\nExtensión del raster de predicción:\n")
print(ext(r_pred))

cat("\nCRS del raster:\n")
print(crs(r_pred, proj = TRUE))


# ==== Preparar datos para gráficos ====

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
    title = "Predicción por Kriging Universal",
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
    title = "Varianza de Kriging Universal",
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
    paste0("038_", nombre_dataset, "_", nombre_variable, "_UK_prediccion.tif")
  )
  
  nombre_raster_var <- file.path(
    dir_rasters,
    paste0("038_", nombre_dataset, "_", nombre_variable, "_UK_varianza.tif")
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
    paste0("038_", nombre_dataset, "_", nombre_variable, "_UK_prediccion.", formato_figura)
  )
  
  nombre_figura_var <- file.path(
    dir_figuras,
    paste0("038_", nombre_dataset, "_", nombre_variable, "_UK_varianza.", formato_figura)
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