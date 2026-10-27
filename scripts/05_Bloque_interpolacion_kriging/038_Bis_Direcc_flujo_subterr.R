###############################################################
#
#  038Bis. Dirección del flujo subterráneo
#
#  Descripción:
#  Calcula la dirección potencial del flujo subterráneo a
#  partir de una superficie piezométrica interpolada mediante
#  kriging universal.
#
#  El flujo se obtiene a partir del gradiente hidráulico:
#
#      q ∝ -grad(h)
#
#  donde h es la cota piezométrica. El signo negativo indica
#  que el flujo se produce desde mayor hacia menor potencial
#  hidráulico.
#
#  El script calcula:
#  - derivada de h respecto de X
#  - derivada de h respecto de Y
#  - módulo del gradiente hidráulico
#  - azimut de la dirección de flujo
#  - vectores de flujo para representación cartográfica
#
#  Entrada:
#  - raster GeoTIFF de predicción generado por el script 038
#
#  Salidas:
#  - salidas/flujo/
#      038Bis_dataset_gradiente_hidraulico.tif
#      038Bis_dataset_azimut_flujo.tif
#      038Bis_dataset_flujo_vectores.shp
#  - salidas/figuras/
#      038Bis_dataset_mapa_flujo.pdf o .png
#
###############################################################


# ==== Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("terra", "sf", "ggplot2", "viridis")

instalar_si_falta <- function(pkgs) {
  nuevos <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(nuevos) > 0) {
    message("Instalando paquetes faltantes: ", paste(nuevos, collapse = ", "))
    install.packages(nuevos, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

instalar_si_falta(paquetes_requeridos)


# ==== Raster de entrada ====

ruta_raster <- "salidas/kriging/038_wolfcamp_gw_UK_prediccion.tif"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_raster)) {
  stop(paste("No se encuentra el raster:", ruta_raster))
}

r_h <- rast(ruta_raster)

cat("\nRaster cargado:\n")
print(r_h)


# ==== Parámetros a editar ====

# Densidad de flechas.
# Se representa una flecha cada N celdas.
salto_flechas <- 15            # <-- EDITAR AQUÍ

# Longitud visual relativa de las flechas
factor_flecha <- 0.3          # <-- EDITAR AQUÍ

# Guardado
guardar_rasters  <- TRUE
guardar_vectores <- TRUE
guardar_figura   <- TRUE

formato_figura <- "pdf"       # "png" o "pdf"

ancho_figura <- 9
alto_figura  <- 7
dpi_figura   <- 300           # Solo para PNG


# ==== Comprobaciones ====

if (nlyr(r_h) != 1) {
  stop("El raster de entrada debe tener una única capa.")
}

if (is.lonlat(r_h)) {
  stop(
    paste(
      "El raster está en coordenadas geográficas.",
      "Para calcular gradientes debe utilizarse un CRS proyectado."
    )
  )
}

if (salto_flechas < 1) {
  stop("salto_flechas debe ser mayor o igual que 1.")
}


# ==== Información espacial ====

res_x <- res(r_h)[1]
res_y <- res(r_h)[2]

cat("\nResolución X:", res_x, "\n")
cat("Resolución Y:", res_y, "\n")
cat("CRS:\n")
print(crs(r_h, proj = TRUE))


# ==== Derivadas espaciales ====

# Derivada respecto de X mediante diferencias centrales
mat_dx <- matrix(
  c(
    0, 0, 0,
    -1, 0, 1,
    0, 0, 0
  ),
  nrow = 3,
  byrow = TRUE
)

# Derivada respecto de Y
mat_dy <- matrix(
  c(
    0, 1, 0,
    0, 0, 0,
    0, -1, 0
  ),
  nrow = 3,
  byrow = TRUE
)

dh_dx <- focal(
  r_h,
  w = mat_dx,
  fun = sum,
  na.policy = "omit",
  fillvalue = NA
) / (2 * res_x)

dh_dy <- focal(
  r_h,
  w = mat_dy,
  fun = sum,
  na.policy = "omit",
  fillvalue = NA
) / (2 * res_y)

names(dh_dx) <- "dh_dx"
names(dh_dy) <- "dh_dy"


# ==== Componentes del flujo ====

# El flujo va hacia potencial decreciente: q ∝ -grad(h)
flujo_x <- -dh_dx
flujo_y <- -dh_dy

names(flujo_x) <- "flujo_x"
names(flujo_y) <- "flujo_y"


# ==== Módulo del gradiente hidráulico ====

gradiente_hidraulico <- sqrt(
  dh_dx^2 + dh_dy^2
)

names(gradiente_hidraulico) <- "gradiente_hidraulico"


# ==== Azimut del flujo ====

# atan2(x, y) permite obtener azimut geográfico:
# 0°   = Norte | 90° = Este | 180° = Sur | 270° = Oeste

azimut_flujo <- atan2(
  flujo_x,
  flujo_y
) * 180 / pi

azimut_flujo <- ifel(
  azimut_flujo < 0,
  azimut_flujo + 360,
  azimut_flujo
)

names(azimut_flujo) <- "azimut_flujo"


# ==== Preparar puntos para vectores ====

r_stack <- c(
  r_h,
  flujo_x,
  flujo_y,
  gradiente_hidraulico,
  azimut_flujo
)

# Asegurar nombres explícitos
names(r_stack) <- c(
  "h",
  "flujo_x",
  "flujo_y",
  "gradiente_hidraulico",
  "azimut_flujo"
)

puntos_vect <- as.points(
  r_stack,
  na.rm = TRUE
)

puntos_df <- as.data.frame(
  puntos_vect
)

coords_puntos <- terra::crds(
  puntos_vect
)

puntos_df$x <- coords_puntos[, 1]
puntos_df$y <- coords_puntos[, 2]

cat("\nColumnas disponibles en puntos_df:\n")
print(names(puntos_df))


# ==== Reducir densidad de flechas ====

# Espaciado espacial entre flechas
espaciado_x <- res_x * salto_flechas
espaciado_y <- res_y * salto_flechas

# Índices espaciales de los puntos
indice_x <- floor(
  (puntos_df$x - xmin(r_h)) / espaciado_x
)

indice_y <- floor(
  (puntos_df$y - ymin(r_h)) / espaciado_y
)

# Seleccionar un punto por bloque espacial
bloques <- paste(indice_x, indice_y, sep = "_")

puntos_df <- puntos_df[
  !duplicated(bloques),
  ,
  drop = FALSE
]


# ==== Comprobaciones ====

columnas_necesarias <- c(
  "x",
  "y",
  "flujo_x",
  "flujo_y",
  "gradiente_hidraulico",
  "azimut_flujo"
)

faltan <- columnas_necesarias[
  !columnas_necesarias %in% names(puntos_df)
]

if (length(faltan) > 0) {
  stop(
    paste(
      "Faltan estas columnas en puntos_df:",
      paste(faltan, collapse = ", ")
    )
  )
}


# ==== Dirección e intensidad de los vectores ====

# Módulo del vector de flujo
modulo_vector <- sqrt(
  puntos_df$flujo_x^2 +
    puntos_df$flujo_y^2
)

# Eliminar valores no válidos y gradientes nulos
validos <- is.finite(modulo_vector) &
  modulo_vector > 0

puntos_df <- puntos_df[
  validos,
  ,
  drop = FALSE
]

modulo_vector <- modulo_vector[
  validos
]


# ==== Vector unitario: únicamente dirección ====

puntos_df$ux <- puntos_df$flujo_x / modulo_vector
puntos_df$uy <- puntos_df$flujo_y / modulo_vector


# ==== Escalado del gradiente para longitud de flecha ====

# Se utiliza el percentil 95 como referencia para evitar que valores extremos dominen
gradiente_referencia <- quantile(
  puntos_df$gradiente_hidraulico,
  probs = 0.95,
  na.rm = TRUE
)

# Escala relativa entre 0 y 1
puntos_df$gradiente_relativo <- puntos_df$gradiente_hidraulico / gradiente_referencia

# Limitar los valores mayores que el percentil 95 a 1
puntos_df$gradiente_relativo <- pmin(
  puntos_df$gradiente_relativo,
  1
)


# ==== Longitud máxima de las flechas ====

longitud_maxima <- min(
  res_x,
  res_y
) * salto_flechas * factor_flecha


# Longitud individual proporcional al gradiente
puntos_df$longitud_flecha <- longitud_maxima * puntos_df$gradiente_relativo


# ==== Coordenadas finales de las flechas ====

puntos_df$x_fin <- puntos_df$x + puntos_df$ux * puntos_df$longitud_flecha
puntos_df$y_fin <- puntos_df$y + puntos_df$uy * puntos_df$longitud_flecha


# ==== Crear líneas de flujo ====

crear_linea <- function(x1, y1, x2, y2) {
  st_linestring(
    matrix(
      c(
        x1, y1,
        x2, y2
      ),
      ncol = 2,
      byrow = TRUE
    )
  )
}

lista_lineas <- mapply(
  crear_linea,
  puntos_df$x,
  puntos_df$y,
  puntos_df$x_fin,
  puntos_df$y_fin,
  SIMPLIFY = FALSE
)

flujo_sf <- st_sf(
  puntos_df[
    ,
    c(
      "gradiente_hidraulico",
      "azimut_flujo"
    )
  ],
  geometry = st_sfc(
    lista_lineas,
    crs = crs(r_h)
  )
)


# ==== Preparar raster para gráfico ====

h_df <- as.data.frame(
  r_h,
  xy = TRUE,
  na.rm = TRUE
)

colnames(h_df)[3] <- "h"


# ==== Mapa de flujo ====

grafico_flujo <- ggplot() +
  geom_raster(
    data = h_df,
    aes(
      x = .data[["x"]],
      y = .data[["y"]],
      fill = .data[["h"]]
    )
  ) +
  scale_fill_viridis_c(
    name = "Cota piezométrica"
  ) +
  geom_segment(
    data = puntos_df,
    aes(
      x = .data[["x"]],
      y = .data[["y"]],
      xend = .data[["x_fin"]],
      yend = .data[["y_fin"]]
    ),
    arrow = arrow(
      length = unit(0.12, "cm")
    ),
    linewidth = 0.4
  ) +
  coord_sf(
    crs = st_crs(crs(r_h)),
    expand = FALSE
  ) +
  labs(
    title = "Dirección Potencial del Flujo Subterráneo",
    subtitle = "Vectores calculados a partir del gradiente de la superficie piezométrica",
    x = "X",
    y = "Y"
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
      size = 11,
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

print(grafico_flujo)


# ==== Nombres de salida ====

nombre_dataset <- tools::file_path_sans_ext(
  basename(ruta_raster)
)

nombre_dataset <- sub(
  "_UK_prediccion$",
  "",
  nombre_dataset
)

# Limpiar posibles prefijos anteriores de script si los hubiera
nombre_dataset <- sub("^038_", "", nombre_dataset)

dir_flujo   <- "salidas/flujo"
dir_figuras <- "salidas/figuras"

if (!dir.exists(dir_flujo)) {
  dir.create(dir_flujo, recursive = TRUE)
}

if (!dir.exists(dir_figuras)) {
  dir.create(dir_figuras, recursive = TRUE)
}


# ==== Guardar rasters ====

if (guardar_rasters) {
  
  nombre_gradiente <- file.path(
    dir_flujo,
    paste0("038Bis_", nombre_dataset, "_gradiente_hidraulico.tif")
  )
  
  nombre_azimut <- file.path(
    dir_flujo,
    paste0("038Bis_", nombre_dataset, "_azimut_flujo.tif")
  )
  
  writeRaster(
    gradiente_hidraulico,
    nombre_gradiente,
    overwrite = TRUE
  )
  
  writeRaster(
    azimut_flujo,
    nombre_azimut,
    overwrite = TRUE
  )
  
  cat("\n=====================================\n")
  cat(" Rasters guardados exitosamente en:\n")
  cat("=====================================\n")
  cat("Gradiente hidráulico:\n", nombre_gradiente, "\n")
  cat("Azimut de flujo:\n", nombre_azimut, "\n\n")
}


# ==== Guardar vectores ====

if (guardar_vectores) {
  
  nombre_vectores <- file.path(
    dir_flujo,
    paste0(
      "038Bis_",
      nombre_dataset,
      "_flujo_vectores.gpkg"
    )
  )
  
  st_write(
    flujo_sf,
    nombre_vectores,
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
  cat("=====================================\n")
  cat(" Vectores guardados exitosamente en:\n")
  cat("=====================================\n")
  cat("Shapefile de vectores:\n", nombre_vectores, "\n\n")
}


# ==== Guardar figura ====

if (guardar_figura) {
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0("038Bis_", nombre_dataset, "_mapa_flujo.", formato_figura)
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico_flujo,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico_flujo,
      width = ancho_figura,
      height = alto_figura
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("=====================================\n")
  cat(" Figura guardada exitosamente en:\n")
  cat("=====================================\n")
  cat("Mapa de flujo:\n", nombre_figura, "\n\n")
}