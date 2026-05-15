#####################################################
#
#  022. Postplot
#
#  Descripción:
#  Representa datos puntuales georreferenciados mediante
#  símbolos proporcionales al valor de una variable numérica,
#  tanto en un mapa estático con ggplot2 como, opcionalmente,
#  en un mapa interactivo con mapview.
#
#####################################################



# Cargar librerías ====

if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("mapview", quietly = TRUE)) install.packages("mapview")
if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets")

library(sf)
library(ggplot2)
library(mapview)
library(htmlwidgets)



# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

nombre_x <- "coordx"
nombre_y <- "coordy"
nombre_variable <- "variable"

epsg_datos <- 12345  # <-- EDITAR AQUÍ código EPSG de las coordenadas en el dataset

titulo_mapa <- "Título del mapa"
subtitulo_mapa <- ""
etiqueta_leyenda <- "Variable"

color_puntos <- "blue"
alpha_puntos <- 0.7
rango_tamano <- c(1, 8)

# ⚠️ Basemap
tipo_mapa <- "CartoDB.Voyager"   # <-- ahora sí funciona correctamente

# Interactividad
mostrar_mapa_interactivo <- TRUE
guardar_mapa_html <- FALSE

# Guardado figura
guardar_figura <- TRUE
formato_figura <- "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300



# Comprobaciones ====

if (!all(c(nombre_x, nombre_y, nombre_variable) %in% colnames(data))) {
  stop("Alguna de las columnas especificadas no existe en el dataset.")
}

data_plot <- data[
  complete.cases(data[, c(nombre_x, nombre_y, nombre_variable)]),
]

if (nrow(data_plot) == 0) {
  stop("No hay datos válidos.")
}



# Conversión a sf ====

data_sf <- st_as_sf(
  data_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)



# MAPVIEW: fijar basemap correctamente ====

mapviewOptions(
  basemaps = tipo_mapa,
  basemaps.color.shuffle = FALSE
)



# Mapa estático ====

grafico <- ggplot(data_sf) +
  geom_sf(
    aes_string(size = nombre_variable),
    color = color_puntos,
    alpha = alpha_puntos
  ) +
  scale_size_continuous(
    name = etiqueta_leyenda,
    range = rango_tamano
  ) +
  labs(
    title = titulo_mapa,
    subtitle = subtitulo_mapa
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    legend.title = element_text(face = "bold")
  )

print(grafico)



# Guardar figura ====

if (guardar_figura) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) dir.create(dir_figuras, recursive = TRUE)
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0(nombre_dataset, "_", nombre_variable, "_postplot.", formato_figura)
  )
  
  if (formato_figura == "png") {
    ggsave(nombre_figura, grafico, width = ancho_figura, height = alto_figura, dpi = dpi_figura)
  } else {
    ggsave(nombre_figura, grafico, width = ancho_figura, height = alto_figura, device = cairo_pdf)
  }
  
  cat("\n=====================================\n")
  cat(" Figura guardada\n")
  cat("=====================================\n")
  cat(nombre_figura, "\n\n")
}



# Mapa interactivo ====

if (mostrar_mapa_interactivo || guardar_mapa_html) {
  
  data_sf$size_plot <- data_sf[[nombre_variable]]
  
  mapa_interactivo <- mapview(
    data_sf,
    zcol = nombre_variable,
    cex = "size_plot",
    layer.name = "Puntos"
  )
  
  if (mostrar_mapa_interactivo) print(mapa_interactivo)
  
  if (guardar_mapa_html) {
    
    dir_html <- "salidas/html"
    if (!dir.exists(dir_html)) dir.create(dir_html, recursive = TRUE)
    
    nombre_html <- file.path(
      dir_html,
      paste0(nombre_dataset, "_", nombre_variable, "_postplot.html")
    )
    
    htmlwidgets::saveWidget(mapa_interactivo@map, nombre_html, selfcontained = TRUE)
    
    cat("\n=====================================\n")
    cat(" Mapa HTML guardado\n")
    cat("=====================================\n")
    cat(nombre_html, "\n\n")
  }
}
