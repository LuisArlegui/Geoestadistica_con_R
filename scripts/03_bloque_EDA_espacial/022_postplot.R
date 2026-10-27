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
#  Entrada:
#  - datos/brutos/wolfcamp.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - salidas/figuras/022_dataset_variable_postplot.png / .pdf
#  - (opcional) salidas/html/022_dataset_variable_postplot.html
#
#####################################################


# Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("sf", "ggplot2", "mapview", "htmlwidgets", "webshot")

instalar_si_falta <- function(pkgs) {
  nuevos <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(nuevos) > 0) {
    message("Instalando paquetes faltantes: ", paste(nuevos, collapse = ", "))
    install.packages(nuevos, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

instalar_si_falta(paquetes_requeridos)


# Archivo de datos ====

ruta_datos <- "datos/procesados/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ⚙️ PARÁMETROS A EDITAR ====

nombre_x        <- "coord_X"         # <-- EDITAR AQUÍ
nombre_y        <- "coord_Y"         # <-- EDITAR AQUÍ
nombre_variable <- "variable"        # <-- EDITAR AQUÍ

epsg_datos <- 12345  # <-- EDITAR AQUÍ: Código EPSG de las coordenadas (ej. 32613 = UTM zone 13N)

titulo_mapa      <- "Título del mapa"     # <-- EDITAR AQUÍ
subtitulo_mapa   <- "Distribución espacial de valores" # <-- EDITAR AQUÍ
etiqueta_leyenda <- "Valor"               # <-- EDITAR AQUÍ

color_puntos <- "steelblue" # <-- EDITAR AQUÍ
alpha_puntos <- 0.7         # <-- EDITAR AQUÍ
rango_tamano <- c(1, 8)     # Tamaño min y max de los puntos

# ⚠️ Mapa base interactivo

tipo_mapa <- "OpenStreetMap"

# Otras alternativas libres y gratuitas que tampoco requieren API Key:
# tipo_mapa <- "Esri.WorldImagery" # (Ortofoto / Imagen de satélite)
# tipo_mapa <- "Esri.WorldTopoMap" #(Mapa topográfico suave)

# Interactividad
mostrar_mapa_interactivo <- TRUE
guardar_mapa_html        <- TRUE

# Guardado figura estática
guardar_figura <- TRUE
formato_figura <- "pdf"   # "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300


# Comprobaciones ====

if (!all(c(nombre_x, nombre_y, nombre_variable) %in% colnames(data))) {
  stop("Alguna de las columnas especificadas (X, Y o Variable) no existe en el dataset.")
}

if (!is.numeric(data[[nombre_variable]])) {
  stop(paste("La variable", shQuote(nombre_variable), "debe ser numérica."))
}

# Filtrar datos completos
data_plot <- data[complete.cases(data[, c(nombre_x, nombre_y, nombre_variable)]), , drop = FALSE]

if (nrow(data_plot) == 0) {
  stop("No quedan observaciones válidas tras eliminar valores nulos (NA).")
}

cat("\nVariable analizada:", nombre_variable, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")


# Conversión a objeto espacial (sf) ====

data_sf <- st_as_sf(
  data_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)


# Configuración de mapa base interactivo ====

mapviewOptions(
  basemaps = tipo_mapa,
  basemaps.color.shuffle = FALSE
)


# Mapa estático (ggplot2) ====

grafico <- ggplot(data_sf) +
  geom_sf(
    aes(
      size = .data[[nombre_variable]]
    ),
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
  theme_bw() +   # Tema base estructurado para coordenadas espacializadas
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      color = "black"
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      face = "italic",
      color = "gray30"
    ),
    axis.text = element_text(
      colour = "black",
      size = 9
    ),
    legend.title = element_text(
      face = "bold"
    )
  )

# Mostrar mapa estático en ventana
print(grafico)


# Guardar figura estática ====

if (guardar_figura) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) dir.create(dir_figuras, recursive = TRUE)
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0("022_", nombre_dataset, "_", nombre_variable, "_postplot.", formato_figura)
  )
  
  if (formato_figura == "png") {
    ggsave(
      filename = nombre_figura,
      plot = grafico,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
  } else if (formato_figura == "pdf") {
    ggsave(
      filename = nombre_figura,
      plot = grafico,
      width = ancho_figura,
      height = alto_figura
    )
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("\n-------------------------------------\n")
  cat(" Figura guardada exitosamente en:\n")
  cat(nombre_figura, "\n")
}


# Mapa interactivo (mapview) ====

if (mostrar_mapa_interactivo || guardar_mapa_html) {
  
  # Crear mapa interactivo
  mapa_interactivo <- mapview(
    data_sf,
    zcol = nombre_variable,
    cex = nombre_variable,
    layer.name = etiqueta_leyenda
  )
  
  # Mostrar en visor interactivo de RStudio/Navegador
  if (mostrar_mapa_interactivo) {
    print(mapa_interactivo)
  }
  
  # Exportar a archivo HTML interactivo
  if (guardar_mapa_html) {
    
    nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
    dir_html <- "salidas/html"
    
    if (!dir.exists(dir_html)) dir.create(dir_html, recursive = TRUE)
    
    nombre_html <- file.path(
      dir_html,
      paste0("022_", nombre_dataset, "_", nombre_variable, "_postplot.html")
    )
    
    # Exportar utilizando mapshot directamente sobre el objeto mapview
    mapshot(
      mapa_interactivo, 
      url = nombre_html, 
      selfcontained = TRUE
    )
    
    cat(" Mapa HTML interactivo guardado en:\n")
    cat(nombre_html, "\n\n")
  }
}