#####################################################
#
#  035. Modelo de variograma omnidireccional
#
#  Descripción:
#  Calcula el variograma experimental omnidireccional,
#  define un modelo variográfico inicial, realiza un ajuste
#  automático con gstat y representa tanto el modelo inicial
#  como el modelo ajustado. Es un paso fundamental para
#  preparar interpolaciones geoestadísticas como kriging.
#
#  Entrada:
#  - datos/brutos/wolfcamp.csv
#
#  Salida:
#  - gráficos en la ventana gráfica de R
#  - modelo ajustado en consola
#  - (opcional) salidas/variogramas/035_dataset_variable_variograma_modelo_inicial.png o .pdf
#  - (opcional) salidas/variogramas/035_dataset_variable_variograma_modelo_ajustado.png o .pdf
#  - (opcional) salidas/tablas/035_dataset_variable_variograma_experimental.csv
#  - (opcional) salidas/tablas/035_dataset_variable_modelo_variograma_ajustado.csv
#
#####################################################


# Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("ggplot2", "gstat", "sf", "mapview")

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

nombre_x        <- "coord_X"                 # <-- EDITAR AQUÍ
nombre_y        <- "coord_Y"                 # <-- EDITAR AQUÍ
nombre_variable <- "variable"                # <-- EDITAR AQUÍ

epsg_datos <- 12345             # <-- EDITAR AQUÍ: Código EPSG (ej. 32613 = UTM zone 13N)

# Variograma experimental
cutoff_variograma <- 125000     # Distancia máxima (en metros)
lag_variograma    <- 9000      # NULL = gstat decide; o valor numérico (ej. 10000)

# Modelo inicial de variograma
pSill             <- 4000      # Partial sill = sill - nugget
modelo_variograma <- "Sph"      # "Exp", "Sph", "Gau", "Bes", "Mat", etc.
rango             <- 100000      # Range
nugget            <- 800       # Nugget

# Mapa base libre de API Key
tipo_mapa_base <- "OpenStreetMap" # "OpenStreetMap", "Esri.WorldImagery", "Esri.WorldTopoMap"

# Etiquetas y títulos
Label_x        <- "Distancia (m)"
Label_y        <- "Semivarianza γ(h)"
Title_graph    <- "Modelo de Variograma Omnidireccional"
Subtitle_graph <- paste("Variable:", nombre_variable)

# Opciones de visualización
mostrar_mapa            <- TRUE
mostrar_modelo_inicial  <- TRUE
mostrar_modelo_ajustado <- TRUE

# Guardado
guardar_figuras <- TRUE
formato_figura  <- "pdf"         # "png" o "pdf"
guardar_tablas  <- TRUE

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300             # solo para PNG


# Comprobaciones ====

vars <- c(nombre_x, nombre_y, nombre_variable)

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

data_plot <- data[
  complete.cases(data[, vars]),
  vars,
  drop = FALSE
]

if (nrow(data_plot) < 2) {
  stop("Se necesitan al menos dos observaciones válidas para calcular el variograma.")
}


# Conversión a objeto espacial (sf) ====

data_sf <- st_as_sf(
  data_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)


# Mapa rápido (Mapview sin API Key) ====

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


# Variograma experimental ====

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


# Modelo inicial ====

modelo_inicial <- vgm(
  psill = pSill,
  model = modelo_variograma,
  range = rango,
  nugget = nugget
)

cat("\n=====================================\n")
cat(" Modelo inicial configurado:\n")
cat("=====================================\n")
print(modelo_inicial)

linea_modelo_inicial <- variogramLine(
  modelo_inicial,
  maxdist = cutoff_variograma
)

linea_modelo_inicial_df <- as.data.frame(linea_modelo_inicial)


# Ajuste automático del modelo ====

modelo_ajustado <- fit.variogram(
  object = variograma_exp,
  model = modelo_inicial
)

cat("\n=====================================\n")
cat(" Modelo ajustado automáticament por gstat:\n")
cat("=====================================\n")
print(modelo_ajustado)

linea_modelo_ajustado <- variogramLine(
  modelo_ajustado,
  maxdist = cutoff_variograma
)

linea_modelo_ajustado_df <- as.data.frame(linea_modelo_ajustado)


# Límites del gráfico ====

max_gamma <- max(
  variograma_exp$gamma,
  linea_modelo_inicial_df$gamma,
  linea_modelo_ajustado_df$gamma,
  na.rm = TRUE
)

ylim_max <- max_gamma * 1.15


# Gráfico modelo inicial ====

grafico_inicial <- ggplot(variograma_exp, aes(x = dist, y = gamma)) +
  geom_point(
    shape = 21,
    fill = "orange",
    color = "darkred",
    size = 2.5
  ) +
  geom_line(
    data = linea_modelo_inicial_df,
    aes(x = dist, y = gamma),
    color = "blue",
    linewidth = 1
  ) +
  labs(
    x = Label_x,
    y = Label_y,
    title = paste(Title_graph, "- Modelo inicial"),
    subtitle = Subtitle_graph
  ) +
  scale_x_continuous(limits = c(0, cutoff_variograma)) +
  scale_y_continuous(limits = c(0, ylim_max)) +
  theme_gray() +   # Tema gris por defecto de ggplot2
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black", size = 10, face = "bold")
  )

if (mostrar_modelo_inicial) {
  print(grafico_inicial)
}


# Gráfico modelo ajustado ====

grafico_ajustado <- ggplot(variograma_exp, aes(x = dist, y = gamma)) +
  geom_point(
    shape = 21,
    fill = "orange",
    color = "darkred",
    size = 2.5
  ) +
  geom_line(
    data = linea_modelo_ajustado_df,
    aes(x = dist, y = gamma),
    color = "blue",
    linewidth = 1
  ) +
  labs(
    x = Label_x,
    y = Label_y,
    title = paste(Title_graph, "- Modelo ajustado"),
    subtitle = Subtitle_graph
  ) +
  scale_x_continuous(limits = c(0, cutoff_variograma)) +
  scale_y_continuous(limits = c(0, ylim_max)) +
  theme_gray() +   # Tema gris por defecto de ggplot2
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black", size = 10, face = "bold")
  )

if (mostrar_modelo_ajustado) {
  print(grafico_ajustado)
}


# Guardar figuras ====

if (guardar_figuras) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_figuras <- "salidas/variogramas"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura_inicial <- file.path(
    dir_figuras,
    paste0("035_",
           nombre_dataset, "_",
           nombre_variable,
           "_variograma_modelo_inicial.",
           formato_figura
    )
  )
  
  nombre_figura_ajustado <- file.path(
    dir_figuras,
    paste0("035_",
           nombre_dataset, "_",
           nombre_variable,
           "_variograma_modelo_ajustado.",
           formato_figura
    )
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura_inicial,
      plot = grafico_inicial,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
    ggsave(
      filename = nombre_figura_ajustado,
      plot = grafico_ajustado,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura_inicial,
      plot = grafico_inicial,
      width = ancho_figura,
      height = alto_figura
    )
    
    ggsave(
      filename = nombre_figura_ajustado,
      plot = grafico_ajustado,
      width = ancho_figura,
      height = alto_figura
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("\n=====================================\n")
  cat(" Figuras guardadas exitosamente en:\n")
  cat("=====================================\n")
  cat("Modelo inicial:\n", nombre_figura_inicial, "\n")
  cat("Modelo ajustado:\n", nombre_figura_ajustado, "\n\n")
}


# Guardar tablas ====

if (guardar_tablas) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_tablas <- "salidas/tablas"
  
  if (!dir.exists(dir_tablas)) {
    dir.create(dir_tablas, recursive = TRUE)
  }
  
  nombre_variograma <- file.path(
    dir_tablas,
    paste0("035_",
           nombre_dataset, "_",
           nombre_variable,
           "_variograma_experimental.csv"
    )
  )
  
  nombre_modelo <- file.path(
    dir_tablas,
    paste0("035_",
           nombre_dataset, "_",
           nombre_variable,
           "_modelo_variograma_ajustado.csv"
    )
  )
  
  write.csv(variograma_exp, nombre_variograma, row.names = FALSE)
  write.csv(as.data.frame(modelo_ajustado), nombre_modelo, row.names = FALSE)
  
  cat("=====================================\n")
  cat(" Tablas guardadas exitosamente en:\n")
  cat("=====================================\n")
  cat("Variograma experimental:\n", nombre_variograma, "\n")
  cat("Modelo ajustado:\n", nombre_modelo, "\n\n")
}