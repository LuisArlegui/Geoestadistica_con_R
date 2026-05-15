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
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráficos en la ventana gráfica de R
#  - modelo ajustado en consola
#  - (opcional) salidas/figuras/dataset_variable_variograma_modelo_inicial.png o .pdf
#  - (opcional) salidas/figuras/dataset_variable_variograma_modelo_ajustado.png o .pdf
#  - (opcional) salidas/tablas/dataset_variable_variograma_experimental.csv
#  - (opcional) salidas/tablas/dataset_variable_modelo_variograma_ajustado.csv
#
#####################################################

# Cargar librerías ====

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gstat", quietly = TRUE)) install.packages("gstat")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("mapview", quietly = TRUE)) install.packages("mapview")

library(ggplot2)
library(gstat)
library(sf)
library(mapview)



# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))




# Parámetros a editar ====

nombre_x <- "x"                 # <-- EDITAR AQUÍ
nombre_y <- "y"                 # <-- EDITAR AQUÍ
nombre_variable <- "logZn"      # <-- EDITAR AQUÍ

epsg_datos <- 28992             # <-- EDITAR AQUÍ

# Variograma experimental
cutoff_variograma <- 1500       # distancia máxima
lag_variograma <- NULL          # NULL = gstat decide; o valor numérico, p.ej. 75

# Modelo inicial de variograma
pSill <- 0.12                   # partial sill = sill - nugget
modelo_variograma <- "Sph"      # "Exp", "Sph", "Gau", "Bes", "Mat", etc.
rango <- 1000                   # range
nugget <- 0.01                  # nugget

# Etiquetas y títulos
Label_x <- "Distancia"
Label_y <- "Semivarianza"
Title_graph <- "Modelo de variograma omnidireccional"
Subtitle_graph <- nombre_variable

# Opciones
mostrar_mapa <- TRUE
mostrar_modelo_inicial <- TRUE
mostrar_modelo_ajustado <- TRUE

# Guardado
guardar_figuras <- TRUE
formato_figura <- "pdf"         # "png" o "pdf"
guardar_tablas <- TRUE

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300             # solo PNG



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

cat("\nCoordenada X:", nombre_x, "\n")
cat("Coordenada Y:", nombre_y, "\n")
cat("Variable:", nombre_variable, "\n")
cat("EPSG:", epsg_datos, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")
cat("Cutoff:", cutoff_variograma, "\n")
cat("Modelo inicial:", modelo_variograma, "\n")
cat("pSill:", pSill, "\n")
cat("Range:", rango, "\n")
cat("Nugget:", nugget, "\n")




# Conversión a sf ====

data_sf <- st_as_sf(
  data_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)



# Mapa rápido ====

if (mostrar_mapa) {
  mapviewOptions(
    basemaps = "CartoDB.Voyager",
    basemaps.color.shuffle = FALSE
  )
  
  print(
    mapview(
      data_sf,
      zcol = nombre_variable,
      map.types = "CartoDB.Voyager"
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
cat(" Modelo inicial\n")
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
cat(" Modelo ajustado\n")
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

ylim_max <- max_gamma * 1.1



# Gráfico modelo inicial ====

grafico_inicial <- ggplot(variograma_exp, aes(x = dist, y = gamma)) +
  geom_point(
    shape = 21,
    fill = "orange",
    color = "darkred",
    size = 2
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
    title = paste(Title_graph, "- modelo inicial"),
    subtitle = Subtitle_graph
  ) +
  xlim(0, cutoff_variograma) +
  ylim(0, ylim_max) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
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
    size = 2
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
    title = paste(Title_graph, "- modelo ajustado"),
    subtitle = Subtitle_graph
  ) +
  xlim(0, cutoff_variograma) +
  ylim(0, ylim_max) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )

if (mostrar_modelo_ajustado) {
  print(grafico_ajustado)
}



# Guardar figuras ====

if (guardar_figuras) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura_inicial <- file.path(
    dir_figuras,
    paste0(
      nombre_dataset, "_",
      nombre_variable,
      "_variograma_modelo_inicial.",
      formato_figura
    )
  )
  
  nombre_figura_ajustado <- file.path(
    dir_figuras,
    paste0(
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
      height = alto_figura,
      device = cairo_pdf
    )
    
    ggsave(
      filename = nombre_figura_ajustado,
      plot = grafico_ajustado,
      width = ancho_figura,
      height = alto_figura,
      device = cairo_pdf
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("\n=====================================\n")
  cat(" Figuras guardadas\n")
  cat("=====================================\n")
  cat("Archivo modelo inicial:\n", nombre_figura_inicial, "\n")
  cat("Archivo modelo ajustado:\n", nombre_figura_ajustado, "\n\n")
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
    paste0(
      nombre_dataset, "_",
      nombre_variable,
      "_variograma_experimental.csv"
    )
  )
  
  nombre_modelo <- file.path(
    dir_tablas,
    paste0(
      nombre_dataset, "_",
      nombre_variable,
      "_modelo_variograma_ajustado.csv"
    )
  )
  
  write.csv(variograma_exp, nombre_variograma, row.names = FALSE)
  write.csv(as.data.frame(modelo_ajustado), nombre_modelo, row.names = FALSE)
  
  cat("\n=====================================\n")
  cat(" Tablas guardadas\n")
  cat("=====================================\n")
  cat("Variograma experimental:\n", nombre_variograma, "\n")
  cat("Modelo ajustado:\n", nombre_modelo, "\n\n")
}