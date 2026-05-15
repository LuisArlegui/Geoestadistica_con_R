#####################################################
#
#  046. Simulación gaussiana 2: SGS y reversión
#
#  Descripción:
#  Realiza simulación secuencial gaussiana condicional
#  sobre la variable gaussianizada generada en el script
#  045. Posteriormente, revierte cada simulación a las
#  unidades originales mediante la tabla de anamorfosis.
#
#  Requisito previo:
#  - Ejecutar antes el script 045, que guarda los objetos
#    necesarios en la carpeta modelos/
#
#  Entrada:
#  - datos/brutos/meuse_zn.csv
#  - modelos/dataset_variable_anamorfosis.rds
#  - modelos/dataset_variable_modelo_variograma_gaussiano.rds
#  - modelos/dataset_variable_rejilla.rds
#
#  Salida:
#  - modelos/dataset_variable_simulaciones_gaussianas.rds
#  - modelos/dataset_variable_simulaciones_originales.rds
#  - salidas/figuras/dataset_variable_simulacion_gaussiana_N.pdf
#  - salidas/figuras/dataset_variable_simulacion_original_N.pdf
#
#####################################################

# ==== Cargar librerías ====

if (!requireNamespace("gstat", quietly = TRUE)) install.packages("gstat")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")

library(gstat)
library(sf)
library(ggplot2)
library(viridis)

# ==== Archivo de datos ====

ruta_datos <- "datos/brutos/meuse.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))

# ==== Parámetros a editar ====

nombre_x <- "x"
nombre_y <- "y"
nombre_variable <- "zinc"

epsg_datos <- 28992

# Simulación
numero_simulaciones <- 1000
nmax_vecinos <- 155
media_gaussiana <- 0

# Simulación a visualizar
sim_visualizar <- 52

# Guardado
guardar_modelos <- TRUE
guardar_figuras <- TRUE
formato_figura <- "pdf"      # "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300

# ==== Rutas de objetos del script 045 ====

nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))

ruta_anamorfosis <- file.path(
  "modelos",
  paste0(nombre_dataset, "_", nombre_variable, "_anamorfosis.rds")
)

ruta_modelo_variograma <- file.path(
  "modelos",
  paste0(nombre_dataset, "_", nombre_variable, "_modelo_variograma_gaussiano.rds")
)

ruta_rejilla <- file.path(
  "modelos",
  paste0(nombre_dataset, "_", nombre_variable, "_rejilla.rds")
)

if (!file.exists(ruta_anamorfosis)) {
  stop(paste("No se encuentra:", ruta_anamorfosis, "\nEjecuta antes el script 045."))
}

if (!file.exists(ruta_modelo_variograma)) {
  stop(paste("No se encuentra:", ruta_modelo_variograma, "\nEjecuta antes el script 045."))
}

if (!file.exists(ruta_rejilla)) {
  stop(paste("No se encuentra:", ruta_rejilla, "\nEjecuta antes el script 045."))
}

# ==== Comprobaciones de datos ====

vars <- c(nombre_x, nombre_y, nombre_variable)

faltan <- vars[!vars %in% colnames(data)]

if (length(faltan) > 0) {
  stop(paste("No existen estas columnas en el dataset:", paste(faltan, collapse = ", ")))
}

data_plot <- data[
  complete.cases(data[, vars]),
  vars,
  drop = FALSE
]

if (nrow(data_plot) < 3) {
  stop("Se necesitan al menos tres observaciones válidas.")
}

# ==== Cargar objetos del script 045 ====

tabla_anamorfosis <- readRDS(ruta_anamorfosis)
modelo_gauss <- readRDS(ruta_modelo_variograma)
grid_sf <- readRDS(ruta_rejilla)

cat("\n=====================================\n")
cat(" Objetos cargados del script 045\n")
cat("=====================================\n")
cat("Tabla de anamorfosis:", ruta_anamorfosis, "\n")
cat("Modelo variograma:", ruta_modelo_variograma, "\n")
cat("Rejilla:", ruta_rejilla, "\n\n")

# ==== Reconstruir datos gaussianizados ====

data_sf <- st_as_sf(
  data_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)

data_sf$Z_gauss <- qnorm(
  (rank(data_sf[[nombre_variable]], ties.method = "average") - 0.5) /
    length(data_sf[[nombre_variable]])
)

data_sf$Z_gauss <- data_sf$Z_gauss - mean(data_sf$Z_gauss, na.rm = TRUE)

cat("\nResumen de la variable gaussianizada:\n")
print(summary(data_sf$Z_gauss))

# ==== Simulación secuencial gaussiana ====

set.seed(1234)

simulation_kriging <- krige(
  Z_gauss ~ 1,
  locations = data_sf,
  newdata = grid_sf,
  model = modelo_gauss,
  beta = media_gaussiana,
  nsim = numero_simulaciones,
  nmax = nmax_vecinos
)

st_crs(simulation_kriging) <- epsg_datos

cat("\n=====================================\n")
cat(" Simulaciones gaussianas completadas\n")
cat("=====================================\n")
print(summary(simulation_kriging))

simulation_kriging_sf <- st_as_sf(simulation_kriging)
simulation_kriging_sf <- cbind(
  simulation_kriging_sf,
  st_coordinates(simulation_kriging_sf)
)

# ==== Reversión de la anamorfosis ====

# La reversión se realiza mediante interpolación lineal entre
# la tabla de pares: valor_gaussiano -> valor_original.

revertir_anamorfosis <- function(z_val, tabla) {
  approx(
    x = tabla$valor_gaussiano,
    y = tabla$valor_original,
    xout = z_val,
    rule = 2,
    ties = "ordered"
  )$y
}

sim_data <- st_drop_geometry(simulation_kriging_sf)

columnas_sim <- grep("^sim", colnames(sim_data), value = TRUE)

if (length(columnas_sim) == 0) {
  stop("No se han encontrado columnas de simulación con prefijo 'sim'.")
}

sim_original_data <- sim_data

for (col_sim in columnas_sim) {
  sim_original_data[[col_sim]] <- revertir_anamorfosis(
    sim_data[[col_sim]],
    tabla_anamorfosis
  )
}

sim_original_sf <- st_as_sf(
  sim_original_data,
  coords = c("X", "Y"),
  crs = st_crs(simulation_kriging_sf),
  remove = FALSE
)

cat("\n=====================================\n")
cat(" Reversión de anamorfosis completada\n")
cat("=====================================\n")
cat("Número de simulaciones revertidas:", length(columnas_sim), "\n")

# ==== Visualización simulación gaussiana ====

sim_col <- paste0("sim", sim_visualizar)

if (!sim_col %in% columnas_sim) {
  stop(paste("No existe", sim_col, ". Revisa sim_visualizar."))
}

grafico_sim_gauss <- ggplot(simulation_kriging_sf, aes(x = X, y = Y, fill = .data[[sim_col]])) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(
    title = paste("Simulación gaussiana", sim_visualizar),
    subtitle = "Variable gaussianizada",
    x = nombre_x,
    y = nombre_y,
    fill = "Z"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )

print(grafico_sim_gauss)

# ==== Visualización simulación revertida ====

grafico_sim_original <- ggplot(sim_original_sf, aes(x = X, y = Y, fill = .data[[sim_col]])) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(
    title = paste("Simulación revertida", sim_visualizar),
    subtitle = paste(nombre_variable, "en unidades originales"),
    x = nombre_x,
    y = nombre_y,
    fill = paste(nombre_variable, "(ppm)")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )

print(grafico_sim_original)

# ==== Guardar modelos y simulaciones ====

if (guardar_modelos) {
  
  dir_modelos <- "modelos"
  
  if (!dir.exists(dir_modelos)) {
    dir.create(dir_modelos, recursive = TRUE)
  }
  
  ruta_sim_gauss <- file.path(
    dir_modelos,
    paste0(nombre_dataset, "_", nombre_variable, "_simulaciones_gaussianas.rds")
  )
  
  ruta_sim_original <- file.path(
    dir_modelos,
    paste0(nombre_dataset, "_", nombre_variable, "_simulaciones_originales.rds")
  )
  
  saveRDS(simulation_kriging_sf, ruta_sim_gauss)
  saveRDS(sim_original_sf, ruta_sim_original)
  
  cat("\n=====================================\n")
  cat(" Simulaciones guardadas\n")
  cat("=====================================\n")
  cat("Gaussianas:\n", ruta_sim_gauss, "\n")
  cat("Originales:\n", ruta_sim_original, "\n\n")
}

# ==== Guardar figuras ====

if (guardar_figuras) {
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura_gauss <- file.path(
    dir_figuras,
    paste0(
      nombre_dataset, "_",
      nombre_variable,
      "_simulacion_gaussiana_",
      sim_visualizar,
      ".",
      formato_figura
    )
  )
  
  nombre_figura_original <- file.path(
    dir_figuras,
    paste0(
      nombre_dataset, "_",
      nombre_variable,
      "_simulacion_original_",
      sim_visualizar,
      ".",
      formato_figura
    )
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura_gauss,
      plot = grafico_sim_gauss,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
    ggsave(
      filename = nombre_figura_original,
      plot = grafico_sim_original,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura_gauss,
      plot = grafico_sim_gauss,
      width = ancho_figura,
      height = alto_figura,
      device = "pdf"
    )
    
    ggsave(
      filename = nombre_figura_original,
      plot = grafico_sim_original,
      width = ancho_figura,
      height = alto_figura,
      device = "pdf"
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("\n=====================================\n")
  cat(" Figuras guardadas\n")
  cat("=====================================\n")
  cat("Simulación gaussiana:\n", nombre_figura_gauss, "\n")
  cat("Simulación original:\n", nombre_figura_original, "\n\n")
}

