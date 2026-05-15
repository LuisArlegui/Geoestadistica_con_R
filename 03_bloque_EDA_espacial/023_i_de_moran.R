###############################################################
#
#  023. Índice de Moran global
#
#  Descripción:
#  Calcula el índice de Moran global de una variable numérica
#  a partir de un dataset con coordenadas X e Y, utilizando
#  una matriz de pesos espaciales basada en k vecinos más cercanos.
#  También realiza una prueba de Monte Carlo por permutaciones.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - resultados en consola
#  - salidas/tablas/dataset_variable_Moran_Global.csv
#
###############################################################



# Cargar librerías ====

if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("spdep", quietly = TRUE)) install.packages("spdep")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(sf)
library(spdep)
library(dplyr)



# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

nombre_x <- "columnaX"              # <-- EDITAR AQUÍ
nombre_y <- "columnay"              # <-- EDITAR AQUÍ
nombre_variable <- "variable_bjetivo"     # <-- EDITAR AQUÍ

epsg_datos <- 12345          # <-- EDITAR AQUÍ

# Número de vecinos más cercanos
k_vecinos <- 8               # <-- EDITAR AQUÍ

# Número de simulaciones Monte Carlo
n_sim <- 999                 # <-- EDITAR AQUÍ

# Guardado
guardar_resultados <- TRUE   # <-- TRUE / FALSE



# Comprobaciones ====

if (!nombre_x %in% colnames(data)) {
  stop(paste("La columna X", shQuote(nombre_x), "no existe en el dataset."))
}

if (!nombre_y %in% colnames(data)) {
  stop(paste("La columna Y", shQuote(nombre_y), "no existe en el dataset."))
}

if (!nombre_variable %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable), "no existe en el dataset."))
}

if (!is.numeric(data[[nombre_x]])) {
  stop(paste("La columna", shQuote(nombre_x), "debe ser numérica."))
}

if (!is.numeric(data[[nombre_y]])) {
  stop(paste("La columna", shQuote(nombre_y), "debe ser numérica."))
}

if (!is.numeric(data[[nombre_variable]])) {
  stop(paste("La variable", shQuote(nombre_variable), "debe ser numérica."))
}

# Filtrar NA en coordenadas y variable
data <- data %>%
  filter(
    !is.na(.data[[nombre_x]]),
    !is.na(.data[[nombre_y]]),
    !is.na(.data[[nombre_variable]])
  )

if (nrow(data) < 3) {
  stop("No hay suficientes observaciones válidas para calcular Moran's I.")
}

if (k_vecinos >= nrow(data)) {
  stop("k_vecinos debe ser menor que el número de observaciones válidas.")
}

cat("\nVariable seleccionada:", nombre_variable, "\n")
cat("Coordenadas:", nombre_x, ",", nombre_y, "\n")
cat("Número de observaciones válidas:", nrow(data), "\n")
cat("k vecinos:", k_vecinos, "\n")



# Conversión a objeto sf ====

data_sf <- st_as_sf(
  data,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)


# Matriz de pesos espaciales ====

coords <- st_coordinates(data_sf)

knn_nb <- knn2nb(knearneigh(coords, k = k_vecinos))

lw <- nb2listw(knn_nb, style = "W")



# Moran's I global ====

moran_result <- moran.test(data_sf[[nombre_variable]], lw)

cat("\n=====================================\n")
cat(" Moran's I global\n")
cat("=====================================\n")
print(moran_result)



# Moran Monte Carlo ====

moran_mc <- moran.mc(data_sf[[nombre_variable]], lw, nsim = n_sim)

cat("\n=====================================\n")
cat(" Moran Monte Carlo\n")
cat("=====================================\n")
print(moran_mc)




# Resumen de resultados ====

estadistico_I <- unname(moran_result$estimate[1])
esperanza_I <- unname(moran_result$estimate[2])
varianza_I <- unname(moran_result$estimate[3])
p_moran <- moran_result$p.value

estadistico_mc <- unname(moran_mc$statistic)
p_mc <- moran_mc$p.value

tabla_resultados <- data.frame(
  Variable = nombre_variable,
  N = nrow(data),
  EPSG = epsg_datos,
  K_vecinos = k_vecinos,
  Moran_I = estadistico_I,
  Moran_EI = esperanza_I,
  Moran_VI = varianza_I,
  Moran_p_value = p_moran,
  MonteCarlo_I = estadistico_mc,
  MonteCarlo_p_value = p_mc,
  NSIM = n_sim
)

cat("\n=====================================\n")
cat(" Resumen de resultados\n")
cat("=====================================\n")
print(tabla_resultados)



# Guardar resultados ====

if (guardar_resultados) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_salida <- "salidas/tablas"
  
  if (!dir.exists(dir_salida)) {
    dir.create(dir_salida, recursive = TRUE)
  }
  
  nombre_salida <- file.path(
    dir_salida,
    paste0(
      nombre_dataset, "_",
      nombre_variable,
      "_Moran_Global.csv"
    )
  )
  
  write.csv(tabla_resultados, nombre_salida, row.names = FALSE)
  
  cat("\n=====================================\n")
  cat(" Resultados guardados\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_salida, "\n\n")
}