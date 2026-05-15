#####################################################
#
#  018. Matriz de correlación numérica (Pearson)
#
#  Descripción:
#  Calcula una matriz numérica de correlación de Pearson
#  entre variables numéricas y la guarda en un archivo CSV,
#  útil para documentar y exportar relaciones multivariantes.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - salidas/tablas/dataset_correlacion_Pearson.csv
#
#####################################################



# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

# Columnas a excluir del análisis
columnas_excluir <- c(
  "columna1",
  "columna2",
  "columna3",
  "columna4",
  "columna5"
)   # <-- EDITAR AQUÍ

# Guardado de resultados
guardar_tabla <- TRUE   # <-- TRUE / FALSE



# Preparación de datos ====

# Excluir columnas indicadas
data_filtrado <- data[, !(names(data) %in% columnas_excluir), drop = FALSE]

cat("\nColumnas excluidas del análisis:\n")
print(columnas_excluir)

cat("\nColumnas restantes tras el filtrado:\n")
print(colnames(data_filtrado))

if (ncol(data_filtrado) == 0) {
  stop("No quedan columnas tras aplicar el filtrado.")
}

# Conservar solo variables numéricas
data_num <- data_filtrado[, sapply(data_filtrado, is.numeric), drop = FALSE]

cat("\nVariables numéricas utilizadas en la matriz de correlación:\n")
print(colnames(data_num))

if (ncol(data_num) < 2) {
  stop("Se necesitan al menos dos variables numéricas para calcular una matriz de correlación.")
}



# Resumen estadístico ====

cat("\nResumen estadístico de las variables seleccionadas:\n")
print(summary(data_num))



# Matriz de correlación ====

correlaciones <- cor(data_num, use = "complete.obs", method = "pearson")

cat("\nMatriz de correlación calculada:\n")
print(correlaciones)



# Guardar matriz de correlación ====

if (guardar_tabla) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_salida <- "salidas/tablas"
  
  if (!dir.exists(dir_salida)) {
    dir.create(dir_salida, recursive = TRUE)
  }
  
  nombre_salida <- file.path(
    dir_salida,
    paste0(nombre_dataset, "_correlacion_Pearson.csv")
  )
  
  write.csv(correlaciones, file = nombre_salida, row.names = TRUE)
  
  cat("\n=====================================\n")
  cat(" Matriz de correlación guardada\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_salida, "\n\n")
}