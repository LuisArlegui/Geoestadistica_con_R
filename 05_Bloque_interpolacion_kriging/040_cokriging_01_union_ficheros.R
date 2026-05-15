#####################################################
#
#  040. Cokriging 1: unión de ficheros
#
#  Descripción:
#  Combina dos archivos CSV con estructura compatible,
#  conservando únicamente las columnas seleccionadas.
#  Este script puede usarse para preparar un dataset
#  auxiliar combinado antes de aplicar cokriging.
#
#  Entrada:
#  - datos/brutos/fichero_1.csv
#  - datos/brutos/fichero_2.csv
#
#  Salida:
#  - datos/procesados/dataset_combinado.csv
#
#####################################################

# ==== Cargar librerías ====

if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# ==== Archivos de datos ====

ruta_datos_1 <- "datos/brutos/cloruros/LabSamples.csv"      # <-- EDITAR AQUÍ
ruta_datos_2 <- "datos/brutos/cloruros/FieldSamples.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos_1)) {
  stop(paste("No se encuentra el fichero:", ruta_datos_1))
}

if (!file.exists(ruta_datos_2)) {
  stop(paste("No se encuentra el fichero:", ruta_datos_2))
}

data1 <- read.csv(ruta_datos_1, sep = ",")   # <-- ajustar si hace falta
data2 <- read.csv(ruta_datos_2, sep = ",")   # <-- ajustar si hace falta

cat("Columnas disponibles en el primer dataset:\n")
print(colnames(data1))

cat("\nColumnas disponibles en el segundo dataset:\n")
print(colnames(data2))

# ==== Parámetros a editar ====

# Columnas que se desea conservar en ambos datasets
columnas_conservar <- c("x", "y", "FmSm")   # <-- EDITAR AQUÍ

# Nombre del archivo combinado de salida
nombre_salida <- "Field_conduc_combi.csv"   # <-- EDITAR AQUÍ

guardar_resultado <- TRUE

# ==== Comprobaciones ====

faltan_data1 <- columnas_conservar[!columnas_conservar %in% colnames(data1)]
faltan_data2 <- columnas_conservar[!columnas_conservar %in% colnames(data2)]

if (length(faltan_data1) > 0) {
  stop(
    paste(
      "En el primer dataset faltan estas columnas:",
      paste(faltan_data1, collapse = ", ")
    )
  )
}

if (length(faltan_data2) > 0) {
  stop(
    paste(
      "En el segundo dataset faltan estas columnas:",
      paste(faltan_data2, collapse = ", ")
    )
  )
}

common_columns <- intersect(colnames(data1), colnames(data2))

cat("\nColumnas comunes entre ambos datasets:\n")
print(common_columns)

cat("\nColumnas que se conservarán:\n")
print(columnas_conservar)

# ==== Selección de columnas ====

data1_filtered <- data1 %>%
  select(all_of(columnas_conservar))

data2_filtered <- data2 %>%
  select(all_of(columnas_conservar))

# ==== Unión de datasets ====

combined_data <- bind_rows(
  data1_filtered,
  data2_filtered
)

cat("\nNúmero de filas del primer dataset:", nrow(data1_filtered), "\n")
cat("Número de filas del segundo dataset:", nrow(data2_filtered), "\n")
cat("Número de filas del dataset combinado:", nrow(combined_data), "\n")

# ==== Guardar resultado ====

if (guardar_resultado) {
  
  dir_salida <- "datos/procesados"
  
  if (!dir.exists(dir_salida)) {
    dir.create(dir_salida, recursive = TRUE)
  }
  
  ruta_salida <- file.path(
    dir_salida,
    nombre_salida
  )
  
  write.csv(
    combined_data,
    ruta_salida,
    row.names = FALSE
  )
  
  cat("\n=====================================\n")
  cat(" Dataset combinado guardado\n")
  cat("=====================================\n")
  cat("Archivo:\n", ruta_salida, "\n\n")
}

# ==== Vista previa ====

print(head(combined_data))
