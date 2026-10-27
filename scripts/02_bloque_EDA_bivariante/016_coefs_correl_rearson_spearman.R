#####################################################
#
#  016. Coeficientes de correlación de Pearson y Spearman
#
#  Descripción:
#  Calcula los coeficientes de correlación de Pearson y
#  Spearman entre dos variables numéricas, mostrando los
#  resultados completos de ambos tests y guardando un
#  resumen en un archivo CSV.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - resultados en consola
#  - salidas/tablas/016_dataset_varX_varY_correlacion.csv
#
#####################################################

# Cargar librerías ====
# Este script utiliza exclusivamente funciones de R Base (stats, utils).
# No requiere instalar ni cargar paquetes adicionales.

# Archivo de datos ===

ruta_datos <- "datos/brutos/meuse.csv"

# Comprobar que el fichero existe
if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

# Leer datos
data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ⚙️ PARÁMETROS A EDITAR ===

nombre_variable_x <- "zinc"   # <-- EDITAR AQUÍ
nombre_variable_y <- "lead"   # <-- EDITAR AQUÍ

# Guardar resultados en CSV
guardar_resultados <- TRUE        # <-- TRUE / FALSE



# Comprobaciones ===

if (!nombre_variable_x %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_x), "no existe en el dataset"))
}

if (!nombre_variable_y %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_y), "no existe en el dataset"))
}

if (!is.numeric(data[[nombre_variable_x]])) {
  stop(paste("La variable", shQuote(nombre_variable_x), "debe ser numérica"))
}

if (!is.numeric(data[[nombre_variable_y]])) {
  stop(paste("La variable", shQuote(nombre_variable_y), "debe ser numérica"))
}

# Filtrar filas completas para ambas variables
data_cor <- data[complete.cases(data[, c(nombre_variable_x, nombre_variable_y)]), ]

if (nrow(data_cor) < 3) {
  stop("Se requieren al menos 3 observaciones válidas para calcular la correlación.")
}

Variable_X <- data_cor[[nombre_variable_x]]
Variable_Y <- data_cor[[nombre_variable_y]]

cat("\nVariable X seleccionada:", nombre_variable_x, "\n")
cat("Variable Y seleccionada:", nombre_variable_y, "\n")
cat("Número de observaciones válidas:", nrow(data_cor), "\n")



# Correlación de Pearson ====

pearson_result <- cor.test(
  Variable_X,
  Variable_Y,
  method = "pearson"
)

cat(" Correlación de Pearson\n")
print(pearson_result)



# Correlación de Spearman ====

spearman_result <- cor.test(
  Variable_X,
  Variable_Y,
  method = "spearman",
  exact = FALSE
)

cat(" Correlación de Spearman\n")
print(spearman_result)



# Resumen de resultados ====
# Extracción segura de intervalos de confianza
conf_low  <- if (!is.null(pearson_result$conf.int)) pearson_result$conf.int[1] else NA
conf_high <- if (!is.null(pearson_result$conf.int)) pearson_result$conf.int[2] else NA

resultado_resumen <- data.frame(
  Variable_X = nombre_variable_x,
  Variable_Y = nombre_variable_y,
  N = nrow(data_cor),
  Pearson_r = unname(pearson_result$estimate),
  Pearson_p_value = pearson_result$p.value,
  Pearson_conf_low = pearson_result$conf.int[1],
  Pearson_conf_high = pearson_result$conf.int[2],
  Spearman_rho = unname(spearman_result$estimate),
  Spearman_p_value = spearman_result$p.value
)

cat(" Resumen de resultados\n")
print(resultado_resumen)



# Guardar resultados ====

if (guardar_resultados) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_salida <- "salidas/tablas"
  
  if (!dir.exists(dir_salida)) {
    dir.create(dir_salida, recursive = TRUE)
  }
  
  nombre_salida <- file.path(
    dir_salida,
    paste0("016_",
      nombre_dataset, "_",
      nombre_variable_x, "_",
      nombre_variable_y,
      "_correlacion.csv"
    )
  )
  
  write.csv(resultado_resumen, nombre_salida, row.names = FALSE)
  
 cat(" Resultados guardados\n")
  cat("Archivo:\n", nombre_salida, "\n\n")
}