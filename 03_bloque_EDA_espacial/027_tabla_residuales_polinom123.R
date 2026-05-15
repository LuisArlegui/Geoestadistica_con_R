#####################################################
#
#  027. Tabla de residuales: superficie polinómica 2D
#
#  Descripción:
#  Ajusta una superficie polinómica de orden 1, 2 o 3
#  a una variable Z en función de dos coordenadas o
#  variables explicativas X e Y. Calcula valores predichos,
#  residuales y guarda una tabla de resultados.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - resumen del modelo en consola
#  - datos/procesados/dataset_varZ_residuales_orden_N.csv
#
#####################################################



# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data_original <- read.csv(ruta_datos, sep = ",")   # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data_original))



# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable_x <- "explicativa1"   # <-- EDITAR AQUÍ
nombre_variable_y <- "explicativa2"   # <-- EDITAR AQUÍ
nombre_variable_z <- "objetivo"   # <-- EDITAR AQUÍ

orden_modelo <- 2                 # <-- grado del polinomio: 1, 2 o 3

guardar_resultados <- TRUE        # <-- TRUE / FALSE




# Comprobaciones ====

vars <- c(nombre_variable_x, nombre_variable_y, nombre_variable_z)

faltan <- vars[!vars %in% colnames(data_original)]

if (length(faltan) > 0) {
  stop(paste("No existen estas columnas en el dataset:", paste(faltan, collapse = ", ")))
}

vars_no_num <- vars[!sapply(data_original[vars], is.numeric)]

if (length(vars_no_num) > 0) {
  stop(paste("Estas variables deben ser numéricas:", paste(vars_no_num, collapse = ", ")))
}

if (!orden_modelo %in% c(1, 2, 3)) {
  stop("orden_modelo debe ser 1, 2 o 3.")
}

# Filtrar filas completas
data_model <- data_original[
  complete.cases(data_original[, vars]),
  vars,
  drop = FALSE
]

if (nrow(data_model) == 0) {
  stop("No quedan observaciones válidas tras eliminar NA en las variables seleccionadas.")
}

colnames(data_model) <- c("X", "Y", "Z")

cat("\nVariable X:", nombre_variable_x, "\n")
cat("Variable Y:", nombre_variable_y, "\n")
cat("Variable Z:", nombre_variable_z, "\n")
cat("Orden del modelo:", orden_modelo, "\n")
cat("Número de observaciones válidas:", nrow(data_model), "\n")



# Ajuste del modelo ====

if (orden_modelo == 1) {
  formula_modelo <- Z ~ X + Y
}

if (orden_modelo == 2) {
  formula_modelo <- Z ~ X + Y + I(X^2) + I(Y^2) + I(X * Y)
}

if (orden_modelo == 3) {
  formula_modelo <- Z ~ X + Y +
    I(X^2) + I(Y^2) + I(X * Y) +
    I(X^3) + I(Y^3) +
    I(X^2 * Y) + I(X * Y^2)
}

modelo <- lm(formula_modelo, data = data_model)

cat("\n=====================================\n")
cat(" Resumen del modelo\n")
cat("=====================================\n")
print(summary(modelo))




# Tabla de residuales ====

residual_values <- data.frame(
  id = seq_len(nrow(data_model)),
  X = data_model$X,
  Y = data_model$Y,
  observado = data_model$Z,
  predicho = predict(modelo),
  residual = residuals(modelo)
)

residual_values$verificacion <- residual_values$predicho + residual_values$residual

cat("\nVerificación observado = predicho + residual:\n")
print(all.equal(residual_values$observado, residual_values$verificacion))




# Guardar resultados ====

if (guardar_resultados) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_salida <- "datos/procesados"
  
  if (!dir.exists(dir_salida)) {
    dir.create(dir_salida, recursive = TRUE)
  }
  
  nombre_salida <- file.path(
    dir_salida,
    paste0(
      nombre_dataset, "_",
      nombre_variable_z,
      "_residuales_orden_",
      orden_modelo,
      ".csv"
    )
  )
  
  write.csv(residual_values, nombre_salida, row.names = FALSE)
  
  cat("\n=====================================\n")
  cat(" Residuales guardados\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_salida, "\n\n")
}

