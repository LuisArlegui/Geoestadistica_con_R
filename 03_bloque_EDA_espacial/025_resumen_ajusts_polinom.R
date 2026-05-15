#####################################################
#
#  025. Resumen de ajustes polinómicos 2D
#
#  Descripción:
#  Ajusta modelos de regresión lineal y polinómica
#  de segundo y tercer orden entre dos variables
#  numéricas, muestra sus resúmenes en consola y
#  exporta una tabla comparativa con R², R² ajustado,
#  AIC, BIC y p-value global del modelo.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - resultados en consola
#  - salidas/tablas/dataset_varX_varY_resumen_ajustes_polinomicos.csv
#
#####################################################



# Archivo de datos ====

ruta_datos <- "datos/brutos/wolfcamp.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))




# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable_x <- "x"   # <-- EDITAR AQUÍ
nombre_variable_y <- "gw"   # <-- EDITAR AQUÍ

guardar_resultados <- TRUE        # <-- TRUE / FALSE




# Comprobaciones ====

if (!nombre_variable_x %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_x), "no existe en el dataset."))
}

if (!nombre_variable_y %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_y), "no existe en el dataset."))
}

if (!is.numeric(data[[nombre_variable_x]])) {
  stop(paste("La variable", shQuote(nombre_variable_x), "debe ser numérica."))
}

if (!is.numeric(data[[nombre_variable_y]])) {
  stop(paste("La variable", shQuote(nombre_variable_y), "debe ser numérica."))
}

data_model <- data[
  complete.cases(data[, c(nombre_variable_x, nombre_variable_y)]),
  c(nombre_variable_x, nombre_variable_y),
  drop = FALSE
]

if (nrow(data_model) == 0) {
  stop("No quedan observaciones válidas tras eliminar NA en las variables seleccionadas.")
}

cat("\nVariable X seleccionada:", nombre_variable_x, "\n")
cat("Variable Y seleccionada:", nombre_variable_y, "\n")
cat("Número de observaciones válidas:", nrow(data_model), "\n")




# Ajustes polinómicos ====

formula_1 <- as.formula(
  paste(nombre_variable_y, "~", nombre_variable_x)
)

formula_2 <- as.formula(
  paste(nombre_variable_y, "~ poly(", nombre_variable_x, ", degree = 2, raw = TRUE)")
)

formula_3 <- as.formula(
  paste(nombre_variable_y, "~ poly(", nombre_variable_x, ", degree = 3, raw = TRUE)")
)

modelo_1 <- lm(formula_1, data = data_model)
modelo_2 <- lm(formula_2, data = data_model)
modelo_3 <- lm(formula_3, data = data_model)




# Resultados completos en consola ====

cat("\n=====================================\n")
cat(" MODELO DE PRIMER ORDEN\n")
cat("=====================================\n")
print(summary(modelo_1))

cat("\n=====================================\n")
cat(" MODELO DE SEGUNDO ORDEN\n")
cat("=====================================\n")
print(summary(modelo_2))

cat("\n=====================================\n")
cat(" MODELO DE TERCER ORDEN\n")
cat("=====================================\n")
print(summary(modelo_3))



# Función auxiliar para resumir modelos ====

extraer_resumen_modelo <- function(modelo, nombre_modelo) {
  
  resumen <- summary(modelo)
  fstat <- resumen$fstatistic
  
  if (!is.null(fstat)) {
    p_global <- pf(
      q = fstat[1],
      df1 = fstat[2],
      df2 = fstat[3],
      lower.tail = FALSE
    )
  } else {
    p_global <- NA_real_
  }
  
  data.frame(
    Modelo = nombre_modelo,
    N = length(residuals(modelo)),
    R2 = resumen$r.squared,
    R2_ajustado = resumen$adj.r.squared,
    AIC = AIC(modelo),
    BIC = BIC(modelo),
    p_value_global = p_global
  )
}



# Tabla comparativa ====

tabla_resumen <- rbind(
  extraer_resumen_modelo(modelo_1, "Lineal"),
  extraer_resumen_modelo(modelo_2, "Polinomico_2"),
  extraer_resumen_modelo(modelo_3, "Polinomico_3")
)

cat("\n=====================================\n")
cat(" TABLA COMPARATIVA DE MODELOS\n")
cat("=====================================\n")
print(tabla_resumen)



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
      nombre_variable_x, "_",
      nombre_variable_y,
      "_resumen_ajustes_polinomicos.csv"
    )
  )
  
  write.csv(tabla_resumen, nombre_salida, row.names = FALSE)
  
  cat("\n=====================================\n")
  cat(" Resultados guardados\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_salida, "\n\n")
}