###############################################################
#
#  027. Selección de superficie polinómica y tabla de residuales
#
#  Descripción:
#  Ajusta superficies polinómicas de orden 1, 2 y 3 a una
#  variable Z en función de dos coordenadas o variables
#  explicativas X e Y. Compara los modelos mediante R²,
#  R² ajustado, AIC, BIC y el valor-p global, selecciona
#  el modelo preferido según el criterio indicado y genera
#  una tabla con valores observados, predichos y residuales.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salidas:
#  - resumen de los modelos en consola
#  - tabla comparativa en consola
#  - salidas/tablas/027_dataset_comparacion_superficies.csv
#  - salidas/tablas/027_dataset_variable_residuales_orden_N.csv
#
###############################################################

# Cargar librerías ====
# Este script utiliza exclusivamente funciones de R Base (stats, utils).
# No requiere instalar ni cargar paquetes adicionales.


# Archivo de datos ====

ruta_datos <- "datos/brutos/wolfcamp.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data_original <- read.csv(ruta_datos, sep = ",")   # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data_original))



# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable_x <- "x"    # <-- EDITAR AQUÍ
nombre_variable_y <- "y"    # <-- EDITAR AQUÍ
nombre_variable_z <- "gw"   # <-- EDITAR AQUÍ

# Criterio de selección:
# "AIC", "BIC", "R2", "R2aj" o "manual"
criterio_seleccion <- "BIC"   # <-- EDITAR AQUÍ

# Solo se utiliza cuando criterio_seleccion = "manual"
orden_modelo_manual <- 2       # <-- 1, 2 o 3

guardar_resultados <- TRUE     # <-- TRUE / FALSE



# Comprobaciones ====

vars <- c(
  nombre_variable_x,
  nombre_variable_y,
  nombre_variable_z
)

faltan <- vars[!vars %in% colnames(data_original)]

if (length(faltan) > 0) {
  stop(
    paste(
      "No existen estas columnas en el dataset:",
      paste(faltan, collapse = ", ")
    )
  )
}

vars_no_num <- vars[!sapply(data_original[vars], is.numeric)]

if (length(vars_no_num) > 0) {
  stop(
    paste(
      "Estas variables deben ser numéricas:",
      paste(vars_no_num, collapse = ", ")
    )
  )
}

criterios_validos <- c("AIC", "BIC", "R2", "R2aj", "manual")

if (!criterio_seleccion %in% criterios_validos) {
  stop(
    paste(
      "criterio_seleccion debe ser uno de:",
      paste(criterios_validos, collapse = ", ")
    )
  )
}

if (!orden_modelo_manual %in% c(1, 2, 3)) {
  stop("orden_modelo_manual debe ser 1, 2 o 3.")
}

# Filtrar filas completas
data_model <- data_original[
  complete.cases(data_original[, vars]),
  vars,
  drop = FALSE
]

if (nrow(data_model) == 0) {
  stop(
    paste(
      "No quedan observaciones válidas tras eliminar NA",
      "en las variables seleccionadas."
    )
  )
}

colnames(data_model) <- c("X", "Y", "Z")

cat("\nVariable X:", nombre_variable_x, "\n")
cat("Variable Y:", nombre_variable_y, "\n")
cat("Variable Z:", nombre_variable_z, "\n")
cat("Número de observaciones válidas:", nrow(data_model), "\n")
cat("Criterio de selección:", criterio_seleccion, "\n")



# Definición de las fórmulas ====

formula_orden_1 <- Z ~ X + Y

formula_orden_2 <- Z ~
  X + Y +
  I(X^2) + I(Y^2) +
  I(X * Y)

formula_orden_3 <- Z ~
  X + Y +
  I(X^2) + I(Y^2) +
  I(X * Y) +
  I(X^3) + I(Y^3) +
  I(X^2 * Y) + I(X * Y^2)



# Ajuste de los modelos ====

modelo_1 <- lm(
  formula_orden_1,
  data = data_model
)

modelo_2 <- lm(
  formula_orden_2,
  data = data_model
)

modelo_3 <- lm(
  formula_orden_3,
  data = data_model
)

modelos <- list(
  Orden_1 = modelo_1,
  Orden_2 = modelo_2,
  Orden_3 = modelo_3
)



# Resumen de los modelos ====


cat(" Resumen del modelo de orden 1\n")
print(summary(modelo_1))


cat(" Resumen del modelo de orden 2\n")
print(summary(modelo_2))

cat(" Resumen del modelo de orden 3\n")
print(summary(modelo_3))



# Función para obtener el valor-p global ====

obtener_p_global <- function(modelo) {
  
  resumen <- summary(modelo)
  
  estadistico_f <- resumen$fstatistic
  
  if (is.null(estadistico_f)) {
    return(NA_real_)
  }
  
  pf(
    estadistico_f[1],
    estadistico_f[2],
    estadistico_f[3],
    lower.tail = FALSE
  )
}



# Tabla comparativa ====

tabla_comparacion <- data.frame(
  Modelo = c(
    "Orden_1",
    "Orden_2",
    "Orden_3"
  ),
  Orden = c(1, 2, 3),
  N = c(
    nobs(modelo_1),
    nobs(modelo_2),
    nobs(modelo_3)
  ),
  Numero_parametros = c(
    length(coef(modelo_1)),
    length(coef(modelo_2)),
    length(coef(modelo_3))
  ),
  R2 = c(
    summary(modelo_1)$r.squared,
    summary(modelo_2)$r.squared,
    summary(modelo_3)$r.squared
  ),
  R2_ajustado = c(
    summary(modelo_1)$adj.r.squared,
    summary(modelo_2)$adj.r.squared,
    summary(modelo_3)$adj.r.squared
  ),
  AIC = c(
    AIC(modelo_1),
    AIC(modelo_2),
    AIC(modelo_3)
  ),
  BIC = c(
    BIC(modelo_1),
    BIC(modelo_2),
    BIC(modelo_3)
  ),
  p_value_global = c(
    obtener_p_global(modelo_1),
    obtener_p_global(modelo_2),
    obtener_p_global(modelo_3)
  )
)

rownames(tabla_comparacion) <- NULL

cat(" Comparación de superficies polinómicas\n")
tabla_print <- tabla_comparacion
tabla_print[, 5:9] <- round(tabla_print[, 5:9], 4)
print(tabla_print)


# Selección del modelo ====

if (criterio_seleccion == "AIC") {
  
  fila_seleccionada <- which.min(tabla_comparacion$AIC)
  descripcion_criterio <- "menor AIC"
  
} else if (criterio_seleccion == "BIC") {
  
  fila_seleccionada <- which.min(tabla_comparacion$BIC)
  descripcion_criterio <- "menor BIC"
  
} else if (criterio_seleccion == "R2") {
  
  fila_seleccionada <- which.max(tabla_comparacion$R2)
  descripcion_criterio <- "mayor R2"
  
} else if (criterio_seleccion == "R2aj") {
  
  fila_seleccionada <- which.max(tabla_comparacion$R2_ajustado)
  descripcion_criterio <- "mayor R2 ajustado"
  
} else {
  
  fila_seleccionada <- which(
    tabla_comparacion$Orden == orden_modelo_manual
  )
  
  descripcion_criterio <- paste0(
    "selección manual: orden ",
    orden_modelo_manual
  )
}

orden_seleccionado <- tabla_comparacion$Orden[fila_seleccionada]
modelo_seleccionado <- modelos[[fila_seleccionada]]

cat(" Modelo seleccionado\n")
cat("Orden:", orden_seleccionado, "\n")
cat("Criterio:", descripcion_criterio, "\n")
cat("R2:", tabla_comparacion$R2[fila_seleccionada], "\n")
cat(
  "R2 ajustado:",
  tabla_comparacion$R2_ajustado[fila_seleccionada],
  "\n"
)
cat("AIC:", tabla_comparacion$AIC[fila_seleccionada], "\n")
cat("BIC:", tabla_comparacion$BIC[fila_seleccionada], "\n")
cat(
  "Valor-p global:",
  tabla_comparacion$p_value_global[fila_seleccionada],
  "\n"
)



# Tabla de residuales ====

valores_predichos <- predict(
  modelo_seleccionado,
  newdata = data_model
)

valores_residuales <- residuals(
  modelo_seleccionado
)

tabla_residuales <- data.frame(
  id = seq_len(nrow(data_model)),
  X = data_model$X,
  Y = data_model$Y,
  observado = data_model$Z,
  predicho = valores_predichos,
  residual = valores_residuales
)

tabla_residuales$verificacion <-
  tabla_residuales$predicho +
  tabla_residuales$residual

cat("\nVerificación observado = predicho + residual:\n")
print(
  all.equal(
    tabla_residuales$observado,
    tabla_residuales$verificacion
  )
)



# Guardar resultados ====

if (guardar_resultados) {
  
  nombre_dataset <- tools::file_path_sans_ext(
    basename(ruta_datos)
  )
  
  # Directorios de salida
  dir_tablas <- "salidas/tablas"
  dir_datos  <- "datos/procesados"
  
  if (!dir.exists(dir_tablas)) {
    dir.create(dir_tablas, recursive = TRUE)
  }
  
  if (!dir.exists(dir_datos)) {
    dir.create(dir_datos, recursive = TRUE)
  }
  
  # Tabla comparativa
  nombre_salida_comparacion <- file.path(
    dir_tablas,
    paste0(
      "027_",
      nombre_dataset,
      "_comparacion_superficies.csv"
    )
  )
  
  # Tabla de residuales
  nombre_salida_residuales <- file.path(
    dir_datos,
    paste0(
      "027_",
      nombre_dataset, "_",
      nombre_variable_z,
      "_residuales_orden_",
      orden_seleccionado,
      ".csv"
    )
  )
  
  write.csv(
    tabla_comparacion,
    nombre_salida_comparacion,
    row.names = FALSE
  )
  
  write.csv(
    tabla_residuales,
    nombre_salida_residuales,
    row.names = FALSE
  )
  
  cat(" Resultados guardados\n")
  
  
  cat(
    "Tabla comparativa:\n",
    nombre_salida_comparacion,
    "\n\n"
  )
  
  cat(
    "Tabla de residuales:\n",
    nombre_salida_residuales,
    "\n\n"
  )
}
