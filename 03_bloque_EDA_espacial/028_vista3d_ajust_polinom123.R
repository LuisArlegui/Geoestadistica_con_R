#####################################################
#
#  028. Vista 3D con superficie polinómica
#
#  Descripción:
#  Genera una vista interactiva 3D de datos puntuales
#  y superpone una superficie polinómica ajustada de
#  orden 1, 2 o 3, útil para explorar tendencias
#  espaciales o multivariantes en una variable Z.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico interactivo en el visor de RStudio
#  - resumen del modelo en consola
#  - (opcional) salidas/html/dataset_varZ_superficie_orden_N.html
#
#####################################################


# Cargar librerías ====

if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets")

library(plotly)
library(htmlwidgets)




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
nombre_variable_y <- "y"   # <-- EDITAR AQUÍ
nombre_variable_z <- "gw"   # <-- EDITAR AQUÍ

orden_modelo <- 2                 # <-- 1, 2 o 3

grid_lines <- 50                  # resolución de la superficie

titulo_grafico <- NULL            # si es NULL, se genera automáticamente

tam_puntos <- 4
color_puntos <- "blue"
opacidad_puntos <- 0.6
opacidad_superficie <- 0.8

guardar_html <- TRUE              # <-- TRUE / FALSE



# Comprobaciones ====

vars <- c(nombre_variable_x, nombre_variable_y, nombre_variable_z)

faltan <- vars[!vars %in% names(data)]

if (length(faltan) > 0) {
  stop(paste("No existen estas columnas en el dataset:", paste(faltan, collapse = ", ")))
}

vars_no_num <- vars[!sapply(data[vars], is.numeric)]

if (length(vars_no_num) > 0) {
  stop(paste("Estas variables deben ser numéricas:", paste(vars_no_num, collapse = ", ")))
}

if (!orden_modelo %in% c(1, 2, 3)) {
  stop("orden_modelo debe ser 1, 2 o 3.")
}

datos <- data[
  complete.cases(data[, vars]),
  vars,
  drop = FALSE
]

if (nrow(datos) == 0) {
  stop("No quedan observaciones válidas tras eliminar NA en las variables seleccionadas.")
}

colnames(datos) <- c("X", "Y", "Z")

cat("\nVariable X:", nombre_variable_x, "\n")
cat("Variable Y:", nombre_variable_y, "\n")
cat("Variable Z:", nombre_variable_z, "\n")
cat("Orden del modelo:", orden_modelo, "\n")
cat("Número de observaciones válidas:", nrow(datos), "\n")



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

fit <- lm(formula_modelo, data = datos)

cat("\n=====================================\n")
cat(" Resumen del modelo\n")
cat("=====================================\n")
print(summary(fit))


# Predicción en cuadrícula regular ====

x_pred <- seq(
  min(datos$X, na.rm = TRUE),
  max(datos$X, na.rm = TRUE),
  length.out = grid_lines
)

y_pred <- seq(
  min(datos$Y, na.rm = TRUE),
  max(datos$Y, na.rm = TRUE),
  length.out = grid_lines
)

grid <- expand.grid(
  X = x_pred,
  Y = y_pred
)

grid$Z_pred <- predict(fit, newdata = grid)

# Matriz para plotly:
# filas = y, columnas = x
z_mat <- matrix(
  grid$Z_pred,
  nrow = length(x_pred),
  ncol = length(y_pred)
)


# Título automático ====

if (is.null(titulo_grafico)) {
  titulo_grafico <- paste(
    "Vista 3D con superficie ajustada - orden",
    orden_modelo
  )
}



# Gráfico 3D ====

grafico_3d <- plot_ly() %>%
  add_markers(
    data = datos,
    x = ~X,
    y = ~Y,
    z = ~Z,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = tam_puntos,
      color = color_puntos,
      opacity = opacidad_puntos
    ),
    name = "Datos observados"
  ) %>%
  add_surface(
    x = x_pred,
    y = y_pred,
    z = z_mat,
    colorscale = "Viridis",
    opacity = opacidad_superficie,
    name = "Superficie ajustada"
  ) %>%
  layout(
    title = titulo_grafico,
    scene = list(
      xaxis = list(title = nombre_variable_x),
      yaxis = list(title = nombre_variable_y),
      zaxis = list(title = nombre_variable_z)
    )
  )

grafico_3d


# Guardar HTML ====

if (guardar_html) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_html <- "salidas/html"
  
  if (!dir.exists(dir_html)) {
    dir.create(dir_html, recursive = TRUE)
  }
  
  nombre_html <- file.path(
    dir_html,
    paste0(
      nombre_dataset, "_",
      nombre_variable_z,
      "_superficie_orden_",
      orden_modelo,
      ".html"
    )
  )
  
  htmlwidgets::saveWidget(
    widget = grafico_3d,
    file = nombre_html,
    selfcontained = TRUE
  )
  
  cat("\n=====================================\n")
  cat(" Gráfico 3D guardado\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_html, "\n\n")
}