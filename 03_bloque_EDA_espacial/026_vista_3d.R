#####################################################
#
#  026. Vista 3D
#
#  Descripción:
#  Genera un gráfico interactivo 3D de dispersión a
#  partir de tres variables numéricas, útil para explorar
#  relaciones espaciales, topográficas o multivariantes.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico interactivo en el visor de RStudio
#  - (opcional) salidas/html/dataset_varX_varY_varZ_vista3D.html
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
nombre_variable_z <- "gw"    # <-- EDITAR AQUÍ

titulo_grafico <- "Vista 3D"       # <-- EDITAR AQUÍ

tam_puntos <- 5
color_puntos <- "blue"
opacidad_puntos <- 0.5

guardar_html <- TRUE               # <-- TRUE / FALSE



# Comprobaciones ====

vars <- c(nombre_variable_x, nombre_variable_y, nombre_variable_z)

faltan <- vars[!vars %in% colnames(data)]

if (length(faltan) > 0) {
  stop(paste("No existen estas columnas en el dataset:", paste(faltan, collapse = ", ")))
}

vars_no_num <- vars[!sapply(data[vars], is.numeric)]

if (length(vars_no_num) > 0) {
  stop(paste("Estas variables deben ser numéricas:", paste(vars_no_num, collapse = ", ")))
}

datos_3d <- data[complete.cases(data[, vars]), vars, drop = FALSE]

if (nrow(datos_3d) == 0) {
  stop("No quedan observaciones válidas tras eliminar NA en las variables seleccionadas.")
}

colnames(datos_3d) <- c("X", "Y", "Z")

cat("\nVariables seleccionadas:\n")
cat("X:", nombre_variable_x, "\n")
cat("Y:", nombre_variable_y, "\n")
cat("Z:", nombre_variable_z, "\n")
cat("Número de observaciones válidas:", nrow(datos_3d), "\n")



# Gráfico 3D ====

grafico_3d <- plot_ly(
  data = datos_3d,
  x = ~X,
  y = ~Y,
  z = ~Z,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = tam_puntos,
    color = color_puntos,
    opacity = opacidad_puntos
  )
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
      nombre_variable_x, "_",
      nombre_variable_y, "_",
      nombre_variable_z,
      "_vista3D.html"
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