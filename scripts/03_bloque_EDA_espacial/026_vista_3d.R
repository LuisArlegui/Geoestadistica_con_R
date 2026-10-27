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
#  - (opcional) salidas/html/026_dataset_varX_varY_varZ_vista3D.html
#
#####################################################



# Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("plotly", "htmlwidgets")

instalar_si_falta <- function(pkgs) {
  nuevos <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(nuevos) > 0) {
    message("Instalando paquetes faltantes: ", paste(nuevos, collapse = ", "))
    install.packages(nuevos, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

instalar_si_falta(paquetes_requeridos)



# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))




# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable_x <- "coord_x"   # <-- EDITAR AQUÍ
nombre_variable_y <- "coord_y"   # <-- EDITAR AQUÍ
nombre_variable_z <- "variable"    # <-- EDITAR AQUÍ

titulo_grafico <- "Representación Espacial 3D"   # <-- EDITAR AQUÍ

tam_puntos <- 5
opacidad_puntos <- 0.8
# Colorear puntos según el valor de Z (recomendado para mapas 3D)
colorear_segun_z <- TRUE   # <-- TRUE (escala de color) / FALSE (color fijo)
color_fijo       <- "blue" # Solo se usa si colorear_segun_z = FALSE

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
# Configurar estéticas de los marcadores
marcador_config <- if (colorear_segun_z) {
  list(
    size = tam_puntos,
    color = ~Z,
    colorscale = "Viridis",
    showscale = TRUE,
    opacity = opacidad_puntos,
    colorbar = list(title = nombre_variable_z)
  )
} else {
  list(
    size = tam_puntos,
    color = color_fijo,
    opacity = opacidad_puntos
  )
}

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
    paste0("026_",
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
