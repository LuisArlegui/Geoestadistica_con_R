#####################################################
#
#  013. Scatter Plots múltiples
#
#  Descripción:
#  Genera una matriz de gráficos de dispersión entre
#  varias variables numéricas, incluyendo relaciones
#  bivariantes, distribuciones individuales y coeficientes
#  de correlación, útil para el análisis exploratorio multivariante.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - matriz gráfica en la ventana gráfica de R
#  - salidas/figuras/dataset_scatterplots_multiples.png
#
#####################################################



# Cargar librerías ====

if (!requireNamespace("GGally", quietly = TRUE)) install.packages("GGally")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(GGally)
library(ggplot2)



# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

# Vector con las variables a incluir en la matriz
variables_grafico <- c(
  "variable1",
  "variable2",
  "variable3",
  "variable4"
)   # <-- EDITAR AQUÍ

# Parámetros estéticos de los puntos
color_puntos <- "steelblue"    # <-- EDITAR AQUÍ
alpha_puntos <- 0.6            # <-- EDITAR AQUÍ
tam_puntos   <- 1.8            # <-- EDITAR AQUÍ

# Parámetros estéticos de la densidad
color_densidad   <- "darkred"     # <-- EDITAR AQUÍ
relleno_densidad <- "lightcoral"  # <-- EDITAR AQUÍ
alpha_densidad   <- 0.4           # <-- EDITAR AQUÍ
grosor_densidad  <- 1.2           # <-- EDITAR AQUÍ

# Nombre del archivo de figura
sufijo_figura <- "scatterplots_multiples"   # <-- EDITAR AQUÍ

# Dimensiones de exportación
ancho_figura <- 12   # pulgadas
alto_figura  <- 12   # pulgadas
dpi_figura   <- 300



# Comprobaciones ====

# Verificar que todas las variables existen
faltan <- variables_grafico[!variables_grafico %in% colnames(data)]

if (length(faltan) > 0) {
  stop(
    paste(
      "Las siguientes variables no existen en el dataset:",
      paste(faltan, collapse = ", ")
    )
  )
}

# Verificar que todas las variables son numéricas
vars_no_numericas <- variables_grafico[!sapply(data[variables_grafico], is.numeric)]

if (length(vars_no_numericas) > 0) {
  stop(
    paste(
      "Las siguientes variables no son numéricas:",
      paste(vars_no_numericas, collapse = ", ")
    )
  )
}

cat("\nVariables seleccionadas para el gráfico:\n")
print(variables_grafico)

# Filtrar filas completas en las variables seleccionadas
data_plot <- data[complete.cases(data[, variables_grafico]), variables_grafico]

if (nrow(data_plot) == 0) {
  stop("No quedan filas completas tras eliminar NA en las variables seleccionadas.")
}

cat("\nNúmero de observaciones válidas:", nrow(data_plot), "\n")



# Preparación del plot ====

# Nota:
# - Diagonal: distribución de cada variable
# - Inferior: scatter plots
# - Superior: coeficientes de correlación

my_points <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(
      color = color_puntos,
      alpha = alpha_puntos,
      size = tam_puntos
    ) +
    theme_minimal()
}

my_density <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_density(
      color = color_densidad,
      fill = relleno_densidad,
      alpha = alpha_densidad,
      linewidth = grosor_densidad
    ) +
    theme_minimal()
}



# Función personalizada ====

custom_ggpairs <- function(data, columns) {
  ggpairs(
    data,
    columns = columns,
    lower = list(
      continuous = my_points
    ),
    diag = list(
      continuous = my_density
    ),
    upper = list(
      continuous = GGally::ggally_cor
    )
  )
}



# Generar gráfico ====

plot_matriz <- custom_ggpairs(
  data_plot,
  columns = 1:length(variables_grafico)
)

print(plot_matriz)



# Guardar figura ====

nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))

dir_figuras <- "salidas/figuras"

if (!dir.exists(dir_figuras)) {
  dir.create(dir_figuras, recursive = TRUE)
}

nombre_figura <- file.path(
  dir_figuras,
  paste0(nombre_dataset, "_", sufijo_figura, ".png")
)

ggsave(
  filename = nombre_figura,
  plot = plot_matriz,
  width = ancho_figura,
  height = alto_figura,
  dpi = dpi_figura
)

cat("\n=====================================\n")
cat(" Matriz de scatter plots generada\n")
cat("=====================================\n")
cat("Figura guardada en:\n", nombre_figura, "\n\n")