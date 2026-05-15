#####################################################
#
#  014. Scatter Plot con histogramas marginales
#
#  Descripción:
#  Genera un gráfico de dispersión entre dos variables
#  numéricas acompañado de sus histogramas marginales,
#  útil para analizar conjuntamente la relación bivariante
#  y la distribución individual de cada variable.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - resumen del modelo lineal en consola
#  - (opcional) salidas/figuras/dataset_varX_varY_scatter_hist.png
#
#####################################################


# Cargar librerías ====

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("grid", quietly = TRUE)) install.packages("grid")
if (!requireNamespace("gtable", quietly = TRUE)) install.packages("gtable")

library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)



# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable_x <- "variable1"   # <-- EDITAR AQUÍ
nombre_variable_y <- "variable2"   # <-- EDITAR AQUÍ

etiqueta_x <- "etiqueta-var1"             # <-- EDITAR AQUÍ
etiqueta_y <- "etiqueta-var2"             # <-- EDITAR AQUÍ
titulo_grafico <- "Titulo"   # <-- EDITAR AQUÍ

color_puntos <- "blue"            # <-- EDITAR AQUÍ
alpha_puntos <- 0.7               # <-- EDITAR AQUÍ

color_hist <- "gold2"             # <-- EDITAR AQUÍ
color_borde_hist <- "grey30"      # <-- EDITAR AQUÍ

mostrar_recta_lm <- TRUE          # <-- TRUE / FALSE
guardar_figura <- TRUE            # <-- TRUE / FALSE

ancho_figura <- 10
alto_figura  <- 8
dpi_figura   <- 300



# Comprobaciones ====

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
data_plot <- data[complete.cases(data[, c(nombre_variable_x, nombre_variable_y)]), ]

if (nrow(data_plot) == 0) {
  stop("No quedan observaciones válidas tras eliminar NA en las variables seleccionadas.")
}

Variable_X <- data_plot[[nombre_variable_x]]
Variable_Y <- data_plot[[nombre_variable_y]]

cat("\nVariable X seleccionada:", nombre_variable_x, "\n")
cat("Variable Y seleccionada:", nombre_variable_y, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")



# Gráficos individuales ====

hist_top <- ggplot(data_plot, aes_string(x = nombre_variable_x, y = "after_stat(count)")) +
  geom_histogram(
    color = color_borde_hist,
    fill = color_hist,
    alpha = 0.9
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text = element_text(colour = "black")
  )

empty <- ggplot() +
  geom_point(aes(x = 1, y = 1), colour = "white") +
  theme_void()

scatter <- ggplot(data_plot, aes_string(x = nombre_variable_x, y = nombre_variable_y)) +
  geom_point(colour = color_puntos, alpha = alpha_puntos) +
  labs(x = etiqueta_x, y = etiqueta_y) +
  theme_minimal() +
  theme(
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    axis.title = element_text(face = "bold")
  )

if (mostrar_recta_lm) {
  scatter <- scatter + geom_smooth(method = "lm", se = TRUE)
}

hist_right <- ggplot(data_plot, aes_string(x = nombre_variable_y, y = "after_stat(count)")) +
  geom_histogram(
    color = color_borde_hist,
    fill = color_hist,
    alpha = 0.9
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text = element_text(colour = "black")
  )

titulo_panel <- textGrob(
  titulo_grafico,
  gp = gpar(fontsize = 18, fontface = 2)
)



# Panel compuesto ====

panel <- arrangeGrob(
  hist_top, empty, scatter, hist_right,
  ncol = 2,
  nrow = 2,
  widths = c(4, 2),
  heights = c(2, 4)
)

grid.arrange(panel, top = titulo_panel)



# Ajuste lineal ====

modeloLM <- lm(Variable_Y ~ Variable_X)

cat("\n=====================================\n")
cat(" Resumen del modelo lineal\n")
cat("=====================================\n")
print(summary(modeloLM))

cat("\nCoeficientes del modelo:\n")
print(coef(modeloLM))

cat("\nIntervalos de confianza (95%):\n")
print(confint(modeloLM))

cat("\nR²:\n")
print(summary(modeloLM)$r.squared)



# Guardar figura ====

if (guardar_figura) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0(
      nombre_dataset, "_",
      nombre_variable_x, "_",
      nombre_variable_y,
      "_scatter_hist.png"
    )
  )
  
  ggsave(
    filename = nombre_figura,
    plot = panel,
    width = ancho_figura,
    height = alto_figura,
    dpi = dpi_figura
  )
  
  cat("\n=====================================\n")
  cat(" Figura guardada\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_figura, "\n\n")
}