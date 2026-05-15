#####################################################
#
#  033. Variograma experimental direccional
#
#  Descripción:
#  Calcula y representa el variograma experimental
#  direccional de una variable regionalizada, utilizando
#  una dirección principal y una tolerancia angular.
#  Es útil para explorar anisotropía espacial.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/dataset_variable_variograma_direccional_AAA.png o .pdf
#  - (opcional) salidas/tablas/dataset_variable_variograma_direccional_AAA.csv
#
#####################################################



# Cargar librerías ====

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gstat", quietly = TRUE)) install.packages("gstat")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(ggplot2)
library(gstat)
library(sf)



# Archivo de datos ====

ruta_datos <- "datos/procesados/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

nombre_x <- "columnaX"                 # <-- EDITAR AQUÍ
nombre_y <- "columnaY"                 # <-- EDITAR AQUÍ
nombre_variable <- "objetivo"      # <-- EDITAR AQUÍ

epsg_datos <- 12345             # <-- EDITAR AQUÍ

cutoff_variograma <- 1234       # distancia máxima
lag_variograma <- 12            # anchura de lag

angulo_direccion <- 45          # dirección del variograma, en grados EDITAR
tolerancia_angulo <- 22.5       # tolerancia angular, en grados

Label_x <- "Distancia"
Label_y <- "Semivarianza"
Title_graph <- "Variograma experimental direccional"
Subtitle_graph <- paste0(
  "Dirección: ",
  angulo_direccion,
  "° ± ",
  tolerancia_angulo,
  "°"
)

# Estética
color_relleno <- "orange"
color_borde <- "darkred"
tam_puntos <- 2
mostrar_np <- TRUE

# Guardado
guardar_figura <- TRUE
formato_figura <- "pdf"         # "png" o "pdf"
guardar_tabla <- TRUE

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300             # solo PNG



# Comprobaciones ====

vars <- c(nombre_x, nombre_y, nombre_variable)

faltan <- vars[!vars %in% colnames(data)]

if (length(faltan) > 0) {
  stop(paste("No existen estas columnas en el dataset:", paste(faltan, collapse = ", ")))
}

vars_no_num <- vars[!sapply(data[vars], is.numeric)]

if (length(vars_no_num) > 0) {
  stop(paste("Estas variables deben ser numéricas:", paste(vars_no_num, collapse = ", ")))
}

if (cutoff_variograma <= 0) {
  stop("cutoff_variograma debe ser mayor que cero.")
}

if (lag_variograma <= 0) {
  stop("lag_variograma debe ser mayor que cero.")
}

if (lag_variograma >= cutoff_variograma) {
  warning("lag_variograma es mayor o igual que cutoff_variograma; se generarán muy pocas clases.")
}

if (tolerancia_angulo <= 0 || tolerancia_angulo > 90) {
  stop("tolerancia_angulo debe estar entre 0 y 90 grados.")
}

data_plot <- data[
  complete.cases(data[, vars]),
  vars,
  drop = FALSE
]

if (nrow(data_plot) < 2) {
  stop("Se necesitan al menos dos observaciones válidas para calcular el variograma.")
}

cat("\nCoordenada X:", nombre_x, "\n")
cat("Coordenada Y:", nombre_y, "\n")
cat("Variable:", nombre_variable, "\n")
cat("EPSG:", epsg_datos, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")
cat("Cutoff:", cutoff_variograma, "\n")
cat("Lag:", lag_variograma, "\n")
cat("Dirección:", angulo_direccion, "grados\n")
cat("Tolerancia angular:", tolerancia_angulo, "grados\n")



# Conversión a sf ====

data_sf <- st_as_sf(
  data_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)



# Variograma experimental direccional ====

formula_variograma <- as.formula(
  paste(nombre_variable, "~ 1")
)

g <- gstat(
  id = nombre_variable,
  formula = formula_variograma,
  data = data_sf
)

variogram_data <- variogram(
  g,
  cutoff = cutoff_variograma,
  width = lag_variograma,
  alpha = angulo_direccion,
  tol.hor = tolerancia_angulo
)

if (nrow(variogram_data) == 0) {
  stop("El variograma direccional no contiene clases. Revisa cutoff, lag, dirección o tolerancia.")
}

cat("\nVariograma direccional calculado:\n")
print(variogram_data)



# Elaboracion del grafico ====

# Límites del gráfico 

max_gamma <- max(variogram_data$gamma, na.rm = TRUE)
ylim_max <- max_gamma * 1.1




# Gráfico

grafico <- ggplot(variogram_data, aes(x = dist, y = gamma)) +
  geom_point(
    shape = 21,
    fill = color_relleno,
    color = color_borde,
    size = tam_puntos
  ) +
  labs(
    x = Label_x,
    y = Label_y,
    title = Title_graph,
    subtitle = Subtitle_graph
  ) +
  xlim(0, cutoff_variograma) +
  ylim(0, ylim_max) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )

if (mostrar_np) {
  grafico <- grafico +
    geom_text(
      aes(label = np),
      hjust = 1.15,
      vjust = 0,
      size = 3
    )
}

print(grafico)



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
      nombre_variable,
      "_variograma_direccional_",
      sprintf("%03d", round(angulo_direccion)),
      ".",
      formato_figura
    )
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico,
      width = ancho_figura,
      height = alto_figura,
      device = cairo_pdf
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("\n=====================================\n")
  cat(" Figura guardada\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_figura, "\n\n")
}



# Guardar tabla del variograma direccional ====

if (guardar_tabla) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_tablas <- "salidas/tablas"
  
  if (!dir.exists(dir_tablas)) {
    dir.create(dir_tablas, recursive = TRUE)
  }
  
  nombre_tabla <- file.path(
    dir_tablas,
    paste0(
      nombre_dataset, "_",
      nombre_variable,
      "_variograma_direccional_",
      sprintf("%03d", round(angulo_direccion)),
      ".csv"
    )
  )
  
  write.csv(variogram_data, nombre_tabla, row.names = FALSE)
  
  cat("\n=====================================\n")
  cat(" Tabla guardada\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_tabla, "\n\n")
}