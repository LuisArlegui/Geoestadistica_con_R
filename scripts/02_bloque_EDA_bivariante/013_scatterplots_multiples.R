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
#  - datos/brutos/meuse.csv
#
#  Salida:
#  - matriz gráfica en la ventana gráfica de R
#  - salidas/figuras/013_dataset_scatterplots_multiples.pdf (o .png)
#
#####################################################


# ==== Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("ggplot2", "GGally")

instalar_si_falta <- function(pkgs) {
  nuevos <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(nuevos) > 0) {
    message("Instalando paquetes faltantes: ", paste(nuevos, collapse = ", "))
    install.packages(nuevos, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

instalar_si_falta(paquetes_requeridos)


# ==== Archivo de datos ====

ruta_datos <- "datos/brutos/meuse.csv"

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ==== ⚙️ PARÁMETROS A EDITAR ====

# Vector con las variables a incluir en la matriz
variables_grafico <- c(
  "zinc",
  "copper",
  "lead",
  "cadmium"
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

# Nombre del archivo de figura y formato
sufijo_figura  <- "scatterplots_multiples"   # <-- EDITAR AQUÍ
guardar_figura <- TRUE                       # <-- TRUE / FALSE
formato_figura <- "pdf"                      # <-- "png" o "pdf"

# Dimensiones de exportación
ancho_figura <- 12   # pulgadas
alto_figura  <- 12   # pulgadas
dpi_figura   <- 300  # Solo para PNG


# ==== Comprobaciones ====

faltan <- variables_grafico[!variables_grafico %in% colnames(data)]

if (length(faltan) > 0) {
  stop(
    paste(
      "Las siguientes variables no existen en el dataset:",
      paste(faltan, collapse = ", ")
    )
  )
}

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

data_plot <- data[complete.cases(data[variables_grafico]), variables_grafico, drop = FALSE]

if (nrow(data_plot) == 0) {
  stop("No quedan filas completas tras eliminar NA en las variables seleccionadas.")
}

cat("\nNúmero de observaciones válidas:", nrow(data_plot), "\n")


# ==== Generar gráfico ====

plot_matriz <- ggpairs(
  data_plot,
  columns = 1:ncol(data_plot),
  lower = list(
    continuous = wrap(
      "points", 
      color = color_puntos, 
      alpha = alpha_puntos, 
      size  = tam_puntos
    )
  ),
  diag = list(
    continuous = wrap(
      "densityDiag", 
      color     = color_densidad, 
      fill      = relleno_densidad, 
      alpha     = alpha_densidad, 
      linewidth = grosor_densidad
    )
  ),
  upper = list(
    continuous = wrap("cor", size = 4)
  )
) + theme_gray()   # Tema por defecto estandarizado

# Mostrar el gráfico en la ventana gráfica
print(plot_matriz)


# ==== Guardar figura ====

if (guardar_figura) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_figuras    <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0("013_", nombre_dataset, "_", sufijo_figura, ".", formato_figura)
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura,
      plot     = plot_matriz,
      width    = ancho_figura,
      height   = alto_figura,
      dpi      = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura,
      plot     = plot_matriz,
      width    = ancho_figura,
      height   = alto_figura
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("\n=====================================\n")
  cat(" Figura guardada exitosamente en:\n")
  cat("=====================================\n")
  cat(nombre_figura, "\n\n")
}