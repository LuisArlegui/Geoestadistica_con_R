#####################################################
#
#  012. Scatter Plot simple
#
#  Descripción:
#  Genera un gráfico de dispersión simple entre dos variables
#  numéricas utilizando ggplot2 con theme_gray() para explorar
#  visualmente su relación, detectar tendencias y outliers.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/012_dataset_varX_varY_scatterplot.png / .pdf
#
#####################################################


# ==== Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("ggplot2")

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

ruta_datos <- "datos/brutos/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ==== ⚙️ PARÁMETROS A EDITAR ====

nombre_variable_x <- "variable_x"    # <-- EDITAR AQUÍ
nombre_variable_y <- "varibale_y"    # <-- EDITAR AQUÍ

titulo_grafico    <- "Gráfico de Dispersión Simple"   # <-- EDITAR AQUÍ
subtitulo_grafico <- "Relación bivariante"           # <-- EDITAR AQUÍ
etiqueta_x        <- "Etiqueta eje X"                # <-- EDITAR AQUÍ
etiqueta_y        <- "Etiqueta eje Y"                # <-- EDITAR AQUÍ

color_puntos <- "steelblue"          # <-- EDITAR AQUÍ
tam_puntos   <- 2.5                  # <-- EDITAR AQUÍ
alpha_puntos <- 0.7                  # <-- EDITAR AQUÍ

# Opciones de guardado
guardar_figura <- TRUE               # <-- TRUE / FALSE
formato_figura <- "pdf"              # <-- "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300                  # Solo para PNG


# ==== Comprobaciones ====

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

# Filtrar casos completos
data_plot <- data[complete.cases(data[, c(nombre_variable_x, nombre_variable_y)]), , drop = FALSE]

if (nrow(data_plot) == 0) {
  stop("No quedan observaciones válidas tras eliminar valores nulos (NA).")
}

cat("\nVariable X seleccionada:", nombre_variable_x, "\n")
cat("Variable Y seleccionada:", nombre_variable_y, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")


# ==== Gráfico con ggplot2 ====

grafico <- ggplot(
  data_plot,
  aes(
    x = .data[[nombre_variable_x]],
    y = .data[[nombre_variable_y]]
  )
) +
  geom_point(
    color = color_puntos,
    size  = tam_puntos,
    alpha = alpha_puntos
  ) +
  labs(
    title    = titulo_grafico,
    subtitle = subtitulo_grafico,
    x        = etiqueta_x,
    y        = etiqueta_y
  ) +
  theme_gray() +   # Tema estándar gris por defecto
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic", color = "black"),
    axis.title    = element_text(face = "bold", size = 11),
    axis.text     = element_text(colour = "black", size = 10)
  )

# Visualizar gráfico
print(grafico)


# ==== Guardar figura ====

if (guardar_figura) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_figuras    <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0(
      "012_",
      nombre_dataset, "_",
      nombre_variable_x, "_",
      nombre_variable_y,
      "_scatterplot.",
      formato_figura
    )
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura,
      plot     = grafico,
      width    = ancho_figura,
      height   = alto_figura,
      dpi      = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura,
      plot     = grafico,
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