#####################################################
#
#  002. Histograma y densidad con ggplot
#
#  Descripción:
#  Genera un histograma de una variable numérica y
#  superpone su curva de densidad usando ggplot2.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/002_dataset_variable_hist_dens.png o .pdf
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


# Archivo de datos  ====


ruta_datos <- "datos/brutos/fichero.csv" # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

datos <- read.csv(ruta_datos, sep = ",") # <-- ajustar si necesario

cat("Columnas disponibles en el dataset:\n")
print(colnames(datos))


# ⚙️ PARÁMETROS A EDITAR  ====


nombre_variable <- "variable"   # <-- EDITAR AQUÍ

numero_clases <- 10             # <-- EDITAR AQUÍ

etiqueta_x <- "Etiqueta x"     # <-- EDITAR AQUÍ
etiqueta_y <- "densidad"               # <-- EDITAR AQUÍ
titulo_grafico <- "Histograma y densidad"   # <-- EDITAR AQUÍ
subtitulo_grafico <- " data set"                 # <-- EDITAR AQUÍ

# Estética
color_hist <- "orange"
color_dens <- "blue"

# Guardado
guardar_figura <- TRUE         # <-- TRUE / FALSE
formato_figura <- "pdf"        # <-- "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300            # solo PNG


# Comprobaciones  ====


if (!nombre_variable %in% colnames(datos)) {
  stop(paste("La variable", shQuote(nombre_variable), "no existe en el fichero de datos."))
}

if (!is.numeric(datos[[nombre_variable]])) {
  stop(paste("La variable", shQuote(nombre_variable), "debe ser numérica."))
}

datos <- datos[!is.na(datos[[nombre_variable]]), ]

if (nrow(datos) == 0) {
  stop("No quedan datos válidos tras eliminar NA.")
}

cat("\nVariable seleccionada:", nombre_variable, "\n")
cat("Número de observaciones:", nrow(datos), "\n")


# Gráfico  ====


grafico <- ggplot(datos, aes(x = .data[[nombre_variable]])) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = numero_clases,
    color = "black",
    fill = color_hist,
    alpha = 0.3
  ) +
  geom_density(
    color = color_dens,
    linewidth = 1,
    alpha = 0.8
  ) +
  labs(
    x = etiqueta_x,
    y = etiqueta_y,
    title = titulo_grafico,
    subtitle = subtitulo_grafico
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )

print(grafico)


# Guardar figura  ====


if (guardar_figura) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0(
      "002_",
      nombre_dataset, "_",
      nombre_variable,
      "_hist_dens.",
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
