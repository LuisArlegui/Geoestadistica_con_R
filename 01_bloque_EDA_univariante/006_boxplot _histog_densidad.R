#####################################################
#
#  006. Boxplot / histograma / densidad
#
#  Descripción:
#  Representa un gráfico compuesto por histograma,
#  curva de densidad kernel y boxplot de una variable
#  numérica, útil para visualizar su distribución,
#  tendencia central y posibles valores atípicos.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/dataset_variable_hist_dens_box.png o .pdf
#
#####################################################


# Cargar librerías ====


if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("cowplot", quietly = TRUE)) install.packages("cowplot")

library(ggplot2)
library(cowplot)


# Archivo de datos ====


ruta_datos <- "datos/brutos/meuse.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")    # <-- ajustar si hace falta



# Selección de variable ====


cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====


nombre_variable <- "zinc"   # <-- EDITAR AQUÍ

EtiquetaX <- "etiqueta del eje x"   # <-- EDITAR AQUÍ
EtiquetaY <- "densidad"             # <-- EDITAR AQUÍ
TituloGrafico <- "Histograma + densidad + boxplot"   # <-- EDITAR AQUÍ
SubTituloGrafico <- "subtítulo"     # <-- EDITAR AQUÍ

# Número de bins
numero_bins <- 12   # <-- EDITAR AQUÍ

# Límites del eje X:
# si quieres fijarlos manualmente, cambia usar_limites_manual a TRUE
usar_limites_manual <- FALSE   # <-- TRUE / FALSE

xMin <- 113    # <-- solo si usar_limites_manual = TRUE
xMax <- 1839   # <-- solo si usar_limites_manual = TRUE

# Posición vertical de las etiquetas de media y mediana
# si se deja NULL, se calcula automáticamente
pos_y_texto <- NULL   # <-- o por ejemplo 0.00235

# Estética
color_hist <- "aquamarine3"
color_dens_linea <- "blue"
color_dens_relleno <- "#56B4E9"
color_media <- "red"
color_mediana <- "grey30"

# Guardado
guardar_figura <- TRUE      # <-- TRUE / FALSE
formato_figura <- "pdf"     # <-- "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 7
dpi_figura   <- 300         # solo PNG



# Comprobaciones ====


if (!nombre_variable %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable), "no existe en el fichero de datos."))
}

if (!is.numeric(data[[nombre_variable]])) {
  stop(paste("La variable", shQuote(nombre_variable), "debe ser numérica."))
}

data_plot <- data[!is.na(data[[nombre_variable]]), , drop = FALSE]

if (nrow(data_plot) == 0) {
  stop("No quedan datos válidos tras eliminar NA.")
}

Parametro <- data_plot[[nombre_variable]]

cat("\nVariable seleccionada:", nombre_variable, "\n")
cat("Número de observaciones:", length(Parametro), "\n")



# Estadísticos básicos ====


media_valor <- mean(Parametro, na.rm = TRUE)
mediana_valor <- median(Parametro, na.rm = TRUE)

if (usar_limites_manual) {
  x_min_plot <- xMin
  x_max_plot <- xMax
} else {
  x_min_plot <- min(Parametro, na.rm = TRUE)
  x_max_plot <- max(Parametro, na.rm = TRUE)
}

intervalo_bin <- abs(x_max_plot - x_min_plot) / numero_bins

# Para colocar el texto de media y mediana
densidad_tmp <- density(Parametro, na.rm = TRUE)
if (is.null(pos_y_texto)) {
  pos_y_texto <- max(densidad_tmp$y, na.rm = TRUE) * 0.95
}

# Límites visuales algo ampliados
limite_x_inf <- x_min_plot * 0.9
limite_x_sup <- x_max_plot * 1.1


# Grafica ====

# Gráfico superior: histograma + densidad 

a <- ggplot(data_plot, aes_string(x = nombre_variable, y = "after_stat(density)")) +
  geom_histogram(
    breaks = seq(from = x_min_plot, to = x_max_plot, by = intervalo_bin),
    color = "grey30",
    fill = color_hist,
    alpha = 0.9
  ) +
  geom_density(
    alpha = 0.5,
    color = color_dens_linea,
    fill = color_dens_relleno
  ) +
  geom_vline(
    xintercept = media_valor,
    color = color_media,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_vline(
    xintercept = mediana_valor,
    color = color_mediana,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = media_valor,
    y = pos_y_texto,
    angle = 90,
    label = paste0("Media = ", round(media_valor, 2)),
    vjust = -0.2,
    color = color_media,
    size = 3.5
  ) +
  annotate(
    "text",
    x = mediana_valor,
    y = pos_y_texto,
    angle = 90,
    label = paste0("Mediana = ", round(mediana_valor, 2)),
    vjust = 1.2,
    color = color_mediana,
    size = 3.5
  ) +
  labs(
    x = EtiquetaX,
    y = EtiquetaY,
    title = TituloGrafico,
    subtitle = SubTituloGrafico
  ) +
  xlim(limite_x_inf, limite_x_sup) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold")
  )


# Gráfico inferior: boxplot 

b <- ggplot(data_plot, aes_string(x = nombre_variable)) +
  geom_boxplot(
    color = "grey30",
    fill = color_hist,
    alpha = 0.9
  ) +
  geom_vline(
    xintercept = media_valor,
    color = color_media,
    linewidth = 0.5
  ) +
  xlim(limite_x_inf, limite_x_sup) +
  theme(
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


# Panel compuesto 

panel <- plot_grid(
  a, b,
  align = "v",
  ncol = 1,
  rel_heights = c(1, 1/6)
)

print(panel)


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
      "_hist_dens_box.",
      formato_figura
    )
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura,
      plot = panel,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura,
      plot = panel,
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