#####################################################
#
#  020. Scatter Plot + 2 variables categóricas
#
#  Descripción:
#  Genera un gráfico de dispersión entre dos variables
#  numéricas, representando simultáneamente dos variables
#  categóricas mediante forma y color, útil para explorar
#  agrupaciones y diferencias entre categorías.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/020_dataset_varX_varY_cat1_cat2_scatter_2categs.png / .pdf
#
#####################################################


# Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("ggplot2", "viridis")

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

ruta_datos <- "datos/procesados/fichero_transf.csv"

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable_x    <- "variable_1"   # <-- EDITAR AQUÍ
nombre_variable_y    <- "variable_2"   # <-- EDITAR AQUÍ
nombre_variable_cat1 <- "var_categorica_1"   # <-- EDITAR AQUÍ (se asignará a FORMA)
nombre_variable_cat2 <- "var_categorica_2"   # <-- EDITAR AQUÍ (se asignará a COLOR)

titulo_grafico    <- "Título del gráfico"      # <-- EDITAR AQUÍ
subtitulo_grafico <- "Subtítulo del gráfico"   # <-- EDITAR AQUÍ
etiqueta_x        <- "Columna1"                # <-- EDITAR AQUÍ
etiqueta_y        <- "Columna2"                # <-- EDITAR AQUÍ

titulo_leyenda_forma <- "Leyenda 1 (Forma)"   # <-- EDITAR AQUÍ
titulo_leyenda_color <- "Leyenda 2 (Color)"   # <-- EDITAR AQUÍ

tam_puntos   <- 2.5   # <-- EDITAR AQUÍ
alpha_puntos <- 0.8   # <-- EDITAR AQUÍ

guardar_figura <- TRUE    # <-- TRUE / FALSE
formato_figura <- "pdf"   # <-- "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300       # solo para PNG


# Comprobaciones ====

if (!nombre_variable_x %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_x), "no existe en el dataset"))
}

if (!nombre_variable_y %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_y), "no existe en el dataset"))
}

if (!nombre_variable_cat1 %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_cat1), "no existe en el dataset"))
}

if (!nombre_variable_cat2 %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_cat2), "no existe en el dataset"))
}

if (!is.numeric(data[[nombre_variable_x]])) {
  stop(paste("La variable", shQuote(nombre_variable_x), "debe ser numérica"))
}

if (!is.numeric(data[[nombre_variable_y]])) {
  stop(paste("La variable", shQuote(nombre_variable_y), "debe ser numérica"))
}

# Filtrar filas completas en las cuatro variables
cols_interes <- c(nombre_variable_x, nombre_variable_y, nombre_variable_cat1, nombre_variable_cat2)
data_plot <- data[complete.cases(data[, cols_interes]), , drop = FALSE]

if (nrow(data_plot) == 0) {
  stop("No quedan observaciones válidas tras eliminar NA en las variables seleccionadas.")
}

# Comprobación del número de niveles en la variable de FORMA
niveles_cat1 <- length(unique(data_plot[[nombre_variable_cat1]]))
if (niveles_cat1 > 6) {
  warning(paste("La variable", shQuote(nombre_variable_cat1), "tiene", niveles_cat1, 
                "categorías. ggplot2 soporta un máximo de 6 formas por defecto."))
}

cat("\nVariable X seleccionada:", nombre_variable_x, "\n")
cat("Variable Y seleccionada:", nombre_variable_y, "\n")
cat("Variable categórica 1 (forma):", nombre_variable_cat1, "\n")
cat("Variable categórica 2 (color):", nombre_variable_cat2, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")


# Gráfico ====

grafico <- ggplot(
  data_plot,
  aes(
    x = .data[[nombre_variable_x]],
    y = .data[[nombre_variable_y]]
  )
) +
  geom_point(
    aes(
      shape = factor(.data[[nombre_variable_cat1]]),
      colour = factor(.data[[nombre_variable_cat2]])
    ),
    size = tam_puntos,
    alpha = alpha_puntos
  ) +
  scale_color_viridis_d(
    option = "viridis",
    name = titulo_leyenda_color
  ) +
  labs(
    x = etiqueta_x,
    y = etiqueta_y,
    title = titulo_grafico,
    subtitle = subtitulo_grafico,
    shape = titulo_leyenda_forma,
    colour = titulo_leyenda_color
  ) +
  theme_gray() +   # Tema base estructurado
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      color = "black"
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      face = "italic",
      color = "gray30"
    ),
    axis.title = element_text(
      face = "bold",
      size = 11
    ),
    axis.text = element_text(
      colour = "black",
      size = 10
    ),
    legend.title = element_text(
      face = "bold"
    )
  )

# Mostrar en consola/pantalla
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
    paste0("020_",
           nombre_dataset, "_",
           nombre_variable_x, "_",
           nombre_variable_y, "_",
           nombre_variable_cat1, "_",
           nombre_variable_cat2,
           "_scatter_2categs.",
           formato_figura
    )
  )
  
  if (formato_figura %in% c("png", "pdf")) {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico,
      width = ancho_figura,
      height = alto_figura,
      dpi = if (formato_figura == "png") dpi_figura else NULL
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("\n-------------------------------------\n")
  cat(" Figura guardada exitosamente en:\n")
  cat(nombre_figura, "\n\n")
}