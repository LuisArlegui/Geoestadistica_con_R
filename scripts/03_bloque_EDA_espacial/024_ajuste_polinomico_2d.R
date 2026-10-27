#####################################################
#
#  024. Ajuste polinómico 2D
#
#  Descripción:
#  Genera un gráfico de dispersión entre dos variables
#  numéricas y superpone ajustes de regresión lineal,
#  polinómica de segundo orden y polinómica de tercer
#  orden, útil para comparar tendencias y curvaturas.
#
#  Entrada:
#  - datos/brutos/wolfcamp.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/figuras/024_dataset_varX_varY_ajuste_polinomico.png / .pdf
#
#####################################################


# Cargar e instalar librerías automáticamente ====

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


# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable_x <- "coord_y"    # <-- EDITAR AQUÍ
nombre_variable_y <- "variable"   # <-- EDITAR AQUÍ

Titulo_graf    <- "Ajustes Polinómicos de Regresión" # <-- EDITAR AQUÍ
Subtitulo_graf <- "Comparación de tendencias (1º, 2º y 3º grado)" # <-- EDITAR AQUÍ
Etiqueta_x     <- "Coordenada Y (Norte)"  # <-- EDITAR AQUÍ
Etiqueta_y     <- "Nivel Piezométrico (GW)" # <-- EDITAR AQUÍ

# Mostrar u ocultar cada ajuste
mostrar_lineal  <- TRUE         # <-- TRUE / FALSE
mostrar_segundo <- TRUE         # <-- TRUE / FALSE
mostrar_tercero <- TRUE         # <-- TRUE / FALSE

# Mostrar banda de confianza
mostrar_banda_confianza <- FALSE   # <-- TRUE / FALSE

# Guardado de figura
guardar_figura <- TRUE         # <-- TRUE / FALSE
formato_figura <- "pdf"        # <-- "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300            # solo PNG


# Comprobaciones ====

if (!nombre_variable_x %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_x), "no existe en el dataset"))
}

if (!nombre_variable_y %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable_y), "debe ser numérica"))
}

if (!is.numeric(data[[nombre_variable_x]])) {
  stop(paste("La variable", shQuote(nombre_variable_x), "debe ser numérica"))
}

if (!is.numeric(data[[nombre_variable_y]])) {
  stop(paste("La variable", shQuote(nombre_variable_y), "debe ser numérica"))
}

# Filtrar filas completas
datos_plot <- data[
  complete.cases(data[, c(nombre_variable_x, nombre_variable_y)]),
  c(nombre_variable_x, nombre_variable_y),
  drop = FALSE
]

if (nrow(datos_plot) == 0) {
  stop("No quedan observaciones válidas tras eliminar NA en las variables seleccionadas.")
}

colnames(datos_plot) <- c("X", "Y")

cat("\nVariable X seleccionada:", nombre_variable_x, "\n")
cat("Variable Y seleccionada:", nombre_variable_y, "\n")
cat("Número de observaciones válidas:", nrow(datos_plot), "\n")


# Gráfico base ====

grafico <- ggplot(
  datos_plot,
  aes(
    x = .data[["X"]],
    y = .data[["Y"]]
  )
) +
  geom_point(
    colour = "gray30",
    alpha = 0.7,
    size = 2
  ) +
  labs(
    x = Etiqueta_x,
    y = Etiqueta_y,
    title = Titulo_graf,
    subtitle = Subtitulo_graf,
    colour = "Tipo de regresión"
  ) +
  theme_gray() +   # Tema estructurado limpio
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


# Añadir ajustes polinómicos ====

if (mostrar_lineal) {
  grafico <- grafico +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = mostrar_banda_confianza,
      linewidth = 1.1,
      aes(colour = "Lineal")
    )
}

if (mostrar_segundo) {
  grafico <- grafico +
    geom_smooth(
      method = "lm",
      formula = y ~ poly(x, 2, raw = TRUE),
      se = mostrar_banda_confianza,
      linewidth = 1.1,
      aes(colour = "Segundo_orden")
    )
}

if (mostrar_tercero) {
  grafico <- grafico +
    geom_smooth(
      method = "lm",
      formula = y ~ poly(x, 3, raw = TRUE),
      se = mostrar_banda_confianza,
      linewidth = 1.1,
      aes(colour = "Tercer_orden")
    )
}

# Asignación manual de colores a la leyenda
grafico <- grafico +
  scale_color_manual(
    values = c(
      "Lineal"        = "firebrick",
      "Segundo_orden" = "royalblue",
      "Tercer_orden"  = "forestgreen"
    )
  )

# Mostrar mapa en la ventana gráfica
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
    paste0("024_",
           nombre_dataset, "_",
           nombre_variable_x, "_",
           nombre_variable_y,
           "_ajuste_polinomico.",
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
      height = alto_figura
    )
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("\n=====================================\n")
  cat(" Figura guardada exitosamente en:\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_figura, "\n\n")
}