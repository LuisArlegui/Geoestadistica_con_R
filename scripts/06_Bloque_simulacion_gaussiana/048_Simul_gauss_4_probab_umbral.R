#####################################################
#
#  048. Simulación gaussiana 4: probabilidad de superar un umbral
#
#  Descripción:
#  Calcula, celda a celda, la proporción de simulaciones
#  que alcanzan o superan un umbral definido. El resultado
#  se interpreta como la probabilidad espacial empírica
#  de excedencia.
#
#  Requisito previo:
#  - Ejecutar antes los scripts 045 y 046, que guardan:
#    modelos/045_dataset_variable_rejilla.rds
#    modelos/046_dataset_variable_simulaciones_originales.rds
#
#  Entrada:
#  - modelos/045_dataset_variable_rejilla.rds
#  - modelos/046_dataset_variable_simulaciones_originales.rds
#
#  Salida:
#  - salidas/simulaciones/048_dataset_variable_probabilidad_superar_umbral_N.tif
#  - salidas/simulaciones/048_dataset_variable_numero_superaciones_umbral_N.tif
#  - salidas/figuras/048_dataset_variable_probabilidad_superar_umbral_N.pdf o .png
#
#####################################################


# ==== Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("terra", "sf", "ggplot2", "viridis")

instalar_si_falta <- function(pkgs) {
  nuevos <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(nuevos) > 0) {
    message("Instalando paquetes faltantes: ", paste(nuevos, collapse = ", "))
    install.packages(nuevos, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

instalar_si_falta(paquetes_requeridos)


# ==== Archivo de datos base ====

ruta_datos <- "datos/brutos/meuse.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}


# ==== Nombre del dataset ====

nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))


# ==== Parámetros a editar ====

nombre_variable <- "zinc"                # <-- EDITAR AQUÍ

umbral <- 150                            # Umbral crítico de análisis (ppm, mg/kg, etc.)

guardar_rasters <- TRUE
guardar_figura  <- TRUE
formato_figura  <- "pdf"                 # "png" o "pdf"

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300                     # Solo para PNG


# ==== Archivos de entrada ====

ruta_sim_originales <- file.path(
  "modelos",
  paste0("046_", nombre_dataset, "_", nombre_variable, "_simulaciones_originales.rds")
)

ruta_rejilla <- file.path(
  "modelos",
  paste0("045_", nombre_dataset, "_", nombre_variable, "_rejilla.rds")
)


# ==== Comprobaciones ====

if (!file.exists(ruta_sim_originales)) {
  stop(
    paste(
      "No se encuentra el fichero:",
      ruta_sim_originales,
      "\nEjecuta antes el script 046."
    )
  )
}

if (!file.exists(ruta_rejilla)) {
  stop(
    paste(
      "No se encuentra el fichero:",
      ruta_rejilla,
      "\nEjecuta antes el script 045."
    )
  )
}

if (!is.numeric(umbral) || length(umbral) != 1) {
  stop("umbral debe ser un único valor numérico.")
}


# ==== Cargar simulaciones y plantilla raster ====

simulaciones_sf <- readRDS(ruta_sim_originales)
r_template      <- readRDS(ruta_rejilla)

cat("\n=====================================\n")
cat(" Archivos cargados exitosamente:\n")
cat("=====================================\n")
cat("Simulaciones:\n",     ruta_sim_originales, "\n")
cat("Plantilla raster:\n", ruta_rejilla,        "\n\n")


# ==== Comprobar plantilla raster ====

if (!inherits(r_template, "SpatRaster")) {
  stop("El objeto guardado en ruta_rejilla no es un SpatRaster. Revisa el script 045.")
}

cat("Características de la plantilla raster:\n")
cat("Filas:", nrow(r_template), "\n")
cat("Columnas:", ncol(r_template), "\n")
cat("Número de celdas:", ncell(r_template), "\n")
cat("Resolución X:", res(r_template)[1], "\n")
cat("Resolución Y:", res(r_template)[2], "\n")


# ==== Comprobar correspondencia espacial ====

if (nrow(simulaciones_sf) != ncell(r_template)) {
  stop(
    paste(
      "El número de localizaciones de las simulaciones",
      "no coincide con el número de celdas de la plantilla raster."
    )
  )
}


# ==== Identificar simulaciones ====

simulaciones_df <- st_drop_geometry(simulaciones_sf)

columnas_sim <- grep(
  "^sim[0-9]+$",
  names(simulaciones_df),
  value = TRUE
)

if (length(columnas_sim) == 0) {
  stop("No se han encontrado columnas de simulación con prefijo 'sim'.")
}

cat("\nNúmero de simulaciones analizadas:", length(columnas_sim), "\n")
cat("Número de celdas:", nrow(simulaciones_df), "\n")
cat("Umbral evaluado:", umbral, "\n")


# ==== Calcular excedencias ====

simulation_values <- as.matrix(
  simulaciones_df[, columnas_sim, drop = FALSE]
)

if (!is.numeric(simulation_values)) {
  stop("Las columnas de simulación deben ser numéricas.")
}

if (any(!is.finite(simulation_values))) {
  stop(
    "Las simulaciones contienen valores no finitos. ",
    "Revisa los resultados del script 046."
  )
}

# Número de simulaciones
N <- ncol(simulation_values)

# Identificar superaciones del umbral
simulation_binary <- simulation_values >= umbral

# Número de superaciones por celda
simulaciones_df$numero_superaciones <- rowSums(
  simulation_binary
)

# Probabilidad empírica de superación
simulaciones_df$probabilidad_superacion <-
  simulaciones_df$numero_superaciones / N

cat("\nResumen del número de superaciones del umbral:\n")
print(summary(simulaciones_df$numero_superaciones))

cat("\nResumen de la probabilidad de superación:\n")
print(summary(simulaciones_df$probabilidad_superacion))


# ==== Crear rasters ====

r_numero <- r_template
r_prob   <- r_template

values(r_numero) <- simulaciones_df$numero_superaciones
values(r_prob)   <- simulaciones_df$probabilidad_superacion

names(r_numero) <- paste0(nombre_variable, "_numero_superaciones_", umbral)
names(r_prob)   <- paste0(nombre_variable, "_prob_superar_", umbral)


# ==== Preparar datos para visualización ====

prob_df <- as.data.frame(
  r_prob,
  xy = TRUE,
  na.rm = TRUE
)

nombre_capa_prob <- names(r_prob)


# ==== Visualización de la Probabilidad de Excedencia ====

grafico_prob <- ggplot(
  prob_df,
  aes(
    x = x,
    y = y,
    fill = .data[[nombre_capa_prob]]
  )
) +
  geom_raster() +
  scale_fill_viridis_c(
    option = "magma",
    limits = c(0, 1),
    name = "Probabilidad"
  ) +
  coord_sf(
    crs = st_crs(simulaciones_sf),
    expand = FALSE
  ) +
  labs(
    title = "Probabilidad de Superar el Umbral Crítico",
    subtitle = paste(nombre_variable, "≥", umbral),
    x = "X",
    y = "Y"
  ) +
  theme_gray() +   # Tema gris por defecto
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(colour = "black", size = 10)
  )

print(grafico_prob)


# ==== Guardar rasters ====

if (guardar_rasters) {
  
  dir_simulaciones <- "salidas/simulaciones"
  
  if (!dir.exists(dir_simulaciones)) {
    dir.create(dir_simulaciones, recursive = TRUE)
  }
  
  nombre_raster_numero <- file.path(
    dir_simulaciones,
    paste0("048_", nombre_dataset, "_", nombre_variable, "_numero_superaciones_umbral_", umbral, ".tif")
  )
  
  nombre_raster_prob <- file.path(
    dir_simulaciones,
    paste0("048_", nombre_dataset, "_", nombre_variable, "_probabilidad_superar_umbral_", umbral, ".tif")
  )
  
  writeRaster(r_numero, nombre_raster_numero, overwrite = TRUE)
  writeRaster(r_prob,   nombre_raster_prob,   overwrite = TRUE)
  
  cat("\n=====================================\n")
  cat(" Rasters guardados exitosamente en:\n")
  cat("=====================================\n")
  cat("Número de superaciones:\n", nombre_raster_numero, "\n")
  cat("Probabilidad de superación:\n", nombre_raster_prob, "\n\n")
}


# ==== Guardar figura ====

if (guardar_figura) {
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0("048_", nombre_dataset, "_", nombre_variable, "_probabilidad_superar_umbral_", umbral, ".", formato_figura)
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico_prob,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico_prob,
      width = ancho_figura,
      height = alto_figura
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("=====================================\n")
  cat(" Figura guardada exitosamente en:\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_figura, "\n\n")
}