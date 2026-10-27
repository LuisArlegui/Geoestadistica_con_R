#####################################################
#
#  047. Simulación gaussiana 3: promedio de simulaciones
#
#  Descripción:
#  Calcula el promedio celda a celda de todas las
#  simulaciones gaussianas ya revertidas a unidades
#  originales. El resultado representa el valor medio
#  simulado (media E-type) de la variable objetivo.
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
#  - salidas/simulaciones/047_dataset_variable_promedio_simulaciones.tif
#  - salidas/figuras/047_dataset_variable_promedio_simulaciones.pdf o .png
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

guardar_raster <- TRUE
guardar_figura <- TRUE
formato_figura <- "pdf"                 # "png" o "pdf"

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


# ==== Cargar simulaciones y plantilla raster ====

simulaciones_sf <- readRDS(ruta_sim_originales)
r_template      <- readRDS(ruta_rejilla)

cat("\n=====================================\n")
cat(" Archivos cargados exitosamente:\n")
cat("=====================================\n")
cat("Simulaciones:\n",       ruta_sim_originales, "\n")
cat("Plantilla raster:\n",   ruta_rejilla,        "\n\n")


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

cat("\nNúmero de simulaciones detectadas:", length(columnas_sim), "\n")
cat("Número de celdas analizadas:", nrow(simulaciones_df), "\n")


# ==== Calcular promedio E-Type ====

simulaciones_df$promedio <- rowMeans(
  simulaciones_df[, columnas_sim, drop = FALSE],
  na.rm = TRUE
)

cat("\nResumen del promedio (Media E-Type) de simulaciones:\n")
print(summary(simulaciones_df$promedio))


# ==== Crear raster del promedio ====

r_promedio <- r_template
values(r_promedio) <- simulaciones_df$promedio
names(r_promedio)  <- paste0(nombre_variable, "_promedio_simulaciones")


# ==== Preparar datos para visualización ====

promedio_df <- as.data.frame(
  r_promedio,
  xy = TRUE,
  na.rm = TRUE
)

nombre_capa <- names(r_promedio)


# ==== Visualización del Promedio (E-Type) ====

grafico_promedio <- ggplot(
  promedio_df,
  aes(
    x = x,
    y = y,
    fill = .data[[nombre_capa]]
  )
) +
  geom_raster() +
  scale_fill_viridis_c(name = nombre_variable) +
  coord_sf(
    crs = st_crs(simulaciones_sf),
    expand = FALSE
  ) +
  labs(
    title = "Promedio de Simulaciones Gaussianas (E-Type)",
    subtitle = paste(nombre_variable, "en unidades originales"),
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

print(grafico_promedio)


# ==== Guardar raster ====

if (guardar_raster) {
  
  dir_simulaciones <- "salidas/simulaciones"
  
  if (!dir.exists(dir_simulaciones)) {
    dir.create(dir_simulaciones, recursive = TRUE)
  }
  
  nombre_raster <- file.path(
    dir_simulaciones,
    paste0("047_", nombre_dataset, "_", nombre_variable, "_promedio_simulaciones.tif")
  )
  
  writeRaster(
    r_promedio,
    nombre_raster,
    overwrite = TRUE
  )
  
  cat("\n=====================================\n")
  cat(" Raster guardado exitosamente en:\n")
  cat("=====================================\n")
  cat("Promedio simulaciones:\n", nombre_raster, "\n\n")
}


# ==== Guardar figura ====

if (guardar_figura) {
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0("047_", nombre_dataset, "_", nombre_variable, "_promedio_simulaciones.", formato_figura)
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico_promedio,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura,
      plot = grafico_promedio,
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