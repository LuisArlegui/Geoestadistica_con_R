#####################################################
#
#  049. Simulación gaussiana 5: niveles de contaminación
#
#  Descripción:
#  Evalúa, para cada simulación, la superficie ocupada
#  por distintos niveles de contaminación.
#
#  Calcula el número de celdas y el área correspondiente
#  a cada clase, genera histogramas de las superficies
#  simuladas y obtiene curvas empíricas de probabilidad
#  de excedencia de superficie.
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
#  - salidas/tablas/049_dataset_variable_estadisticas_niveles_contaminacion.csv
#  - salidas/tablas/049_dataset_variable_conteos_niveles_contaminacion.csv
#  - salidas/tablas/049_dataset_variable_curvas_excedencia_contaminacion.csv
#  - salidas/figuras/049_dataset_variable_histogramas_niveles_contaminacion.pdf o .png
#  - salidas/figuras/049_dataset_variable_curvas_excedencia_contaminacion.pdf o .png
#
#####################################################


# ==== Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("terra", "sf", "ggplot2", "gridExtra", "grid")

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

# Niveles de contaminación:
# Convención: Min <= valor < Max (excepto último nivel cuyo Max es Inf)
niveles <- data.frame(
  Nivel = c("Bajo", "Medio", "Alto", "Muy alto"),
  Min   = c(250, 500, 750, 1000),
  Max   = c(500, 750, 1000, Inf)
)

numero_bins <- 10

# Guardado
guardar_tablas <- TRUE
guardar_figura <- TRUE

formato_figura <- "pdf"                 # "png" o "pdf"

ancho_figura <- 10
alto_figura  <- 8
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

if (!all(c("Nivel", "Min", "Max") %in% names(niveles))) {
  stop("El data frame 'niveles' debe tener las columnas Nivel, Min y Max.")
}

if (any(niveles$Min >= niveles$Max)) {
  stop("Cada valor Min debe ser menor que su correspondiente Max.")
}

if (numero_bins < 1) {
  stop("numero_bins debe ser al menos 1.")
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


# ==== Comprobar correspondencia espacial ====

if (nrow(simulaciones_df) != ncell(r_template)) {
  stop("El número de localizaciones de las simulaciones no coincide con el número de celdas de la plantilla raster.")
}

cat("Número de simulaciones analizadas:", length(columnas_sim), "\n")
cat("Número de celdas por simulación:", ncell(r_template), "\n")


# ==== Calcular área de celda ====

resolucion <- res(r_template)
area_celda_m2 <- abs(resolucion[1] * resolucion[2])

cat("\nResolución de la rejilla:\n")
cat("X:", resolucion[1], "m | Y:", resolucion[2], "m\n")
cat("Área de cada celda:", area_celda_m2, "m² (", area_celda_m2 / 10000, "ha )\n")


# ==== Mostrar niveles definidos ====

cat("\nNiveles de contaminación definidos:\n")
print(niveles)


# ==== Calcular conteos y áreas por nivel ====

lista_conteos <- list()

for (i in seq_len(nrow(niveles))) {
  
  nivel_i <- niveles$Nivel[i]
  min_i   <- niveles$Min[i]
  max_i   <- niveles$Max[i]
  
  conteos_i <- numeric(length(columnas_sim))
  
  for (j in seq_along(columnas_sim)) {
    valores_sim <- simulaciones_df[[columnas_sim[j]]]
    conteos_i[j] <- sum(valores_sim >= min_i & valores_sim < max_i, na.rm = TRUE)
  }
  
  lista_conteos[[nivel_i]] <- data.frame(
    Nivel       = nivel_i,
    Simulacion  = seq_along(columnas_sim),
    Pixel_count = conteos_i,
    Area_m2     = conteos_i * area_celda_m2,
    Area_ha     = conteos_i * area_celda_m2 / 10000
  )
}

tabla_conteos <- do.call(rbind, lista_conteos)
row.names(tabla_conteos) <- NULL

cat("\nVista previa de conteos por nivel:\n")
print(head(tabla_conteos))


# ==== Calcular estadísticas por nivel ====

estadisticas_niveles <- do.call(
  rbind,
  lapply(
    split(tabla_conteos, tabla_conteos$Nivel),
    function(df) {
      data.frame(
        Nivel                       = unique(df$Nivel),
        Media_pixeles               = mean(df$Pixel_count, na.rm = TRUE),
        Mediana_pixeles             = median(df$Pixel_count, na.rm = TRUE),
        Desviacion_estandar_pixeles = sd(df$Pixel_count, na.rm = TRUE),
        Varianza_pixeles            = var(df$Pixel_count, na.rm = TRUE),
        Minimo_pixeles              = min(df$Pixel_count, na.rm = TRUE),
        Maximo_pixeles              = max(df$Pixel_count, na.rm = TRUE),
        Media_ha                    = mean(df$Area_ha, na.rm = TRUE),
        Mediana_ha                  = median(df$Area_ha, na.rm = TRUE),
        Desviacion_estandar_ha      = sd(df$Area_ha, na.rm = TRUE),
        Varianza_ha                 = var(df$Area_ha, na.rm = TRUE),
        Minimo_ha                   = min(df$Area_ha, na.rm = TRUE),
        Maximo_ha                   = max(df$Area_ha, na.rm = TRUE)
      )
    }
  )
)

row.names(estadisticas_niveles) <- NULL

cat("\n=====================================\n")
cat(" Estadísticas de superficie por nivel\n")
cat("=====================================\n")
print(estadisticas_niveles)


# ==== Función auxiliar para crear histogramas por nivel ====

crear_histograma_nivel <- function(df, nivel, min_val, max_val) {
  
  media   <- mean(df$Area_ha, na.rm = TRUE)
  mediana <- median(df$Area_ha, na.rm = TRUE)
  desv    <- sd(df$Area_ha, na.rm = TRUE)
  
  if (is.infinite(max_val)) {
    subtitulo <- paste0(nombre_variable, " ≥ ", min_val)
  } else {
    subtitulo <- paste0(min_val, " ≤ ", nombre_variable, " < ", max_val)
  }
  
  ggplot(df, aes(x = Area_ha)) +
    geom_histogram(
      bins = numero_bins,
      fill = "steelblue",
      color = "black",
      alpha = 0.7
    ) +
    geom_vline(
      xintercept = media,
      color = "red",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_vline(
      xintercept = mediana,
      color = "darkgreen",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    labs(
      title = paste("Nivel", nivel),
      subtitle = paste0(
        subtitulo,
        "\nMedia = ", round(media, 2),
        " ha | Mediana = ", round(mediana, 2),
        " ha | SD = ", round(desv, 2), " ha"
      ),
      x = "Superficie simulada (ha)",
      y = "Frecuencia"
    ) +
    theme_gray() +   # Tema gris por defecto
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5, color = "black"),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "black"),
      axis.title = element_text(face = "bold"),
      axis.text  = element_text(colour = "black", size = 9)
    )
}


# ==== Generar histogramas por nivel ====

graficos <- list()

for (i in seq_len(nrow(niveles))) {
  
  nivel_i <- niveles$Nivel[i]
  min_i   <- niveles$Min[i]
  max_i   <- niveles$Max[i]
  
  df_i <- tabla_conteos[tabla_conteos$Nivel == nivel_i, , drop = FALSE]
  
  graficos[[nivel_i]] <- crear_histograma_nivel(
    df = df_i,
    nivel = nivel_i,
    min_val = min_i,
    max_val = max_i
  )
}


# ==== Panel gráfico de histogramas ====

titulo_panel <- textGrob(
  "Superficie simulada por nivel de contaminación",
  gp = gpar(fontsize = 15, fontface = 2)
)

panel <- arrangeGrob(
  grobs = graficos,
  ncol = 2,
  top = titulo_panel
)

grid.arrange(panel)


# ==== Curvas de probabilidad de excedencia de superficie ====

curvas_excedencia <- do.call(
  rbind,
  lapply(
    split(tabla_conteos, tabla_conteos$Nivel),
    function(df) {
      
      areas <- df$Area_ha
      
      # Superficies distintas, ordenadas
      areas_ordenadas <- sort(unique(areas))
      
      # Probabilidad empírica de alcanzar o superar cada superficie
      probabilidades <- vapply(
        areas_ordenadas,
        function(a) mean(areas >= a),
        numeric(1)
      )
      
      data.frame(
        Nivel = unique(df$Nivel),
        Area_ha = areas_ordenadas,
        Probabilidad_superacion = probabilidades
      )
    }
  )
)

row.names(curvas_excedencia) <- NULL


# ==== Gráfico de curvas de excedencia ====

grafico_excedencia <- ggplot(
  curvas_excedencia,
  aes(
    x = Area_ha,
    y = Probabilidad_superacion,
    colour = Nivel
  )
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Probabilidad de excedencia por nivel de contaminación",
    subtitle = "Probabilidad P(Superficie simulada ≥ superficie umbral)",
    x = "Superficie umbral (ha)",
    y = "Probabilidad de excedencia",
    colour = "Nivel"
  ) +
  theme_gray() +   # Tema gris por defecto
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic", color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(colour = "black", size = 10),
    legend.title = element_text(face = "bold")
  )

print(grafico_excedencia)


# ==== Guardar tablas ====

if (guardar_tablas) {
  
  dir_tablas <- "salidas/tablas"
  
  if (!dir.exists(dir_tablas)) {
    dir.create(dir_tablas, recursive = TRUE)
  }
  
  nombre_tabla_conteos     <- file.path(dir_tablas, paste0("049_", nombre_dataset, "_", nombre_variable, "_conteos_niveles_contaminacion.csv"))
  nombre_tabla_estadisticas <- file.path(dir_tablas, paste0("049_", nombre_dataset, "_", nombre_variable, "_estadisticas_niveles_contaminacion.csv"))
  nombre_tabla_excedencia  <- file.path(dir_tablas, paste0("049_", nombre_dataset, "_", nombre_variable, "_curvas_excedencia_contaminacion.csv"))
  
  write.csv(tabla_conteos,        nombre_tabla_conteos,        row.names = FALSE)
  write.csv(estadisticas_niveles, nombre_tabla_estadisticas, row.names = FALSE)
  write.csv(curvas_excedencia,    nombre_tabla_excedencia,     row.names = FALSE)
  
  cat("\n=====================================\n")
  cat(" Tablas guardadas exitosamente en:\n")
  cat("=====================================\n")
  cat("Conteos:\n",              nombre_tabla_conteos,     "\n")
  cat("Estadísticas:\n",         nombre_tabla_estadisticas,"\n")
  cat("Curvas de excedencia:\n", nombre_tabla_excedencia,  "\n\n")
}


# ==== Guardar figuras ====

if (guardar_figura) {
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura_histogramas <- file.path(
    dir_figuras,
    paste0("049_", nombre_dataset, "_", nombre_variable, "_histogramas_niveles_contaminacion.", formato_figura)
  )
  
  nombre_figura_excedencia <- file.path(
    dir_figuras,
    paste0("049_", nombre_dataset, "_", nombre_variable, "_curvas_excedencia_contaminacion.", formato_figura)
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura_histogramas,
      plot = panel,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
    ggsave(
      filename = nombre_figura_excedencia,
      plot = grafico_excedencia,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura_histogramas,
      plot = panel,
      width = ancho_figura,
      height = alto_figura
    )
    
    ggsave(
      filename = nombre_figura_excedencia,
      plot = grafico_excedencia,
      width = ancho_figura,
      height = alto_figura
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("=====================================\n")
  cat(" Figuras guardadas exitosamente en:\n")
  cat("=====================================\n")
  cat("Histogramas:\n",           nombre_figura_histogramas, "\n")
  cat("Curvas de excedencia:\n",  nombre_figura_excedencia,  "\n\n")
}