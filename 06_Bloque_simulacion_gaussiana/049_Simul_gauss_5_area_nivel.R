#####################################################
#
#  049. Simulación gaussiana 5: niveles de contaminación
#
#  Descripción:
#  Evalúa, para cada simulación, la superficie ocupada
#  por distintos niveles de contaminación. Calcula el
#  número de celdas y el área correspondiente por clase,
#  genera histogramas de superficie simulada y exporta
#  una tabla resumen de estadísticas.
#
#  Requisito previo:
#  - Ejecutar antes el script 046, que guarda:
#    modelos/meuse_zinc_simulaciones_originales.rds
#
#  Entrada:
#  - modelos/meuse_zinc_simulaciones_originales.rds
#
#  Salida:
#  - salidas/tablas/meuse_zinc_estadisticas_niveles_contaminacion.csv
#  - salidas/tablas/meuse_zinc_conteos_niveles_contaminacion.csv
#  - salidas/figuras/meuse_zinc_histogramas_niveles_contaminacion.pdf o .png
#
#####################################################

# ==== Cargar librerías ====

if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("grid", quietly = TRUE)) install.packages("grid")

library(sf)
library(ggplot2)
library(gridExtra)
library(grid)

# ==== Parámetros a editar ====

ruta_sim_originales <- "modelos/meuse_zinc_simulaciones_originales.rds"

nombre_dataset <- "meuse"
nombre_variable <- "zinc"

# Tamaño aproximado de celda.
# Si la rejilla procede del script 045 con 100 x 100 celdas,
# el área se calcula automáticamente a partir de las coordenadas.
calcular_area_celda_auto <- TRUE
area_celda_manual_m2 <- NA

# Niveles de contaminación
niveles <- data.frame(
  Nivel = c("Bajo", "Medio", "Alto", "Muy alto"),
  Min = c(251, 501, 751, 1001),
  Max = c(500, 750, 1000, Inf)
)

numero_bins <- 10

# Guardado
guardar_tablas <- TRUE
guardar_figura <- TRUE
formato_figura <- "pdf"      # "png" o "pdf"

ancho_figura <- 10
alto_figura  <- 8
dpi_figura   <- 300          # solo PNG

# ==== Comprobaciones ====

if (!file.exists(ruta_sim_originales)) {
  stop(paste(
    "No se encuentra el fichero:",
    ruta_sim_originales,
    "\nEjecuta antes el script 046."
  ))
}

if (!all(c("Nivel", "Min", "Max") %in% names(niveles))) {
  stop("El data frame 'niveles' debe tener las columnas: Nivel, Min y Max.")
}

# ==== Cargar simulaciones ====

simulaciones_sf <- readRDS(ruta_sim_originales)

cat("\nSimulaciones cargadas:\n")
cat(ruta_sim_originales, "\n")

simulaciones_df <- st_drop_geometry(simulaciones_sf)

columnas_sim <- grep("^sim", names(simulaciones_df), value = TRUE)

if (length(columnas_sim) == 0) {
  stop("No se han encontrado columnas de simulación con prefijo 'sim'.")
}

cat("Número de simulaciones:", length(columnas_sim), "\n")
cat("Número de celdas:", nrow(simulaciones_df), "\n")

# ==== Preparar coordenadas y área de celda ====

if (!all(c("X", "Y") %in% colnames(simulaciones_df))) {
  coords <- st_coordinates(simulaciones_sf)
  simulaciones_df$X <- coords[, 1]
  simulaciones_df$Y <- coords[, 2]
}

if (calcular_area_celda_auto) {
  
  dx <- min(diff(sort(unique(simulaciones_df$X))), na.rm = TRUE)
  dy <- min(diff(sort(unique(simulaciones_df$Y))), na.rm = TRUE)
  
  area_celda_m2 <- abs(dx * dy)
  
} else {
  
  if (is.na(area_celda_manual_m2) || area_celda_manual_m2 <= 0) {
    stop("Si calcular_area_celda_auto = FALSE, area_celda_manual_m2 debe ser > 0.")
  }
  
  area_celda_m2 <- area_celda_manual_m2
}

cat("\nÁrea de celda estimada:", area_celda_m2, "m2\n")
cat("Área de celda estimada:", area_celda_m2 / 10000, "ha\n")

# ==== Calcular conteos por nivel ====

lista_conteos <- list()

for (i in seq_len(nrow(niveles))) {
  
  nivel_i <- niveles$Nivel[i]
  min_i <- niveles$Min[i]
  max_i <- niveles$Max[i]
  
  conteos_i <- numeric(length(columnas_sim))
  
  for (j in seq_along(columnas_sim)) {
    
    valores_sim <- simulaciones_df[[columnas_sim[j]]]
    
    conteos_i[j] <- sum(
      valores_sim >= min_i & valores_sim <= max_i,
      na.rm = TRUE
    )
  }
  
  lista_conteos[[nivel_i]] <- data.frame(
    Nivel = nivel_i,
    Simulacion = seq_along(columnas_sim),
    Pixel_count = conteos_i,
    Area_m2 = conteos_i * area_celda_m2,
    Area_ha = conteos_i * area_celda_m2 / 10000
  )
}

tabla_conteos <- do.call(rbind, lista_conteos)

cat("\nVista previa de conteos por nivel:\n")
print(head(tabla_conteos))

# ==== Calcular estadísticas por nivel ====

estadisticas_niveles <- do.call(
  rbind,
  lapply(
    split(tabla_conteos, tabla_conteos$Nivel),
    function(df) {
      data.frame(
        Nivel = unique(df$Nivel),
        Media_pixeles = mean(df$Pixel_count, na.rm = TRUE),
        Mediana_pixeles = median(df$Pixel_count, na.rm = TRUE),
        Desviacion_estandar_pixeles = sd(df$Pixel_count, na.rm = TRUE),
        Varianza_pixeles = var(df$Pixel_count, na.rm = TRUE),
        Minimo_pixeles = min(df$Pixel_count, na.rm = TRUE),
        Maximo_pixeles = max(df$Pixel_count, na.rm = TRUE),
        Media_ha = mean(df$Area_ha, na.rm = TRUE),
        Mediana_ha = median(df$Area_ha, na.rm = TRUE),
        Desviacion_estandar_ha = sd(df$Area_ha, na.rm = TRUE),
        Varianza_ha = var(df$Area_ha, na.rm = TRUE),
        Minimo_ha = min(df$Area_ha, na.rm = TRUE),
        Maximo_ha = max(df$Area_ha, na.rm = TRUE)
      )
    }
  )
)

row.names(estadisticas_niveles) <- NULL

cat("\n=====================================\n")
cat(" Estadísticas por nivel\n")
cat("=====================================\n")
print(estadisticas_niveles)

# ==== Crear histogramas ====

crear_histograma_nivel <- function(df, nivel, min_val, max_val) {
  
  media <- mean(df$Area_ha, na.rm = TRUE)
  mediana <- median(df$Area_ha, na.rm = TRUE)
  desv <- sd(df$Area_ha, na.rm = TRUE)
  
  if (is.infinite(max_val)) {
    subtitulo <- paste0(nombre_variable, " > ", min_val - 1, " ppm")
  } else {
    subtitulo <- paste0(nombre_variable, " entre ", min_val, " y ", max_val, " ppm")
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
        " ha; Mediana = ", round(mediana, 2),
        " ha; SD = ", round(desv, 2),
        " ha"
      ),
      x = "Superficie simulada (ha)",
      y = "Frecuencia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle = element_text(size = 9, hjust = 0.5),
      axis.title = element_text(face = "bold")
    )
}

graficos <- list()

for (i in seq_len(nrow(niveles))) {
  
  nivel_i <- niveles$Nivel[i]
  min_i <- niveles$Min[i]
  max_i <- niveles$Max[i]
  
  df_i <- tabla_conteos[tabla_conteos$Nivel == nivel_i, ]
  
  graficos[[nivel_i]] <- crear_histograma_nivel(
    df = df_i,
    nivel = nivel_i,
    min_val = min_i,
    max_val = max_i
  )
}

# ==== Panel gráfico ====

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

# ==== Guardar tablas ====

if (guardar_tablas) {
  
  dir_tablas <- "salidas/tablas"
  
  if (!dir.exists(dir_tablas)) {
    dir.create(dir_tablas, recursive = TRUE)
  }
  
  nombre_tabla_conteos <- file.path(
    dir_tablas,
    paste0(nombre_dataset, "_", nombre_variable, "_conteos_niveles_contaminacion.csv")
  )
  
  nombre_tabla_estadisticas <- file.path(
    dir_tablas,
    paste0(nombre_dataset, "_", nombre_variable, "_estadisticas_niveles_contaminacion.csv")
  )
  
  write.csv(tabla_conteos, nombre_tabla_conteos, row.names = FALSE)
  write.csv(estadisticas_niveles, nombre_tabla_estadisticas, row.names = FALSE)
  
  cat("\n=====================================\n")
  cat(" Tablas guardadas\n")
  cat("=====================================\n")
  cat("Conteos:\n", nombre_tabla_conteos, "\n")
  cat("Existe:", file.exists(nombre_tabla_conteos), "\n")
  cat("Estadísticas:\n", nombre_tabla_estadisticas, "\n")
  cat("Existe:", file.exists(nombre_tabla_estadisticas), "\n\n")
}

# ==== Guardar figura ====

if (guardar_figura) {
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0(
      nombre_dataset,
      "_",
      nombre_variable,
      "_histogramas_niveles_contaminacion.",
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
      device = "pdf"
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("\n=====================================\n")
  cat(" Figura guardada\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_figura, "\n")
  cat("Existe:", file.exists(nombre_figura), "\n\n")
}


# ==== Curvas de riesgo por nivel ====

# La curva de riesgo representa:
# P(Area_ha >= a)
# es decir, la probabilidad de que la superficie ocupada
# por un nivel de contaminación supere un valor dado.

curvas_riesgo <- do.call(
  rbind,
  lapply(
    split(tabla_conteos, tabla_conteos$Nivel),
    function(df) {
      
      areas_ordenadas <- sort(df$Area_ha)
      n <- length(areas_ordenadas)
      
      data.frame(
        Nivel = unique(df$Nivel),
        Area_ha = areas_ordenadas,
        Probabilidad_superacion = rev(seq_len(n)) / n
      )
    }
  )
)

# ==== Gráfico de curvas de riesgo ====

grafico_riesgo <- ggplot(
  curvas_riesgo,
  aes(x = Area_ha, y = Probabilidad_superacion, colour = Nivel)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Curvas de riesgo por nivel de contaminación",
    subtitle = "Probabilidad de superar una determinada superficie",
    x = "Superficie umbral (ha)",
    y = "P(Superficie simulada ≥ superficie umbral)",
    colour = "Nivel"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

print(grafico_riesgo)

# ==== Guardar curva de riesgo ====

if (guardar_tablas) {
  
  nombre_tabla_riesgo <- file.path(
    dir_tablas,
    paste0(nombre_dataset, "_", nombre_variable, "_curvas_riesgo_contaminacion.csv")
  )
  
  write.csv(curvas_riesgo, nombre_tabla_riesgo, row.names = FALSE)
  
  cat("\nCurvas de riesgo guardadas en:\n", nombre_tabla_riesgo, "\n")
}

if (guardar_figura) {
  
  nombre_figura_riesgo <- file.path(
    dir_figuras,
    paste0(
      nombre_dataset,
      "_",
      nombre_variable,
      "_curvas_riesgo_contaminacion.",
      formato_figura
    )
  )
  
  if (formato_figura == "png") {
    
    ggsave(
      filename = nombre_figura_riesgo,
      plot = grafico_riesgo,
      width = ancho_figura,
      height = alto_figura,
      dpi = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    ggsave(
      filename = nombre_figura_riesgo,
      plot = grafico_riesgo,
      width = ancho_figura,
      height = alto_figura,
      device = "pdf"
    )
  }
  
  cat("\nFigura de curvas de riesgo guardada en:\n", nombre_figura_riesgo, "\n")
}
