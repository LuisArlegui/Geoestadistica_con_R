#####################################################
#
#  044. Cokriging 5: validación cruzada LOOCV
#
#  Descripción:
#  Realiza validación cruzada leave-one-out del modelo
#  de cokriging ajustado previamente mediante LMC.
#  Genera tablas de resultados, métricas de error y
#  gráficos diagnósticos de residuales.
#
#  Requisito previo:
#  - Ejecutar antes el script 042, que debe guardar:
#    modelos/lmc_model.rds
#
#  Entrada:
#  - modelos/lmc_model.rds
#
#  Salida:
#  - salidas/tablas/validacion_cruzada_CK.csv
#  - salidas/tablas/metricas_validacion_CK.csv
#  - salidas/figuras/validacion_cruzada_CK.pdf o .png
#
#####################################################

# ==== Cargar librerías ====

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gstat", quietly = TRUE)) install.packages("gstat")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("grid", quietly = TRUE)) install.packages("grid")

library(ggplot2)
library(gstat)
library(sf)
library(gridExtra)
library(grid)

# ==== Archivos de entrada ====

ruta_modelo_lmc <- "modelos/lmc_model.rds"

if (!file.exists(ruta_modelo_lmc)) {
  stop(paste(
    "No se encuentra el modelo LMC:",
    ruta_modelo_lmc,
    "\nEjecuta antes el script 042."
  ))
}



# ==== Parámetros a editar ====

# Deben coincidir con los ids usados en el script 042
id_objetivo <- "Cl"
id_auxiliar <- "FmSm"

# Gráficos
numero_bins <- 10
titulo_general <- "Validación cruzada LOOCV del cokriging"

# Guardado
guardar_tablas <- TRUE
guardar_figura <- TRUE
formato_figura <- "pdf"     # "png" o "pdf"

ancho_figura <- 10
alto_figura  <- 8
dpi_figura   <- 300         # solo PNG

# ==== Cargar modelo LMC ====

crosscorrection <- readRDS(ruta_modelo_lmc)

cat("\n=====================================\n")
cat(" Modelo LMC cargado\n")
cat("=====================================\n")
print(crosscorrection)

# ==== Validación cruzada ====

cross_validation_CK <- gstat.cv(crosscorrection)

cat("\n=====================================\n")
cat(" Validación cruzada completada\n")
cat("=====================================\n")
print(summary(cross_validation_CK))

cv_df <- as.data.frame(cross_validation_CK)

cat("\nColumnas disponibles en la validación cruzada:\n")
print(colnames(cv_df))

# ==== Preparar tabla de validación ====

cv_df$X <- cv_df$coords.x1
cv_df$Y <- cv_df$coords.x2

# En algunos objetos gstat.cv multivariables aparece una columna id.
# Si existe, filtramos la variable objetivo.
if ("id" %in% colnames(cv_df)) {
  cv_obj <- cv_df[cv_df$id == id_objetivo, ]
} else {
  cv_obj <- cv_df
}

# Detectar columna de predicción
posibles_pred <- c(
  paste0(id_objetivo, ".pred"),
  "var1.pred"
)

campo_pred <- posibles_pred[posibles_pred %in% colnames(cv_obj)][1]

if (is.na(campo_pred)) {
  stop("No se ha encontrado la columna de predicción. Revisa colnames(cv_df).")
}

# Detectar columna observada
posibles_obs <- c(
  "observed",
  paste0(id_objetivo, ".obs")
)

campo_obs <- posibles_obs[posibles_obs %in% colnames(cv_obj)][1]

if (is.na(campo_obs)) {
  stop("No se ha encontrado la columna observada. Revisa colnames(cv_df).")
}

# Detectar residual
if (!"residual" %in% colnames(cv_obj)) {
  cv_obj$residual <- cv_obj[[campo_obs]] - cv_obj[[campo_pred]]
}

# Renombrado interno para gráficos
cv_obj$observado <- cv_obj[[campo_obs]]
cv_obj$predicho  <- cv_obj[[campo_pred]]

# ==== Métricas de error ====

ME <- mean(cv_obj$residual, na.rm = TRUE)

MAE <- mean(abs(cv_obj$residual), na.rm = TRUE)

MSE <- mean(cv_obj$residual^2, na.rm = TRUE)

RMSE <- sqrt(MSE)

R2 <- 1 - sum(cv_obj$residual^2, na.rm = TRUE) /
  sum((cv_obj$observado - mean(cv_obj$observado, na.rm = TRUE))^2, na.rm = TRUE)

tabla_metricas <- data.frame(
  Modelo = "Cokriging",
  Variable_objetivo = id_objetivo,
  N = nrow(cv_obj),
  ME = ME,
  MAE = MAE,
  MSE = MSE,
  RMSE = RMSE,
  R2_pseudo = R2
)

cat("\n=====================================\n")
cat(" Métricas de validación cruzada\n")
cat("=====================================\n")
print(tabla_metricas)

# ==== Guardar tablas ====

if (guardar_tablas) {
  
  dir_tablas <- "salidas/tablas"
  
  if (!dir.exists(dir_tablas)) {
    dir.create(dir_tablas, recursive = TRUE)
  }
  
  nombre_cv <- file.path(
    dir_tablas,
    paste0(id_objetivo, "_", id_auxiliar, "_validacion_cruzada_CK.csv")
  )
  
  nombre_metricas <- file.path(
    dir_tablas,
    paste0(id_objetivo, "_", id_auxiliar, "_metricas_validacion_CK.csv")
  )
  
  write.csv(cv_obj, nombre_cv, row.names = FALSE)
  write.csv(tabla_metricas, nombre_metricas, row.names = FALSE)
  
  cat("\n=====================================\n")
  cat(" Tablas guardadas\n")
  cat("=====================================\n")
  cat("Validación cruzada:\n", nombre_cv, "\n")
  cat("Métricas:\n", nombre_metricas, "\n\n")
}

# ==== Histograma de residuales ====

grafico_hist <- ggplot(cv_obj, aes(x = residual)) +
  geom_histogram(
    bins = numero_bins,
    color = "black",
    fill = "grey70",
    alpha = 0.7
  ) +
  geom_vline(
    xintercept = 0,
    color = "red",
    linewidth = 0.8
  ) +
  labs(
    x = "Residual",
    y = "Frecuencia",
    title = "Histograma de residuales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

# ==== QQ plot de residuales ====

grafico_qq <- ggplot(cv_obj, aes(sample = residual)) +
  stat_qq_line(
    color = "black",
    linewidth = 0.5
  ) +
  stat_qq(
    color = "purple",
    size = 2,
    alpha = 0.7
  ) +
  labs(
    x = "Cuantiles teóricos normales",
    y = "Cuantiles observados",
    title = "QQ plot de residuales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

# ==== Mapa de residuales ====

grafico_residuales <- ggplot(cv_obj, aes(x = X, y = Y)) +
  geom_point(
    aes(size = abs(residual), color = residual),
    alpha = 0.8
  ) +
  scale_color_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  coord_fixed(ratio = 1) +
  labs(
    x = "X",
    y = "Y",
    size = "|Residual|",
    color = "Residual",
    title = "Mapa de residuales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

# ==== Observado vs predicho ====

grafico_obs_pred <- ggplot(cv_obj, aes(x = observado, y = predicho)) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red",
    linewidth = 1
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "black",
    linewidth = 1,
    se = FALSE
  ) +
  geom_point(
    color = "blue",
    alpha = 0.7
  ) +
  coord_fixed() +
  labs(
    x = "Observado",
    y = "Predicho",
    title = "Observado vs predicho"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

# ==== Panel diagnóstico ====

titulo_panel <- textGrob(
  titulo_general,
  gp = gpar(fontsize = 15, fontface = 2)
)

panel <- arrangeGrob(
  grafico_residuales,
  grafico_qq,
  grafico_hist,
  grafico_obs_pred,
  ncol = 2,
  nrow = 2,
  top = titulo_panel
)

grid.arrange(panel)

# ==== Guardar figura ====

if (guardar_figura) {
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0(id_objetivo, "_", id_auxiliar, "_validacion_cruzada_CK.", formato_figura)
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
  cat("Archivo:\n", nombre_figura, "\n\n")
}
