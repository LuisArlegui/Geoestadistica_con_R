#####################################################
#
#  044. Cokriging 5: validación cruzada LOOCV
#
#  Descripción:
#  Realiza validación cruzada leave-one-out (LOOCV)
#  del modelo de cokriging ajustado previamente mediante
#  un modelo lineal de corregionalización (LMC).
#  Genera tablas de resultados, métricas de error y
#  gráficos diagnósticos de los residuales.
#
#  Requisito previo:
#  - Ejecutar antes el script 042, que debe guardar:
#    modelos/042_dataset_objetivo_auxiliar_modelo_LMC.rds
#
#  Entrada:
#  - datos/brutos/cloruros/LabSamples.csv
#  - datos/procesados/Field_conduc_combi.csv
#  - modelos/042_dataset_objetivo_auxiliar_modelo_LMC.rds
#
#  Salida:
#  - salidas/tablas/044_objetivo_auxiliar_validacion_cruzada_CK.csv
#  - salidas/tablas/044_objetivo_auxiliar_metricas_validacion_CK.csv
#  - salidas/figuras/044_objetivo_auxiliar_validacion_cruzada_CK.pdf o .png
#
#####################################################


# ==== Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("ggplot2", "gstat", "sf", "gridExtra", "grid")

instalar_si_falta <- function(pkgs) {
  nuevos <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(nuevos) > 0) {
    message("Instalando paquetes faltantes: ", paste(nuevos, collapse = ", "))
    install.packages(nuevos, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

instalar_si_falta(paquetes_requeridos)


# ==== Archivos de entrada ====

ruta_objetivo <- "datos/brutos/cloruros/LabSamples.csv"     # <-- EDITAR AQUÍ
ruta_auxiliar <- "datos/procesados/040_dataset_combinado.csv"  # <-- EDITAR AQUÍ

if (!file.exists(ruta_objetivo)) {
  stop(paste("No se encuentra el fichero:", ruta_objetivo))
}

if (!file.exists(ruta_auxiliar)) {
  stop(paste("No se encuentra el fichero:", ruta_auxiliar))
}


# ==== Nombres de los datasets ====

nombre_dataset_obj <- tools::file_path_sans_ext(basename(ruta_objetivo))
nombre_dataset_aux <- tools::file_path_sans_ext(basename(ruta_auxiliar))


# ==== Ruta dinámica del modelo LMC ====

ruta_modelo_lmc <- file.path(
  "modelos",
  paste0(
    "042_",
    nombre_dataset_obj, "_",
    nombre_dataset_aux,
    "_modelo_LMC.rds"
  )
)

if (!file.exists(ruta_modelo_lmc)) {
  stop(
    paste(
      "No se encuentra el modelo LMC:",
      ruta_modelo_lmc,
      "\nEjecuta antes el script 042."
    )
  )
}


# ==== Parámetros a editar ====

# Deben coincidir con los IDs usados en el script 042
id_objetivo <- "Cl"
id_auxiliar <- "FmSm"

# Gráficos
numero_bins    <- 10
titulo_general <- "Validación Cruzada LOOCV de Cokriging"

# Guardado
guardar_tablas <- TRUE
guardar_figura <- TRUE
formato_figura <- "pdf"     # "png" o "pdf"

ancho_figura <- 10
alto_figura  <- 8
dpi_figura   <- 300         # Solo para PNG


# ==== Cargar modelo LMC ====

crosscorrection <- readRDS(ruta_modelo_lmc)

cat("\n=====================================\n")
cat(" Modelo LMC cargado exitosamente:\n")
cat("=====================================\n")
print(crosscorrection)

cat("\nVariables configuradas en el modelo LMC:\n")
print(names(crosscorrection$data))


# ==== Comprobar identificadores ====

ids_modelo     <- names(crosscorrection$data)
ids_necesarios <- c(id_objetivo, id_auxiliar)

faltan_ids <- ids_necesarios[!ids_necesarios %in% ids_modelo]

if (length(faltan_ids) > 0) {
  stop(
    paste(
      "Estos identificadores no existen en el modelo LMC:",
      paste(faltan_ids, collapse = ", "),
      "\nRevisa que id_objetivo e id_auxiliar coincidan",
      "con los utilizados en el script 042."
    )
  )
}


# ==== Número de observaciones de la variable objetivo ====

n_objetivo <- nrow(crosscorrection$data[[id_objetivo]]$data)

if (n_objetivo < 2) {
  stop("Se necesitan al menos dos observaciones de la variable objetivo.")
}

cat("\nNúmero de observaciones objetivo:", n_objetivo, "\n")


# ==== Validación cruzada LOOCV ====

cross_validation_CK <- gstat.cv(
  crosscorrection,
  nfold = n_objetivo
)

cat("\n=====================================\n")
cat(" Validación cruzada completada exitosamente\n")
cat("=====================================\n")
print(summary(cross_validation_CK))

cv_df <- as.data.frame(cross_validation_CK)

cat("\nColumnas disponibles en la validación cruzada:\n")
print(colnames(cv_df))


# ==== Preparar tabla de validación ====

# Coordenadas
if (all(c("coords.x1", "coords.x2") %in% colnames(cv_df))) {
  cv_df$X <- cv_df$coords.x1
  cv_df$Y <- cv_df$coords.x2
} else {
  stop("No se han encontrado las columnas de coordenadas coords.x1 y coords.x2 en gstat.cv().")
}

# Selección de los resultados pertenecientes únicamente a la variable objetivo
if ("id" %in% colnames(cv_df)) {
  cv_obj <- cv_df[cv_df$id == id_objetivo, , drop = FALSE]
} else {
  cv_obj <- cv_df
}

if (nrow(cv_obj) == 0) {
  stop(paste("No se han encontrado resultados de validación para la variable objetivo:", id_objetivo))
}


# ==== Detectar columna de predicción ====

posibles_pred <- c(paste0(id_objetivo, ".pred"), "var1.pred")
campo_pred    <- posibles_pred[posibles_pred %in% colnames(cv_obj)][1]

if (length(campo_pred) == 0 || is.na(campo_pred)) {
  stop("No se ha encontrado la columna de predicción. Revisa colnames(cv_df).")
}


# ==== Detectar columna observada ====

posibles_obs <- c("observed", paste0(id_objetivo, ".obs"))
campo_obs    <- posibles_obs[posibles_obs %in% colnames(cv_obj)][1]

if (length(campo_obs) == 0 || is.na(campo_obs)) {
  stop("No se ha encontrado la columna observada. Revisa colnames(cv_df).")
}


# ==== Residuales ====

if (!"residual" %in% colnames(cv_obj)) {
  cv_obj$residual <- cv_obj[[campo_obs]] - cv_obj[[campo_pred]]
}

# Renombrado interno para facilitar gráficos y cálculos
cv_obj$observado <- cv_obj[[campo_obs]]
cv_obj$predicho  <- cv_obj[[campo_pred]]


# ==== Métricas de validación cruzada ====

ME   <- mean(cv_obj$residual, na.rm = TRUE)
MAE  <- mean(abs(cv_obj$residual), na.rm = TRUE)
MSE  <- mean(cv_obj$residual^2, na.rm = TRUE)
RMSE <- sqrt(MSE)

Eficiencia_pred <- 1 - sum(cv_obj$residual^2, na.rm = TRUE) /
  sum((cv_obj$observado - mean(cv_obj$observado, na.rm = TRUE))^2, na.rm = TRUE)

Cor_obs_pred <- cor(cv_obj$observado, cv_obj$predicho, use = "complete.obs")


# ==== Errores estandarizados ====

if ("zscore" %in% colnames(cv_obj)) {
  MZE  <- mean(cv_obj$zscore, na.rm = TRUE)
  MSZE <- mean(cv_obj$zscore^2, na.rm = TRUE)
} else {
  MZE  <- NA_real_
  MSZE <- NA_real_
  warning("La salida de gstat.cv() no contiene zscore. No se calcularán MZE ni MSZE.")
}


# ==== Tabla de métricas ====

tabla_metricas <- data.frame(
  Modelo = "Cokriging",
  Variable_objetivo = id_objetivo,
  N = nrow(cv_obj),
  ME = ME,
  MAE = MAE,
  MSE = MSE,
  RMSE = RMSE,
  Eficiencia_pred = Eficiencia_pred,
  Cor_obs_pred = Cor_obs_pred,
  MZE = MZE,
  MSZE = MSZE
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
    paste0("044_", nombre_dataset_obj, "_", nombre_dataset_aux, "_validacion_cruzada_CK.csv")
  )
  
  nombre_metricas <- file.path(
    dir_tablas,
    paste0("044_", nombre_dataset_obj, "_", nombre_dataset_aux, "_metricas_validacion_CK.csv")
  )
  
  write.csv(cv_obj, nombre_cv, row.names = FALSE)
  write.csv(tabla_metricas, nombre_metricas, row.names = FALSE)
  
  cat("\n=====================================\n")
  cat(" Tablas guardadas exitosamente en:\n")
  cat("=====================================\n")
  cat("Validación cruzada:\n", nombre_cv, "\n")
  cat("Métricas de precisión:\n", nombre_metricas, "\n\n")
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
  theme_gray() +   # Tema gris por defecto
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5, color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(colour = "black", size = 9)
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
    title = "QQ Plot de residuales"
  ) +
  theme_gray() +   # Tema gris por defecto
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5, color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(colour = "black", size = 9)
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
    midpoint = 0,
    name = "Residual"
  ) +
  scale_size_continuous(name = "|Residual|") +
  coord_fixed(ratio = 1) +
  labs(
    x = "X",
    y = "Y",
    title = "Mapa espacial de residuales"
  ) +
  theme_gray() +   # Tema gris por defecto
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5, color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(colour = "black", size = 9)
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
    x = "Valor observado",
    y = "Valor predicho",
    title = "Observado vs. Predicho"
  ) +
  theme_gray() +   # Tema gris por defecto
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5, color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(colour = "black", size = 9)
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
    paste0("044_", nombre_dataset_obj, "_", nombre_dataset_aux, "_validacion_cruzada_CK.", formato_figura)
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