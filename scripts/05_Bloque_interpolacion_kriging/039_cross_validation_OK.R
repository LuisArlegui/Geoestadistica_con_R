#####################################################
#
#  039. Validación cruzada de kriging ordinario
#
#  Descripción:
#  Realiza una validación cruzada leave-one-out para
#  un modelo de kriging ordinario, calcula métricas de
#  error (ME, MAE, RMSE, R², MZE, MSZE) y genera un panel
#  de gráficos diagnósticos de los residuales y de la
#  relación observado-predicho.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - resultados y métricas en consola
#  - salidas/tablas/039_dataset_variable_validacion_cruzada_OK.csv
#  - salidas/tablas/039_dataset_variable_metricas_validacion_OK.csv
#  - (opcional) salidas/figuras/039_dataset_variable_validacion_cruzada_OK.png o .pdf
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


# ==== Archivo de datos ====

ruta_datos <- "datos/procesados/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))

cat("\nDirectorio raíz del proyecto:\n", getwd(), "\n")


# ==== Parámetros a editar ====

nombre_x        <- "coord_x"                     # <-- EDITAR AQUÍ
nombre_y        <- "coord_y"                     # <-- EDITAR AQUÍ
nombre_variable <- "variable"                    # <-- EDITAR AQUÍ

epsg_datos <- 12345                        # <-- EDITAR AQUÍ: Código EPSG (ej. 32613 = UTM zone 13N)

# Variograma experimental
cutoff_variograma <- 4000                # Distancia máxima (metros)
lag_variograma    <- 250                 # Anchura de lag (metros)

# Modelo de variograma
pSill             <- 0.15
modelo_variograma <- "Sph"                 # "Exp", "Sph", "Gau", "Bes", "Mat", etc.
rango             <- 632
nugget            <- 0.007

# Gráficos
numero_bins    <- 12
titulo_general <- "Validación Cruzada de Kriging Ordinario (LOOCV)"

# Guardado
guardar_tablas <- TRUE
guardar_figura <- TRUE
formato_figura <- "pdf"                    # "png" o "pdf"

ancho_figura <- 10
alto_figura  <- 8
dpi_figura   <- 300                        # Solo para PNG


# ==== Comprobaciones ====

vars <- c(nombre_x, nombre_y, nombre_variable)

faltan <- vars[!vars %in% colnames(data)]

if (length(faltan) > 0) {
  stop(paste("No existen estas columnas en el dataset:", paste(faltan, collapse = ", ")))
}

vars_no_num <- vars[!sapply(data[vars], is.numeric)]

if (length(vars_no_num) > 0) {
  stop(paste("Estas variables deben ser numéricas:", paste(vars_no_num, collapse = ", ")))
}

if (cutoff_variograma <= 0) {
  stop("cutoff_variograma debe ser mayor que cero.")
}

if (lag_variograma <= 0) {
  stop("lag_variograma debe ser mayor que cero.")
}

data_plot <- data[
  complete.cases(data[, vars]),
  vars,
  drop = FALSE
]

if (nrow(data_plot) < 3) {
  stop("Se necesitan al menos tres observaciones válidas para validación cruzada.")
}

cat("\nCoordenada X:", nombre_x, "\n")
cat("Coordenada Y:", nombre_y, "\n")
cat("Variable:", nombre_variable, "\n")
cat("EPSG:", epsg_datos, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")


# ==== Conversión a sf ====

data_sf <- st_as_sf(
  data_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)


# ==== Variograma experimental ====

formula_ok <- as.formula(
  paste(nombre_variable, "~ 1")
)

variograma_exp <- variogram(
  formula_ok,
  data = data_sf,
  cutoff = cutoff_variograma,
  width = lag_variograma
)

cat("\nVariograma experimental:\n")
print(variograma_exp)


# ==== Modelo variográfico ====

modelo_variografico <- vgm(
  psill = pSill,
  model = modelo_variograma,
  range = rango,
  nugget = nugget
)

cat("\n=====================================\n")
cat(" Modelo variográfico usado\n")
cat("=====================================\n")
print(modelo_variografico)


# ==== Validación cruzada (Leave-One-Out) ====

kcv_ok <- krige.cv(
  formula = formula_ok,
  locations = data_sf,
  model = modelo_variografico
)

cat("\n=====================================\n")
cat(" Resumen de validación cruzada\n")
cat("=====================================\n")
print(summary(kcv_ok))

kcv_df <- as.data.frame(kcv_ok)

# Añadir coordenadas para gráficos y exportación
coords_cv <- st_coordinates(kcv_ok)
kcv_df$X <- coords_cv[, 1]
kcv_df$Y <- coords_cv[, 2]


# ==== Métricas de validación cruzada ====

ME  <- mean(kcv_df$residual, na.rm = TRUE)
MAE <- mean(abs(kcv_df$residual), na.rm = TRUE)
MSE <- mean(kcv_df$residual^2, na.rm = TRUE)
RMSE <- sqrt(MSE)

Eficiencia_pred <- 1 - sum(kcv_df$residual^2, na.rm = TRUE) /
  sum((kcv_df$observed - mean(kcv_df$observed, na.rm = TRUE))^2, na.rm = TRUE)

Cor_obs_pred <- cor(kcv_df$observed, kcv_df$var1.pred, use = "complete.obs")

MZE  <- mean(kcv_df$zscore, na.rm = TRUE)
MSZE <- mean(kcv_df$zscore^2, na.rm = TRUE)

tabla_metricas <- data.frame(
  Variable = nombre_variable,
  N = nrow(kcv_df),
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
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_tablas     <- "salidas/tablas"
  
  if (!dir.exists(dir_tablas)) {
    dir.create(dir_tablas, recursive = TRUE)
  }
  
  nombre_cv <- file.path(
    dir_tablas,
    paste0("039_", nombre_dataset, "_", nombre_variable, "_validacion_cruzada_OK.csv")
  )
  
  nombre_metricas <- file.path(
    dir_tablas,
    paste0("039_", nombre_dataset, "_", nombre_variable, "_metricas_validacion_OK.csv")
  )
  
  write.csv(kcv_df, nombre_cv, row.names = FALSE)
  write.csv(tabla_metricas, nombre_metricas, row.names = FALSE)
  
  cat("\n=====================================\n")
  cat(" Tablas guardadas exitosamente en:\n")
  cat("=====================================\n")
  cat("Validación cruzada:\n", nombre_cv, "\n")
  cat("Métricas de precisión:\n", nombre_metricas, "\n\n")
}


# ==== Histograma de residuales ====

grafico_hist <- ggplot(kcv_df, aes(x = residual)) +
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

grafico_qq <- ggplot(kcv_df, aes(sample = residual)) +
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

grafico_residuales <- ggplot(kcv_df, aes(x = X, y = Y)) +
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
    x = nombre_x,
    y = nombre_y,
    title = "Mapa espacial de residuales"
  ) +
  theme_gray() +   # Tema gris por defecto
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5, color = "black"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(colour = "black", size = 9)
  )


# ==== Observado vs predicho ====

grafico_obs_pred <- ggplot(kcv_df, aes(x = observed, y = var1.pred)) +
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
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_figuras    <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0("039_", nombre_dataset, "_", nombre_variable, "_validacion_cruzada_OK.", formato_figura)
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