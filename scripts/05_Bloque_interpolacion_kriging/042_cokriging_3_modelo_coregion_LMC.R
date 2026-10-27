#####################################################
#
#  042. Cokriging 3: modelo de corregionalización (LMC)
#
#  Descripción:
#  Ajusta un modelo lineal de corregionalización (LMC)
#  a partir de los variogramas directos y cruzado entre
#  una variable objetivo y una variable auxiliar. Este
#  modelo es necesario para aplicar cokriging ordinario.
#
#  Entrada:
#  - datos/brutos/cloruros/LabSamples.csv
#  - datos/procesados/Field_conduc_combi.csv
#
#  Salida:
#  - gráfico del modelo LMC en la ventana gráfica de R
#  - salidas/figuras/042_dataset_objetivo_auxiliar_modelo_LMC.png o .pdf
#  - salidas/tablas/042_dataset_objetivo_auxiliar_variograma_cruzado.csv
#  - salidas/tablas/042_dataset_objetivo_auxiliar_modelo_LMC.csv
#  - modelos/042_dataset_objetivo_auxiliar_modelo_LMC.rds
#
#####################################################


# ==== Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("gstat", "sf")

instalar_si_falta <- function(pkgs) {
  nuevos <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(nuevos) > 0) {
    message("Instalando paquetes faltantes: ", paste(nuevos, collapse = ", "))
    install.packages(nuevos, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

instalar_si_falta(paquetes_requeridos)


# ==== Archivos de datos ====

ruta_objetivo <- "datos/brutos/cloruros/LabSamples.csv"     # <-- EDITAR AQUÍ
ruta_auxiliar <- "datos/procesados/040_dataset_combinado.csv"  # <-- EDITAR AQUÍ

if (!file.exists(ruta_objetivo)) {
  stop(paste("No se encuentra el fichero:", ruta_objetivo))
}

if (!file.exists(ruta_auxiliar)) {
  stop(paste("No se encuentra el fichero:", ruta_auxiliar))
}

z1 <- read.csv(ruta_objetivo, sep = ",")
z2 <- read.csv(ruta_auxiliar, sep = ",")

cat("Columnas disponibles en el dataset objetivo:\n")
print(colnames(z1))

cat("\nColumnas disponibles en el dataset auxiliar:\n")
print(colnames(z2))


# ==== Parámetros a editar ====

nombre_x <- "x"
nombre_y <- "y"

nombre_objetivo <- "ClmgKg"
nombre_auxiliar <- "FmSm"

epsg_datos <- 32613                        # <-- EDITAR AQUÍ: Código EPSG

id_objetivo <- "Cl"
id_auxiliar <- "FmSm"

# Variograma experimental
cutoff_variograma <- 1400                  # Distancia máxima (metros)
lag_variograma    <- 140                   # Anchura de lag (metros)

# Modelo inicial LMC
pSill             <- 12.295
modelo_variograma <- "Sph"                 # "Exp", "Sph", "Gau", "Bes", "Mat", etc.
rango             <- 1385.3
nugget            <- 4.25

# Ajuste del LMC
fit_method       <- 6                      # Método de mínimos cuadrados no ponderados
correct_diagonal <- 1.01                   # Corrección ligera de la diagonal para asegurar semi-definida positiva

# Gráfico base de gstat (Lattice)
xlim_grafico <- c(0, 1400)
ylim_grafico <- NULL                       # Usar NULL para automático; o c(0, 12000)

# Guardado
guardar_figura <- TRUE
guardar_tablas <- TRUE
formato_figura <- "pdf"                    # "png" o "pdf"

ancho_figura <- 9
alto_figura  <- 6
dpi_figura   <- 300                        # Solo para PNG


# ==== Comprobaciones ====

vars_obj <- c(nombre_x, nombre_y, nombre_objetivo)
vars_aux <- c(nombre_x, nombre_y, nombre_auxiliar)

faltan_obj <- vars_obj[!vars_obj %in% colnames(z1)]
faltan_aux <- vars_aux[!vars_aux %in% colnames(z2)]

if (length(faltan_obj) > 0) {
  stop(paste("En el dataset objetivo faltan:", paste(faltan_obj, collapse = ", ")))
}

if (length(faltan_aux) > 0) {
  stop(paste("En el dataset auxiliar faltan:", paste(faltan_aux, collapse = ", ")))
}

vars_no_num_obj <- vars_obj[!sapply(z1[vars_obj], is.numeric)]
vars_no_num_aux <- vars_aux[!sapply(z2[vars_aux], is.numeric)]

if (length(vars_no_num_obj) > 0) {
  stop(paste("Estas columnas del dataset objetivo deben ser numéricas:", paste(vars_no_num_obj, collapse = ", ")))
}

if (length(vars_no_num_aux) > 0) {
  stop(paste("Estas columnas del dataset auxiliar deben ser numéricas:", paste(vars_no_num_aux, collapse = ", ")))
}

if (cutoff_variograma <= 0) {
  stop("cutoff_variograma debe ser mayor que cero.")
}

if (lag_variograma <= 0) {
  stop("lag_variograma debe ser mayor que cero.")
}

z1_plot <- z1[
  complete.cases(z1[, vars_obj]),
  vars_obj,
  drop = FALSE
]

z2_plot <- z2[
  complete.cases(z2[, vars_aux]),
  vars_aux,
  drop = FALSE
]

if (nrow(z1_plot) < 2) {
  stop("El dataset objetivo necesita al menos dos observaciones válidas.")
}

if (nrow(z2_plot) < 2) {
  stop("El dataset auxiliar necesita al menos dos observaciones válidas.")
}

cat("\nVariable objetivo:", nombre_objetivo, "\n")
cat("Variable auxiliar:", nombre_auxiliar, "\n")
cat("EPSG:", epsg_datos, "\n")
cat("Observaciones objetivo:", nrow(z1_plot), "\n")
cat("Observaciones auxiliar:", nrow(z2_plot), "\n")
cat("Cutoff:", cutoff_variograma, "m\n")
cat("Lag:", lag_variograma, "m\n")


# ==== Conversión a sf ====

z1_sf <- st_as_sf(
  z1_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)

z2_sf <- st_as_sf(
  z2_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)


# ==== Objeto gstat multivariable ====

g <- gstat(
  NULL,
  id = id_objetivo,
  formula = as.formula(paste(nombre_objetivo, "~ 1")),
  data = z1_sf
)

g <- gstat(
  g,
  id = id_auxiliar,
  formula = as.formula(paste(nombre_auxiliar, "~ 1")),
  data = z2_sf
)


# ==== Variogramas directos y cruzado ====

crossvariogram <- variogram(
  g,
  cutoff = cutoff_variograma,
  width = lag_variograma
)

if (nrow(crossvariogram) == 0) {
  stop("El variograma cruzado no contiene clases. Revisa cutoff y lag.")
}

cat("\nVariogramas directos y cruzado calculados:\n")
print(crossvariogram)


# ==== Modelo inicial LMC ====

modelo_base <- vgm(
  psill = pSill,
  model = modelo_variograma,
  range = rango,
  nugget = nugget
)

crossmodel <- gstat(
  g,
  model = modelo_base,
  fill.all = TRUE
)

cat("\n=====================================\n")
cat(" Modelo inicial LMC:\n")
cat("=====================================\n")
print(crossmodel)


# ==== Ajuste del modelo LMC ====

crosscorrection <- fit.lmc(
  v = crossvariogram,
  g = crossmodel,
  fit.method = fit_method,
  correct.diagonal = correct_diagonal
)


# ==== Comprobación del modelo LMC ====

if (is.null(crosscorrection$model)) {
  stop(
    paste(
      "No se ha podido obtener un modelo LMC válido.",
      "Revisa el modelo variográfico inicial y sus parámetros."
    )
  )
}

cat("\n=====================================\n")
cat(" Modelo LMC ajustado exitosamente:\n")
cat("=====================================\n")
print(crosscorrection)

cat("\nModelos variográficos incluidos en el LMC:\n")
print(names(crosscorrection$model))


# ==== Gráfico del modelo LMC (Lattice/gstat) ====

grafico_lmc <- plot(
  crossvariogram,
  model = crosscorrection$model,
  xlim = xlim_grafico,
  ylim = ylim_grafico,
  plot.numbers = TRUE
)

# Mostrar en pantalla
print(grafico_lmc)


# ==== Nombres de los datasets ====

nombre_dataset_obj <- tools::file_path_sans_ext(basename(ruta_objetivo))
nombre_dataset_aux <- tools::file_path_sans_ext(basename(ruta_auxiliar))


# ==== Guardar figura ====

if (guardar_figura) {
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0(
      "042_",
      nombre_dataset_obj, "_",
      nombre_dataset_aux,
      "_modelo_LMC.",
      formato_figura
    )
  )
  
  if (formato_figura == "png") {
    
    png(
      filename = nombre_figura,
      width = ancho_figura,
      height = alto_figura,
      units = "in",
      res = dpi_figura
    )
    
    print(
      plot(
        crossvariogram,
        model = crosscorrection$model,
        xlim = xlim_grafico,
        ylim = ylim_grafico,
        plot.numbers = TRUE
      )
    )
    
    dev.off()
    
  } else if (formato_figura == "pdf") {
    
    pdf(
      file = nombre_figura,
      width = ancho_figura,
      height = alto_figura
    )
    
    print(
      plot(
        crossvariogram,
        model = crosscorrection$model,
        xlim = xlim_grafico,
        ylim = ylim_grafico,
        plot.numbers = TRUE
      )
    )
    
    dev.off()
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  cat("\n=====================================\n")
  cat(" Figura guardada exitosamente en:\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_figura, "\n\n")
}


# ==== Guardar tablas ====

if (guardar_tablas) {
  
  dir_tablas <- "salidas/tablas"
  
  if (!dir.exists(dir_tablas)) {
    dir.create(dir_tablas, recursive = TRUE)
  }
  
  nombre_tabla_variograma <- file.path(
    dir_tablas,
    paste0(
      "042_",
      nombre_dataset_obj, "_",
      nombre_dataset_aux,
      "_variograma_cruzado.csv"
    )
  )
  
  nombre_tabla_modelo <- file.path(
    dir_tablas,
    paste0(
      "042_",
      nombre_dataset_obj, "_",
      nombre_dataset_aux,
      "_modelo_LMC.csv"
    )
  )
  
  write.csv(
    crossvariogram,
    nombre_tabla_variograma,
    row.names = FALSE
  )
  
  modelo_lmc_df <- do.call(
    rbind,
    lapply(
      names(crosscorrection$model),
      function(nombre_modelo) {
        df <- as.data.frame(crosscorrection$model[[nombre_modelo]])
        df$Variograma <- nombre_modelo
        return(df)
      }
    )
  )
  
  write.csv(
    modelo_lmc_df,
    nombre_tabla_modelo,
    row.names = FALSE
  )
  
  cat("=====================================\n")
  cat(" Tablas guardadas exitosamente en:\n")
  cat("=====================================\n")
  cat("Variograma cruzado:\n", nombre_tabla_variograma, "\n")
  cat("Modelo LMC:\n", nombre_tabla_modelo, "\n\n")
}


# ==== Guardar modelo LMC en disco (.rds) ====

dir_modelos <- "modelos"

if (!dir.exists(dir_modelos)) {
  dir.create(dir_modelos, recursive = TRUE)
}

ruta_modelo <- file.path(
  dir_modelos,
  paste0(
    "042_",
    nombre_dataset_obj, "_",
    nombre_dataset_aux,
    "_modelo_LMC.rds"
  )
)

saveRDS(
  crosscorrection,
  ruta_modelo
)

cat("=====================================\n")
cat(" Objeto de modelo LMC guardado en:\n")
cat("=====================================\n")
cat("Archivo RDS:\n", ruta_modelo, "\n\n")