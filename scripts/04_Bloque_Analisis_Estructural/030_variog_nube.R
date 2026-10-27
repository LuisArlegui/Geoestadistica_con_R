#####################################################
#
#  030. Variograma nube omnidireccional
#
#  Descripción:
#  Calcula y representa el variograma nube omnidireccional
#  de una variable regionalizada, mostrando la semivarianza
#  para todos los pares de puntos en función de la distancia.
#  Es útil como primer análisis exploratorio de la estructura
#  espacial y para detectar dispersión, outliers o tendencias.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - salidas/variogramas/030_dataset_variable_variograma_nube.png o .pdf
#  - (opcional) salidas/tablas/030_dataset_variable_variograma_nube_pares.csv
#
#####################################################


# Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("ggplot2", "gstat", "sf")

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

ruta_datos <- "datos/procesados/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ⚙️ PARÁMETROS A EDITAR ====

nombre_x        <- "coord_X"                 # <-- EDITAR AQUÍ
nombre_y        <- "coord_Y"                 # <-- EDITAR AQUÍ
nombre_variable <- "variable"                # <-- EDITAR AQUÍ

epsg_datos <- 12345             # <-- EDITAR AQUÍ: Código EPSG (ej. 32613 = UTM zone 13N)

cutoff_variograma <- 450000     # Distancia máxima (en metros para CRS proyectados)

Label_x        <- "Distancia (m)"
Label_y        <- "Semivarianza γ(h)"
Title_graph    <- "Variograma Nube Omnidireccional"
Subtitle_graph <- "Análisis de pares de puntos"

# Estética
color_puntos <- "blue"
tam_puntos   <- 1.2
alpha_puntos <- 0.5

# Guardado
guardar_figura <- TRUE
formato_figura <- "pdf"         # "png" o "pdf"
guardar_tabla  <- FALSE         # Exportar pares calculados a CSV

ancho_figura <- 8
alto_figura  <- 6
dpi_figura   <- 300             # solo para PNG


# Comprobaciones ====

vars <- c(nombre_x, nombre_y, nombre_variable)

faltan <- vars[!vars %in% colnames(data)]

if (length(faltan) > 0) {
  stop(paste("No existen estas columnas en el dataset:", paste(faltan, collapse = ", ")))
}

vars_no_num <- vars[!sapply(data[vars], is.numeric)]

if (length(vars_no_num) > 0) {
  stop(paste("Estas variables deben ser numéricas:", paste(vars_no_num, collapse = ", ")))
}

data_plot <- data[
  complete.cases(data[, vars]),
  vars,
  drop = FALSE
]

if (nrow(data_plot) < 2) {
  stop("Se necesitan al menos dos observaciones válidas para calcular el variograma nube.")
}

cat("\nCoordenada X:", nombre_x, "\n")
cat("Coordenada Y:", nombre_y, "\n")
cat("Variable:", nombre_variable, "\n")
cat("EPSG:", epsg_datos, "\n")
cat("Número de observaciones válidas:", nrow(data_plot), "\n")
cat("Cutoff:", cutoff_variograma, "m\n")


# Conversión a objeto espacial (sf) ====

data_sf <- st_as_sf(
  data_plot,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)


# Variograma nube omnidireccional ====

formula_variograma <- as.formula(
  paste(nombre_variable, "~ 1")
)

cloud_variogram <- variogram(
  formula_variograma,
  data = data_sf,
  cutoff = cutoff_variograma,
  cloud = TRUE
)

cat("\nNúmero de pares de puntos calculados:", nrow(cloud_variogram), "\n")


# Gráfico (ggplot2) ====

grafico <- ggplot(cloud_variogram, aes(x = dist, y = gamma)) +
  geom_point(
    colour = color_puntos,
    size = tam_puntos,
    alpha = alpha_puntos
  ) +
  labs(
    x = Label_x,
    y = Label_y,
    title = Title_graph,
    subtitle = Subtitle_graph
  ) +
  theme_gray() +   # Tema gris por defecto de ggplot2
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0.5,
      color = "black"
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      face = "italic",
      color = "black"
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text = element_text(
      colour = "black",
      size = 10,
      face = "bold"
    )
  )

# Mostrar en pantalla
print(grafico)


# Guardar figura ====

if (guardar_figura) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_figuras <- "salidas/variogramas"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0("030_",
           nombre_dataset, "_",
           nombre_variable,
           "_variograma_nube.",
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
  cat("Archivo:\n", nombre_figura, "\n")
}


# Guardar tabla de pares ====

if (guardar_tabla) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_tablas <- "salidas/tablas"
  
  if (!dir.exists(dir_tablas)) {
    dir.create(dir_tablas, recursive = TRUE)
  }
  
  nombre_tabla <- file.path(
    dir_tablas,
    paste0("030_",
           nombre_dataset, "_",
           nombre_variable,
           "_variograma_nube_pares.csv"
    )
  )
  
  write.csv(cloud_variogram, nombre_tabla, row.names = FALSE)
  
  cat("\n=====================================\n")
  cat(" Tabla de pares guardada en:\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_tabla, "\n\n")
}