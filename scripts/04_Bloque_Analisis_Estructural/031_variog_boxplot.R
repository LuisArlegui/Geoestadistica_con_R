#####################################################
#
#  031. Variograma nube con boxplots
#
#  Descripción:
#  Calcula el variograma nube omnidireccional de una
#  variable regionalizada y resume la dispersión de las
#  semivarianzas mediante boxplots agrupados por clases
#  de distancia. Es útil para explorar la variabilidad
#  de los pares y orientar la elección de lag y cutoff.
#
#  Entrada:
#  - datos/brutos/wolfcamp.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - (opcional) salidas/variogramas/031_dataset_variable_variograma_nube_boxplots.png o .pdf
#  - (opcional) salidas/tablas/031_dataset_variable_variograma_nube_boxplots.csv
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

ruta_datos <- "datos/brutos/fichero.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ⚙️ PARÁMETROS A EDITAR ====

nombre_x        <- "coord_x"                 # <-- EDITAR AQUÍ
nombre_y        <- "coord_y"                 # <-- EDITAR AQUÍ
nombre_variable <- "variable"                # <-- EDITAR AQUÍ

epsg_datos <- 12345             # <-- EDITAR AQUÍ: Código EPSG (ej. 32613 = UTM zone 13N)

cutoff_variograma <- 100000     # Distancia máxima (en metros)
lag_cajas         <- 10000      # Anchura de clase/intervalo para los boxplots (en metros)

Label_x        <- "Clase de distancia (m)"
Label_y        <- "Semivarianza γ(h)"
Title_graph    <- "Variograma Nube Omnidireccional con Boxplots"
Subtitle_graph <- "Distribución de semivarianzas por clases de distancia"

# Estética
color_box   <- "skyblue"
color_borde <- "gray30"
alpha_box   <- 0.6

# Guardado
guardar_figura <- TRUE
formato_figura <- "pdf"         # "png" o "pdf"
guardar_tabla  <- TRUE         # Exportar resumen por clases a CSV

ancho_figura <- 9
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

if (cutoff_variograma <= 0) {
  stop("cutoff_variograma debe ser mayor que cero.")
}

if (lag_cajas <= 0) {
  stop("lag_cajas debe ser mayor que cero.")
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
cat("Lag de cajas:", lag_cajas, "m\n")


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

if (nrow(cloud_variogram) == 0) {
  stop("El variograma nube no contiene pares. Revisa cutoff_variograma.")
}

cat("\nNúmero de pares calculados:", nrow(cloud_variogram), "\n")


# Crear clases de distancia ====

max_dist <- max(cloud_variogram$dist, na.rm = TRUE)
breaks_dist <- seq(
  from = 0,
  to = ceiling(max_dist / lag_cajas) * lag_cajas,
  by = lag_cajas
)

cloud_variogram$clase_distancia <- cut(
  cloud_variogram$dist,
  breaks = breaks_dist,
  include.lowest = TRUE,
  right = FALSE
)

# Eliminar posibles filas sin clase asignada
cloud_variogram <- cloud_variogram[!is.na(cloud_variogram$clase_distancia), ]


# Gráfico (ggplot2 con theme_gray) ====

grafico <- ggplot(cloud_variogram, aes(x = clase_distancia, y = gamma)) +
  geom_boxplot(
    fill = color_box,
    color = color_borde,
    alpha = alpha_box,
    outlier.alpha = 0.4,
    outlier.size = 1
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
      size = 16,
      hjust = 0.5,
      color = "black"
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      face = "italic",
      color = "black"
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text.x = element_text(
      colour = "black",
      size = 8,
      face = "bold",
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(
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
    paste0("031_",
           nombre_dataset, "_",
           nombre_variable,
           "_variograma_nube_boxplots.",
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


# Guardar tabla de resumen por clases ====

if (guardar_tabla) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  dir_tablas <- "salidas/tablas"
  
  if (!dir.exists(dir_tablas)) {
    dir.create(dir_tablas, recursive = TRUE)
  }
  
  # Resumen estadístico por clase de distancia
  tabla_resumen_clases <- aggregate(
    gamma ~ clase_distancia,
    data = cloud_variogram,
    FUN = function(x) {
      c(
        N_pares = length(x),
        Min = min(x),
        Q1 = quantile(x, 0.25),
        Mediana = median(x),
        Media = mean(x),
        Q3 = quantile(x, 0.75),
        Max = max(x)
      )
    }
  )
  
  # Aplanar la matriz de estadísticos resultante
  tabla_resumen_clases <- data.frame(
    Clase_distancia = tabla_resumen_clases$clase_distancia,
    tabla_resumen_clases$gamma
  )
  
  nombre_tabla <- file.path(
    dir_tablas,
    paste0("031_",
           nombre_dataset, "_",
           nombre_variable,
           "_variograma_nube_boxplots.csv"
    )
  )
  
  write.csv(tabla_resumen_clases, nombre_tabla, row.names = FALSE)
  
  cat("\n=====================================\n")
  cat(" Tabla resumen por clases guardada en:\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_tabla, "\n\n")
}