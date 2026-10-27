###############################################################
#
#  034. Variograma experimental en cuatro direcciones
#
#  Descripción:
#  Calcula variogramas experimentales direccionales para una
#  variable numérica, utilizando cuatro azimuts principales:
#  0°, 45°, 90° y 135°. La comparación entre las curvas
#  permite explorar la posible existencia de anisotropía,
#  es decir, cambios de la continuidad espacial en función
#  de la dirección.
#
#  Los pares de observaciones se asignan a cada dirección
#  aplicando una tolerancia angular definida por el usuario.
#
#  Entrada:
#  - datos/procesados/fichero.csv
#
#  Salidas:
#  - gráfico en la ventana gráfica de R
#  - salidas/variogramas/
#      034_dataset_variable_variograma_4_direcciones.pdf o .png
#  - salidas/variogramas/
#      034_dataset_variable_variograma_4_direcciones.csv
#
###############################################################


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


# ==== Archivo de datos ====

ruta_datos <- "datos/procesados/fichero_transf.csv"   # <-- EDITAR AQUÍ

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(
  ruta_datos,
  sep = ","
)

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ==== ⚙️ PARÁMETROS A EDITAR ====

nombre_x <- "coord_x"                    # <-- EDITAR AQUÍ
nombre_y <- "coord_y"                    # <-- EDITAR AQUÍ
nombre_variable <- "variable"    # <-- EDITAR AQUÍ

epsg_datos <- 12345                # <-- EDITAR AQUÍ

# Parámetros del variograma
direcciones <- c(0, 45, 90, 135)

cutoff_variograma <- 1234          # distancia máxima analizada
lag_variograma <- 60               # anchura de las clases de distancia
tolerancia_angular <- 22.5         # semiancho angular de cada dirección

# Etiquetas
etiqueta_x <- "Distancia"
etiqueta_y <- "Semivarianza"

titulo_grafico <- "Variograma experimental direccional"
subtitulo_grafico <- nombre_variable

# Apariencia
tam_puntos <- 2
grosor_linea <- 0.7

# Guardado
guardar_figura <- TRUE
guardar_tabla <- FALSE

formato_figura <- "pdf"            # "png" o "pdf"

ancho_figura <- 8
alto_figura <- 6
dpi_figura <- 300                   # solo para PNG


# ==== Comprobaciones ====

variables_necesarias <- c(
  nombre_x,
  nombre_y,
  nombre_variable
)

variables_ausentes <- variables_necesarias[
  !variables_necesarias %in% colnames(data)
]

if (length(variables_ausentes) > 0) {
  stop(
    paste(
      "No existen estas columnas en el dataset:",
      paste(variables_ausentes, collapse = ", ")
    )
  )
}

variables_no_numericas <- variables_necesarias[
  !sapply(data[variables_necesarias], is.numeric)
]

if (length(variables_no_numericas) > 0) {
  stop(
    paste(
      "Estas variables deben ser numéricas:",
      paste(variables_no_numericas, collapse = ", ")
    )
  )
}

if (cutoff_variograma <= 0) {
  stop("cutoff_variograma debe ser mayor que cero.")
}

if (lag_variograma <= 0) {
  stop("lag_variograma debe ser mayor que cero.")
}

if (lag_variograma >= cutoff_variograma) {
  stop("lag_variograma debe ser menor que cutoff_variograma.")
}

if (tolerancia_angular <= 0 || tolerancia_angular > 90) {
  stop("tolerancia_angular debe estar comprendida entre 0 y 90 grados.")
}

if (length(direcciones) < 1) {
  stop("Debe especificarse al menos una dirección.")
}

if (any(direcciones < 0 | direcciones >= 180)) {
  stop("Las direcciones deben estar comprendidas entre 0 y menos de 180 grados.")
}

# Eliminar observaciones incompletas
data_model <- data[
  complete.cases(data[, variables_necesarias]),
  variables_necesarias,
  drop = FALSE
]

if (nrow(data_model) < 3) {
  stop(
    paste(
      "Se necesitan al menos tres observaciones válidas",
      "para calcular el variograma."
    )
  )
}



# ==== Conversión a objeto sf ====

data_sf <- st_as_sf(
  data_model,
  coords = c(nombre_x, nombre_y),
  crs = epsg_datos,
  remove = FALSE
)

if (st_is_longlat(data_sf)) {
  stop(
    paste(
      "El CRS es geográfico.",
      "Para calcular distancias debe utilizarse un CRS proyectado."
    )
  )
}


# ==== Objeto gstat ====

formula_variograma <- as.formula(
  paste(nombre_variable, "~ 1")
)

g <- gstat(
  id = "Z",
  formula = formula_variograma,
  data = data_sf
)


# ==== Variogramas experimentales direccionales ====

variograma_direccional <- variogram(
  g,
  alpha = direcciones,
  cutoff = cutoff_variograma,
  width = lag_variograma,
  tol.hor = tolerancia_angular
)

if (nrow(variograma_direccional) == 0) {
  stop(
    paste(
      "El variograma no contiene clases de distancia.",
      "Revisa el cutoff, el lag y la tolerancia angular."
    )
  )
}

cat("\nVariogramas experimentales calculados:\n")
print(variograma_direccional)


# ==== Preparar etiquetas de dirección ====

variograma_direccional$Direccion <- factor(
  variograma_direccional$dir.hor,
  levels = direcciones,
  labels = paste0(direcciones, "°")
)


# ==== Gráfico ====

grafico <- ggplot(
  variograma_direccional,
  aes(
    x = .data[["dist"]],
    y = .data[["gamma"]],
    colour = .data[["Direccion"]],
    group = .data[["Direccion"]]
  )
) +
  geom_point(
    size = tam_puntos
  ) +
  geom_line(
    linewidth = grosor_linea
  ) +
  labs(
    x = etiqueta_x,
    y = etiqueta_y,
    title = titulo_grafico,
    subtitle = subtitulo_grafico,
    colour = "Dirección"
  ) +
  coord_cartesian(
    xlim = c(0, cutoff_variograma)
  ) +
  theme_gray() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0.5,
      colour = "black"
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      face = "italic",
      colour = "black"
    ),
    axis.text = element_text(
      colour = "black",
      size = 10,
      face = "bold"
    ),
    axis.title = element_text(
      face = "bold"
    ),
    legend.title = element_text(
      face = "bold"
    )
  )

print(grafico)


# ==== Guardar resultados ====

if (guardar_figura) {
  
  nombre_dataset <- tools::file_path_sans_ext(
    basename(ruta_datos)
  )
  
  dir_variogramas <- "salidas/variogramas"
  
  if (!dir.exists(dir_variogramas)) {
    dir.create(
      dir_variogramas,
      recursive = TRUE
    )
  }
}


# ==== Guardar figura ====

if (guardar_figura) {
  
  nombre_figura <- file.path(
    dir_variogramas,
    paste0(
      "034_",
      nombre_dataset, "_",
      nombre_variable,
      "_variograma_4_direcciones.",
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
      height = alto_figura,
      device = "pdf"
    )
    
  } else {
    
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
 cat(" Figura guardada\n")
 cat("Archivo:\n", nombre_figura, "\n")
  cat("Existe:", file.exists(nombre_figura), "\n")
}


# Guardar tabla del variograma direccional ====

if (guardar_tabla) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_tablas <- "salidas/tablas"
  
  if (!dir.exists(dir_tablas)) {
    dir.create(dir_tablas, recursive = TRUE)
  }
  
  nombre_tabla <- file.path(
    dir_tablas,
    paste0("034_",
           nombre_dataset, "_",
           nombre_variable,
           "_variograma_4_direc",
           ".csv"
    )
  )
  
  write.csv(variograma_direccional, nombre_tabla, row.names = FALSE)
  
  cat(" Tabla guardada\n")
  cat("Archivo:\n", nombre_tabla, "\n\n")
}
