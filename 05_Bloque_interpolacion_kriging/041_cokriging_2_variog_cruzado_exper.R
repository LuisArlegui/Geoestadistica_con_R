#####################################################
#
#  041. Cokriging 2: variograma cruzado experimental
#
#  Descripción:
#  Calcula los variogramas directos y el variograma
#  cruzado entre una variable objetivo y una variable
#  auxiliar. En este caso, la variable objetivo es el
#  contenido en cloruros medido en LabSamples, y la
#  variable auxiliar es la conductividad de campo FmSm,
#  disponible con mayor densidad espacial tras la unión
#  realizada en el script 040.
#
#  Entrada:
#  - datos/brutos/cloruros/LabSamples.csv
#  - datos/procesados/Field_conduc_combi.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - salidas/figuras/dataset_objetivo_auxiliar_variograma_cruzado.png o .pdf
#  - salidas/tablas/dataset_objetivo_auxiliar_variograma_cruzado.csv
#
#####################################################

# ==== Cargar librerías ====

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gstat", quietly = TRUE)) install.packages("gstat")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("mapview", quietly = TRUE)) install.packages("mapview")

library(ggplot2)
library(gstat)
library(sf)
library(mapview)

# ==== Archivos de datos ====

ruta_objetivo <- "datos/brutos/cloruros/LabSamples.csv"
ruta_auxiliar <- "datos/procesados/Field_conduc_combi.csv"

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

# Dataset objetivo: LabSamples
nombre_objetivo <- "ClmgKg"

# Dataset auxiliar: Field_conduc_combi
nombre_auxiliar <- "FmSm"

epsg_datos <- 32613

# Identificadores internos para gstat y leyendas
id_objetivo <- "Cl"
id_auxiliar <- "FmSm"

# Variograma
cutoff_variograma <- 1400
lag_variograma <- 140

# Etiquetas y títulos
Label_x <- "Distancia"
Label_y <- "Semivarianza"
Title_graph <- "Variogramas directos y cruzado"
Subtitle_graph <- "Cokriging: ClmgKg y FmSm"

# Opciones
mostrar_mapa <- TRUE
mostrar_np <- TRUE

# Guardado
guardar_figura <- TRUE
guardar_tabla <- TRUE
formato_figura <- "pdf"

ancho_figura <- 9
alto_figura  <- 6
dpi_figura   <- 300

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
  stop(paste(
    "Estas columnas del dataset objetivo deben ser numéricas:",
    paste(vars_no_num_obj, collapse = ", ")
  ))
}

if (length(vars_no_num_aux) > 0) {
  stop(paste(
    "Estas columnas del dataset auxiliar deben ser numéricas:",
    paste(vars_no_num_aux, collapse = ", ")
  ))
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
cat("Cutoff:", cutoff_variograma, "\n")
cat("Lag:", lag_variograma, "\n")

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

# ==== Mapas rápidos ====

if (mostrar_mapa) {
  
  print(
    mapview(
      z1_sf,
      zcol = nombre_objetivo,
      map.types = "CartoDB.Voyager"
    )
  )
  
  print(
    mapview(
      z2_sf,
      zcol = nombre_auxiliar,
      map.types = "CartoDB.Voyager"
    )
  )
}

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

cross_variogram <- variogram(
  g,
  cutoff = cutoff_variograma,
  width = lag_variograma
)

if (nrow(cross_variogram) == 0) {
  stop("El variograma cruzado no contiene clases. Revisa cutoff y lag.")
}

cat("\nVariogramas directos y cruzado:\n")
print(cross_variogram)

# ==== Gráfico ====

grafico <- ggplot(
  cross_variogram,
  aes(x = dist, y = gamma, colour = id)
) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.7) +
  labs(
    x = Label_x,
    y = Label_y,
    title = Title_graph,
    subtitle = Subtitle_graph,
    colour = "Variograma"
  ) +
  scale_color_manual(
    values = c("blue", "green", "red"),
    labels = c(
      paste0(nombre_objetivo, " (directo)"),
      paste0(nombre_auxiliar, " (directo)"),
      paste0(nombre_objetivo, "-", nombre_auxiliar, " (cruzado)")
    )
  ) +
  xlim(0, cutoff_variograma) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

if (mostrar_np) {
  grafico <- grafico +
    geom_text(
      aes(label = np),
      hjust = -0.15,
      vjust = 0,
      size = 3,
      show.legend = FALSE
    )
}

print(grafico)

# ==== Guardar figura ====

if (guardar_figura) {
  
  nombre_dataset_obj <- tools::file_path_sans_ext(basename(ruta_objetivo))
  nombre_dataset_aux <- tools::file_path_sans_ext(basename(ruta_auxiliar))
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0(
      nombre_dataset_obj, "_",
      nombre_dataset_aux,
      "_variograma_cruzado.",
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
  
  cat("\n=====================================\n")
  cat(" Figura guardada\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_figura, "\n\n")
}

# ==== Guardar tabla ====

if (guardar_tabla) {
  
  nombre_dataset_obj <- tools::file_path_sans_ext(basename(ruta_objetivo))
  nombre_dataset_aux <- tools::file_path_sans_ext(basename(ruta_auxiliar))
  
  dir_tablas <- "salidas/tablas"
  
  if (!dir.exists(dir_tablas)) {
    dir.create(dir_tablas, recursive = TRUE)
  }
  
  nombre_tabla <- file.path(
    dir_tablas,
    paste0(
      nombre_dataset_obj, "_",
      nombre_dataset_aux,
      "_variograma_cruzado.csv"
    )
  )
  
  write.csv(
    cross_variogram,
    nombre_tabla,
    row.names = FALSE
  )
  
  cat("\n=====================================\n")
  cat(" Tabla guardada\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_tabla, "\n\n")
}

