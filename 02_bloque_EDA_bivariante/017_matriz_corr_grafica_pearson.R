#####################################################
#
#  017. Matriz de correlación gráfica (Pearson)
#
#  Descripción:
#  Calcula y representa gráficamente una matriz de
#  correlación de Pearson entre variables numéricas,
#  útil para explorar relaciones multivariantes de
#  forma rápida e intuitiva.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#  - salidas/figuras/dataset_matriz_correlacion_pearson.png o .pdf
#  - (opcional) salidas/tablas/dataset_correlacion_Pearson.csv
#
#####################################################



# Cargar librerías ====

if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
library(corrplot)



# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

# Columnas a excluir del análisis
columnas_excluir <- c(
  "columna1",
  "columna2",
  "columna3",
  "columna4",
  "columna5"
)   # <-- EDITAR AQUÍ

# Método gráfico de corrplot: "circle", "square", "ellipse", etc.
metodo_corrplot <- "circle"   # <-- EDITAR AQUÍ

# Título del gráfico
titulo_grafico <- "Matriz de correlación de Pearson"   # <-- EDITAR AQUÍ

# Guardado de resultados
guardar_figura <- TRUE        # <-- TRUE / FALSE
guardar_tabla  <- TRUE        # <-- TRUE / FALSE

# Formato de figura: "png" o "pdf"
formato_figura <- "pdf"       # <-- EDITAR AQUÍ

# Dimensiones de exportación
ancho_figura <- 8
alto_figura  <- 8
dpi_figura   <- 300           # solo se usa en PNG



# Preparación de datos ====

data_filtrado <- data[, !(names(data) %in% columnas_excluir), drop = FALSE]

cat("\nColumnas excluidas del análisis:\n")
print(columnas_excluir)

cat("\nColumnas restantes tras el filtrado:\n")
print(colnames(data_filtrado))

if (ncol(data_filtrado) == 0) {
  stop("No quedan columnas tras aplicar el filtrado.")
}

data_num <- data_filtrado[, sapply(data_filtrado, is.numeric), drop = FALSE]

cat("\nVariables numéricas utilizadas en la matriz de correlación:\n")
print(colnames(data_num))

if (ncol(data_num) < 2) {
  stop("Se necesitan al menos dos variables numéricas para calcular una matriz de correlación.")
}



# Resumen estadístico ====

cat("\nResumen estadístico de las variables seleccionadas:\n")
print(summary(data_num))



# Matriz de correlación ====

correlaciones <- cor(data_num, use = "complete.obs", method = "pearson")

cat("\nMatriz de correlación calculada:\n")
print(correlaciones)



# Gráfico en pantalla ====

corrplot(
  correlaciones,
  method = metodo_corrplot,
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  mar = c(0, 0, 1, 0),
  title = titulo_grafico
)



# Guardar figura ====

if (guardar_figura) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_figuras <- "salidas/figuras"
  
  if (!dir.exists(dir_figuras)) {
    dir.create(dir_figuras, recursive = TRUE)
  }
  
  nombre_figura <- file.path(
    dir_figuras,
    paste0(nombre_dataset, "_matriz_correlacion_pearson.", formato_figura)
  )
  
  # Dispositivo gráfico según formato
  if (formato_figura == "png") {
    
    png(
      filename = nombre_figura,
      width = ancho_figura,
      height = alto_figura,
      units = "in",
      res = dpi_figura
    )
    
  } else if (formato_figura == "pdf") {
    
    pdf(
      file = nombre_figura,
      width = ancho_figura,
      height = alto_figura
    )
    
  } else {
    stop("Formato no soportado. Usa 'png' o 'pdf'.")
  }
  
  # Dibujar gráfico en el dispositivo
  corrplot(
    correlaciones,
    method = metodo_corrplot,
    type = "upper",
    tl.col = "black",
    tl.srt = 45,
    addCoef.col = "black",
    mar = c(0, 0, 1, 0),
    title = titulo_grafico
  )
  
  dev.off()
  
  cat("\n=====================================\n")
  cat(" Figura guardada\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_figura, "\n\n")
}



# Guardar tabla de correlaciones ====

if (guardar_tabla) {
  
  nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))
  
  dir_tablas <- "salidas/tablas"
  
  if (!dir.exists(dir_tablas)) {
    dir.create(dir_tablas, recursive = TRUE)
  }
  
  nombre_tabla <- file.path(
    dir_tablas,
    paste0(nombre_dataset, "_correlacion_Pearson.csv")
  )
  
  write.csv(correlaciones, file = nombre_tabla, row.names = TRUE)
  
  cat("\n=====================================\n")
  cat(" Tabla guardada\n")
  cat("=====================================\n")
  cat("Archivo:\n", nombre_tabla, "\n\n")
}