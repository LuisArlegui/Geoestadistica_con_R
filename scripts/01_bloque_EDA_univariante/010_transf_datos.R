#####################################################
#
#  010. Transformación de datos
#
#  Descripción:
#  Genera nuevas variables transformadas a partir de una variable numérica
#  del dataset original (logaritmo en base 10, inverso y raíz cuadrada),
#  y guarda el resultado como un nuevo archivo CSV en datos/procesados.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - datos/procesados/dataset_transf.csv
#
#####################################################

# Cargar librerías ====

if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)


# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))

# ⚙️ PARÁMETROS A EDITAR ====

nombre_variable <- "variable"   # <-- EDITAR AQUÍ


# Comprobaciones ====

if (!nombre_variable %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable), "no existe en el dataset"))
}

if (!is.numeric(data[[nombre_variable]])) {
  stop(paste("La variable", shQuote(nombre_variable), "debe ser numérica"))
}

# Eliminar NA (opcional pero recomendable)
data <- data[!is.na(data[[nombre_variable]]), ]

if (nrow(data) == 0) {
  stop("No quedan datos válidos tras eliminar NA.")
}

Parametro <- data[[nombre_variable]]

cat("\nVariable seleccionada:", nombre_variable, "\n")
cat("Número de observaciones válidas:", length(Parametro), "\n")


# Advertencias sobre transformaciones ====

if (any(Parametro <= 0, na.rm = TRUE)) {
  warning("Hay valores <= 0: log10 puede producir -Inf o NaN")
}

if (any(Parametro == 0, na.rm = TRUE)) {
  warning("Hay ceros: el inverso producirá Inf")
}


# Transformaciones ====

# Construcción automática de nombres de columnas
nombre_log   <- paste0("log10_", nombre_variable)
nombre_inv   <- paste0("inv_", nombre_variable)
nombre_raiz  <- paste0("sqrt_", nombre_variable)

# Aplicar transformaciones respetando sus dominios matemáticos

x <- data[[nombre_variable]]

data[[nombre_log]] <- NA_real_
data[[nombre_inv]] <- NA_real_
data[[nombre_raiz]] <- NA_real_

# Logaritmo: x > 0
validos_log <- is.finite(x) & x > 0
data[[nombre_log]][validos_log] <- log10(x[validos_log])

# Inverso: x != 0
validos_inv <- is.finite(x) & x != 0
data[[nombre_inv]][validos_inv] <- 1 / x[validos_inv]

# Raíz cuadrada: x >= 0
validos_raiz <- is.finite(x) & x >= 0
data[[nombre_raiz]][validos_raiz] <- sqrt(x[validos_raiz])

cat("\nTransformaciones generadas:\n")
cat("-", nombre_log, "\n")
cat("-", nombre_inv, "\n")
cat("-", nombre_raiz, "\n")



# Guardar dataset transformado ====

# Nombre del dataset sin extensión
nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))

# Carpeta de salida
dir_salida <- "datos/procesados"

if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
}

# Nombre del archivo de salida
nombre_salida <- file.path(
  dir_salida,
  paste0(nombre_dataset, "_transf.csv")
)

# Guardar CSV
write.csv(data, nombre_salida, row.names = FALSE)

cat("\n=====================================\n")
cat(" Transformación completada\n")
cat("=====================================\n")
cat("Archivo guardado en:\n", nombre_salida, "\n\n")



# Visualización (primeras filas) ====

cat("Primeras filas del dataset transformado:\n")
print(head(data))

