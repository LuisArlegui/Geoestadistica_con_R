#####################################################
#
#  003. Tabla resumen estadístico
#
#  Descripción:
#  Calcula estadísticos descriptivos básicos de una
#  variable numérica y guarda el resultado en CSV.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - salidas/tablas/dataset_variable_Resumen_Estat.csv
#
#####################################################



# Cargar librerías ====

if (!requireNamespace("moments", quietly = TRUE)) install.packages("moments")
library(moments)



# Archivo de datos ====

# Ruta al fichero CSV dentro de la estructura del proyecto
ruta_datos <- "datos/brutos/fichero.csv" # <-- ⚠️ EDITAR AQUÍ

# Comprobar que el fichero existe
if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

# Leer datos
data <- read.csv(ruta_datos, sep = ",") # <-- ⚠️ EDITAR AQUÍ (si falla el sep)

# Mostrar nombres de columnas para facilitar la elección de variable
cat("Columnas disponibles en el dataset:\n")
print(colnames(data))



# ⚙️ PARÁMETROS A EDITAR ====

# Escribir aquí el nombre de la variable a analizar
nombre_variable <- "columna"   # <-- ⚠️ EDITAR AQUÍ



# Comprobaciones ====

# Verificar que la variable existe en el dataset
if (!nombre_variable %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable), "no existe en el fichero de datos."))
}

# Verificar que la variable es numérica
if (!is.numeric(data[[nombre_variable]])) {
  stop(paste("La variable", shQuote(nombre_variable), "debe ser numérica."))
}

# Extraer la variable
Parametro <- data[[nombre_variable]]

# Eliminar NA para evitar problemas en los cálculos
Parametro <- Parametro[!is.na(Parametro)]

# Comprobar que quedan datos válidos
if (length(Parametro) == 0) {
  stop("La variable seleccionada no contiene datos válidos tras eliminar NA.")
}

cat("\nVariable seleccionada:", nombre_variable, "\n")



# Resumen estadístico ====

Resumen_estadistico <- data.frame(
  Resumen_estadistico = c(
    sum(Parametro),
    length(Parametro),
    length(unique(Parametro)),
    min(Parametro),
    max(Parametro),
    max(Parametro) - min(Parametro),
    mean(Parametro),
    median(Parametro),
    as.numeric(quantile(Parametro, 0.25)),
    as.numeric(quantile(Parametro, 0.75)),
    sd(Parametro),
    skewness(Parametro),
    kurtosis(Parametro)
  ),
  row.names = c(
    "Suma",
    "Contaje",
    "Valores_unicos",
    "Valor_minimo",
    "Valor_maximo",
    "Rango",
    "Media",
    "Mediana",
    "Q1",
    "Q3",
    "Desviacion_estandar",
    "Skewness",
    "Curtosis"
  )
)



# Mostrar resultado en consola ====

print(Resumen_estadistico)



# Guardar resultado en CSV ====

# Nombre del dataset sin extensión
nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))

# Carpeta de salida
dir_salida <- "salidas/tablas"

if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
}

# Nombre del archivo de salida
nombre_salida <- file.path(
  dir_salida,
  paste0(nombre_dataset, "_", nombre_variable, "_Resumen_Estat.csv")
)

# Guardar CSV
write.csv(Resumen_estadistico, nombre_salida, row.names = TRUE)

cat("\nArchivo guardado en:\n", nombre_salida, "\n")