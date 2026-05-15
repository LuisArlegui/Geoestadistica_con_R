#####################################################
#
#       019 Clasificación de 'dist.m' en categorías
#       script MUY específico para meuse
#
#####################################################


# Cargar librerías ====

if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)



# Archivo de datos ====

ruta_datos <- "datos/brutos/meuse.csv"

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")


# Comprobación de variable ====

if (!"dist.m" %in% colnames(data)) {
  stop("La variable 'dist.m' no existe en el dataset")
}



# Clasificación de 'dist.m' ====

data <- data %>%
  mutate(dist_catego = case_when(
    dist.m >= 0   & dist.m < 100 ~ "0-100m",
    dist.m >= 100 & dist.m < 200 ~ "100-200m",
    dist.m >= 200 & dist.m < 300 ~ "200-300m",
    dist.m >= 300 & dist.m < 400 ~ "300-400m",
    dist.m >= 400 & dist.m < 500 ~ "400-500m",
    dist.m >= 500 & dist.m < 600 ~ "500-600m",
    dist.m >= 600 & dist.m < 700 ~ "600-700m",
    dist.m >= 700 & dist.m < 800 ~ "700-800m",
    dist.m >= 800 & dist.m < 900 ~ "800-900m",
    dist.m >= 900               ~ "900m o más",
    TRUE ~ NA_character_
  ))



# Visualización rápida ====

cat("\nPrimeras filas del dataset:\n")
print(head(data))



# Guardar resultado ====

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
  paste0(nombre_dataset, "_categorizado.csv")
)

# Guardar CSV
write.csv(data, file = nombre_salida, row.names = FALSE)

cat("\n=====================================\n")
cat(" Clasificación completada\n")
cat("=====================================\n")
cat("Archivo guardado en:\n", nombre_salida, "\n\n")
