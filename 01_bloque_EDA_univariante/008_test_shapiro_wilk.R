#####################################################
#
#       008 Test Shapiro_Wilk      
# Evalúa la normalidad de una variable mediante el test de Shapiro-Wilk,
# proporcionando un p-value y una conclusión sobre la adecuación a una
# distribución normal, útil para decidir transformaciones posteriores.
#
#####################################################


# cargamos librerias, si es necesario, se instala ====


if (!requireNamespace("nortest", quietly = TRUE)) install.packages("nortest")

library(nortest) 



# Archivo de datos ====

# Ruta al fichero CSV dentro de la estructura del proyecto
ruta_datos <- "datos/brutos/fichero.csv"

# Comprobar que el fichero existe
if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

# Leer datos
data <- read.csv(ruta_datos, sep = ",")



# Selección de variable ====

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))

# Escribir aquí el nombre de la variable a analizar
nombre_variable <- "columna"   # <-- EDITAR AQUÍ

# Comprobar que la variable existe en el data frame
if (!nombre_variable %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable), "no existe en el fichero de datos."))
}

# Extraer la variable
Parametro <- data[[nombre_variable]]

# Opcional: eliminar NA antes de calcular estadísticos
Parametro <- Parametro[!is.na(Parametro)]

# Comprobar que quedan datos
if (length(Parametro) == 0) {
  stop("La variable seleccionada no contiene datos válidos tras eliminar NA.")
}

cat("\nVariable seleccionada:", nombre_variable, "\n")


# Realizar la prueba de Shapiro-Wilk
ShapiroResult <- shapiro.test(Parametro)

# Imprimir los resultados
print(ShapiroResult)

# Se obtiene un parámetro "p" que debe ser mayor que 0,05 
# (probabilidad del 95\%) para no rechazar H0, que es 
# la hipótesis de normalidad de la distribución, o sea,
# si p-value < 0.05, no es distribución normal

p_valor <- ShapiroResult$p.value

if (p_valor < 0.05) {
  conclusion <- "NO normal"
} else {
  conclusion <- "Compatible con normalidad"
}

cat("\n=====================================\n")
cat("Test de Shapiro-Wilk\n")
cat("=====================================\n")
cat("p-value:", signif(p_valor, 4), "\n")
cat("Conclusión:", conclusion, "\n")
# Interpretación:
# - p-value < 0.05 → se rechaza normalidad
# - p-value ≥ 0.05 → compatible con normalidad
