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
nombre_variable <- "variable"   # <-- EDITAR AQUÍ

# Comprobar que la variable existe en el data frame
if (!nombre_variable %in% colnames(data)) {
  stop(paste("La variable", shQuote(nombre_variable), "no existe en el fichero de datos."))
}

# Comprobar que la variable es numérica
if (!is.numeric(data[[nombre_variable]])) {
  stop(paste("La variable", shQuote(nombre_variable), "debe ser numérica."))
}

# Extraer la variable y eliminar valores no finitos
Parametro <- data[[nombre_variable]]
Parametro <- Parametro[is.finite(Parametro)]

# Comprobar tamaño muestral
n <- length(Parametro)

if (n < 3 || n > 5000) {
  stop("El test de Shapiro-Wilk requiere entre 3 y 5000 observaciones válidas.")
}

# Comprobar que existe variabilidad
if (length(unique(Parametro)) < 2) {
  stop("La variable es constante. No puede aplicarse el test de Shapiro-Wilk.")
}

cat("\nVariable seleccionada:", nombre_variable, "\n")
cat("Número de observaciones:", n, "\n")

cat("\nVariable seleccionada:", nombre_variable, "\n")


# Realizar la prueba de Shapiro-Wilk
ShapiroResult <- shapiro.test(Parametro)

# Imprimir los resultados
print(ShapiroResult)

# Se obtiene un parámetro "p" que debe ser mayor que 0,05 
# (probabilidad del 95\%) para no rechazar H0, que es 
# la hipótesis de normalidad de la distribución, o sea,
# si p-value < 0.05, no es distribución normal

# Nivel de significación
alpha <- 0.05

# Extraer resultados
p_valor <- ShapiroResult$p.value
W <- unname(ShapiroResult$statistic)

# Interpretación
if (p_valor < alpha) {
  conclusion <- "Se rechaza la hipótesis de normalidad."
} else {
  conclusion <- "No se rechaza la hipótesis de normalidad."
}

cat("\n=====================================\n")
cat("Test de Shapiro-Wilk\n")
cat("=====================================\n")
cat("Variable:", nombre_variable, "\n")
cat("Observaciones:", n, "\n")
cat("Estadístico W:", round(W, 4), "\n")
cat("p-value:", signif(p_valor, 4), "\n")
cat("Nivel de significación:", alpha, "\n")
cat("Conclusión:", conclusion, "\n")