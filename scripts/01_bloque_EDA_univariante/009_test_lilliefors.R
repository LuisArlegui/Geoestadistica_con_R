#####################################################
#
# 009_Test_Lilliefors
#
# Evalúa la normalidad de una variable mediante el test de Lilliefors,
# proporcionando un p-value y una conclusión sobre la adecuación a una
# distribución normal, útil en muestras de tamaño moderado o grande.
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

# Tamaño muestral
n <- length(Parametro)

if (n < 5) {
  stop("El test de Lilliefors requiere al menos 5 observaciones válidas.")
}

# Comprobar variabilidad
if (length(unique(Parametro)) < 2) {
  stop("La variable es constante. No puede aplicarse el test de Lilliefors.")
}

cat("\nVariable seleccionada:", nombre_variable, "\n")
cat("Número de observaciones:", n, "\n")


# Realizar la prueba de Lilliefors ====



LillieforsResult <- lillie.test(Parametro)

# Imprimir los resultados
print(LillieforsResult)


# Interpretación del contraste ====

# Nivel de significación
alpha <- 0.05

# Extraer resultados
p_valor <- LillieforsResult$p.value
D <- unname(LillieforsResult$statistic)

# Interpretación
if (p_valor < alpha) {
  conclusion <- "Se rechaza la hipótesis de normalidad."
} else {
  conclusion <- "No se rechaza la hipótesis de normalidad."
}

# Mostrar resultados
cat("\n=====================================\n")
cat("Test de Lilliefors\n")
cat("=====================================\n")
cat("Variable:", nombre_variable, "\n")
cat("Observaciones:", n, "\n")
cat("Estadístico D:", round(D, 4), "\n")
cat("p-value:", signif(p_valor, 4), "\n")
cat("Nivel de significación:", alpha, "\n")
cat("Conclusión:", conclusion, "\n")