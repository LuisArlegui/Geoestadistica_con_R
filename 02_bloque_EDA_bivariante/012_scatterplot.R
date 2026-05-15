#####################################################
#
#  012. Scatter Plot simple
#
#  Descripción:
#  Genera un sencillo gráfico de dispersión entre dos variables
#  numéricas para explorar visualmente su relación,
#  detectar tendencias, correlaciones y posibles outliers.
#
#  Entrada:
#  - datos/brutos/fichero.csv
#
#  Salida:
#  - gráfico en la ventana gráfica de R
#
#####################################################



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

Variable_X <- data$columna1  	# elegimos una variable para el eje x
Variable_Y <- data$columna2	# elegimos la otra variable, para el y



# Gráfico ====

plot(Variable_Y ~ Variable_X,
     xlab = "eje x",
     ylab = "eje y",
     main = "Titulo")
