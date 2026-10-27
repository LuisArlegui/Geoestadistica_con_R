#        001. Histograma sencillo (solo seis lineas de código R!)

# carga la librería necesaria para importar csv

if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
library(readr)

# con esa libreria, lee el documento, ojo con el working directory

meuse <- read_csv("datos/brutos/meuse.csv")

# generación de un histograma automático pero pintón

hist(meuse$zinc, breaks=13, main="Meandro del Meuse, estudio de contaminación del suelo",
     xlab="Zn [ppm]")

# podemos añadir, con la función rug, los datos situados sobre el eje x

rug(meuse$zinc)

# obtenemos un resumen de la variable Zn

summary(meuse$zinc)
