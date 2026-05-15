#####################################################
#
#        034 Variograma 4 direcciones       
#
#####################################################

# Cargar las bibliotecas necesarias----
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gstat", quietly = TRUE)) install.packages("gstat")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(ggplot2)
library(gstat)
library(sf)

# Definir el archivo CSV y cargar los datos----
csv_file <- "datos/procesados/fichero.csv"  # Cambia esto al camino de tu archivo
data <- read.csv(csv_file)

# Verificar nombres de columnas
print(colnames(data))

# Definir las variables a usar----
# Supongamos que tus datos tienen columnas "x", "y", y "parametro"
x_col <- "columnaX"  # Cambia esto al nombre de la columna x en tu archivo CSV
y_col <- "columnaY"  # Cambia esto al nombre de la columna y en tu archivo CSV
z_col <- "objetivo"  # Cambia esto al nombre de la columna de tu parámetro en tu archivo CSV

cutoff <- 1234  # Cambia el valor (máxima distancia entre puntos, para empezar)
lag <- 12  # Cambia este valor: espaciado entre puntos
tolerance_angle <- 22.5 # al definir 4 direcciones, la tolerancia debe ser 45/2

# Definir etiquetas y título----
Label_x <- "Distancia"
Label_y <- "Semivarianza"
Title_graph <- "Variograma Experimental Direccional"
Subtitle_graph <- "Datos Ejemplo"

# Objeto espacial y variograma----
# Convertir los datos a un objeto sf
data_sf <- st_as_sf(data, coords = c(x_col, y_col), crs = 12345)  # Ajusta el CRS según corresponda

# Crear un objeto de gstat
g <- gstat(id = 'Z', formula = as.formula(paste(z_col, "~ 1")), data = data_sf)

# Calcular el variograma direccional
variogram_data <- variogram(g, alpha = c(0, 45, 90, 135), cutoff = cutoff, tol.hor = tolerance_angle, width = lag)

# Graficar el variograma----
p <- ggplot(variogram_data, aes(x = dist, y = gamma, col = factor(dir.hor))) + 
  geom_point(size = 2) +
  geom_line() + 
  labs(x = Label_x, y = Label_y, title = Title_graph, subtitle = Subtitle_graph) + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "black")) + 
  theme(axis.text = element_text(colour = "black", size = 10, face = "bold")) + 
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", color = "black")) +
  labs(colour = "Dirección")


p