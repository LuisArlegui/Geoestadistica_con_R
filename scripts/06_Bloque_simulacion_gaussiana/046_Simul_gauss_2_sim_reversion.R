#####################################################
#
#  046. Simulación gaussiana 2: SGS y reversión
#
#  Realiza simulación secuencial gaussiana condicional
#  utilizando los datos gaussianizados, el variograma
#  y la rejilla del script 045. Retrotransforma cada
#  realización mediante anamorfosis empírica.
#
#  Requisito: ejecutar previamente el script 045,
#  incluida la exportación de datos gaussianizados
#  (045_*_datos_gaussianizados.rds).
#
#  Entradas (modelos/):
#    045_dataset_variable_datos_gaussianizados.rds
#    045_dataset_variable_anamorfosis.rds
#    045_dataset_variable_modelo_variograma_gaussiano.rds
#    045_dataset_variable_rejilla.rds
#
#  Salidas:
#    modelos/046_dataset_variable_simulaciones_gaussianas.rds
#    modelos/046_dataset_variable_simulaciones_originales.rds
#    salidas/figuras/046_dataset_variable_simulacion_gaussiana_N.pdf/png
#    salidas/figuras/046_dataset_variable_simulacion_original_N.pdf/png
#
#####################################################

# ==== Cargar e instalar librerías automáticamente ====

paquetes_requeridos <- c("terra", "gstat", "sf", "ggplot2", "viridis")

instalar_si_falta <- function(pkgs) {
  nuevos <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(nuevos) > 0) {
    message("Instalando paquetes faltantes: ", paste(nuevos, collapse = ", "))
    install.packages(nuevos, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

instalar_si_falta(paquetes_requeridos)

# ==== Parámetros a editar ====

ruta_datos <- "datos/brutos/meuse.csv"       # <-- EDITAR AQUÍ (para identificar el dataset)
nombre_variable <- "zinc"                    # <-- EDITAR AQUÍ
nombre_x <- "x"                             # <-- EDITAR AQUÍ (etiqueta del gráfico)
nombre_y <- "y"                             # <-- EDITAR AQUÍ (etiqueta del gráfico)

numero_simulaciones <- 1000                  # <-- EDITAR AQUÍ
nmax_vecinos <- 50                          # <-- EDITAR AQUÍ
media_gaussiana <- 0                        # Media SK, coherente con script 045
semilla <- 1234                             # <-- EDITAR AQUÍ
sim_visualizar <- 48                        # <-- EDITAR AQUÍ

guardar_modelos <- TRUE
guardar_figuras <- TRUE
formato_figura <- "pdf"                      # "png" o "pdf"
ancho_figura <- 8
alto_figura <- 6
dpi_figura <- 300

# ==== Comprobaciones de parámetros ====

if (!is.numeric(numero_simulaciones) || length(numero_simulaciones) != 1 ||
    !is.finite(numero_simulaciones) || numero_simulaciones < 1 ||
    numero_simulaciones != as.integer(numero_simulaciones)) {
  stop("numero_simulaciones debe ser un entero positivo.")
}
if (!is.numeric(nmax_vecinos) || length(nmax_vecinos) != 1 ||
    !is.finite(nmax_vecinos) || nmax_vecinos < 1 ||
    nmax_vecinos != as.integer(nmax_vecinos)) {
  stop("nmax_vecinos debe ser un entero positivo.")
}
if (!is.numeric(sim_visualizar) || length(sim_visualizar) != 1 ||
    !is.finite(sim_visualizar) || sim_visualizar < 1 ||
    sim_visualizar != as.integer(sim_visualizar) ||
    sim_visualizar > numero_simulaciones) {
  stop("sim_visualizar debe ser un entero entre 1 y numero_simulaciones.")
}
if (!formato_figura %in% c("png", "pdf")) {
  stop("Formato no soportado. Usa 'png' o 'pdf'.")
}
if (!is.numeric(media_gaussiana) || length(media_gaussiana) != 1 ||
    !is.finite(media_gaussiana)) {
  stop("media_gaussiana debe ser un número finito.")
}

nombre_dataset <- tools::file_path_sans_ext(basename(ruta_datos))

# ==== Rutas de los objetos del script 045 ====

ruta_datos_gauss <- file.path(
  "modelos", paste0("045_", nombre_dataset, "_", nombre_variable,
                    "_datos_gaussianizados.rds")
)
ruta_anamorfosis <- file.path(
  "modelos", paste0("045_", nombre_dataset, "_", nombre_variable,
                    "_anamorfosis.rds")
)
ruta_modelo_variograma <- file.path(
  "modelos", paste0("045_", nombre_dataset, "_", nombre_variable,
                    "_modelo_variograma_gaussiano.rds")
)
ruta_rejilla <- file.path(
  "modelos", paste0("045_", nombre_dataset, "_", nombre_variable,
                    "_rejilla.rds")
)

rutas_necesarias <- c(
  datos_gaussianizados = ruta_datos_gauss,
  anamorfosis = ruta_anamorfosis,
  modelo_variograma = ruta_modelo_variograma,
  rejilla = ruta_rejilla
)

faltantes <- rutas_necesarias[!file.exists(rutas_necesarias)]
if (length(faltantes) > 0) {
  stop(
    "Faltan objetos del script 045:\n",
    paste(faltantes, collapse = "\n"),
    "\nEjecuta el script 045 con la exportación de datos gaussianizados activada."
  )
}

# ==== Cargar objetos y verificar su coherencia ====

data_sf <- readRDS(ruta_datos_gauss)
tabla_anamorfosis <- readRDS(ruta_anamorfosis)
modelo_gauss <- readRDS(ruta_modelo_variograma)
r_template <- readRDS(ruta_rejilla)

if (!inherits(data_sf, "sf") || !"Z_gauss" %in% names(data_sf)) {
  stop("Los datos gaussianizados del 045 deben ser un objeto sf con columna Z_gauss.")
}
if (!nombre_variable %in% names(data_sf)) {
  stop("No se encuentra la variable original en los datos gaussianizados.")
}
if (nrow(data_sf) < 3 ||
    any(!is.finite(data_sf$Z_gauss)) ||
    any(!is.finite(data_sf[[nombre_variable]]))) {
  stop("Los datos gaussianizados contienen observaciones insuficientes o no finitas.")
}
if (!inherits(r_template, "SpatRaster") || terra::nlyr(r_template) != 1) {
  stop("La plantilla del 045 debe ser un SpatRaster de una capa.")
}
if (terra::ncell(r_template) < 1) {
  stop("La plantilla ráster no contiene celdas.")
}
if (isTRUE(sf::st_is_longlat(data_sf)) || isTRUE(terra::is.lonlat(r_template))) {
  stop("Los datos y la rejilla deben utilizar un CRS proyectado.")
}
if (is.na(sf::st_crs(data_sf)) || is.na(sf::st_crs(terra::crs(r_template)))) {
  stop("No se reconoce el CRS de los datos o de la rejilla.")
}
if (sf::st_crs(data_sf) != sf::st_crs(terra::crs(r_template))) {
  stop("El CRS de los datos gaussianizados no coincide con el de la rejilla.")
}
if (!inherits(modelo_gauss, "variogramModel") ||
    any(!is.finite(modelo_gauss$psill)) ||
    any(!is.finite(modelo_gauss$range))) {
  stop("El modelo variográfico del 045 no es válido.")
}

columnas_anamorfosis <- c("valor_original", "valor_gaussiano")
if (!is.data.frame(tabla_anamorfosis) ||
    !all(columnas_anamorfosis %in% names(tabla_anamorfosis))) {
  stop("La tabla de anamorfosis no contiene las columnas requeridas.")
}
if (nrow(tabla_anamorfosis) < 2 ||
    any(!is.finite(tabla_anamorfosis$valor_original)) ||
    any(!is.finite(tabla_anamorfosis$valor_gaussiano)) ||
    length(unique(tabla_anamorfosis$valor_gaussiano)) < 2) {
  stop("La tabla de anamorfosis no es apta para interpolación inversa.")
}

# Comprobar que los datos condicionantes y la tabla pertenecen
# a la misma transformación, incluyendo observaciones repetidas.
pares_datos <- data.frame(
  valor_original = data_sf[[nombre_variable]],
  valor_gaussiano = data_sf$Z_gauss
)
orden_pares <- function(tabla) {
  tabla[order(tabla$valor_gaussiano, tabla$valor_original),
        c("valor_original", "valor_gaussiano"), drop = FALSE]
}
if (!isTRUE(all.equal(
  unname(as.matrix(orden_pares(pares_datos))),
  unname(as.matrix(orden_pares(tabla_anamorfosis))),
  tolerance = 1e-10, check.attributes = FALSE
))) {
  stop("La tabla de anamorfosis no corresponde a los datos gaussianizados del 045.")
}

cat("\nDatos condicionantes:", nrow(data_sf), "\n")
cat("Simulaciones solicitadas:", numero_simulaciones, "\n")
cat("Vecinos máximos:", nmax_vecinos, "\n")
cat("Celdas de la rejilla:", terra::ncell(r_template), "\n")

# ==== Crear puntos de simulación ====

grid_vect <- terra::as.points(r_template, values = FALSE, na.rm = FALSE)
grid_sf <- sf::st_as_sf(grid_vect)

if (nrow(grid_sf) != terra::ncell(r_template)) {
  stop("Los puntos de simulación no coinciden con las celdas de la plantilla.")
}

# ==== Simulación secuencial gaussiana (SGS) ====

set.seed(semilla)
cat("\nEjecutando simulación secuencial gaussiana (SGS)...\n")

simulation_kriging <- gstat::krige(
  Z_gauss ~ 1,
  locations = data_sf,
  newdata = grid_sf,
  model = modelo_gauss,
  beta = media_gaussiana,
  nsim = numero_simulaciones,
  nmax = nmax_vecinos
)

simulation_kriging_sf <- sf::st_as_sf(simulation_kriging)
coords_sim <- sf::st_coordinates(simulation_kriging_sf)
simulation_kriging_sf$X <- coords_sim[, 1]
simulation_kriging_sf$Y <- coords_sim[, 2]

sim_data <- sf::st_drop_geometry(simulation_kriging_sf)
columnas_sim <- grep("^sim[0-9]+$", names(sim_data), value = TRUE)

if (length(columnas_sim) != numero_simulaciones) {
  stop("El número de simulaciones generadas no coincide con numero_simulaciones.")
}
if (nrow(sim_data) != terra::ncell(r_template)) {
  stop("El número de localizaciones simuladas no coincide con la plantilla.")
}
if (any(!is.finite(as.matrix(sim_data[columnas_sim])))) {
  stop("Se han obtenido valores simulados gaussianos no finitos.")
}

cat("Simulaciones gaussianas completadas:", length(columnas_sim), "\n")

# ==== Diagnóstico de valores fuera del rango empírico ====

limites_gauss <- range(tabla_anamorfosis$valor_gaussiano)
valores_simulados <- unlist(sim_data[columnas_sim], use.names = FALSE)
fuera_rango <- valores_simulados < limites_gauss[1] |
  valores_simulados > limites_gauss[2]
porcentaje_fuera <- 100 * mean(fuera_rango)

cat("\nValores gaussianos fuera del rango de la anamorfosis:",
    sum(fuera_rango), "de", length(valores_simulados), "\n")
cat("Porcentaje fuera de rango:", round(porcentaje_fuera, 2), "%\n")
cat("Política de colas: truncamiento a los extremos observados (rule = 2).\n")

# ==== Retrotransformación empírica ====

# Dentro del intervalo: interpolación lineal.
# Fuera del intervalo: valores mínimo/máximo observados (rule = 2).
# La política de colas es explícita y puede limitar los extremos simulados.
revertir_anamorfosis <- function(z_val, tabla) {
  stats::approx(
    x = tabla$valor_gaussiano,
    y = tabla$valor_original,
    xout = z_val,
    rule = 2,
    ties = "ordered"
  )$y
}

cat("\nRevirtiendo simulaciones a unidades originales...\n")
sim_original_data <- sim_data

for (col_sim in columnas_sim) {
  sim_original_data[[col_sim]] <- revertir_anamorfosis(
    sim_data[[col_sim]], tabla_anamorfosis
  )
}

# Conservar exactamente la geometría y el CRS originales;
# solo se sustituyen los atributos de las realizaciones.
sim_original_sf <- simulation_kriging_sf
for (col_sim in columnas_sim) {
  sim_original_sf[[col_sim]] <- sim_original_data[[col_sim]]
}

# ==== Rásteres de la realización seleccionada ====

sim_col <- paste0("sim", sim_visualizar)
if (!sim_col %in% columnas_sim) {
  stop("No existe la simulación solicitada: ", sim_col)
}

r_sim_gauss <- r_template
terra::values(r_sim_gauss) <- sim_data[[sim_col]]
names(r_sim_gauss) <- sim_col

r_sim_original <- r_template
terra::values(r_sim_original) <- sim_original_data[[sim_col]]
names(r_sim_original) <- sim_col

# ==== Preparar gráficos ====

sim_gauss_df <- as.data.frame(r_sim_gauss, xy = TRUE, na.rm = TRUE)
sim_original_plot_df <- as.data.frame(r_sim_original, xy = TRUE, na.rm = TRUE)

grafico_sim_gauss <- ggplot(sim_gauss_df, aes(
  x = x, y = y, fill = .data[[sim_col]]
)) +
  geom_raster() +
  scale_fill_viridis_c(name = "Z Gauss") +
  coord_sf(crs = st_crs(data_sf), expand = FALSE) +
  labs(
    title = paste("Simulación gaussiana nº", sim_visualizar),
    subtitle = "Variable gaussianizada (escala normal)",
    x = nombre_x, y = nombre_y
  ) +
  theme_gray() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black", size = 10)
  )

grafico_sim_original <- ggplot(sim_original_plot_df, aes(
  x = x, y = y, fill = .data[[sim_col]]
)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", name = nombre_variable) +
  coord_sf(crs = st_crs(data_sf), expand = FALSE) +
  labs(
    title = paste("Simulación revertida nº", sim_visualizar),
    subtitle = paste(nombre_variable, "en unidades originales"),
    x = nombre_x, y = nombre_y
  ) +
  theme_gray() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black", size = 10)
  )

print(grafico_sim_gauss)
print(grafico_sim_original)

# ==== Guardar simulaciones ====

if (guardar_modelos) {
  dir_modelos <- "modelos"
  if (!dir.exists(dir_modelos)) dir.create(dir_modelos, recursive = TRUE)

  ruta_sim_gauss <- file.path(
    dir_modelos,
    paste0("046_", nombre_dataset, "_", nombre_variable,
           "_simulaciones_gaussianas.rds")
  )
  ruta_sim_original <- file.path(
    dir_modelos,
    paste0("046_", nombre_dataset, "_", nombre_variable,
           "_simulaciones_originales.rds")
  )

  saveRDS(simulation_kriging_sf, ruta_sim_gauss)
  saveRDS(sim_original_sf, ruta_sim_original)

  cat("\nSimulaciones gaussianas guardadas:", ruta_sim_gauss, "\n")
  cat("Simulaciones originales guardadas:", ruta_sim_original, "\n")
}

# ==== Guardar figuras ====

if (guardar_figuras) {
  dir_figuras <- "salidas/figuras"
  if (!dir.exists(dir_figuras)) dir.create(dir_figuras, recursive = TRUE)

  nombre_figura_gauss <- file.path(
    dir_figuras,
    paste0("046_", nombre_dataset, "_", nombre_variable,
           "_simulacion_gaussiana_", sim_visualizar, ".", formato_figura)
  )
  nombre_figura_original <- file.path(
    dir_figuras,
    paste0("046_", nombre_dataset, "_", nombre_variable,
           "_simulacion_original_", sim_visualizar, ".", formato_figura)
  )

  ggsave(
    filename = nombre_figura_gauss,
    plot = grafico_sim_gauss,
    width = ancho_figura, height = alto_figura,
    dpi = dpi_figura
  )
  ggsave(
    filename = nombre_figura_original,
    plot = grafico_sim_original,
    width = ancho_figura, height = alto_figura,
    dpi = dpi_figura
  )

  cat("\nFigura gaussiana guardada:", nombre_figura_gauss, "\n")
  cat("Figura original guardada:", nombre_figura_original, "\n")
}
