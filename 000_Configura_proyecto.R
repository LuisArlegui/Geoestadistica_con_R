# =========================================================
# 00_configuracion_proyecto.R
# ---------------------------------------------------------
# Script de inicialización de un proyecto en RStudio
# Crea una estructura estándar de carpetas para prácticas
# de geoestadística / SIG / análisis de datos espaciales
#
# INSTRUCCIONES:
# 1. Crear una carpeta vacía para el proyecto
# 2. Abrir esa carpeta como proyecto en RStudio
# 3. Ejecutar este script
# =========================================================

cat("============================================\n")
cat(" CONFIGURACION DEL PROYECTO EN RSTUDIO\n")
cat("============================================\n\n")

#----------------------------------------------------------
# 1. Directorio raíz
#----------------------------------------------------------

raiz <- getwd()

cat("Directorio de trabajo detectado:\n")
cat(raiz, "\n\n")

# Confirmación básica (opcional pero recomendable en docencia)
respuesta <- readline(prompt = "¿Deseas crear la estructura en este directorio? (s/n): ")

if (tolower(respuesta) != "s") {
  stop("Proceso cancelado por el usuario.")
}

#----------------------------------------------------------
# 2. Definición de estructura de carpetas
#----------------------------------------------------------

carpetas <- c(
  # Datos
  "datos",
  "datos/brutos",
  "datos/procesados",
  "datos/externos",
  
  # Scripts
  "scripts",
  
  # Salidas
  "salidas",
  "salidas/tablas",
  "salidas/figuras",
  "salidas/rasters",
  "salidas/vectores",
  "salidas/variogramas",
  "salidas/kriging",
  "salidas/simulaciones",
  
  # Documentación
  "docs",
  
  # Temporal
  "temp"
)

#----------------------------------------------------------
# 3. Crear carpetas
#----------------------------------------------------------

cat("\nCreando estructura de carpetas...\n\n")

for (carpeta in carpetas) {
  
  dir_path <- file.path(raiz, carpeta)
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("✔ Creada:", carpeta, "\n")
  } else {
    cat("• Ya existe:", carpeta, "\n")
  }
}

#----------------------------------------------------------
# 4. Crear README (si no existe)
#----------------------------------------------------------

readme_path <- file.path(raiz, "README.txt")

if (!file.exists(readme_path)) {
  
  contenido_readme <- c(
    "========================================",
    " PROYECTO R - GEOESTADISTICA / SIG",
    "========================================",
    "",
    "Estructura del proyecto:",
    "",
    "datos/brutos      -> datos originales (NO modificar)",
    "datos/procesados  -> datos transformados",
    "datos/externos    -> datos auxiliares",
    "",
    "scripts           -> scripts en R",
    "",
    "salidas/tablas    -> tablas de resultados",
    "salidas/figuras   -> graficos",
    "salidas/rasters   -> raster (GeoTIFF, etc.)",
    "salidas/vectores  -> shapefile, gpkg, etc.",
    "salidas/variogramas -> resultados de variogramas",
    "salidas/kriging   -> resultados de kriging",
    "salidas/simulaciones -> simulaciones",
    "",
    "docs              -> informes (Rmd, Quarto, PDF)",
    "temp              -> archivos temporales",
    "",
    "REGLAS:",
    "- Usar rutas relativas (NO setwd())",
    "- No modificar datos/brutos",
    "- Guardar resultados en 'salidas'",
    "",
    "========================================"
  )
  
  writeLines(contenido_readme, readme_path)
  cat("\n✔ README creado\n")
  
} else {
  cat("\n• README ya existe\n")
}

#----------------------------------------------------------
# 5. Crear script plantilla (opcional)
#----------------------------------------------------------

script_base <- file.path(raiz, "scripts", "01_plantilla_script.R")

if (!file.exists(script_base)) {
  
  contenido_script <- c(
    "# =========================================================",
    "# 01_plantilla_script.R",
    "# Plantilla básica para análisis en este proyecto",
    "# =========================================================",
    "",
    "# NO usar setwd()",
    "",
    "# Ejemplo: lectura de datos",
    "# datos <- read.csv('datos/brutos/archivo.csv')",
    "",
    "# Ejemplo: guardar resultados",
    "# write.csv(resultado, 'salidas/tablas/resultado.csv', row.names = FALSE)",
    "",
    "# Ejemplo: guardar figura",
    "# ggsave('salidas/figuras/figura.png')",
    "",
    "# Ejemplo raster (terra)",
    "# terra::writeRaster(r, 'salidas/rasters/mapa.tif', overwrite = TRUE)",
    "",
    "# ========================================================="
  )
  
  writeLines(contenido_script, script_base)
  cat("✔ Script plantilla creado\n")
  
} else {
  cat("• Script plantilla ya existe\n")
}

#----------------------------------------------------------
# 6. Mensaje final
#----------------------------------------------------------

cat("\n============================================\n")
cat(" ESTRUCTURA CREADA CORRECTAMENTE\n")
cat("============================================\n\n")

cat("Siguientes pasos recomendados:\n")
cat("1. Copiar datos a 'datos/brutos'\n")
cat("2. Trabajar con scripts en 'scripts'\n")
cat("3. Guardar resultados en 'salidas'\n")
cat("4. NO usar rutas absolutas\n\n")

cat("✔ Proyecto listo para trabajar\n\n")