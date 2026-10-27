# =========================================================
# 00_configuracion_proyecto.R
# ---------------------------------------------------------
# Script de inicialización de un proyecto en RStudio
# Crea una estructura estándar de carpetas para prácticas
# de geoestadística / SIG / análisis de datos espaciales
#
# INSTRUCCIONES:
# 1. Descargar y descomprimir la carpeta del proyecto.
# 2. Guardarla en una ubicación estable del ordenador.
# 3. Abrir el proyecto en RStudio haciendo doble clic
#    en Proyecto_Geostadistica.Rproj.
# 4. Ejecutar este script para completar la estructura
#    de directorios.
# =========================================================

#----------------------------------------------------------
# 1. Directorio raíz
#----------------------------------------------------------

raiz <- getwd()
# Comprobar que se está trabajando desde la raíz del proyecto

archivo_proyecto <- "Proyecto_Geostadistica.Rproj"

if (!file.exists(file.path(raiz, archivo_proyecto))) {
  stop(
    "No se encuentra el archivo del proyecto RStudio.\n",
    "Abre primero Proyecto_Geostadistica.Rproj ",
    "y vuelve a ejecutar este script."
  )
}

cat("Proyecto:", raiz, "\n")

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
  
  # Modelos
  "modelos",
  
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

