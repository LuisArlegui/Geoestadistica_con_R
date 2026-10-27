#####################################################
#
# 018. Análisis inferencial de correlaciones (Pearson)
#
# Descripción:
# Calcula la matriz de correlaciones de Pearson y realiza
# el análisis inferencial para cada par de variables:
#   - coeficiente de correlación (r)
#   - p-valor
#   - intervalo de confianza (95 %)
#   - p-valor ajustado
#
# Salidas:
#   salidas/tablas/
#      018_<dataset>_correlacion_Pearson.csv
#      018_<dataset>_pvalores_Pearson.csv
#      018_<dataset>_IC95_Pearson.csv
#      018_<dataset>_resumen_correlaciones.csv
#
#####################################################

# Archivo de datos ====

ruta_datos <- "datos/brutos/fichero.csv"

if (!file.exists(ruta_datos)) {
  stop(paste("No se encuentra el fichero:", ruta_datos))
}

data <- read.csv(ruta_datos, sep = ",")

cat("Columnas disponibles en el dataset:\n")
print(colnames(data))


# ===== PARÁMETROS =====

columnas_excluir <- c(
  "variable1",
  "variable2",
  "variable3",
  "variable4",
  "variable5"
)

alpha <- 0.05
metodo_ajuste <- "holm"     # none, holm, bonferroni, BH
guardar_tablas <- TRUE

# ===== Preparación =====

data <- data[, !(names(data) %in% columnas_excluir), drop = FALSE]
data <- data[, sapply(data, is.numeric), drop = FALSE]

if(ncol(data)<2)
  stop("Se necesitan al menos dos variables numéricas.")

vars <- names(data)
n <- length(vars)

R <- cor(data, use="complete.obs")

P <- matrix(NA,n,n,dimnames=list(vars,vars))
IC <- matrix("",n,n,dimnames=list(vars,vars))

tabla <- data.frame()

for(i in 1:(n-1)){
  for(j in (i+1):n){
    
    prueba <- cor.test(data[[i]], data[[j]],
                       method="pearson")
    
    P[i,j] <- P[j,i] <- prueba$p.value
    
    ICtxt <- sprintf("[%.3f ; %.3f]",
                     prueba$conf.int[1],
                     prueba$conf.int[2])
    
    IC[i,j] <- IC[j,i] <- ICtxt
    
    tabla <- rbind(
      tabla,
      data.frame(
        Variable1 = vars[i],
        Variable2 = vars[j],
        r = unname(prueba$estimate),
        p = prueba$p.value,
        IC_inf = prueba$conf.int[1],
        IC_sup = prueba$conf.int[2],
        N = prueba$parameter + 2
      )
    )
  }
}

tabla$p_ajustado <- p.adjust(tabla$p,
                             method = metodo_ajuste)

tabla$Significativa <- ifelse(
  tabla$p_ajustado < alpha,
  "Sí",
  "No"
)

tabla <- tabla[order(-abs(tabla$r)),]

print(tabla)

if(guardar_tablas){
  
  dir.create("salidas/tablas",
             recursive=TRUE,
             showWarnings=FALSE)
  
  nombre <- tools::file_path_sans_ext(
    basename(ruta_datos)
  )
  
  write.csv(
    R,
    file=file.path(
      "salidas/tablas",
      paste0("018_",nombre,
             "_correlacion_Pearson.csv"))
  )
  
  write.csv(
    P,
    file=file.path(
      "salidas/tablas",
      paste0("018_",nombre,
             "_pvalores_Pearson.csv"))
  )
  
  write.csv(
    IC,
    file=file.path(
      "salidas/tablas",
      paste0("018_",nombre,
             "_IC95_Pearson.csv"))
  )
  
  write.csv(
    tabla,
    file=file.path(
      "salidas/tablas",
      paste0("018_",nombre,
             "_resumen_correlaciones.csv")),
    row.names=FALSE
  )
  
  cat("\nTablas guardadas correctamente.\n")
}

cat("Variables:",n,"\n")
cat("Comparaciones:",nrow(tabla),"\n")
cat("Significativas:",
    sum(tabla$Significativa=="Sí"),
    "\n")

