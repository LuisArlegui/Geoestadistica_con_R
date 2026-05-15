# Geoestadística con R

Colección de scripts en R utilizados en el bloque correspondiente de la asignatura  _Geoestadística, Teledetección y GIS_ del Máster en Geología Aplicada a la Ingeniería y el Medio Ambiente de la Universidad de Zaragoza.

Los scripts acompañan al libro docente:

> **Arlegui, L. E. (2026). _Apuntes de Geoestadística: teoría y aplicaciones en R_.**  
> DOI: https://doi.org/10.5281/zenodo.20201309

Los ejercicios responden a cuestiones y ejemplos planteados en el libro, aunque con un esfuerzo mínimo pueden adaptarse a flujos de trabajo independientes, ya que la mayoría utilizan rutas relativas y nombres de fichero genéricos fácilmente editables.

---

# Estructura del repositorio

```text
000_Configura_proyecto.R

01_bloque_EDA_univariante/
    001_histograma_sencillo.R
    002_histograma_densidad.R
    003_tabla_estadisticos.R
    004_funcion_probab_acumulada.R
    005_boxplot.R
    006_boxplot_histog_densidad.R
    007_qqplot.R
    008_test_shapiro_wilk.R
    009_test_lilliefors.R
    010_transf_datos.R
    011_diagnostico_normalidad.R

02_bloque_EDA_bivariante/
    012_scatterplot.R
    013_scatterplots_multiples.R
    014_scatterplot_regresion_lineal.R
    015_scatterplot_histogramas.R
    016_coefs_correl_pearson_spearman.R
    017_matriz_corr_grafica_pearson.R
    018_matriz_corr_numerica_pearson.R
    019_scatterplot_categorica.R
    020_categorizacion_distm.R
    021_scatterplot_2categoricas.R

03_bloque_EDA_espacial/
    022_postplot.R
    023_i_de_moran.R
    024_ajuste_polinomico_2d.R
    025_resumen_ajustes_polinom.R
    026_vista_3d.R
    027_tabla_residuales_polinom123.R
    028_vista3d_ajust_polinom123.R
    029_geotif_GIS_predic.R

04_Bloque_Analisis_Estructural/
    030_variog_nube.R
    031_variog_boxplot.R
    032_variog_experimental.R
    033_variog_direcc_045.R
    034_variog_4_direcc.R
    035_variog_modelo_omnidirec.R

05_Bloque_interpolacion_kriging/
    036_ordinary_kriging.R
    037_indicator_kriging.R
    038_universal_kriging.R
    039_cross_validation_OK.R
    040_cokriging_01_union_ficheros.R
    041_cokriging_02_variog_cruzado_exper.R
    042_cokriging_03_modelo_coregion_LMC.R
    043_cokriging_04_CK_ordinario.R
    044_cokriging_05_validac_cruzada_LOOCV.R

06_Bloque_simulacion_gaussiana/
    045_Simul_gauss_01_anamorfosis_KS.R
    046_Simul_gauss_02_sim_reversion.R
    047_Simul_gauss_03_promedio.R
    048_Simul_gauss_04_probab_umbral.R
    049_Simul_gauss_05_area_nivel.R

Proyecto_Geoestadistica.Rproj
```

---

# Requisitos

Los scripts han sido desarrollados y probados en:

- R
- RStudio
- macOS / Windows

y utilizan principalmente paquetes como:

- ggplot2
- dplyr
- sf
- terra
- gstat
- GGally
- corrplot

---

# Licencia

Este repositorio se distribuye con fines docentes y académicos.
