# =============================================================================
#  Simulación de las elecciones presidenciales y de gobernadores de Argentina
#  2027 a partir de datos reales (encuestas, resultados 2023-2025 y error
#  histórico de las encuestas).
# -----------------------------------------------------------------------------
#  Autor   : Matías Costabile — Licenciatura en Ciencia de Datos, UCA
#  Archivo : main.R (PUNTO DE ENTRADA)
#
#  Cómo correrlo: abrir Elecciones2027.Rproj en RStudio y ejecutar main.R
#  (botón "Source"). Tarda menos de un minuto.
#
#  Pasos:
#    R/01_datos.R                  -> carga y valida los datasets (data/)
#    R/02_agregador.R              -> promedio de encuestas y error histórico
#    R/03_simulacion_presidencial.R-> Monte Carlo de 1a vuelta y balotaje
#    R/04_simulacion_provincial.R  -> Monte Carlo de las 22 gobernaciones
#    R/05_resultados.R             -> gráficos, tablas y cifras del informe
#
#  IMPORTANTE: es una SIMULACIÓN probabilística, no un pronóstico cierto.
#  Cada dato de entrada tiene su fuente en la columna `fuente` del CSV.
# =============================================================================

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}

if (!file.exists("data/encuestas_balotaje.csv")) {
  stop("Abrí el proyecto con Elecciones2027.Rproj o hacé setwd() a la raíz.")
}
SEMILLA <- 2027
N_SIM   <- 20000      # cantidad de elecciones simuladas
set.seed(SEMILLA)
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
ARCHIVO_MACROS <- "results/numeros.tex"
if (file.exists(ARCHIVO_MACROS)) invisible(file.remove(ARCHIVO_MACROS))

source("R/utils.R",                     encoding = "UTF-8")
source("R/01_datos.R",                  encoding = "UTF-8")
source("R/02_agregador.R",              encoding = "UTF-8")
source("R/03_simulacion_presidencial.R",encoding = "UTF-8")
source("R/04_simulacion_provincial.R",  encoding = "UTF-8")
source("R/05_resultados.R",             encoding = "UTF-8")
cat("\nListo. Gráficos en figures/ y tablas en results/\n")
