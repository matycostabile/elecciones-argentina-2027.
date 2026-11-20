# =============================================================================
# 01_datos.R — Carga y validación de los datasets
# -----------------------------------------------------------------------------
# PRE : existen los CSV en data/.
# POST: en memoria: bal (encuestas de balotaje), pv (primera vuelta),
#       err (errores históricos), prov (resultados 2025 por distrito),
#       dist (situación de cada gobernación 2027), encprov (encuestas
#       provinciales), efecto (efecto oficialismo), caba_mayo.
# INV : los datos no se modifican salvo conversión de tipos.
# =============================================================================

cat("\n[1/5] Carga de datos...\n")
leer <- function(f) read.csv(file.path("data", f), stringsAsFactors = FALSE,
                             encoding = "UTF-8")
bal     <- leer("encuestas_balotaje.csv")
pv      <- leer("encuestas_primera_vuelta.csv")
err     <- leer("errores_historicos.csv")
prov    <- leer("provincias_2025.csv")
dist    <- leer("distritos_2027.csv")
encprov <- leer("encuestas_provinciales.csv")
efecto  <- leer("efecto_oficialismo.csv")
caba_mayo <- leer("caba_mayo_2025.csv")

bal$fecha_fin <- as.Date(bal$fecha_fin)
pv$fecha_fin  <- as.Date(pv$fecha_fin)
encprov$fecha_fin <- as.Date(encprov$fecha_fin)

# ---- Validaciones (precondiciones sobre los datos) ---------------------------
stopifnot(
  nrow(dist) == 22,                                  # 21 provincias + CABA
  setequal(unique(prov$distrito), dist$distrito),
  all(bal$milei + bal$kicillof + bal$blanco_indeciso <= 100.5),
  all(bal$milei + bal$kicillof + bal$blanco_indeciso >= 99.5),
  all(abs(rowSums(pv[c("lla","pro","pj","fit","pu","otros","blanco_ns")]) - 100) < 0.6),
  all(tapply(prov$pct_2025, prov$distrito, sum) <= 100),
  all(nchar(bal$fuente) > 10), all(nchar(dist$fuente) > 10),
  all(dist$oficialismo_familia %in% c("LLA", "PJ", "PROV")),
  # cada fila tiene un enlace de fuente bien leído (detecta CSV mal parseados)
  all(grepl("^https?://", c(bal$fuente, pv$fuente, err$fuente, dist$fuente,
                             encprov$fuente, efecto$fuente_provincial))),
  is.numeric(encprov$pct_oficialismo), is.numeric(encprov$pct_retador)
)
# Cada distrito debe tener su bloque oficialista entre los resultados 2025
for (d in dist$distrito) {
  b <- dist$oficialismo_bloque[dist$distrito == d]
  stopifnot(b %in% prov$bloque[prov$distrito == d])
}

# Resultado nacional 2025 (Diputados) para medir el cambio desde entonces
LLA_2025 <- 40.66; FP_2025 <- 33.79
DOS_PARTIDOS_2025 <- LLA_2025 / (LLA_2025 + FP_2025)

cat(sprintf("  Encuestas de balotaje: %d | primera vuelta: %d | distritos: %d\n",
            nrow(bal), nrow(pv), nrow(dist)))
escribir_macro(ARCHIVO_MACROS, "nEncBal", nrow(bal) - 1)
escribir_macro(ARCHIVO_MACROS, "nSim", formatC(N_SIM, big.mark = ".", decimal.mark = ",", format = "d"))
