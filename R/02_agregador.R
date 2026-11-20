# =============================================================================
# 02_agregador.R — Promedio de encuestas y calibración de la incertidumbre
# -----------------------------------------------------------------------------
# PRE : existen bal, pv, err (01_datos.R).
# POST: existen
#   mu_bal        -> proporción de Milei sobre (Milei + Kicillof) en balotaje
#   mu_pv, mu_pv_sinpro -> votos afirmativos esperados en 1a vuelta (escenarios)
#   sigma_encuesta-> error histórico de las encuestas (en puntos de proporción)
#   SIGMA_DERIVA  -> cambio posible de la opinión hasta octubre de 2027
# INV : las encuestas originales no se modifican.
# =============================================================================

cat("\n[2/5] Promedio de encuestas...\n")

# ---- 1. Balotaje Milei vs. Kicillof ------------------------------------------
# Se trabaja con la proporción de dos candidatos: p = Milei / (Milei + Kicillof),
# que equivale al porcentaje de votos válidos en un balotaje.
bal$p_milei <- bal$milei / (bal$milei + bal$kicillof)
bal$es_promedio <- grepl("^Promedio", bal$encuestadora)
bal$peso <- peso_encuesta(bal$n, bal$fecha_fin)
# El promedio de Clarín resume 11 encuestadoras; 3 de ellas (CELAG, Proyección,
# RDT) ya están en la base individual, así que cuenta como 8 encuestas.
bal$peso[bal$es_promedio] <- 8 * bal$peso[bal$es_promedio]
mu_bal <- media_ponderada(bal$p_milei, bal$peso)
# Dispersión entre encuestadoras (desvío ponderado): mide el "efecto casa"
disp_bal <- sqrt(sum(bal$peso * (bal$p_milei - mu_bal)^2) / sum(bal$peso))

# Promedio "como hubiera sido" a fines de mayo (sólo encuestas hasta esa fecha)
promedio_a_fecha <- function(fecha_corte) {
  sub <- bal[bal$fecha_fin <= fecha_corte, ]
  w <- sqrt(ifelse(is.na(sub$n), 1000, pmin(sub$n, 3000))) *
       0.5^(as.numeric(fecha_corte - sub$fecha_fin) / 45)
  w[sub$es_promedio] <- 8 * w[sub$es_promedio]
  media_ponderada(sub$p_milei, w)
}
mu_bal_mayo <- promedio_a_fecha(as.Date("2026-05-31"))
cat(sprintf("  Balotaje: Milei %.1f%% de los válidos (dispersión entre encuestas %.1f pp). A fines de mayo: %.1f%%\n",
            100 * mu_bal, 100 * disp_bal, 100 * mu_bal_mayo))

# ---- 2. Primera vuelta --------------------------------------------------------
# Escenario base: PRO va aliado con LLA (como en las legislativas 2025).
# Escenario alternativo: PRO compite por separado (su voto pasa a "Otros").
cats <- c("LLA", "PJ", "FIT", "PU", "OTROS")
pv_base <- data.frame(LLA = pv$lla + pv$pro, PJ = pv$pj, FIT = pv$fit,
                      PU = pv$pu, OTROS = pv$otros)
pv_sinpro <- data.frame(LLA = pv$lla, PJ = pv$pj, FIT = pv$fit,
                        PU = pv$pu, OTROS = pv$otros + pv$pro)
afirm_base   <- a_votos_afirmativos(pv_base, cats)
afirm_sinpro <- a_votos_afirmativos(pv_sinpro, cats)
pv$peso <- peso_encuesta(pv$n, pv$fecha_fin)
# El promedio de Clarín de 11 encuestadoras no se superpone con Tendencias ni
# con Haime, por eso pesa como 11 encuestas.
pv$peso[grepl("^Promedio", pv$encuestadora)] <-
  11 * pv$peso[grepl("^Promedio", pv$encuestadora)]
mu_pv        <- apply(afirm_base,   2, media_ponderada, w = pv$peso)
mu_pv_sinpro <- apply(afirm_sinpro, 2, media_ponderada, w = pv$peso)
stopifnot(abs(sum(mu_pv) - 100) < 1e-8, abs(sum(mu_pv_sinpro) - 100) < 1e-8)
cat("  1a vuelta (afirmativos, escenario base):",
    paste(names(mu_pv), sprintf("%.1f", mu_pv), collapse = " | "), "\n")

# ---- 3. Error histórico de las encuestas en Argentina -------------------------
# error = |margen real - margen que daban las encuestas| entre los dos primeros
err$error <- abs(err$margen_real - err$margen_encuestas)
rmse_margen <- sqrt(mean(err$error^2))
# Un error de E puntos en el margen equivale a E/2 en la proporción de cada uno
sigma_encuesta <- (rmse_margen / 2) / 100
cat(sprintf("  Error histórico (RMSE del margen): %.1f pp -> desvío %.1f pp por candidato\n",
            rmse_margen, 100 * sigma_encuesta))

# ---- 4. Deriva hasta la elección ------------------------------------------------
# Faltan 11 meses. La opinión cambia: el promedio de balotaje pasó de
# mu_bal_mayo (fin de mayo) a mu_bal (noviembre). Se adopta un desvío de 5 pp
# para la deriva hasta octubre de 2027 y se muestra la sensibilidad con 0, 3 y 7.
SIGMA_DERIVA <- 0.05
sigma_total <- sqrt(sigma_encuesta^2 + SIGMA_DERIVA^2)
cat(sprintf("  Desvío total a octubre 2027: %.1f pp\n", 100 * sigma_total))

escribir_macro(ARCHIVO_MACROS, "muBal", fmt(100 * mu_bal))
escribir_macro(ARCHIVO_MACROS, "muBalK", fmt(100 * (1 - mu_bal)))
escribir_macro(ARCHIVO_MACROS, "muBalMayo", fmt(100 * mu_bal_mayo))
escribir_macro(ARCHIVO_MACROS, "dispBal", fmt(100 * disp_bal))
escribir_macro(ARCHIVO_MACROS, "rmseMargen", fmt(rmse_margen))
escribir_macro(ARCHIVO_MACROS, "sigmaEnc", fmt(100 * sigma_encuesta))
escribir_macro(ARCHIVO_MACROS, "sigmaDeriva", fmt(100 * SIGMA_DERIVA))
escribir_macro(ARCHIVO_MACROS, "sigmaTotal", fmt(100 * sigma_total))
for (k in cats) {
  escribir_macro(ARCHIVO_MACROS, paste0("pv", k), fmt(mu_pv[[k]]))
  escribir_macro(ARCHIVO_MACROS, paste0("pvSinpro", k), fmt(mu_pv_sinpro[[k]]))
}
