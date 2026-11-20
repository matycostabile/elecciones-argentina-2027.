# =============================================================================
# 03_simulacion_presidencial.R — Monte Carlo de la elección presidencial
# -----------------------------------------------------------------------------
# Modelo (por cada una de las N_SIM elecciones simuladas):
#   1. Shock común z ~ N(0,1): representa a la vez el error sistemático de las
#      encuestas y el cambio de opinión hasta octubre. Es común a la 1a vuelta
#      y al balotaje (correlación RHO), porque si las encuestas subestiman a
#      un candidato en una vuelta, suelen subestimarlo en la otra.
#   2. Primera vuelta: LLA = mu + sigma*u, PJ = mu - sigma*u (+ ruido propio),
#      terceros con ruido de 1,5 pp; se renormaliza a 100 y se aplican las
#      reglas constitucionales (45%, o 40% con 10 de diferencia).
#   3. Si hay balotaje LLA vs PJ: p_Milei = mu_bal + sigma*b, con b correlacionado
#      con u.
# Se corre dos veces: "si la elección fuera hoy" (sólo error de encuestas) y
# "octubre 2027" (error de encuestas + deriva).
#
# PRE : existen mu_pv, mu_pv_sinpro, mu_bal, sigma_encuesta, SIGMA_DERIVA.
# POST: existe la lista `sim_pres` con los resultados de cada escenario.
# INV : la semilla se fija en main.R; los resultados son reproducibles.
# =============================================================================

cat("\n[3/5] Simulación presidencial...\n")
RHO <- 0.8          # correlación entre el error de 1a vuelta y el de balotaje
SIGMA_TERCEROS <- 1.5

simular_presidencial <- function(mu_1v, mu_b, sigma, n = N_SIM) {
  stopifnot(abs(sum(mu_1v) - 100) < 1e-6, mu_b > 0, mu_b < 1, sigma >= 0)
  z  <- rnorm(n)                                    # shock común
  u  <- RHO * z + sqrt(1 - RHO^2) * rnorm(n)        # 1a vuelta
  b  <- RHO * z + sqrt(1 - RHO^2) * rnorm(n)        # balotaje
  s  <- 100 * sigma                                 # en puntos porcentuales
  V <- matrix(rep(mu_1v, each = n), nrow = n, dimnames = list(NULL, names(mu_1v)))
  V[, "LLA"] <- V[, "LLA"] + s * u
  V[, "PJ"]  <- V[, "PJ"]  - s * u + 0.5 * s * rnorm(n)
  for (k in c("FIT", "PU", "OTROS")) V[, k] <- V[, k] + SIGMA_TERCEROS * rnorm(n)
  V <- pmax(V, 0.1)
  V <- 100 * V / rowSums(V)
  ganador_1v <- apply(V, 1, gana_primera_vuelta)
  p_bal <- pmin(pmax(mu_b + sigma * b, 0.01), 0.99)
  dos_primeros <- t(apply(V, 1, function(v) names(sort(v, decreasing = TRUE))[1:2]))
  es_lla_pj <- apply(dos_primeros, 1, function(x) setequal(x, c("LLA", "PJ")))
  ganador_bal <- ifelse(is.na(ganador_1v),
                        ifelse(es_lla_pj, ifelse(p_bal > 0.5, "LLA", "PJ"),
                               dos_primeros[, 1]),       # caso raro: otro par
                        NA)
  ganador <- ifelse(is.na(ganador_1v), ganador_bal, ganador_1v)
  res <- list(V = V, ganador_1v = ganador_1v, p_bal = p_bal,
              hay_balotaje = is.na(ganador_1v), ganador = ganador)
  stopifnot(length(res$ganador) == n, !anyNA(res$ganador))   # postcondición
  res
}

# Escenario C: sólo encuestas que preguntan por CANDIDATOS (Tendencias, Haime),
# sin el promedio por espacios políticos.
es_cand <- !grepl("^Promedio", pv$encuestadora)
mu_pv_cand <- apply(afirm_base[es_cand, , drop = FALSE], 2, media_ponderada,
                    w = pv$peso[es_cand])

sim_pres <- list(
  hoy       = simular_presidencial(mu_pv, mu_bal, sigma_encuesta),
  oct2027   = simular_presidencial(mu_pv, mu_bal, sigma_total),       # A: PRO aliado
  sinpro    = simular_presidencial(mu_pv_sinpro, mu_bal, sigma_total),# B: PRO separado
  candidato = simular_presidencial(mu_pv_cand, mu_bal, sigma_total)   # C
)

resumen_pres <- function(s) c(
  milei_1v    = mean(s$ganador_1v %in% "LLA"),
  kicillof_1v = mean(s$ganador_1v %in% "PJ"),
  balotaje    = mean(s$hay_balotaje),
  milei_total = mean(s$ganador == "LLA"),
  kici_total  = mean(s$ganador == "PJ"),
  milei_bal_si_hay = mean(s$p_bal[s$hay_balotaje] > 0.5))
tabla_pres <- sapply(sim_pres, resumen_pres)
# RESULTADO FINAL: la alianza LLA-PRO está en negociación, así que se le asigna
# 50% de probabilidad y se promedian los escenarios A y B.
tabla_pres <- cbind(tabla_pres, final = (tabla_pres[, "oct2027"] + tabla_pres[, "sinpro"]) / 2)
print(round(100 * tabla_pres, 1))

# Sensibilidad a la deriva supuesta
sens_deriva <- sapply(c(0, 0.03, 0.05, 0.07), function(sd_d) {
  s <- simular_presidencial(mu_pv, mu_bal, sqrt(sigma_encuesta^2 + sd_d^2))
  c(deriva = 100 * sd_d, milei = mean(s$ganador == "LLA"),
    balotaje = mean(s$hay_balotaje))
})
print(round(t(sens_deriva), 3))
write.csv(round(100 * tabla_pres, 2), "results/resumen_presidencial.csv")
