# =============================================================================
# 04_simulacion_provincial.R — Monte Carlo de las 22 gobernaciones
# -----------------------------------------------------------------------------
# Para cada distrito y cada simulación:
#   1. Punto de partida: resultado real de Diputados 2025 (tres fuerzas más
#      votadas + "otros").
#   2. Cambio nacional: se traslada a LLA y al peronismo el cambio de la
#      proporción LLA/(LLA+PJ) entre 2025 y las encuestas actuales, con un
#      error común a todas las provincias (si el país se mueve, se mueve todo).
#   3. Efecto oficialismo: el oficialismo provincial suma delta puntos,
#      calibrado con las 7 elecciones provinciales desdobladas de 2025
#      (Corrientes, Santiago del Estero, Buenos Aires, Chaco, Jujuy, Misiones
#      y Santa Fe): delta = % en la elección provincial - % de la misma fuerza
#      en la legislativa nacional de octubre. Puede ser negativo.
#   4. Ruido propio del distrito (6 pp por fuerza, coherente con el error
#      histórico de las encuestas).
#   5. Si hay encuesta provincial, se combina con el modelo (peso según
#      antigüedad).
#   6. Gana la fuerza más votada.
#
# PRE : existen prov, dist, encprov, efecto, caba_mayo, mu_pv, mu_pv_sinpro,
#       sigma_total, DOS_PARTIDOS_2025.
# POST: existen tabla_prov (escenario base), tabla_prov_alianzas y
#       sims_gob (cantidad de gobernaciones por fuerza en cada simulación).
# INV : los resultados 2025 no se modifican.
# =============================================================================

cat("\n[4/5] Simulación provincial (22 distritos)...\n")

# ---- Parámetros calibrados con datos ----------------------------------------------
efecto$delta <- efecto$pct_oficialismo_provincial -
                efecto$pct_misma_fuerza_legislativa_nacional_2025
MU_DELTA <- mean(efecto$delta)            # ~11 pp
SD_DELTA <- sd(efecto$delta)              # dispersión entre provincias
SE_DELTA <- SD_DELTA / sqrt(nrow(efecto)) # incertidumbre sobre la media
SIGMA_DISTRITO <- 6                       # ruido propio por fuerza (pp)
two_party <- function(m) m[["LLA"]] / (m[["LLA"]] + m[["PJ"]])
MU_2P <- mean(c(two_party(mu_pv), two_party(mu_pv_sinpro)))   # promedio escenarios A y B
cat(sprintf("  Efecto oficialismo: %.1f pp (desvío %.1f) | cambio nacional LLA/(LLA+PJ) desde 2025: %+.1f pp\n",
            MU_DELTA, SD_DELTA, 100 * (MU_2P - DOS_PARTIDOS_2025)))

# Alianzas "en construcción" o "en negociación" (escenario B)
ALIANZAS_B <- c("Chubut", "Cordoba", "Neuquen", "San Juan", "Santa Fe")

# -----------------------------------------------------------------------------
# simular_distrito(base, oficialismo, shock_nac, mu_delta_s, poll)
#   base        : data.frame(bloque, familia, pct)
#   oficialismo : nombre del bloque oficialista
#   shock_nac   : vector N_SIM con el cambio nacional de proporción LLA/(LLA+PJ)
#   mu_delta_s  : vector N_SIM con el efecto oficialismo medio de cada simulación
# PRE : la suma de base$pct <= 100; oficialismo está en base$bloque.
# POST: matriz N_SIM x bloques con porcentajes que suman 100 por fila.
# -----------------------------------------------------------------------------
simular_distrito <- function(base, oficialismo, shock_nac, mu_delta_s, poll = NULL) {
  stopifnot(sum(base$pct) <= 100.01, oficialismo %in% base$bloque)
  base <- rbind(base, data.frame(bloque = "Otros", familia = "RESTO",
                                 pct = max(0, 100 - sum(base$pct))))
  n <- length(shock_nac)
  M <- matrix(rep(base$pct, each = n), nrow = n,
              dimnames = list(NULL, base$bloque))
  # 2. Cambio nacional sobre LLA y peronismo
  il <- which(base$familia == "LLA"); ip <- which(base$familia == "PJ")
  if (length(il) == 1 && length(ip) == 1) {
    T2 <- M[, il] + M[, ip]
    pl <- pmin(pmax(M[, il] / T2 + shock_nac, 0.02), 0.98)
    M[, il] <- pl * T2; M[, ip] <- (1 - pl) * T2
  }
  # 3. Efecto oficialismo (se resta proporcionalmente al resto)
  io <- which(base$bloque == oficialismo)
  delta <- mu_delta_s + sqrt(max(SD_DELTA^2 - SE_DELTA^2, 1)) * rnorm(n)
  resto <- rowSums(M[, -io, drop = FALSE])
  quita <- pmax(pmin(delta, resto - 1), -(M[, io] - 1))   # puede ser negativo
  M[, -io] <- M[, -io, drop = FALSE] * (1 - quita / resto)
  M[, io]  <- M[, io] + quita
  # 4. Ruido propio del distrito
  M <- M + matrix(rnorm(length(M), 0, SIGMA_DISTRITO), nrow = n)
  M <- pmax(M, 0.5)
  M <- 100 * M / rowSums(M)
  # 5. Encuesta provincial (si existe)
  if (!is.null(poll)) {
    w <- 0.5 * 0.5^(as.numeric(FECHA_REFERENCIA - poll$fecha_fin) / 90)
    if (poll$retador_familia == "RESTO") {
      obj <- pmin(pmax(poll$pct_oficialismo + 100 * sigma_encuesta * rnorm(n), 1), 99)
      nuevo <- (1 - w) * M[, io] + w * obj
      M[, -io] <- M[, -io, drop = FALSE] * ((100 - nuevo) / (100 - M[, io]))
      M[, io] <- nuevo
    } else {
      ic <- which(base$familia == poll$retador_familia)[1]
      T2 <- M[, io] + M[, ic]
      r_poll <- poll$pct_oficialismo / (poll$pct_oficialismo + poll$pct_retador) +
                sigma_encuesta * rnorm(n)
      r <- (1 - w) * (M[, io] / T2) + w * pmin(pmax(r_poll, 0.02), 0.98)
      M[, io] <- r * T2; M[, ic] <- (1 - r) * T2
    }
  }
  stopifnot(all(abs(rowSums(M) - 100) < 1e-6))      # postcondición
  M
}

# -----------------------------------------------------------------------------
# correr_escenario(alianzas, caba_separados)
# POST: lista con la matriz de cada distrito y la tabla resumen.
# -----------------------------------------------------------------------------
correr_escenario <- function(alianzas = character(0), caba_separados = FALSE) {
  shock_nac  <- (MU_2P - DOS_PARTIDOS_2025) + sigma_total * rnorm(N_SIM)
  mu_delta_s <- MU_DELTA + SE_DELTA * rnorm(N_SIM)
  salida <- list(); filas <- list(); ganadores_fam <- list(); probs <- list()
  for (d in dist$distrito) {
    info <- dist[dist$distrito == d, ]
    base <- prov[prov$distrito == d, c("bloque", "familia")]
    base$pct <- prov$pct_2025[prov$distrito == d]
    ofi <- info$oficialismo_bloque
    if (d %in% alianzas) {                # el oficialismo provincial se alía con LLA
      il <- which(base$familia == "LLA"); io <- which(base$bloque == ofi)
      base$pct[io] <- base$pct[io] + base$pct[il]
      base$bloque[io] <- paste0(base$bloque[io], " + LLA")
      base <- base[-il, ]; ofi <- base$bloque[io - (il < io)]
    }
    if (d == "Ciudad de Buenos Aires" && caba_separados) {
      base <- caba_mayo; names(base)[names(base) == "pct"] <- "pct"
      ofi <- "PRO (Lospennato)"
    }
    poll <- if (d %in% encprov$distrito && !(d %in% alianzas)) encprov[encprov$distrito == d, ][1, ] else NULL
    M <- simular_distrito(base, ofi, shock_nac, mu_delta_s, poll)
    gan <- colnames(M)[max.col(M, ties.method = "first")]
    p_gan <- table(factor(gan, levels = colnames(M))) / N_SIM
    fav <- names(which.max(p_gan))
    fam_of <- c(setNames(base$familia, base$bloque), Otros = "RESTO")
    # Familia del ganador, tratando al oficialismo aliado de LLA como "LLA y aliados"
    fam_gan <- fam_of[gan]
    fam_gan[gan == ofi & info$oficialismo_familia == "PROV" & !(d %in% alianzas)] <- "PROV"
    fam_gan[grepl("\\+ LLA$", gan)] <- "LLA"
    ganadores_fam[[d]] <- fam_gan
    probs[[d]] <- data.frame(bloque = names(p_gan), familia = fam_of[names(p_gan)],
                             p = as.numeric(p_gan), es_ofi = names(p_gan) == ofi,
                             row.names = NULL)
    salida[[d]] <- M
    filas[[d]] <- data.frame(
      distrito = d, oficialismo = ofi,
      p_oficialismo = as.numeric(p_gan[ofi]),
      favorito = fav, p_favorito = as.numeric(max(p_gan)),
      med_oficialismo = median(M[, ofi]),
      segundo = names(sort(apply(M, 2, median), decreasing = TRUE))[
        names(sort(apply(M, 2, median), decreasing = TRUE)) != ofi][1],
      med_segundo = sort(apply(M, 2, median), decreasing = TRUE)[
        names(sort(apply(M, 2, median), decreasing = TRUE)) != ofi][1],
      row.names = NULL)
  }
  tabla <- do.call(rbind, filas)
  fam_mat <- do.call(cbind, ganadores_fam)
  conteo <- t(apply(fam_mat, 1, function(r) table(factor(r, levels = c("LLA", "PJ", "PROV", "PROV2", "RESTO", "OTRO", "IZQ")))))
  list(M = salida, tabla = tabla, conteo = conteo, fam_mat = fam_mat, probs = probs)
}

esc_base    <- correr_escenario()
esc_alianza <- correr_escenario(alianzas = ALIANZAS_B)
esc_caba    <- correr_escenario(caba_separados = TRUE)
tabla_prov  <- esc_base$tabla
tabla_prov$favorito_alianzas <- esc_alianza$tabla$favorito
tabla_prov$p_fav_alianzas    <- esc_alianza$tabla$p_favorito
print(tabla_prov[, c("distrito", "favorito", "p_favorito", "p_oficialismo",
                     "favorito_alianzas", "p_fav_alianzas")], digits = 2)
cat("  Gobernaciones (mediana) por fuerza, escenario base:\n")
print(apply(esc_base$conteo, 2, median))
write.csv(tabla_prov, "results/gobernaciones.csv", row.names = FALSE)

escribir_macro(ARCHIVO_MACROS, "muDelta", fmt(MU_DELTA))
escribir_macro(ARCHIVO_MACROS, "sdDelta", fmt(SD_DELTA))
escribir_macro(ARCHIVO_MACROS, "cambioNac", fmt(100 * (MU_2P - DOS_PARTIDOS_2025)))
