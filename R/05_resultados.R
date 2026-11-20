# =============================================================================
# 05_resultados.R — Gráficos, tablas y cifras que usa el informe
# -----------------------------------------------------------------------------
# PRE : existen bal, pv, err, efecto, dist, prov, encprov, sim_pres, tabla_pres,
#       sens_deriva, esc_base, esc_alianza, esc_caba (scripts 01 a 04).
# POST: figuras en figures/ (PDF y PNG), filas de tablas LaTeX en results/ y
#       todas las cifras citadas en el texto en results/numeros.tex.
# INV : no se modifica ningún resultado de la simulación; sólo se resume.
# =============================================================================

cat("\n[5/5] Gráficos, tablas y cifras del informe...\n")

escribir_filas <- function(filas, archivo) {
  con <- file(file.path("results", archivo), open = "w", encoding = "UTF-8")
  writeLines(filas, con); close(con)
  stopifnot(file.exists(file.path("results", archivo)))
}
mac <- function(nombre, valor) escribir_macro(ARCHIVO_MACROS, nombre, valor)
pct <- function(x, d = 1) fmt(100 * x, d)          # proporción -> "60,8"
enlace <- function(u) sprintf("\\href{%s}{[link]}", gsub("([%#])", "\\\\\\1", u))
dom <- function(u) sub("^www\\.", "", sub("^https?://([^/]+).*$", "\\1", u))

NOMBRE_FUERZA <- c(LLA = "LLA (Milei)", PJ = "Peronismo (Kicillof)",
                   FIT = "FIT-U (izquierda)", PU = "Provincias Unidas", OTROS = "Otros")
FAMILIA_FUERZA <- c(LLA = "LLA", PJ = "PJ", FIT = "IZQ", PU = "PROV", OTROS = "OTRO")

# =============================================================================
# 1. ENCUESTAS DE BALOTAJE: evolución y promedio
# =============================================================================
fechas_prom <- seq(min(bal$fecha_fin), FECHA_REFERENCIA, by = "week")
prom_t <- data.frame(fecha = fechas_prom,
                     p = sapply(fechas_prom, promedio_a_fecha))
bal$tipo <- ifelse(bal$es_promedio, "Promedio de Clarín (11 encuestadoras)",
                   "Encuesta individual")
g <- ggplot() +
  geom_hline(yintercept = 50, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  geom_line(data = prom_t, aes(fecha, 100 * p), colour = COLORES[["LLA"]], linewidth = 0.8) +
  geom_point(data = bal, aes(fecha_fin, 100 * p_milei, shape = tipo),
             size = 2.4, colour = "grey20", stroke = 0.8) +
  annotate("text", x = FECHA_REFERENCIA, y = 41.5, hjust = 1, size = 3.1,
           label = sprintf("Promedio ponderado al %s: %s%%",
                           format(FECHA_REFERENCIA, "%d/%m/%Y"), fmt(100 * mu_bal))) +
  annotate("text", x = min(bal$fecha_fin), y = 50.6, hjust = 0, size = 2.8,
           colour = "grey40", label = "50% = empate") +
  scale_shape_manual(values = c(16, 5), name = NULL) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_y_continuous(limits = c(40, 62), breaks = seq(40, 60, 5)) +
  labs(title = "Balotaje Milei vs. Kicillof: encuestas publicadas en 2026",
       subtitle = "% de Milei sobre los votos de ambos candidatos (sin blancos ni indecisos).\nLínea azul: promedio ponderado móvil de las encuestas",
       x = NULL, y = "% de Milei (votos válidos)") +
  tema_tp()
guardar_figura(g, "fig_encuestas_balotaje", alto = 3.8)

filas <- with(bal[order(bal$fecha_fin, decreasing = TRUE), ],
  sprintf("%s & %s & %s & %s & %s & %s & %s & %s \\\\",
          esc(acentuar(encuestadora)), format(fecha_fin, "%d/%m/%Y"),
          ifelse(is.na(n), "s/d", formatC(n, big.mark = ".", decimal.mark = ",", format = "d")),
          fmt(milei), fmt(kicillof), fmt(100 * p_milei),
          fmt(100 * peso / sum(bal$peso)), enlace(fuente)))
escribir_filas(filas, "tabla_encuestas_balotaje.tex")

# =============================================================================
# 2. PRIMERA VUELTA: proyección por escenario
# =============================================================================
resumen_1v <- function(s, mu, nombre) {
  V <- s$V
  data.frame(escenario = nombre, fuerza = colnames(V), media = as.numeric(mu[colnames(V)]),
             lo = apply(V, 2, quantile, 0.05), hi = apply(V, 2, quantile, 0.95),
             row.names = NULL)
}
ESCENARIOS_1V <- c("A. PRO aliado con LLA", "B. PRO por separado",
                   "C. Sólo encuestas por candidato")
d1v <- rbind(resumen_1v(sim_pres$oct2027, mu_pv, ESCENARIOS_1V[1]),
             resumen_1v(sim_pres$sinpro, mu_pv_sinpro, ESCENARIOS_1V[2]),
             resumen_1v(sim_pres$candidato, mu_pv_cand, ESCENARIOS_1V[3]))
d1v$escenario <- factor(d1v$escenario, levels = ESCENARIOS_1V)
d1v$etq <- factor(NOMBRE_FUERZA[d1v$fuerza], levels = rev(NOMBRE_FUERZA))
d1v$familia <- FAMILIA_FUERZA[d1v$fuerza]
g <- ggplot(d1v, aes(x = media, y = etq, fill = familia)) +
  geom_col(width = 0.62) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.25, orientation = "y",
                colour = "grey25", linewidth = 0.4) +
  geom_text(aes(x = hi, label = fmt(media)), hjust = -0.25, size = 3) +
  geom_vline(xintercept = 45, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = 40, linetype = "dotted", colour = "grey40", linewidth = 0.4) +
  facet_wrap(~escenario, ncol = 1) +
  scale_fill_manual(values = COLORES, guide = "none") +
  scale_x_continuous(limits = c(0, 64), breaks = seq(0, 60, 10)) +
  labs(title = "Primera vuelta proyectada (votos afirmativos, %)",
       subtitle = "Barra: promedio de encuestas. Línea negra: intervalo del 90% a oct-2027.\nUmbrales del art. 98: 40% (punteada) y 45% (guiones)",
       x = "% de votos afirmativos", y = NULL) +
  tema_tp()
guardar_figura(g, "fig_primera_vuelta", alto = 5.6)

filas <- sapply(names(NOMBRE_FUERZA), function(k) {
  celdas <- sapply(ESCENARIOS_1V, function(e) {
    r <- d1v[d1v$fuerza == k & d1v$escenario == e, ]
    sprintf("%s {\\footnotesize [%s--%s]}", fmt(r$media), fmt(r$lo, 0), fmt(r$hi, 0))
  })
  sprintf("%s & %s \\\\", NOMBRE_FUERZA[[k]], paste(celdas, collapse = " & "))
})
escribir_filas(filas, "tabla_primera_vuelta.tex")
filas <- with(pv, sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
  esc(acentuar(encuestadora)), format(fecha_fin, "%d/%m/%Y"),
  fmt(lla), fmt(pro), fmt(pj), fmt(fit), fmt(pu), fmt(otros), fmt(blanco_ns), enlace(fuente)))
escribir_filas(filas, "tabla_encuestas_pv.tex")

# =============================================================================
# 3. PROBABILIDADES DEL RESULTADO PRESIDENCIAL
# =============================================================================
NOMBRE_ESC <- c(hoy = "Si la elección fuera hoy (A)", oct2027 = "A. PRO aliado con LLA",
                sinpro = "B. PRO por separado", candidato = "C. Encuestas por candidato",
                final = "RESULTADO FINAL (A y B, 50% c/u)")
dprob <- do.call(rbind, lapply(colnames(tabla_pres), function(e) {
  t <- tabla_pres[, e]
  data.frame(escenario = NOMBRE_ESC[[e]], resultado = names(COLORES_RESULTADO),
             p = c(t[["milei_1v"]], t[["milei_total"]] - t[["milei_1v"]],
                   t[["kici_total"]] - t[["kicillof_1v"]], t[["kicillof_1v"]]))
}))
stopifnot(all(abs(tapply(dprob$p, dprob$escenario, sum) - 1) < 1e-9))
dprob$escenario <- factor(dprob$escenario, levels = rev(NOMBRE_ESC))
dprob$resultado <- factor(dprob$resultado, levels = rev(names(COLORES_RESULTADO)))
g <- ggplot(dprob, aes(x = 100 * p, y = escenario, fill = resultado)) +
  geom_col(width = 0.62, colour = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(p >= 0.05, fmt(100 * p, 0), "")),
            position = position_stack(vjust = 0.5), size = 3,
            colour = ifelse(dprob$resultado %in% c("Milei gana en 1a vuelta",
                                                   "Kicillof gana en 1a vuelta"),
                            "white", "black")) +
  geom_vline(xintercept = 50, linetype = "dashed", colour = "grey30", linewidth = 0.4) +
  scale_fill_manual(values = COLORES_RESULTADO, breaks = names(COLORES_RESULTADO), name = NULL) +
  scale_x_continuous(breaks = seq(0, 100, 25), limits = c(0, 100.5), expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(title = "¿Quién gana la presidencia? Probabilidad de cada resultado",
       subtitle = sprintf("%% de %s elecciones simuladas por escenario", format(N_SIM, big.mark = ".", decimal.mark = ",")),
       x = "% de las simulaciones", y = NULL) +
  tema_tp() + theme(panel.grid.major.y = element_blank())
guardar_figura(g, "fig_probabilidades", alto = 3.6)

FILAS_PRES <- c(milei_1v = "Milei gana en primera vuelta",
                kicillof_1v = "Kicillof gana en primera vuelta",
                balotaje = "Hay balotaje",
                milei_bal_si_hay = "\\quad Milei gana el balotaje (si lo hay)",
                milei_total = "\\textbf{Milei es presidente 2027--2031}",
                kici_total = "\\textbf{Kicillof es presidente 2027--2031}")
filas <- sapply(names(FILAS_PRES), function(k) {
  v <- tabla_pres[k, c("hoy", "oct2027", "sinpro", "candidato", "final")]
  celdas <- pct(v); celdas[5] <- sprintf("\\textbf{%s}", celdas[5])
  sprintf("%s & %s \\\\", FILAS_PRES[[k]], paste(celdas, collapse = " & "))
})
escribir_filas(filas, "tabla_presidencial.tex")

for (e in c("hoy", "oct2027", "sinpro", "candidato", "final")) {
  sufijo <- c(hoy = "Hoy", oct2027 = "A", sinpro = "B", candidato = "C", final = "Final")[[e]]
  mac(paste0("pMilei", sufijo),   pct(tabla_pres["milei_total", e]))
  mac(paste0("pKici", sufijo),    pct(tabla_pres["kici_total", e]))
  mac(paste0("pBal", sufijo),     pct(tabla_pres["balotaje", e]))
  mac(paste0("pMileiPV", sufijo), pct(tabla_pres["milei_1v", e]))
  mac(paste0("pKiciPV", sufijo),  pct(tabla_pres["kicillof_1v", e]))
  mac(paste0("pMileiCond", sufijo), pct(tabla_pres["milei_bal_si_hay", e]))
}
# Votos proyectados del resultado final (mezcla de A y B)
V_final <- rbind(sim_pres$oct2027$V, sim_pres$sinpro$V)
pbal_final <- c(sim_pres$oct2027$p_bal, sim_pres$sinpro$p_bal)
hay_bal_final <- c(sim_pres$oct2027$hay_balotaje, sim_pres$sinpro$hay_balotaje)
mac("votoLLAFinal", fmt(median(V_final[, "LLA"])))
mac("votoPJFinal",  fmt(median(V_final[, "PJ"])))
mac("votoLLAFinalLo", fmt(quantile(V_final[, "LLA"], 0.05)))
mac("votoLLAFinalHi", fmt(quantile(V_final[, "LLA"], 0.95)))
mac("votoPJFinalLo", fmt(quantile(V_final[, "PJ"], 0.05)))
mac("votoPJFinalHi", fmt(quantile(V_final[, "PJ"], 0.95)))
mac("balMedTodas", fmt(100 * median(pbal_final)))
mac("balMedCond",  fmt(100 * median(pbal_final[hay_bal_final])))
mac("balMedCondK", fmt(100 - 100 * median(pbal_final[hay_bal_final])))
mac("pKiciBalFinal", pct(tabla_pres["kici_total", "final"] - tabla_pres["kicillof_1v", "final"]))
mac("pMileiBalFinal", pct(tabla_pres["milei_total", "final"] - tabla_pres["milei_1v", "final"]))
mac("pvCandLLA", fmt(mu_pv_cand[["LLA"]])); mac("pvCandPJ", fmt(mu_pv_cand[["PJ"]]))
mac("pvCandFIT", fmt(mu_pv_cand[["FIT"]]))

# =============================================================================
# 4. BALOTAJE: todas las simulaciones vs. sólo cuando hay balotaje
# =============================================================================
dbal <- rbind(data.frame(panel = "Todas las simulaciones", p = 100 * pbal_final),
              data.frame(panel = "Sólo las simulaciones en que hay balotaje",
                         p = 100 * pbal_final[hay_bal_final]))
dbal$ganador <- ifelse(dbal$p > 50, "Gana Milei", "Gana Kicillof")
dbal$panel <- factor(dbal$panel, levels = unique(dbal$panel))
med_panel <- aggregate(p ~ panel, dbal, median)
g <- ggplot(dbal, aes(x = p, fill = ganador)) +
  geom_histogram(binwidth = 1, boundary = 50, colour = "white", linewidth = 0.2) +
  geom_vline(xintercept = 50, colour = "grey20", linewidth = 0.4) +
  geom_vline(data = med_panel, aes(xintercept = p), linetype = "dashed", colour = "grey35") +
  geom_text(data = med_panel, aes(x = p, y = Inf, label = paste0("mediana ", fmt(p))),
            inherit.aes = FALSE, vjust = 1.6, hjust = -0.08, size = 3) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Gana Milei" = COLORES[["LLA"]], "Gana Kicillof" = COLORES[["PJ"]]),
                    name = NULL) +
  scale_x_continuous(breaks = seq(20, 80, 5)) + coord_cartesian(xlim = c(25, 75)) +
  labs(title = "Balotaje simulado: % de Milei sobre los votos válidos",
       subtitle = "Hay balotaje sobre todo cuando a Milei le va peor que lo esperado\nen primera vuelta; por eso el panel de abajo se corre hacia Kicillof",
       x = "% de Milei en el balotaje", y = "Simulaciones") +
  tema_tp()
guardar_figura(g, "fig_balotaje", alto = 4.4)

# Sensibilidad a la deriva
filas <- apply(sens_deriva, 2, function(c)
  sprintf("%s & %s & %s \\\\", fmt(c[["deriva"]], 0), pct(c[["milei"]]), pct(c[["balotaje"]])))
escribir_filas(filas, "tabla_deriva.tex")
mac("pMileiDerivaCero", pct(sens_deriva["milei", 1]))
mac("pMileiDerivaSiete", pct(sens_deriva["milei", 4]))

# =============================================================================
# 5. ERROR HISTÓRICO DE LAS ENCUESTAS
# =============================================================================
derr <- rbind(data.frame(eleccion = err$eleccion, fecha = err$fecha, tipo = "Lo que daban las encuestas",
                         margen = err$margen_encuestas),
              data.frame(eleccion = err$eleccion, fecha = err$fecha, tipo = "Resultado real",
                         margen = err$margen_real))
err$etq <- paste0(format(as.Date(err$fecha), "%Y"), ": ", acentuar(err$eleccion))
derr$eleccion <- factor(paste0(format(as.Date(derr$fecha), "%Y"), ": ", acentuar(derr$eleccion)),
                        levels = rev(err$etq))
g <- ggplot(derr, aes(x = margen, y = eleccion)) +
  geom_line(aes(group = eleccion), colour = "grey70", linewidth = 1.2) +
  geom_point(aes(shape = tipo, colour = tipo), size = 3) +
  geom_text(data = transform(err, eleccion = factor(etq, levels = rev(err$etq))),
            aes(x = margen_real, y = eleccion, label = paste0("error ", fmt(error))),
            hjust = -0.25, size = 3) +
  scale_colour_manual(values = c("grey55", "grey10"), name = NULL) +
  scale_shape_manual(values = c(1, 16), name = NULL) +
  scale_x_continuous(limits = c(-1, 17)) +
  labs(title = "Error de las encuestas argentinas, 2023-2025",
       subtitle = "Margen entre los dos primeros, en puntos, a favor del ganador real",
       x = "Margen (pp)", y = NULL) +
  tema_tp()
guardar_figura(g, "fig_errores", alto = 2.9)
filas <- with(err, sprintf("%s & %s & %s & %s & %s & %s \\\\",
  esc(acentuar(eleccion)), format(as.Date(fecha), "%d/%m/%Y"), fmt(margen_encuestas),
  fmt(margen_real), fmt(error), enlace(fuente)))
escribir_filas(filas, "tabla_errores.tex")

# =============================================================================
# 6. EFECTO OFICIALISMO (calibración con 2025)
# =============================================================================
ord <- efecto$provincia[order(efecto$delta)]
def <- rbind(data.frame(provincia = efecto$provincia, tipo = "Legislativa nacional (oct-2025)",
                        pct = efecto$pct_misma_fuerza_legislativa_nacional_2025),
             data.frame(provincia = efecto$provincia, tipo = "Elección provincial 2025",
                        pct = efecto$pct_oficialismo_provincial))
def$provincia <- factor(def$provincia, levels = ord)
g <- ggplot(def, aes(x = pct, y = provincia)) +
  geom_line(aes(group = provincia), colour = "grey70", linewidth = 1.2) +
  geom_point(aes(shape = tipo, colour = tipo), size = 3) +
  geom_text(data = transform(efecto, provincia = factor(provincia, levels = ord)),
            aes(x = pmax(pct_oficialismo_provincial, pct_misma_fuerza_legislativa_nacional_2025),
                y = provincia, label = sub("\\.", ",", sprintf("%+.1f", delta))),
            hjust = -0.35, size = 3) +
  scale_colour_manual(values = c("Elección provincial 2025" = "grey10",
                                 "Legislativa nacional (oct-2025)" = "grey55"), name = NULL) +
  scale_shape_manual(values = c("Elección provincial 2025" = 16,
                                "Legislativa nacional (oct-2025)" = 1), name = NULL) +
  scale_x_continuous(limits = c(15, 82)) +
  labs(title = "Efecto oficialismo: elecciones provinciales vs. nacional (2025)",
       subtitle = sprintf("Diferencia media: %s pp (desvío %s pp, %d provincias)",
                          fmt(MU_DELTA), fmt(SD_DELTA), nrow(efecto)),
       x = "% de votos de la fuerza del gobernador", y = NULL) +
  tema_tp()
guardar_figura(g, "fig_efecto_oficialismo", alto = 3.4)
filas <- with(efecto, sprintf("%s & %s & %s & %s & %s & %s \\\\",
  acentuar(provincia), tipo, fmt(pct_oficialismo_provincial),
  fmt(pct_misma_fuerza_legislativa_nacional_2025), sub("\\.", ",", sprintf("%+.1f", delta)),
  enlace(fuente_provincial)))
escribir_filas(filas, "tabla_efecto.tex")
mac("nEfecto", nrow(efecto))
mac("muDeltaGob", fmt(mean(efecto$delta[efecto$tipo == "Gobernador"])))
mac("muDeltaLeg", fmt(mean(efecto$delta[efecto$tipo != "Gobernador"])))

# =============================================================================
# 7. GOBERNACIONES: resultado final (mezcla de escenarios con y sin alianza)
# =============================================================================
# Para cada distrito se promedian dos escenarios con 50% cada uno:
#   - base: alianzas vigentes en 2025;
#   - alternativo: el oficialismo se alía con LLA (distritos en ALIANZAS_B) o,
#     en CABA, PRO y LLA compiten por separado. En el resto de los distritos el
#     escenario alternativo es una réplica independiente del base.
alternativo <- function(d) if (d == "Ciudad de Buenos Aires") esc_caba else esc_alianza
clave <- function(pr) ifelse(pr$es_ofi, "OFI", pr$familia)
NOMBRE_CLAVE <- c(LLA = "La Libertad Avanza", PJ = "Peronismo", PROV = "Otra fuerza provincial",
                  PROV2 = "Otra fuerza provincial", IZQ = "Izquierda", OTRO = "Otros", RESTO = "Otros")
final_prov <- do.call(rbind, lapply(dist$distrito, function(d) {
  a <- esc_base$probs[[d]]; b <- alternativo(d)$probs[[d]]
  a$k <- clave(a); b$k <- clave(b)
  ks <- union(a$k, b$k)
  p <- sapply(ks, function(k) (sum(a$p[a$k == k]) + sum(b$p[b$k == k])) / 2)
  stopifnot(abs(sum(p) - 1) < 1e-9)
  p_ret <- p[names(p) != "OFI"]
  k_ret <- names(which.max(p_ret))
  bloque_ret <- a$bloque[a$k == k_ret][1]
  if (is.na(bloque_ret)) bloque_ret <- b$bloque[b$k == k_ret][1]
  fav <- names(which.max(p))
  info <- dist[dist$distrito == d, ]
  tb <- esc_base$tabla[esc_base$tabla$distrito == d, ]
  data.frame(distrito = d, gobernador = info$gobernador,
             oficialismo = info$oficialismo_bloque,
             familia_ofi = info$oficialismo_familia,
             p_ofi = p[["OFI"]], retador = bloque_ret, familia_ret = k_ret,
             p_ret = max(p_ret), favorito_es_ofi = fav == "OFI",
             med_ofi = tb$med_oficialismo, segundo = tb$segundo, med_segundo = tb$med_segundo,
             p_ofi_base = tb$p_oficialismo,
             p_ofi_alt = alternativo(d)$tabla$p_oficialismo[alternativo(d)$tabla$distrito == d],
             row.names = NULL)
}))
final_prov$favorito <- ifelse(final_prov$favorito_es_ofi, final_prov$oficialismo, final_prov$retador)
final_prov$familia_fav <- ifelse(final_prov$favorito_es_ofi, final_prov$familia_ofi, final_prov$familia_ret)
final_prov$p_fav <- ifelse(final_prov$favorito_es_ofi, final_prov$p_ofi, final_prov$p_ret)
final_prov$cambia <- final_prov$distrito %in% c(ALIANZAS_B, "Ciudad de Buenos Aires")
write.csv(final_prov, "results/gobernaciones_final.csv", row.names = FALSE)
print(final_prov[, c("distrito", "favorito", "p_fav", "retador", "p_ret")], digits = 2)

# Figura: probabilidad de que el oficialismo retenga cada distrito
fp <- final_prov
fp$nombre <- acentuar(fp$distrito)
fp$nombre <- factor(fp$nombre, levels = fp$nombre[order(fp$p_ofi)])
fp$etq_fam <- ETIQUETA_FAMILIA[fp$familia_ofi]
fp$etq_fam[fp$familia_ofi == "LLA"] <- "Oficialismo aliado de LLA"
fp$etq_fam[fp$familia_ofi == "PJ"] <- "Oficialismo peronista"
fp$etq_fam[fp$familia_ofi == "PROV"] <- "Oficialismo provincial"
COL_OFI <- c("Oficialismo aliado de LLA" = COLORES[["LLA"]],
             "Oficialismo peronista" = COLORES[["PJ"]],
             "Oficialismo provincial" = COLORES[["PROV"]])
g <- ggplot(fp, aes(y = nombre)) +
  geom_vline(xintercept = 50, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_segment(data = fp[fp$cambia, ], aes(x = 100 * p_ofi_base, xend = 100 * p_ofi_alt,
                                            yend = nombre), colour = "grey75", linewidth = 1.6) +
  geom_point(aes(x = 100 * p_ofi, colour = etq_fam), size = 3) +
  geom_text(aes(x = 100 * p_ofi, label = fmt(100 * p_ofi, 0)), hjust = -0.6, size = 2.8) +
  scale_colour_manual(values = COL_OFI, name = NULL) +
  scale_y_discrete(limits = levels(fp$nombre)) +
  guides(colour = guide_legend(nrow = 2)) +
  scale_x_continuous(limits = c(0, 108), breaks = seq(0, 100, 25)) +
  labs(title = "¿Retiene el oficialismo? Probabilidad por distrito (%)",
       subtitle = "Punto: resultado final. Barra gris: rango entre los escenarios\ncon y sin alianza con LLA (o PRO y LLA separados, en CABA)",
       x = "% de las simulaciones en que gana el oficialismo", y = NULL) +
  tema_tp()
guardar_figura(g, "fig_gobernaciones", alto = 5.4)

# Mapa en grilla (cada provincia es un cuadrado ubicado aproximadamente)
GRILLA <- data.frame(
  distrito = c("Jujuy", "Salta", "Formosa", "Misiones",
               "Catamarca", "Tucuman", "Chaco", "Corrientes",
               "San Juan", "La Rioja", "Santiago del Estero", "Santa Fe", "Entre Rios",
               "Mendoza", "San Luis", "Cordoba", "Buenos Aires", "Ciudad de Buenos Aires",
               "Neuquen", "La Pampa", "Rio Negro", "Chubut", "Santa Cruz", "Tierra del Fuego"),
  abrev = c("JUJ", "SAL", "FOR", "MIS", "CAT", "TUC", "CHA", "COR", "SJ", "LR", "SDE", "SF",
            "ER", "MZA", "SL", "CBA", "BA", "CABA", "NQN", "LP", "RN", "CHU", "SC", "TDF"),
  col = c(2, 3, 4, 5, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 2, 2, 2, 3),
  fila = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 6, 7, 8, 9))
mapa <- merge(GRILLA, final_prov[, c("distrito", "familia_fav", "p_fav")], all.x = TRUE)
mapa$relleno <- ifelse(is.na(mapa$familia_fav), "Eligió gobernador en 2025",
                       c(LLA = "Oficialismo aliado de LLA", PJ = "Oficialismo peronista",
                         PROV = "Oficialismo provincial")[mapa$familia_fav])
mapa$texto <- ifelse(is.na(mapa$p_fav), mapa$abrev,
                     paste0(mapa$abrev, "\n", fmt(100 * mapa$p_fav, 0), "%"))
g <- ggplot(mapa, aes(x = col, y = -fila)) +
  geom_tile(aes(fill = relleno), colour = "white", linewidth = 1.2, width = 0.95, height = 0.95) +
  geom_text(aes(label = texto), size = 2.7, lineheight = 0.9) +
  scale_fill_manual(values = c(COL_OFI, "Eligió gobernador en 2025" = "#e3e3e0"), name = NULL) +
  coord_equal() +
  guides(fill = guide_legend(ncol = 1)) +
  labs(title = "Mapa de las gobernaciones 2027",
       subtitle = "Fuerza favorita y su probabilidad\n(cuadrícula aproximada, no a escala)") +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold"), legend.position = "right")
guardar_figura(g, "fig_mapa", ancho = 6.2, alto = 5.6)

# Cantidad de gobernaciones por fuerza (mezcla de escenarios, distrito por distrito)
usa_alt <- matrix(runif(N_SIM * nrow(dist)) < 0.5, nrow = N_SIM)
fam_final <- esc_base$fam_mat
for (j in seq_len(ncol(fam_final))) {
  d <- colnames(fam_final)[j]
  alt <- alternativo(d)$fam_mat[, d]
  fam_final[usa_alt[, j], j] <- alt[usa_alt[, j]]
}
GRUPO <- c(LLA = "LLA (propia o aliada)", PJ = "Peronismo", PROV = "Provincialismos",
           PROV2 = "Provincialismos", RESTO = "Otros", OTRO = "Otros", IZQ = "Otros")
conteo_final <- t(apply(fam_final, 1, function(r)
  table(factor(GRUPO[r], levels = unique(GRUPO)))))
stopifnot(all(rowSums(conteo_final) == nrow(dist)))
res_conteo <- data.frame(grupo = colnames(conteo_final),
                         media = colMeans(conteo_final),
                         mediana = apply(conteo_final, 2, median),
                         lo = apply(conteo_final, 2, quantile, 0.05),
                         hi = apply(conteo_final, 2, quantile, 0.95))
print(res_conteo)
dcont <- do.call(rbind, lapply(c("LLA (propia o aliada)", "Peronismo", "Provincialismos"), function(gq)
  data.frame(grupo = gq, k = 0:22,
             p = sapply(0:22, function(k) mean(conteo_final[, gq] == k)))))
dcont$grupo <- factor(dcont$grupo, levels = c("LLA (propia o aliada)", "Peronismo", "Provincialismos"))
g <- ggplot(dcont[dcont$k <= 16, ], aes(x = k, y = 100 * p, fill = grupo)) +
  geom_col(width = 0.8) +
  facet_wrap(~grupo, ncol = 1) +
  scale_fill_manual(values = c("LLA (propia o aliada)" = COLORES[["LLA"]],
                               "Peronismo" = COLORES[["PJ"]],
                               "Provincialismos" = COLORES[["PROV"]]), guide = "none") +
  scale_x_continuous(breaks = 0:16) +
  labs(title = "¿Cuántas de las 22 gobernaciones gana cada espacio?",
       subtitle = "Distribución en las simulaciones (resultado final)",
       x = "Gobernaciones ganadas", y = "% de las simulaciones") +
  tema_tp()
guardar_figura(g, "fig_conteo_gobernaciones", alto = 4.6)

for (i in seq_len(nrow(res_conteo))) {
  nm <- c("LLA (propia o aliada)" = "LLA", "Peronismo" = "PJ", "Provincialismos" = "PROV",
          "Otros" = "Otros")[[res_conteo$grupo[i]]]
  mac(paste0("gob", nm, "Med"), fmt(res_conteo$mediana[i], 0))
  mac(paste0("gob", nm, "Lo"), fmt(res_conteo$lo[i], 0))
  mac(paste0("gob", nm, "Hi"), fmt(res_conteo$hi[i], 0))
  mac(paste0("gob", nm, "Media"), fmt(res_conteo$media[i]))
}
mac("cambiosEsperados", fmt(sum(1 - final_prov$p_ofi)))
mac("nOfiFavorito", sum(final_prov$favorito_es_ofi))
# Probabilidad de que TODOS los oficialismos retengan (escenario base)
gana_ofi_base <- sapply(dist$distrito, function(d) {
  M <- esc_base$M[[d]]; ofi <- esc_base$tabla$oficialismo[esc_base$tabla$distrito == d]
  colnames(M)[max.col(M, ties.method = "first")] == ofi
})
mac("pTodosRetienen", pct(mean(apply(gana_ofi_base, 1, all))))
mac("medOfiRetienen", fmt(median(rowSums(gana_ofi_base)), 0))

# Tabla de gobernaciones (resultado final)
final_prov$candidato <- sub(" \\(.*$", "", dist$candidato_oficialismo[match(final_prov$distrito, dist$distrito)])
filas <- with(final_prov[order(final_prov$distrito), ], sprintf(
  "%s & %s & %s & %s & %s & %s & %s \\\\",
  acentuar(distrito),
  paste0(esc(acentuar(oficialismo)), ifelse(favorito_es_ofi, "", "$^\\dagger$")),
  esc(acentuar(candidato)),
  sprintf("\\textbf{%s}", pct(p_ofi, 0)),
  esc(acentuar(retador)), pct(p_ret, 0),
  ifelse(cambia, sprintf("%s / %s", pct(p_ofi_base, 0), pct(p_ofi_alt, 0)), "--")))
escribir_filas(filas, "tabla_gobernaciones.tex")
# Medianas de voto (escenario base)
filas <- with(final_prov[order(final_prov$distrito), ], sprintf("%s & %s & %s & %s \\\\",
  acentuar(distrito), fmt(med_ofi), esc(acentuar(segundo)), fmt(med_segundo)))
escribir_filas(filas, "tabla_votos_provinciales.tex")

# Tabla de candidatos y situación (datos de entrada)
corta <- function(x) ifelse(x == "" | is.na(x), "--", esc(acentuar(x)))
filas <- with(dist[order(dist$distrito), ], sprintf(
  "%s & %s & %s & %s & %s & %s & %s \\\\",
  acentuar(distrito), corta(gobernador), corta(reeleccion), corta(candidato_oficialismo),
  corta(candidato_lla), corta(ifelse(candidato_otro != "", paste0(candidato_pj, "; otros: ", candidato_otro), candidato_pj)),
  enlace(fuente)))
escribir_filas(filas, "tabla_candidatos.tex")

# Encuestas provinciales
filas <- with(encprov, sprintf("%s & %s & %s & %s & %s & %s \\\\",
  acentuar(distrito), esc(encuestadora), format(fecha_fin, "%d/%m/%Y"),
  esc(acentuar(detalle)), sprintf("%s / %s", fmt(pct_oficialismo), fmt(pct_retador)), enlace(fuente)))
escribir_filas(filas, "tabla_encuestas_prov.tex")

# Cifras sueltas usadas en el texto
mac("pSantaFeBase", pct(final_prov$p_ofi_base[final_prov$distrito == "Santa Fe"]))
mac("pSantaFeAlt",  pct(final_prov$p_ofi_alt[final_prov$distrito == "Santa Fe"]))
mac("pCabaBase", pct(final_prov$p_ofi_base[final_prov$distrito == "Ciudad de Buenos Aires"]))
mac("pCabaAlt",  pct(final_prov$p_ofi_alt[final_prov$distrito == "Ciudad de Buenos Aires"]))
mac("pPBA", pct(final_prov$p_ofi[final_prov$distrito == "Buenos Aires"]))
mac("medPBAofi", fmt(final_prov$med_ofi[final_prov$distrito == "Buenos Aires"]))
mac("medPBAseg", fmt(final_prov$med_segundo[final_prov$distrito == "Buenos Aires"]))
mac("pJujuy", pct(final_prov$p_ofi[final_prov$distrito == "Jujuy"]))
mac("pSantaCruz", pct(final_prov$p_ofi[final_prov$distrito == "Santa Cruz"]))
mac("pChubut", pct(final_prov$p_ofi[final_prov$distrito == "Chubut"]))
mac("pRioNegro", pct(final_prov$p_ofi[final_prov$distrito == "Rio Negro"]))
mac("pCordoba", pct(final_prov$p_ofi[final_prov$distrito == "Cordoba"]))
mac("pSantaFe", pct(final_prov$p_ofi[final_prov$distrito == "Santa Fe"]))
mac("pCaba", pct(final_prov$p_ofi[final_prov$distrito == "Ciudad de Buenos Aires"]))
mac("nEncProv", nrow(encprov))
mac("nFilasDatos", nrow(bal) + nrow(pv) + nrow(err) + nrow(prov) + nrow(dist) +
                   nrow(efecto) + nrow(encprov) + nrow(caba_mayo))
mac("fechaCorte", format(FECHA_REFERENCIA, "%d/%m/%Y"))
cat("  Figuras:", length(list.files("figures", pattern = "\\.pdf$")),
    "| tablas:", length(list.files("results", pattern = "^tabla.*\\.tex$")), "\n")
