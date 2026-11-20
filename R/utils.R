# =============================================================================
# utils.R — Funciones auxiliares
# -----------------------------------------------------------------------------
# Cada función documenta PRE (precondiciones), POST (postcondiciones) e
# INV (invariantes), y las verifica con stopifnot().
# =============================================================================

FECHA_REFERENCIA <- as.Date("2026-11-19")   # fecha de corte de los datos
FECHA_ELECCION   <- as.Date("2027-10-24")   # primera vuelta presidencial

# -----------------------------------------------------------------------------
# peso_encuesta(n, fecha, vida_media)
#   Peso de una encuesta en el promedio: raíz del tamaño muestral (con tope de
#   3.000 para que una encuesta online gigante no domine) por un decaimiento
#   exponencial según antigüedad.
#     w = sqrt(min(n, 3000)) * 0.5^(días / vida_media)
#   Si n no fue informado se asume n = 1.000 (supuesto explícito).
# PRE : n numérico (admite NA), fecha Date, vida_media > 0.
# POST: vector de pesos > 0 del mismo largo.
# -----------------------------------------------------------------------------
peso_encuesta <- function(n, fecha, vida_media = 45) {
  stopifnot(length(n) == length(fecha), inherits(fecha, "Date"), vida_media > 0)
  n2 <- ifelse(is.na(n), 1000, pmin(n, 3000))
  dias <- as.numeric(FECHA_REFERENCIA - fecha)
  stopifnot(all(dias >= 0))                      # no hay encuestas "del futuro"
  w <- sqrt(n2) * 0.5^(dias / vida_media)
  stopifnot(all(w > 0), length(w) == length(n))
  w
}

# -----------------------------------------------------------------------------
# media_ponderada(x, w)
# PRE : x y w del mismo largo, w >= 0 con suma > 0.
# POST: escalar dentro del rango de x.
# -----------------------------------------------------------------------------
media_ponderada <- function(x, w) {
  stopifnot(length(x) == length(w), all(w >= 0), sum(w) > 0)
  m <- sum(x * w) / sum(w)
  stopifnot(m >= min(x) - 1e-9, m <= max(x) + 1e-9)
  m
}

# -----------------------------------------------------------------------------
# a_votos_afirmativos(df, cols)
#   Convierte porcentajes de intención de voto a porcentaje de votos
#   afirmativos válidos (lo que exige la Constitución para la 1a vuelta):
#   se descartan blanco, nulo e indecisos y se reparte proporcionalmente.
# PRE : df tiene las columnas `cols`, todas >= 0.
# POST: matriz con filas que suman 100.
# -----------------------------------------------------------------------------
a_votos_afirmativos <- function(df, cols) {
  stopifnot(all(cols %in% names(df)), all(as.matrix(df[cols]) >= 0))
  m <- as.matrix(df[cols])
  m <- 100 * m / rowSums(m)
  stopifnot(all(abs(rowSums(m) - 100) < 1e-8))
  m
}

# -----------------------------------------------------------------------------
# gana_primera_vuelta(v)
#   Reglas de los artículos 97 y 98 de la Constitución Nacional:
#   gana quien supera el 45% de los votos afirmativos, o quien obtiene al
#   menos 40% con más de 10 puntos de diferencia sobre el segundo.
# PRE : v vector con nombres, porcentajes que suman 100.
# POST: nombre del ganador o NA si hay balotaje.
# -----------------------------------------------------------------------------
gana_primera_vuelta <- function(v) {
  stopifnot(!is.null(names(v)), abs(sum(v) - 100) < 1e-6)
  o <- sort(v, decreasing = TRUE)
  if (o[1] >= 45 || (o[1] >= 40 && o[1] - o[2] >= 10)) names(o)[1] else NA
}

# -----------------------------------------------------------------------------
# guardar_figura / tema / formatos (igual que en el proyecto anterior)
# -----------------------------------------------------------------------------
guardar_figura <- function(grafico, nombre, ancho = 6.5, alto = 4) {
  stopifnot(inherits(grafico, "ggplot"), is.character(nombre))
  dir.create("figures", showWarnings = FALSE)
  ggplot2::ggsave(file.path("figures", paste0(nombre, ".pdf")), grafico,
                  width = ancho, height = alto, device = cairo_pdf, bg = "white")
  ggplot2::ggsave(file.path("figures", paste0(nombre, ".png")), grafico,
                  width = ancho, height = alto, dpi = 150, bg = "white")
  stopifnot(file.exists(file.path("figures", paste0(nombre, ".pdf"))))
  invisible(NULL)
}
tema_tp <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   plot.title.position = "plot",
                   plot.margin = ggplot2::margin(6, 14, 6, 6),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "bottom")
}

# Paleta neutral: NO se usan los colores partidarios oficiales. Orden
# categórico validado para daltonismo (diferencia perceptual >= 8 entre
# colores vecinos). Los grises quedan para "otros".
COLORES <- c(LLA = "#2a78d6", PJ = "#eb6834", PROV = "#1baf7a",
             PROV2 = "#eda100", IZQ = "#e87ba4", OTRO = "#9e9e9e",
             RESTO = "#9e9e9e")
ETIQUETA_FAMILIA <- c(LLA = "La Libertad Avanza y aliados",
                      PJ = "Peronismo", PROV = "Oficialismo provincial",
                      PROV2 = "Otro provincialismo", IZQ = "Izquierda",
                      OTRO = "Otros", RESTO = "Otros")
# Resultado presidencial: dos tonos por candidato (1a vuelta oscuro, balotaje claro)
COLORES_RESULTADO <- c("Milei gana en 1a vuelta" = "#184f95",
                       "Milei gana el balotaje" = "#5598e7",
                       "Kicillof gana el balotaje" = "#f0875a",
                       "Kicillof gana en 1a vuelta" = "#b5441a")

escribir_macro <- function(archivo, nombre, valor) {
  stopifnot(grepl("^[A-Za-z]+$", nombre))
  cat(sprintf("\\newcommand{\\%s}{%s}\n", nombre, valor),
      file = archivo, append = TRUE)
}
fmt <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
fmt_pct <- function(x, d = 1) paste0(fmt(100 * x, d), "\\%")

# Escapa caracteres especiales de LaTeX en texto plano
esc <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x
}
# Restituye acentos en nombres guardados sin tildes en los CSV
ACENTOS <- c("\\bCordoba\\b" = "Córdoba", "cordobes\\b" = "cordobés",
  "\\bEntre Rios\\b" = "Entre Ríos", "\\bNeuquen\\b" = "Neuquén",
  "\\bRio Negro\\b" = "Río Negro", "\\bTucuman\\b" = "Tucumán",
  "\\bSaenz\\b" = "Sáenz", "\\bMartin\\b" = "Martín", "\\bRaul\\b" = "Raúl",
  "\\bInsfran\\b" = "Insfrán", "\\bRamirez\\b" = "Ramírez",
  "\\bAchaval\\b" = "Achával", "\\bGarcia\\b" = "García",
  "\\bSuarez\\b" = "Suárez", "\\bCatalan\\b" = "Catalán",
  "\\bPerez\\b" = "Pérez", "\\bSaltenos\\b" = "Salteños",
  "\\bSantacrucena\\b" = "Santacruceña", "\\bProyeccion\\b" = "Proyección",
  "\\bClarin\\b" = "Clarín", "\\bPublica\\b" = "Pública",
  "\\breeleccion\\b" = "reelección", "\\bclausula\\b" = "cláusula",
  "\\bnegociacion\\b" = "negociación", "\\bNegociacion\\b" = "Negociación",
  "\\bconstruccion\\b" = "construcción", "\\bOposicion\\b" = "Oposición",
  "\\bagresion\\b" = "agresión", "\\bsegun\\b" = "según",
  "\\bel mas\\b" = "el más", "\\bmas firme\\b" = "más firme",
  "\\bSi\\b" = "Sí", "\\bdescarta pintarse\\b" = "descarta pintarse",
  "\\bJusticia\\b" = "Justicia", "\\bsucesion\\b" = "sucesión",
  "\\bElias\\b" = "Elías", "\\bValdes\\b" = "Valdés",
  "\\bescrutado\\b" = "escrutado", "\\bAdrian\\b" = "Adrián", "\\bLopez\\b" = "López")
acentuar <- function(x) {
  for (k in names(ACENTOS)) x <- gsub(k, ACENTOS[[k]], x, perl = TRUE)
  x
}
