# =====================================================================
# ACTIVIDAD 6: Especificación del modelo (v4 corregida)
# Taller 1 - Fase 1 | SEM | USTA
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(lavaan)
library(semPlot)
library(writexl)

set.seed(2026)

# 1. CARGA DE LA BASE ANALÍTICA ----------------------------------------
base <- readRDS(here("salidas", "base_analitica.rds"))

items_liderazgo   <- paste0("C21", LETTERS[1:10])   # C21A..C21J
items_clima       <- paste0("C18", LETTERS[1:4])    # C18A..C18D
items_satisf      <- paste0("C03", LETTERS[1:5])    # C03A..C03E (C03F excluido)

cat("=== DIMENSIONES DE LA BASE ===\n")
cat("Filas:", nrow(base), "\n")
cat("Ítems Liderazgo:", length(items_liderazgo), "\n")
cat("Ítems Clima:", length(items_clima), "\n")
cat("Ítems Satisfacción:", length(items_satisf), "\n\n")

# =====================================================================
# 6.1. HIPÓTESIS
# =====================================================================
hipotesis <- tribble(
  ~Hipotesis, ~Relacion, ~Parametro, ~Signo, ~Sustento_teorico,
  "H1", "Liderazgo -> Clima laboral", "gamma_11", "+",
  "El liderazgo de apoyo y transformacional mejora la percepción de clima (Avolio & Bass, 2004; Schneider et al., 2013)",
  "H2", "Clima laboral -> Satisfacción", "beta_21", "+",
  "Un clima positivo se asocia con mayor satisfacción (Carr et al., 2003; Parker et al., 2003)",
  "H3", "Liderazgo -> Satisfacción (directo)", "gamma_21", "+",
  "El líder influye directamente en la satisfacción de sus colaboradores (Judge & Piccolo, 2004)",
  "H4", "Liderazgo -> Satisfacción (indirecto vía Clima)", "gamma_11*beta_21", "+",
  "El clima media la relación liderazgo-satisfacción (Walumbwa et al., 2008)"
)

cat("=== HIPÓTESIS ===\n")
print(hipotesis, n = Inf)
cat("\n")

# =====================================================================
# 6.2. DIAGRAMA DE CAMINOS
# =====================================================================
# NOTA: Para dibujar con semPaths, usamos una sintaxis SIN los " := "
modelo_diagrama <- '
  # --- Modelo de medición ---
  Liderazgo =~ C21A + C21B + C21C + C21D + C21E +
               C21F + C21G + C21H + C21I + C21J
  Clima =~ C18A + C18B + C18C + C18D
  Satisfaccion =~ C03A + C03B + C03C + C03D + C03E

  # --- Modelo estructural ---
  Clima ~ a*Liderazgo
  Satisfaccion ~ b*Clima + c*Liderazgo
'

# Objeto lavaan SIN estimar (do.fit = FALSE)
modelo_lavaan <- lavaan::sem(
  model  = modelo_diagrama,
  data   = base,
  do.fit = FALSE
)

# --- Diagrama propuesto ---
png(here("salidas", "act6_diagrama_caminos.png"),
    width = 12, height = 7, units = "in", res = 150)
semPaths(
  modelo_lavaan,
  whatLabels     = "name",
  layout         = "tree",
  rotation       = 2,
  edge.label.cex = 0.8,
  sizeMan        = 6,
  sizeLat        = 10,
  color          = list(lat = "#D6EAF8", man = "#FDEBD0"),
  edge.color     = "black",
  title          = TRUE
)
dev.off()

# --- Modelo rival M2: sin mediación (Liderazgo -> Clima, Liderazgo -> Satisfacción) ---
modelo_rival_diagrama <- '
  Liderazgo =~ C21A + C21B + C21C + C21D + C21E +
               C21F + C21G + C21H + C21I + C21J
  Clima =~ C18A + C18B + C18C + C18D
  Satisfaccion =~ C03A + C03B + C03C + C03D + C03E

  Clima ~ a*Liderazgo
  Satisfaccion ~ c*Liderazgo

  # sem() libera por defecto la covarianza entre residuos de Clima y Satisfaccion;
  # se fija en 0 para que M2 tenga t = 40 y gl = 150 (anidado en M1)
  Clima ~~ 0*Satisfaccion
'

modelo_rival_lavaan <- lavaan::sem(
  model  = modelo_rival_diagrama,
  data   = base,
  do.fit = FALSE
)

png(here("salidas", "act6_diagrama_rival.png"),
    width = 12, height = 7, units = "in", res = 150)
semPaths(
  modelo_rival_lavaan,
  whatLabels     = "name",
  layout         = "tree",
  rotation       = 2,
  edge.label.cex = 0.8,
  sizeMan        = 6,
  sizeLat        = 10,
  color          = list(lat = "#D6EAF8", man = "#FDEBD0"),
  edge.color     = "black",
  title          = TRUE
)
dev.off()

# =====================================================================
# 6.3. ECUACIONES Y MATRICES (notación LISREL)
# =====================================================================
parametros <- tribble(
  ~Matriz, ~Dimension, ~Elementos_libres, ~Elementos_fijos, ~Descripcion,
  "Λ_x", "10 x 1", 9,  1,  "Cargas de Liderazgo: 1 fija (C21A = 1), 9 libres",
  "Λ_y", "9 x 2",  7,  2,  "Cargas de Clima (1 fija en C18A) y Satisfacción (1 fija en C03A); 7 libres; 9 ceros (sin cargas cruzadas)",
  "B",   "2 x 2",  1,  3,  "β21 (Clima -> Satisfacción) libre; resto fijos en 0",
  "Γ",   "2 x 1",  2,  0,  "γ11 (Liderazgo -> Clima) y γ21 (Liderazgo -> Satisfacción) libres",
  "Φ",   "1 x 1",  1,  0,  "Varianza de Liderazgo libre (escala fijada con el marcador C21A = 1)",
  "Ψ",   "2 x 2",  2,  2,  "Varianzas de ζ1 y ζ2 libres; covarianza fija en 0 (diagonal)",
  "Θε",  "9 x 9",  9, 72,  "Varianzas de error de y libres; covarianzas fijas en 0",
  "Θδ",  "10 x 10", 10, 90, "Varianzas de error de x libres; covarianzas fijas en 0"
)

cat("=== PARÁMETROS DEL MODELO ===\n")
print(parametros, n = Inf)
cat("\n")

# =====================================================================
# 6.4. IDENTIFICACIÓN
# =====================================================================
k        <- length(items_liderazgo) + length(items_clima) + length(items_satisf)
momentos <- k * (k + 1) / 2
t_libre  <- 9 + 7 + 1 + 2 + 1 + 2 + 9 + 10   # = 41 (Phi libre; escala por marcador)
gl       <- momentos - t_libre

cat("=== IDENTIFICACIÓN ===\n")
cat("k (número de indicadores):", k, "\n")
cat("Momentos (k(k+1)/2):", momentos, "\n")
cat("t (parámetros libres):", t_libre, "\n")
cat("gl =", momentos, "-", t_libre, "=", gl, "\n")
cat("¿Modelo identificado? (gl >= 0):", ifelse(gl >= 0, "SÍ", "NO"), "\n\n")

cat("Regla de los tres indicadores:\n")
cat("  - Liderazgo:", length(items_liderazgo), "indicadores (>= 3) OK\n")
cat("  - Clima:", length(items_clima), "indicadores (>= 3) OK\n")
cat("  - Satisfacción:", length(items_satisf), "indicadores (>= 3) OK\n\n")

cat("Regla recursiva: B es triangular inferior (β12 = 0), modelo recursivo OK\n\n")

# =====================================================================
# 6.5. MODELOS ALTERNATIVOS
# =====================================================================
modelos_alternativos <- tribble(
  ~Modelo, ~Descripcion, ~Efectos, ~Nota,
  "M1 (propuesto)", "Mediación parcial: Liderazgo -> Clima -> Satisfacción y Liderazgo -> Satisfacción",
  "γ11, β21, γ21", "Modelo principal",
  "M2 (sin mediación)", "Solo efectos directos: Liderazgo -> Satisfacción y Liderazgo -> Clima",
  "γ11, γ21", "Rival plausible: el clima no media",
  "M3 (dirección opuesta)", "Clima -> Liderazgo -> Satisfacción",
  "γ11', β21'", "Rival teórico: el clima permite liderazgo",
  "M4 (equivalente)", "Liderazgo -> Satisfacción -> Clima",
  "γ11'', β21''", "M2, M3 y M4 son equivalentes entre sí (mismo gl = 150; MacCallum et al., 1993)"
)

cat("=== MODELOS ALTERNATIVOS ===\n")
print(modelos_alternativos, n = Inf)
cat("\n")

# =====================================================================
# 6.6. SINTAXIS LAVAAN (Para el informe y Fase 2)
# =====================================================================
sintaxis_propuesta <- '
  # --- Modelo de medición ---
  Liderazgo =~ C21A + C21B + C21C + C21D + C21E +
               C21F + C21G + C21H + C21I + C21J
  Clima =~ C18A + C18B + C18C + C18D
  Satisfaccion =~ C03A + C03B + C03C + C03D + C03E

  # --- Modelo estructural ---
  Clima ~ a*Liderazgo
  Satisfaccion ~ b*Clima + c*Liderazgo

  # --- Efectos ---
  indirecto := a * b
  directo   := c
  total     := c + (a * b)
'

cat("=== SINTAXIS LAVAAN (MODELO PROPUESTO) ===\n")
cat(sintaxis_propuesta, "\n\n")

sintaxis_rival <- '
  Liderazgo =~ C21A + C21B + C21C + C21D + C21E +
               C21F + C21G + C21H + C21I + C21J
  Clima =~ C18A + C18B + C18C + C18D
  Satisfaccion =~ C03A + C03B + C03C + C03D + C03E

  Satisfaccion ~ c*Liderazgo
  Clima ~ a*Liderazgo

  # Covarianza residual fija en 0 (lavaan la libera por defecto)
  Clima ~~ 0*Satisfaccion
'

cat("=== SINTAXIS LAVAAN (MODELO RIVAL SIN MEDIACIÓN) ===\n")
cat(sintaxis_rival, "\n\n")

# =====================================================================
# 6.7. EXPORTACIÓN
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

writeLines(sintaxis_propuesta, here("salidas", "act6_sintaxis_propuesta.txt"))
writeLines(sintaxis_rival,     here("salidas", "act6_sintaxis_rival.txt"))

write_xlsx(
  list(
    hipotesis            = hipotesis,
    parametros           = parametros,
    identificacion       = tibble(
      k          = k,
      momentos   = momentos,
      t_libres   = t_libre,
      gl         = gl,
      identificado = ifelse(gl >= 0, "Sí", "No")
    ),
    modelos_alternativos = modelos_alternativos
  ),
  here("salidas", "Actividad6_Especificacion_Modelo.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad6_Especificacion_Modelo.xlsx\n")
cat("salidas/act6_sintaxis_propuesta.txt\n")
cat("salidas/act6_sintaxis_rival.txt\n")
cat("Gráficos: act6_diagrama_caminos.png y act6_diagrama_rival.png\n")

sessionInfo()
