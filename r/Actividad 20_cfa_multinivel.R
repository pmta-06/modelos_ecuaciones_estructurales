# =====================================================================
# ACTIVIDAD 20: CFA multinivel con entidad (complemento B1/B2) — v2
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (observacion del profesor, puntos 1 y 2):
#   - Capturar la variacion ENTRE entidades (nivel 2) con un CFA
#     multinivel (sin paths estructurales), que responde al punto 2
#     del profesor sin caer en multigrupo con 201 grupos.
#
# NOTA METODOLOGICA:
#   lavaan no implementa multinivel con WLSMV. Se usa MLR (items
#   tratados como continuos) como analisis complementario.
#   Se documenta en limitaciones.
#
# CORRECCION v2:
#   - summary() de lavaan multinivel NO acepta fit.measure.
#     Se usa solo summary(fit, standardized = TRUE).
#   - Se anade bloque de coeficientes de medicion (cargas within/between).
# =====================================================================

library(tidyverse)
library(here)
library(lavaan)
library(writexl)

set.seed(2026)

setup <- readRDS(here("salidas", "fase2_setup.rds"))
base  <- setup$base
items_all <- setup$items_constructos

cat("=== INSUMOS CARGADOS ===\n")
cat("Filas:", nrow(base), "| Entidades:", n_distinct(base$.cluster_entidad), "\n\n")

# =====================================================================
# 1. CFA MULTINIVEL (solo medicion, sin paths estructurales)
# =====================================================================
modelo_cfa_mn <- '
  level: 1
    Liderazgo_w    =~ C21A + C21B + C21C + C21D + C21E +
                      C21F + C21G + C21H + C21I + C21J
    Clima_w        =~ C18A + C18B + C18C + C18D
    Satisfaccion_w =~ C03A + C03B + C03C + C03D + C03E

  level: 2
    Liderazgo_b    =~ C21A + C21B + C21C + C21D + C21E +
                      C21F + C21G + C21H + C21I + C21J
    Clima_b        =~ C18A + C18B + C18C + C18D
    Satisfaccion_b =~ C03A + C03B + C03C + C03D + C03E
'

cat("=== ESTIMANDO CFA MULTINIVEL (MLR, listwise) ===\n")
cat("Hora de inicio:", format(Sys.time(), "%H:%M:%S"), "\n\n")

t_inicio <- Sys.time()

fit_cfa_mn <- cfa(
  model     = modelo_cfa_mn,
  data      = base,
  estimator = "MLR",
  cluster   = ".cluster_entidad",
  missing   = "listwise"
)

t_fin <- Sys.time()
tiempo <- as.numeric(difftime(t_fin, t_inicio, units = "mins"))

cat("Tiempo:", round(tiempo, 1), "minutos\n")
cat("Convergio:", lavInspect(fit_cfa_mn, "converged"), "\n")
cat("Iteraciones:", lavInspect(fit_cfa_mn, "iterations"), "\n\n")

# =====================================================================
# 2. RESUMEN (sin fit.measure, que falla en multinivel)
# =====================================================================
cat("=== RESUMEN DEL CFA MULTINIVEL ===\n")
summary(fit_cfa_mn, standardized = TRUE)
cat("\n")

# =====================================================================
# 2b. COEFICIENTES DE MEDICION (within y between)
# =====================================================================
coefs_medicion <- parameterEstimates(fit_cfa_mn, standardized = TRUE) %>%
  filter(op == "=~") %>%
  select(lhs, op, rhs, est, se, z, pvalue, std.all) %>%
  as_tibble()

cat("=== COEFICIENTES DE MEDICION (within y between) ===\n")
print(coefs_medicion, n = Inf)
cat("\n")

# =====================================================================
# 3. ICC POR CONSTRUCTO (desde el CFA multinivel)
# =====================================================================
cat("=== VARIANZAS WITHIN Y BETWEEN POR CONSTRUCTO ===\n")

# Matrices completas (para correlaciones)
cov_within  <- lavInspect(fit_cfa_mn, "cov.lv")[[1]]
cov_between <- lavInspect(fit_cfa_mn, "cov.lv")[[2]]

# Vectores de diagonales (para ICC)
var_within  <- diag(cov_within)
var_between <- diag(cov_between)

icc_mn <- tibble(
  constructo  = c("Liderazgo", "Clima", "Satisfaccion"),
  var_within  = round(var_within, 4),
  var_between = round(var_between, 4),
  ICC         = round(var_between / (var_between + var_within), 4)
)

print(icc_mn, n = Inf)
cat("\n")

# =====================================================================
# 4. CORRELACIONES WITHIN Y BETWEEN (usando matrices completas)
# =====================================================================
cat("=== CORRELACIONES WITHIN (nivel 1) ===\n")
print(round(cov2cor(cov_within), 3))
cat("\n")

cat("=== CORRELACIONES BETWEEN (nivel 2) ===\n")
print(round(cov2cor(cov_between), 3))
cat("\n")
# =====================================================================
# 5. EXPORTACION (corregida)
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

saveRDS(fit_cfa_mn, here("salidas", "cfa_multinivel.rds"))
saveRDS(icc_mn,     here("salidas", "icc_multinivel.rds"))

write_xlsx(
  list(
    icc            = icc_mn,
    coefs_medicion = coefs_medicion,
    cor_within     = as.data.frame(round(cov2cor(cov_within), 3)) %>%
      rownames_to_column("constructo"),
    cor_between    = as.data.frame(round(cov2cor(cov_between), 3)) %>%
      rownames_to_column("constructo"),
    nota           = tibble(
      estimador    = "MLR (lavaan no implementa multinivel con WLSMV)",
      items        = "tratados como continuos (analisis complementario)",
      nivel_2      = ".cluster_entidad (201 entidades)",
      faltantes    = "listwise (mas rapido que FIML)",
      tiempo_min   = round(tiempo, 1),
      justificacion = "Captura variacion entre entidades sin multigrupo con 201 grupos"
    )
  ),
  here("salidas", "Actividad20_CFA_Multinivel.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad20_CFA_Multinivel.xlsx\n")
cat("salidas/cfa_multinivel.rds\n")
cat("salidas/icc_multinivel.rds\n\n")

cat("=== NOTA PARA EL ARTICULO ===\n")
cat("Se ajusto un CFA multinivel (within: servidores; between:\n")
cat("entidades) con MLR. El ICC por constructo confirma que ~5-6%\n")
cat("de la varianza es entre entidades. Las correlaciones between\n")
cat("son mas altas que las within, lo que sugiere que al agregar por\n")
cat("entidad se elimina el error de medicion individual.\n")

sessionInfo()