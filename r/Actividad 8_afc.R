# =====================================================================
# ACTIVIDAD 8: Estimación y ajuste del AFC (A2 del Taller 2) — v2
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (A2 del Taller 2):
#   - Estimar el modelo de medición con todos los factores correlacionados
#     (modelo estructural saturado).
#   - Verificar convergencia y ausencia de soluciones impropias (Heywood).
#   - Reportar χ², gl, p, CFI, TLI, RMSEA (IC 90 %) y SRMR.
#   - Incorporar el diseño muestral (cluster de entidad).
#
# CORRECCIONES v2:
#   - WLSMV no acepta se = "robust.huber.white" (eso es para MLR).
#     Se usa el se robusto por defecto de WLSMV.
#   - lavaan no admite sampling.weights + cluster simultáneamente con
#     WLSMV. Se prioriza cluster (errores robustos a conglomeración),
#     que es lo crítico para la EDI (193 entidades). Se documenta.
#   - Se fuerza as_tibble() en las tablas que se imprimen con n = Inf.
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(lavaan)
library(semTools)
library(semPlot)
library(writexl)

# 1. CARGAR EL SETUP DE LA ACTIVIDAD 7 ---------------------------------
setup <- readRDS(here("salidas", "fase2_setup.rds"))
base  <- setup$base

cat("=== SETUP CARGADO ===\n")
cat("Filas:", nrow(base), "| Ítems:", length(setup$items_constructos), "\n")
cat("Estimador:", setup$estimador, "\n")
cat("Diseño muestral:", setup$usar_diseno, "\n\n")

# 2. ESTIMACIÓN DEL AFC SATURADO ---------------------------------------
# Modelo de medición: 3 factores correlacionados (sin paths estructurales).
# Con WLSMV + ordered, lavaan usa pairwise para faltantes (default).
#
# Diseño muestral:
#   - Se usa cluster = .cluster_entidad (COD_ENT o CUIN con SIN_ENTIDAD).
#   - NO se usa sampling.weights: lavaan no admite pesos + cluster con
#     WLSMV. Se prioriza cluster por la estructura de conglomerados de
#     la EDI. Se documenta como limitación en el artículo.
#   - Con WLSMV + cluster, lavaan calcula errores robustos por defecto
#     (se = "robust.sem"). NO forzar "robust.huber.white" (es para MLR).

args_afc <- list(
  model     = setup$modelo_medicion,
  data      = base,
  ordered   = setup$items_constructos,
  estimator = setup$estimador
)

if (isTRUE(setup$usar_diseno)) {
  args_afc$cluster <- ".cluster_entidad"
  # NO se agrega se = "robust.huber.white" (WLSMV no lo acepta)
  # NO se agrega sampling.weights (incompatible con cluster en WLSMV)
}

afc <- do.call(cfa, args_afc)

cat("=== AFC ESTIMADO ===\n")
cat("¿Convergió?:", lavInspect(afc, "converged"), "\n")
cat("Iteraciones:", lavInspect(afc, "iterations"), "\n\n")

# 3. VERIFICACIÓN DE SOLUCIONES IMPROPIAS (HEYWOOD) --------------------
# 3.1 Post-check general de lavaan
post_check <- lavInspect(afc, "post.check")
cat("=== POST-CHECK ===\n")
cat("post.check (TRUE = sin problemas):", post_check, "\n\n")

# 3.2 Varianzas de error negativas (Heywood clásico)
theta <- lavInspect(afc, "theta")
var_negativas <- which(diag(theta) < 0)
if (length(var_negativas) == 0) {
  cat("OK: no hay varianzas de error negativas (sin Heywood).\n")
} else {
  warning("Varianzas de error negativas en: ",
          paste(names(var_negativas), collapse = ", "))
}

# 3.3 Correlaciones entre factores > 1 (Heywood en Phi)
cor_lv <- lavInspect(afc, "cor.lv")
cor_fuera <- which(abs(cor_lv[upper.tri(cor_lv)]) > 1)
if (length(cor_fuera) == 0) {
  cat("OK: correlaciones entre factores <= 1.\n\n")
} else {
  warning("Correlaciones entre factores > 1 (revisar identificación).")
}

# 3.4 Cargas estandarizadas (deben ser < 1 en valor absoluto)
std_sol <- standardizedSolution(afc)
cargas  <- std_sol %>% filter(op == "=~")
if (any(abs(cargas$est.std) >= 1, na.rm = TRUE)) {
  warning("Cargas estandarizadas >= 1: posible Heywood.")
} else {
  cat("OK: todas las cargas estandarizadas < 1 en valor absoluto.\n\n")
}

# 4. AJUSTE GLOBAL -----------------------------------------------------
# Con WLSMV + cluster, reportamos las versiones ROBUSTAS/escaladas.
# RMSEA con IC 90 % (robusto).
fit_afc <- fitMeasures(afc, c(
  "chisq.scaled", "df.scaled", "pvalue.scaled",
  "cfi.scaled", "tli.scaled",
  "rmsea.scaled",
  "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
  "cfi.robust", "tli.robust", "rmsea.robust",
  "srmr"
))

cat("=== AJUSTE GLOBAL DEL AFC ===\n")
print(round(fit_afc, 4))
cat("\n")

# Tabla de ajuste: indices ESCALADOS como criterio principal (homogeneos
# con las Actividades 9, 12 y 17-19) y robustos como sensibilidad.
tabla_ajuste <- tibble(
  Indice = c("chi2 escalado", "gl", "p",
             "CFI escalado", "TLI escalado",
             "RMSEA escalado", "RMSEA IC90 inferior", "RMSEA IC90 superior",
             "SRMR"),
  Valor  = round(as.numeric(fit_afc[c(
    "chisq.scaled", "df.scaled", "pvalue.scaled",
    "cfi.scaled", "tli.scaled",
    "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
    "srmr")]), 4),
  Robusto = c(NA, NA, NA,
              round(as.numeric(fit_afc[c("cfi.robust", "tli.robust",
                                         "rmsea.robust")]), 4),
              NA, NA, NA),
  Criterio = c("—", "—", "—",
               ">= 0.95", ">= 0.95",
               "<= 0.06", "—", "—",
               "<= 0.08")
) %>% as_tibble()

cat("=== TABLA DE AJUSTE (para el artículo) ===\n")
print(tabla_ajuste, n = Inf)
cat("\n")

# 5. CARGAS Y CORRELACIONES ENTRE FACTORES -----------------------------
cargas_tabla <- cargas %>%
  select(lhs, rhs, est.std, se, z, pvalue) %>%
  rename(factor = lhs, item = rhs, lambda = est.std) %>%
  mutate(
    significativa = ifelse(pvalue < 0.001, "***",
                           ifelse(pvalue < 0.01,  "**",
                                  ifelse(pvalue < 0.05,  "*", "n.s.")))
  ) %>%
  as_tibble()   # <-- fuerza tibble para evitar error de print

cat("=== CARGAS ESTANDARIZADAS ===\n")
print(cargas_tabla, n = Inf)
cat("\n")

# Tabla de correlaciones como tibble (para imprimir sin problemas)
cor_lv_tabla <- as.data.frame(cor_lv) %>%
  rownames_to_column("factor") %>%
  as_tibble()

cat("=== CORRELACIONES ENTRE FACTORES ===\n")
print(cor_lv_tabla, n = Inf)
cat("\n")

# 6. DIAGRAMA DEL AFC ESTANDARIZADO ------------------------------------
if (!dir.exists(here("figuras"))) dir.create(here("figuras"))

png(here("figuras", "act8_afc_estandarizado.png"),
    width = 12, height = 8, units = "in", res = 150)
semPaths(
  afc,
  whatLabels     = "std",
  layout         = "tree",
  rotation       = 2,
  edge.label.cex = 0.7,
  sizeMan        = 6,
  sizeLat        = 10,
  color          = list(lat = "#D6EAF8", man = "#FDEBD0"),
  edge.color     = "black",
  title          = TRUE
)
dev.off()

# 7. EXPORTACIÓN -------------------------------------------------------
saveRDS(afc,           here("salidas", "afc_fit.rds"))
saveRDS(fit_afc,       here("salidas", "afc_fitmeasures.rds"))
saveRDS(cargas_tabla,  here("salidas", "afc_cargas.rds"))

write_xlsx(
  list(
    ajuste         = tabla_ajuste,
    cargas         = cargas_tabla,
    correlaciones  = cor_lv_tabla,
    convergencia   = tibble(
      convergio      = lavInspect(afc, "converged"),
      iteraciones    = lavInspect(afc, "iterations"),
      post_check     = post_check,
      heywood_var    = length(var_negativas) > 0,
      heywood_cor    = length(cor_fuera) > 0,
      nota_diseno    = "Se usó cluster; sampling.weights no incorporable con WLSMV+cluster"
    ) %>% as_tibble()
  ),
  here("salidas", "Actividad8_AFC.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad8_AFC.xlsx\n")
cat("salidas/afc_fit.rds\n")
cat("salidas/afc_fitmeasures.rds\n")
cat("salidas/afc_cargas.rds\n")
cat("figuras/act8_afc_estandarizado.png\n\n")

sessionInfo()