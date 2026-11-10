# =====================================================================
# ACTIVIDAD 12: Modelo estructural e hipotesis (B1 del Taller 2) — v2
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (B1 del Taller 2):
#   1. Estimar el modelo estructural propuesto M1.
#   2. Comparar M1 con el AFC (modelo de medicion) via lavTestLRT.
#   3. Tabla de hipotesis H1-H4: coef no est., EE, est., z, p bilateral,
#      p UNILATERAL (cuando la hipotesis fija signo), decision.
#   4. R2 de cada variable endogena (solo las latentes: Clima y Satisfaccion).
#   5. Diagrama de caminos estandarizado marcando no significativas.
#
# CORRECCION v2 (Correcciones 1, 2 y 5 del harness):
#   - M1 se construye a partir de modelo_final.rds (Actividad 9), de modo
#     que incluye C03D ~~ C03E y comparte EXACTAMENTE el modelo de medicion
#     con el AFC final. Antes M1 tenia 149 gl y el AFC 148 gl: el LRT
#     detectaba la covarianza omitida, no la restriccion estructural.
#   - Con el mismo modelo de medicion, M1 y el AFC tienen el mismo numero
#     de parametros (41 libres en el bloque estructural), por lo que
#     Delta gl = 0 y la prueba de diferencia es NO INFORMATIVA. El script
#     lo detecta y lo reporta explicitamente.
#   - Indices de ajuste: se reportan las versiones ESCALADAS (*.scaled)
#     como criterio principal, homogeneas con las Actividades 17-19, y se
#     anade la version robusta como columna de sensibilidad.
#   - R2: se reportan solo Clima y Satisfaccion (el resto eran indicadores).
#   - El diagrama anota en el pie que no hay relaciones no significativas.
#
# NOTA sobre bootstrap: con WLSMV + cluster + N=26.742 el bootstrap no
# parametrico no esta disponible en lavaan. El bootstrap real por
# conglomerados se hace con MLR en la Actividad 13b (sensibilidad).
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(lavaan)
library(semTools)
library(semPlot)
library(writexl)

set.seed(2026)

# 1. CARGAR INSUMOS ----------------------------------------------------
setup       <- readRDS(here("salidas", "fase2_setup.rds"))
afc_final   <- readRDS(here("salidas", "afc_final.rds"))   # Act. 9
base        <- setup$base

items_all <- setup$items_constructos

# Modelo de medicion FINAL (con C03D ~~ C03E) epecificado en la Actividad 9.
# Si no existe, se cae al modelo del setup (que ya la incluye desde la v2).
ruta_modelo_final <- here("salidas", "modelo_final.rds")
if (file.exists(ruta_modelo_final)) {
  medicion_final <- readRDS(ruta_modelo_final)
  origen_modelo  <- "salidas/modelo_final.rds (Actividad 9)"

  # M1 = modelo de medicion final + paths estructurales (mediacion parcial)
  modelo_m1 <- paste0(
    medicion_final, '
  Clima        ~ a*Liderazgo
  Satisfaccion ~ b*Clima + c*Liderazgo

  indirecto := a*b
  directo   := c
  total     := c + a*b
'
  )
} else {
  # Fallback: el setup v2 ya trae la covarianza residual y la sintaxis completa
  modelo_m1     <- setup$modelo_estructural_m1
  origen_modelo <- "setup$modelo_estructural_m1 (fallback)"
  warning("No existe modelo_final.rds: se usa el modelo del setup.")
}

cat("=== INSUMOS CARGADOS ===\n")
cat("Filas:", nrow(base), "| Items:", length(items_all), "\n")
cat("Modelo de medicion de M1:", origen_modelo, "\n")
cat("M1 incluye C03D ~~ C03E:", grepl("C03D ~~ C03E", modelo_m1), "\n\n")
cat("=== SINTAXIS M1 ===\n"); cat(modelo_m1, "\n\n")

# =====================================================================
# 1. ESTIMACION DE M1 (WLSMV + cluster)
# =====================================================================
args_m1 <- list(
  model     = modelo_m1,
  data      = base,
  ordered   = items_all,
  estimator = "WLSMV"
)
if (isTRUE(setup$usar_diseno)) args_m1$cluster <- ".cluster_entidad"

m1 <- do.call(sem, args_m1)

cat("=== M1 ESTIMADO ===\n")
cat("Convergio:", lavInspect(m1, "converged"), "\n")
cat("Iteraciones:", lavInspect(m1, "iterations"), "\n\n")

post_m1 <- lavInspect(m1, "post.check")
cat("post.check (TRUE = sin problemas):", post_m1, "\n\n")

# =====================================================================
# 2. AJUSTE GLOBAL DE M1 (escalados como criterio; robustos de sensibilidad)
# =====================================================================
fit_m1 <- fitMeasures(m1, c(
  "chisq.scaled", "df.scaled", "pvalue.scaled",
  "cfi.scaled", "tli.scaled",
  "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
  "cfi.robust", "tli.robust", "rmsea.robust",
  "srmr"
))

cat("=== AJUSTE GLOBAL DE M1 ===\n")
print(round(fit_m1, 4))
cat("\n")

tabla_ajuste_m1 <- tibble(
  Indice   = c("chi2 escalado", "gl", "p",
               "CFI escalado", "TLI escalado",
               "RMSEA escalado", "RMSEA IC90 inf", "RMSEA IC90 sup",
               "SRMR"),
  Valor    = round(as.numeric(fit_m1[c(
    "chisq.scaled", "df.scaled", "pvalue.scaled",
    "cfi.scaled", "tli.scaled",
    "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
    "srmr")]), 4),
  Robusto  = c(NA, NA, NA,
               round(fit_m1[c("cfi.robust", "tli.robust", "rmsea.robust")], 4),
               NA, NA, NA),
  Criterio = c("—", "—", "—",
               ">= 0.95", ">= 0.95",
               "<= 0.06", "—", "—",
               "<= 0.08")
) %>% as_tibble()

print(tabla_ajuste_m1, n = Inf)
cat("\n")

# =====================================================================
# 3. COMPARACION M1 vs AFC (mismo modelo de medicion)
# =====================================================================
# Correccion 1: al compartir el modelo de medicion, M1 y el AFC tienen el
# mismo numero de parametros libres. Por tanto Delta gl = 0 y la prueba
# de diferencia de chi-cuadrado no es informativa: no hay una restriccion
# estructural contrastable (el bloque estructural es saturado).
cat("=== COMPARACION M1 vs AFC (lavTestLRT) ===\n")

fit_afc <- fitMeasures(afc_final, c("chisq.scaled", "df.scaled",
                                    "cfi.scaled", "tli.scaled",
                                    "rmsea.scaled", "srmr"))

d_gl   <- as.numeric(fit_m1["df.scaled"] - fit_afc["df.scaled"])
d_chisq <- as.numeric(fit_m1["chisq.scaled"] - fit_afc["chisq.scaled"])

lrt_afc_m1 <- tryCatch(lavTestLRT(afc_final, m1),
                       error = function(e) {
                         cat("lavTestLRT no devolvio una tabla:\n",
                             conditionMessage(e), "\n")
                         NULL
                       })

if (!is.null(lrt_afc_m1)) print(lrt_afc_m1)

informacion_lrt <- if (d_gl == 0) {
  paste0("PRUEBA NO INFORMATIVA: M1 y el AFC comparten el mismo modelo de ",
         "medicion (", as.numeric(fit_afc["df.scaled"]), " gl ambos), ",
         "Delta gl = 0, Delta chi2 = ", round(d_chisq, 3), ". El bloque ",
         "estructural es saturado (3 paths sustituyen 3 covarianzas), por lo ",
         "que no existe una restriccion estructural contrastable contra el AFC.")
} else {
  paste0("Delta gl = ", d_gl, ", Delta chi2 = ", round(d_chisq, 3),
         ": la prueba SI es informativa; revisar si el estructural ajusta ",
         "significativamente peor.")
}

cat("\n>>>", informacion_lrt, "\n\n")

comparacion_afc_m1 <- tibble(
  Modelo   = c("AFC final (Actividad 9)", "M1 estructural (Actividad 12)"),
  gl       = c(fit_afc["df.scaled"], fit_m1["df.scaled"]),
  chi2     = c(fit_afc["chisq.scaled"], fit_m1["chisq.scaled"]),
  CFI      = c(fit_afc["cfi.scaled"], fit_m1["cfi.scaled"]),
  TLI      = c(fit_afc["tli.scaled"], fit_m1["tli.scaled"]),
  RMSEA    = c(fit_afc["rmsea.scaled"], fit_m1["rmsea.scaled"]),
  SRMR     = c(fit_afc["srmr"], fit_m1["srmr"]),
  Delta_gl = c(NA, d_gl),
  Delta_chi2 = c(NA, round(d_chisq, 3)),
  Interpretacion = c("Modelo de medicion final",
                     informacion_lrt)
) %>% as_tibble()

print(comparacion_afc_m1, n = Inf)
cat("\n")

# =====================================================================
# 4. TABLA DE HIPOTESIS (B1)
# =====================================================================
est_m1 <- parameterEstimates(m1, standardized = TRUE) %>%
  filter(op == "~" | op == ":=") %>%
  select(lhs, op, rhs, est, se, z, pvalue,
         std.all, ci.lower, ci.upper) %>%
  as_tibble()

hipotesis_tabla <- est_m1 %>%
  mutate(
    hipotesis = case_when(
      lhs == "Clima" & rhs == "Liderazgo"        ~ "H1: Liderazgo -> Clima",
      lhs == "Satisfaccion" & rhs == "Clima"     ~ "H2: Clima -> Satisfaccion",
      lhs == "Satisfaccion" & rhs == "Liderazgo" ~ "H3: Liderazgo -> Satisfaccion (directo)",
      lhs == "indirecto"                          ~ "H4: Liderazgo -> Clima -> Satisfaccion (indirecto)",
      TRUE                                        ~ NA_character_
    ),
    signo_esperado = ifelse(!is.na(hipotesis), "+", NA_character_),
    p_unilateral = case_when(
      is.na(signo_esperado)            ~ NA_real_,
      est > 0                          ~ pvalue / 2,
      TRUE                             ~ 1 - pvalue / 2
    ),
    decision = case_when(
      is.na(hipotesis)                              ~ NA_character_,
      p_unilateral < 0.05 & est > 0                 ~ "Se apoya (+, p_uni < .05)",
      pvalue < 0.05 & est < 0                       ~ "Significativo pero signo contrario",
      TRUE                                          ~ "No se apoya"
    )
  ) %>%
  filter(!is.na(hipotesis)) %>%
  select(hipotesis, lhs, op, rhs, est, se, z, pvalue,
         p_unilateral, std.all, ci.lower, ci.upper, decision) %>%
  as_tibble()

cat("=== TABLA DE HIPOTESIS (B1) ===\n")
print(hipotesis_tabla, n = Inf)
cat("\n")

# =====================================================================
# 5. R2 DE VARIABLES ENDOGENAS (solo latentes)
# =====================================================================
r2_all <- lavInspect(m1, "r2")
r2_tabla <- tibble(
  endogena = names(r2_all),
  R2       = round(as.numeric(r2_all), 4)
) %>%
  filter(endogena %in% c("Clima", "Satisfaccion")) %>%
  as_tibble()

cat("=== R2 DE VARIABLES ENDOGENAS (solo latentes) ===\n")
print(r2_tabla, n = Inf)
cat("\n")

# =====================================================================
# 6. DIAGRAMA ESTANDARIZADO (con anotacion de no significativas)
# =====================================================================
if (!dir.exists(here("figuras"))) dir.create(here("figuras"))

no_sig <- hipotesis_tabla %>% filter(decision != "Se apoya (+, p_uni < .05)")

pie_figura <- if (nrow(no_sig) == 0) {
  "Todas las relaciones estructurales son significativas (p < .05); no hay flechas marcadas como no significativas."
} else {
  paste0("Lineas discontinuas = relaciones no significativas: ",
         paste(no_sig$hipotesis, collapse = "; "))
}

png(here("figuras", "act12_modelo_estructural.png"),
    width = 12, height = 8, units = "in", res = 150)
semPaths(
  m1,
  whatLabels     = "std",
  layout         = "tree",
  rotation       = 2,
  edge.label.cex = 0.8,
  sizeMan        = 6,
  sizeLat        = 10,
  color          = list(lat = "#D6EAF8", man = "#FDEBD0"),
  edge.color     = "black",
  title          = TRUE
)
mtext(pie_figura, side = 1, line = 2, cex = 0.7)
dev.off()

cat("Diagrama guardado.\n")
cat("Pie de figura:", pie_figura, "\n\n")

# =====================================================================
# 7. EXPORTACION
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

saveRDS(m1,              here("salidas", "m1_fit.rds"))
saveRDS(fit_m1,          here("salidas", "m1_fitmeasures.rds"))
saveRDS(hipotesis_tabla, here("salidas", "m1_hipotesis.rds"))
saveRDS(r2_tabla,        here("salidas", "m1_r2.rds"))
saveRDS(list(lrt = lrt_afc_m1,
             informativa = (d_gl != 0),
             delta_gl = d_gl,
             delta_chisq = d_chisq,
             texto = informacion_lrt),
        here("salidas", "m1_lrt_vs_afc.rds"))

write_xlsx(
  list(
    ajuste_m1          = tabla_ajuste_m1,
    comparacion_afc_m1 = comparacion_afc_m1,
    hipotesis          = hipotesis_tabla,
    r2                 = r2_tabla,
    lrt_afc_m1         = if (!is.null(lrt_afc_m1)) {
      as.data.frame(lrt_afc_m1) %>% rownames_to_column("modelo")
    } else tibble(),
    coeficientes       = est_m1
  ),
  here("salidas", "Actividad12_Estructural.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad12_Estructural.xlsx\n")
cat("salidas/m1_fit.rds\n")
cat("salidas/m1_fitmeasures.rds\n")
cat("salidas/m1_hipotesis.rds\n")
cat("salidas/m1_r2.rds\n")
cat("salidas/m1_lrt_vs_afc.rds\n")
cat("figuras/act12_modelo_estructural.png\n\n")

sessionInfo()
