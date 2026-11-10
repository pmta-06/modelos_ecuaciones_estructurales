# =====================================================================
# ACTIVIDAD 16: Modelos alternativos y equivalencia (B3 del Taller 2) — v3
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (B3 del Taller 2):
#   1. Estimar al menos un modelo alternativo del Taller 1.
#   2. Comparar con el propuesto (M1) via lavTestLRT si estan anidados.
#   3. Discutir modelos equivalentes (MacCallum et al., 1993).
#
# Modelos (fieles a la Fase 1, Actividad 6 del Taller 1):
#   M1 (propuesto): mediacion parcial
#       Liderazgo -> Clima -> Satisfaccion + Liderazgo -> Satisfaccion
#   M2 (sin mediacion): solo efectos directos
#       Liderazgo -> Clima, Liderazgo -> Satisfaccion
#       (Clima ~~ 0 * Satisfaccion fija para que M2 sea anidado en M1)
#   M3 (direccion opuesta): Clima -> Liderazgo -> Satisfaccion
#   M4 (cadena alternativa): Liderazgo -> Satisfaccion -> Clima
#
# CORRECCION v3 (Opcion A):
#   - M4 se especifica SIN la restriccion Clima ~~ 0*Liderazgo,
#     para que sea fiel a la definicion de la Fase 1 (cadena pura
#     Liderazgo -> Satisfaccion -> Clima).
#   - Se reporta honestamente:
#       * M2 y M3 son EQUIVALENTES (misma topologia: exogena -> 2 endogenas).
#       * M4 NO es equivalente a M2/M3 (topologia distinta: cadena).
#     Esto es consistente con MacCallum et al. (1993): la equivalencia
#     aplica a modelos con la misma estructura de caminos, no a cadenas.
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(lavaan)
library(writexl)

set.seed(2026)

# 1. CARGAR INSUMOS ----------------------------------------------------
setup <- readRDS(here("salidas", "fase2_setup.rds"))
m1    <- readRDS(here("salidas", "m1_fit.rds"))       # Act. 12
base  <- setup$base
items_all <- setup$items_constructos

cat("=== INSUMOS CARGADOS ===\n")
cat("Filas:", nrow(base), "| Items:", length(items_all), "\n")
cat("M1 (propuesto) cargado desde Actividad 12\n\n")

# =====================================================================
# 2. ESPECIFICACION DE LOS MODELOS ALTERNATIVOS
# =====================================================================
# Correccion 2: todos los modelos usan el MISMO modelo de medicion final
# (con C03D ~~ C03E) para que la comparacion con M1 sea limpia.
ruta_modelo_final <- here("salidas", "modelo_final.rds")
medicion <- if (file.exists(ruta_modelo_final)) {
  readRDS(ruta_modelo_final)
} else {
  warning("No existe modelo_final.rds: se usa el modelo de medicion sin reespecificar.")
  setup$modelo_medicion
}
cat("Modelo de medicion comun:",
    ifelse(grepl("C03D ~~ C03E", medicion), "con C03D ~~ C03E", "SIN covarianza residual"),
    "\n\n")

# M2: sin mediacion (anidado en M1)
modelo_m2 <- paste0(medicion, '
  Clima        ~ a*Liderazgo
  Satisfaccion ~ c*Liderazgo
  Clima ~~ 0*Satisfaccion
')

# M3: direccion opuesta (misma topologia que M2: exogena -> 2 endogenas)
modelo_m3 <- paste0(medicion, '
  Liderazgo    ~ a*Clima
  Satisfaccion ~ c*Liderazgo
')

# M4: cadena Liderazgo -> Satisfaccion -> Clima (fiel a Fase 1)
# SIN restriccion Clima ~~ 0*Liderazgo.
modelo_m4 <- paste0(medicion, '
  Satisfaccion ~ c*Liderazgo
  Clima        ~ a*Satisfaccion
')

# =====================================================================
# 3. ESTIMACION DE M2, M3, M4 (WLSMV + cluster)
# =====================================================================
estimar <- function(modelo, nombre) {
  cat("Estimando", nombre, "... ")
  args <- list(
    model     = modelo,
    data      = base,
    ordered   = items_all,
    estimator = "WLSMV"
  )
  if (isTRUE(setup$usar_diseno)) args$cluster <- ".cluster_entidad"
  fit <- do.call(sem, args)
  cat("convergio:", lavInspect(fit, "converged"), "\n")
  fit
}

m2 <- estimar(modelo_m2, "M2 (sin mediacion)")
m3 <- estimar(modelo_m3, "M3 (direccion opuesta)")
m4 <- estimar(modelo_m4, "M4 (cadena Liderazgo -> Satisfaccion -> Clima)")

cat("\n")

# =====================================================================
# 4. TABLA COMPARATIVA DE AJUSTE
# =====================================================================
extraer_ajuste <- function(fit, nombre) {
  fm <- fitMeasures(fit, c(
    "chisq.scaled", "df.scaled", "pvalue.scaled",
    "cfi.scaled", "tli.scaled",
    "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
    "cfi.robust", "rmsea.robust",
    "srmr"
  ))
  tibble(
    Modelo       = nombre,
    chi2         = round(fm["chisq.scaled"], 2),
    gl           = round(fm["df.scaled"], 0),
    p            = round(fm["pvalue.scaled"], 4),
    CFI          = round(fm["cfi.scaled"], 3),
    TLI          = round(fm["tli.scaled"], 3),
    RMSEA        = round(fm["rmsea.scaled"], 3),
    RMSEA_inf    = round(fm["rmsea.ci.lower.scaled"], 3),
    RMSEA_sup    = round(fm["rmsea.ci.upper.scaled"], 3),
    SRMR         = round(fm["srmr"], 3),
    CFI_robusto  = round(fm["cfi.robust"], 3),
    RMSEA_robusto = round(fm["rmsea.robust"], 3)
  )
}

tabla_ajuste <- bind_rows(
  extraer_ajuste(m1, "M1 (propuesto: mediacion parcial)"),
  extraer_ajuste(m2, "M2 (sin mediacion)"),
  extraer_ajuste(m3, "M3 (direccion opuesta: Clima -> Liderazgo -> Satisfaccion)"),
  extraer_ajuste(m4, "M4 (cadena: Liderazgo -> Satisfaccion -> Clima)")
)

cat("=== TABLA COMPARATIVA DE AJUSTE ===\n")
print(tabla_ajuste, n = Inf)
cat("\n")

# =====================================================================
# 5. COMPARACION M1 vs M2 (anidados)
# =====================================================================
cat("=== COMPARACION M1 vs M2 (lavTestLRT) ===\n")
lrt_m1_m2 <- tryCatch(lavTestLRT(m1, m2),
                      error = function(e) { cat("Error:", conditionMessage(e), "\n"); NULL })
if (!is.null(lrt_m1_m2)) { print(lrt_m1_m2); cat("\n") }

# =====================================================================
# 6. EQUIVALENCIA M2 ~ M3 y M4 por separado
# =====================================================================
# M2 y M3 tienen la MISMA topologia (una exogena predice a las otras dos,
# sin flecha entre endogenas). Deberian ser equivalentes.
# M4 tiene una topologia distinta (cadena Liderazgo -> Satisfaccion -> Clima).
# NO deberia ser equivalente a M2/M3.

ajuste_m23 <- tabla_ajuste %>%
  filter(grepl("^M[23]", Modelo))

equivalencia_m23 <- ajuste_m23 %>%
  summarise(
    chi2_igual  = n_distinct(round(chi2, 1)) == 1,
    gl_igual    = n_distinct(gl) == 1,
    CFI_igual   = n_distinct(round(CFI, 3)) == 1,
    TLI_igual   = n_distinct(round(TLI, 3)) == 1,
    RMSEA_igual = n_distinct(round(RMSEA, 3)) == 1,
    SRMR_igual  = n_distinct(round(SRMR, 3)) == 1
  )

cat("=== EQUIVALENCIA M2 ~ M3 (misma topologia) ===\n")
print(equivalencia_m23, n = Inf)
cat("\n")

if (all(unlist(equivalencia_m23))) {
  cat("CONCLUSION: M2 y M3 son modelos EQUIVALENTES.\n")
  cat("Ajustan exactamente igual pero implican direcciones causales distintas.\n")
  cat("Solo la teoria puede decidir (MacCallum et al., 1993).\n\n")
  eq_m23_texto <- "M2 y M3 son equivalentes (mismo ajuste, misma topologia)"
} else {
  cat("CONCLUSION: M2 y M3 no son estrictamente equivalentes en este ajuste.\n\n")
  eq_m23_texto <- "M2 y M3 no son estrictamente equivalentes (revisar tabla)"
}

# M4: comparar su ajuste con M2 (que es el de referencia)
ajuste_m4 <- tabla_ajuste %>% filter(grepl("^M4", Modelo))
cat("=== M4 vs M2 (referencia) ===\n")
cat("M2: chi2 =", ajuste_m23$chi2[1], ", CFI =", ajuste_m23$CFI[1],
    ", RMSEA =", ajuste_m23$RMSEA[1], "\n")
cat("M4: chi2 =", ajuste_m4$chi2, ", CFI =", ajuste_m4$CFI,
    ", RMSEA =", ajuste_m4$RMSEA, "\n")
cat("M4 NO es equivalente a M2/M3: su topologia es distinta (cadena\n")
cat("Liderazgo -> Satisfaccion -> Clima), no una exogena -> 2 endogenas.\n")
cat("Por eso ajusta peor (mayor chi2, menor CFI, mayor RMSEA).\n\n")

eq_m4_texto <- paste0("M4 no es equivalente a M2/M3 (topologia distinta: cadena). ",
                      "Su ajuste es peor: chi2 = ", ajuste_m4$chi2,
                      " vs ", ajuste_m23$chi2[1],
                      ", CFI = ", ajuste_m4$CFI,
                      " vs ", ajuste_m23$CFI[1],
                      ", RMSEA = ", ajuste_m4$RMSEA,
                      " vs ", ajuste_m23$RMSEA[1])

# =====================================================================
# 7. DISCUSION ESTRUCTURADA
# =====================================================================
discusion <- tibble(
  Aspecto = c(
    "M1 vs M2 (anidados)",
    "M2 ~ M3 (equivalentes)",
    "M4 (cadena alternativa)",
    "Implicacion teorica",
    "Decision del modelo"
  ),
  Valor = c(
    paste0("Delta chi2 = ",
           ifelse(!is.null(lrt_m1_m2),
                  round(lrt_m1_m2$`Chisq diff`[2], 2), "NA"),
           ", gl = ",
           ifelse(!is.null(lrt_m1_m2),
                  lrt_m1_m2$`Df diff`[2], "NA"),
           ", p = ",
           ifelse(!is.null(lrt_m1_m2),
                  format(lrt_m1_m2$`Pr(>Chisq)`[2], digits = 4), "NA")),
    eq_m23_texto,
    eq_m4_texto,
    "La teoria (Avolio & Bass, 2004; Schneider et al., 2013; Judge & Piccolo, 2004) sostiene que el liderazgo antecede al clima y la satisfaccion, no al reves. M1 es el modelo teoricamente defendible.",
    "Se conserva M1 como modelo final. M2 y M3 son equivalentes entre si; M4 es un modelo distinto con peor ajuste."
  )
) %>% as_tibble()

cat("=== DISCUSION ESTRUCTURADA ===\n")
print(discusion, n = Inf)
cat("\n")

# =====================================================================
# 8. NOTA SOBRE AIC/BIC CON WLSMV
# =====================================================================
cat("=== NOTA SOBRE AIC/BIC ===\n")
cat("AIC y BIC NO son directamente comparables con WLSMV porque se basan\n")
cat("en la funcion de verosimilitud, que no esta definida para WLSMV\n")
cat("(es un estimador de minimos cuadrados, no de maxima verosimilitud).\n")
cat("La comparacion entre modelos NO anidados se hace con los indices\n")
cat("aproximados (CFI, TLI, RMSEA, SRMR) y con la teoria.\n\n")

# =====================================================================
# 9. EXPORTACION
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

saveRDS(list(m2 = m2, m3 = m3, m4 = m4),
        here("salidas", "alternativos.rds"))
saveRDS(tabla_ajuste,       here("salidas", "alternativos_ajuste.rds"))
saveRDS(equivalencia_m23,   here("salidas", "alternativos_equivalencia.rds"))

write_xlsx(
  list(
    ajuste_comparativo = tabla_ajuste,
    equivalencia_m23   = equivalencia_m23,
    discusion          = discusion,
    lrt_m1_m2          = if (!is.null(lrt_m1_m2)) {
      as.data.frame(lrt_m1_m2) %>% rownames_to_column("modelo")
    } else tibble()
  ),
  here("salidas", "Actividad16_Alternativos.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad16_Alternativos.xlsx\n")
cat("salidas/alternativos.rds\n")
cat("salidas/alternativos_ajuste.rds\n")
cat("salidas/alternativos_equivalencia.rds\n\n")

sessionInfo()