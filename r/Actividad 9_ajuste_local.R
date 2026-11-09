# =====================================================================
# ACTIVIDAD 9: Ajuste local y reespecificacion (A3 del Taller 2) — v3
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (A3 del Taller 2):
#   1. Revisar residuos de CORRELACION (|r| > 0.10).
#   2. Revisar indices de modificacion (IM) junto con EPC.
#   3. Reespecificar DE A UNA, con justificacion teorica, y documentar.
#   4. Usar split calibracion/validacion (N lo permite) para confirmar
#      el modelo final en la submuestra de validacion.
#
# CORRECCION v3 (Correccion 3 del harness):
#   - ANTES: se usaba res$cov.z (residuos estandarizados) con umbral 0.10,
#     lo que marcaba 170 de 171 pares (un z de 0.10 es trivial).
#   - AHORA: se usan los residuos de CORRELACION con |r| > 0.10, que es el
#     criterio del Taller (Kline, 2023). Con datos ordinales lavaan 0.7
#     devuelve las correlaciones residuales en el componente $cov (no hay
#     $cor); el codigo acepta ambos nombres segun la version.
#   - Se exportan tablas de ajuste ordenadas (Indice / Valor / Robusto /
#     Criterio) en lugar de la traspuesta sin nombres, para que el libro
#     de resultados pueda leer el modelo final.
#   - Indices ESCALADOS (*.scaled) como criterio principal, homogeneos con
#     el resto de actividades; robustos como columna de sensibilidad.
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(lavaan)
library(semTools)
library(semPlot)
library(writexl)

# 1. CARGAR INSUMOS DE LA ACTIVIDAD 8 ----------------------------------
setup <- readRDS(here("salidas", "fase2_setup.rds"))
afc   <- readRDS(here("salidas", "afc_fit.rds"))
base  <- setup$base

cat("=== INSUMOS CARGADOS ===\n")
cat("Filas:", nrow(base), "| Items:", length(setup$items_constructos), "\n")
cat("Split disponible:", ".split" %in% names(base), "\n\n")

# Helper: tabla de ajuste homogenea (escalado principal + robusto)
tabla_ajuste_tidy <- function(fit) {
  fm <- fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                           "cfi.scaled", "tli.scaled",
                           "rmsea.scaled", "rmsea.ci.lower.scaled",
                           "rmsea.ci.upper.scaled", "srmr",
                           "cfi.robust", "tli.robust", "rmsea.robust"))
  tibble(
    Indice  = c("chi2 escalado", "gl", "p", "CFI escalado", "TLI escalado",
                "RMSEA escalado", "RMSEA IC90 inf", "RMSEA IC90 sup", "SRMR"),
    Valor   = round(as.numeric(fm[c("chisq.scaled", "df.scaled",
                                    "pvalue.scaled", "cfi.scaled", "tli.scaled",
                                    "rmsea.scaled", "rmsea.ci.lower.scaled",
                                    "rmsea.ci.upper.scaled", "srmr")]), 4),
    Robusto = c(NA, NA, NA,
                round(as.numeric(fm[c("cfi.robust", "tli.robust",
                                      "rmsea.robust")]), 4),
                NA, NA, NA),
    Criterio = c("—", "—", "—", ">= 0.95", ">= 0.95",
                 "<= 0.06", "—", "—", "<= 0.08")
  )
}

# =====================================================================
# 2. RESIDUOS DE CORRELACION (|r| > 0.10)
# =====================================================================
# lavResiduals con datos ordinales: la matriz de momentos es la de
# correlaciones policoricas, de modo que res$cov contiene las
# correlaciones residuales (en algunas versiones el componente se llama
# res$cor). Se acepta cualquiera de los dos.
res <- lavResiduals(afc, zstat = TRUE, summary = FALSE)

res_cor <- if (!is.null(res$cor)) res$cor else res$cov
if (is.null(res_cor)) stop("lavResiduals no devolvio residuos de correlacion.")

mat_res <- res_cor
mat_res[lower.tri(mat_res, diag = TRUE)] <- NA
residuos_altos <- as.data.frame(as.table(mat_res)) %>%
  filter(!is.na(Freq), abs(Freq) > 0.10) %>%
  rename(item1 = Var1, item2 = Var2, residuo_r = Freq) %>%
  arrange(desc(abs(residuo_r))) %>%
  as_tibble()

cat("=== RESIDUOS DE CORRELACION |r| > 0.10 ===\n")
if (nrow(residuos_altos) == 0) {
  cat("OK: ningun par de items con residuo de correlacion > 0.10.\n\n")
} else {
  cat("Pares con |r| > 0.10:", nrow(residuos_altos), "de",
      ncol(res_cor) * (ncol(res_cor) - 1) / 2, "pares posibles\n")
  print(residuos_altos, n = Inf)
  cat("\n")
}

# =====================================================================
# 3. INDICES DE MODIFICACION (IM) CON EPC
# =====================================================================
mi <- modindices(afc, sort. = TRUE, minimum.value = 10) %>%
  as_tibble() %>%
  filter(op == "~~" | op == "=~") %>%
  arrange(desc(mi))

cat("=== INDICES DE MODIFICACION (IM > 10) ===\n")
print(head(mi, 20), n = Inf)
cat("\n")

# =====================================================================
# 4. SPLIT CALIBRACION / VALIDACION
# =====================================================================
base_cal <- base %>% filter(.split == "calibracion")
base_val <- base %>% filter(.split == "validacion")

cat("=== SPLIT ===\n")
cat("Calibracion:", nrow(base_cal), "| Validacion:", nrow(base_val), "\n\n")

# =====================================================================
# 5. REESTIMAR EN CALIBRACION (punto de partida)
# =====================================================================
args_cal <- list(model = setup$modelo_medicion, data = base_cal,
                 ordered = setup$items_constructos,
                 estimator = setup$estimador)
if (isTRUE(setup$usar_diseno)) args_cal$cluster <- ".cluster_entidad"
afc_cal <- do.call(cfa, args_cal)

cat("=== AFC EN CALIBRACION ===\n")
cat("Convergio:", lavInspect(afc_cal, "converged"), "\n")
print(tabla_ajuste_tidy(afc_cal), n = Inf)
cat("\n")

# =====================================================================
# 6. PROPUESTA DE REESPECIFICACION (DE A UNA, JUSTIFICADA)
# =====================================================================
mi_cov <- mi %>% filter(op == "~~") %>% arrange(desc(mi))
par_top <- mi_cov %>% slice(1)

cat("=== PAR CON MAYOR IM (covarianza residual) ===\n")
print(par_top, n = Inf)
cat("\n")

items_lid <- setup$items_liderazgo
items_cli <- setup$items_clima
items_sat <- setup$items_satisf

mismo_factor <- function(a, b) {
  (a %in% items_lid & b %in% items_lid) |
    (a %in% items_cli & b %in% items_cli) |
    (a %in% items_sat & b %in% items_sat)
}

justificable <- mismo_factor(par_top$lhs, par_top$rhs)
cat("¿Par intra-factor (justificable)?:", justificable, "\n\n")

# =====================================================================
# 7. MODELO REESPECIFICADO (SI HAY JUSTIFICACION)
# =====================================================================
modelo_final   <- setup$modelo_medicion
reespecificado <- FALSE

if (justificable && par_top$mi > 10 && abs(par_top$epc) > 0.05) {
  linea_cov <- paste0("  ", par_top$lhs, " ~~ ", par_top$rhs)
  modelo_final <- paste0(setup$modelo_medicion, "\n", linea_cov)
  reespecificado <- TRUE
  cat("=== REESPECIFICACION APLICADA ===\n")
  cat("Covarianza residual liberada:", par_top$lhs, "~~", par_top$rhs, "\n")
  cat("IM:", round(par_top$mi, 2), "| EPC:", round(par_top$epc, 3), "\n\n")
} else {
  cat("=== SIN REESPECIFICACION ===\n")
  cat("No hay par intra-factor con IM y EPC suficientes.\n\n")
}

# =====================================================================
# 8. REESTIMAR MODELO FINAL EN CALIBRACION
# =====================================================================
args_final_cal <- list(model = modelo_final, data = base_cal,
                       ordered = setup$items_constructos,
                       estimator = setup$estimador)
if (isTRUE(setup$usar_diseno)) args_final_cal$cluster <- ".cluster_entidad"
afc_final_cal <- do.call(cfa, args_final_cal)

fit_cal <- fitMeasures(afc_final_cal, c("chisq.scaled", "df.scaled",
                                        "cfi.scaled", "tli.scaled",
                                        "rmsea.scaled", "srmr"))
cat("=== AFC FINAL EN CALIBRACION ===\n")
print(round(fit_cal, 4)); cat("\n")

# =====================================================================
# 9. CONFIRMACION EN VALIDACION
# =====================================================================
args_final_val <- list(model = modelo_final, data = base_val,
                       ordered = setup$items_constructos,
                       estimator = setup$estimador)
if (isTRUE(setup$usar_diseno)) args_final_val$cluster <- ".cluster_entidad"
afc_final_val <- do.call(cfa, args_final_val)

fit_val <- fitMeasures(afc_final_val, c("chisq.scaled", "df.scaled",
                                        "cfi.scaled", "tli.scaled",
                                        "rmsea.scaled", "srmr"))
cat("=== AFC FINAL EN VALIDACION ===\n")
print(round(fit_val, 4)); cat("\n")

delta_cfi   <- fit_val["cfi.scaled"]   - fit_cal["cfi.scaled"]
delta_rmsea <- fit_val["rmsea.scaled"] - fit_cal["rmsea.scaled"]

cat("=== CONFIRMACION EN VALIDACION ===\n")
cat("Delta CFI:", round(delta_cfi, 4), "| Delta RMSEA:", round(delta_rmsea, 4), "\n")
cat("¿Se sostiene? (dCFI >= -0.010 y dRMSEA <= 0.015):",
    (delta_cfi >= -0.010) & (delta_rmsea <= 0.015), "\n\n")

# =====================================================================
# 10. REESTIMAR MODELO FINAL EN MUESTRA COMPLETA
# =====================================================================
args_final_full <- list(model = modelo_final, data = base,
                        ordered = setup$items_constructos,
                        estimator = setup$estimador)
if (isTRUE(setup$usar_diseno)) args_final_full$cluster <- ".cluster_entidad"
afc_final <- do.call(cfa, args_final_full)

cat("=== AFC FINAL EN MUESTRA COMPLETA ===\n")
cat("Convergio:", lavInspect(afc_final, "converged"), "\n")
fit_final <- fitMeasures(afc_final, c("chisq.scaled", "df.scaled",
                                      "pvalue.scaled", "cfi.scaled",
                                      "tli.scaled", "rmsea.scaled",
                                      "rmsea.ci.lower.scaled",
                                      "rmsea.ci.upper.scaled", "srmr"))
print(round(fit_final, 4)); cat("\n")

tabla_final <- tabla_ajuste_tidy(afc_final)
print(tabla_final, n = Inf); cat("\n")

# =====================================================================
# 11. DIAGRAMA DEL MODELO FINAL
# =====================================================================
if (!dir.exists(here("figuras"))) dir.create(here("figuras"))

png(here("figuras", "act9_afc_final.png"),
    width = 12, height = 8, units = "in", res = 150)
semPaths(afc_final, whatLabels = "std", layout = "tree", rotation = 2,
         edge.label.cex = 0.7, sizeMan = 6, sizeLat = 10,
         color = list(lat = "#D6EAF8", man = "#FDEBD0"))
dev.off()

# =====================================================================
# 12. EXPORTACION
# =====================================================================
tabla_decision <- tibble(
  Aspecto = c(
    "Residuos de correlacion |r| > 0.10",
    "Par con mayor IM",
    "IM del par",
    "EPC del par",
    "¿Intra-factor?",
    "¿Reespecificado?",
    "Justificacion",
    "Ajuste calibracion (CFI/TLI/RMSEA/SRMR)",
    "Ajuste validacion (CFI/TLI/RMSEA/SRMR)",
    "Delta CFI (val - cal)",
    "Delta RMSEA (val - cal)",
    "¿Se sostiene en validacion?"
  ),
  Valor = c(
    as.character(nrow(residuos_altos)),
    paste(par_top$lhs, "~~", par_top$rhs),
    round(par_top$mi, 2),
    round(par_top$epc, 3),
    as.character(justificable),
    as.character(reespecificado),
    ifelse(reespecificado,
           "Covarianza residual intra-factor con IM>10 y EPC>0.05; redaccion afin",
           "No hay par intra-factor con IM y EPC suficientes; se conserva el original"),
    paste(round(fit_cal[c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr")], 3),
          collapse = " / "),
    paste(round(fit_val[c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr")], 3),
          collapse = " / "),
    round(delta_cfi, 4),
    round(delta_rmsea, 4),
    as.character((delta_cfi >= -0.010) & (delta_rmsea <= 0.015))
  )
) %>% as_tibble()

cat("=== TABLA DE DECISION (A3) ===\n")
print(tabla_decision, n = Inf); cat("\n")

saveRDS(afc_final,    here("salidas", "afc_final.rds"))
saveRDS(modelo_final, here("salidas", "modelo_final.rds"))
saveRDS(fit_final,    here("salidas", "afc_final_fitmeasures.rds"))

write_xlsx(
  list(
    residuos_altos   = residuos_altos,
    modindices       = mi,
    par_top          = par_top,
    decision         = tabla_decision,
    ajuste_cal       = tabla_ajuste_tidy(afc_final_cal),
    ajuste_val       = tabla_ajuste_tidy(afc_final_val),
    ajuste_final     = tabla_final
  ),
  here("salidas", "Actividad9_Ajuste_Local.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad9_Ajuste_Local.xlsx\n")
cat("salidas/afc_final.rds\n")
cat("salidas/modelo_final.rds\n")
cat("salidas/afc_final_fitmeasures.rds\n")
cat("figuras/act9_afc_final.png\n\n")

sessionInfo()
