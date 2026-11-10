# =====================================================================
# ACTIVIDAD 17: Invarianza de medicion por sexo (B4 del Taller 2) — v5
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (B4 del Taller 2):
#   - Evaluar invarianza de medicion entre sexos (B04).
#   - Secuencia Wu & Estabrook (2016) para indicadores ORDINALES.
#
# CORRECCION v5 (Correccion 3 del harness):
#   1. Se agrega cluster = ".cluster_entidad" a TODOS los modelos, igual que
#      en las Actividades 8, 12 y 18. Antes los indices y EE no eran
#      comparables entre actividades.
#   2. Se elimina el nivel "Estricta": con parametrizacion theta los
#      residuales ya estan fijos (1), asi que el modelo estricto era
#      identico al escalar (Delta = 0) y no probaba nada. Se documenta como
#      "no probado" en la hoja de nota.
#   3. Delta SRMR: 0.030 para metrica y 0.010 para escalar (Chen, 2007),
#      en lugar de 0.030 para todos los niveles.
#   4. Indices ESCALADOS (*.scaled) como criterio, homogeneos con 8, 9, 12
#      y 19; se agregan las versiones robustas como columna informativa.
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(lavaan)
library(semTools)
library(writexl)

set.seed(2026)

# 1. CARGAR INSUMOS ----------------------------------------------------
setup <- readRDS(here("salidas", "fase2_setup.rds"))
base  <- setup$base
items_all <- setup$items_constructos

base_inv <- base %>%
  filter(!is.na(B04)) %>%
  mutate(B04 = factor(B04, levels = c(1, 2), labels = c("Hombre", "Mujer")))

cat("=== INSUMOS CARGADOS ===\n")
cat("Total con B04 valido:", nrow(base_inv), "\n")
print(table(base_inv$B04))
cat("\n")

# =====================================================================
# 2. MODELO DE MEDICION
# =====================================================================
medicion <- '
  Liderazgo    =~ C21A + C21B + C21C + C21D + C21E +
                  C21F + C21G + C21H + C21I + C21J
  Clima        =~ C18A + C18B + C18C + C18D
  Satisfaccion =~ C03A + C03B + C03C + C03D + C03E
'

# =====================================================================
# 3. GENERAR SINTAXIS CON measEq.syntax (return.fit = FALSE)
# =====================================================================
generar_sintaxis <- function(group_equal = NULL) {
  measEq.syntax(
    configural.model = medicion,
    data             = base_inv,
    ordered          = items_all,
    parameterization = "theta",
    ID.fac           = "std.lv",
    ID.cat           = "Wu.Estabrook.2016",
    group            = "B04",
    group.equal      = group_equal,
    return.fit       = FALSE
  )
}

cat("=== GENERANDO SINTAXIS ===\n")
syn_config <- generar_sintaxis(NULL)
syn_thres  <- generar_sintaxis("thresholds")
syn_metric <- generar_sintaxis(c("thresholds", "loadings"))
syn_scalar <- generar_sintaxis(c("thresholds", "loadings", "intercepts"))
cat("Sintaxis generada para 4 niveles: configural, umbrales, metrica, escalar.\n\n")

# =====================================================================
# 4. AJUSTAR CADA MODELO CON cfa() (con cluster)
# =====================================================================
ajustar <- function(sintaxis, nombre) {
  cat("Ajustando", nombre, "... ")
  fit <- cfa(
    model     = as.character(sintaxis),
    data      = base_inv,
    ordered   = items_all,
    estimator = "WLSMV",
    group     = "B04",
    cluster   = ".cluster_entidad"
  )
  cat("convergio:", lavInspect(fit, "converged"), "\n")
  fit
}

fit_config <- ajustar(syn_config, "Configural")
fit_thres  <- ajustar(syn_thres,  "Umbrales")
fit_metric <- ajustar(syn_metric, "Metrica")
fit_scalar <- ajustar(syn_scalar, "Escalar")
cat("\n")

# =====================================================================
# 5. TABLA COMPARATIVA DE AJUSTE (escalados + robustos)
# =====================================================================
extraer_ajuste_inv <- function(fit, nombre) {
  fm <- fitMeasures(fit, c(
    "chisq.scaled", "df.scaled", "pvalue.scaled",
    "cfi.scaled", "tli.scaled",
    "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
    "srmr",
    "cfi.robust", "rmsea.robust"
  ))
  tibble(
    Modelo    = nombre,
    chi2      = round(fm["chisq.scaled"], 2),
    gl        = round(fm["df.scaled"], 0),
    p         = round(fm["pvalue.scaled"], 4),
    CFI       = round(fm["cfi.scaled"], 3),
    TLI       = round(fm["tli.scaled"], 3),
    RMSEA     = round(fm["rmsea.scaled"], 3),
    RMSEA_inf = round(fm["rmsea.ci.lower.scaled"], 3),
    RMSEA_sup = round(fm["rmsea.ci.upper.scaled"], 3),
    SRMR      = round(fm["srmr"], 3),
    CFI_robusto   = round(fm["cfi.robust"], 3),
    RMSEA_robusto = round(fm["rmsea.robust"], 3)
  )
}

tabla_inv <- bind_rows(
  extraer_ajuste_inv(fit_config, "Configural"),
  extraer_ajuste_inv(fit_thres,  "Umbrales"),
  extraer_ajuste_inv(fit_metric, "Metrica (cargas)"),
  extraer_ajuste_inv(fit_scalar, "Escalar (interceptos)")
)

cat("=== TABLA DE INVARIANZA (indices *.scaled; *.robust informativos) ===\n")
print(tabla_inv, n = Inf)
cat("\n")

# Contraste formal entre niveles (diferencia escalada de chi-cuadrado).
# Con N grande el contraste de chi-cuadrado suele rechazar aunque los indices
# aproximados no se deterioren; por eso la decision se basa en Delta CFI,
# Delta RMSEA y Delta SRMR (Cheung & Rensvold, 2002; Chen, 2007).
lrt_inv <- tryCatch(
  lavTestLRT(fit_config, fit_thres, fit_metric, fit_scalar),
  error = function(e) { cat("lavTestLRT no disponible:", conditionMessage(e), "\n"); NULL }
)
if (!is.null(lrt_inv)) {
  cat("=== CONTRASTE FORMAL ENTRE NIVELES (lavTestLRT) ===\n")
  print(lrt_inv)
  cat("\n")
}

# =====================================================================
# 6. DELTAS Y CRITERIOS (Chen, 2007)
# =====================================================================
# Delta SRMR: 0.030 para invarianza metrica y 0.010 para la escalar.
tabla_inv <- tabla_inv %>%
  mutate(
    delta_CFI   = round(CFI - lag(CFI), 4),
    delta_RMSEA = round(RMSEA - lag(RMSEA), 4),
    delta_SRMR  = round(SRMR - lag(SRMR), 4),
    criterio_SRMR_umbral = c(NA, 0.030, 0.030, 0.010),
    # Diferencia escalada de chi-cuadrado frente al nivel anterior
    delta_chi2 = if (!is.null(lrt_inv)) {
      c(NA, round(as.numeric(lrt_inv$`Chisq diff`[2:4]), 2))
    } else NA_real_,
    delta_gl_lrt = if (!is.null(lrt_inv)) {
      c(NA, as.numeric(lrt_inv$`Df diff`[2:4]))
    } else NA_real_,
    p_lrt = if (!is.null(lrt_inv)) {
      c(NA, round(as.numeric(lrt_inv$`Pr(>Chisq)`[2:4]), 4))
    } else NA_real_
  )

cat("=== DELTAS RESPECTO AL MODELO ANTERIOR ===\n")
print(tabla_inv %>% select(Modelo, CFI, RMSEA, SRMR,
                           delta_CFI, delta_RMSEA, delta_SRMR,
                           criterio_SRMR_umbral), n = Inf)
cat("\n")

tabla_inv <- tabla_inv %>%
  mutate(
    criterio_CFI   = ifelse(is.na(delta_CFI), NA_character_,
                            ifelse(delta_CFI >= -0.010, "OK", "FALLA")),
    criterio_RMSEA = ifelse(is.na(delta_RMSEA), NA_character_,
                            ifelse(delta_RMSEA <= 0.015, "OK", "FALLA")),
    criterio_SRMR  = ifelse(is.na(delta_SRMR), NA_character_,
                            ifelse(delta_SRMR <= criterio_SRMR_umbral,
                                   "OK", "FALLA")),
    decision = case_when(
      is.na(criterio_CFI) ~ "Referencia",
      criterio_CFI == "OK" & criterio_RMSEA == "OK" &
        criterio_SRMR == "OK" ~ "Se sostiene",
      TRUE ~ "No se sostiene"
    )
  )

cat("=== CRITERIOS Y DECISION ===\n")
print(tabla_inv %>% select(Modelo, criterio_CFI, criterio_RMSEA,
                           criterio_SRMR, decision), n = Inf)
cat("\n")

# =====================================================================
# 7. INVARIANZA PARCIAL (si algun nivel falla)
# =====================================================================
nivel_falla <- tabla_inv %>%
  filter(decision == "No se sostiene") %>%
  pull(Modelo)

if (length(nivel_falla) > 0) {
  cat("=== INVARIANZA PARCIAL ===\n")
  cat("Nivel(es) que fallan:", paste(nivel_falla, collapse = ", "), "\n")

  score_test <- tryCatch(
    lavTestScore(fit_scalar),
    error = function(e) { cat("Error en lavTestScore:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(score_test)) {
    cat("=== lavTestScore (primeros 10) ===\n")
    print(head(score_test$uni, 10))
    cat("\n")
  }
} else {
  cat("=== INVARIANZA PARCIAL ===\n")
  cat("No se requiere: los niveles configural, umbrales, metrico y escalar se sostienen.\n\n")
}

# =====================================================================
# 8. EXPORTACION
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

saveRDS(list(config = fit_config, thres = fit_thres,
             metric = fit_metric, scalar = fit_scalar),
        here("salidas", "invarianza_medicion.rds"))

saveRDS(tabla_inv, here("salidas", "invarianza_medicion_tabla.rds"))

write_xlsx(
  list(
    tabla_invarianza = tabla_inv,
    deltas           = tabla_inv %>% select(Modelo, delta_chi2, delta_gl_lrt,
                                            p_lrt, delta_CFI, delta_RMSEA,
                                            delta_SRMR, criterio_SRMR_umbral,
                                            decision),
    lrt              = if (!is.null(lrt_inv)) {
      as.data.frame(lrt_inv) %>% rownames_to_column("modelo")
    } else tibble(),
    nota             = tibble(
      secuencia      = "Wu & Estabrook (2016)",
      cluster        = ".cluster_entidad (errores robustos a conglomeracion)",
      criterios      = "Chen (2007): dCFI >= -0.010, dRMSEA <= 0.015, dSRMR <= 0.030 (metrica) y <= 0.010 (escalar)",
      indices        = "*.scaled como criterio; *.robust informativos",
      grupo          = "B04 (sexo: Hombre vs Mujer)",
      nivel_estricto = "No probado: con parametrizacion theta los residuales ya estan fijos en 1, por lo que el modelo estricto es identico al escalar"
    )
  ),
  here("salidas", "Actividad17_Invarianza_Medicion.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad17_Invarianza_Medicion.xlsx\n")
cat("salidas/invarianza_medicion.rds\n")
cat("salidas/invarianza_medicion_tabla.rds\n\n")

sessionInfo()
