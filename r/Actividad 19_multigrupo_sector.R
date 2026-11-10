# =====================================================================
# ACTIVIDAD 19: Invarianza por sector (complemento B4/B5) — v11
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# CORRECCION v11 (segun revision de Claude):
#   1. Restaurar decision_Chen en la tabla de invarianza (v10 solo dejo deltas).
#   2. Modelo estructural con group.equal = c("thresholds", "loadings")
#      para que los coeficientes entre sectores sean comparables
#      (sin esto, cada sector tiene su propia metrica y el Wald
#      detecta diferencias de escala, no de relacion).
#   3. Version con y sin cluster del modelo estructural (sensibilidad).
#   4. Funcion Wald reutilizable con Bonferroni.
#   5. (Opcional) Bloque para identificar items que rompen invarianza escalar.
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(lavaan)
library(semTools)
library(writexl)

set.seed(2026)

# 1. CARGAR INSUMOS ----------------------------------------------------
setup     <- readRDS(here("salidas", "fase2_setup.rds"))
base      <- setup$base
items_all <- setup$items_constructos

cat("=== INSUMOS CARGADOS ===\n")
cat("Filas:", nrow(base), "| Items:", length(items_all), "\n\n")

min_n  <- 200
min_cl <- 10
has_cl <- ".cluster_entidad" %in% names(base)

# =====================================================================
# 2. BASE DE CASOS COMPLETOS Y VERIFICACION
# =====================================================================
cat("=== VERIFICACION DE SECTORES ===\n")

base_cc <- base %>%
  mutate(across(all_of(items_all), ~ ifelse(.x %in% 1:5, .x, NA))) %>%
  drop_na(all_of(c("CODSECTOR", items_all,
                   if (has_cl) ".cluster_entidad")))

sectores_unicos <- sort(unique(base_cc$CODSECTOR))

verificacion <- map_dfr(sectores_unicos, function(s) {
  sub <- base_cc %>% filter(CODSECTOR == s)
  items_ok <- sapply(items_all, function(it) {
    all(table(factor(sub[[it]], levels = 1:5)) > 0)
  })
  tibble(
    CODSECTOR          = s,
    n                  = nrow(sub),
    n_cluster          = if (has_cl) n_distinct(sub$.cluster_entidad) else NA_integer_,
    todos_items_ok     = all(items_ok),
    items_con_problema = paste(names(items_ok)[!items_ok], collapse = ", ")
  )
})

tabla_sector <- verificacion %>%
  mutate(
    incluir     = n >= min_n & todos_items_ok & (is.na(n_cluster) | n_cluster >= min_cl),
    pct_muestra = round(n / sum(n) * 100, 2)
  ) %>%
  arrange(desc(n))

print(tabla_sector, n = Inf)
cat("\n")

sectores_incluir     <- tabla_sector %>% filter(incluir) %>% pull(CODSECTOR)
sectores_excluir_n   <- tabla_sector %>% filter(!incluir, n < min_n) %>% pull(CODSECTOR)
sectores_excluir_cat <- tabla_sector %>% filter(!incluir, n >= min_n, !todos_items_ok) %>% pull(CODSECTOR)
sectores_excluir_cl  <- tabla_sector %>% filter(!incluir, n >= min_n, todos_items_ok,
                                                n_cluster < min_cl) %>% pull(CODSECTOR)

cat("Sectores incluidos:", length(sectores_incluir), "\n")
cat("Excluidos por n <", min_n, ":", length(sectores_excluir_n), "\n")
cat("Excluidos por categorias vacias:", length(sectores_excluir_cat), "\n")
cat("Excluidos por <", min_cl, "entidades:", length(sectores_excluir_cl), "\n\n")

stopifnot(length(sectores_incluir) >= 2)

base_sector <- base_cc %>%
  filter(CODSECTOR %in% sectores_incluir) %>%
  mutate(
    CODSECTOR = droplevels(factor(CODSECTOR)),
    across(all_of(items_all), ~ ordered(.x, levels = 1:5))
  )

stopifnot(all(sapply(items_all, function(it)
  all(table(base_sector$CODSECTOR, base_sector[[it]]) > 0))))

if (has_cl) {
  stopifnot(all(tapply(base_sector$.cluster_entidad,
                       base_sector$CODSECTOR,
                       function(x) n_distinct(x)) >= 2))
}

cat("Base limpia:", nrow(base_sector), "filas |",
    nlevels(base_sector$CODSECTOR), "grupos\n\n")

# =====================================================================
# 3. MODELO DE MEDICION
# =====================================================================
medicion <- '
  Liderazgo    =~ C21A + C21B + C21C + C21D + C21E +
                    C21F + C21G + C21H + C21I + C21J
  Clima        =~ C18A + C18B + C18C + C18D
  Satisfaccion =~ C03A + C03B + C03C + C03D + C03E
'

# =====================================================================
# 4. SECUENCIA DE INVARIANZA (Wu & Estabrook, 2016)
# =====================================================================
generar_sintaxis <- function(group_equal = NULL) {
  measEq.syntax(
    configural.model = medicion,
    data             = base_sector,
    ordered          = items_all,
    parameterization = "theta",
    ID.fac           = "std.lv",
    ID.cat           = "Wu.Estabrook.2016",
    group            = "CODSECTOR",
    group.equal      = group_equal,
    return.fit       = FALSE
  )
}

cat("=== GENERANDO SINTAXIS ===\n")
syn_config <- generar_sintaxis(NULL)
syn_thres  <- generar_sintaxis("thresholds")
syn_metric <- generar_sintaxis(c("thresholds", "loadings"))
syn_scalar <- generar_sintaxis(c("thresholds", "loadings", "intercepts"))
cat("OK.\n\n")

# =====================================================================
# 5. AJUSTAR MODELOS (CON parameterization = "theta")
# =====================================================================
cluster_arg <- if (has_cl) ".cluster_entidad" else NULL

ajustar <- function(sintaxis, nombre, usar_cluster = TRUE) {
  cat("Ajustando", nombre, "... ")
  fit <- cfa(
    model            = as.character(sintaxis),
    data             = base_sector,
    ordered          = items_all,
    estimator        = "WLSMV",
    parameterization = "theta",
    group            = "CODSECTOR",
    cluster          = if (usar_cluster) cluster_arg else NULL
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
# 6. COMPARACION FORMAL (lavTestLRT) + DECISION DE CHEN RESTAURADA
# =====================================================================
cat("=== COMPARACION FORMAL DE MODELOS ANIDADOS (lavTestLRT) ===\n")
lrt_inv <- tryCatch(
  lavTestLRT(fit_config, fit_thres, fit_metric, fit_scalar),
  error = function(e) { cat("Error:", conditionMessage(e), "\n"); NULL }
)
if (!is.null(lrt_inv)) { print(lrt_inv); cat("\n") }

extraer_ajuste <- function(fit, nombre) {
  fm <- fitMeasures(fit, c(
    "chisq.scaled", "df.scaled", "pvalue.scaled",
    "cfi.scaled", "tli.scaled",
    "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
    "srmr"
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
    SRMR      = round(fm["srmr"], 3)
  )
}

tabla_inv <- bind_rows(
  extraer_ajuste(fit_config, "Configural"),
  extraer_ajuste(fit_thres,  "Umbrales"),
  extraer_ajuste(fit_metric, "Metrica (cargas)"),
  extraer_ajuste(fit_scalar, "Escalar (interceptos)")
)

# DELTAS Y DECISION DE CHEN (restaurada)
tabla_inv <- tabla_inv %>%
  mutate(
    delta_CFI   = round(CFI - lag(CFI), 4),
    delta_RMSEA = round(RMSEA - lag(RMSEA), 4),
    delta_SRMR  = round(SRMR - lag(SRMR), 4),
    decision_Chen = case_when(
      is.na(delta_CFI) ~ "Referencia",
      delta_CFI >= -0.010 & delta_RMSEA <= 0.015 ~ "Se sostiene",
      TRUE ~ "No se sostiene"
    )
  )

cat("=== TABLA DE INVARIANZA CON DECISION DE CHEN ===\n")
print(tabla_inv %>% select(Modelo, CFI, RMSEA, SRMR,
                           delta_CFI, delta_RMSEA, delta_SRMR, decision_Chen),
      n = Inf)
cat("\n")

# =====================================================================
# 7. SENSIBILIDAD: SIN CLUSTER (verificar warning de vcov)
# =====================================================================
cat("=== SENSIBILIDAD: AJUSTE SIN CLUSTER ===\n")
fit_config_sc <- ajustar(syn_config, "Configural (sin cluster)", usar_cluster = FALSE)
fit_thres_sc  <- ajustar(syn_thres,  "Umbrales (sin cluster)",   usar_cluster = FALSE)
fit_metric_sc <- ajustar(syn_metric, "Metrica (sin cluster)",    usar_cluster = FALSE)
fit_scalar_sc <- ajustar(syn_scalar, "Escalar (sin cluster)",    usar_cluster = FALSE)
cat("\n")

lrt_inv_sc <- tryCatch(
  lavTestLRT(fit_config_sc, fit_thres_sc, fit_metric_sc, fit_scalar_sc),
  error = function(e) { cat("Error:", conditionMessage(e), "\n"); NULL }
)
if (!is.null(lrt_inv_sc)) { print(lrt_inv_sc); cat("\n") }

# =====================================================================
# 8. MODELO ESTRUCTURAL CON METRICA COMUN (group.equal = thresholds + loadings)
# =====================================================================
cat("=== MODELO ESTRUCTURAL CON METRICA COMUN ===\n")

n_grupos    <- nlevels(base_sector$CODSECTOR)
etiquetas_a <- paste0("a", seq_len(n_grupos))
etiquetas_b <- paste0("b", seq_len(n_grupos))
etiquetas_c <- paste0("c", seq_len(n_grupos))

modelo_est_sector <- paste0(medicion, '
  Clima        ~ c(', paste(etiquetas_a, collapse = ", "), ')*Liderazgo
  Satisfaccion ~ c(', paste(etiquetas_b, collapse = ", "), ')*Clima +
                   c(', paste(etiquetas_c, collapse = ", "), ')*Liderazgo
')

ajustar_est <- function(usar_cluster = TRUE) {
  sem(
    model            = modelo_est_sector,
    data             = base_sector,
    ordered          = items_all,
    estimator        = "WLSMV",
    parameterization = "theta",
    group            = "CODSECTOR",
    group.equal      = c("thresholds", "loadings"),   # <- metrica comun
    cluster          = if (usar_cluster) cluster_arg else NULL
  )
}

fit_est_sector    <- ajustar_est(TRUE)    # principal (con cluster)
fit_est_sector_sc <- ajustar_est(FALSE)   # sensibilidad (sin cluster)

cat("Convergio (con cluster):", lavInspect(fit_est_sector, "converged"), "\n")
cat("Convergio (sin cluster):", lavInspect(fit_est_sector_sc, "converged"), "\n\n")

# =====================================================================
# 9. PRUEBA DE WALD (funcion reutilizable, con y sin cluster)
# =====================================================================
wald_tab <- function(fit) {
  prueba <- function(et) tryCatch(
    lavTestWald(fit, constraints = paste0(et[1], " == ", et[-1], collapse = " ; ")),
    error = function(e) NULL
  )
  res <- lapply(list(a = etiquetas_a, b = etiquetas_b, c = etiquetas_c), prueba)
  extraer <- function(campo) map_dbl(res, ~ if (is.null(.x)) NA_real_ else unname(.x[[campo]]))
  tibble(
    parametro = names(res),
    chisq     = round(extraer("stat"), 3),
    gl        = extraer("df"),
    p         = round(extraer("p.value"), 4)
  ) %>%
    mutate(p_bonferroni = pmin(round(p * 3, 4), 1))
}

wald_tabla    <- wald_tab(fit_est_sector)
wald_tabla_sc <- wald_tab(fit_est_sector_sc)

cat("=== WALD (con cluster) ===\n")
print(wald_tabla, n = Inf)
cat("\n=== WALD (sin cluster, sensibilidad) ===\n")
print(wald_tabla_sc, n = Inf)
cat("\n")

# =====================================================================
# 10. COEFICIENTES POR SECTOR (con mapeo group -> CODSECTOR)
# =====================================================================
etq_grupos <- lavInspect(fit_est_sector, "group.label")

coefs_sector <- parameterEstimates(fit_est_sector, standardized = TRUE) %>%
  filter(op == "~") %>%
  mutate(CODSECTOR = etq_grupos[group]) %>%
  select(CODSECTOR, lhs, op, rhs, group, est, se, z, pvalue, std.all) %>%
  as_tibble()

cat("=== COEFICIENTES ESTRUCTURALES POR SECTOR (metrica comun) ===\n")
print(coefs_sector, n = Inf)
cat("\n")

# =====================================================================
# 11. MEDIAS OBSERVADAS POR SECTOR
# =====================================================================
cat("=== MEDIAS OBSERVADAS POR SECTOR ===\n")

base_punt <- base_cc %>%
  filter(CODSECTOR %in% sectores_incluir) %>%
  mutate(
    punt_liderazgo = rowMeans(across(all_of(setup$items_liderazgo)), na.rm = TRUE),
    punt_clima     = rowMeans(across(all_of(setup$items_clima)),     na.rm = TRUE),
    punt_satisf    = rowMeans(across(all_of(setup$items_satisf)),    na.rm = TRUE)
  )

medias_sector <- base_punt %>%
  group_by(CODSECTOR) %>%
  summarise(
    n         = n(),
    lid_media = round(mean(punt_liderazgo, na.rm = TRUE), 3),
    cli_media = round(mean(punt_clima,     na.rm = TRUE), 3),
    sat_media = round(mean(punt_satisf,    na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(sat_media))

cat("=== MEDIAS POR SECTOR ===\n")
print(medias_sector, n = Inf)
cat("\n")

rango_sector <- medias_sector %>%
  summarise(
    lid_min = min(lid_media), lid_max = max(lid_media),
    cli_min = min(cli_media), cli_max = max(cli_media),
    sat_min = min(sat_media), sat_max = max(sat_media)
  )
print(rango_sector, n = Inf)
cat("\n")

# =====================================================================
# 12. OPCIONAL: DONDE SE ROMPE LA INVARIANZA ESCALAR
# =====================================================================
cat("=== DONDE SE ROMPE LA INVARIANZA ESCALAR (opcional) ===\n")
score_test <- tryCatch(
  lavTestScore(fit_scalar_sc, epc = TRUE),
  error = function(e) { cat("Error:", conditionMessage(e), "\n"); NULL }
)
if (!is.null(score_test)) {
  uni <- as_tibble(score_test$uni) %>% arrange(desc(X2)) %>% slice_head(n = 15)
  print(uni, n = Inf)
  cat("\n")
}

# =====================================================================
# 13. EXPORTACION
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

saveRDS(list(
  config = fit_config, thres = fit_thres,
  metric = fit_metric, scalar = fit_scalar,
  config_sc = fit_config_sc, thres_sc = fit_thres_sc,
  metric_sc = fit_metric_sc, scalar_sc = fit_scalar_sc,
  est = fit_est_sector, est_sc = fit_est_sector_sc
), here("salidas", "invarianza_sector.rds"))

write_xlsx(
  list(
    sectores_incluidos = tabla_sector,
    tabla_ajuste       = tabla_inv,
    lrt_formal         = if (!is.null(lrt_inv)) {
      as.data.frame(lrt_inv) %>% rownames_to_column("modelo")
    } else tibble(),
    lrt_sin_cluster    = if (!is.null(lrt_inv_sc)) {
      as.data.frame(lrt_inv_sc) %>% rownames_to_column("modelo")
    } else tibble(),
    coeficientes       = coefs_sector,
    wald               = wald_tabla,
    wald_sin_cluster   = wald_tabla_sc,
    medias_por_sector  = medias_sector,
    rango_medias       = rango_sector,
    nota               = tibble(
      parameterization   = "theta (v11)",
      group_equal_est    = "thresholds + loadings (metrica comun)",
      sectores_incluidos = length(sectores_incluir),
      excluidos_n        = length(sectores_excluir_n),
      excluidos_cat      = length(sectores_excluir_cat),
      excluidos_cl       = length(sectores_excluir_cl),
      cobertura_pct      = round(sum(tabla_sector$n[tabla_sector$incluir]) /
                                   sum(tabla_sector$n) * 100, 2),
      criterio           = paste0("n >= ", min_n, ", 5 categorias, >= ",
                                  min_cl, " entidades"),
      secuencia          = "Wu & Estabrook (2016)"
    )
  ),
  here("salidas", "Actividad19_Invarianza_Sector_v11.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad19_Invarianza_Sector_v11.xlsx\n")
cat("salidas/invarianza_sector.rds\n\n")

sessionInfo()