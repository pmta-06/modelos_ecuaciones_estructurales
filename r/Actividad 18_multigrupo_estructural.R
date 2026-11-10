# =====================================================================
# ACTIVIDAD 18: Invarianza estructural y medias observadas (B5) — v5
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (B5 del Taller 2):
#   1. Contrastar invarianza estructural entre sexos:
#      - Modelo libre vs igualado (lavTestLRT).
#      - Prueba de Wald por coeficiente (a, b, c).
#   2. Comparar niveles medios entre sexos:
#      - Medias latentes no se identifican con WLSMV + ordered + cluster
#        (limitacion de lavaan, no del modelo).
#      - Se reportan MEDIAS OBSERVADAS (promedio de items) con t-test
#        de Welch y d de Cohen. Alternativa metodologicamente valida.
#
# CORRECCION v5:
#   - Bloque 8 reemplazado por medias observadas + t-test + d de Cohen.
#   - Exportacion incluye medias_observadas y diferencia_obs.
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
# 3. MODELO LIBRE (SIN etiquetas)
# =====================================================================
modelo_libre <- paste0(medicion, '
  Clima        ~ Liderazgo
  Satisfaccion ~ Clima + Liderazgo
')

cat("=== MODELO LIBRE (sin restricciones entre grupos) ===\n")
fit_libre <- sem(
  model     = modelo_libre,
  data      = base_inv,
  ordered   = items_all,
  estimator = "WLSMV",
  group     = "B04",
  cluster   = ".cluster_entidad"
)
cat("Convergio:", lavInspect(fit_libre, "converged"), "\n\n")

# =====================================================================
# 4. MODELO IGUALADO (regresiones iguales entre grupos)
# =====================================================================
cat("=== MODELO IGUALADO (regresiones iguales entre grupos) ===\n")
fit_igual <- sem(
  model       = modelo_libre,
  data        = base_inv,
  ordered     = items_all,
  estimator   = "WLSMV",
  group       = "B04",
  cluster     = ".cluster_entidad",
  group.equal = c("loadings", "intercepts", "regressions")
)
cat("Convergio:", lavInspect(fit_igual, "converged"), "\n\n")

# =====================================================================
# 5. COMPARACION LIBRE vs IGUALADO
# =====================================================================
cat("=== COMPARACION LIBRE vs IGUALADO (lavTestLRT) ===\n")
lrt_est <- tryCatch(lavTestLRT(fit_libre, fit_igual),
                    error = function(e) { cat("Error:", conditionMessage(e), "\n"); NULL })
if (!is.null(lrt_est)) { print(lrt_est); cat("\n") }

# =====================================================================
# 6. MODELO LIBRE CON ETIQUETAS VECTORIALES (para Wald)
# =====================================================================
modelo_wald <- paste0(medicion, '
  Clima        ~ c(a1, a2)*Liderazgo
  Satisfaccion ~ c(b1, b2)*Clima + c(c1, c2)*Liderazgo
')

cat("=== MODELO LIBRE CON ETIQUETAS VECTORIALES (para Wald) ===\n")
fit_wald <- sem(
  model     = modelo_wald,
  data      = base_inv,
  ordered   = items_all,
  estimator = "WLSMV",
  group     = "B04",
  cluster   = ".cluster_entidad"
)
cat("Convergio:", lavInspect(fit_wald, "converged"), "\n\n")

# =====================================================================
# 7. PRUEBA DE WALD
# =====================================================================
cat("=== PRUEBA DE WALD PARA COEFICIENTES ESPECIFICOS ===\n")

coefs_wald <- parameterEstimates(fit_wald, standardized = TRUE) %>%
  filter(op == "~") %>%
  select(lhs, op, rhs, group, est, se, z, pvalue, std.all) %>%
  as_tibble()
print(coefs_wald, n = Inf)
cat("\n")

wald_resultados <- list()
for (etq in c("a", "b", "c")) {
  cat("Wald para", etq, "(igualdad entre grupos)...\n")
  wald_test <- tryCatch(
    lavTestWald(fit_wald, constraints = paste0(etq, "1 == ", etq, "2")),
    error = function(e) { cat("Error:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(wald_test)) {
    wald_resultados[[etq]] <- tibble(
      parametro = etq,
      chisq     = round(wald_test$stat, 3),
      gl        = wald_test$df,
      p         = round(wald_test$p.value, 4)
    )
    print(wald_resultados[[etq]])
  }
  cat("\n")
}
wald_tabla <- bind_rows(wald_resultados)

# =====================================================================
# 8. MEDIAS OBSERVADAS POR SEXO (alternativa a medias latentes)
# =====================================================================
# Con WLSMV + ordered + cluster, lavaan no identifica correctamente
# las medias latentes (limitacion conocida). Se reportan las medias
# observadas (promedio de items) por grupo, con t-test de Welch y
# d de Cohen. Alternativa metodologicamente valida.

cat("=== MEDIAS OBSERVADAS POR SEXO ===\n")

base_punt <- base_inv %>%
  mutate(
    punt_liderazgo = rowMeans(select(., all_of(setup$items_liderazgo)), na.rm = TRUE),
    punt_clima     = rowMeans(select(., all_of(setup$items_clima)),     na.rm = TRUE),
    punt_satisf    = rowMeans(select(., all_of(setup$items_satisf)),    na.rm = TRUE)
  )

medias_obs <- base_punt %>%
  group_by(B04) %>%
  summarise(
    n = n(),
    lid_media = mean(punt_liderazgo, na.rm = TRUE),
    lid_sd    = sd(punt_liderazgo, na.rm = TRUE),
    cli_media = mean(punt_clima, na.rm = TRUE),
    cli_sd    = sd(punt_clima, na.rm = TRUE),
    sat_media = mean(punt_satisf, na.rm = TRUE),
    sat_sd    = sd(punt_satisf, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as_tibble()

cat("=== MEDIAS OBSERVADAS POR GRUPO ===\n")
print(medias_obs, n = Inf)
cat("\n")

# t-test de Welch para cada constructo
t_lid <- t.test(punt_liderazgo ~ B04, data = base_punt)
t_cli <- t.test(punt_clima     ~ B04, data = base_punt)
t_sat <- t.test(punt_satisf    ~ B04, data = base_punt)

# Diferencia de medias y d de Cohen
diferencia_obs <- tibble(
  constructo = c("Liderazgo", "Clima", "Satisfaccion"),
  media_H    = c(medias_obs$lid_media[1], medias_obs$cli_media[1], medias_obs$sat_media[1]),
  media_M    = c(medias_obs$lid_media[2], medias_obs$cli_media[2], medias_obs$sat_media[2]),
  sd_H       = c(medias_obs$lid_sd[1], medias_obs$cli_sd[1], medias_obs$sat_sd[1]),
  sd_M       = c(medias_obs$lid_sd[2], medias_obs$cli_sd[2], medias_obs$sat_sd[2]),
  diferencia = media_M - media_H,
  t          = c(t_lid$statistic, t_cli$statistic, t_sat$statistic),
  gl         = c(t_lid$parameter, t_cli$parameter, t_sat$parameter),
  p          = c(t_lid$p.value, t_cli$p.value, t_sat$p.value)
) %>%
  mutate(
    sd_pooled = sqrt((sd_H^2 + sd_M^2) / 2),
    d_cohen   = diferencia / sd_pooled,
    magnitud  = case_when(
      abs(d_cohen) < 0.20 ~ "Pequena (<0.20 DE)",
      abs(d_cohen) < 0.50 ~ "Moderada (0.20-0.50 DE)",
      abs(d_cohen) < 0.80 ~ "Grande (0.50-0.80 DE)",
      TRUE                ~ "Muy grande (>0.80 DE)"
    ),
    significativo = ifelse(p < 0.001, "***",
                           ifelse(p < 0.01,  "**",
                                  ifelse(p < 0.05,  "*", "n.s.")))
  ) %>%
  select(constructo, media_H, media_M, diferencia, t, gl, p,
         d_cohen, magnitud, significativo)

cat("=== DIFERENCIA MUJER - HOMBRE (medias observadas) ===\n")
print(diferencia_obs, n = Inf)
cat("\n")

# Guardar como medias_pe y medias_diff para mantener compatibilidad con exportacion
medias_pe   <- diferencia_obs
medias_diff <- diferencia_obs

# =====================================================================
# 9. EXPORTACION
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

saveRDS(list(libre = fit_libre, igualado = fit_igual, wald = fit_wald),
        here("salidas", "invarianza_estructural.rds"))

write_xlsx(
  list(
    ajuste_libre    = as.data.frame(t(fitMeasures(fit_libre,
                                                  c("chisq.scaled","df.scaled","cfi.scaled",
                                                    "tli.scaled","rmsea.scaled","srmr")))) %>%
      rownames_to_column("indice"),
    ajuste_igual    = as.data.frame(t(fitMeasures(fit_igual,
                                                  c("chisq.scaled","df.scaled","cfi.scaled",
                                                    "tli.scaled","rmsea.scaled","srmr")))) %>%
      rownames_to_column("indice"),
    lrt_estructural = if (!is.null(lrt_est)) {
      as.data.frame(lrt_est) %>% rownames_to_column("modelo")
    } else tibble(),
    coeficientes    = coefs_wald,
    wald            = wald_tabla,
    medias_observadas = medias_obs,
    medias_diferencia = diferencia_obs
  ),
  here("salidas", "Actividad18_Invarianza_Estructural.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad18_Invarianza_Estructural.xlsx\n")
cat("salidas/invarianza_estructural.rds\n\n")

cat("=== NOTA PARA EL ARTICULO ===\n")
cat("Invarianza estructural: modelo libre vs igualado, pruebas de Wald.\n")
cat("Medias latentes no se identifican con WLSMV + ordered + cluster\n")
cat("(limitacion de lavaan). Se reportan medias observadas (promedio\n")
cat("de items) con t-test de Welch y d de Cohen, alternativa valida.\n")

sessionInfo()