# =====================================================================
# ACTIVIDAD 10: Confiabilidad y validez (A4 del Taller 2)
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (A4 del Taller 2):
#   - Cargas estandarizadas finales (modelo con C03D ~~ C03E).
#   - CR (omega) y AVE por constructo.
#   - Validez convergente: cargas >= 0.50, CR >= 0.70, AVE >= 0.50.
#   - Validez discriminante: Fornell-Larcker (sqrt(AVE) > correlaciones)
#     y HTMT < 0.85.
#   - COMPLEMENTO (observacion del profesor):
#       ICC por constructo (variacion entre entidades vs dentro).
#       Responde al punto 2: "mirar la calificacion entre entidades".
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(lavaan)
library(semTools)
library(psych)       # polychoric
library(writexl)
library(lme4)        # para ICC multinivel

# 1. CARGAR INSUMOS ----------------------------------------------------
setup     <- readRDS(here("salidas", "fase2_setup.rds"))
afc_final <- readRDS(here("salidas", "afc_final.rds"))
base      <- setup$base

items_lid <- setup$items_liderazgo
items_cli <- setup$items_clima
items_sat <- setup$items_satisf
items_all <- setup$items_constructos

cat("=== INSUMOS CARGADOS ===\n")
cat("Filas:", nrow(base), "| Items:", length(items_all), "\n")
cat("Modelo final con C03D ~~ C03E:", grepl("C03D ~~ C03E",
                                            readRDS(here("salidas", "modelo_final.rds"))), "\n\n")

# =====================================================================
# 1. CARGAS ESTANDARIZADAS FINALES
# =====================================================================
std_sol <- standardizedSolution(afc_final)
cargas <- std_sol %>%
  filter(op == "=~") %>%
  select(lhs, rhs, est.std, se, z, pvalue) %>%
  rename(factor = lhs, item = rhs, lambda = est.std) %>%
  mutate(
    significativa = ifelse(pvalue < 0.001, "***",
                           ifelse(pvalue < 0.01,  "**",
                                  ifelse(pvalue < 0.05,  "*", "n.s.")))
  ) %>%
  as_tibble()

cat("=== CARGAS ESTANDARIZADAS FINALES ===\n")
print(cargas, n = Inf)
cat("\n")

# Verificacion de validez convergente a nivel de item
cargas_debiles <- cargas %>% filter(abs(lambda) < 0.50)
if (nrow(cargas_debiles) == 0) {
  cat("OK: todas las cargas >= 0.50 (validez convergente a nivel de item).\n\n")
} else {
  cat("ATENCION: items con carga < 0.50:\n")
  print(cargas_debiles, n = Inf)
  cat("\n")
}

# =====================================================================
# 2. CR Y AVE POR CONSTRUCTO (con cargas y errores estandarizados)
# =====================================================================
# CR = (sum lambda)^2 / ((sum lambda)^2 + sum theta)
# AVE = mean(lambda^2)
# theta = 1 - lambda^2 (varianza de error estandarizada)
cr_ave <- cargas %>%
  mutate(theta = 1 - lambda^2) %>%
  group_by(factor) %>%
  summarise(
    k   = n(),
    CR  = round(sum(lambda)^2 / (sum(lambda)^2 + sum(theta)), 3),
    AVE = round(mean(lambda^2), 3),
    .groups = "drop"
  ) %>%
  mutate(
    CR_ok  = CR  >= 0.70,
    AVE_ok = AVE >= 0.50,
    convergencia_ok = CR_ok & AVE_ok
  ) %>%
  as_tibble()

cat("=== CR Y AVE POR CONSTRUCTO ===\n")
print(cr_ave, n = Inf)
cat("\n")

# =====================================================================
# 3. VALIDEZ DISCRIMINANTE: FORNELL-LARCKER
# =====================================================================
# sqrt(AVE) en la diagonal debe superar las correlaciones entre factores.
cor_lv <- lavInspect(afc_final, "cor.lv")
sqrt_ave <- sqrt(cr_ave$AVE)
names(sqrt_ave) <- cr_ave$factor

fl <- cor_lv
diag(fl) <- sqrt_ave[rownames(fl)]
fl_tabla <- as.data.frame(fl) %>%
  rownames_to_column("factor") %>%
  as_tibble()

cat("=== FORNELL-LARCKER (sqrt(AVE) en diagonal) ===\n")
print(fl_tabla, n = Inf)
cat("\n")

# Verificacion: sqrt(AVE) > correlaciones fuera de la diagonal
fl_ok <- TRUE
for (i in rownames(cor_lv)) {
  for (j in colnames(cor_lv)) {
    if (i != j && sqrt_ave[i] <= abs(cor_lv[i, j])) fl_ok <- FALSE
  }
}
cat("¿Se cumple Fornell-Larcker?:", fl_ok, "\n\n")

# =====================================================================
# 4. HTMT MANUAL DESDE POLICORICAS
# =====================================================================
# semTools::htmt falla o da resultados raros con WLSMV + ordered + cluster.
# HTMT manual (Henseler et al., 2015):
#   HTMT(F1,F2) = promedio de correlaciones heterorrasgo-heterometodo /
#                 promedio de correlaciones heterorrasgo-monometodo
# Con correlaciones policoricas (items ordinales).

poli <- psych::polychoric(base %>% select(all_of(items_all)))
R    <- poli$rho

# Funcion HTMT entre dos constructos (definicion de Henseler et al., 2015).
# NOTA (Correccion 5): no se usa abs(); la definicion original promedia las
# correlaciones heterorrasgo-heterometodo y las monorrasgo-heterometodo
# tal como se estiman (con su signo).
htmt_pair <- function(items_a, items_b, R) {
  # Heterorrasgo-heterometodo: entre items de A y B
  R_ab <- R[items_a, items_b]
  het_het <- mean(R_ab)

  # Monorrasgo-heterometodo dentro de A (fuera de diagonal)
  R_aa <- R[items_a, items_a]
  mono_a <- mean(R_aa[upper.tri(R_aa)])

  # Monorrasgo-heterometodo dentro de B
  R_bb <- R[items_b, items_b]
  mono_b <- mean(R_bb[upper.tri(R_bb)])

  # HTMT
  het_het / sqrt(mono_a * mono_b)
}

pares <- list(
  c("Liderazgo", "Clima"),
  c("Liderazgo", "Satisfaccion"),
  c("Clima", "Satisfaccion")
)
items_list <- list(
  Liderazgo    = items_lid,
  Clima        = items_cli,
  Satisfaccion = items_sat
)

htmt_tabla <- map_dfr(pares, function(p) {
  tibble(
    par  = paste(p[1], "-", p[2]),
    htmt = round(htmt_pair(items_list[[p[1]]], items_list[[p[2]]], R), 3)
  )
}) %>%
  mutate(
    criterio = "HTMT < 0.85",
    ok = htmt < 0.85
  ) %>%
  as_tibble()

cat("=== HTMT (manual desde policoricas) ===\n")
print(htmt_tabla, n = Inf)
cat("\n")

# =====================================================================
# 5. ICC POR CONSTRUCTO (entre entidades)
# =====================================================================
# Complemento observacion del profesor (punto 2):
# "Mirar la calificacion entre entidades".
# El ICC mide que proporcion de la varianza total se debe a diferencias
# ENTRE entidades. ICC alto => el cluster (entidad) importa.
#
# Se calcula con un modelo nulo (solo intercepto aleatorio por entidad)
# sobre el PUNTAJE PROMEDIO de cada constructo (media de items).

base_icc <- base %>%
  mutate(
    punt_liderazgo = rowMeans(select(., all_of(items_lid)), na.rm = TRUE),
    punt_clima     = rowMeans(select(., all_of(items_cli)), na.rm = TRUE),
    punt_satisf    = rowMeans(select(., all_of(items_sat)), na.rm = TRUE)
  ) %>%
  filter(!is.na(.cluster_entidad))

calcular_icc <- function(y, cluster, nombre) {
  df <- tibble(y = y, g = cluster) %>% filter(!is.na(y))
  m  <- lme4::lmer(y ~ 1 + (1 | g), data = df, REML = TRUE)
  # ICC = varianza entre entidades / (entre + dentro).
  # Se calcula desde VarCorr para no depender del paquete 'performance'.
  vc <- as.data.frame(lme4::VarCorr(m))
  var_g <- vc$vcov[vc$grp == "g"][1]
  var_e <- vc$vcov[vc$grp == "Residual"][1]
  icc_val <- var_g / (var_g + var_e)
  tibble(
    constructo = nombre,
    n_obs      = nrow(df),
    n_entidades = n_distinct(df$g),
    ICC        = round(icc_val, 4)
  )
}

icc_tabla <- bind_rows(
  calcular_icc(base_icc$punt_liderazgo, base_icc$.cluster_entidad, "Liderazgo"),
  calcular_icc(base_icc$punt_clima,     base_icc$.cluster_entidad, "Clima"),
  calcular_icc(base_icc$punt_satisf,    base_icc$.cluster_entidad, "Satisfaccion")
) %>%
  mutate(
    interpretacion = case_when(
      ICC < 0.05 ~ "Baja: entidad explica poco",
      ICC < 0.10 ~ "Moderada-baja",
      ICC < 0.20 ~ "Moderada",
      TRUE       ~ "Alta: fuerte efecto de entidad"
    )
  ) %>%
  as_tibble()

cat("=== ICC POR CONSTRUCTO (entre entidades) ===\n")
print(icc_tabla, n = Inf)
cat("\n")

# =====================================================================
# 6. EXPORTACION
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

# Guardar objetos para actividades posteriores
saveRDS(cargas,   here("salidas", "conf_cargas.rds"))
saveRDS(cr_ave,   here("salidas", "conf_cr_ave.rds"))
saveRDS(fl_tabla, here("salidas", "conf_fornell.rds"))
saveRDS(htmt_tabla, here("salidas", "conf_htmt.rds"))
saveRDS(icc_tabla, here("salidas", "conf_icc.rds"))
saveRDS(R,         here("salidas", "conf_policoricas.rds"))

write_xlsx(
  list(
    cargas          = cargas,
    cr_ave          = cr_ave,
    fornell_larcker = fl_tabla,
    htmt            = htmt_tabla,
    icc             = icc_tabla,
    cargas_debiles  = cargas_debiles
  ),
  here("salidas", "Actividad10_Confiabilidad_Validez.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad10_Confiabilidad_Validez.xlsx\n")
cat("salidas/conf_cargas.rds\n")
cat("salidas/conf_cr_ave.rds\n")
cat("salidas/conf_fornell.rds\n")
cat("salidas/conf_htmt.rds\n")
cat("salidas/conf_icc.rds\n")
cat("salidas/conf_policoricas.rds\n\n")

sessionInfo()