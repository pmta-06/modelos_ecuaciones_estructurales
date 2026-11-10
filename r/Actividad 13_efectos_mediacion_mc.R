# =====================================================================
# ACTIVIDAD 13: Efectos directo/indirecto/total con Monte Carlo (B2)
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (B2 del Taller 2):
#   - Calcular efectos directo, indirecto y total con IC.
#   - Usar metodo Monte Carlo (bootstrap parametrico) porque el
#     bootstrap no parametrico NO esta implementado para WLSMV en lavaan.
#
# JUSTIFICACION METODOLOGICA:
#   - lavaan no guarda replicas bootstrap con WLSMV (verificado:
#     length(m1_boot@boot$coef) = 0).
#   - semTools::monteCarloCI implementa el metodo de MacKinnon et al.
#     (2004) y Preacher & Selig (2012): simula parametros desde una
#     normal multivariada con media = coef(m1) y covarianza = vcov(m1),
#     y calcula IC percentil para las funciones definidas con :=.
#   - Es un bootstrap parametrico que RESPETA el WLSMV y el cluster.
#   - nRep = 20000 simulaciones.
#
# SALIDA REAL DE monteCarloCI() (verificada):
#   columnas: est, ci.lower, ci.upper  (NO incluye se)
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(lavaan)
library(semTools)   # monteCarloCI
library(writexl)

set.seed(2026)

# 1. CARGAR INSUMOS ----------------------------------------------------
setup <- readRDS(here("salidas", "fase2_setup.rds"))
m1    <- readRDS(here("salidas", "m1_fit.rds"))       # Act. 12
base  <- setup$base

cat("=== INSUMOS CARGADOS ===\n")
cat("Filas:", nrow(base), "| Entidades:", n_distinct(base$.cluster_entidad), "\n")
cat("Modelo M1 (WLSMV + cluster) cargado desde Actividad 12\n\n")

# =====================================================================
# 2. MONTE CARLO PARA EFECTOS (indirecto, directo, total)
# =====================================================================
cat("=== MONTE CARLO (20000 simulaciones) ===\n")
cat("Hora de inicio:", format(Sys.time(), "%H:%M:%S"), "\n\n")

t_inicio <- Sys.time()

ic_mc <- monteCarloCI(
  object = m1,
  nRep   = 20000,
  level  = 0.95
)

t_fin <- Sys.time()
tiempo_mc <- as.numeric(difftime(t_fin, t_inicio, units = "secs"))

cat("Monte Carlo completado en:", round(tiempo_mc, 1), "segundos\n\n")

# Diagnostico de estructura
cat("=== ESTRUCTURA DE ic_mc ===\n")
cat("Columnas:", paste(names(ic_mc), collapse = ", "), "\n")
print(ic_mc)
cat("\n")

# =====================================================================
# 3. FORMATEAR RESULTADOS
# =====================================================================
# ic_mc SOLO tiene: est, ci.lower, ci.upper (sin se).
efectos_mc <- as_tibble(ic_mc, rownames = "efecto") %>%
  select(efecto,
         estimacion = est,
         ic95_inf   = ci.lower,
         ic95_sup   = ci.upper) %>%
  mutate(
    significativo = ifelse(ic95_inf > 0 | ic95_sup < 0, "Si", "No"),
    interpretacion = case_when(
      efecto == "indirecto" ~ "Efecto indirecto (a*b)",
      efecto == "directo"   ~ "Efecto directo (c)",
      efecto == "total"     ~ "Efecto total (c + a*b)",
      TRUE                  ~ efecto
    )
  ) %>%
  select(efecto, interpretacion, estimacion, ic95_inf, ic95_sup, significativo)

cat("=== EFECTOS CON IC MONTE CARLO (95%, 20000 simulaciones) ===\n")
print(efectos_mc, n = Inf)
cat("\n")

# =====================================================================
# 4. COMPARACION CON IC DELTA (Actividad 12)
# =====================================================================
# efectos_delta viene de parameterEstimates(m1), que SI tiene se.
efectos_delta <- parameterEstimates(m1, standardized = TRUE) %>%
  filter(op == ":=") %>%
  select(efecto          = lhs,
         estimacion_delta = est,
         ee_delta         = se,
         ic95_inf_delta   = ci.lower,
         ic95_sup_delta   = ci.upper) %>%
  as_tibble()

comparacion <- efectos_mc %>%
  left_join(efectos_delta, by = "efecto") %>%
  mutate(
    dif_ic_inf     = round(ic95_inf - ic95_inf_delta, 4),
    dif_ic_sup     = round(ic95_sup - ic95_sup_delta, 4),
    amplitud_mc    = round(ic95_sup - ic95_inf, 4),
    amplitud_delta = round(ic95_sup_delta - ic95_inf_delta, 4)
  )

cat("=== COMPARACION: MONTE CARLO vs DELTA (solo IC) ===\n")
print(comparacion, n = Inf)
cat("\n")

# =====================================================================
# 5. EXPORTACION
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

saveRDS(efectos_mc,  here("salidas", "efectos_montecarlo.rds"))
saveRDS(comparacion, here("salidas", "efectos_mc_vs_delta.rds"))

write_xlsx(
  list(
    efectos_montecarlo = efectos_mc,
    efectos_delta      = efectos_delta,
    comparacion        = comparacion,
    metadatos          = tibble(
      metodo        = "Monte Carlo (semTools::monteCarloCI)",
      simulaciones  = 20000,
      nivel_ic      = 0.95,
      tipo_ic       = "Percentil",
      modelo_base   = "WLSMV + cluster (Actividad 12)",
      justificacion = "Bootstrap no parametrico no disponible para WLSMV en lavaan; Monte Carlo es bootstrap parametrico que respeta el estimador"
    )
  ),
  here("salidas", "Actividad13_Efectos_Mediacion_MC.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad13_Efectos_Mediacion_MC.xlsx\n")
cat("salidas/efectos_montecarlo.rds\n")
cat("salidas/efectos_mc_vs_delta.rds\n\n")

cat("=== NOTA PARA EL ARTICULO ===\n")
cat("Los intervalos de confianza del efecto indirecto se obtuvieron\n")
cat("mediante el metodo Monte Carlo (20000 simulaciones), que genera\n")
cat("una distribucion empirica de los parametros a partir de coef(m1)\n")
cat("y vcov(m1) del modelo WLSMV con conglomerados. Se reportan IC\n")
cat("percentil al 95%. Este metodo es un bootstrap parametrico que\n")
cat("respeta el estimador original y es recomendado cuando el bootstrap\n")
cat("no parametrico no esta disponible (MacKinnon et al., 2004;\n")
cat("Preacher & Selig, 2012).\n")

sessionInfo()