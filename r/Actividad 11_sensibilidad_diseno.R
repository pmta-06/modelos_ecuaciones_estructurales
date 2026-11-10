# =====================================================================
# ACTIVIDAD 11: Sensibilidad del diseno muestral (complemento A1/A2)
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (observacion del profesor, punto 1):
#   "Encuesta, factores de expansion, cuantas entidades".
#
# Que hace:
#   1. Reporta el diseno de la encuesta: n entidades, FEXPFINAL,
#      estratos CODSECTOR/CODGRUPO.
#   2. AFC con SOLO cluster (referencia, ya estimado en Act. 8).
#   3. AFC con SOLO sampling.weights (sin cluster).
#   4. Compara cargas y ajuste entre ambas especificaciones.
#   5. Documenta la limitacion de lavaan (WLSMV no admite pesos +
#      cluster simultaneamente) y de lavaan.survey (solo continuas).
#
# NOTA: lavaan.survey NO se usa porque solo implementa variables
# continuas y estimadores ML; nuestro modelo es WLSMV + ordered.
# Se documenta en limitaciones del articulo.
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(lavaan)
library(semTools)
library(writexl)

# 1. CARGAR INSUMOS ----------------------------------------------------
setup     <- readRDS(here("salidas", "fase2_setup.rds"))
afc_cluster <- readRDS(here("salidas", "afc_fit.rds"))   # Act. 8
base      <- setup$base

items_all <- setup$items_constructos
modelo    <- setup$modelo_medicion

cat("=== INSUMOS CARGADOS ===\n")
cat("Filas:", nrow(base), "| Items:", length(items_all), "\n")
cat("Modelo: AFC saturado (3 factores correlacionados)\n\n")

# =====================================================================
# 1. REPORTE DEL DISENO DE LA ENCUESTA (punto 1 del profesor)
# =====================================================================
cat("=== DISENO DE LA ENCUESTA (EDI 2025) ===\n")

# Entidades
n_entidades <- n_distinct(base$.cluster_entidad)
cat("Entidades unicas (cluster):", n_entidades, "\n")

# Factor de expansion
peso_summary <- base %>%
  summarise(
    n      = n(),
    media  = round(mean(.peso_exp), 3),
    sd     = round(sd(.peso_exp), 3),
    min    = round(min(.peso_exp), 3),
    q25    = round(quantile(.peso_exp, 0.25), 3),
    mediana = round(median(.peso_exp), 3),
    q75    = round(quantile(.peso_exp, 0.75), 3),
    max    = round(max(.peso_exp), 3),
    suma   = round(sum(.peso_exp), 1)
  )
cat("\nFactor de expansion (FEXPFINAL):\n")
print(peso_summary, n = Inf)

# Estratos: CODSECTOR y CODGRUPO
if ("CODSECTOR" %in% names(base)) {
  sectores <- base %>% count(CODSECTOR, name = "n") %>%
    arrange(desc(n)) %>% as_tibble()
  cat("\nSectores (CODSECTOR):", nrow(sectores), "categorias\n")
  print(head(sectores, 10), n = Inf)
}

if ("CODGRUPO" %in% names(base)) {
  grupos <- base %>% count(CODGRUPO, name = "n") %>%
    arrange(desc(n)) %>% as_tibble()
  cat("\nGrupos funcionales (CODGRUPO):", nrow(grupos), "categorias\n")
  print(head(grupos, 10), n = Inf)
}

# Tamano de entidades (para contextualizar el cluster)
tam_ent <- base %>% count(.cluster_entidad, name = "n_servidores") %>%
  summarise(
    min = min(n_servidores),
    q25 = quantile(n_servidores, 0.25),
    mediana = median(n_servidores),
    q75 = quantile(n_servidores, 0.75),
    max = max(n_servidores),
    media = round(mean(n_servidores), 1)
  )
cat("\nTamano de las entidades (servidores por entidad):\n")
print(tam_ent, n = Inf)
cat("\n")

# =====================================================================
# 2. AFC CON SOLO CLUSTER (REFERENCIA, ya estimado en Act. 8)
# =====================================================================
cat("=== AFC CON SOLO CLUSTER (referencia Act. 8) ===\n")
fit_cluster <- fitMeasures(afc_cluster, c(
  "chisq.scaled", "df.scaled", "pvalue.scaled",
  "cfi.scaled", "tli.scaled",
  "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
  "srmr"
))
print(round(fit_cluster, 4))
cat("\n")

cargas_cluster <- standardizedSolution(afc_cluster) %>%
  filter(op == "=~") %>%
  select(factor = lhs, item = rhs, lambda_cluster = est.std) %>%
  as_tibble()

# =====================================================================
# 3. AFC CON SOLO PESOS (sampling.weights, sin cluster)
# =====================================================================
cat("=== AFC CON SOLO PESOS (sin cluster) ===\n")

afc_pesos <- cfa(
  model            = modelo,
  data             = base,
  ordered          = items_all,
  estimator        = "WLSMV",
  sampling.weights = ".peso_exp"
  # SIN cluster
)

cat("Convergio:", lavInspect(afc_pesos, "converged"), "\n")
cat("Iteraciones:", lavInspect(afc_pesos, "iterations"), "\n\n")

fit_pesos <- fitMeasures(afc_pesos, c(
  "chisq.scaled", "df.scaled", "pvalue.scaled",
  "cfi.scaled", "tli.scaled",
  "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
  "srmr"
))
print(round(fit_pesos, 4))
cat("\n")

cargas_pesos <- standardizedSolution(afc_pesos) %>%
  filter(op == "=~") %>%
  select(factor = lhs, item = rhs, lambda_pesos = est.std) %>%
  as_tibble()

# =====================================================================
# 4. COMPARACION DE CARGAS ENTRE ESPECIFICACIONES
# =====================================================================
comparacion_cargas <- cargas_cluster %>%
  left_join(cargas_pesos, by = c("factor", "item")) %>%
  mutate(
    diferencia = round(lambda_pesos - lambda_cluster, 4),
    dif_abs    = round(abs(diferencia), 4)
  ) %>%
  arrange(desc(dif_abs)) %>%
  as_tibble()

cat("=== COMPARACION DE CARGAS (cluster vs pesos) ===\n")
print(comparacion_cargas, n = Inf)
cat("\n")

# Resumen de la diferencia
dif_resumen <- comparacion_cargas %>%
  summarise(
    dif_max   = max(dif_abs),
    dif_media = round(mean(dif_abs), 4),
    n_mayor_05 = sum(dif_abs > 0.05),
    n_mayor_10 = sum(dif_abs > 0.10)
  )
cat("Resumen de diferencias absolutas en cargas:\n")
print(dif_resumen, n = Inf)
cat("\n")

# Interpretacion automatica
if (dif_resumen$dif_max < 0.05) {
  conclusion <- "Cargas practicamente identicas: resultados robustos a la especificacion del diseno."
} else if (dif_resumen$dif_max < 0.10) {
  conclusion <- "Diferencias pequenas (<0.10) en algunas cargas: resultados sustancialmente equivalentes."
} else {
  conclusion <- "Diferencias >0.10 en al menos un item: revisar sensibilidad en el articulo."
}
cat("Conclusion de sensibilidad:", conclusion, "\n\n")

# =====================================================================
# 5. COMPARACION DEL AJUSTE GLOBAL
# =====================================================================
comparacion_ajuste <- tibble(
  Indice = c("chi2 escalado", "gl", "p",
             "CFI escalado", "TLI escalado",
             "RMSEA escalado", "RMSEA IC90 inf", "RMSEA IC90 sup",
             "SRMR"),
  Cluster = round(as.numeric(fit_cluster), 4),
  Pesos   = round(as.numeric(fit_pesos), 4),
  Diferencia = round(as.numeric(fit_pesos) - as.numeric(fit_cluster), 4)
) %>% as_tibble()

cat("=== COMPARACION DEL AJUSTE GLOBAL ===\n")
print(comparacion_ajuste, n = Inf)
cat("\n")

# =====================================================================
# 6. EXPORTACION
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

saveRDS(afc_pesos,            here("salidas", "afc_pesos.rds"))
saveRDS(fit_pesos,            here("salidas", "afc_pesos_fitmeasures.rds"))
saveRDS(comparacion_cargas,   here("salidas", "sensibilidad_cargas.rds"))
saveRDS(comparacion_ajuste,   here("salidas", "sensibilidad_ajuste.rds"))

write_xlsx(
  list(
    diseno_encuesta      = peso_summary,
    sectores             = if (exists("sectores")) sectores else tibble(),
    grupos_funcionales   = if (exists("grupos")) grupos else tibble(),
    tamano_entidades     = tam_ent,
    ajuste_cluster       = as.data.frame(t(fit_cluster)) %>% rownames_to_column("indice"),
    ajuste_pesos         = as.data.frame(t(fit_pesos)) %>% rownames_to_column("indice"),
    comparacion_ajuste   = comparacion_ajuste,
    comparacion_cargas   = comparacion_cargas,
    resumen_diferencias  = dif_resumen,
    conclusion           = tibble(conclusion = conclusion)
  ),
  here("salidas", "Actividad11_Sensibilidad_Diseno.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad11_Sensibilidad_Diseno.xlsx\n")
cat("salidas/afc_pesos.rds\n")
cat("salidas/afc_pesos_fitmeasures.rds\n")
cat("salidas/sensibilidad_cargas.rds\n")
cat("salidas/sensibilidad_ajuste.rds\n\n")

sessionInfo()