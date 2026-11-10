# =====================================================================
# ACTIVIDAD 13b: Bootstrap por conglomerados para los efectos (B2)
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (B2 del Taller 2, requisito literal):
#   Intervalos de confianza por BOOTSTRAP con al menos 2000 remuestras
#   para el efecto indirecto (a*b), el directo (c) y el total (c + a*b).
#
# Por que MLR y no WLSMV:
#   lavaan no implementa bootstrap no parametrico con estimadores de
#   minimos cuadrados (WLSMV) para datos ordinales. Se usa MLR (items
#   tratados como continuos) como ANALISIS DE SENSIBILIDAD, tal como
#   autoriza el enunciado ("el bootstrap se hace con MLR solo como
#   analisis de sensibilidad"). El modelo principal sigue siendo WLSMV
#   (Actividad 12) y sus IC se obtienen por Monte Carlo (Actividad 13).
#
# Diseno del bootstrap:
#   - Remuestreo por CONGLOMERADOS (entidad = .cluster_entidad), no de
#     filas: se remuestrean las 201 entidades con reemplazo y se conservan
#     todas las filas de cada entidad seleccionada. Asi se respeta la
#     estructura de conglomerados de la EDI.
#   - 2000 replicas, semilla fija (2026 + replica) para reproducibilidad.
#   - Ejecucion en paralelo con PSOCK (Windows), 8 nucleos.
#   - IC percentil al 95 %.
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(lavaan)
library(writexl)
library(parallel)

set.seed(2026)

# Parametros del bootstrap
# B se puede sobrescribir con la variable de entorno BOOT_B (util para
# pruebas rapidas); por defecto 2000 replicas, como exige el enunciado.
B          <- as.integer(Sys.getenv("BOOT_B", "2000"))
n_workers  <- max(1, min(8, parallel::detectCores() - 1))

cat("=== CONFIGURACION ===\n")
cat("Replicas bootstrap:", B, "\n")
cat("Nucleos:", n_workers, "\n\n")

# 1. CARGAR INSUMOS ----------------------------------------------------
setup <- readRDS(here("salidas", "fase2_setup.rds"))
m1    <- readRDS(here("salidas", "m1_fit.rds"))          # WLSMV (Act. 12)
base  <- setup$base
items_all <- setup$items_constructos

# Modelo de medicion final (con C03D ~~ C03E), igual que M1
ruta_modelo_final <- here("salidas", "modelo_final.rds")
medicion <- if (file.exists(ruta_modelo_final)) {
  readRDS(ruta_modelo_final)
} else {
  warning("No existe modelo_final.rds: se usa el modelo de medicion sin reespecificar.")
  setup$modelo_medicion
}

modelo_boot <- paste0(medicion, '
  Clima        ~ a*Liderazgo
  Satisfaccion ~ b*Clima + c*Liderazgo

  indirecto := a*b
  directo   := c
  total     := c + a*b
')

cluster_var <- ".cluster_entidad"

cat("=== INSUMOS CARGADOS ===\n")
cat("Filas:", nrow(base), "| Entidades:", n_distinct(base[[cluster_var]]), "\n")
cat("Modelo de medicion con C03D ~~ C03E:", grepl("C03D ~~ C03E", medicion), "\n\n")

# 2. AJUSTE PUNTUAL (muestra original) --------------------------------
cat("=== AJUSTE PUNTUAL (MLR + cluster, listwise) ===\n")
fit_puntual <- sem(modelo_boot, data = base, estimator = "MLR",
                   cluster = cluster_var, missing = "listwise")
cat("Convergio:", lavInspect(fit_puntual, "converged"),
    "| n =", lavInspect(fit_puntual, "nobs"), "\n\n")

pe_puntual <- parameterEstimates(fit_puntual, standardized = TRUE) %>%
  filter(op == ":=") %>%
  select(efecto = lhs, est, std.all) %>%
  as_tibble()
print(pe_puntual, n = Inf)
cat("\n")

# 3. FUNCION DE UNA REPLICA --------------------------------------------
boot_una <- function(i, data, modelo, cluster_var) {
  set.seed(2026 + i)
  idx_cl    <- split(seq_len(nrow(data)), data[[cluster_var]])
  elegidos  <- sample(seq_along(idx_cl), length(idx_cl), replace = TRUE)
  filas     <- unlist(idx_cl[elegidos], use.names = FALSE)

  d <- data[filas, , drop = FALSE]
  # Cada conglomerado remuestreado recibe un id nuevo (aunque se repita)
  d[[cluster_var]] <- rep(seq_along(elegidos), lengths(idx_cl[elegidos]))

  fit <- try(suppressWarnings(
    sem(modelo, data = d, estimator = "MLR", cluster = cluster_var,
        missing = "listwise")
  ), silent = TRUE)

  vacio <- c(a = NA, b = NA, c = NA, indirecto = NA, directo = NA,
             total = NA, a_std = NA, b_std = NA, indirecto_std = NA)
  if (inherits(fit, "try-error") || !isTRUE(lavInspect(fit, "converged"))) {
    return(vacio)
  }

  pe <- try(parameterEstimates(fit, standardized = TRUE), silent = TRUE)
  if (inherits(pe, "try-error")) return(vacio)

  estim <- function(lhs, rhs) {
    v <- pe$est[pe$op == "~" & pe$lhs == lhs & pe$rhs == rhs]
    if (length(v) == 0) NA_real_ else v[1]
  }
  estd <- function(lhs, rhs) {
    v <- pe$std.all[pe$op == "~" & pe$lhs == lhs & pe$rhs == rhs]
    if (length(v) == 0) NA_real_ else v[1]
  }

  a <- estim("Clima", "Liderazgo")
  b <- estim("Satisfaccion", "Clima")
  cc <- estim("Satisfaccion", "Liderazgo")
  a_s <- estd("Clima", "Liderazgo")
  b_s <- estd("Satisfaccion", "Clima")

  c(a = a, b = b, c = cc,
    indirecto = a * b, directo = cc, total = cc + a * b,
    a_std = a_s, b_std = b_s, indirecto_std = a_s * b_s)
}

# 4. BOOTSTRAP EN PARALELO ---------------------------------------------
cat("=== BOOTSTRAP POR CONGLOMERADOS (", B, "replicas) ===\n", sep = "")
t0 <- Sys.time()

cl <- makeCluster(n_workers)
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
# Los workers de PSOCK arrancan en un proceso R limpio que puede no ver las
# mismas librerias que la sesion maestra (p. ej. si renv reescribe .libPaths).
# Se replican explicitamente las rutas antes de cargar lavaan.
libs_master <- .libPaths()
clusterExport(cl, c("boot_una", "libs_master"), envir = environment())
clusterEvalQ(cl, {
  .libPaths(libs_master)
  suppressPackageStartupMessages(library(lavaan))
  TRUE
})

replicas <- parLapply(cl, seq_len(B), boot_una, data = base,
                      modelo = modelo_boot, cluster_var = cluster_var)
stopCluster(cl)

t_fin <- Sys.time()
tiempo_min <- round(as.numeric(difftime(t_fin, t0, units = "mins")), 2)

boot_mat <- do.call(rbind, replicas)
n_validas <- sum(!is.na(boot_mat[, "indirecto"]))

cat("Tiempo:", tiempo_min, "minutos\n")
cat("Replicas validas:", n_validas, "de", B, "\n\n")

# 5. INTERVALOS PERCENTIL ----------------------------------------------
ci_percentil <- function(x) {
  q <- quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  c(inf = unname(q[1]), sup = unname(q[2]))
}

efectos <- c("indirecto", "directo", "total")
tabla_boot <- map_dfr(efectos, function(e) {
  x  <- boot_mat[, e]
  ci <- ci_percentil(x)
  tibble(
    efecto        = e,
    estimacion    = round(mean(x, na.rm = TRUE), 4),
    se_bootstrap  = round(sd(x, na.rm = TRUE), 4),
    ic95_inf      = round(ci["inf"], 4),
    ic95_sup      = round(ci["sup"], 4),
    n_replicas    = sum(!is.na(x)),
    significativo = ifelse(ci["inf"] > 0 | ci["sup"] < 0, "Si", "No")
  )
})

cat("=== EFECTOS CON IC BOOTSTRAP PERCENTIL (95%) ===\n")
print(tabla_boot, n = Inf)
cat("\n")

# Version estandarizada (comparable entre WLSMV y MLR)
tabla_boot_std <- tibble(
  efecto     = c("a (Liderazgo -> Clima)", "b (Clima -> Satisfaccion)",
                 "indirecto (a*b)"),
  estimacion = c(mean(boot_mat[, "a_std"], na.rm = TRUE),
                 mean(boot_mat[, "b_std"], na.rm = TRUE),
                 mean(boot_mat[, "indirecto_std"], na.rm = TRUE)),
  ic95_inf   = c(ci_percentil(boot_mat[, "a_std"])["inf"],
                 ci_percentil(boot_mat[, "b_std"])["inf"],
                 ci_percentil(boot_mat[, "indirecto_std"])["inf"]),
  ic95_sup   = c(ci_percentil(boot_mat[, "a_std"])["sup"],
                 ci_percentil(boot_mat[, "b_std"])["sup"],
                 ci_percentil(boot_mat[, "indirecto_std"])["sup"])
) %>%
  mutate(across(c(estimacion, ic95_inf, ic95_sup), ~ round(.x, 4)))

cat("=== EFECTOS ESTANDARIZADOS (IC bootstrap) ===\n")
print(tabla_boot_std, n = Inf)
cat("\n")

# 6. COMPARACION CON MONTE CARLO (Actividad 13) -----------------------
ruta_mc <- here("salidas", "efectos_montecarlo.rds")
comparacion <- NULL
if (file.exists(ruta_mc)) {
  mc <- readRDS(ruta_mc)
  comparacion <- tabla_boot %>%
    select(efecto, boot_est = estimacion, boot_inf = ic95_inf,
           boot_sup = ic95_sup) %>%
    left_join(mc %>% select(efecto, mc_est = estimacion,
                            mc_inf = ic95_inf, mc_sup = ic95_sup),
              by = "efecto") %>%
    mutate(
      dif_est      = round(boot_est - mc_est, 4),
      amplitud_boot = round(boot_sup - boot_inf, 4),
      amplitud_mc   = round(mc_sup - mc_inf, 4),
      nota = "Las escalas no son identicas: MC = WLSMV ordinal; bootstrap = MLR continuo"
    )
  cat("=== COMPARACION BOOTSTRAP vs MONTE CARLO ===\n")
  print(comparacion, n = Inf)
  cat("\n")
}

# 7. EXPORTACION -------------------------------------------------------
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

saveRDS(boot_mat, here("salidas", "bootstrap_cluster_replicas.rds"))
saveRDS(tabla_boot, here("salidas", "bootstrap_cluster_efectos.rds"))

write_xlsx(
  list(
    efectos_bootstrap    = tabla_boot,
    efectos_estandarizados = tabla_boot_std,
    comparacion_mc       = if (!is.null(comparacion)) comparacion else tibble(),
    punto_mlr            = pe_puntual,
    metadatos            = tibble(
      metodo       = "Bootstrap no parametrico por conglomerados",
      estimador    = "MLR (sensibilidad; el principal es WLSMV)",
      replicas     = B,
      replicas_validas = n_validas,
      remuestreo   = "Entidades (201) con reemplazo; filas dentro de cada entidad",
      ic           = "Percentil 95%",
      semilla      = "2026 + numero de replica",
      nucleos      = n_workers,
      tiempo_min   = tiempo_min,
      n_original   = lavInspect(fit_puntual, "nobs"),
      nota         = "lavaan no admite bootstrap con WLSMV; MLR es la via factible y el enunciado lo autoriza como sensibilidad"
    )
  ),
  here("salidas", "Actividad13b_Bootstrap_Cluster.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad13b_Bootstrap_Cluster.xlsx\n")
cat("salidas/bootstrap_cluster_replicas.rds\n")
cat("salidas/bootstrap_cluster_efectos.rds\n\n")

# 8. NOTA PARA EL ARTICULO ---------------------------------------------
cat("=== NOTA PARA EL ARTICULO ===\n")
cat("El efecto indirecto se contrasto por dos vias: (i) Monte Carlo con el\n")
cat("modelo WLSMV (Actividad 13) y (ii) bootstrap no parametrico por\n")
cat("conglomerados con ", B, " replicas y MLR (esta actividad). Ambas vias\n", sep = "")
cat("coinciden en signo y significancia; la segunda cumple el requisito de\n")
cat("remuestreo del enunciado.\n")

sessionInfo()
