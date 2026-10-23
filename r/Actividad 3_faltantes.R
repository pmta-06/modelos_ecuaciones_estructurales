# =====================================================================
# ACTIVIDAD 3: Calidad de los datos (v2 corregida)
# Taller 1 - Fase 1 | SEM | USTA
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(naniar)     # patrones de faltantes y prueba de Little
library(writexl)
library(ggplot2)
library(grid)       # para grid.draw() en objetos upset

set.seed(2026)

# 1. CARGA DE LA BASE ANALÍTICA (generada en Actividad 2) --------------
base <- readRDS(here("salidas", "base_analitica.rds"))

# Ítems de los tres constructos
items_constructos <- c(
  paste0("C21", LETTERS[1:10]),   # Liderazgo
  paste0("C18", LETTERS[1:4]),    # Clima
  paste0("C03", LETTERS[1:6])     # Satisfacción
)

datos_items <- base %>% select(all_of(items_constructos))

cat("=== DIMENSIONES DE LA BASE ANALÍTICA ===\n")
cat("Filas:", nrow(base), "| Columnas:", ncol(base), "\n")
cat("Ítems de los constructos:", length(items_constructos), "\n\n")

# =====================================================================
# 3.1. RANGO Y DUPLICADOS
# =====================================================================

# --- Rango: cada ítem debe estar entre 1 y 5, o NA ---
rangos <- datos_items %>%
  summarise(across(everything(), list(
    min    = ~ min(.x, na.rm = TRUE),
    max    = ~ max(.x, na.rm = TRUE),
    n_NA   = ~ sum(is.na(.x)),
    pct_NA = ~ round(mean(is.na(.x)) * 100, 2)
  ))) %>%
  pivot_longer(everything(),
               names_to = c("item", "stat"),
               names_pattern = "^(.*)_(min|max|n_NA|pct_NA)$") %>%
  pivot_wider(names_from = stat, values_from = value)

cat("=== RANGOS POR ÍTEM ===\n")
print(rangos, n = Inf)

# Verificación programática: ¿algún valor fuera de 1-5?
fuera_rango <- datos_items %>%
  summarise(across(everything(),
                   ~ sum(!is.na(.x) & (.x < 1 | .x > 5)))) %>%
  pivot_longer(everything(), names_to = "item", values_to = "n_fuera") %>%
  filter(n_fuera > 0)

if (nrow(fuera_rango) == 0) {
  cat("OK: ningún ítem tiene valores fuera de 1-5.\n\n")
} else {
  cat("ATENCIÓN: ítems con valores fuera de 1-5:\n")
  print(fuera_rango)
}

# --- Duplicados ---
n_dup <- sum(duplicated(base$CUIN))
cat("Duplicados por CUIN:", n_dup, "\n")
n_dup_full <- sum(duplicated(base))
cat("Duplicados de fila completa:", n_dup_full, "\n\n")

# =====================================================================
# 3.2. DATOS FALTANTES
# =====================================================================

# --- Porcentaje por indicador ---
faltantes_item <- datos_items %>%
  summarise(across(everything(), ~ round(mean(is.na(.x)) * 100, 2))) %>%
  pivot_longer(everything(), names_to = "item", values_to = "pct_NA") %>%
  arrange(desc(pct_NA))

cat("=== % DE FALTANTES POR INDICADOR ===\n")
print(faltantes_item, n = Inf)
cat("\n")

# --- Porcentaje por observación ---
faltantes_obs <- datos_items %>%
  mutate(n_NA = rowSums(is.na(across(everything()))),
         pct_NA = round(n_NA / ncol(datos_items) * 100, 2))

cat("=== DISTRIBUCIÓN DE FALTANTES POR OBSERVACIÓN ===\n")
print(summary(faltantes_obs$pct_NA))
cat("Observaciones con 100% de ítems faltantes:",
    sum(faltantes_obs$pct_NA == 100), "\n")
cat("Observaciones con 0% de ítems faltantes:",
    sum(faltantes_obs$pct_NA == 0), "\n\n")

# --- Porcentaje de faltantes por constructo (opcional, para el reporte) ---
faltantes_constructo <- tibble(
  constructo = c("Liderazgo", "Clima laboral", "Satisfacción"),
  items = list(
    paste0("C21", LETTERS[1:10]),
    paste0("C18", LETTERS[1:4]),
    paste0("C03", LETTERS[1:6])
  )
) %>%
  mutate(
    n_items          = map_int(items, length),
    pct_NA_promedio  = map_dbl(items, ~ mean(is.na(datos_items[.x])) * 100),
    pct_obs_con_al_menos_un_NA = map_dbl(items, ~ mean(rowSums(is.na(datos_items[.x])) > 0) * 100)
  ) %>%
  select(-items)

cat("=== FALTANTES POR CONSTRUCTO ===\n")
print(faltantes_constructo)
cat("\n")

# --- Patrones de faltantes (naniar) ---
cat("=== PRUEBA DE LITTLE (MCAR) ===\n")
patrones <- mcar_test(datos_items)
print(patrones)
cat("\n")

# Mapa de % faltante por ítem (objeto ggplot: ggsave funciona)
p_miss <- gg_miss_var(datos_items, show_pct = TRUE) +
  labs(title = "Porcentaje de faltantes por ítem",
       x = "Ítem", y = "% faltante") +
  theme_minimal()
ggsave(here("salidas", "act3_faltantes_por_item.png"),
       p_miss, width = 8, height = 5, dpi = 150)

# Patrón de faltantes (objeto upset: NO usar ggsave, usar png + print)
p_patron <- gg_miss_upset(datos_items)
png(here("salidas", "act3_patron_faltantes.png"),
    width = 8, height = 5, units = "in", res = 150)
print(p_patron)
dev.off()

# --- Interpretación del mecanismo ---
cat("Estadístico chi-cuadrado:", patrones$statistic, "\n")
cat("gl:", patrones$df, "\n")
cat("p-valor:", patrones$p.value, "\n")
if (patrones$p.value < 0.05) {
  cat("Interpretación: se RECHAZA MCAR (p < 0.05).\n")
  cat("Mecanismo plausible: MAR o MNAR.\n")
  cat("Estrategia Fase 2: FIML (si el estimador es ML/MLR) o pairwise/\n")
  cat("imputación múltiple ordinal (si el estimador es WLSMV).\n\n")
} else {
  cat("Interpretación: no se rechaza MCAR.\n")
  cat("Estrategia Fase 2: casos completos o FIML.\n\n")
}

# =====================================================================
# 3.3. ATÍPICOS UNIVARIADOS (z robusto basado en mediana y MAD)
# =====================================================================
# z_robusto = 0.6745 * (x - mediana) / MAD
# Umbral típico: |z_robusto| > 3.5

calcular_z_robusto <- function(x) {
  med   <- median(x, na.rm = TRUE)
  mad_x <- mad(x, na.rm = TRUE)
  if (mad_x == 0) return(rep(0, length(x)))
  0.6745 * (x - med) / mad_x
}

z_robustos <- datos_items %>%
  mutate(across(everything(), calcular_z_robusto, .names = "z_{.col}")) %>%
  select(starts_with("z_"))

atipicos_univ <- z_robustos %>%
  summarise(across(everything(), ~ sum(abs(.x) > 3.5, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "item", values_to = "n_atipicos") %>%
  arrange(desc(n_atipicos))

cat("=== ATÍPICOS UNIVARIADOS (|z robusto| > 3.5) ===\n")
print(atipicos_univ, n = Inf)
cat("\n")

# Diagramas de caja por ítem
datos_largos <- datos_items %>%
  pivot_longer(everything(), names_to = "item", values_to = "valor")

p_box <- ggplot(datos_largos, aes(x = item, y = valor)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.color = "red") +
  labs(title = "Diagramas de caja por ítem (atípicos en rojo)",
       x = "Ítem", y = "Valor Likert") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7))

ggsave(here("salidas", "act3_boxplots.png"),
       p_box, width = 10, height = 5, dpi = 150)

# =====================================================================
# 3.4. ATÍPICOS MULTIVARIADOS (Distancia de Mahalanobis)
# =====================================================================

datos_completos <- datos_items %>% drop_na()
cat("Casos completos para Mahalanobis:", nrow(datos_completos), "\n")

if (nrow(datos_completos) > ncol(datos_completos)) {
  centro   <- colMeans(datos_completos)
  cov_mat  <- cov(datos_completos)
  d2       <- mahalanobis(datos_completos, center = centro, cov = cov_mat)
  gl       <- ncol(datos_completos)
  corte    <- qchisq(0.999, df = gl)
  
  n_atipicos_multiv <- sum(d2 > corte)
  cat("Parámetros: p =", gl, "| corte chi2(0.999) =", round(corte, 2), "\n")
  cat("Atípicos multivariados (p < 0.001):", n_atipicos_multiv,
      sprintf("(%.2f%% de los casos completos)\n\n",
              n_atipicos_multiv / nrow(datos_completos) * 100))
  
  df_mahal <- tibble(indice = seq_along(d2), d2 = d2) %>%
    mutate(atipico = d2 > corte)
  
  p_mahal <- ggplot(df_mahal, aes(x = indice, y = d2, color = atipico)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = corte, linetype = "dashed", color = "red") +
    labs(title = "Distancia de Mahalanobis por observación",
         subtitle = paste("Corte chi2(0.999,", gl, ") =", round(corte, 2)),
         x = "Observación", y = "D cuadrada") +
    scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
    theme_minimal()
  
  ggsave(here("salidas", "act3_mahalanobis.png"),
         p_mahal, width = 8, height = 5, dpi = 150)
} else {
  cat("ATENCIÓN: no hay suficientes casos completos para Mahalanobis.\n\n")
}

# =====================================================================
# 3.5. STRAIGHTLINING
# =====================================================================

straight <- datos_items %>%
  mutate(
    n_distintos   = apply(datos_items, 1, function(x) length(unique(na.omit(x)))),
    straightliner = n_distintos == 1 & rowSums(!is.na(datos_items)) > 0
  )

n_straight <- sum(straight$straightliner, na.rm = TRUE)
cat("=== STRAIGHTLINING ===\n")
cat("Observaciones con todos los ítems idénticos:", n_straight,
    sprintf("(%.2f%%)\n", n_straight / nrow(datos_items) * 100))

if (n_straight > 0) {
  valores_straight <- straight %>%
    filter(straightliner) %>%
    select(all_of(items_constructos)) %>%
    pivot_longer(everything()) %>%
    count(value) %>%
    arrange(desc(n))
  cat("Valores más repetidos en straightliners:\n")
  print(valores_straight)
}
cat("\n")

# =====================================================================
# 3.6. DECISIÓN SOBRE CONSERVACIÓN DE CASOS
# =====================================================================

decision_calidad <- tibble(
  Criterio = c(
    "Duplicados por CUIN",
    "Duplicados de fila completa",
    "Valores fuera de rango (1-5)",
    "Casos con 100% faltantes",
    "Atípicos univariados (|z| > 3.5)",
    "Atípicos multivariados (Mahalanobis p < 0.001)",
    "Straightliners"
  ),
  n = c(
    n_dup,
    n_dup_full,
    if (exists("fuera_rango")) sum(fuera_rango$n_fuera) else 0,
    sum(faltantes_obs$pct_NA == 100),
    sum(atipicos_univ$n_atipicos),
    if (exists("n_atipicos_multiv")) n_atipicos_multiv else NA_integer_,
    n_straight
  ),
  Decision = c(
    "No aplica: CUIN identifica entidad, no servidor",
    "Se conservan (coincidencia plausible en ítems Likert)",
    "No se encontraron (rango válido)",
    "Se conservan; lavaan los excluye al estimar",
    "Poco informativo: MAD = 0 en ítems ordinales (valores extremos legítimos)",
    "Se conservan (sensibilidad en Fase 2)",
    "Se conservan (sensibilidad en Fase 2)"
  )
)

cat("=== DECISIÓN SOBRE CASOS ===\n")
print(decision_calidad, n = Inf)
cat("\n")

# =====================================================================
# 3.7. ESTRATEGIA ANTICIPADA POR ESCENARIO (para el reporte)
# =====================================================================

estrategia_fase2 <- tribble(
  ~Escenario, ~Estimador_Fase2, ~Estrategia_faltantes, ~Justificacion,
  "Normalidad multivariada + ítems continuos",
  "ML / MLR", "FIML (missing = 'fiml')",
  "FIML usa todos los casos sin imputar; estándar con ML (Enders, 2022)",
  "No normalidad + ítems ordinales (Likert)",
  "WLSMV", "Pairwise (default lavaan) o imputación múltiple ordinal",
  "WLSMV trabaja con policóricas; FIML no está implementado en lavaan para WLSMV",
  "MCAR + < 5% faltantes por ítem",
  "Cualquiera", "Listwise (casos completos)",
  "Pérdida mínima; sesgo despreciable si MCAR (Little, 1988)"
)

cat("=== ESTRATEGIA ANTICIPADA PARA FASE 2 ===\n")
print(estrategia_fase2, n = Inf)
cat("\n")

# =====================================================================
# 3.8. EXPORTACIÓN DE RESULTADOS
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

write_xlsx(
  list(
    rangos            = rangos,
    faltantes_item    = faltantes_item,
    faltantes_constructo = faltantes_constructo,
    little_mcar       = tibble(
      estadistico    = patrones$statistic,
      gl             = patrones$df,
      p_valor        = patrones$p.value,
      interpretacion = ifelse(patrones$p.value < 0.05,
                              "Rechaza MCAR: MAR o MNAR plausible",
                              "No rechaza MCAR")
    ),
    atipicos_univ     = atipicos_univ,
    atipicos_multiv   = if (exists("n_atipicos_multiv")) {
      tibble(p = gl, corte = corte, n_atipicos = n_atipicos_multiv,
             pct = round(n_atipicos_multiv / nrow(datos_completos) * 100, 2))
    } else tibble(),
    straightliners    = tibble(n = n_straight,
                               pct = round(n_straight / nrow(datos_items) * 100, 2)),
    decision_calidad  = decision_calidad,
    estrategia_fase2  = estrategia_fase2
  ),
  here("salidas", "Actividad3_Calidad_Datos.xlsx")
)

cat("=== ARCHIVO GENERADO ===\n")
cat("salidas/Actividad3_Calidad_Datos.xlsx\n")
cat("Gráficos: act3_*.png en /salidas\n")

sessionInfo()