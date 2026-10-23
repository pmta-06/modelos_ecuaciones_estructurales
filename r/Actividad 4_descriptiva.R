# =====================================================================
# ACTIVIDAD 4: Descripción y distribución de los indicadores
# Taller 1 - Fase 1 | SEM | USTA
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(psych)      # describe, skew, kurtosis
library(MVN)        # mardia (normalidad multivariada)
library(writexl)
library(ggplot2)

set.seed(2026)

# 1. CARGA DE LA BASE ANALÍTICA ----------------------------------------
base <- readRDS(here("salidas", "base_analitica.rds"))

items_liderazgo   <- paste0("C21", LETTERS[1:10])
items_clima       <- paste0("C18", LETTERS[1:4])
items_satisf      <- paste0("C03", LETTERS[1:5])    # C03A..C03E (C03F excluido)
items_constructos <- c(items_liderazgo, items_clima, items_satisf)

datos_items <- base %>% select(all_of(items_constructos))

cat("=== DIMENSIONES ===\n")
cat("Filas:", nrow(datos_items), "| Ítems:", ncol(datos_items), "\n\n")

# =====================================================================
# 4.1. TABLA DESCRIPTIVA POR CONSTRUCTO
# =====================================================================

descriptivos <- datos_items %>%
  describe() %>%
  rownames_to_column("item") %>%
  select(item, n, mean, sd, median, min, max, skew, kurtosis) %>%
  mutate(
    constructo = case_when(
      item %in% items_liderazgo ~ "Liderazgo",
      item %in% items_clima     ~ "Clima laboral",
      item %in% items_satisf    ~ "Satisfacción",
      TRUE                      ~ "Otro"
    )
  ) %>%
  relocate(constructo, .after = item) %>%
  arrange(constructo, item) %>%
  as_tibble()

cat("=== DESCRIPTIVOS POR ÍTEM ===\n")
print(descriptivos, n = Inf)
cat("\n")

resumen_constructo <- descriptivos %>%
  group_by(constructo) %>%
  summarise(
    n_items        = n(),
    media_prom     = round(mean(mean,     na.rm = TRUE), 2),
    sd_prom        = round(mean(sd,       na.rm = TRUE), 2),
    asimetria_prom = round(mean(skew,     na.rm = TRUE), 2),
    curtosis_prom  = round(mean(kurtosis, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  as_tibble()

cat("=== RESUMEN POR CONSTRUCTO ===\n")
print(resumen_constructo, n = Inf)
cat("\n")

frecuencias <- datos_items %>%
  pivot_longer(everything(), names_to = "item", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  count(item, valor) %>%
  group_by(item) %>%
  mutate(pct = round(n / sum(n) * 100, 2)) %>%
  ungroup() %>%
  pivot_wider(names_from = valor, values_from = c(n, pct),
              names_glue = "{.value}_{valor}") %>%
  arrange(item)

cat("=== FRECUENCIAS POR CATEGORÍA (primeras filas) ===\n")
print(head(frecuencias, 10))
cat("\n")

# =====================================================================
# 4.2. EFECTOS PISO Y TECHO (corregido)
# =====================================================================
piso_techo <- datos_items %>%
  summarise(across(everything(), list(
    pct_piso  = ~ round(mean(.x == 1, na.rm = TRUE) * 100, 2),
    pct_techo = ~ round(mean(.x == 5, na.rm = TRUE) * 100, 2)
  ))) %>%
  pivot_longer(
    everything(),
    names_to      = c("item", "stat"),
    names_pattern = "^([^_]+)_(.+)$"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    constructo = case_when(
      item %in% items_liderazgo ~ "Liderazgo",
      item %in% items_clima     ~ "Clima laboral",
      item %in% items_satisf    ~ "Satisfacción",
      TRUE                      ~ "Otro"
    ),
    alerta = case_when(
      pct_piso > 15 | pct_techo > 15 ~ "Revisar",
      TRUE                           ~ "OK"
    )
  ) %>%
  relocate(constructo, .after = item) %>%
  arrange(constructo, item)

cat("=== EFECTOS PISO Y TECHO (%) ===\n")
print(piso_techo, n = Inf)
cat("\n")

p_piso <- piso_techo %>%
  pivot_longer(c(pct_piso, pct_techo),
               names_to = "tipo", values_to = "pct") %>%
  ggplot(aes(x = item, y = pct, fill = tipo)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 15, linetype = "dashed", color = "red") +
  labs(title = "Efectos piso y techo por ítem",
       subtitle = "Línea roja: umbral 15%",
       x = "Ítem", y = "% de respuestas", fill = "Efecto") +
  scale_fill_manual(values = c("pct_piso"  = "tomato",
                               "pct_techo" = "steelblue"),
                    labels = c("Piso (1)", "Techo (5)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7))

ggsave(here("salidas", "act4_piso_techo.png"),
       p_piso, width = 10, height = 5, dpi = 150)

# =====================================================================
# 4.3. NORMALIDAD UNIVARIADA
# =====================================================================
normalidad_univ <- descriptivos %>%
  select(constructo, item, skew, kurtosis) %>%
  mutate(
    alerta_asim = ifelse(abs(skew) > 2, "Revisar", "OK"),
    alerta_kurt = ifelse(abs(kurtosis) > 7, "Revisar", "OK")
  )

cat("=== NORMALIDAD UNIVARIADA ===\n")
print(normalidad_univ, n = Inf)
cat("\n")

# =====================================================================
# 4.4. NORMALIDAD MULTIVARIADA: PRUEBA DE MARDIA (corregido)
# =====================================================================
datos_completos <- datos_items %>% drop_na()
cat("Casos completos para Mardia:", nrow(datos_completos), "\n")
cat("Número de variables (p):", ncol(datos_completos), "\n")
cat("Valor esperado de b2,p bajo normalidad: p(p+2) =",
    ncol(datos_completos) * (ncol(datos_completos) + 2), "\n\n")

# Se usa MVN::mardia() directamente para evitar conflictos con otros paquetes
resultado_mardia <- MVN::mardia(datos_completos)

cat("=== PRUEBA DE MARDIA ===\n")
print(resultado_mardia)
cat("\n")

# Extracción robusta de b1,p y b2,p
mardia_stats <- as_tibble(resultado_mardia)

# OJO: en MVN, "Statistic" es chi-cuadrado (asimetría) y z (curtosis), NO b1,p ni b2,p
chi2_asim <- mardia_stats$Statistic[grepl("Skew", mardia_stats$Test, ignore.case = TRUE)]
z_kurt    <- mardia_stats$Statistic[grepl("Kurt", mardia_stats$Test, ignore.case = TRUE)]
p_asim <- mardia_stats$p.value[grepl("Skew", mardia_stats$Test, ignore.case = TRUE)]
p_kurt <- mardia_stats$p.value[grepl("Kurt", mardia_stats$Test, ignore.case = TRUE)]

cat("Mardia asimetría: chi2 =", chi2_asim, "\n")
cat("Mardia curtosis: z =", z_kurt, "\n\n")

# Cálculo directo de b1,p y b2,p (Mardia, 1970); b1,p por bloques para no
# construir la matriz n x n completa en memoria
Xc  <- scale(as.matrix(datos_completos), scale = FALSE)
n_c <- nrow(Xc)
Z   <- Xc %*% solve(crossprod(Xc) / n_c)
b2p <- mean(rowSums(Z * Xc)^2)
b1p <- 0
for (i in seq(1, n_c, by = 500)) {
  idx <- i:min(i + 499, n_c)
  b1p <- b1p + sum((Xc[idx, ] %*% t(Z))^3)
}
b1p <- b1p / n_c^2

cat("b1,p (asimetría multivariada):", round(b1p, 4), "\n")
cat("b2,p (curtosis multivariada):", round(b2p, 4), "\n")
cat("Valor esperado b2,p bajo normalidad: p(p+2) =",
    ncol(datos_completos) * (ncol(datos_completos) + 2), "\n\n")

# =====================================================================
# 4.5. DECISIÓN PRELIMINAR DEL ESTIMADOR
# =====================================================================
tipo_items         <- "Ordinal (Likert 1-5)"
rechaza_normalidad <- any(c(p_asim, p_kurt) < 0.05, na.rm = TRUE)

estimador_sugerido <- case_when(
  tipo_items == "Ordinal (Likert 1-5)" & rechaza_normalidad  ~ "WLSMV",
  tipo_items == "Ordinal (Likert 1-5)" & !rechaza_normalidad ~ "WLSMV (o MLR con corrección)",
  tipo_items == "Continuo" & rechaza_normalidad              ~ "MLR",
  tipo_items == "Continuo" & !rechaza_normalidad             ~ "ML",
  TRUE                                                       ~ "Revisar"
)

cat("=== DECISIÓN PRELIMINAR DEL ESTIMADOR ===\n")
cat("Tipo de ítems:", tipo_items, "\n")
cat("¿Mardia rechaza normalidad multivariada?:",
    ifelse(rechaza_normalidad, "SÍ", "NO"), "\n")
cat("Estimador sugerido para Fase 2:", estimador_sugerido, "\n\n")

tabla_decision_estimador <- tibble(
  Criterio = c(
    "Tipo de indicadores",
    "Prueba de Mardia (asimetría)",
    "Prueba de Mardia (curtosis)",
    "¿Se rechaza normalidad multivariada?",
    "Estimador sugerido Fase 2",
    "Justificación"
  ),
  Valor = c(
    tipo_items,
    paste0("b1,p = ", round(b1p, 4), "; chi2 = ", round(chi2_asim, 1), " (p = ", round(p_asim, 4), ")"),
    paste0("b2,p = ", round(b2p, 4), "; z = ", round(z_kurt, 1), " (p = ", round(p_kurt, 4), ")"),
    ifelse(rechaza_normalidad, "Sí", "No"),
    estimador_sugerido,
    "Li (2016): con ítems ordinales y no normalidad, WLSMV es más apropiado que ML"
  )
)

print(tabla_decision_estimador, n = Inf)
cat("\n")

# =====================================================================
# 4.6. GRÁFICOS DE DISTRIBUCIÓN
# =====================================================================
datos_largos <- datos_items %>%
  pivot_longer(everything(), names_to = "item", values_to = "valor") %>%
  mutate(
    constructo = case_when(
      item %in% items_liderazgo ~ "Liderazgo",
      item %in% items_clima     ~ "Clima laboral",
      item %in% items_satisf    ~ "Satisfacción",
      TRUE                      ~ "Otro"
    )
  )

p_hist <- ggplot(datos_largos, aes(x = valor)) +
  geom_histogram(bins = 5, fill = "steelblue", color = "white", alpha = 0.8) +
  facet_wrap(~ constructo + item, scales = "free_y", ncol = 5) +
  labs(title = "Distribución de los ítems por constructo",
       x = "Valor Likert", y = "Frecuencia") +
  theme_minimal() +
  theme(strip.text = element_text(size = 7))

ggsave(here("salidas", "act4_histogramas.png"),
       p_hist, width = 14, height = 8, dpi = 150)

p_box_constructo <- ggplot(datos_largos, aes(x = constructo, y = valor, fill = constructo)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(title = "Distribución por constructo",
       x = "Constructo", y = "Valor Likert") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here("salidas", "act4_boxplot_constructo.png"),
       p_box_constructo, width = 8, height = 5, dpi = 150)

# =====================================================================
# 4.7. EXPORTACIÓN
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

write_xlsx(
  list(
    descriptivos       = descriptivos,
    resumen_constructo = resumen_constructo,
    frecuencias        = frecuencias,
    piso_techo         = piso_techo,
    normalidad_univ    = normalidad_univ,
    mardia             = mardia_stats,
    decision_estimador = tabla_decision_estimador
  ),
  here("salidas", "Actividad4_Descriptivos_Normalidad.xlsx")
)

cat("=== ARCHIVO GENERADO ===\n")
cat("salidas/Actividad4_Descriptivos_Normalidad.xlsx\n")
cat("Gráficos: act4_*.png en /salidas\n")

sessionInfo()