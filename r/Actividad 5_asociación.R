# =====================================================================
# ACTIVIDAD 5: Asociación entre indicadores y estructura exploratoria
# Taller 1 - Fase 1 | SEM | USTA
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(psych)       # polychoric, alpha, omega, fa, KMO, cortest.bartlett
library(corrplot)    # mapa de calor de correlaciones
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
# 5.1. MATRIZ DE CORRELACIONES POLICÓRICAS
# =====================================================================
# Los ítems son ordinales (Likert 1-5); la correlación apropiada es
# la policórica (Holgado-Tello et al., 2010).

poli <- polychoric(datos_items)
matriz_poli <- poli$rho

# Verificar que la matriz sea positiva definida (requisito para AFE)
cat("=== VERIFICACIÓN DE LA MATRIZ POLICÓRICA ===\n")
cat("Determinante:", det(matriz_poli), "\n")
cat("¿Es positiva definida?:",
    all(eigen(matriz_poli, symmetric = TRUE, only.values = TRUE)$values > 0),
    "\n\n")

# --- Mapa de calor ordenado por constructo ---
# Reordenamos las columnas/filas por constructo para que se vean los bloques
orden_constructos <- c(items_liderazgo, items_clima, items_satisf)
matriz_ordenada <- matriz_poli[orden_constructos, orden_constructos]

# Etiquetas con color por constructo
etiquetas <- data.frame(
  item = orden_constructos,
  constructo = c(
    rep("Liderazgo", length(items_liderazgo)),
    rep("Clima", length(items_clima)),
    rep("Satisfacción", length(items_satisf))
  )
)

png(here("salidas", "act5_mapa_calor_poli.png"),
    width = 10, height = 9, units = "in", res = 150)
corrplot(matriz_ordenada,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.7,
         tl.srt = 90,
         addCoef.col = "black",
         number.cex = 0.45,
         col = colorRampPalette(c("tomato", "white", "steelblue"))(200),
         title = "Correlaciones policóricas (ordenadas por constructo)",
         mar = c(0, 0, 2, 0))
dev.off()

# --- Verificación descriptiva: dentro vs. entre constructos ---
# Promedio de correlaciones dentro de cada constructo
promedio_dentro <- function(items, matriz) {
  sub <- matriz[items, items]
  mean(sub[upper.tri(sub)], na.rm = TRUE)
}

cors_dentro <- tibble(
  constructo      = c("Liderazgo", "Clima", "Satisfacción"),
  cor_prom_dentro = c(
    promedio_dentro(items_liderazgo, matriz_poli),
    promedio_dentro(items_clima,     matriz_poli),
    promedio_dentro(items_satisf,    matriz_poli)
  )
)

# Promedio de correlaciones entre cada par de constructos
promedio_entre <- function(items_a, items_b, matriz) {
  sub <- matriz[items_a, items_b]
  mean(sub, na.rm = TRUE)
}

cors_entre <- tibble(
  par = c("Liderazgo-Clima", "Liderazgo-Satisfacción", "Clima-Satisfacción"),
  cor_prom_entre = c(
    promedio_entre(items_liderazgo, items_clima,  matriz_poli),
    promedio_entre(items_liderazgo, items_satisf, matriz_poli),
    promedio_entre(items_clima,     items_satisf, matriz_poli)
  )
)

cat("=== CORRELACIONES DENTRO DE CADA CONSTRUCTO ===\n")
print(cors_dentro, n = Inf)
cat("\n=== CORRELACIONES ENTRE CONSTRUCTOS ===\n")
print(cors_entre, n = Inf)
cat("\n")

# Evidencia preliminar de validez convergente/discriminante
cat("Interpretación descriptiva:\n")
for (i in seq_len(nrow(cors_dentro))) {
  cat(sprintf("  - %s: promedio dentro = %.3f\n",
              cors_dentro$constructo[i], cors_dentro$cor_prom_dentro[i]))
}
for (i in seq_len(nrow(cors_entre))) {
  cat(sprintf("  - %s: promedio entre = %.3f\n",
              cors_entre$par[i], cors_entre$cor_prom_entre[i]))
}
cat("  Convergente si: dentro > entre.\n")
cat("  Discriminante si: dentro > entre de forma consistente.\n\n")

# =====================================================================
# 5.2. CONFIABILIDAD POR CONSTRUCTO
# =====================================================================
# α de Cronbach (1951) y ω de McDonald (1999), más correlación ítem-resto.

calcular_confiabilidad <- function(items, datos, nombre) {
  sub <- datos %>% select(all_of(items))
  
  # Alpha
  alpha_res <- psych::alpha(sub, check.keys = FALSE, warnings = FALSE)
  
  # Omega (requiere nfactors = 1)
  omega_res <- tryCatch(
    psych::omega(sub, nfactors = 1, plot = FALSE, warnings = FALSE),
    error = function(e) NULL
  )
  
  # Ítem-resto
  item_resto <- alpha_res$item.stats$r.drop
  
  tibble(
    constructo   = nombre,
    n_items      = length(items),
    alpha        = round(alpha_res$total$raw_alpha, 3),
    omega        = if (!is.null(omega_res)) round(omega_res$omega.tot, 3) else NA_real_,
    item         = items,
    item_resto   = round(item_resto, 3)
  )
}

conf_liderazgo <- calcular_confiabilidad(items_liderazgo, datos_items, "Liderazgo")
conf_clima     <- calcular_confiabilidad(items_clima,     datos_items, "Clima laboral")
conf_satisf    <- calcular_confiabilidad(items_satisf,    datos_items, "Satisfacción")

confiabilidad <- bind_rows(conf_liderazgo, conf_clima, conf_satisf)

cat("=== CONFIABILIDAD POR CONSTRUCTO ===\n")
print(confiabilidad, n = Inf)
cat("\n")

# Resumen (una fila por constructo)
resumen_conf <- confiabilidad %>%
  group_by(constructo, n_items, alpha, omega) %>%
  summarise(
    item_resto_min = min(item_resto, na.rm = TRUE),
    item_resto_max = max(item_resto, na.rm = TRUE),
    .groups = "drop"
  )

cat("=== RESUMEN DE CONFIABILIDAD ===\n")
print(resumen_conf, n = Inf)
cat("\n")

# =====================================================================
# 5.3. ANÁLISIS FACTORIAL EXPLORATORIO
# =====================================================================

# --- KMO y Bartlett ---
kmo_res <- KMO(matriz_poli)
bartlett_res <- cortest.bartlett(matriz_poli, n = nrow(datos_items))

cat("=== KMO ===\n")
print(kmo_res$MSA)
cat("\n=== BARTLETT ===\n")
print(bartlett_res)
cat("\n")

# --- Análisis paralelo (Horn, 1965) para número de factores ---
cat("=== ANÁLISIS PARALELO ===\n")
png(here("salidas", "act5_analisis_paralelo.png"),
    width = 8, height = 6, units = "in", res = 150)
fa.parallel(matriz_poli,
            n.obs = nrow(datos_items),
            fa    = "fa",
            fm    = "minres",
            main  = "Análisis paralelo (Horn, 1965)")
dev.off()

# --- AFE con rotación oblicua (oblimin) ---
# Se prueban 2, 3 y 4 factores para comparar
afe_2 <- fa(matriz_poli, nfactors = 2, rotate = "oblimin", fm = "minres")
afe_3 <- fa(matriz_poli, nfactors = 3, rotate = "oblimin", fm = "minres")
afe_4 <- fa(matriz_poli, nfactors = 4, rotate = "oblimin", fm = "minres")

cat("=== AFE 3 FACTORES (estructura hipotetizada) ===\n")
print(afe_3$loadings, cutoff = 0.30)
cat("\n")

cat("=== VARIANZA EXPLICADA (3 factores) ===\n")
print(afe_3$Vaccounted)
cat("\n")

# --- Tabla de cargas ordenada por constructo hipotético ---
cargas_afe3 <- as.data.frame(unclass(afe_3$loadings)) %>%
  rownames_to_column("item") %>%
  mutate(
    constructo_hipotetico = case_when(
      item %in% items_liderazgo ~ "Liderazgo",
      item %in% items_clima     ~ "Clima laboral",
      item %in% items_satisf    ~ "Satisfacción",
      TRUE                      ~ "Otro"
    )
  ) %>%
  relocate(constructo_hipotetico, .after = item) %>%
  arrange(constructo_hipotetico, item) %>%
  as_tibble()

cat("=== CARGAS AFE 3 FACTORES ORDENADAS POR CONSTRUCTO HIPOTÉTICO ===\n")
print(cargas_afe3, n = Inf)
cat("\n")

# --- Comparación estructura empírica vs. hipotetizada ---
# Para cada ítem, ¿en qué factor carga más?
comparacion <- cargas_afe3 %>%
  pivot_longer(starts_with("MR"), names_to = "factor", values_to = "carga") %>%
  group_by(item) %>%
  slice_max(abs(carga), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(item, constructo_hipotetico, factor, carga)

cat("=== COMPARACIÓN: FACTOR DE MAYOR CARGA POR ÍTEM ===\n")
print(comparacion, n = Inf)
cat("\n")

# Conteo de coincidencias
# Mapeo según el AFE real (ver cargas): MR1 = Clima, MR2 = Satisfacción, MR3 = Liderazgo
mapa_factores <- c("MR1" = "Clima laboral",
                   "MR2" = "Satisfacción",
                   "MR3" = "Liderazgo")

comparacion <- comparacion %>%
  mutate(
    factor_interpretado = mapa_factores[factor],
    coincide = factor_interpretado == constructo_hipotetico
  )

cat("Coincidencias (ítem cargó en el factor esperado):",
    sum(comparacion$coincide, na.rm = TRUE),
    "de", nrow(comparacion), "\n\n")

print(comparacion %>% count(constructo_hipotetico, coincide), n = Inf)
cat("\n")

# --- Gráfico de cargas (heatmap) ---
cargas_largas <- cargas_afe3 %>%
  pivot_longer(starts_with("MR"), names_to = "factor", values_to = "carga")

p_cargas <- ggplot(cargas_largas, aes(x = factor, y = item, fill = carga)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(carga, 2)), size = 2.5) +
  scale_fill_gradient2(low = "tomato", mid = "white", high = "steelblue",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Cargas factoriales AFE (3 factores, rotación oblimin)",
       x = "Factor", y = "Ítem") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))

ggsave(here("salidas", "act5_cargas_afe.png"),
       p_cargas, width = 6, height = 8, dpi = 150)

# =====================================================================
# 5.4. EXPORTACIÓN
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

# Preparar matriz de correlaciones para exportar
matriz_export <- as.data.frame(matriz_poli) %>%
  rownames_to_column("item")

write_xlsx(
  list(
    matriz_poli       = matriz_export,
    cors_dentro       = cors_dentro,
    cors_entre        = cors_entre,
    confiabilidad     = confiabilidad,
    resumen_conf      = resumen_conf,
    kmo_bartlett      = tibble(
      indicador = c("KMO (MSA global)",
                    "Bartlett chi2",
                    "Bartlett gl",
                    "Bartlett p"),
      valor = c(round(kmo_res$MSA, 3),
                round(bartlett_res$chisq, 3),
                bartlett_res$df,
                round(bartlett_res$p.value, 6))
    ),
    cargas_afe3       = cargas_afe3,
    comparacion_afe   = comparacion,
    varianza_afe3     = as.data.frame(afe_3$Vaccounted) %>% rownames_to_column("metrica")
  ),
  here("salidas", "Actividad5_Asociacion_AFE.xlsx")
)

cat("=== ARCHIVO GENERADO ===\n")
cat("salidas/Actividad5_Asociacion_AFE.xlsx\n")
cat("Gráficos: act5_*.png en /salidas\n")

sessionInfo()