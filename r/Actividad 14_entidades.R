# =====================================================================
# ACTIVIDAD 14: Analisis por entidades (complemento B1/B2)
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (observacion del profesor, punto 2):
#   "Mirar la calificacion entre entidades".
#
# Que hace:
#   1. Puntajes promedio por entidad (201 filas) para Liderazgo, Clima
#      y Satisfaccion: media simple y media ponderada por FEXPFINAL.
#   2. Ranking top 10 / bottom 10 por cada constructo.
#   3. Distribucion de las medias por entidad (histograma, boxplot).
#   4. Correlaciones entre las medias de los 3 constructos a nivel
#      de entidad (¿entidades con mejor liderazgo tienen mejor clima
#      y satisfaccion?).
#   5. Dispersion Liderazgo vs Satisfaccion con etiquetas de extremos.
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(writexl)
library(ggplot2)

# 1. CARGAR INSUMOS ----------------------------------------------------
setup <- readRDS(here("salidas", "fase2_setup.rds"))
base  <- setup$base

items_lid <- setup$items_liderazgo
items_cli <- setup$items_clima
items_sat <- setup$items_satisf

cat("=== INSUMOS CARGADOS ===\n")
cat("Filas:", nrow(base), "| Entidades:", n_distinct(base$.cluster_entidad), "\n\n")

# =====================================================================
# 1. PUNTAJES PROMEDIO POR ENTIDAD
# =====================================================================
# Media simple: promedio de los items, luego promedio por entidad.
# Media ponderada: promedio ponderado por FEXPFINAL dentro de cada entidad.
# (La ponderada corrige el hecho de que no todos los servidores de una
#  entidad tienen el mismo peso de expansion.)

base_punt <- base %>%
  mutate(
    punt_lid = rowMeans(select(., all_of(items_lid)), na.rm = TRUE),
    punt_cli = rowMeans(select(., all_of(items_cli)), na.rm = TRUE),
    punt_sat = rowMeans(select(., all_of(items_sat)), na.rm = TRUE)
  )

medias_entidad <- base_punt %>%
  group_by(.cluster_entidad) %>%
  summarise(
    n_servidores = n(),
    # Media simple
    lid_simple = mean(punt_lid, na.rm = TRUE),
    cli_simple = mean(punt_cli, na.rm = TRUE),
    sat_simple = mean(punt_sat, na.rm = TRUE),
    # Media ponderada por FEXPFINAL
    lid_pond   = weighted.mean(punt_lid, w = .peso_exp, na.rm = TRUE),
    cli_pond   = weighted.mean(punt_cli, w = .peso_exp, na.rm = TRUE),
    sat_pond   = weighted.mean(punt_sat, w = .peso_exp, na.rm = TRUE),
    peso_total = sum(.peso_exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(lid_pond))

cat("=== MEDIAS POR ENTIDAD (primeras 10) ===\n")
print(head(medias_entidad, 10), n = Inf)
cat("\n")

cat("=== RESUMEN DE LAS MEDIAS POR ENTIDAD ===\n")
resumen_medias <- medias_entidad %>%
  summarise(
    across(c(lid_pond, cli_pond, sat_pond),
           list(min = min, q25 = ~quantile(.x, .25),
                mediana = median, q75 = ~quantile(.x, .75),
                max = max, media = mean, sd = sd),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(), names_to = c("constructo", "stat"),
               names_pattern = "^(lid|cli|sat)_pond_(.+)$") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(constructo = case_when(
    constructo == "lid" ~ "Liderazgo",
    constructo == "cli" ~ "Clima",
    constructo == "sat" ~ "Satisfaccion"
  ))
print(resumen_medias, n = Inf)
cat("\n")

# =====================================================================
# 2. RANKING TOP 10 / BOTTOM 10
# =====================================================================
top_bottom <- function(var, nombre) {
  top <- medias_entidad %>% arrange(desc(.data[[var]])) %>%
    slice_head(n = 10) %>% mutate(posicion = "Top 10", constructo = nombre)
  bottom <- medias_entidad %>% arrange(.data[[var]]) %>%
    slice_head(n = 10) %>% mutate(posicion = "Bottom 10", constructo = nombre)
  bind_rows(top, bottom) %>%
    select(constructo, posicion, .cluster_entidad,
           n_servidores, media = all_of(var))
}

ranking <- bind_rows(
  top_bottom("lid_pond", "Liderazgo"),
  top_bottom("cli_pond", "Clima"),
  top_bottom("sat_pond", "Satisfaccion")
)

cat("=== RANKING TOP/BOTTOM 10 (media ponderada) ===\n")
print(ranking, n = Inf)
cat("\n")

# =====================================================================
# 3. DISTRIBUCION DE LAS MEDIAS POR ENTIDAD (graficos)
# =====================================================================
if (!dir.exists(here("figuras"))) dir.create(here("figuras"))

# Histograma
medias_largas <- medias_entidad %>%
  select(.cluster_entidad, lid_pond, cli_pond, sat_pond) %>%
  pivot_longer(-.cluster_entidad, names_to = "constructo", values_to = "media") %>%
  mutate(constructo = factor(constructo,
                             levels = c("lid_pond", "cli_pond", "sat_pond"),
                             labels = c("Liderazgo", "Clima", "Satisfaccion")))

p_hist <- ggplot(medias_largas, aes(x = media, fill = constructo)) +
  geom_histogram(bins = 20, color = "white", alpha = 0.85) +
  facet_wrap(~ constructo, scales = "free", ncol = 3) +
  labs(title = "Distribucion de las medias por entidad (ponderadas)",
       x = "Media del constructo", y = "Numero de entidades") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave(here("figuras", "act14_histograma_entidades.png"),
       p_hist, width = 12, height = 4, dpi = 150)

# Boxplot
p_box <- ggplot(medias_largas, aes(x = constructo, y = media, fill = constructo)) +
  geom_boxplot(alpha = 0.75, outlier.color = "red") +
  labs(title = "Boxplot de medias por entidad",
       x = "Constructo", y = "Media") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave(here("figuras", "act14_boxplot_entidades.png"),
       p_box, width = 8, height = 5, dpi = 150)

# =====================================================================
# 4. CORRELACIONES A NIVEL DE ENTIDAD
# =====================================================================
# ¿Las entidades con mejor liderazgo tienen mejor clima y satisfaccion?
cor_entidades <- medias_entidad %>%
  select(lid_pond, cli_pond, sat_pond) %>%
  cor(use = "complete.obs")

cat("=== CORRELACIONES A NIVEL DE ENTIDAD (201 entidades) ===\n")
print(round(cor_entidades, 3))
cat("\n")

# Test de significancia (Pearson)
cor_test_lid_sat <- cor.test(medias_entidad$lid_pond, medias_entidad$sat_pond)
cor_test_lid_cli <- cor.test(medias_entidad$lid_pond, medias_entidad$cli_pond)
cor_test_cli_sat <- cor.test(medias_entidad$cli_pond, medias_entidad$sat_pond)

tabla_cor_entidades <- tibble(
  par = c("Liderazgo-Clima", "Liderazgo-Satisfaccion", "Clima-Satisfaccion"),
  r   = round(c(cor_test_lid_cli$estimate,
                cor_test_lid_sat$estimate,
                cor_test_cli_sat$estimate), 3),
  p   = c(cor_test_lid_cli$p.value,
          cor_test_lid_sat$p.value,
          cor_test_cli_sat$p.value)
) %>% mutate(p = ifelse(p < 2.2e-16, "< 2.2e-16", sprintf("%.4f", p)))

cat("=== CORRELACIONES CON TEST ===\n")
print(tabla_cor_entidades, n = Inf)
cat("\n")

# =====================================================================
# 5. DISPERSION LIDERAZGO vs SATISFACCION
# =====================================================================
# Etiquetas de entidades extremas (top 5 y bottom 5 en satisfaccion)
extremos <- medias_entidad %>%
  arrange(desc(sat_pond)) %>%
  slice(c(1:5, (n() - 4):n())) %>%
  mutate(etiqueta = paste0("E", .cluster_entidad))

p_disp <- ggplot(medias_entidad, aes(x = lid_pond, y = sat_pond)) +
  geom_point(aes(size = n_servidores), color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_text(data = extremos, aes(label = etiqueta),
            vjust = -0.5, size = 2.5, color = "black") +
  labs(title = "Liderazgo vs Satisfaccion a nivel de entidad",
       subtitle = "Puntos = entidades; tamano = numero de servidores",
       x = "Media de Liderazgo (ponderada)",
       y = "Media de Satisfaccion (ponderada)") +
  theme_minimal()
ggsave(here("figuras", "act14_dispersion_entidades.png"),
       p_disp, width = 8, height = 6, dpi = 150)

# =====================================================================
# 6. EXPORTACION
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

saveRDS(medias_entidad,     here("salidas", "entidades_medias.rds"))
saveRDS(ranking,            here("salidas", "entidades_ranking.rds"))
saveRDS(tabla_cor_entidades, here("salidas", "entidades_correlaciones.rds"))

write_xlsx(
  list(
    medias_entidad      = medias_entidad,
    resumen_medias      = resumen_medias,
    ranking_top_bottom  = ranking,
    correlaciones       = tabla_cor_entidades,
    matriz_correlacion  = as.data.frame(round(cor_entidades, 3)) %>%
      rownames_to_column("constructo")
  ),
  here("salidas", "Actividad14_Entidades.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad14_Entidades.xlsx\n")
cat("salidas/entidades_medias.rds\n")
cat("salidas/entidades_ranking.rds\n")
cat("salidas/entidades_correlaciones.rds\n")
cat("figuras/act14_histograma_entidades.png\n")
cat("figuras/act14_boxplot_entidades.png\n")
cat("figuras/act14_dispersion_entidades.png\n\n")

cat("=== NOTA PARA EL ARTICULO ===\n")
cat("Se calcularon medias por entidad (n = 201) ponderadas por el factor\n")
cat("de expansion, para comparar la percepcion entre entidades. La\n")
cat("correlacion Liderazgo-Satisfaccion a nivel de entidad fue r =",
    round(cor_test_lid_sat$estimate, 3), ".\n")

sessionInfo()