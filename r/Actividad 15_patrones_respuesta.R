# =====================================================================
# ACTIVIDAD 15: Patrones de respuesta (complemento A3/B1) — v3
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (observacion del profesor, punto 4): "Patron de respuestas".
#
# CORRECCION v3:
#   - straightliner = n_distintos == 1 & n_validos >= 10
#     (evita contar como straightliner a quien respondio 1-2 items).
#   - straight_valores se calcula dentro del flujo, sin left_join
#     many-to-many (que inflaba los conteos).
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
items_all <- setup$items_constructos

cat("=== INSUMOS CARGADOS ===\n")
cat("Filas:", nrow(base), "| Items:", length(items_all),
    "| Entidades:", n_distinct(base$.cluster_entidad), "\n\n")

# =====================================================================
# 1. PATRONES A NIVEL DE SERVIDOR
# =====================================================================
patrones_servidor <- base %>%
  mutate(
    n_validos     = rowSums(!is.na(across(all_of(items_all)))),
    n_distintos   = apply(select(., all_of(items_all)), 1,
                          function(x) length(unique(na.omit(x)))),
    # Definicion endurecida: straightliner si uso un solo valor y
    # respondio al menos 10 items (evita 1-2 items).
    straightliner = n_distintos == 1 & n_validos >= 10,
    n_45 = rowSums(across(all_of(items_all), ~ .x >= 4), na.rm = TRUE),
    n_15 = rowSums(across(all_of(items_all), ~ .x %in% c(1, 5)),
                   na.rm = TRUE),
    aquiescencia = ifelse(n_validos > 0, n_45 / n_validos, NA_real_),
    extremos     = ifelse(n_validos > 0, n_15 / n_validos, NA_real_),
    pct_NA       = (length(items_all) - n_validos) / length(items_all) * 100
  ) %>%
  select(.cluster_entidad, n_validos, n_distintos, straightliner,
         aquiescencia, extremos, pct_NA)

cat("=== RESUMEN A NIVEL DE SERVIDOR ===\n")
cat("Total servidores:", nrow(patrones_servidor), "\n")
cat("Straightliners (n_validos >= 10):",
    sum(patrones_servidor$straightliner, na.rm = TRUE),
    sprintf("(%.2f%%)\n",
            mean(patrones_servidor$straightliner, na.rm = TRUE) * 100))
cat("Aquiescencia media:",
    round(mean(patrones_servidor$aquiescencia, na.rm = TRUE), 3), "\n")
cat("Extremos medios:",
    round(mean(patrones_servidor$extremos, na.rm = TRUE), 3), "\n")
cat("Faltantes medios (%):",
    round(mean(patrones_servidor$pct_NA, na.rm = TRUE), 2), "\n\n")

# =====================================================================
# 1b. VALORES USADOS POR STRAIGHTLINERS (sin join many-to-many)
# =====================================================================
# Se calcula directamente sobre base, aplicando la misma definicion
# endurecida, y pivot_longer sobre los items.

straight_valores <- base %>%
  mutate(
    n_validos     = rowSums(!is.na(across(all_of(items_all)))),
    n_distintos   = apply(select(., all_of(items_all)), 1,
                          function(x) length(unique(na.omit(x)))),
    straightliner = n_distintos == 1 & n_validos >= 10
  ) %>%
  filter(straightliner) %>%
  select(all_of(items_all)) %>%
  pivot_longer(everything(), names_to = "item", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  count(valor)

cat("=== VALORES USADOS POR STRAIGHTLINERS (n_validos >= 10) ===\n")
print(straight_valores, n = Inf)
cat("\n")

# =====================================================================
# 2. PATRONES A NIVEL DE ENTIDAD
# =====================================================================
patrones_entidad <- patrones_servidor %>%
  group_by(.cluster_entidad) %>%
  summarise(
    n_servidores       = n(),
    pct_straightliner  = round(mean(straightliner, na.rm = TRUE) * 100, 2),
    aquiescencia_media = round(mean(aquiescencia, na.rm = TRUE), 3),
    extremos_media     = round(mean(extremos, na.rm = TRUE), 3),
    faltantes_media    = round(mean(pct_NA, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_straightliner))

cat("=== PATRONES POR ENTIDAD (top 10 straightlining) ===\n")
print(head(patrones_entidad, 10), n = Inf)
cat("\n")

cat("=== ENTIDADES CON MAS AQUIESCENCIA (top 10) ===\n")
print(patrones_entidad %>% arrange(desc(aquiescencia_media)) %>% head(10),
      n = Inf)
cat("\n")

resumen_entidad <- patrones_entidad %>%
  summarise(
    across(c(pct_straightliner, aquiescencia_media, extremos_media,
             faltantes_media),
           list(min = min, mediana = median, max = max, media = mean),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(), names_to = c("metrica", "stat"),
               names_pattern = "^(pct_straightliner|aquiescencia_media|extremos_media|faltantes_media)_(.+)$") %>%
  pivot_wider(names_from = stat, values_from = value)

cat("=== RESUMEN DE PATRONES POR ENTIDAD ===\n")
print(resumen_entidad, n = Inf)
cat("\n")

# =====================================================================
# 3. GRAFICOS
# =====================================================================
if (!dir.exists(here("figuras"))) dir.create(here("figuras"))

p_straight <- ggplot(patrones_entidad, aes(x = pct_straightliner)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.85) +
  geom_vline(xintercept = mean(patrones_entidad$pct_straightliner),
             linetype = "dashed", color = "red") +
  labs(title = "Porcentaje de straightliners por entidad",
       subtitle = paste("Linea roja: media =",
                        round(mean(patrones_entidad$pct_straightliner), 2), "%"),
       x = "% straightliners en la entidad", y = "Numero de entidades") +
  theme_minimal()
ggsave(here("figuras", "act15_straightlining_entidad.png"),
       p_straight, width = 8, height = 5, dpi = 150)

p_disp <- ggplot(patrones_entidad,
                 aes(x = aquiescencia_media, y = pct_straightliner)) +
  geom_point(aes(size = n_servidores), color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(title = "Aquiescencia vs straightlining por entidad",
       subtitle = "Tamano del punto = numero de servidores",
       x = "Aquiescencia media (proporcion de 4-5)",
       y = "% straightliners") +
  theme_minimal()
ggsave(here("figuras", "act15_aquiescencia_vs_straight.png"),
       p_disp, width = 8, height = 5, dpi = 150)

top15 <- patrones_entidad %>% slice_max(pct_straightliner, n = 15)

p_top <- ggplot(top15, aes(x = reorder(paste0("E", .cluster_entidad),
                                       pct_straightliner),
                           y = pct_straightliner)) +
  geom_col(fill = "tomato", alpha = 0.85) +
  coord_flip() +
  labs(title = "Top 15 entidades con mayor % de straightliners",
       x = "Entidad", y = "% straightliners") +
  theme_minimal()
ggsave(here("figuras", "act15_top15_straightliners.png"),
       p_top, width = 8, height = 6, dpi = 150)

p_aq <- ggplot(patrones_entidad, aes(x = aquiescencia_media)) +
  geom_histogram(bins = 20, fill = "seagreen", color = "white", alpha = 0.85) +
  geom_vline(xintercept = mean(patrones_entidad$aquiescencia_media),
             linetype = "dashed", color = "red") +
  labs(title = "Aquiescencia media por entidad",
       subtitle = "Proporcion de respuestas 4-5",
       x = "Aquiescencia media", y = "Numero de entidades") +
  theme_minimal()
ggsave(here("figuras", "act15_aquiescencia_entidad.png"),
       p_aq, width = 8, height = 5, dpi = 150)

# =====================================================================
# 4. EXPORTACION
# =====================================================================
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

saveRDS(patrones_servidor, here("salidas", "patrones_servidor.rds"))
saveRDS(patrones_entidad,  here("salidas", "patrones_entidad.rds"))
saveRDS(resumen_entidad,   here("salidas", "patrones_resumen.rds"))

write_xlsx(
  list(
    patrones_servidor   = patrones_servidor,
    patrones_entidad    = patrones_entidad,
    resumen_entidad     = resumen_entidad,
    straight_valores    = straight_valores,
    top15_straightliner = top15
  ),
  here("salidas", "Actividad15_Patrones_Respuesta.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad15_Patrones_Respuesta.xlsx\n")
cat("salidas/patrones_servidor.rds\n")
cat("salidas/patrones_entidad.rds\n")
cat("salidas/patrones_resumen.rds\n")
cat("figuras/act15_straightlining_entidad.png\n")
cat("figuras/act15_aquiescencia_vs_straight.png\n")
cat("figuras/act15_top15_straightliners.png\n")
cat("figuras/act15_aquiescencia_entidad.png\n\n")

cat("=== NOTA PARA EL ARTICULO ===\n")
cat("Se identificaron", sum(patrones_servidor$straightliner, na.rm = TRUE),
    "straightliners (",
    round(mean(patrones_servidor$straightliner, na.rm = TRUE) * 100, 2),
    "%) que respondieron al menos 10 items con el mismo valor.\n")
cat("La aquiescencia media fue",
    round(mean(patrones_servidor$aquiescencia, na.rm = TRUE), 3),
    "y los extremos medios",
    round(mean(patrones_servidor$extremos, na.rm = TRUE), 3), ".\n")
cat("Se reviso la distribucion por entidad para detectar patrones atipicos.\n")

sessionInfo()