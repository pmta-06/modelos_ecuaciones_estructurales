# =====================================================================
# ACTIVIDAD 21: Libro de resultados unificado — v2
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# CORRECCION v2:
#   - Hoja 'medicion' del libro obligatorio ahora incluye CR y AVE
#     (el Taller 2 pide "cargas, CR, AVE").
#   - Se mantiene la estructura de 6 hojas obligatorias + complementarias.
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(writexl)

cat("=== ACTIVIDAD 21: LIBRO DE RESULTADOS ===\n\n")

# =====================================================================
# 1. FUNCION AUXILIAR PARA LEER HOJAS DE EXCEL
# =====================================================================
leer_hoja <- function(archivo, hoja) {
  ruta <- here("salidas", archivo)
  if (!file.exists(ruta)) {
    cat("AVISO: no existe", archivo, "- se omite la hoja", hoja, "\n")
    return(tibble())
  }
  tryCatch(
    readxl::read_xlsx(ruta, sheet = hoja),
    error = function(e) {
      cat("AVISO: no se pudo leer", hoja, "de", archivo, "\n")
      tibble()
    }
  )
}

# =====================================================================
# 2. HOJA 1: AJUSTE
# =====================================================================
cat("1. Construyendo hoja 'ajuste'...\n")

# Correccion 2 y 5: la hoja 'ajuste' debe mostrar el MODELO FINAL
# (con C03D ~~ C03E, 148 gl), no el AFC sin reespecificar de la Actividad 8.
ruta_ajuste_final <- here("salidas", "Actividad9_Ajuste_Local.xlsx")
if (file.exists(ruta_ajuste_final)) {
  ajuste_afc <- leer_hoja("Actividad9_Ajuste_Local.xlsx", "ajuste_final") %>%
    mutate(modelo = "AFC final reespecificado (Actividad 9)")
} else {
  ajuste_afc <- leer_hoja("Actividad8_AFC.xlsx", "ajuste") %>%
    mutate(modelo = "AFC (Actividad 8)")
  warning("No se encontro Actividad9_Ajuste_Local.xlsx: se usa el AFC de la Actividad 8.")
}

ajuste_m1     <- leer_hoja("Actividad12_Estructural.xlsx", "ajuste_m1") %>%
  mutate(modelo = "M1 estructural (Actividad 12)")
ajuste_alt    <- leer_hoja("Actividad16_Alternativos.xlsx", "ajuste_comparativo")
ajuste_inv_sexo <- leer_hoja("Actividad17_Invarianza_Medicion.xlsx",
                             "tabla_invarianza") %>%
  mutate(modelo = paste("Invarianza sexo:", Modelo))
ajuste_inv_sector <- leer_hoja("Actividad19_Invarianza_Sector_v11.xlsx",
                               "tabla_ajuste") %>%
  mutate(modelo = paste("Invarianza sector:", Modelo))

# La hoja 'ajuste' se escribe en formato LARGO y homogeneo, para que el
# articulo pueda leerla directamente:
#   modelo | indice | valor | robusto | criterio
# (antes era una tabla ancha de 48 columnas con bloques prefijados y NAs)
largo_indices <- function(df, indices) {
  df %>%
    select(modelo, all_of(indices)) %>%
    pivot_longer(-modelo, names_to = "indice", values_to = "valor")
}

indices_alt <- c("chi2", "gl", "p", "CFI", "TLI", "RMSEA", "RMSEA_inf",
                 "RMSEA_sup", "SRMR", "CFI_robusto", "RMSEA_robusto")
indices_inv <- c("chi2", "gl", "p", "CFI", "TLI", "RMSEA", "RMSEA_inf",
                 "RMSEA_sup", "SRMR")

ajuste_afc_largo <- ajuste_afc %>%
  transmute(modelo, indice = Indice, valor = Valor,
            robusto = Robusto, criterio = Criterio)
ajuste_m1_largo <- ajuste_m1 %>%
  transmute(modelo, indice = Indice, valor = Valor,
            robusto = Robusto, criterio = Criterio)
ajuste_alt_largo <- ajuste_alt %>%
  select(Modelo, all_of(indices_alt)) %>%
  pivot_longer(-Modelo, names_to = "indice", values_to = "valor") %>%
  mutate(modelo = paste("Alternativo:", Modelo), robusto = NA_real_,
         criterio = NA_character_) %>%
  select(modelo, indice, valor, robusto, criterio)
ajuste_inv_largo <- bind_rows(
  largo_indices(ajuste_inv_sexo, indices_inv),
  largo_indices(ajuste_inv_sector, indices_inv)
) %>%
  mutate(robusto = NA_real_, criterio = NA_character_)

hoja_ajuste <- bind_rows(ajuste_afc_largo, ajuste_m1_largo,
                         ajuste_alt_largo, ajuste_inv_largo) %>%
  mutate(valor = round(as.numeric(valor), 4))

cat("   Filas:", nrow(hoja_ajuste), "\n")

# =====================================================================
# 3. HOJA 2: MEDICION (cargas + CR + AVE)
# =====================================================================
cat("2. Construyendo hoja 'medicion'...\n")

cargas_afc   <- leer_hoja("Actividad8_AFC.xlsx", "cargas")
cr_ave_afc   <- leer_hoja("Actividad10_Confiabilidad_Validez.xlsx", "cr_ave")
cargas_m1    <- leer_hoja("Actividad12_Estructural.xlsx", "coeficientes") %>%
  filter(op == "=~") %>%
  select(lhs, op, rhs, est, se, z, pvalue, std.all)

# Hoja medicion completa: cargas AFC + CR/AVE
medicion_completa <- bind_rows(
  cargas_afc %>% mutate(tipo = "Cargas AFC"),
  cr_ave_afc %>% mutate(tipo = "CR/AVE"),
  cargas_m1 %>% mutate(tipo = "Cargas M1")
)

cat("   Cargas AFC:", nrow(cargas_afc),
    "| CR/AVE:", nrow(cr_ave_afc),
    "| Cargas M1:", nrow(cargas_m1), "\n")

# =====================================================================
# 4. HOJA 3: DISCRIMINANTE (Fornell-Larcker + HTMT)
# =====================================================================
cat("3. Construyendo hoja 'discriminante'...\n")

fornell <- leer_hoja("Actividad10_Confiabilidad_Validez.xlsx", "fornell_larcker")
htmt    <- leer_hoja("Actividad10_Confiabilidad_Validez.xlsx", "htmt")

hoja_discriminante <- bind_rows(
  fornell %>% mutate(tipo = "Fornell-Larcker"),
  htmt %>% mutate(tipo = "HTMT")
)

cat("   Fornell:", nrow(fornell), "| HTMT:", nrow(htmt), "\n")

# =====================================================================
# 5. HOJA 4: HIPOTESIS
# =====================================================================
cat("4. Construyendo hoja 'hipotesis'...\n")

hipotesis_m1 <- leer_hoja("Actividad12_Estructural.xlsx", "hipotesis")

cat("   Filas:", nrow(hipotesis_m1), "\n")

# =====================================================================
# 6. HOJA 5: EFECTOS
# =====================================================================
cat("5. Construyendo hoja 'efectos'...\n")

efectos_mc  <- leer_hoja("Actividad13_Efectos_Mediacion_MC.xlsx", "efectos_montecarlo")
efectos_del <- leer_hoja("Actividad13_Efectos_Mediacion_MC.xlsx", "efectos_delta")
comparacion <- leer_hoja("Actividad13_Efectos_Mediacion_MC.xlsx", "comparacion")

hoja_efectos <- bind_rows(
  efectos_mc %>% mutate(tipo = "Monte Carlo"),
  efectos_del %>% mutate(tipo = "Delta")
)

cat("   Efectos MC:", nrow(efectos_mc), "| Delta:", nrow(efectos_del), "\n")

# Bootstrap por conglomerados (Actividad 13b): analisis de sensibilidad
boot_cluster <- leer_hoja("Actividad13b_Bootstrap_Cluster.xlsx",
                          "efectos_bootstrap")
cat("   Bootstrap cluster:", nrow(boot_cluster), "\n")

# =====================================================================
# 7. HOJA 6: INVARIANZA (sexo + sector)
# =====================================================================
cat("6. Construyendo hoja 'invarianza'...\n")

inv_sexo <- leer_hoja("Actividad17_Invarianza_Medicion.xlsx", "tabla_invarianza") %>%
  mutate(
    grupo = "Sexo (B04)",
    decision = as.character(decision),
    umbral_SRMR = criterio_SRMR_umbral
  )

inv_sector <- leer_hoja("Actividad19_Invarianza_Sector_v11.xlsx",
                        "tabla_ajuste") %>%
  mutate(
    grupo = "Sector (CODSECTOR)",
    decision = as.character(decision_Chen),
    umbral_SRMR = NA_real_
  )

# Hoja 'invarianza' en formato largo homogeneo (misma estructura para los
# dos grupos): secuencia de modelos y diferencias en los indices.
cols_inv <- c("modelo", "chi2", "gl", "p", "CFI", "TLI", "RMSEA",
              "RMSEA_inf", "RMSEA_sup", "SRMR",
              "delta_CFI", "delta_RMSEA", "delta_SRMR", "umbral_SRMR",
              "decision")
renombrar_inv <- function(df, nombre_grupo) {
  df %>% mutate(modelo = Modelo, grupo = .env$nombre_grupo) %>%
    select(grupo, all_of(cols_inv))
}

hoja_invarianza <- bind_rows(
  renombrar_inv(inv_sexo,   "Sexo (B04)"),
  renombrar_inv(inv_sector, "Sector (CODSECTOR)")
) %>%
  mutate(across(c(chi2, CFI, TLI, RMSEA, RMSEA_inf, RMSEA_sup, SRMR,
                  delta_CFI, delta_RMSEA, delta_SRMR, umbral_SRMR),
                ~ round(as.numeric(.x), 3)))

cat("   Filas:", nrow(hoja_invarianza), "\n")

# =====================================================================
# 8. HOJA 7: ICC (Actividad 10 + Actividad 20)
# =====================================================================
cat("7. Construyendo hoja 'icc'...\n")

icc_act10 <- leer_hoja("Actividad10_Confiabilidad_Validez.xlsx", "icc") %>%
  mutate(fuente = "Actividad 10 (lme4)")
icc_act20 <- leer_hoja("Actividad20_CFA_Multinivel.xlsx", "icc") %>%
  mutate(fuente = "Actividad 20 (CFA multinivel)")

hoja_icc <- bind_rows(
  icc_act10 %>% rename_with(~ paste0("act10_", .), -fuente),
  icc_act20 %>% rename_with(~ paste0("act20_", .), -fuente)
)

cat("   Filas:", nrow(hoja_icc), "\n")

# =====================================================================
# 9. HOJA 8: SENSIBILIDAD (Actividad 11)
# =====================================================================
cat("8. Construyendo hoja 'sensibilidad'...\n")

sens_cargas <- leer_hoja("Actividad11_Sensibilidad_Diseno.xlsx",
                         "comparacion_cargas")
sens_ajuste <- leer_hoja("Actividad11_Sensibilidad_Diseno.xlsx",
                         "comparacion_ajuste")

cat("   Cargas:", nrow(sens_cargas), "| Ajuste:", nrow(sens_ajuste), "\n")

# =====================================================================
# 10. HOJA 9: ENTIDADES (Actividad 14)
# =====================================================================
cat("9. Construyendo hoja 'entidades'...\n")

ent_medias  <- leer_hoja("Actividad14_Entidades.xlsx", "medias_entidad")
ent_ranking <- leer_hoja("Actividad14_Entidades.xlsx", "ranking_top_bottom")
ent_cor     <- leer_hoja("Actividad14_Entidades.xlsx", "correlaciones")

cat("   Medias:", nrow(ent_medias), "| Ranking:", nrow(ent_ranking), "\n")

# =====================================================================
# 11. HOJA 10: PATRONES (Actividad 15)
# =====================================================================
cat("10. Construyendo hoja 'patrones'...\n")

pat_resumen <- leer_hoja("Actividad15_Patrones_Respuesta.xlsx",
                         "resumen_entidad")
pat_entidad <- leer_hoja("Actividad15_Patrones_Respuesta.xlsx",
                         "patrones_entidad")
pat_valores <- leer_hoja("Actividad15_Patrones_Respuesta.xlsx",
                         "straight_valores")

cat("   Resumen:", nrow(pat_resumen), "| Entidades:", nrow(pat_entidad), "\n")

# =====================================================================
# 12. HOJA 11: INVARIANZA SEXO (Actividades 17-18)
# =====================================================================
cat("11. Construyendo hoja 'invarianza_sexo'...\n")

inv_sexo_med     <- leer_hoja("Actividad17_Invarianza_Medicion.xlsx",
                              "tabla_invarianza")
inv_sexo_wald    <- leer_hoja("Actividad18_Invarianza_Estructural.xlsx", "wald")
inv_sexo_med_obs <- leer_hoja("Actividad18_Invarianza_Estructural.xlsx",
                              "medias_diferencia")

cat("   Medicion:", nrow(inv_sexo_med), "| Wald:", nrow(inv_sexo_wald), "\n")

# =====================================================================
# 13. HOJA 12: INVARIANZA SECTOR (Actividad 19)
# =====================================================================
cat("12. Construyendo hoja 'invarianza_sector'...\n")

inv_sector_ajuste  <- leer_hoja("Actividad19_Invarianza_Sector_v11.xlsx",
                                "tabla_ajuste")
inv_sector_wald    <- leer_hoja("Actividad19_Invarianza_Sector_v11.xlsx", "wald")
inv_sector_wald_sc <- leer_hoja("Actividad19_Invarianza_Sector_v11.xlsx",
                                "wald_sin_cluster")
inv_sector_coefs   <- leer_hoja("Actividad19_Invarianza_Sector_v11.xlsx",
                                "coeficientes")

cat("   Ajuste:", nrow(inv_sector_ajuste), "| Wald:", nrow(inv_sector_wald), "\n")

# =====================================================================
# 14. HOJA 13: MULTINIVEL (Actividad 20)
# =====================================================================
cat("13. Construyendo hoja 'multinivel'...\n")

mn_icc   <- leer_hoja("Actividad20_CFA_Multinivel.xlsx", "icc")
mn_cor_w <- leer_hoja("Actividad20_CFA_Multinivel.xlsx", "cor_within")
mn_cor_b <- leer_hoja("Actividad20_CFA_Multinivel.xlsx", "cor_between")
mn_coefs <- leer_hoja("Actividad20_CFA_Multinivel.xlsx", "coefs_medicion")

cat("   ICC:", nrow(mn_icc), "| Cor within:", nrow(mn_cor_w), "\n")

# =====================================================================
# 15. LIBRO OBLIGATORIO (6 hojas)
# =====================================================================
cat("\n15. Escribiendo libro obligatorio (6 hojas)...\n")

libro_obligatorio <- list(
  ajuste         = hoja_ajuste,
  medicion       = medicion_completa,   # <-- AHORA INCLUYE CR Y AVE
  discriminante  = hoja_discriminante,
  hipotesis      = hipotesis_m1,
  efectos        = hoja_efectos,
  invarianza     = hoja_invarianza
)

write_xlsx(
  libro_obligatorio,
  here("salidas", "Taller2_SEM_resultados.xlsx")
)

cat("   OK: salidas/Taller2_SEM_resultados.xlsx\n")

# =====================================================================
# 16. LIBRO COMPLEMENTARIO (hojas adicionales)
# =====================================================================
cat("\n16. Escribiendo libro complementario...\n")

libro_complementario <- list(
  icc                 = hoja_icc,
  efectos_bootstrap   = boot_cluster,
  sensibilidad_cargas = sens_cargas,
  sensibilidad_ajuste = sens_ajuste,
  entidades_medias    = ent_medias,
  entidades_ranking   = ent_ranking,
  entidades_cor       = ent_cor,
  patrones_resumen    = pat_resumen,
  patrones_entidad    = pat_entidad,
  patrones_valores    = pat_valores,
  inv_sexo_medicion   = inv_sexo_med,
  inv_sexo_wald       = inv_sexo_wald,
  inv_sexo_medias     = inv_sexo_med_obs,
  inv_sector_ajuste   = inv_sector_ajuste,
  inv_sector_wald     = inv_sector_wald,
  inv_sector_wald_sc  = inv_sector_wald_sc,
  inv_sector_coefs    = inv_sector_coefs,
  multinivel_icc      = mn_icc,
  multinivel_cor_within = mn_cor_w,
  multinivel_cor_between = mn_cor_b,
  multinivel_coefs    = mn_coefs
)

write_xlsx(
  libro_complementario,
  here("salidas", "Taller2_SEM_resultados_complementarios.xlsx")
)

cat("   OK: salidas/Taller2_SEM_resultados_complementarios.xlsx\n")

# =====================================================================
# 17. RESUMEN FINAL
# =====================================================================
cat("\n=== RESUMEN FINAL ===\n")
cat("Libro obligatorio: Taller2_SEM_resultados.xlsx (6 hojas)\n")
cat("  - ajuste:", nrow(libro_obligatorio$ajuste), "filas\n")
cat("  - medicion:", nrow(libro_obligatorio$medicion),
    "filas (cargas + CR + AVE)\n")
cat("  - discriminante:", nrow(libro_obligatorio$discriminante), "filas\n")
cat("  - hipotesis:", nrow(libro_obligatorio$hipotesis), "filas\n")
cat("  - efectos:", nrow(libro_obligatorio$efectos), "filas\n")
cat("  - invarianza:", nrow(libro_obligatorio$invarianza), "filas\n\n")

cat("Libro complementario: Taller2_SEM_resultados_complementarios.xlsx\n")
cat("  - 20 hojas con analisis adicionales\n\n")

cat("=== ACTIVIDAD 21 COMPLETADA ===\n")

sessionInfo()