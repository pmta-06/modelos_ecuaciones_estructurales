# =====================================================================
# ACTIVIDAD 7: Punto de partida de la Fase 2 (A1 del Taller 2)
# Taller 2 - Fases 2 y 3 | SEM | USTA
# =====================================================================
# Objetivo (A1 del Taller 2):
#   - Retomar la base analítica, el diagrama, las hipótesis y la sintaxis
#     del Taller 1.
#   - DECIDIR Y JUSTIFICAR:
#       (i)   el estimador,
#       (ii)  el tratamiento de faltantes,
#       (iii) el uso del diseño muestral (FEXPFINAL + CUIN),
#       (iv)  la identificación de la escala de cada factor.
#   - Dejar un objeto 'setup' con TODO lo que necesitan las Actividades 8-15.
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(writexl)
library(lavaan)
library(semTools)

set.seed(2026)

# 1. CARGA DE INSUMOS DE LA FASE 1 -------------------------------------
# Preferimos el .rds (más rápido y preserva tipos); si no existe, caemos
# al Excel final del Taller 1.
ruta_rds   <- here("salidas", "base_analitica.rds")
ruta_xlsx  <- here("salidas", "Taller1_SEM_base.xlsx")

if (file.exists(ruta_rds)) {
  base <- readRDS(ruta_rds)
  origen_base <- "salidas/base_analitica.rds"
} else if (file.exists(ruta_xlsx)) {
  base <- readxl::read_xlsx(ruta_xlsx, sheet = "datos")
  origen_base <- "salidas/Taller1_SEM_base.xlsx (hoja 'datos')"
} else {
  stop("No se encontró la base del Taller 1. Corre primero la Fase 1.")
}

cat("=== BASE CARGADA ===\n")
cat("Origen:", origen_base, "\n")
cat("Filas:", nrow(base), "| Columnas:", ncol(base), "\n\n")

# 2. DEFINICIÓN DE ÍTEMS, FACTORES Y MODELO ----------------------------
# 19 indicadores analíticos (C03F excluido en Fase 1, se conserva en la
# base pero NO entra al modelo).
items_liderazgo   <- paste0("C21", LETTERS[1:10])   # 10
items_clima       <- paste0("C18", LETTERS[1:4])    # 4
items_satisf      <- paste0("C03", LETTERS[1:5])    # 5 (C03A..C03E)
items_constructos <- c(items_liderazgo, items_clima, items_satisf)

# Verificación de existencia y tipo
faltan <- setdiff(c(items_constructos, "FEXPFINAL", "CUIN", "B04"),
                  names(base))
if (length(faltan) > 0) {
  stop("Faltan columnas en la base: ", paste(faltan, collapse = ", "))
}

# Forzar numérico en ítems (por si Excel los trajo como character)
base <- base %>%
  mutate(across(all_of(items_constructos), as.numeric))

# Modelo de medición (AFC saturado: 3 factores correlacionados)
# Se usa el MISMO modelo que la sintaxis del Taller 1 (indicador marcador).
modelo_medicion <- '
  Liderazgo    =~ C21A + C21B + C21C + C21D + C21E +
                  C21F + C21G + C21H + C21I + C21J
  Clima        =~ C18A + C18B + C18C + C18D
  Satisfaccion =~ C03A + C03B + C03C + C03D + C03E
'

# Modelo estructural propuesto M1 (para Actividad 12).
# IMPORTANTE (Corrección 1): incluye la covarianza residual C03D ~~ C03E
# liberada en la Actividad 9. Así el modelo de medición de M1 es idéntico
# al AFC final (148 gl) y el contraste AFC vs M1 no confunde la restricción
# estructural con la covarianza omitida.
modelo_estructural_m1 <- '
  Liderazgo    =~ C21A + C21B + C21C + C21D + C21E +
                  C21F + C21G + C21H + C21I + C21J
  Clima        =~ C18A + C18B + C18C + C18D
  Satisfaccion =~ C03A + C03B + C03C + C03D + C03E

  # Covarianza residual justificada en la Actividad 9 (C03D ~~ C03E)
  C03D ~~ C03E

  Clima        ~ a*Liderazgo
  Satisfaccion ~ b*Clima + c*Liderazgo

  indirecto := a*b
  directo   := c
  total     := c + a*b
'

# 3. DECISIÓN (i): ESTIMADOR -------------------------------------------
# Justificación:
#   - Ítems ordinales Likert 1-5.
#   - Mardia rechazó normalidad multivariada (p < 0.001) en Fase 1.
#   - Li (2016): con ordinales y no normalidad, WLSMV supera a ML.
#   - Taller 2, Tabla 1: "Ordinales ... WLSMV".
estimador_elegido <- "WLSMV"

# 4. DECISIÓN (ii): TRATAMIENTO DE FALTANTES ---------------------------
# Justificación:
#   - Little rechazó MCAR (p < 0.001) → MAR/MNAR plausible.
#   - Con WLSMV, lavaan NO implementa FIML; usa eliminación por pares
#     (pairwise) por defecto. El Taller 2 lo admite explícitamente:
#     "eliminación por pares con WLSMV".
#   - Solo ~21 casos con 100% NA; impacto despreciable.
tratamiento_faltantes <- "pairwise (default de lavaan con WLSMV)"

# 5. DECISIÓN (iii): DISEÑO MUESTRAL -----------------------------------
# La EDI es una encuesta compleja: estratos (CODSECTOR, CODGRUPO),
# factor de expansión (FEXPFINAL) y conglomerados (entidades vía CUIN).
# lavaan permite incorporarlo con sampling.weights y cluster.
# NOTA: CUIN identifica ENTIDAD (no servidor); se usa como cluster.
#       Hay 1.711 vacíos en CUIN. Para el cluster necesitamos un ID de
#       entidad sin NA: usamos COD_ENT si existe y, si no, un factor
#       creado a partir de CUIN con un nivel "SIN_ENTIDAD" para los NA.
usar_diseno_muestral <- TRUE

if (usar_diseno_muestral) {
  # Cluster de entidad robusto a NA
  if ("COD_ENT" %in% names(base)) {
    base <- base %>% mutate(.cluster_entidad = as.character(COD_ENT))
  } else {
    base <- base %>%
      mutate(.cluster_entidad = ifelse(is.na(CUIN) | CUIN == "",
                                       "SIN_ENTIDAD", as.character(CUIN)))
  }
  # Peso de expansión: NA o <=0 → 1 (no expandir ese caso)
  base <- base %>%
    mutate(.peso_exp = ifelse(is.na(FEXPFINAL) | FEXPFINAL <= 0,
                              1, FEXPFINAL))
  
  cat("=== DISEÑO MUESTRAL ===\n")
  cat("Peso (FEXPFINAL): media =", round(mean(base$.peso_exp), 2),
      "| min =", round(min(base$.peso_exp), 2),
      "| max =", round(max(base$.peso_exp), 2), "\n")
  cat("Clusters de entidad únicos:", length(unique(base$.cluster_entidad)), "\n")
  cat("Casos con CUIN vacío (cluster = SIN_ENTIDAD):",
      sum(base$.cluster_entidad == "SIN_ENTIDAD"), "\n\n")
} else {
  base <- base %>% mutate(.cluster_entidad = "NA", .peso_exp = 1)
}

# 6. DECISIÓN (iv): IDENTIFICACIÓN DE ESCALA ---------------------------
# Se usa INDICADOR MARCADOR (C21A=1, C18A=1, C03A=1) para mantener
# coherencia con la notación LISREL del Taller 1 (Actividad 6).
# Equivale a std.lv = FALSE en lavaan (default).
identificacion_escala <- "indicador marcador (std.lv = FALSE)"

# 7. VERIFICACIONES PREVIAS A LA ESTIMACIÓN ----------------------------
# 7.1 Rangos válidos
fuera_rango <- base %>%
  summarise(across(all_of(items_constructos),
                   ~ sum(!is.na(.x) & (.x < 1 | .x > 5)))) %>%
  pivot_longer(everything(), names_to = "item", values_to = "n_fuera") %>%
  filter(n_fuera > 0)
if (nrow(fuera_rango) == 0) {
  cat("OK: ningún ítem con valores fuera de 1-5.\n")
} else {
  warning("Ítems con valores fuera de 1-5: ",
          paste(fuera_rango$item, collapse = ", "))
}

# 7.2 Faltantes por ítem (resumen)
falt_por_item <- base %>%
  summarise(across(all_of(items_constructos),
                   ~ round(mean(is.na(.x)) * 100, 2))) %>%
  pivot_longer(everything(), names_to = "item", values_to = "pct_NA") %>%
  arrange(desc(pct_NA))
cat("Rango de % faltantes por ítem:",
    min(falt_por_item$pct_NA), "a", max(falt_por_item$pct_NA), "%\n")

# 7.3 Casos con 100% NA en los 19 ítems
n_100na <- sum(rowSums(!is.na(base[items_constructos])) == 0)
cat("Casos con 100% NA en los 19 ítems:", n_100na, "\n\n")

# 7.4 Grupos para invarianza (B04)
tabla_b04 <- base %>% count(B04, name = "n") %>% arrange(B04)
cat("=== GRUPOS PARA INVARIANZA (B04) ===\n")
print(tabla_b04)
cat("¿Ambos grupos >= 200?:",
    all(tabla_b04$n >= 200, na.rm = TRUE), "\n\n")

# 8. TABLA DE DECISIONES METODOLÓGICAS (A1) ----------------------------
decisiones <- tibble(
  Decision = c(
    "Estimador",
    "Tipo de indicador",
    "Tratamiento de faltantes",
    "Diseño muestral: peso",
    "Diseño muestral: cluster",
    "Identificación de escala",
    "Ítems del modelo",
    "Ítem excluido",
    "Variable de agrupación (invarianza)",
    "Semilla"
  ),
  Eleccion = c(
    estimador_elegido,
    "Ordinal (Likert 1-5) → ordered = TRUE",
    tratamiento_faltantes,
    "sampling.weights = FEXPFINAL (NA/<=0 → 1)",
    "cluster = COD_ENT (o CUIN con 'SIN_ENTIDAD' para NA)",
    identificacion_escala,
    paste0(length(items_constructos), " (",
           length(items_liderazgo), " Liderazgo + ",
           length(items_clima), " Clima + ",
           length(items_satisf), " Satisfacción)"),
    "C03F (sentido inverso; excluido del constructo en Fase 1)",
    "B04 (sexo: 1=Hombre, 2=Mujer)",
    "2026"
  ),
  Justificacion = c(
    "Li (2016); Mardia rechazó normalidad multivariada (Taller 1); Taller 2 Tabla 1",
    "Ítems Likert 1-5; AFE con policóricas en Fase 1",
    "Little rechazó MCAR; WLSMV no admite FIML en lavaan; pérdida mínima (~21 casos 100% NA)",
    "La EDI es encuesta compleja; FEXPFINAL corrige representatividad",
    "CUIN identifica entidad (no servidor); errores robustos a conglomeración (Oberski, 2014)",
    "Coherencia con notación LISREL del Taller 1 (Actividad 6); C21A=C18A=C03A=1",
    "Estructura factorial validada por AFE (73.5% varianza) en Fase 1",
    "Mide sobrecarga/exigencia (sentido inverso), no satisfacción",
    "Única sociodemográfica disponible (B02, B03, B05-B07 ausentes); grupos >200",
    "Reproducibilidad de bootstrap y split calibración/validación"
  )
)

cat("=== DECISIONES METODOLÓGICAS (A1) ===\n")
print(decisiones, n = Inf)
cat("\n")

# 9. SPLIT CALIBRACIÓN / VALIDACIÓN (para Actividad 9) -----------------
# El Taller 2 (A3) sugiere dividir la muestra si N lo permite. N=26.742
# lo permite sobradamente. Guardamos los índices para reusarlos.
set.seed(2026)
idx_calib <- sample(seq_len(nrow(base)), size = floor(0.7 * nrow(base)))
base <- base %>%
  mutate(.split = ifelse(row_number() %in% idx_calib,
                         "calibracion", "validacion"))

cat("=== SPLIT CALIBRACIÓN / VALIDACIÓN ===\n")
print(base %>% count(.split))
cat("\n")

# 10. EMPAQUETAR TODO EN UN OBJETO 'setup' -----------------------------
setup <- list(
  base                = base,
  items_liderazgo     = items_liderazgo,
  items_clima         = items_clima,
  items_satisf        = items_satisf,
  items_constructos   = items_constructos,
  modelo_medicion     = modelo_medicion,
  modelo_estructural_m1 = modelo_estructural_m1,
  estimador           = estimador_elegido,
  faltantes           = tratamiento_faltantes,
  usar_diseno         = usar_diseno_muestral,
  identificacion      = identificacion_escala,
  grupo_invarianza    = "B04",
  semilla             = 2026,
  fecha_setup         = Sys.Date()
)

# 11. EXPORTACIÓN ------------------------------------------------------
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

saveRDS(setup, here("salidas", "fase2_setup.rds"))

write_xlsx(
  list(
    decisiones        = decisiones,
    faltantes_item    = falt_por_item,
    grupos_invarianza = tabla_b04,
    split             = base %>% count(.split)
  ),
  here("salidas", "Actividad7_Decisiones_Metodologicas.xlsx")
)

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/fase2_setup.rds\n")
cat("salidas/Actividad7_Decisiones_Metodologicas.xlsx\n\n")

cat("=== RESUMEN DEL SETUP ===\n")
cat("Estimador:", setup$estimador, "\n")
cat("Faltantes:", setup$faltantes, "\n")
cat("Diseño muestral:", setup$usar_diseno, "\n")
cat("Identificación:", setup$identificacion, "\n")
cat("Ítems:", length(setup$items_constructos), "\n")

sessionInfo()
