# =====================================================================
# ACTIVIDAD 2: Construcción de la base analítica (CORREGIDO)
# Taller 1 - Fase 1 | SEM | USTA
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(writexl)

set.seed(2026)

# 1. LECTURA DE INSUMOS DE LA ACTIVIDAD 1 ------------------------------
ficha_tecnica <- readRDS(here("salidas", "ficha_tecnica.rds"))
indicadores   <- readRDS(here("salidas", "indicadores.rds"))
hipotesis     <- readRDS(here("salidas", "hipotesis.rds"))
referencias   <- readRDS(here("salidas", "referencias.rds"))

# 2. LECTURA DEL ARCHIVO CRUDO -----------------------------------------
crudos <- read_xlsx(here("excel", "edi2025_entidad.xlsx"), sheet = 1)

cat("=== ESTADO INICIAL ===\n")
cat("Filas crudas:", nrow(crudos), "\n")
cat("Columnas crudas:", ncol(crudos), "\n\n")

flujo_registros <- tibble(
  Paso       = "0. Base cruda",
  n          = nrow(crudos),
  Eliminados = NA_integer_,
  Motivo     = "Archivo original descargado del DANE"
)

# 3. VERIFICACIÓN DE COLUMNAS NECESARIAS -------------------------------
vars_a_usar <- c(
  "COD_ENT", "CUIN", "B04",
  "CODSECTOR", "CODGRUPO", "FEXPFINAL",
  paste0("C21", LETTERS[1:10]),
  paste0("C18", LETTERS[1:4]),
  paste0("C03", LETTERS[1:6])
)

faltantes_cols <- setdiff(vars_a_usar, names(crudos))
if (length(faltantes_cols) > 0) {
  stop("Faltan columnas en el archivo: ",
       paste(faltantes_cols, collapse = ", "))
}
cat("OK: todas las columnas necesarias existen en el archivo.\n\n")

# 4. SELECCIÓN DE VARIABLES --------------------------------------------
base <- crudos %>%
  select(
    COD_ENT, CUIN, B04,
    CODSECTOR, CODGRUPO, FEXPFINAL,
    C21A, C21B, C21C, C21D, C21E,
    C21F, C21G, C21H, C21I, C21J,
    C18A, C18B, C18C, C18D,
    C03A, C03B, C03C, C03D, C03E, C03F
  )

cat("=== TRAS SELECCIÓN ===\n")
cat("Filas:", nrow(base), "| Columnas:", ncol(base), "\n\n")

flujo_registros <- flujo_registros %>%
  add_row(
    Paso       = "1. Selección de variables",
    n          = nrow(base),
    Eliminados = 0L,
    Motivo     = "Se conservan todos los registros; solo se reducen columnas"
  )

# 5. FILTROS (reportando n por paso) -----------------------------------
# 5.1 Diagnóstico: CUIN NO es identificador de servidor, es de entidad
n_dup_cuin <- sum(duplicated(base$CUIN))
cat("CUIN duplicados (esperado: hay varios servidores por entidad):", 
    n_dup_cuin, "\n")
cat("Entidades únicas:", length(unique(base$CUIN)), "\n\n")

# NO SE ELIMINAN LOS DUPLICADOS. Cada fila es un servidor diferente.
flujo_registros <- flujo_registros %>%
  add_row(
    Paso       = "2. Diagnóstico CUIN",
    n          = nrow(base),
    Eliminados = 0L,
    Motivo     = "CUIN identifica entidad, no servidor; se conservan todos"
  )

# 5.2 Filtro: sexo reportado (B04 no NA)
n_antes <- nrow(base)
base <- base %>% filter(!is.na(B04))
flujo_registros <- flujo_registros %>%
  add_row(
    Paso       = "3. Filtro: B04 (sexo) reportado",
    n          = nrow(base),
    Eliminados = n_antes - nrow(base),
    Motivo     = "Sociodemográfica clave sin dato"
  )

# 5.3 Filtro: al menos 1 ítem respondido
items_constructos <- c(
  paste0("C21", LETTERS[1:10]),
  paste0("C18", LETTERS[1:4]),
  paste0("C03", LETTERS[1:6])
)

n_antes <- nrow(base)
base <- base %>%
  filter(rowSums(!is.na(across(all_of(items_constructos)))) > 0)
flujo_registros <- flujo_registros %>%
  add_row(
    Paso       = "4. Filtro: al menos 1 ítem respondido",
    n          = nrow(base),
    Eliminados = n_antes - nrow(base),
    Motivo     = "Patrón vacío en todos los indicadores"
  )

cat("\n=== FLUJO DE REGISTROS ===\n")
print(flujo_registros)
cat("\n")

# 6. RECODIFICACIÓN DE CÓDIGOS ESPECIALES ------------------------------
codigos_especiales <- c(9, 99, 98, 88, -1)

base <- base %>%
  mutate(
    across(
      all_of(items_constructos),
      ~ ifelse(.x %in% codigos_especiales, NA, .x)
    )
  )

restantes <- base %>%
  summarise(across(all_of(items_constructos),
                   ~ sum(.x %in% codigos_especiales, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "item", values_to = "n_cod_esp") %>%
  filter(n_cod_esp > 0)

if (nrow(restantes) == 0) {
  cat("OK: no quedan códigos especiales en los ítems.\n\n")
} else {
  print(restantes)
}

# 7. ÍTEMS INVERSOS ----------------------------------------------------
# Ninguno en este modelo (C03F excluido del constructo).

# 8. VERIFICACIÓN DE RANGOS VÁLIDOS (Corregido el regex) ---------------
rangos <- base %>%
  summarise(across(
    all_of(items_constructos),
    list(
      min    = ~ min(.x, na.rm = TRUE),
      max    = ~ max(.x, na.rm = TRUE),
      n_NA   = ~ sum(is.na(.x)),
      pct_NA = ~ round(mean(is.na(.x)) * 100, 2)
    )
  )) %>%
  pivot_longer(everything(),
               names_to = c("item", "stat"),
               names_pattern = "^([^_]+)_(.+)$") %>%  # <- Corrección aquí
  pivot_wider(names_from = stat, values_from = value)

print(rangos, n = Inf)
cat("\n")

# 9. DICCIONARIO DE VARIABLES ------------------------------------------
diccionario_extra <- tribble(
  ~nombre, ~etiqueta, ~enunciado, ~constructo, ~tipo, ~categorias_codigos,
  ~sentido, ~recodificacion, ~nombre_original,
  
  "COD_ENT", "Código de entidad", "Código de la entidad pública", "Identificación",
  "Carácter", "Código asignado por DANE", "N/A", "Sin cambio", "COD_ENT",
  "CUIN", "Identificador de unidad", "Identificador único de la unidad de observación",
  "Identificación", "Carácter", "Único", "N/A", "Sin cambio", "CUIN",
  "CODSECTOR", "Código sector", "Código sector institucional", "Diseño",
  "Carácter", "1..32", "N/A", "Sin cambio", "CODSECTOR",
  "CODGRUPO", "Código grupo funcional", "Código grupo funcional", "Diseño",
  "Carácter", "1..20", "N/A", "Sin cambio", "CODGRUPO",
  "FEXPFINAL", "Factor de expansión", "Factor de expansión final corregido",
  "Diseño", "Continuo", "> 0", "N/A", "Sin cambio", "FEXPFINAL",
  "B04", "Sexo", "Sexo del servidor", "Sociodemográfico",
  "Nominal", "1=Hombre; 2=Mujer", "N/A", "Sin cambio", "B04"
)

diccionario_items <- indicadores %>%
  transmute(
    nombre             = Codigo,
    etiqueta           = Enunciado,
    enunciado          = Enunciado,
    constructo         = Constructo,
    tipo               = "Ordinal",
    categorias_codigos = Escala,
    sentido            = Sentido,
    recodificacion     = "9,99,98,88,-1 -> NA",
    nombre_original    = Codigo
  )

diccionario <- bind_rows(diccionario_extra, diccionario_items)

cat("=== DICCIONARIO (", nrow(diccionario), "filas ) ===\n")
print(diccionario, n = Inf)
cat("\n")

# 10. METADATOS --------------------------------------------------------
metadatos <- bind_rows(
  ficha_tecnica %>% rename(campo = Campo, valor = Valor),
  tibble(
    campo = c(
      "n crudo", "n tras filtros", "Variables en base analítica",
      "Códigos especiales recodificados", "Ítems inversos aplicados",
      "Limitación: sociodemográficas ausentes"
    ),
    valor = c(
      as.character(flujo_registros$n[1]),
      as.character(tail(flujo_registros$n, 1)),
      as.character(ncol(base)),
      "9, 99, 98, 88, -1 -> NA",
      "Ninguno (C03F excluido del constructo)",
      "La base real no trae B02, B03, B05, B06, B07; solo B04 (sexo)"
    )
  )
)

cat("=== METADATOS ===\n")
print(metadatos, n = Inf)
cat("\n")

# 11. EXPORTACIÓN ------------------------------------------------------
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

write_xlsx(
  list(
    datos       = base,
    diccionario = diccionario,
    metadatos   = metadatos,
    flujo       = flujo_registros
  ),
  here("salidas", "Actividad2_Base_Analitica.xlsx")
)

saveRDS(base,            here("salidas", "base_analitica.rds"))
saveRDS(diccionario,     here("salidas", "diccionario.rds"))
saveRDS(metadatos,       here("salidas", "metadatos.rds"))
saveRDS(flujo_registros, here("salidas", "flujo_registros.rds"))

cat("=== ARCHIVOS GENERADOS ===\n")
cat("salidas/Actividad2_Base_Analitica.xlsx  (hojas: datos, diccionario, metadatos, flujo)\n")
cat("salidas/base_analitica.rds\n")
cat("salidas/diccionario.rds\n")
cat("salidas/metadatos.rds\n")
cat("salidas/flujo_registros.rds\n")

sessionInfo()