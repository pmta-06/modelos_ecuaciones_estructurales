# =====================================================================
# 10. EXPORTACIÓN: Excel final con las 3 hojas obligatorias
# Taller 1 - Fase 1 | SEM | USTA  (se corre DESPUÉS de Actividad 1-6)
# Autocontenido: solo usa here() y los .rds de salidas/.
# =====================================================================
library(here); library(dplyr); library(tibble); library(writexl)
set.seed(2026)

# 1. Insumos generados por las Actividades 1-2 ------------------------
base       <- readRDS(here("salidas", "base_analitica.rds"))
diccionario <- readRDS(here("salidas", "diccionario.rds"))
flujo      <- readRDS(here("salidas", "flujo_registros.rds"))
ficha      <- readRDS(here("salidas", "ficha_tecnica.rds"))

# 2. Hoja `datos` -------------------------------------------------------
# Una fila por servidor. CUIN identifica ENTIDAD (y tiene vacíos): el
# identificador único es ID_SERV. Faltantes = celdas vacías (writexl
# escribe NA como celda vacía).
if (!"ID_SERV" %in% names(base)) {
  base <- base %>% mutate(ID_SERV = sprintf("S%05d", row_number()), .before = 1)
}
items_liderazgo <- paste0("C21", LETTERS[1:10])
items_clima     <- paste0("C18", LETTERS[1:4])
items_satisf    <- paste0("C03", LETTERS[1:5])
datos <- base %>%
  select(ID_SERV, COD_ENT, CUIN, B04, CODSECTOR, CODGRUPO, FEXPFINAL,
         all_of(items_liderazgo), all_of(items_clima), all_of(items_satisf), C03F)

stopifnot(!anyDuplicated(datos$ID_SERV))                       # una fila por observación
stopifnot(all(unlist(datos[c(items_liderazgo, items_clima, items_satisf, "C03F")]) %in%
                c(1:5, NA)))                                    # sin códigos especiales

# 3. Hoja `diccionario` (9 campos de la Sección 6.1 del taller) --------
campos9 <- c("nombre", "etiqueta", "enunciado", "constructo", "tipo",
             "categorias_codigos", "sentido", "recodificacion", "nombre_original")

fila_id <- tibble(
  nombre = "ID_SERV", etiqueta = "Identificador de servidor",
  enunciado = "Consecutivo único por fila, creado en la Actividad 2",
  constructo = "Identificación", tipo = "Carácter",
  categorias_codigos = "S00001 .. S26742", sentido = "N/A",
  recodificacion = "Creado (sprintf sobre row_number)", nombre_original = "(no existe en la fuente)"
)
diccionario <- diccionario %>%
  mutate(
    enunciado = ifelse(nombre == "CUIN", "Código de unidad/entidad de la fuente; se repite entre servidores", enunciado),
    categorias_codigos = ifelse(nombre == "CUIN", "No único; 1.711 vacíos; NO es llave", categorias_codigos),
    constructo = ifelse(nombre == "C03F", "Ninguno (excluido del modelo)", constructo),
    recodificacion = ifelse(nombre == "C03F",
                            "9,99,98,88,-1 -> NA; sentido inverso NO recodificado porque el ítem se excluye", recodificacion)
  )
if (!"ID_SERV" %in% diccionario$nombre) diccionario <- bind_rows(fila_id, diccionario)
diccionario <- diccionario %>%
  select(all_of(campos9)) %>%
  arrange(match(nombre, names(datos)))

stopifnot(identical(diccionario$nombre, names(datos)))          # una fila por variable
stopifnot(ncol(diccionario) == 9)

# 4. Hoja `metadatos`: ficha + flujo de registros + criterios -----------
meta_ficha <- ficha %>%
  transmute(seccion = "Ficha técnica", campo = Campo, valor = Valor)

meta_flujo <- flujo %>%
  transmute(seccion = "Flujo de registros",
            campo = Paso,
            valor = paste0("n = ", n,
                           ifelse(is.na(Eliminados), "", paste0("; eliminados = ", Eliminados)),
                           "; ", Motivo))

meta_criterios <- tribble(
  ~seccion, ~campo, ~valor,
  "Criterios de exclusión", "Registro sin ninguna respuesta en los 19 indicadores",
  "Se conserva en la base (22 casos tras recodificar códigos especiales); lavaan lo excluye al estimar.",
  "Criterios de exclusión", "Duplicados por CUIN", "No aplica: CUIN identifica entidad, no servidor",
  "Criterios de exclusión", "Filas idénticas (1.178)", "Se conservan: coincidencia plausible en ítems Likert de 5 categorías",
  "Criterios de exclusión", "Atípicos multivariados (Mahalanobis p < 0,001)", "Se conservan; sensibilidad en Fase 2",
  "Criterios de exclusión", "Straightlining (19 ítems idénticos)", "Se conservan; sensibilidad en Fase 2",
  "Criterios de exclusión", "Ítem C03F", "Excluido del constructo Satisfacción (mide exigencia laboral, sentido inverso)",
  "Códigos especiales", "9, 98, 99, 88, -1", "Convertidos a NA antes de cualquier cálculo",
  "Descarga", "Fecha de descarga", "2026-10-21 (fecha fija; no usar Sys.Date())"
)
metadatos <- bind_rows(meta_ficha, meta_flujo, meta_criterios)

# 5. Exportación del Excel final (3 hojas obligatorias) ---------------
ruta_xlsx <- here("salidas", "Taller1_SEM_base.xlsx")
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))
write_xlsx(list(datos = datos, diccionario = diccionario, metadatos = metadatos),
           path = ruta_xlsx)
stopifnot(file.exists(ruta_xlsx))
cat("OK:", ruta_xlsx, "| filas datos:", nrow(datos), "| variables:", ncol(datos), "\n")