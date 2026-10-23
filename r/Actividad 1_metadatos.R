# =====================================================================
# ACTIVIDAD 1: Fuente, ficha técnica y marco conceptual
# Taller 1 - Fase 1 | SEM | USTA
# =====================================================================

# 0. Paquetes -----------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(writexl)
library(knitr)

set.seed(2026)

# 2. LECTURA DE METADATOS DEL ARCHIVO ----------------------------------
# Leemos solo la primera hoja para inspeccionar estructura y dimensiones
crudos <- readxl::read_xlsx(
  here("excel", "edi2025_entidad.xlsx"),
  sheet = 1
)

dim_crudas <- dim(crudos)
cat("Filas crudas:", dim_crudas[1], "\n")
cat("Columnas crudas:", dim_crudas[2], "\n\n")

# 2. VERIFICACIÓN DE VARIABLES DISPONIBLES -----------------------------
sociodem_diccionario <- c("B02", "B03", "B04", "B05", "B06", "B07")
sociodem_disponibles <- intersect(sociodem_diccionario, names(crudos))
sociodem_ausentes    <- setdiff(sociodem_diccionario, names(crudos))

cat("Sociodemográficas disponibles en la base:",
    paste(sociodem_disponibles, collapse = ", "), "\n")
cat("Sociodemográficas ausentes en la base:",
    paste(sociodem_ausentes, collapse = ", "), "\n\n")

# 3. FICHA TÉCNICA (borrador para el PDF, Sección 4) -------------------
ficha_tecnica <- tibble(
  Campo = c(
    "Entidad",
    "Operación estadística",
    "Año de referencia",
    "Unidad de observación",
    "Cobertura",
    "Tipo de diseño",
    "Tamaño de la base cruda",
    "Número de variables",
    "Fecha de descarga",
    "URL exacta",
    "Términos de uso",
    "Archivo fuente",
    "Sociodemográficas disponibles",
    "Sociodemográficas ausentes (limitación)",
    "Identificador de unidad",
    "Variables de diseño muestral"
  ),
  Valor = c(
    "Departamento Administrativo Nacional de Estadística (DANE)",
    "Encuesta sobre Ambiente y Desempeño Institucional (EDI) - Sector Público",
    "2025",
    "Servidor/a público/a de entidades del Estado colombiano",
    "Nacional (ramas ejecutiva, legislativa, judicial y organismos de control)",
    "Encuesta probabilística, muestreo estratificado por sector y grupo funcional",
    paste0(dim_crudas[1], " filas x ", dim_crudas[2], " columnas"),
    as.character(dim_crudas[2]),
    as.character(Sys.Date()),
    "https://microdatos.dane.gov.co/index.php/catalog/887/get-microdata",
    "Datos abiertos - uso académico citando la fuente (DANE)",
    "edi2025_entidad.xlsx",
    paste(sociodem_disponibles, collapse = ", "),
    paste(sociodem_ausentes, collapse = ", "),
    "CUIN (identificador único de la unidad de observación)",
    "CODSECTOR, CODGRUPO, FEXPFINAL"
  )
)

print(ficha_tecnica, n = Inf)

# 4. TABLA DE INDICADORES POR CONSTRUCTO -------------------------------
indicadores_por_constructo <- tribble(
  ~Constructo, ~Codigo, ~Enunciado, ~Escala, ~Sentido, ~Rol,
  
  # Constructo 1: Liderazgo (exógeno)
  "Liderazgo", "C21A", "Asignó las cargas de trabajo de forma equitativa", "Likert 1-5", "Directo", "Exógeno",
  "Liderazgo", "C21B", "Tuvo en cuenta mis aportes en la toma de decisiones", "Likert 1-5", "Directo", "Exógeno",
  "Liderazgo", "C21C", "Me motivó para realizar mejor mi trabajo", "Likert 1-5", "Directo", "Exógeno",
  "Liderazgo", "C21D", "Confío en las decisiones que toma", "Likert 1-5", "Directo", "Exógeno",
  "Liderazgo", "C21E", "Lideró dando un buen ejemplo", "Likert 1-5", "Directo", "Exógeno",
  "Liderazgo", "C21F", "Se interesó en mi bienestar personal", "Likert 1-5", "Directo", "Exógeno",
  "Liderazgo", "C21G", "Promovió la aplicación de los valores del Código de integridad", "Likert 1-5", "Directo", "Exógeno",
  "Liderazgo", "C21H", "Reconoció cuando hice bien mi trabajo", "Likert 1-5", "Directo", "Exógeno",
  "Liderazgo", "C21I", "Impulsó la cultura de la transformación digital", "Likert 1-5", "Directo", "Exógeno",
  "Liderazgo", "C21J", "Asignó tareas sin prejuicios por razón de raza, etnia, etc.", "Likert 1-5", "Directo", "Exógeno",
  
  # Constructo 2: Clima laboral (endógeno mediador)
  "Clima laboral", "C18A", "Soy tratado con respeto por las personas con quienes trabajo", "Likert 1-5", "Directo", "Endógeno (mediador)",
  "Clima laboral", "C18B", "Mis compañeros cuentan con las competencias necesarias", "Likert 1-5", "Directo", "Endógeno (mediador)",
  "Clima laboral", "C18C", "Mis compañeros de trabajo son confiables", "Likert 1-5", "Directo", "Endógeno (mediador)",
  "Clima laboral", "C18D", "Las experiencias de trabajo con mis compañeros han sido positivas", "Likert 1-5", "Directo", "Endógeno (mediador)",
  
  # Constructo 3: Satisfacción laboral (endógeno final)
  "Satisfacción", "C03A", "Estoy satisfecho con mi trabajo", "Likert 1-5", "Directo", "Endógeno",
  "Satisfacción", "C03B", "Estoy orgulloso del trabajo que hago", "Likert 1-5", "Directo", "Endógeno",
  "Satisfacción", "C03C", "Me siento feliz de trabajar en esta entidad", "Likert 1-5", "Directo", "Endógeno",
  "Satisfacción", "C03D", "Estoy dispuesto a extender mi jornada para terminar un trabajo pendiente", "Likert 1-5", "Directo", "Endógeno",
  "Satisfacción", "C03E", "Doy mi mejor esfuerzo para cumplir con mi trabajo", "Likert 1-5", "Directo", "Endógeno",
  "Satisfacción", "C03F", "Me exigieron cumplir con tareas por fuera de mi horario", "Likert 1-5", "Inverso", "Excluido del constructo"
)

# Verificar que todos los códigos existen en la base real
items_en_base   <- names(crudos)
items_tabla     <- unique(indicadores_por_constructo$Codigo)
items_faltantes <- setdiff(items_tabla, items_en_base)

if (length(items_faltantes) > 0) {
  warning("Ítems de la tabla que NO están en la base: ",
          paste(items_faltantes, collapse = ", "))
} else {
  cat("OK: todos los ítems de la tabla existen en la base.\n\n")
}

# 5. MARCO CONCEPTUAL: HIPÓTESIS ---------------------------------------
hipotesis <- tribble(
  ~Hipotesis, ~Relacion, ~Signo, ~Sustento_teorico,
  "H1", "Liderazgo -> Clima laboral", "+",
  "El liderazgo de apoyo y transformacional mejora la percepción de clima (Avolio & Bass, 2004; Schneider et al., 2013)",
  "H2", "Clima laboral -> Satisfacción", "+",
  "Un clima positivo se asocia con mayor satisfacción (Carr et al., 2003; Parker et al., 2003)",
  "H3", "Liderazgo -> Satisfacción (directo)", "+",
  "El líder influye directamente en la satisfacción de sus colaboradores (Judge & Piccolo, 2004)",
  "H4", "Liderazgo -> Satisfacción (indirecto vía clima)", "+",
  "El clima media la relación liderazgo-satisfacción (Walumbwa et al., 2008)"
)

print(hipotesis)

# 6. REFERENCIAS DEL MARCO CONCEPTUAL ----------------------------------
referencias_marco <- tibble(
  Autor = c(
    "Avolio, B. J., & Bass, B. M.",
    "Carr, J. Z., Schmidt, A. M., Ford, J. K., & DeShon, R. P.",
    "Judge, T. A., & Piccolo, R. F.",
    "Parker, C. P., Baltes, B. B., Young, S. A., et al.",
    "Schneider, B., Ehrhart, M. G., & Macey, W. H.",
    "Walumbwa, F. O., Avolio, B. J., & Zhu, W."
  ),
  Año = c(2004, 2003, 2004, 2003, 2013, 2008),
  Titulo = c(
    "Multifactor Leadership Questionnaire (3rd ed.)",
    "Climate perceptions matter: A meta-analytic path analysis...",
    "Transformational and transactional leadership: A meta-analytic test...",
    "Relationships between psychological climate perceptions and work outcomes...",
    "Organizational climate and culture",
    "The effect of leader-member exchange on employee performance..."
  ),
  Fuente = c(
    "Mind Garden",
    "Journal of Applied Psychology, 88(4), 605-619",
    "Journal of Applied Psychology, 89(5), 755-768",
    "Journal of Organizational Behavior, 24(4), 389-416",
    "Annual Review of Psychology, 64, 361-388",
    "Journal of Management, 34(4), 758-780"
  )
)

print(referencias_marco)

# 7. GUARDAR INSUMOS PARA ACTIVIDAD 2 ----------------------------------
if (!dir.exists(here("salidas"))) dir.create(here("salidas"))

saveRDS(ficha_tecnica,              here("salidas", "ficha_tecnica.rds"))
saveRDS(indicadores_por_constructo, here("salidas", "indicadores.rds"))
saveRDS(hipotesis,                  here("salidas", "hipotesis.rds"))
saveRDS(referencias_marco,          here("salidas", "referencias.rds"))

cat("\n=== INSUMOS GUARDADOS ===\n")
cat("salidas/ficha_tecnica.rds\n")
cat("salidas/indicadores.rds\n")
cat("salidas/hipotesis.rds\n")
cat("salidas/referencias.rds\n")

sessionInfo()