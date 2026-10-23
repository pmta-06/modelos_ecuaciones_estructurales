# 📊 Taller 1 - Fase 1: Modelos de Ecuaciones Estructurales (SEM)

Proyecto de análisis estadístico para la modelación de la percepción del ambiente institucional. Diagnostica, depura y especifica un modelo de mediación (Liderazgo → Clima laboral → Satisfacción) con datos abiertos del DANE (Encuesta sobre Ambiente y Desempeño Institucional, EDI 2025).

## Requisitos previos

- **R (versión 4.5.2 o superior)** (verificar con `R --version`)
- **RStudio (versión 2024.04 o superior)**
- **Git**
- **renv** (incluido en el repositorio para gestionar dependencias)

> **Nota para Windows/Mac/Linux:** el proyecto es multiplataforma. Usa `here()` para todas las rutas (sin `setwd()`), por lo que funciona en cualquier sistema operativo.

## Scripts principales

| Actividad | Script | Descripción |
|---|---|---|
| `1` | `Actividad_1_metadatos.R` | Ficha técnica, indicadores por constructo e hipótesis. |
| `2` | `Actividad_2_construccion.R` | Depura la base, recodifica y construye el diccionario. |
| `3` | `Actividad_3_faltantes.R` | Calidad de datos: rangos, faltantes, prueba de Little, atípicos. |
| `4` | `Actividad_4_descriptiva.R` | Descriptivos, piso/techo y prueba de Mardia. |
| `5` | `Actividad_5_asociacion.R` | Correlaciones policóricas, confiabilidad y AFE. |
| `6` | `Actividad_6_modelo.R` | Especifica el modelo LISREL, identificación y sintaxis lavaan. |
| `Unión` | `Union_excel.R` | Genera el Excel final con las hojas `datos`, `diccionario` y `metadatos`. |

## 1. Clonar el repositorio

```bash
git clone https://github.com/pmta-06/modelos_ecuaciones_estructurales.git
cd modelos_ecuaciones_estructurales
```

## 2. Restaurar el entorno de paquetes

Este proyecto usa `renv` para que todos tengan las mismas versiones.

**En la consola de R (no en la terminal):**

```r
renv::restore()
```

**Importante:** acepta la instalación de todos los paquetes. Puede tardar unos minutos la primera vez.


## 3. Ejecutar el análisis completo

Para generar todos los resultados y el entregable final, ejecuta **en este orden** (cada actividad usa los objetos que guarda la anterior):

```r
source("r/Actividad_1_metadatos.R")
source("r/Actividad_2_construccion.R")
source("r/Union_excel.R")
source("r/Actividad_3_faltantes.R")
source("r/Actividad_4_descriptiva.R")
source("r/Actividad_5_asociacion.R")
source("r/Actividad_6_modelo.R")

```

El entregable final (Excel con las hojas `datos`, `diccionario` y `metadatos`) se guarda en:
`salidas/Taller1_SEM_base.xlsx`

## 5. Resultados y entregables

Los resultados se guardan automáticamente en la carpeta `salidas/`:

- **Tablas procesadas:** `Actividad1_Ficha_Tecnica.xlsx`, `Actividad2_Base_Analitica.xlsx`, etc.
- **Gráficos:** histogramas, mapas de calor, análisis paralelo y diagramas de caminos (`.png`).
- **Sintaxis:** `act6_sintaxis_propuesta.txt` y `act6_sintaxis_rival.txt` (modelos lavaan para la Fase 2).
- **Informe:** reporte estadístico en PDF (compilado desde LaTeX).

## 6. Hallazgos principales (Fase 1)

- **Muestra:** 26.742 servidores públicos de 193 entidades (la columna `CUIN` identifica la entidad, no al servidor).
- **Confiabilidad:** consistencia interna buena a excelente (α entre 0,795 y 0,956; ω entre 0,822 y 0,958).
- **Estructura exploratoria:** el AFE respaldó los 3 factores hipotetizados (19 ítems), con el 73,5 % de la varianza explicada.
- **Normalidad:** la prueba de Mardia rechazó la normalidad multivariada (p < 0,001); como los ítems son ordinales (Likert 1–5), se usará el estimador **WLSMV** en la Fase 2.
- **Identificación:** el modelo propuesto está sobreidentificado, con 149 grados de libertad (k = 19 indicadores, t = 41 parámetros libres).

## 7. Estructura del proyecto

```text
modelos_ecuaciones_estructurales/
├── excel/                       # Base cruda del DANE y diccionario original
│   ├── edi2025_entidad.xlsx     # Microdatos originales (26.742 filas x 243 columnas)
│   └── Diccionario.xlsx
├── r/                           # Scripts de R
│   ├── Actividad_1_metadatos.R
│   ├── Actividad_2_construccion.R
│   ├── Actividad_3_faltantes.R
│   ├── Actividad_4_descriptiva.R
│   ├── Actividad_5_asociacion.R
│   ├── Actividad_6_modelo.R
│   ├── Union_excel.R
│   └── codigo_resultados.Rmd    # Todo el flujo en un solo documento
├── salidas/                     # Resultados generados
│   ├── Informe.tex              # Codigo Latex del informe
    ├── *.xlsx                   # Tablas de resultados
│   ├── *.png                    # Gráficos
│   └── *.rds                    # Objetos intermedios (ignorados en Git)
├── renv/                        # Entorno virtual de R
├── renv.lock                    # Versiones exactas de paquetes
├── .gitignore                   # Archivos excluidos (.RData, *.rds, etc.)
├── proyecto_edi2025.Rproj       # Proyecto de RStudio
└── README.md                    # Este archivo
```

## 8. Solución de problemas

- **Error `there is no package called 'lavaan'`:** ejecuta `renv::restore()` o instala con `install.packages("lavaan")`.
- **`unable to start png() device` o los gráficos no se generan:** verifica que la carpeta `salidas/` exista, que ningún `.png` esté abierto en otro programa y cierra gráficos pendientes con `while (!is.null(dev.list())) dev.off()`.
- **Errores con tildes o espacios en nombres de archivo:** usa nombres sin tildes ni espacios (por ejemplo `Union_excel.R` en lugar de `Unión_excel.R`).

---

**Equipo:** Kevin Leonardo Chaparro Reyes, Valentina Muñoz Palma,Paula Margarita Triana Ancinez
**Curso:** Modelación de Ecuaciones Estructurales (SEM) — Universidad Santo Tomás — 2026-II
