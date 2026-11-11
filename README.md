# Liderazgo, clima y satisfacción en el empleo público colombiano: un modelo de ecuaciones estructurales (EDI 2025)

**Proyecto de investigación reproducible · Modelación de Ecuaciones Estructurales · Universidad Santo Tomás (USTA)**

> Un proyecto de investigación reproducible que separa la medición de la estructura: primero se valida el instrumento (AFC, fiabilidad, validez), después se estima la mediación parcial y, por último, se prueba si el modelo es equivalente entre grupos. Todo se reconstruye desde el microdato del DANE con **22 scripts de R** —6 de la Fase 1 y 16 de la Fase 2— y un entorno fijado con **`renv`**.

---

## Ficha del proyecto

| Campo | Detalle |
|---|---|
| **Título** | *Liderazgo, clima y satisfacción en el empleo público colombiano: un modelo de ecuaciones estructurales con indicadores ordinales* |
| **Pregunta** | ¿En qué medida el liderazgo directivo se asocia con la satisfacción laboral y qué papel desempeña el clima laboral como mediador, una vez que se controla el error de medición y se respeta la estructura de conglomerados de la encuesta? |
| **Curso** | Modelación de Ecuaciones Estructurales — Taller 2 (Fases 2 y 3) |
| **Universidad** | Universidad Santo Tomás (USTA), Bogotá D. C. — Facultad de Estadística |
| **Fuente** | Encuesta sobre Ambiente y Desempeño Institucional (EDI 2025), DANE — microdatos anonimizados |
| **Unidad de análisis** | Servidor público individual |
| **Muestra** | **26.742** servidores públicos; base cruda de 243 columnas; **201** entidades (conglomerados) |
| **Indicadores** | **19** ítems ordinales Likert 1–5: 10 de Liderazgo (C21A–C21J), 4 de Clima (C18A–C18D) y 5 de Satisfacción (C03A–C03E) |
| **Estimador** | WLSMV con `ordered = TRUE` y `cluster = .cluster_entidad` |
| **Stack** | R 4.5.2 · `lavaan` 0.7-2 · `semTools` 0.5-9 · `renv` · LaTeX |
| **Salidas** | `salidas/` (52 `.rds` y 27 `.xlsx`) · `figuras/` (22 figuras) · `logs/` · `articulo.tex` |

La cadena lógica del proyecto es una sola línea, y cada eslabón tiene su propio script:

**liderazgo directivo → clima laboral → satisfacción laboral**, con ítems ordinales, errores robustos por entidad y una prueba explícita de invarianza entre sexos y entre sectores.

Este README explica cómo reconstruir esa cadena de principio a fin, sin atajos y sin resultados inventados.

---

## Hallazgos principales

Los resultados se resumen en unas pocas cifras. El AFC final alcanza un ajuste satisfactorio —**CFI = 0,988; TLI = 0,986; RMSEA = 0,050 [0,049; 0,051]; SRMR = 0,025**— con cargas estandarizadas entre 0,623 y 0,931, fiabilidad compuesta superior a 0,89 y AVE superior a 0,62. El modelo estructural explica el **48,7 %** de la varianza del clima y el **46,2 %** de la satisfacción: el liderazgo predice el clima (**0,725**), el clima predice la satisfacción (**0,349**) y el liderazgo conserva un efecto directo (**0,447**), con un efecto indirecto de **0,253** (estandarizado 0,230). Ese indirecto representa el **36,1 %** del efecto total: la mediación es **parcial**, no total. Y la medición se sostiene invariante por sexo y por sector en los cuatro niveles de la secuencia de Wu y Estabrook, con **ΔCFI ≤ 0,004** y **ΔSRMR ≤ 0,001**.

> **Lectura rigurosa (tres advertencias que el paper declara y el repositorio permite verificar):**
> **(i)** M1 reproduce exactamente el ajuste del AFC —χ² = 8.856,34 con 148 gl en ambos—, porque el bloque estructural es saturado: el contraste AFC vs. M1 tiene Δgl = 0 y **no es informativo**, no es una prueba de bondad del modelo estructural.
> **(ii)** El RMSEA **robusto** del AFC es 0,070 y no cumple el umbral de 0,06, mientras el **escalado** es 0,050 y sí lo cumple; el paper reporta ambos y trata la discrepancia como limitación del ajuste global.
> **(iii)** M2 (sin ruta directa) y M3 (dirección opuesta) ajustan idénticamente: son modelos **equivalentes**, de modo que el ajuste no identifica la dirección causal. Por eso el lenguaje es asociativo.

**La medición no es un trámite.** El paso de la Fase 2 no fue estimar el modelo estructural, sino demostrar que los 19 ítems miden lo que dicen medir: cargas, CR, AVE, Fornell-Larcker, HTMT, validez en una submuestra de validación y, al final, invarianza entre grupos. Sin ese primer paso, cualquier coeficiente estructural sería una relación entre constructos mal medidos. El HTMT máximo es 0,703 (liderazgo–clima), muy por debajo de 0,85.

---

## Estructura del repositorio

```text
proyecto_edi2025_entidad/
│
├── excel/                                  # ⇦ MICRODATOS ORIGINALES
│   ├── edi2025_entidad.xlsx                # EDI 2025 · 26.742 filas × 243 columnas
│   └── Diccionario.xlsx                    # Diccionario de variables del DANE
│
├── r/                                      # ⇦ PIPELINE REPRODUCIBLE (22 scripts + utilidades)
│   ├── Unión_excel.R                       # Exporta el libro de la Fase 1 (datos, diccionario, metadatos)
│   ├── Actividad 1_metadatos.R             # ┐
│   ├── Actividad 2_construccion.R          # │
│   ├── Actividad 3_faltantes.R             # │ FASE 1 — Taller 1 (6 scripts)
│   ├── Actividad 4_descriptiva.R           # │ se corren con RUN_FASE1=1
│   ├── Actividad 5_asociación.R            # │
│   ├── Actividad 6_modelo.R                # ┘
│   ├── Actividad 7_setup_fase2.R           # ┐
│   ├── Actividad 8_afc.R                   # │
│   ├── Actividad 9_ajuste_local.R          # │ FASE 2 — Taller 2 (16 scripts, el nombre es la actividad)
│   ├── …                                   # │
│   ├── Actividad 17_multigrupo_medicion.R  # │
│   └── Actividad 21_libro_resultados.R     # ┘
│
├── salidas/                                # ⇦ RESULTADOS INTERMEDIOS Y FINALES
│   ├── base_analitica.rds                  # Base analítica · 26.742 × 26
│   ├── modelo_final.rds                    # Modelo de medición final (AFC reespecificado)
│   ├── m1_fit.rds                          # Modelo estructural M1
│   ├── Actividad7_…xlsx … Actividad20_…xlsx  # Un libro por actividad
│   ├── Taller2_SEM_resultados.xlsx         # Libro obligatorio (6 hojas)
│   └── Taller2_SEM_resultados_complementarios.xlsx  # Análisis adicionales (21 hojas)
│
├── figuras/                                # ⇦ FIGURAS DE LA FASE 2
│   ├── act8_afc_estandarizado.png
│   ├── act9_afc_final.png
│   ├── act12_modelo_estructural.png
│   ├── act14_histograma_entidades.png
│   ├── act14_boxplot_entidades.png
│   ├── act14_dispersion_entidades.png
│   └── act15_*.png                         # 4 figuras de patrones de respuesta
│
├── logs/                                   # Un .log por script (salida de 00_run_all.R)
├── Articulo/                               # Copia del artículo con sus figuras y salidas
├── informe/                                # Informe del Taller 1 (PDF)
├── entrega_taller2/                        # Paquete de entrega (scripts, figuras y salidas)
│
├── renv/ , renv.lock                       # Entorno reproducible (206 paquetes)
├── .Rprofile                               # Activa renv al abrir el proyecto
├── 00_run_all.R                            # Ejecutor maestro
├── CORRECCIONES.md                         # Registro de las cinco correcciones del pipeline
├── articulo.tex · referencias.bib          # Artículo final en LaTeX
├── Taller2_SEM.pdf                         # Compilación del trabajo en PDF
├── proyecto_edi2025_entidad.Rproj          # Proyecto de RStudio
└── README.md                               # Este archivo
```

**Regla de oro:** `excel/` contiene lo que se descarga una vez; `salidas/` contiene lo que produce el pipeline; `figuras/` contiene las figuras de la Fase 2; `logs/` contiene la evidencia de que el pipeline corrió. Nada de eso se edita a mano.

---

## Requisitos

- **R 4.5.2** (la versión registrada en `renv.lock`).
- **RStudio** (recomendado) o línea de comandos con `Rscript`.
- **Git** para clonar el repositorio.
- **`renv`** para restaurar las versiones exactas de los paquetes.
- **LaTeX** (solo si vas a recompilar `articulo.tex`; no es necesario para reproducir los resultados).
- **8 GB de RAM o más**: el bootstrap por conglomerados, la invarianza por sector y el CFA multinivel son las etapas más exigentes.
- **≈ 120 MB de disco** para `salidas/`: el `.rds` de invarianza sectorial solo pesa 92 MB.

---

## Clonar y restaurar el entorno

### 1. Clona el repositorio

```bash
git clone https://github.com/pmta-06/modelos_ecuaciones_estructurales.git proyecto_edi2025_entidad
cd proyecto_edi2025_entidad
```

> Usa `git clone`, no la descarga en ZIP: así conservas el historial y el `renv.lock`.

### 2. Abre el proyecto y restaura `renv`

Abre `proyecto_edi2025_entidad.Rproj` en RStudio. El archivo `.Rprofile` activa `renv` automáticamente; en la consola verás algo como:

```text
- Project 'proyecto_edi2025_entidad' loaded. [renv 1.x.y]
```

Ahora restaura las versiones exactas de los paquetes:

```r
renv::restore()
```

- Responde **`Y`** cuando pregunte si deseas continuar.
- La primera restauración es la etapa más lenta del montaje.
- Si se interrumpe, vuelve a ejecutar `renv::restore()`: es idempotente.

Verifica que el entorno quedó consistente:

```r
renv::status()
```

La respuesta esperada es:

```text
No issues found -- the project is in a consistent state.
```

> **Nota de versiones.** Las salidas versionadas se generaron con **R 4.5.2, `lavaan` 0.7-2 y `semTools` 0.5-9**. El `renv.lock` registra el conjunto completo de 206 paquetes y fija `semTools` 0.5-10; una restauración limpia puede instalar esa versión posterior. Las funciones usadas (`cfa`, `sem`, `measEq.syntax`, `reliability`, `htmt`) son estables entre ambas, pero si necesitas replicación exacta instala `renv::install("semTools@0.5-9")` después de restaurar.

> **Nota (Windows):** si al cargar `semPlot` aparece un error de carga de DLL (`LoadLibrary failure`), suele ser una política de Control de Aplicaciones de Windows. Reintenta la carga; los diagramas solo se necesitan para las figuras.

---

## Fuentes de datos

El proyecto separa **datos originales** (`excel/`) de **datos derivados** (`salidas/`). Los originales se descargan de su fuente oficial y se dejan intactos; en este repositorio están versionados (≈ 22 MB) para que el clon sea autosuficiente. Si prefieres un clon ligero, puedes excluirlos y descargarlos aparte.

| Fuente | Qué descargar | Enlace oficial | Guardar en |
|---|---|---|---|
| **DANE — EDI 2025** | Microbase anonimizada de la Encuesta sobre Ambiente y Desempeño Institucional 2025 (una fila por servidor) | [Microdatos DANE](https://microdatos.dane.gov.co/) | `excel/edi2025_entidad.xlsx` |
| **DANE — Diccionario** | Diccionario de variables de la EDI 2025 | [Microdatos DANE](https://microdatos.dane.gov.co/) | `excel/Diccionario.xlsx` |

**Condiciones que deben cumplirse:**

- La **primera hoja** de `edi2025_entidad.xlsx` debe contener los **26.742 registros × 243 columnas**; el script de construcción lee `sheet = 1`. Si el DANE entrega varios módulos, consolídalos en un solo libro dejando los datos en la primera hoja.
- Los nombres de archivo están escritos en el código (`r/Actividad 2_construccion.R` lee `excel/edi2025_entidad.xlsx`). No los cambies, o actualiza la ruta en el script.
- La base es **anónima**: la fuente no trae identificador de persona (`ID_SERV` es un consecutivo sintético que crea la Actividad 2). `CUIN` identifica la **entidad**, no al servidor, y tiene 1.711 vacíos en la fuente; por eso el conglomerado se construye con `COD_ENT`.

> **Verificación rápida antes de ejecutar.** Desde la raíz del proyecto:
>
> ```r
> library(readxl); library(here)
> length(list.files("excel"))                                  # 2
> dim(read_excel(here("excel", "edi2025_entidad.xlsx")))       # 26742 x 243
> ```
>
> Si el número de filas no coincide, detente: todos los resultados del paper están calculados sobre esas 26.742 observaciones.

**Procesamiento aplicado a la fuente (Fase 1):** selección de 243 → 26 columnas, recodificación de códigos especiales (9, 99, 98, 88 y −1) como valores perdidos, **cero registros eliminados** y exclusión del ítem **C03F** del constructo de satisfacción por medir sobrecarga en sentido inverso (permanece en la base, pero no entra al modelo).

---

## Orden de ejecución

**Los scripts deben ejecutarse en orden secuencial y sin saltos.** Cada paso consume las salidas del anterior: el AFC (paso 02) necesita el `setup` del paso 01, y el ajuste local (paso 03) necesita el AFC y produce el `modelo_final.rds` que usan los pasos 06, 07, 08 y 11. Alterar el orden produce errores de archivos faltantes o, peor, resultados silenciosamente inconsistentes.

### Opción A — Ejecución completa (recomendada)

```bash
Rscript 00_run_all.R
```

Ejecuta en orden los 16 scripts de la Fase 2, escribe un log por script en `logs/` y **se detiene en el primer error**. Asume que existe la base del Taller 1 (`salidas/base_analitica.rds`).

### Opción B — Regenerar también la Fase 1 (Taller 1)

```bash
# Linux / macOS
RUN_FASE1=1 Rscript 00_run_all.R

# Windows PowerShell
$env:RUN_FASE1 = "1"; Rscript 00_run_all.R
```

Útil si clonaste el repositorio sin `salidas/base_analitica.rds`, porque `salidas/*.rds` está ignorado por Git.

### Opción C — Un script suelto

```bash
Rscript "r/Actividad 9_ajuste_local.R"
```

> Las comillas son necesarias porque los nombres de archivo llevan espacio (`Actividad N_…`), igual que en la Fase 1.

Todos los scripts usan `here()` (sin `setwd()`); los que tienen componentes aleatorios fijan `set.seed(2026)`, y el cierre con `sessionInfo()` registra el entorno de la corrida.

**Tiempos de referencia** (corrida completa verificada el 2026-11-09: **40,8 minutos**, 16/16 scripts exitosos):

| Etapa | Tiempo aproximado |
|---|---|
| Pasos 01–07, 09–13 y 16 | ≈ 5 min en conjunto |
| 08 — bootstrap por conglomerados (Act. 13b, 2.000 réplicas) | ≈ 8 min |
| 14 — invarianza por sector (Act. 19) | ≈ 11 min |
| 15 — CFA multinivel (Act. 20) | ≈ 17 min |

**Recomendaciones:**

- Ejecuta siempre desde la raíz del proyecto (o con el `.Rproj` abierto): todos los scripts usan `here()` para resolver rutas.
- Si un script falla, corrige la causa y retoma desde ese punto: las salidas anteriores quedan en `salidas/`.
- No borres `salidas/modelo_final.rds` ni `salidas/m1_fit.rds`: son la entrada de los efectos, los alternativos y el libro de resultados.
- Revisa la consola y el `.log` de cada script: los avisos (`AVISO:`) indican insumos ausentes que el script sustituye o salta.

---

## Scripts del pipeline

### Fase 1 — Taller 1 (6 scripts, nombres originales)

| # | Script | Actividad | Productos principales |
|---|---|---|---|
| A1 | `Actividad 1_metadatos.R` | Ficha técnica, indicadores por constructo, hipótesis y referencias | `ficha_tecnica.rds`, `indicadores.rds`, `hipotesis.rds`, `referencias.rds` |
| A2 | `Actividad 2_construccion.R` | Construcción de la base analítica (26.742 × 26), recodificación y flujo de registros | `base_analitica.rds`, `diccionario.rds`, `flujo_registros.rds`, `Actividad2_Base_Analitica.xlsx` |
| A3 | `Actividad 3_faltantes.R` | Faltantes por ítem, patrón, Little, atípicos de Mahalanobis y Mardia | `Actividad3_Calidad_Datos.xlsx` + 4 figuras en `salidas/` |
| A4 | `Actividad 4_descriptiva.R` | Descriptivos, efectos piso/techo, histogramas y normalidad | `Actividad4_Descriptivos_Normalidad.xlsx` + 3 figuras |
| A5 | `Actividad 5_asociación.R` | Correlaciones policóricas, KMO, Bartlett, análisis paralelo y AFE | `Actividad5_Asociacion_AFE.xlsx` + 3 figuras |
| A6 | `Actividad 6_modelo.R` | Sintaxis `lavaan`, diagrama conceptual y modelo rival | `Actividad6_Especificacion_Modelo.xlsx` + 2 figuras |
| — | `Unión_excel.R` | Exporta el libro de la Fase 1 con las hojas `datos`, `diccionario` y `metadatos` | `salidas/Taller1_SEM_base.xlsx` |

> `Unión_excel.R` **no** está incluido en `00_run_all.R`: se ejecuta a mano al final de la Fase 1, cuando ya existen `base_analitica.rds`, `diccionario.rds` y `flujo_registros.rds`. El maestro sí corre los seis scripts de la Fase 1 cuando se invoca con `RUN_FASE1=1`.

### Fase 2 — Taller 2 (16 scripts, el nombre es la actividad)

| Paso | Script | Actividad del Taller 2 | Productos principales |
|---|---|---|---|
| 01 | `Actividad 7_setup_fase2.R` | A1. Punto de partida: estimador, faltantes, diseño muestral, escala y split 70/30 | `fase2_setup.rds`, `Actividad7_Decisiones_Metodologicas.xlsx` |
| 02 | `Actividad 8_afc.R` | A2. AFC y ajuste global | `afc_fit.rds`, `afc_cargas.rds`, `Actividad8_AFC.xlsx`, `figuras/act8_afc_estandarizado.png` |
| 03 | `Actividad 9_ajuste_local.R` | A3. Residuos de correlación, IM/EPC, reespecificación y validación cruzada | `afc_final.rds`, `modelo_final.rds`, `Actividad9_Ajuste_Local.xlsx`, `figuras/act9_afc_final.png` |
| 04 | `Actividad 10_confiabilidad_validez.R` | A4. CR, AVE, Fornell-Larcker, HTMT e ICC por entidad | `conf_cr_ave.rds`, `conf_htmt.rds`, `Actividad10_Confiabilidad_Validez.xlsx` |
| 05 | `Actividad 11_sensibilidad_diseno.R` | Complemento A1/A2. Pesos de expansión vs. clúster | `Actividad11_Sensibilidad_Diseno.xlsx`, `sensibilidad_cargas.rds` |
| 06 | `Actividad 12_estructural.R` | B1. M1, contraste con el AFC, hipótesis y R² | `m1_fit.rds`, `Actividad12_Estructural.xlsx`, `figuras/act12_modelo_estructural.png` |
| 07 | `Actividad 13_efectos_mediacion_mc.R` | B2. Efectos indirecto, directo y total por Monte Carlo (20.000 réplicas) | `efectos_montecarlo.rds`, `Actividad13_Efectos_Mediacion_MC.xlsx` |
| 08 | `Actividad 13b_efectos_bootstrap_cluster.R` | B2. Bootstrap no paramétrico por conglomerados (2.000 réplicas, MLR) | `bootstrap_cluster_replicas.rds`, `Actividad13b_Bootstrap_Cluster.xlsx` |
| 09 | `Actividad 14_entidades.R` | Complemento. Medias por entidad, ranking y correlaciones agregadas | `entidades_medias.rds`, `Actividad14_Entidades.xlsx`, 3 figuras |
| 10 | `Actividad 15_patrones_respuesta.R` | Complemento. Straightlining y aquiescencia por entidad | `patrones_entidad.rds`, `Actividad15_Patrones_Respuesta.xlsx`, 4 figuras |
| 11 | `Actividad 16_modelos_alternativos.R` | B3. M2–M4, equivalencia M2/M3 y LRT M1 vs. M2 | `alternativos.rds`, `Actividad16_Alternativos.xlsx` |
| 12 | `Actividad 17_multigrupo_medicion.R` | B4. Invarianza de medición por sexo (configural → escalar) | `invarianza_medicion.rds`, `Actividad17_Invarianza_Medicion.xlsx` |
| 13 | `Actividad 18_multigrupo_estructural.R` | B5. Invarianza estructural por sexo (Wald) y medias observadas | `invarianza_estructural.rds`, `Actividad18_Invarianza_Estructural.xlsx` |
| 14 | `Actividad 19_multigrupo_sector.R` | Complemento. Invarianza por sector (6 sectores, n = 11.065) | `invarianza_sector.rds`, `Actividad19_Invarianza_Sector_v11.xlsx` |
| 15 | `Actividad 20_cfa_multinivel.R` | Complemento. CFA multinivel (MLR, dos niveles) e ICC | `cfa_multinivel.rds`, `icc_multinivel.rds`, `Actividad20_CFA_Multinivel.xlsx` |
| 16 | `Actividad 21_libro_resultados.R` | Libro de resultados del Taller | `Taller2_SEM_resultados.xlsx` + `…_complementarios.xlsx` |

> **Cómo leer los nombres.** El archivo se llama `Actividad N_…`, donde **N es el número de la actividad del Taller 2**: A1–A5 = Actividades 7–11; B1 = 12; B2 = 13 y 13b; complementos = 14, 15, 19 y 20; B3 = 16; B4 = 17; B5 = 18; libro = 21. La columna **Paso** es el orden real de ejecución (el mismo de `00_run_all.R`): los pasos 01–07 corresponden a las Actividades 7–13, el paso 08 al 13b y los pasos 09–16 a las Actividades 14–21.

---

## Decisiones metodológicas fijas

| Decisión | Elección | Motivo |
|---|---|---|
| Estimador | **WLSMV** con `ordered = TRUE` | Ítems ordinales de 5 categorías; Mardia rechazó normalidad multivariada |
| Identificación | Indicador marcador (C21A = C18A = C03A = 1) | Coherencia con la notación LISREL del Taller 1 |
| Faltantes | Pairwise (default de WLSMV); FIML solo en el análisis MLR | `lavaan` no admite FIML con WLSMV |
| Diseño muestral | `cluster = .cluster_entidad` (201 entidades) | Errores robustos a la conglomeración |
| Pesos | `FEXPFINAL` solo en la Actividad 11 | `sampling.weights` no se combina con `cluster` en WLSMV |
| Modelo de medición final | Incluye `C03D ~~ C03E` | IM = 820,37; EPC = 0,176; justificación teórica y de a un parámetro |
| Índices de ajuste | `*.scaled` como criterio; `*.robust` informativos | Homogeneidad entre actividades |
| Efectos indirectos | Monte Carlo (WLSMV) + bootstrap por conglomerados (MLR) | El Taller exige remuestreo; `lavaan` no hace bootstrap con WLSMV |
| Invarianza | Secuencia de Wu y Estabrook; ΔCFI, ΔRMSEA y ΔSRMR (Chen) | El contraste de χ² está sobrepotenciado con n > 26.000 |
| Semilla | 2026 (y `2026 + réplica` en el bootstrap) | Reproducibilidad |

---

## Resultados clave (modelo final)

### Ajuste

| Modelo | χ² | gl | CFI | TLI | RMSEA [IC90] | SRMR | CFI rob. | RMSEA rob. |
|---|---:|---:|---:|---:|---|---:|---:|---:|
| AFC base (Act. 8) | 9.755,18 | 149 | 0,987 | 0,985 | 0,052 [0,051; 0,053] | 0,028 | 0,958 | 0,075 |
| **AFC final (Act. 9)** | **8.856,34** | **148** | **0,988** | **0,986** | **0,050 [0,049; 0,051]** | **0,025** | 0,964 | 0,070 |
| **M1 estructural (Act. 12)** | **8.856,34** | **148** | **0,988** | **0,986** | **0,050 [0,049; 0,051]** | **0,025** | 0,964 | 0,070 |

La reespecificación con `C03D ~~ C03E` redujo χ² en 898,84 puntos con un grado de libertad y se confirmó en la submuestra de validación (CFI = 0,991; RMSEA = 0,052; ΔCFI = 0,001 frente a calibración).

### Medición: cargas, fiabilidad y validez

| Constructo | k | α | ω | CR | AVE | √AVE | Rango de cargas |
|---|---:|---:|---:|---:|---:|---:|---|
| Liderazgo | 10 | 0,956 | 0,958 | 0,973 | 0,785 | 0,886 | 0,824–0,927 |
| Clima laboral | 4 | 0,876 | 0,881 | 0,930 | 0,769 | 0,877 | 0,823–0,931 |
| Satisfacción laboral | 5 | 0,795 | 0,822 | 0,892 | 0,628 | 0,792 | 0,623–0,911 |

**Validez discriminante:** se cumple por ambos criterios. La raíz del AVE de cada constructo supera sus correlaciones (0,614–0,698) y los tres HTMT quedan bajo 0,85: **0,703** (liderazgo–clima), **0,633** (liderazgo–satisfacción) y **0,611** (clima–satisfacción).

### Hipótesis estructurales (M1)

| H | Relación | β | E.E. | z | β est. | Decisión |
|---|---|---:|---:|---:|---:|---|
| H1 | Liderazgo → Clima | 0,725 | 0,007 | 107,74 | 0,698 | Se apoya |
| H2 | Clima → Satisfacción | 0,349 | 0,010 | 34,72 | 0,330 | Se apoya |
| H3 | Liderazgo → Satisfacción (directo) | 0,447 | 0,010 | 44,98 | 0,407 | Se apoya |
| H4 | Indirecto (a·b) | 0,253 | 0,008 | 32,19 | 0,230 | Se apoya |

R²: Clima = **0,487**; Satisfacción = **0,462**.

### Efectos: Monte Carlo vs. bootstrap

| Efecto | Monte Carlo (WLSMV) | IC 95 % | Bootstrap (MLR) | IC 95 % | β est. (MC / boot) |
|---|---:|---|---:|---|---|
| Indirecto (a·b) | 0,253 | [0,237; 0,268] | 0,147 | [0,135; 0,160] | 0,230 / 0,190 |
| Directo (c) | 0,447 | [0,427; 0,467] | 0,284 | [0,264; 0,303] | 0,407 / — |
| Total | 0,700 | [0,687; 0,713] | 0,431 | [0,413; 0,449] | 0,638 / — |

Las escalas no están en las mismas unidades (WLSMV modela policóricas y umbrales; MLR trata los ítems como continuos), así que la comparación válida es la estandarizada. Ambas vías coinciden en signo, magnitud y significancia; el bootstrap usó 2.000/2.000 réplicas válidas en 7,9 minutos.

### Modelos alternativos

| Modelo | χ² | gl | CFI | TLI | RMSEA | SRMR |
|---|---:|---:|---:|---:|---:|---:|
| **M1: mediación parcial** | **8.856,34** | 148 | **0,988** | **0,986** | **0,050** | **0,025** |
| M2: sin mediación (solo efectos directos) | 14.151,71 | 149 | 0,981 | 0,978 | 0,063 | 0,041 |
| M3: dirección opuesta (clima → liderazgo) | 14.151,71 | 149 | 0,981 | 0,978 | 0,063 | 0,041 |
| M4: cadena liderazgo → satisfacción → clima | 25.161,07 | 149 | 0,966 | 0,961 | 0,084 | 0,058 |

M1 vs. M2 (anidados): Δχ² = 1.124,77 con 1 gl (p < 0,001), lo que respalda la ruta directa. **M2 y M3 son equivalentes**: mismo ajuste, misma topología, direcciones causales distintas. AIC y BIC no se reportan porque no están definidos para WLSMV.

### Invarianza

| Grupo | Nivel | χ² | gl | CFI | RMSEA | SRMR | ΔCFI | ΔRMSEA | ΔSRMR | Decisión |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|---|
| Sexo | Configural | 10.272,47 | 298 | 0,989 | 0,053 | 0,028 | — | — | — | Referencia |
| Sexo | Umbrales | 6.960,52 | 355 | 0,993 | 0,040 | 0,028 | 0,004 | −0,013 | 0,000 | Se sostiene |
| Sexo | Métrica | 5.764,06 | 371 | 0,994 | 0,035 | 0,029 | 0,001 | −0,005 | 0,001 | Se sostiene |
| Sexo | Escalar | 5.669,89 | 387 | 0,994 | 0,034 | 0,029 | 0,000 | −0,001 | 0,000 | Se sostiene |
| Sector | Configural | 3.391,76 | 894 | 0,993 | 0,039 | 0,031 | — | — | — | Referencia |
| Sector | Umbrales | 3.282,23 | 1.084 | 0,994 | 0,033 | 0,031 | 0,001 | −0,006 | 0,000 | Se sostiene |
| Sector | Métrica | 3.075,92 | 1.164 | 0,994 | 0,030 | 0,031 | 0,000 | −0,003 | 0,000 | Se sostiene |
| Sector | Escalar | 2.879,13 | 1.244 | 0,995 | 0,027 | 0,031 | 0,001 | −0,003 | 0,000 | Se sostiene |

Con invarianza escalar se comparan medias: hombres 4,054 / 4,220 / 4,450 y mujeres 3,945 / 4,078 / 4,413 (liderazgo, clima, satisfacción), con **|d| ≤ 0,191**. En la invarianza estructural, solo la ruta liderazgo → clima difiere por sexo (χ² = 11,671; 1 gl; p = 0,0006); por sector difieren las dos primeras rutas tras Bonferroni (p = 0,030 y 0,037) y no la directa (p = 0,287).

### Análisis complementarios

- **Multinivel.** ICC del CFA de dos niveles: 0,0575 (liderazgo), 0,0500 (clima) y 0,0587 (satisfacción); los modelos nulos de efectos aleatorios dan 0,057 / 0,052 / 0,061. Las correlaciones latentes son mayores entre entidades (0,772–0,873) que dentro de ellas (0,523–0,626).
- **Entidades.** Medias ponderadas por entidad: liderazgo 2,996–4,639 (mediana 3,954), clima 3,577–4,575 (4,106) y satisfacción 3,772–4,756 (4,400); correlaciones agregadas 0,809 / 0,798 / 0,711.
- **Patrones de respuesta.** Straightlining mediano de 11,02 % (rango 2,03–41,48 % entre entidades), aquiescencia media 0,801 y respuestas extremas 0,440. Sin anomalías que comprometan la estructura factorial.
- **Sensibilidad del diseño.** Clúster vs. pesos: cargas prácticamente idénticas (diferencia absoluta máxima 0,0066) y ajuste similar (CFI 0,987 vs. 0,991; RMSEA 0,052 vs. 0,045). La conclusión no depende de los pesos.

---

## Resultados y trazabilidad

- **Datos intermedios:** `salidas/`. Cada archivo `.rds` y cada libro `ActividadNN_*.xlsx` corresponde a la actividad que lo produce; así se sabe de dónde salió cada tabla sin abrir el código.
- **Figuras:** 22 en total. Las 12 de la Fase 1 quedan en `salidas/` (`act3_*`, `act4_*`, `act5_*`, `act6_*`) y las 10 de la Fase 2 en `figuras/` (`act8_*`, `act9_*`, `act12_*`, `act14_*`, `act15_*`). El prefijo `actNN` remite a la actividad correspondiente.
- **Logs:** `logs/` guarda la salida completa de cada script de la última corrida del maestro. Los logs que están hoy conservan el nombre numerado de la corrida del 2026-11-09; al volver a ejecutar `00_run_all.R` se generan con el nombre nuevo (`Actividad 7_…`). Es la prueba de que el pipeline corrió y de qué advirtió.

**Cifras estrella y su origen:**

| Cifra | Archivo | Hoja / objeto |
|---|---|---|
| CFI 0,988; RMSEA 0,050; SRMR 0,025 | `salidas/Taller2_SEM_resultados.xlsx` | `ajuste` |
| Cargas, CR y AVE | `salidas/Taller2_SEM_resultados.xlsx` | `medicion` |
| Fornell-Larcker y HTMT | `salidas/Taller2_SEM_resultados.xlsx` | `discriminante` |
| H1–H4 (0,725 / 0,349 / 0,447 / 0,253) | `salidas/Taller2_SEM_resultados.xlsx` | `hipotesis` |
| Efectos Monte Carlo y delta | `salidas/Taller2_SEM_resultados.xlsx` | `efectos` |
| Invarianza por sexo y sector | `salidas/Taller2_SEM_resultados.xlsx` | `invarianza` |
| Bootstrap 2.000 réplicas (0,147 [0,135; 0,160]) | `salidas/Actividad13b_Bootstrap_Cluster.xlsx` | `efectos_bootstrap` |
| ICC y CFA multinivel | `salidas/Taller2_SEM_resultados_complementarios.xlsx` | `icc`, `multinivel_icc` |
| Medias y ranking por entidad | `salidas/Taller2_SEM_resultados_complementarios.xlsx` | `entidades_medias`, `entidades_ranking` |
| Straightlining y aquiescencia | `salidas/Taller2_SEM_resultados_complementarios.xlsx` | `patrones_resumen` |
| Sensibilidad clúster vs. pesos | `salidas/Actividad11_Sensibilidad_Diseno.xlsx` | `comparacion_ajuste`, `comparacion_cargas` |
| Modelos alternativos | `salidas/Actividad16_Alternativos.xlsx` | `ajuste_comparativo`, `discusion` |

**Trazabilidad:** cada cifra del paper se rastrea hasta una hoja del libro de resultados y hasta la línea del script que la produce. Esa es la diferencia entre un resultado reproducible y un resumen escrito a mano.

---

## Entregables y documentos

1. `salidas/Taller2_SEM_resultados.xlsx` — libro obligatorio con las hojas `ajuste`, `medicion`, `discriminante`, `hipotesis`, `efectos` e `invarianza`.
2. `salidas/Taller2_SEM_resultados_complementarios.xlsx` — 21 hojas con los análisis adicionales (ICC, bootstrap, sensibilidad, entidades, patrones, invarianza sectorial y multinivel).
3. Scripts de la Fase 2 nombrados por actividad (`Actividad 7_…` … `Actividad 21_…`), `00_run_all.R`, `CORRECCIONES.md` y este `README.md`.
4. `articulo.tex` + `referencias.bib` (y su compilación `Taller2_SEM.pdf`), con el artículo en LaTeX.
5. `informe/` — informe del Taller 1, del cual provienen la base analítica y la evidencia exploratoria.

Las cinco correcciones aplicadas al pipeline —M1 con `C03D ~~ C03E`, especificación única, residuos de correlación, bootstrap real por conglomerados y ajustes menores— están documentadas con su verificación en [`CORRECCIONES.md`](CORRECCIONES.md).

---

## Notas de reproducibilidad

- **Entorno fijado:** `renv.lock` registra R 4.5.2 y las versiones de **206 paquetes**. `renv::restore()` reconstruye ese entorno en cualquier máquina.
- **Rutas portables:** todos los scripts usan `here()`, nunca rutas absolutas. Por eso deben ejecutarse desde la raíz del proyecto.
- **Semillas fijas:** los scripts con componentes aleatorios fijan `set.seed(2026)`; el bootstrap usa `2026 + número de réplica` y el split de calibración/validación usa la misma semilla. Dos corridas sobre los mismos insumos producen los mismos resultados.
- **Datos originales:** `excel/` conserva la microbase del DANE tal como se descargó (y sí está versionada, ≈ 22 MB); `salidas/*.rds`, `logs/` y `.RData` están en `.gitignore`. El repositorio guarda el código y los libros `.xlsx`; los objetos pesados se reconstruyen.
- **Un solo modelo de medición:** desde la Actividad 4, el modelo estructural, los efectos y los alternativos usan el mismo AFC final (`modelo_final.rds`). La única excepción deliberada son las actividades de invarianza (12–14), que prueban el modelo configural sin la covarianza residual, tal como se declara en las limitaciones.
- **Cadena verificada:** la corrida completa del 2026-11-09 terminó en 40,8 minutos con 16/16 scripts exitosos y sin errores; los logs individuales quedaron en `logs/`. Si una corrida futura falla, compara su `.log` con esos.
- **Dependencias externas:** si actualizas el microdato del DANE, verifica las dimensiones (26.742 × 243) antes de correr el pipeline. Los paquetes `lavaan`, `semTools` y `semPlot` son la fuente más probable de diferencias menores entre versiones.

---

## Limitaciones

El repositorio reproduce el análisis, no elimina sus límites. Estas siete limitaciones están declaradas en el artículo y deben leerse junto con los resultados:

1. **Diseño transversal.** Un solo levantamiento; no permite establecer temporalidad ni causalidad. Los modelos equivalentes M2 y M3 lo ilustran de forma directa.
2. **Sesgo de método común.** Los tres constructos se miden con el mismo instrumento y el mismo informante, lo que puede inflar las asociaciones.
3. **Invarianza sin la covarianza residual.** Las pruebas de invarianza usan el modelo configural base, sin `C03D ~~ C03E`; incluirla obligaría a contrastar también su invarianza.
4. **Diseño muestral parcialmente incorporado.** El modelo principal usa el clúster pero no el factor de expansión, porque `lavaan` no admite ambos con WLSMV.
5. **Reespecificación post hoc.** `C03D ~~ C03E` se liberó tras examinar los índices de modificación; tuvo justificación teórica y se confirmó en validación, pero sigue siendo una decisión guiada por los datos.
6. **Representatividad.** La no respuesta y la eliminación por pares pueden afectar la representatividad de las estimaciones poblacionales.
7. **Ítem más débil.** C03D presenta la carga más baja (0,623) y comparte covarianza residual con C03E; futuras versiones del instrumento podrían revisar su redacción.

A ellas se suma la **discrepancia entre índices escalados y robustos** (RMSEA 0,050 vs. 0,070): el ajuste aproximado es aceptable pero no excelente, y el valor robusto se reporta sin ocultarlo.

---

## Equipo

**Integrantes:**

- **Kevin Leonardo Chaparro Reyes**
- **Valentina Muñoz Palma**
- **Paula Margarita Triana Ancinez**

**Curso:** Modelación de Ecuaciones Estructurales — Taller 2 (Fases 2 y 3)

**Docente:** Juan Carlos Rubriche

**Universidad:** Universidad Santo Tomás (USTA) — Facultad de Estadística — Bogotá D. C.

**Periodo:** 2026-II

---

## Cierre

Replicar este proyecto no es solo ejecutar 16 scripts: es reconstruir una cadena completa de evidencia, desde un microdato anonimizado del DANE hasta un coeficiente estructural con intervalo de confianza. Cada archivo de `salidas/` y cada figura de `figuras/` existen para que cualquier persona pueda auditar el camino: la base, el AFC, la reespecificación, la fiabilidad, la mediación, la invarianza y los análisis complementarios. Si `Rscript 00_run_all.R` corre limpio y `renv::status()` no encuentra problemas, el resultado está listo para defenderse.

**La medición viene primero; la estructura solo es interpretable cuando el instrumento se sostiene.**
