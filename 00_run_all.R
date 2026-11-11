# =====================================================================
# 00_run_all.R — Ejecutor maestro del proyecto SEM (EDI 2025)
# Taller 2 - Fases 2 y 3 | USTA
# =====================================================================
# Corre en orden todos los scripts de la Fase 2 y deja el registro de cada
# ejecucion en logs/. Se detiene en el primer error.
#
# Uso:
#   Rscript 00_run_all.R                 # solo Fase 2 (asume base del Taller 1)
#   RUN_FASE1=1 Rscript 00_run_all.R     # Fase 1 + Fase 2 desde cero
#
# Requisitos: paquetes del renv.lock instalados (renv::restore()).
# =====================================================================

t_inicio <- Sys.time()

# --- Configuracion ----------------------------------------------------
raiz     <- getwd()
dir_logs <- file.path(raiz, "logs")
if (!dir.exists(dir_logs)) dir.create(dir_logs)

rscript  <- file.path(R.home("bin"),
                      if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

# Fase 1 (Taller 1): solo si se pide explicitamente
scripts_fase1 <- c(
  "r/Actividad 1_metadatos.R",
  "r/Actividad 2_construccion.R",
  "r/Actividad 3_faltantes.R",
  "r/Actividad 4_descriptiva.R",
  "r/Actividad 5_asociación.R",
  "r/Actividad 6_modelo.R"
)

# Fase 2 (Taller 2): orden estricto de dependencias (cada script lee los
# .rds que deja el anterior)
scripts_fase2 <- c(
  "r/01_setup_fase2.R",
  "r/02_afc.R",
  "r/03_ajuste_local.R",
  "r/04_confiabilidad_validez.R",
  "r/05_sensibilidad_diseno.R",
  "r/06_estructural.R",
  "r/07_efectos_mediacion_mc.R",
  "r/08_efectos_bootstrap_cluster.R",
  "r/09_entidades.R",
  "r/10_patrones_respuesta.R",
  "r/11_modelos_alternativos.R",
  "r/12_multigrupo_medicion.R",
  "r/13_multigrupo_estructural.R",
  "r/14_multigrupo_sector.R",
  "r/15_cfa_multinivel.R",
  "r/16_libro_resultados.R"
)

# --- Funcion de ejecucion --------------------------------------------
ejecutar <- function(script) {
  ruta <- file.path(raiz, script)
  if (!file.exists(ruta)) stop("No existe el script: ", script)

  cat("\n============================================================\n")
  cat(">> ", script, "  (", format(Sys.time(), "%H:%M:%S"), ")\n", sep = "")
  cat("============================================================\n")

  log <- file.path(dir_logs, paste0(tools::file_path_sans_ext(basename(script)), ".log"))
  salida <- system2(
    rscript,
    args   = c("--vanilla", shQuote(ruta)),
    stdout = TRUE,
    stderr = TRUE
  )
  writeLines(salida, log)

  estado <- attr(salida, "status")
  if (is.null(estado)) estado <- 0L

  # Salida resumida en consola (ultimas lineas)
  cat(paste(tail(salida, 5), collapse = "\n"), "\n")

  if (estado != 0L) {
    stop("Fallo en ", script, " (exit ", estado, "). Ver ", log)
  }
  invisible(TRUE)
}

# --- Ejecucion --------------------------------------------------------
cat("================================================================\n")
cat("PROYECTO SEM EDI 2025 - EJECUCION COMPLETA\n")
cat("Raiz:", raiz, "\n")
cat("R:", R.version.string, "\n")
cat("Inicio:", format(t_inicio, "%Y-%m-%d %H:%M:%S"), "\n")
cat("================================================================\n")

if (identical(Sys.getenv("RUN_FASE1"), "1")) {
  cat("\n### FASE 1 (Taller 1) ###\n")
  for (s in scripts_fase1) ejecutar(s)
} else {
  cat("\n(Fase 1 omitida; use RUN_FASE1=1 para regenerar la base del Taller 1)\n")
}

cat("\n### FASE 2 (Taller 2) ###\n")
for (s in scripts_fase2) ejecutar(s)

# --- Resumen ----------------------------------------------------------
t_fin <- Sys.time()
duracion <- round(as.numeric(difftime(t_fin, t_inicio, units = "mins")), 1)

cat("\n================================================================\n")
cat("EJECUCION COMPLETADA en ", duracion, " minutos\n", sep = "")
cat("Fin:", format(t_fin, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Productos principales:\n")
cat("  salidas/Taller2_SEM_resultados.xlsx\n")
cat("  salidas/Actividad13b_Bootstrap_Cluster.xlsx\n")
cat("  salidas/Actividad17_Invarianza_Medicion.xlsx\n")
cat("  salidas/Actividad9_Ajuste_Local.xlsx\n")
cat("  figuras/act12_modelo_estructural.png\n")
cat("Logs: logs/\n")
cat("================================================================\n")

sessionInfo()
