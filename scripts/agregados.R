# -----------------------------------------------------------------------------
# Agregados ponderados para el dashboard "Explorar" del sitio ENEM
# -----------------------------------------------------------------------------
# Lee el panel armonizado de rENEM (enem_load()) y calcula, para cada variable,
# ola y grupo, estimaciones ponderadas con su intervalo de confianza al 95 %.
# La app Shiny solo ve estos agregados, nunca los microdatos.
#
# Ejecutar desde la raíz del repo (R >= 4.2, locale UTF-8; en Windows es el default):
#   Rscript scripts/agregados.R
#
# Salida (se versionan en git; GitHub Actions exporta la app con ellos):
#   explorar/app-src/agregados.csv   una fila por anio x variable x grupo x nivel x categoria
#   explorar/app-src/variables.csv   metadatos de cada variable (etiquetas ES/EN, tipo, notas)
#
# Método: diseño ponderado con peso_final y ids = ~1 (el mismo supuesto que
# rENEM::enem_trend(); no hay UPM ni estratos en los .dta). Las no respuestas
# (NA) quedan fuera del denominador. Para escalas se calcula la media y la
# distribución; para categóricas, la proporción de cada categoría.
# IC 95 % por linealización (Wald), recortado a [0, 1] en proporciones.
# Se omiten las celdas con menos de N_MIN casos válidos.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages(library(survey))

N_MIN <- 30
SALIDA <- "explorar/app-src"

panel <- rENEM::enem_load()

# --- Grupos para desagregar ---------------------------------------------------
# Los cortes exactos de edad_grupo (1-4) no vienen etiquetados en los .dta;
# por eso se rotulan como grupos ordenados y no como rangos de edad.
panel$g_sexo <- ifelse(panel$mujer == 1, "Mujer", ifelse(panel$mujer == 0, "Hombre", NA))
panel$g_edad <- ifelse(panel$edad_grupo %in% 1:4, paste0("E", panel$edad_grupo), NA)
panel$g_escolaridad <- panel$escolaridad

grupos <- list(
  sexo        = list(col = "g_sexo",        es = "Sexo",        en = "Sex",
                     niveles = c(Hombre = "Hombre|Men", Mujer = "Mujer|Women")),
  edad        = list(col = "g_edad",        es = "Edad",        en = "Age",
                     niveles = c(E1 = "Grupo 1 (más joven)|Group 1 (youngest)", E2 = "Grupo 2|Group 2",
                                 E3 = "Grupo 3|Group 3", E4 = "Grupo 4 (mayor)|Group 4 (oldest)")),
  escolaridad = list(col = "g_escolaridad", es = "Escolaridad", en = "Education",
                     niveles = c(Ninguna = "Ninguna|None", Primaria = "Primaria|Primary",
                                 Secundaria = "Secundaria|Lower secondary",
                                 Preparatoria = "Preparatoria|Upper secondary",
                                 Universidad_o_mas = "Universidad o más|University or more"))
)

# --- Variables ----------------------------------------------------------------
# tipo "escala": numérica ordenada (media + distribución). "categorica": proporciones.
# cats: código = "etiqueta ES|etiqueta EN", en el orden en que se muestran.
termo <- c("0" = "0 · Nada|0 · Not at all", setNames(as.character(1:9), 1:9), "10" = "10 · Mucho|10 · A lot")
termo[2:10] <- paste0(termo[2:10], "|", termo[2:10])
izqder <- c("0" = "0 · Izquierda|0 · Left", setNames(as.character(1:9), 1:9), "10" = "10 · Derecha|10 · Right")
izqder[2:10] <- paste0(izqder[2:10], "|", izqder[2:10])
nota_ideo_partido <- paste0(
  "Ubicación del partido según la persona entrevistada (0 = izquierda, 10 = derecha).|",
  "Respondent's placement of the party (0 = left, 10 = right).")
nota_termo <- paste0(
  "Termómetro 0-10. 2015 aplicó la pregunta a cerca de la mitad de la muestra (versiones de cuestionario).|",
  "0-10 feeling thermometer. In 2015 the item was asked to about half the sample (split questionnaire).")

vars <- list(
  # Evaluaciones
  pdte_aprueba = list(tipo = "categorica", tema = "Evaluaciones|Evaluations",
    et = "Aprobación presidencial|Presidential approval",
    cats = c("1" = "Aprueba|Approves", "0" = "Desaprueba|Disapproves"),
    nota = "Aprueba = muy o algo de acuerdo con la manera como gobierna el presidente.|Approves = strongly or somewhat agrees with the way the president is governing."),
  pdte_acuerdo = list(tipo = "escala", tema = "Evaluaciones|Evaluations",
    et = "Aprobación presidencial (escala 1-4)|Presidential approval (1-4 scale)",
    cats = c("1" = "Muy en desacuerdo|Strongly disagree", "2" = "Algo en desacuerdo|Somewhat disagree",
             "3" = "Algo de acuerdo|Somewhat agree", "4" = "Muy de acuerdo|Strongly agree"),
    nota = "Mayor valor = mayor aprobación.|Higher = more approval."),
  econ_retro = list(tipo = "categorica", tema = "Evaluaciones|Evaluations",
    et = "Economía del país, último año|National economy, past year",
    cats = c(Mejoro = "Mejoró|Better", Igual = "Igual|Same", Empeoro = "Empeoró|Worse"),
    nota = "En 2006 y 2015 la pregunta se aplicó a cerca de la mitad de la muestra.|In 2006 and 2015 the item was asked to about half the sample."),
  limpieza_electoral = list(tipo = "escala", tema = "Evaluaciones|Evaluations",
    et = "Limpieza de la elección|Electoral integrity",
    cats = c("1" = "1 · No fue limpia|1 · Not clean", "2" = "2|2", "3" = "3|3", "4" = "4|4", "5" = "5 · Sí fue limpia|5 · Clean"),
    nota = "Escala 1-5 sobre la elección de esa ola.|1-5 scale about that wave's election."),
  satisfaccion_democracia = list(tipo = "escala", tema = "Evaluaciones|Evaluations",
    et = "Satisfacción con la democracia|Satisfaction with democracy",
    cats = c("1" = "1 · Nada satisfecho|1 · Not at all satisfied", "2" = "2|2", "3" = "3|3", "4" = "4 · Muy satisfecho|4 · Very satisfied"),
    nota = "Mayor valor = más satisfacción. En 2015 se aplicó a cerca de la mitad de la muestra.|Higher = more satisfied. In 2015 asked to about half the sample."),
  # Partidos e ideología
  pid_partido = list(tipo = "categorica", tema = "Partidos e ideología|Parties and ideology",
    et = "Identificación partidista|Party identification",
    cats = c(PAN = "PAN|PAN", PRI = "PRI|PRI", PRD = "PRD|PRD", MORENA = "MORENA|MORENA", PVEM = "PVEM|PVEM",
             PT = "PT|PT", MC = "MC|MC", Otro = "Otro|Other", Ninguno = "Ninguno|None"),
    nota = "1997-2012: filtro de simpatía + partido; 2015-2024: batería directa. «Otro» no es comparable entre olas.|1997-2012: sympathy filter + party; 2015-2024: direct battery. 'Other' is not comparable across waves."),
  ideologia_lr = list(tipo = "escala", tema = "Partidos e ideología|Parties and ideology",
    et = "Autoubicación izquierda-derecha|Left-right self-placement", cats = izqder,
    nota = "0 = izquierda, 10 = derecha. En 2015 se aplicó a cerca de la mitad de la muestra.|0 = left, 10 = right. In 2015 asked to about half the sample."),
  eval_pan  = list(tipo = "escala", tema = "Partidos e ideología|Parties and ideology", et = "Opinión del PAN|Feeling toward PAN",  cats = termo, nota = nota_termo),
  eval_pri  = list(tipo = "escala", tema = "Partidos e ideología|Parties and ideology", et = "Opinión del PRI|Feeling toward PRI",  cats = termo, nota = nota_termo),
  eval_prd  = list(tipo = "escala", tema = "Partidos e ideología|Parties and ideology", et = "Opinión del PRD|Feeling toward PRD",  cats = termo, nota = nota_termo),
  eval_pt   = list(tipo = "escala", tema = "Partidos e ideología|Parties and ideology", et = "Opinión del PT|Feeling toward PT",    cats = termo, nota = nota_termo),
  eval_pvem = list(tipo = "escala", tema = "Partidos e ideología|Parties and ideology", et = "Opinión del PVEM|Feeling toward PVEM", cats = termo, nota = nota_termo),
  ideologia_pan  = list(tipo = "escala", tema = "Partidos e ideología|Parties and ideology", et = "Ubicación ideológica del PAN|Left-right placement of PAN",  cats = izqder, nota = nota_ideo_partido),
  ideologia_pri  = list(tipo = "escala", tema = "Partidos e ideología|Parties and ideology", et = "Ubicación ideológica del PRI|Left-right placement of PRI",  cats = izqder, nota = nota_ideo_partido),
  ideologia_prd  = list(tipo = "escala", tema = "Partidos e ideología|Parties and ideology", et = "Ubicación ideológica del PRD|Left-right placement of PRD",  cats = izqder, nota = nota_ideo_partido),
  # Participación y conocimiento
  voto_reportado = list(tipo = "categorica", tema = "Participación y conocimiento|Participation and knowledge",
    et = "Votó en la elección|Reported turnout",
    cats = c(Si = "Sí votó|Voted", No = "No votó|Did not vote"),
    nota = "La elección referida alterna entre presidencial y legislativa en cada ola.|The election referred to alternates between presidential and midterm across waves."),
  conocimiento_camaras = list(tipo = "categorica", tema = "Participación y conocimiento|Participation and knowledge",
    et = "Sabe qué cámaras forman el Congreso|Knows the chambers of Congress",
    cats = c(Correcto = "Correcto|Correct", Incorrecto = "Incorrecto|Incorrect"),
    nota = "En 1997 y 2000 «Incorrecto» incluye no sabe / no contestó.|In 1997 and 2000 'Incorrect' includes don't know / no answer."),
  conocimiento_diputado_termino = list(tipo = "categorica", tema = "Participación y conocimiento|Participation and knowledge",
    et = "Sabe cuánto dura un diputado|Knows a deputy's term length",
    cats = c(Correcto = "Correcto|Correct", Incorrecto = "Incorrecto|Incorrect"),
    nota = "En 2000 «Incorrecto» incluye no sabe / no contestó.|In 2000 'Incorrect' includes don't know / no answer."),
  conocimiento_gobernador = list(tipo = "categorica", tema = "Participación y conocimiento|Participation and knowledge",
    et = "Sabe el nombre del gobernador|Knows the governor's name",
    cats = c(Correcto = "Correcto|Correct", Incorrecto = "Incorrecto|Incorrect"),
    nota = "Disponible en 2006, 2009, 2012, 2015, 2021 y 2024.|Available in 2006, 2009, 2012, 2015, 2021 and 2024."),
  # Perfil sociodemográfico
  escolaridad = list(tipo = "categorica", tema = "Perfil|Profile", et = "Escolaridad|Education",
    cats = grupos$escolaridad$niveles, nota = "Incompleta y completa se agrupan en cada nivel.|Incomplete and complete are grouped within each level."),
  religion = list(tipo = "categorica", tema = "Perfil|Profile", et = "Religión|Religion",
    cats = c(Catolica = "Católica|Catholic", Otra = "Otra|Other", Ninguna = "Ninguna|None"), nota = "|"),
  asistencia_religiosa = list(tipo = "escala", tema = "Perfil|Profile",
    et = "Asistencia a servicios religiosos|Religious attendance",
    cats = c("1" = "1 · Nunca|1 · Never", "2" = "2|2", "3" = "3|3", "4" = "4|4", "5" = "5|5", "6" = "6 · Semanal o más|6 · Weekly or more"),
    nota = "Escala 1-6; mayor valor = más frecuente.|1-6 scale; higher = more often."),
  etnia = list(tipo = "categorica", tema = "Perfil|Profile", et = "Autoadscripción étnica|Ethnic self-identification",
    cats = c(Mestizo = "Mestizo|Mestizo", Indigena = "Indígena|Indigenous", Blanco = "Blanco|White",
             Afrodescendiente = "Afrodescendiente|Afro-descendant", Otro = "Otro|Other"),
    nota = "Afrodescendiente es categoría explícita solo desde 2024; «Otro» no es comparable entre olas.|Afro-descendant is an explicit option only from 2024; 'Other' is not comparable across waves."),
  estado_civil = list(tipo = "categorica", tema = "Perfil|Profile", et = "Estado civil|Marital status",
    cats = c(Casado_UnionLibre = "Casado/unión libre|Married/cohabiting", Soltero = "Soltero|Single",
             Divorciado_Separado = "Divorciado/separado|Divorced/separated", Viudo = "Viudo|Widowed"), nota = "|"),
  actividad_principal = list(tipo = "categorica", tema = "Perfil|Profile", et = "Actividad principal|Main activity",
    cats = c(Ocupado = "Ocupado|Employed", Desocupado = "Desocupado|Unemployed",
             Fuera_fuerza_laboral = "Fuera de la fuerza laboral|Out of labor force"), nota = "|")
)

# --- Cálculo ------------------------------------------------------------------
dis <- svydesign(ids = ~1, weights = ~peso_final, data = panel)
partir <- function(x, i) vapply(strsplit(x, "|", fixed = TRUE), function(p) c(p, "")[i], "")

estimar_celda <- function(d, v, info, presentes) {
  y <- d$variables[[v]]
  n <- sum(!is.na(y) & is.finite(d$prob))   # casos válidos dentro del dominio
  if (n < N_MIN) return(NULL)
  out <- list()
  if (info$tipo == "escala") {
    m <- svymean(as.formula(paste0("~", v)), d, na.rm = TRUE)
    out[[1]] <- data.frame(categoria = "_media", est = coef(m)[[1]], ee = SE(m)[[1]])
  }
  d2 <- update(d, .f = factor(as.character(y), levels = names(info$cats)))
  p <- svymean(~.f, d2, na.rm = TRUE)
  out[[2]] <- data.frame(categoria = names(info$cats), est = as.numeric(coef(p)), ee = as.numeric(SE(p)))
  # Categorías que el cuestionario de esa ola no ofrece (p. ej. PVEM en 2024,
  # que va dentro de «Otro») no se reportan como 0 %: se omiten.
  out[[2]] <- out[[2]][out[[2]]$categoria %in% presentes, ]
  res <- do.call(rbind, out)
  res$n <- n
  res
}

filas <- list()
for (v in names(vars)) {
  info <- vars[[v]]
  message("· ", v)
  for (a in sort(unique(panel$anio))) {
    d_a <- subset(dis, anio == a)
    presentes <- unique(as.character(panel[[v]][panel$anio == a & !is.na(panel[[v]])]))
    celdas <- list(list(grupo = "total", nivel = "total", d = d_a))
    for (g in names(grupos)) {
      col <- grupos[[g]]$col
      for (lv in names(grupos[[g]]$niveles)) {
        celdas[[length(celdas) + 1]] <- list(grupo = g, nivel = lv,
                                             d = d_a[!is.na(d_a$variables[[col]]) & d_a$variables[[col]] == lv, ])
      }
    }
    for (cc in celdas) {
      r <- estimar_celda(cc$d, v, info, presentes)
      if (is.null(r)) next
      filas[[length(filas) + 1]] <- data.frame(anio = a, variable = v, grupo = cc$grupo, nivel = cc$nivel, r)
    }
  }
}
ag <- do.call(rbind, filas)
es_prop <- ag$categoria != "_media"
ag$li <- ag$est - 1.96 * ag$ee
ag$ls <- ag$est + 1.96 * ag$ee
ag$li[es_prop] <- pmax(0, ag$li[es_prop])
ag$ls[es_prop] <- pmin(1, ag$ls[es_prop])
num <- c("est", "ee", "li", "ls")
ag[num] <- lapply(ag[num], round, 4)

# --- Metadatos ----------------------------------------------------------------
meta_var <- do.call(rbind, lapply(names(vars), function(v) {
  i <- vars[[v]]
  data.frame(variable = v, tipo = i$tipo,
             tema_es = partir(i$tema, 1), tema_en = partir(i$tema, 2),
             etiqueta_es = partir(i$et, 1), etiqueta_en = partir(i$et, 2),
             nota_es = partir(i$nota, 1), nota_en = partir(i$nota, 2))
}))
meta_cat <- do.call(rbind, c(
  lapply(names(vars), function(v) {
    k <- vars[[v]]$cats
    data.frame(clase = "categoria", variable = v, codigo = names(k), orden = seq_along(k),
               etiqueta_es = partir(k, 1), etiqueta_en = partir(k, 2))
  }),
  lapply(names(grupos), function(g) {
    k <- grupos[[g]]$niveles
    rbind(
      data.frame(clase = "grupo", variable = g, codigo = "_grupo", orden = 0,
                 etiqueta_es = grupos[[g]]$es, etiqueta_en = grupos[[g]]$en),
      data.frame(clase = "nivel", variable = g, codigo = names(k), orden = seq_along(k),
                 etiqueta_es = partir(k, 1), etiqueta_en = partir(k, 2)))
  })
))

dir.create(SALIDA, showWarnings = FALSE, recursive = TRUE)
write.csv(ag, file.path(SALIDA, "agregados.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(meta_var, file.path(SALIDA, "variables.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(meta_cat, file.path(SALIDA, "etiquetas.csv"), row.names = FALSE, fileEncoding = "UTF-8")
message(sprintf("Listo: %d filas, %d variables -> %s", nrow(ag), length(vars), SALIDA))
