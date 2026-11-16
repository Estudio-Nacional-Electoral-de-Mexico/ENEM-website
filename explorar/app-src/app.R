# -----------------------------------------------------------------------------
# Explorar ENEM: dashboard Shiny del sitio, exportado a estático con Shinylive.
# -----------------------------------------------------------------------------
# Datos: agregados.csv, variables.csv y etiquetas.csv, generados por
# scripts/agregados.R (no hay microdatos en la app).
# Idioma: ?lang=en en la URL abre la versión en inglés; también hay un selector.
# Solo usa paquetes disponibles en webR: shiny, bslib, ggplot2.
#
# Prueba local:     shiny::runApp("explorar/app-src")
# Export local:     shinylive::export("explorar/app-src", "explorar/app")
# (En GitHub Actions el export se hace solo en cada push; ver publish.yml.)
# -----------------------------------------------------------------------------
library(shiny)
library(bslib)
library(ggplot2)

`%||%` <- function(a, b) if (is.null(a)) b else a

leer <- function(f) read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8")
ag   <- leer("agregados.csv")
vars <- leer("variables.csv")
etq  <- leer("etiquetas.csv")
ag$categoria <- as.character(ag$categoria)
etq$codigo   <- as.character(etq$codigo)

VERDE   <- "#0E5A4F"
GRUPO_COLORES <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4")  # orden fijo
GRUPOS  <- c("total", "sexo", "edad", "escolaridad")
PRESIDENCIALES <- c(2000, 2006, 2012, 2018, 2024)

# --- Textos de la interfaz ----------------------------------------------------
TX <- list(
  es = list(
    tab_tend = "Tendencias 1997–2024", tab_dist = "Distribución por ola",
    variable = "Variable", indicador = "Indicador", promedio = "Promedio",
    grupo = "Desagregar por", total = "Total", anio = "Ola", ic = "Mostrar intervalo de confianza (95 %)",
    descargar = "Descargar CSV", tabla = "Ver tabla",
    col_anio = "Ola", col_nivel = "Grupo", col_cat = "Respuesta", col_est = "Estimación",
    col_ic = "IC 95 %", col_n = "n",
    metodo = paste("Estimaciones ponderadas con el ponderador final (peso_final) del panel armonizado de rENEM.",
                   "Se excluyen no sabe / no contestó. Se omiten las celdas con menos de 30 casos."),
    hover = "Pasa el cursor sobre un punto para ver su valor.",
    pres = "Elección presidencial", inter = "Intermedia",
    sin_datos = "No hay datos suficientes para esta combinación.",
    pct = "% de quienes respondieron"),
  en = list(
    tab_tend = "Trends 1997–2024", tab_dist = "Distribution by wave",
    variable = "Variable", indicador = "Indicator", promedio = "Mean",
    grupo = "Break down by", total = "Total", anio = "Wave", ic = "Show 95% confidence interval",
    descargar = "Download CSV", tabla = "Show table",
    col_anio = "Wave", col_nivel = "Group", col_cat = "Response", col_est = "Estimate",
    col_ic = "95% CI", col_n = "n",
    metodo = paste("Weighted estimates using the final weight (peso_final) of the rENEM harmonized panel.",
                   "Don't know / no answer are excluded. Cells with fewer than 30 cases are omitted."),
    hover = "Hover over a point to see its value.",
    pres = "Presidential election", inter = "Midterm",
    sin_datos = "Not enough data for this combination.",
    pct = "% of respondents")
)

# --- Utilidades ---------------------------------------------------------------
et_var   <- function(v, l) vars[[paste0("etiqueta_", l)]][vars$variable == v]
nota_var <- function(v, l) vars[[paste0("nota_", l)]][vars$variable == v]
tipo_var <- function(v) vars$tipo[vars$variable == v]

cats_de <- function(v, l) {
  e <- etq[etq$clase == "categoria" & etq$variable == v, ]
  e <- e[order(e$orden), ]
  setNames(e$codigo, e[[paste0("etiqueta_", l)]])
}
niveles_de <- function(g, l) {
  if (g == "total") return(setNames("total", TX[[l]]$total))
  e <- etq[etq$clase == "nivel" & etq$variable == g, ]
  e <- e[order(e$orden), ]
  setNames(e$codigo, e[[paste0("etiqueta_", l)]])
}
grupo_choices <- function(l) {
  e <- etq[etq$clase == "grupo", ]
  e <- e[match(GRUPOS[-1], e$variable), ]
  c(setNames("total", TX[[l]]$total), setNames(e$variable, e[[paste0("etiqueta_", l)]]))
}
var_choices <- function(l) {
  temas <- unique(vars[[paste0("tema_", l)]])
  out <- lapply(temas, function(t) {
    s <- vars[vars[[paste0("tema_", l)]] == t, ]
    setNames(s$variable, s[[paste0("etiqueta_", l)]])
  })
  names(out) <- temas
  out
}
rango_escala <- function(v) range(as.numeric(cats_de(v, "es")))

fmt_num <- function(x, prop, l) {
  if (prop) x <- 100 * x
  s <- formatC(x, format = "f", digits = 1)
  if (l == "es") s <- sub(".", ",", s, fixed = TRUE)
  if (prop) paste0(s, if (l == "es") " %" else "%") else s
}
fmt_pct_eje <- function(l) function(x) paste0(formatC(100 * x, format = "f", digits = 0), if (l == "es") " %" else "%")

tema_enem <- function() {
  theme_minimal(base_size = 15, base_family = "sans") +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#E6E8E7", linewidth = .5),
          axis.text = element_text(colour = "#3F3F3F"), axis.title = element_text(colour = "#3F3F3F", size = 12),
          legend.position = "top", legend.justification = "left", legend.title = element_blank(),
          legend.text = element_text(size = 13), legend.margin = margin(0, 0, 0, 0),
          legend.box.spacing = unit(4, "pt"), plot.margin = margin(8, 24, 8, 8))
}
separar <- function(y, gap) {
  o <- order(y); z <- y[o]
  for (i in seq_along(z)[-1]) if (z[i] - z[i - 1] < gap) z[i] <- z[i - 1] + gap
  z <- z - max(0, (mean(z) - mean(y[o])))  # recentra el bloque
  out <- y; out[o] <- z; out
}
colores_para <- function(niveles) {
  if (length(niveles) == 1) return(setNames(VERDE, niveles))
  setNames(GRUPO_COLORES[seq_along(niveles)], niveles)
}

# --- UI -----------------------------------------------------------------------
tema_bs <- bs_theme(
  version = 5, bg = "#FFFFFF", fg = "#000000", primary = VERDE, secondary = "#3F3F3F",
  base_font = font_google("Public Sans", local = FALSE),
  heading_font = font_google("Source Serif 4", local = FALSE),
  "border-radius" = "8px"
)

ui <- page_fluid(
  theme = tema_bs,
  tags$head(tags$style(HTML("
    body { padding: 12px 8px; }
    .barra { display:flex; justify-content:space-between; align-items:center; gap:1rem; margin-bottom:.5rem; flex-wrap:wrap; }
    .barra h1 { font-size:1.25rem; margin:0; }
    .idioma .btn { padding:2px 10px; font-size:.85rem; }
    .nota { font-size:.85rem; color:#3F3F3F; margin:.25rem 0 0; }
    .hover { font-size:.9rem; min-height:1.4em; color:#000; font-variant-numeric: tabular-nums; }
    .sidebar .form-label, .sidebar .control-label { font-weight:600; }
    table { font-variant-numeric: tabular-nums; }
    .nav-underline .nav-link.active { color:#0E5A4F; border-bottom-color:#0E5A4F; }
  "))),
  uiOutput("cuerpo")
)

# --- Server -------------------------------------------------------------------
server <- function(input, output, session) {
  lang <- reactiveVal("es")
  observe({
    q <- parseQueryString(session$clientData$url_search %||% "")
    if (identical(q$lang, "en")) lang("en")
  }) |> bindEvent(session$clientData$url_search, once = TRUE)
  observe(lang(input$idioma)) |> bindEvent(input$idioma, ignoreInit = TRUE)

  # La interfaz completa se regenera al cambiar de idioma, conservando la selección.
  output$cuerpo <- renderUI({
    l <- lang(); t <- TX[[l]]
    prev <- function(id, def) { v <- isolate(input[[id]]); if (is.null(v)) def else v }
    t_var <- prev("t_var", "pdte_aprueba"); d_var <- prev("d_var", "pid_partido")
    tagList(
      div(class = "barra",
          h1(if (l == "es") "Explorar el ENEM" else "Explore ENEM"),
          div(class = "idioma", radioButtons("idioma", NULL, c(ES = "es", EN = "en"), selected = l, inline = TRUE))),
      navset_card_underline(
        id = "pestana", selected = prev("pestana", "tend"),
        nav_panel(t$tab_tend, value = "tend",
          layout_sidebar(
            sidebar = sidebar(width = 300, open = "desktop",
              selectInput("t_var", t$variable, var_choices(l), selected = t_var),
              uiOutput("t_ind_ui"),
              radioButtons("t_grupo", t$grupo, grupo_choices(l), selected = prev("t_grupo", "total")),
              checkboxInput("t_ic", t$ic, value = prev("t_ic", TRUE)),
              downloadButton("t_csv", t$descargar, class = "btn-outline-primary btn-sm")),
            div(class = "hover", textOutput("t_hover")),
            plotOutput("t_plot", height = "430px", hover = hoverOpts("t_hov", delay = 80, delayType = "debounce")),
            uiOutput("t_nota"),
            tags$details(tags$summary(t$tabla), tableOutput("t_tabla")))),
        nav_panel(t$tab_dist, value = "dist",
          layout_sidebar(
            sidebar = sidebar(width = 300, open = "desktop",
              selectInput("d_var", t$variable, var_choices(l), selected = d_var),
              uiOutput("d_anio_ui"),
              radioButtons("d_grupo", t$grupo, grupo_choices(l), selected = prev("d_grupo", "total")),
              downloadButton("d_csv", t$descargar, class = "btn-outline-primary btn-sm")),
            plotOutput("d_plot", height = "460px"),
            uiOutput("d_nota"),
            tags$details(tags$summary(t$tabla), tableOutput("d_tabla"))))
      )
    )
  })

  # ---------------- Tendencias ----------------
  output$t_ind_ui <- renderUI({
    req(input$t_var); l <- lang(); v <- input$t_var
    ch <- cats_de(v, l)
    if (tipo_var(v) == "escala") ch <- c(setNames("_media", TX[[l]]$promedio), ch)
    sel <- isolate(input$t_ind)
    if (is.null(sel) || !sel %in% ch) sel <- ch[[1]]
    selectInput("t_ind", TX[[l]]$indicador, ch, selected = sel)
  })

  t_datos <- reactive({
    req(input$t_var, input$t_ind, input$t_grupo)
    l <- lang(); g <- input$t_grupo
    d <- ag[ag$variable == input$t_var & ag$categoria == input$t_ind & ag$grupo == g, ]
    req(nrow(d) > 0)
    nv <- niveles_de(g, l)
    nv <- nv[nv %in% d$nivel]
    # Rejilla completa para que las olas sin dato corten la línea en vez de unirla.
    rej <- expand.grid(anio = sort(unique(ag$anio)), nivel = unname(nv), stringsAsFactors = FALSE)
    d <- merge(rej, d, all.x = TRUE)
    d$etiqueta <- factor(names(nv)[match(d$nivel, nv)], levels = names(nv))
    d$prop <- input$t_ind != "_media"
    d[order(d$nivel, d$anio), ]
  })

  output$t_plot <- renderPlot({
    d <- t_datos(); l <- lang(); prop <- d$prop[1]
    cols <- colores_para(levels(d$etiqueta))
    ult <- do.call(rbind, lapply(split(d[!is.na(d$est), ], d$etiqueta[!is.na(d$est)], drop = TRUE),
                                 function(x) x[which.max(x$anio), ]))
    p <- ggplot(d, aes(anio, est, colour = etiqueta, fill = etiqueta, group = etiqueta))
    if (isTRUE(input$t_ic)) p <- p + geom_ribbon(aes(ymin = li, ymax = ls), alpha = if (length(cols) > 1) .07 else .12, colour = NA)
    p <- p + geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point(size = 3, na.rm = TRUE, stroke = 1.5, shape = 21, fill = "white") +
      scale_colour_manual(values = cols, aesthetics = c("colour", "fill")) +
      scale_x_continuous(breaks = sort(unique(ag$anio)), expand = expansion(mult = c(.02, .14))) +
      labs(x = NULL, y = if (prop) TX[[l]]$pct else TX[[l]]$promedio) +
      tema_enem()
    if (nrow(ult) > 0) {
      ult$lab <- fmt_num(ult$est, prop, l)
      # Separa verticalmente las etiquetas finales para que no se encimen.
      rng <- if (prop) diff(range(c(0, d$ls), na.rm = TRUE)) else diff(rango_escala(input$t_var))
      ult$y_lab <- separar(ult$est, gap = 0.055 * rng)
    }
    if (nrow(ult) > 0 && length(cols) <= 5)
      p <- p + geom_text(data = ult, aes(x = anio + 0.7, y = y_lab, label = lab), hjust = 0, size = 4.3,
                         colour = "#000000", show.legend = FALSE)
    if (prop) {
      p <- p + scale_y_continuous(labels = fmt_pct_eje(l), limits = c(0, NA), expand = expansion(mult = c(0, .05)))
    } else {
      r <- rango_escala(input$t_var)
      p <- p + scale_y_continuous(limits = r, breaks = pretty(r))
    }
    if (length(cols) == 1) p <- p + theme(legend.position = "none")
    if (length(cols) > 3) p <- p + guides(colour = guide_legend(nrow = 2, byrow = TRUE),
                                          fill = guide_legend(nrow = 2, byrow = TRUE))
    p
  }, res = 96)

  output$t_hover <- renderText({
    l <- lang(); h <- input$t_hov
    if (is.null(h)) return(TX[[l]]$hover)
    d <- t_datos(); d <- d[!is.na(d$est), ]
    np <- nearPoints(d, h, xvar = "anio", yvar = "est", threshold = 12, maxpoints = 1)
    if (nrow(np) == 0) return(TX[[l]]$hover)
    tipo <- if (np$anio %in% PRESIDENCIALES) TX[[l]]$pres else TX[[l]]$inter
    sprintf("%d (%s) · %s: %s  [%s – %s] · n = %s", np$anio, tolower(tipo), np$etiqueta,
            fmt_num(np$est, np$prop, l), fmt_num(np$li, np$prop, l), fmt_num(np$ls, np$prop, l),
            format(np$n, big.mark = ","))
  })

  tabla_fmt <- function(d, l, con_cat = FALSE) {
    t <- TX[[l]]; d <- d[!is.na(d$est), ]
    out <- data.frame(d$anio, as.character(d$etiqueta))
    names(out) <- c(t$col_anio, t$col_nivel)
    if (con_cat) out[[t$col_cat]] <- as.character(d$respuesta)
    out[[t$col_est]] <- fmt_num(d$est, d$prop[1], l)
    out[[t$col_ic]]  <- paste(fmt_num(d$li, d$prop[1], l), "–", fmt_num(d$ls, d$prop[1], l))
    out[[t$col_n]]   <- format(d$n, big.mark = ",")
    out
  }
  output$t_tabla <- renderTable(tabla_fmt(t_datos(), lang()), striped = TRUE, spacing = "s")

  nota_ui <- function(v, l) div(
    if (nzchar(nota_var(v, l))) p(class = "nota", HTML(paste0("<strong>", et_var(v, l), ":</strong> ", nota_var(v, l)))),
    p(class = "nota", TX[[l]]$metodo))
  output$t_nota <- renderUI({ req(input$t_var); nota_ui(input$t_var, lang()) })

  csv_handler <- function(datos, nombre) downloadHandler(
    filename = function() paste0("enem_", nombre(), ".csv"),
    content = function(file) {
      d <- datos(); d <- d[!is.na(d$est), c("anio", "variable", "grupo", "nivel", "categoria", "est", "li", "ls", "n")]
      write.csv(d, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  output$t_csv <- csv_handler(t_datos, function() paste(input$t_var, input$t_ind, input$t_grupo, sep = "_"))

  # ---------------- Distribución ----------------
  output$d_anio_ui <- renderUI({
    req(input$d_var); l <- lang()
    an <- sort(unique(ag$anio[ag$variable == input$d_var & ag$grupo == "total"]), decreasing = TRUE)
    sel <- isolate(input$d_anio)
    if (is.null(sel) || !sel %in% an) sel <- an[1]
    selectInput("d_anio", TX[[l]]$anio, an, selected = sel)
  })

  d_datos <- reactive({
    req(input$d_var, input$d_anio, input$d_grupo)
    l <- lang(); g <- input$d_grupo
    d <- ag[ag$variable == input$d_var & ag$anio == as.integer(input$d_anio) &
            ag$grupo == g & ag$categoria != "_media", ]
    req(nrow(d) > 0)
    nv <- niveles_de(g, l); nv <- nv[nv %in% d$nivel]
    cs <- cats_de(input$d_var, l); cs <- cs[cs %in% d$categoria]
    d$etiqueta  <- factor(names(nv)[match(d$nivel, nv)], levels = names(nv))
    d$respuesta <- factor(names(cs)[match(d$categoria, cs)], levels = names(cs))
    d$prop <- TRUE
    d$lab <- fmt_num(d$est, TRUE, l)
    d[order(d$respuesta, d$etiqueta), ]
  })

  output$d_plot <- renderPlot({
    d <- d_datos(); l <- lang()
    cols <- colores_para(levels(d$etiqueta))
    dg <- position_dodge(width = .8)
    muchas <- nlevels(d$respuesta) > 6
    p <- ggplot(d, aes(respuesta, est, fill = etiqueta)) +
      geom_col(position = dg, width = .76, colour = "white", linewidth = .6) +
      geom_errorbar(aes(ymin = li, ymax = ls), position = dg, width = .2, colour = "#3F3F3F", linewidth = .4) +
      scale_fill_manual(values = cols) +
      scale_y_continuous(labels = fmt_pct_eje(l), expand = expansion(mult = c(0, .1))) +
      labs(x = NULL, y = TX[[l]]$pct) +
      tema_enem()
    if (length(cols) > 3) p <- p + guides(fill = guide_legend(nrow = 2, byrow = TRUE))
    if (length(cols) == 1) {
      p <- p + theme(legend.position = "none") +
        geom_text(aes(y = ls, label = lab), vjust = -0.5, size = if (muchas) 3.4 else 4.2)
    }
    if (muchas) p <- p + theme(axis.text.x = element_text(angle = 35, hjust = 1))
    p
  }, res = 96)

  output$d_tabla <- renderTable(tabla_fmt(d_datos(), lang(), con_cat = TRUE), striped = TRUE, spacing = "s")
  output$d_nota  <- renderUI({ req(input$d_var); nota_ui(input$d_var, lang()) })
  output$d_csv   <- csv_handler(d_datos, function() paste(input$d_var, input$d_anio, input$d_grupo, sep = "_"))
}

shinyApp(ui, server)
