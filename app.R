library(tidyverse)
library(shiny)
library(shinydashboard)

# Cargar los datos
na_df <- readRDS("Data/01_na_df.rds")
kclusts <- readRDS("Data/02_kclusts.rds")
top_pca <- readRDS("Data/03_top_pca.rds")
augmented <- readRDS("Data/04_augmented.rds")

# Tema gráfico
theme_set(theme_minimal())
theme_update(
  text = element_text(size = 16, family = "Bahnschrift"),
  plot.title = element_text(family = "Century Gothic"),
  plot.caption = element_text(family = "PT Sans Caption"),
  plot.background = element_rect(fill = "#ecf0f5", color = "transparent"),
  panel.background = element_rect(fill = "#ecf0f5"),
  panel.grid = element_line(color = "gray85")
)

# set.seed(123)
# df <- data.frame(
#   a = rnorm(100),
#   b = rnorm(100) ^ 2 + rexp(100),
#   c = runif(100),
#   g = rep(c("g1", "g2", "g3", "g4"), 25)
# )

ui <- dashboardPage(
  dashboardHeader(title = "Encuesta Longitudinal"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introducción", tabName = "intro", icon = icon("chess-knight")),
      menuItem("Reporte de ML", tabName = "report", icon = icon("newspaper")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        h2("Encuesta Longitudinal Empresarial"),
        p("El Ministerio de Economía pone a disposición pública la Quinta Encuesta Longitudinal de Empresas (ELE-5). Esta encuesta ha sido elaborada en conjunto por el Instituto Nacional de Estadísticas y la Unidad de Estudios del Ministerio de Economía."),
        p("Se agradece al Ministerio de Economía de Chile el poder compartir la información en su sitio web.
El sitio web original de los datos ", a(href='https://www.economia.gob.cl/2019/03/12/quinta-encuesta-longitudinal-de-empresas-ele5.htm', "se encuentra aquí"), "."),
        h2("Descripción de los datos"),
        p("El dataset está compuesto por 6480 filas y 688 columnas. Entra estas columnas, 679 corresponden a preguntas de la encuesta, subdivididas en 10 tópicos principales (entre la letra A y la J). Debido a la naturaleza de las encuestas, se generó gran cantidad de datos faltantes, como se muestra en la figura a continuación:"),
        plotOutput("na_plot", width = "100%"),
        p("Se da tratamiento a los datos faltantes.")
      ),
tabItem(tabName = "report",
        h2("Reporte")),
tabItem(tabName = "dashboard",
        h2("Dashboard"))
    )
  )
)

server <- function(input, output, session) {
  
  segment_data <- tribble(
    ~x, ~xend, ~y, ~yend,
    .25, .35, 200, 230
  )
  
  output$na_plot <- renderPlot({
    na_df |> 
      ggplot(aes(pct)) +
      geom_histogram(aes(fill = pct > .25),
                     bins = 20, show.legend = FALSE, alpha = .8, color = "#555555") +
      geom_vline(xintercept = .25, lty = 3) +
      geom_curve(data = segment_data, 
                 aes(x = x, y = y, xend = xend, yend = yend),
                 curvature = -.25) +
      annotate(geom = "text", x = .6, y = 230, size = 5, family = "Bahnschrift",
               label = "171 de las 688 columnas sufren de\nun 25% o más de valores faltantes") +
      scale_fill_manual(values = c("#65dfa5", "#cccccc")) +
      labs(title = "Histograma de valores faltantes por columna",
           x = "% de faltantes", 
           y = "# de columnas",
           caption = "5ta Encuesta Longitudinal de Empresas\nMinisterio de Economía con INE Chile") 
  }, res = 96, width = 700, height = 400)
  
  # output$plot <- renderPlot({
  #   df |> 
  #     ggplot(aes(!!input$v)) +
  #     geom_histogram(bins = 15, fill = "dodgerblue4") +
  #     facet_wrap(~g)
  # }, res = 96)
  # 
  # output$table <- renderTable({
  #   head(df)
  # })
}

shinyApp(ui, server)