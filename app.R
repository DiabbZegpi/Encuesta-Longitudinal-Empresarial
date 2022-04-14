library(tidyverse)
library(shiny)
library(shinydashboard)
library(tidytext)
library(plotly)
library(scales)

# Cargar los datos
na_df <- readRDS("Data/01_na_df.rds")
kclusts <- readRDS("Data/02_kclusts.rds")
top_pca <- readRDS("Data/03_top_pca.rds")
augmented <- readRDS("Data/04_augmented.rds")

# Preprocesamiento de PC para el gráfico final
scale_pc <- function(x) {
  for (i in seq_along(x)) {
    if (x[i] < 0) {
      x[i] <- -sqrt(abs(x[i]))
    } else {
      x[i] <- sqrt(x[i])
    }
  }
  scaled <- (x - mean(x)) / sd(x)
  return (scaled)
}

augmented <- augmented |> 
  mutate(across(starts_with("PC"), scale_pc))


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


ui <- dashboardPage(
  dashboardHeader(title = "Encuesta Longitudinal"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introducción", tabName = "intro", icon = icon("chess-knight")),
      menuItem("Reporte de Clustering", tabName = "report", icon = icon("newspaper")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tags$style(HTML("


                .box.box-solid.box-primary>.box-header {

                }

                .box.box-solid.box-primary{

                background: #ecf0f5
                }

                ")),
    tabItems(
      tabItem(
        tabName = "intro",
        h2("Encuesta Longitudinal Empresarial"),
        p("El Ministerio de Economía pone a disposición pública la Quinta Encuesta Longitudinal de Empresas (ELE-5). Esta encuesta ha sido elaborada en conjunto por el Instituto Nacional de Estadísticas y la Unidad de Estudios del Ministerio de Economía."),
        p("Se agradece al Ministerio de Economía de Chile el poder compartir la información en su sitio web.
El sitio web original de los datos ", a(href='https://www.economia.gob.cl/2019/03/12/quinta-encuesta-longitudinal-de-empresas-ele5.htm', "se encuentra aquí"), "."),
p("El objetivo de este trabajo es encontrar la mejor segmentación posible de empresas, usando el algoritmo de clustering k-means."),
h2("Descripción de los datos"),
p("El dataset está compuesto por 6480 filas y 688 columnas. Entra estas columnas, 679 corresponden a preguntas de la encuesta, subdivididas en 10 tópicos principales (entre la letra A y la J). Debido a la naturaleza de las encuestas, se generó gran cantidad de datos faltantes, como se muestra en la figura a continuación:"),
plotOutput("na_plot", width = "100%"),
p("Se da tratamiento a los datos faltantes, eliminando las columnas con más de 25% de missing data y , posteriormente, omitiendo las filas con algún NA.")
      ),
tabItem(
  tabName = "report",
  h2("Reporte de Clustering"),
  p("La preparación de los datos para clustering consiste en las 5 etapas siguientes:"),
  tags$ul(
    tags$li("Dividir el dataset en entrenamiento (75%) y testing (25%)"),
    tags$li("Eliminar columnas con varianza igual a cero"),
    tags$li("Normalizar variables"),
    tags$li("Eliminar variables con correlación lineal mayor que 80%"),
    tags$li("Entrenar los primeros 100 componentes principales")
  ),
  fluidRow(
    column(width = 6,
           plotOutput("optim_k", width = "100%")),
    column(width = 6,
           h2("Elección de k"),
           p("La elección del número de clusters para el algoritmo k-means se efetuó con el método del codo, punto en que la suma de los cuadrados intra cluster sufre un reduce la velocidad con que decrese, en la medida que se aumenta k."))
  ),
  fluidRow(
    column(width = 6,
           h2("Información contenida por los PC"),
           tableOutput("pca_table")),
    column(width = 6,
           plotOutput("pca_plot", width = "100%"))
  )
),
tabItem(tabName = "dashboard",
        h2("Dashboard"),
        fluidRow(
          box(plotlyOutput("cluster_plot"), solidHeader = TRUE, status = "primary"),
          
          box(h2("Control del gráfico"),
              "Seleccione los componentes principales a graficar",
              varSelectInput("var1", label = "Primer componente", data = augmented |> select(starts_with("PC")), selected = "PC001"),
              varSelectInput("var2", label = "Segundo componente", data = augmented |> select(starts_with("PC")), selected = "PC002"), 
              solidHeader = TRUE, status = "primary")
        ),
        
        fluidRow(
          box(tableOutput("first_pc"), solidHeader = TRUE, status = "primary", title = "Resumen del primer componente (normalizado)"),
          box(tableOutput("second_pc"), solidHeader = TRUE, status = "primary", title = "Resumen del segundo componente (normalizado)")
        )
        

    )
  )
))

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
      labs(title = "Histograma de valores faltantes",
           x = "% de faltantes", 
           y = "# de columnas",
           caption = "5ta Encuesta Longitudinal de Empresas\nMinisterio de Economía con INE Chile") 
  }, res = 96, width = 600, height = 400)
  
  
  output$optim_k <- renderPlot({
    kclusts |> 
      unnest(glanced) |> 
      ggplot(aes(k, tot.withinss)) +
      geom_line(alpha = .5, size = 1.2, color = "#65dfa5") +
      geom_point(size = 4, fill = "#65dfa5", color = "#ecf0f5", shape = 21, stroke = 1.5) +
      geom_curve(aes(x = 3, xend = 3.5, y = 732335, yend = 7.5e5), 
                 curvature = -.25) +
      geom_point(data = kclusts |> unnest(glanced) |> filter(k == 3),
                 size = 4, fill = "#658fa5", color = "#ecf0f5", shape = 21) +
      annotate(geom = "text", x = 3.6, y = 7.55e5, size = 5, family = "Bahnschrift",
               label = "Clasificar las empresas en\n3 segmentos parece razonable",
               hjust = 0) +
      scale_x_continuous(breaks = c(1, 3, 5, 7, 9)) +
      labs(y = "Suma de cuadrados intra cluster", 
           title = "Método del codo en el conjunto de testing")
  }, res = 96, width = 700, height = 400)
  
  
  output$pca_plot <- renderPlot({
    top_pca |> 
      ggplot(aes(abs(value), terms, fill = value < 0)) +
      geom_col(alpha = .8, width = .8) +
      scale_y_reordered() +
      scale_x_continuous(labels = scales::label_number(accuracy = .01), n.breaks = 3) +
      scale_fill_manual(values = c("#65dfa5", "#EB290E"),
                        labels = c("positiva", "negativa")) +
      facet_wrap(~component, scales = "free", ncol = 2) +
      labs(x = NULL,
           y = "Pregunta de la encuesta", 
           title = "Las 8 preguntas más importantes por PC",
           fill = "contribución") +
      theme(legend.position = "right")
  }, res = 96, width = 600, height = 400)
  
  
  output$pca_table <- renderTable({
    tribble(
      ~PC1, ~PC2, ~PC3, ~PC4,
      "Valores positivos en PC1 indican gsatos e inversión en recuperación de suelos y aguas subterráneas, inversión en software y sistemas de seguridad informática, que la empresa no sea parte de un holding y que tenga mujeres en su directorio y gerencias.",
      "Valores positivos en PC2 implican que los trabajadores no están capacitados en informática ni se publican indicadores de rendimiento. Además, la empresa mantiene pocos empleados y, por ende, pocos gastos.",
      "Valores positivos en PC3 implican que la empresa mantiene multitud de indicadores, entre ellos: indicador de huella de agua, medidas de eficiencia energética, gestión de residuos y códigos de ética.",
      "Valores positivos en PC4 indican que la empresa no establece alianzas con otras empresas, estas sean para comprar tecnología insumos o materia prima, contratación de asesorías, capacitaciones o desarrollo de nuevos productos."
    )
  })
  
  output$cluster_plot <- renderPlotly({
    p <- augmented |>
      ggplot(aes(!!input$var1, !!input$var2, color = .cluster)) +
      geom_point(alpha = .5, size = 2) +
      labs(color = "Cluster", 
           title = "Clusters por componentes principales")
    
    ggplotly(p)
  })
  
  
  output$first_pc <- renderTable({
    augmented |> 
      select(!!input$var1) |> 
      summarise(`Mínimo` = min(!!input$var1),
                Q1 = quantile(!!input$var1, prob = .25),
                Q2 = median(!!input$var1),
                Q3 = quantile(!!input$var1, prob = .75),
                `Máximo` = max(!!input$var1),
                Media = mean(!!input$var1),
                `Desv. Est.` = sd(!!input$var1))
  })

  
  output$second_pc <- renderTable({
    augmented |> 
      select(!!input$var2) |> 
      summarise(`Mínimo` = min(!!input$var2),
                Q1 = quantile(!!input$var2, prob = .25),
                Q2 = median(!!input$var2),
                Q3 = quantile(!!input$var2, prob = .75),
                `Máximo` = max(!!input$var2),
                Media = mean(!!input$var2),
                `Desv. Est.` = sd(!!input$var2))
  })
}

shinyApp(ui, server)