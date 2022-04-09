library(tidyverse)
theme_set(theme_minimal())
theme_update(
  text = element_text(size = 16, family = "Bahnschrift"),
  plot.title = element_text(family = "Century Gothic"),
  plot.caption = element_text(family = "PT Sans Caption")
)

ele <- read_csv("Data/BBDD-ELE5-Formato-Texto.csv")

na_df <- map_dbl(ele, function(x) sum(is.na(x))) |> 
  enframe() |> 
  mutate(pct = value / nrow(ele))

segment_data <- tribble(
  ~x, ~xend, ~y, ~yend,
  .25, .35, 200, 230
)

(
  p1 <- na_df |> 
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
)

ggsave(filename = "Plots/missing_cols_pct.png",
       plot = p1, device = ragg::agg_tiff, 
       dpi = 300, width = 7)



systemfonts::system_fonts() |> 
  filter(str_detect(family, "Caption")) |> 
  pull(family)

systemfonts::font_info("Bookman Old Style")
