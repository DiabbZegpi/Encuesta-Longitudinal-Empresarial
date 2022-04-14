library(tidyverse)
library(tidymodels)

# Configuración de preferencias
tidymodels_prefer()
theme_set(theme_minimal())
theme_update(
  text = element_text(size = 16, family = "Bahnschrift"),
  plot.title = element_text(family = "Century Gothic"),
  plot.caption = element_text(family = "PT Sans Caption")
)

# Análisis de datos faltantes
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

# ggsave(filename = "Plots/missing_cols_pct.png",
       # plot = p1, device = ragg::agg_tiff, 
       # dpi = 300, width = 7)

systemfonts::system_fonts() |> 
  filter(str_detect(family, "Caption")) |> 
  pull(family)

systemfonts::font_info("Bookman Old Style")


# Limpiar NAs
vars_to_select <- na_df |> 
  filter(pct < .25) |> 
  pull(name)

ele_clean <- ele |>
  filter(rol_ficticio != "500895") |> # outlier extremo
  select(all_of(vars_to_select)) |> 
  select(-all_of(2:8)) |> 
  drop_na() 

# ---------------------------------------------------------------------------
# Machine learning
# ---------------------------------------------------------------------------

# Partición train-test
set.seed(123)
splits <- initial_split(ele_clean, prop = 3/4)
train <- training(splits)
test <- testing(splits)

# Preprocesamiento
preprocessor <- recipe(~ ., data = train) |> 
  update_role(rol_ficticio, new_role = "id") |> 
  step_zv(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_corr(all_numeric_predictors(), threshold = .8) |> 
  step_pca(all_numeric_predictors(), num_comp = 100)

train_baked <- preprocessor |> prep() |> bake(new_data = NULL)
test_baked <- preprocessor |> prep() |> bake(new_data = test)

# Clustering
kclusts <- tibble(k = 1:9) |> 
  mutate(kclust = map(k, ~ kmeans(select(train_baked, -rol_ficticio), .x)),
         glanced = map(kclust, glance, new_data = test_baked |> select(-rol_ficticio)))

kclusts |> 
  unnest(glanced) |> 
  ggplot(aes(k, tot.withinss)) +
  geom_line(alpha = .5, size = 1.2, color = "#65dfa5") +
  geom_point(size = 4, fill = "#65dfa5", color = "#ffffff", shape = 21, stroke = 1.5) +
  geom_curve(aes(x = 3, xend = 3.5, y = 732335, yend = 7.5e5), 
             curvature = -.25) +
  geom_point(data = kclusts |> unnest(glanced) |> filter(k == 3),
             size = 4, fill = "#658fa5", color = "#ffffff", shape = 21) +
  annotate(geom = "text", x = 3.6, y = 7.55e5, size = 5, family = "Bahnschrift",
           label = "Clasificar las empresas en\n3 segmentos parece razonable",
           hjust = 0) +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9)) +
  labs(y = "Suma de cuadrados intra cluster", 
       title = "Método del codo\nconjunto de testing")

# Varianza explicada por PCA
# pca_prep <- preprocessor |> prep()
# sdev <- pca_prep$steps[[4]]$res$sdev
# percent_variation <- sdev ^ 2 / sum(sdev ^ 2)
# tibble(variation = percent_variation) |> 
#   arrange(desc(variation)) |> 
#   mutate(cumsum = cumsum(variation))

tidy_pca <- preprocessor |> 
  prep() |> 
  tidy(4)


library(tidytext)
top_pca <- tidy_pca |> 
  filter(component %in% paste0("PC", 1:4)) |> 
  group_by(component) |> 
  slice_max(order_by = abs(value), n = 8) |> 
  ungroup() |> 
  mutate(terms = reorder_within(terms, abs(value), component)) 
  

top_pca |> 
  ggplot(aes(abs(value), terms, fill = value < 0)) +
  geom_col(alpha = .8) +
  scale_y_reordered() +
  scale_fill_manual(values = c("#65dfa5", "#EB290E"),
                    labels = c("positiva", "negativa")) +
  facet_wrap(~component, scales = "free_y") +
  labs(x = "Valor absoluto del componente",
       y = "Pregunta de la encuesta", 
       title = "Las 8 preguntas que más contribuyeron\nen los primeros 4 componentes principales",
       fill = "contribución") 


# Final fit
ele_baked <- preprocessor |> 
  prep() |> 
  bake(new_data = ele_clean)


final_kclust <- kmeans(ele_baked |> select(-rol_ficticio), centers = 3)
augmented <- augment(final_kclust, data = ele_baked) |> 
  left_join(ele |> select(rol_ficticio, Glosa_CIIU), by = c("rol_ficticio")) |> 
  select(all_of(c(1:5, 102:103)))

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

augmented |> 
  mutate(across(starts_with("PC"), scale_pc)) |>
  ggplot(aes(PC003, PC004, color = .cluster)) +
  geom_point(alpha = .5, size = 2) +
  labs(color = "Cluster", 
       title = "Clusters por componentes principales")

augmented |> slice_max(abs(PC004), n = 10)

# Save files for shiny
saveRDS(na_df, "Data/01_na_df.rds")
saveRDS(kclusts, "Data/02_kclusts.rds")
saveRDS(top_pca, "Data/03_top_pca.rds")
saveRDS(augmented, "Data/04_augmented.rds")






