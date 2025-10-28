library(tidyverse)
library(ggrepel)
library(nanoparquet)

draws_dfs <- list()

for (draws_file in list.files("data_processed/word_draws/", "*.parquet", full.names = TRUE)) {
  draws_dfs[[draws_file]] <- read_parquet(draws_file)
}

word_draws <- bind_rows(draws_dfs)

word_scores_wide <- 
  word_draws |> 
  pivot_wider(names_from = association, values_from = value, id_cols = c(word, wordtype, draw)) 

# correlation
correlations <- 
  word_scores_wide |> 
  group_by(draw, wordtype) |> 
  summarize(
    word = first(word), 
    betrouwbaar_slecht = cor(betrouwbaar, slecht),
    betrouwbaar_slim = cor(betrouwbaar, slim),
    betrouwbaar_vrouwelijk = cor(betrouwbaar, vrouwelijk),
    slecht_slim = cor(slecht, slim),
    slecht_vrouwelijk = cor(slecht, vrouwelijk),
    slim_vrouwelijk = cor(slim, vrouwelijk),
  ) |> 
  pivot_longer(-draw:-word) |> 
  mutate(
    var1 = str_extract(name, "(\\w+)_", group = 1),
    var2 = str_extract(name, "_(\\w+)", group = 1),
  ) 

mean_correlations <- correlations |> group_by(var1, var2, wordtype) |> summarize(value = mean(value)) |> ungroup()


correlations |>
  ggplot(aes(x = value, fill = wordtype)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(data = mean_correlations, aes(xintercept = value), alpha = 0.6) +
  geom_label_repel(
    data = mean_correlations, 
    y = 10.5, 
    aes(label = round(value, 2)),  
    label.padding = 0.1, 
    label.r = 0,
    min.segment.length = 100, 
    force = 0.1
  ) +
  facet_grid(rows = vars(var1), cols = vars(var2)) +
  theme_linedraw() +
  xlim(-1, 1) +
  ylim(0, 11) +
  labs(
    title = "Item-level correlation among semantic dimensions",
    subtitle = "Distributions based on 10000 posterior samples",
    x = "Pearson correlation",
    y = ""
  ) +
  scale_fill_manual(values = c("#B0CCD8", "#C9D7B2", "#CFBCB9"))

ggsave("figures/word_correlations.png", dpi = 600, width = 10, height = 7)


# table
correlations |> 
  group_by(wordtype, var1, var2) |> 
  summarize(
    correlation = mean(value), 
    q5 = quantile(value, 0.05),
    q95 = quantile(value, 0.95)
  ) |> 
  arrange(var1, var2)
