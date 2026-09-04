## Use this script to run exploratory code maybe before to put it into
## the pipeline


# setup -----------------------------------------------------------
library(targets)
library(here)
library(ggplot2)
# load all your custom functions
tar_source()


# Code here below -------------------------------------------------
# use `tar_read(target_name)` to load a target anywhere (note that
# `target_name` is NOT quoted!)

outbreaks <- proximity_assessment |>
  dplyr::select(tidyselect::matches("risk|concern")) |>
  colnames()

a <- proximity_assessment |>
  dplyr::mutate(
    adj = compute_adjustment_score_from_form(long, proximity_assessment)) |>
  dplyr::select(ISO_A3, adj)


set.seed(123)  # for reproducibility
data_long <- tibble::tibble(
  Outbreak = outbreaks,
  `Not concerned at all` = runif(6),
  `Somewhat unconcerned` = runif(6),
  `Somewhat concerned` = runif(6),
  `Very concerned` = runif(6),
  `Not relevant for this condition` = runif(6)
  ) |>
  tidyr::pivot_longer(
    cols = -Outbreak,
    names_to = "Concern Level",
    values_to = "Value"
  )



## Presentazioni

tar_visnetwork()

tar_read(corplot)
tar_read(fig_risk_bars)
tar_read(fig_lolliplot3)
tar_read(fig_lolliplot5)

tar_read(shiny_explorer)



tar_load(long)

disease_plots <- plot_all_diseases_from_long(long)
question_plots <- plot_all_questions_from_long(long)

save_plots_jpeg(disease_plots)
save_plots_jpeg(question_plots)

long |>
  filter(response == "Very concerned") |>
  select(disease) |>
  unique()

sample <- tibble::tibble(
  disease = c(
    "Acute Abdominal Pain",
    "Coma",
    "Eye emergency",
    "Focal neurological symptoms suggestive of Stroke",
    "Head injury or Polytrauma",
    "Lower Back Pain suggestive of renal stones",
    "Mental Health Crisis",
    "Non-traumatic Chest Pain",
    "Open Wounds with Hemorrage",
    "Resting dyspnea",
    "Second degree burns",
    "Snakebite",
    "Sudden Febrile illness in malaria-free area",
    "Sudden Febrile illness in malaria-prone area"
  ),
  `diagnosis delay` = rep("1h diagnosis delay", 14),
  `transport delay` = rep("4h transport delay", 14)
)

# Calcola e visualizza compute_adjustment_score() su 20 soglie
thresholds <- seq(0, 1, length.out = 20)

# Calcola lo score per ogni soglia
results <- tibble::tibble(
  threshold = thresholds,
  score = purrr::map_dbl(thresholds, ~ compute_adjustment_score(long, sample, threshold = .x))
)

# Visualizza lo scatterplot
ggplot2::ggplot(results, ggplot2::aes(x = threshold, y = score)) +
  ggplot2::geom_line(color = "blue") +
  ggplot2::labs(
    title = "Adjustment Score vs Threshold",
    x = "Threshold",
    y = "Adjustment Score"
  ) +
  ggplot2::theme_minimal()


