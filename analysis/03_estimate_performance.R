# Notes -------------------------------------------------------------------

# 

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear workspace (better to restart the session)

set.seed(2020)

source(here::here("analysis", "00_init.R"))

df <- load_masks() %>%
  filter(Skin == 1) # Remove background to make results more generalisable to other images

img <- unique(df[["filename"]])

n_rater <- 4

# Clinician and naive performance ---------------------------------------------------------------------

perf <- lapply(1:n_rater,
               function(k) {
                 
                 tested_rater <- paste0("rater_", k)
                 consensus_raters <- paste0("rater_", setdiff(1:n_rater, k))
                 
                 tmp <- df %>%
                   mutate(Consensus = apply(df[, consensus_raters], 1, function(x) {as.numeric(mean(x) > 0.5)})) %>%
                   rename(Rater = all_of(tested_rater))
                 
                 # Performance of clinician for each image
                 perf_clinician <- tmp %>%
                   group_by(filename) %>%
                   summarise(TP = sum(Consensus == 1 & Rater == 1),
                             TN = sum(Consensus == 0 & Rater == 0),
                             FP = sum(Consensus == 0 & Rater == 1),
                             FN = sum(Consensus == 1 & Rater == 0))
                 perf_clinician <- compute_metrics(perf_clinician)
                 
                 # Performance when predicting all skin to be eczema for each image
                 perf_naive <- tmp %>%
                   group_by(filename) %>%
                   summarise(TP = sum(Consensus == 1 & Skin == 1),
                             TN = sum(Consensus == 0 & Skin == 0),
                             FP = sum(Consensus == 0 & Skin == 1),
                             FN = sum(Consensus == 1 & Skin == 0))
                 perf_naive <- compute_metrics(perf_naive)
                 
                 bind_rows(perf_clinician  %>% mutate(Type = "Clinician"),
                           perf_naive  %>% mutate(Type = "Naive")) %>%
                   mutate(Rater = k)
                 
               }) %>%
  bind_rows()

# Average performance across raters
avg_perf <- perf %>%
  pivot_longer(cols = !all_of(c("Type", "filename", "Rater")), names_to = "Metric", values_to = "Value") %>%
  group_by(Type, filename, Metric) %>%
  summarise(Mean_Rater = mean(Value)) %>%
  ungroup()

# Difference in performance for each image between clinician and naive
diff_perf <- avg_perf %>%
  pivot_wider(names_from = "Type", values_from = "Mean_Rater") %>%
  mutate(Difference = Clinician - Naive)

# Plot difference in performance ------------------------------------------

# Distribution of difference between clinician and naive
diff_perf %>%
  filter(!(Metric %in% c("TP", "TN", "FP", "FN"))) %>%
  ggplot(aes(x = Difference)) +
  facet_grid(rows = vars(Metric)) +
  geom_histogram(binwidth = 0.1) + # geom_density()
  coord_cartesian(xlim = c(-1, 1)) +
  labs(x = "Difference between clinician and naive performance") +
  theme_bw(base_size = 15)

# Mean and SE of difference between clinician and naive
diff_perf %>%
  group_by(Metric) %>%
  summarise(Mean = mean(Difference), SD = sd(Difference), SE = SD / sqrt(n())) %>%
  filter(!(Metric %in% c("TP", "TN", "FP", "FN"))) %>%
  ggplot(aes(x = Metric, y = Mean, ymin = Mean - SE, ymax = Mean + SE)) +
  geom_pointrange() +
  coord_flip(ylim = c(-0.5, 0.5)) +
  labs(y = "Average difference between\nclinician and naive performance", x = "") +
  theme_bw(base_size = 15)

# Plot clinician and naive performance ------------------------------------

# Average performance across images
avg_perf %>%
  group_by(Type, Metric) %>%
  summarise(Mean_Image = mean(Mean_Rater), SD_Image = sd(Mean_Rater), SE_Image = SD_Image / sqrt(n())) %>%
  ungroup() %>%
  filter(!(Metric %in% c("TP", "TN", "FP", "FN"))) %>%
  mutate(Type = factor(Type, levels = c("Naive", "Clinician"), labels = c("Naive rater", "Average rater"))) %>%
  ggplot(aes(x = Type, y = Mean_Image, ymin = Mean_Image - SE_Image, ymax = Mean_Image + SE_Image)) +
  facet_grid(rows = vars(Metric)) +
  geom_pointrange() +
  coord_flip(ylim = c(0.5, 1)) +
  labs(x = "", y = "") +
  theme_bw(base_size = 15) +
  theme(strip.text.y = element_text(angle = 0))
# ggsave(here("results", "baseline_performance.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 2)

if (FALSE) {
  # Colour instead of facet
  avg_perf %>%
    group_by(Type, Metric) %>%
    summarise(Mean_Image = mean(Mean_Rater), SD_Image = sd(Mean_Rater), SE_Image = SD_Image / sqrt(n())) %>%
    ungroup() %>%
    filter(!(Metric %in% c("TP", "TN", "FP", "FN"))) %>%
    mutate(Type = replace(Type, Type == "Naive", "All Eczema")) %>%
    ggplot(aes(x = Metric, y = Mean_Image, ymin = Mean_Image - SE_Image, ymax = Mean_Image + SE_Image, colour = Type)) +
    geom_pointrange(position = position_dodge(width = 0.4)) +
    coord_flip(ylim = c(0.5, 1)) +
    scale_colour_manual(values = c("#000000", "#E69F00")) +
    labs(x = "", y = "", colour = "") +
    theme_bw(base_size = 15) +
    theme(legend.position = "top")
}
