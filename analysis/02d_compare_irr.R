# Notes -------------------------------------------------------------------

# - Distribution of IRR
# - Correlation between IRR
# - Confounding factors for IRR
# - Leave one rater out sensitivity analysis

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear workspace (better to restart the session)

set.seed(2020)

source(here::here("analysis", "00_init.R"))

estimate_names <- c("icc_agg10", "icc_agg15", "icc_agg20", "icc", "ka")

res_files <- here("results", paste0(estimate_names, "_uni.rds"))

# Process data --------------------------------------------------------------------

df0 <- load_imagespec()

# Prevalence
prev <- load_masks() %>%
  filter(Skin == 1) %>%
  pivot_longer(cols = starts_with("rater"), names_to = "Rater", values_to = "Eczema") %>%
  group_by(filename, Rater) %>%
  summarise(Prevalence = mean(Eczema)) %>%
  ungroup()

# Images where the prevalence is too high so we cannot estimate IRR
# Exclude images where the prevalence is more than 95% of at least 50% of raters (we cannot estimate IRR as we need some disagreement)
excl <- prev %>%
  group_by(filename) %>%
  summarise(HighPrevalence = mean(Prevalence > 0.95),
            LowPrevalence = mean(Prevalence < .05)) %>%
  filter(HighPrevalence > .5 | LowPrevalence > .5) %>%
  pull(filename)

# Process IRR -------------------------------------------------------------

res <- lapply(1:length(estimate_names),
              function(i) {
                readRDS(res_files[i]) %>%
                  select(-Model) %>%
                  rename_with(~estimate_names[i], any_of(c("ICC", "KA"))) %>%
                  pivot_longer(!filename, names_to = "Metric", values_to = "Value")
              }) %>%
  bind_rows()

res <- res %>% filter(!(filename %in% excl))

# Combine datasets
df <- full_join(df0,
                res %>%
                  pivot_wider(names_from = "Metric", values_from = "Value"),
                by = "filename") %>%
  drop_na()

# Distribution of IRR metrics ----------------------------------------------------------------

tmp <- res %>%
  pivot_wider(names_from = "Metric", values_from = "Value") %>%
  select(-filename)
cor.test(tmp[["icc"]], tmp[["ka"]], method = "pearson")
# Pair plot
tmp %>%
  rename(`ICC(d=10)` = icc_agg10,
         `ICC(d=15)` = icc_agg15,
         `ICC(d=20)` = icc_agg20,
         `ICC(pixel)` = icc,
         `KA(pixel)` = ka) %>%
  GGally::ggpairs(upper = list(continuous = GGally::wrap("cor", method = "pearson"))) +
  theme_bw(base_size = 15)
# ggsave(here("results", "irr_pairs.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 2)

# Distribution of metrics
res %>%
  ggplot(aes(x = Value, colour = Metric)) +
  geom_density() +
  scale_colour_viridis_d() +
  theme_bw(base_size = 15)

# Summary statistics
summary_stats <- res %>%
  mutate(Metric = recode(Metric,
                         icc = "ICC(pixel)",
                         ka = "KA(pixel)",
                         icc_agg10 = "ICC(d=10)",
                         icc_agg15 = "ICC(d=15)",
                         icc_agg20 = "ICC(d=20)")) %>%
  group_by(Metric) %>%
  summarise(Mean = mean(Value),
            SD = sd(Value),
            SE = SD / sqrt(n()),
            Poor = mean(Value < 0.5),
            Excellent = mean(Value > 0.9),
            Meh = 1 - Poor - Excellent)
summary_stats

# Mean estimate
summary_stats %>%
  ggplot(aes(x = Metric, y = Mean, ymin = Mean - SE, ymax = Mean + SE)) +
  geom_pointrange() +
  coord_flip(ylim = c(0, 1)) +
  labs(x = "", y = "Average IRR across images") +
  theme_bw(base_size = 15)
# ggsave(here("results", "irr_mean.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 1.5)

# Confounding ---------------------------------------------------------

# Normalise some variables
df_fit <- df %>%
  mutate(Region = factor(Region),
         Region = fct_relevel(Region, "Legs"),
         `Avg Labelling Time (min)` = MeanLabellingTime / 60,
         `Normalised severity` = SASSAD / 10) %>%
  rename(`Avg Quality` = MeanQuality)

dep_var <- "`Normalised severity` + Region + `Avg Quality` + `Avg Labelling Time (min)`"

# Effect of confounders for different IRR metrics
mfit <- lapply(estimate_names,
               function(x) {
                 f <- as.formula(paste0(x, " ~ ", dep_var))
                 fit <- glm(f, data = df_fit, family = "gaussian")
                 broom::tidy(fit) %>%
                   mutate(Outcome = x)
               }) %>%
  bind_rows()
p_conf <- mfit %>%
  filter(term != "(Intercept)") %>%
  mutate(term = gsub("Region", "Region: ", term),
         term = gsub("`", "", term),
         Outcome = recode(Outcome,
                          icc = "ICC(pixel)",
                          ka = "KA(pixel)",
                          icc_agg10 = "ICC(d=10)",
                          icc_agg15 = "ICC(d=15)",
                          icc_agg20 = "ICC(d=20)")) %>%
  ggplot(aes(x = term, y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error, colour = Outcome)) +
  geom_pointrange(position = position_dodge(width = .5)) +
  coord_flip(ylim = c(-1, 1)) +
  scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  labs(x = "", colour = "") +
  theme_bw(base_size = 15)
p_conf
# ggsave(here("results", "irr_confounders.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 1.5)

# Sensitivity analysis ----------------------------------------------------
# pixel-wise ICC

sens <- here("results", "icc_sensitivity_uni.rds") %>%
  readRDS() %>%
  rename(Value = "ICC")

# How image IRR are changing
sens_by_img <- sens %>%
  group_by(filename) %>%
  summarise(Mean = mean(Value),
            SD = sd(Value),
            Delta = max(Value) - min(Value))
sens_by_img %>%
  pull(Delta) %>%
  boxplot()

# How the average IRR is changing
sens_by_raterout <- sens %>%
  filter(!(filename %in% excl)) %>%
  group_by(RaterOut) %>%
  summarise(Mean = mean(Value),
            SD = sd(Value),
            SE = SD / sqrt(n()),
            Poor = mean(Value < 0.5),
            Excellent = mean(Value > 0.9),
            Meh = 1 - Poor - Excellent) %>%
  ungroup() %>%
  mutate(RaterOut = gsub("_", " ", RaterOut),
         Label = paste0("Leave ", RaterOut, " out"))
p_sens <- summary_stats %>%
  filter(Metric == "ICC(pixel)") %>%
  mutate(Label = "All raters") %>%
  bind_rows(sens_by_raterout) %>%
  mutate(Label = factor(Label),
         Label = factor(Label, levels = rev(levels(Label)))) %>%
  ggplot(aes(x = Label, y = Mean, ymin = Mean - SE, ymax = Mean + SE)) +
  geom_pointrange(position = position_dodge(width = .5)) +
  coord_flip(ylim = c(0, 1)) +
  labs(x = "", y = "Average ICC(pixel) across images") +
  theme_bw(base_size = 15)
p_sens
# ggsave(here("results", "icc_sensitivity.jpg"), width = 10, height = 5, units = "cm", dpi = 300, scale = 2.5)

# Combine confounding and sensitivity plots -------------------------------

cowplot::plot_grid(p_conf +
                     theme(legend.position = "top") +
                     guides(color = guide_legend(nrow = 2, byrow = TRUE)),
                   p_sens,
                   nrow = 1,
                   rel_widths = c(.52, .48),
                   align = "h",
                   axis = "tb",
                   labels = "AUTO")
# ggsave(here("results", "irr_check.jpg"), width = 11, height = 5, units = "cm", dpi = 300, scale = 2.7, bg = "white")
