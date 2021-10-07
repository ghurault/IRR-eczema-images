# Notes -------------------------------------------------------------------

# 

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear workspace (better to restart the session)

set.seed(2020)

source(here::here("analysis", "00_init.R"))

df <- load_metasegmentation()
df_avg <- load_imagespec()

# IRR ------------------------------------------------------------

# KA
ka <- df %>%
  select(filename, Expert, Quality) %>%
  pivot_wider(names_from = "Expert", values_from = "Quality") %>%
  select(-filename) %>%
  boot::boot(.,
             statistic = function(data, indices) {
               data[indices, ] %>%
                 as.matrix() %>%
                 t() %>%
                 irr::kripp.alpha(., method = "ordinal") %>%
                 .$value
             },
             R = 1000)
# boot::boot.ci(ka)

# ICC
rpt_eval <- rptGaussian(Quality ~ (1 | Expert) + (1 | filename),
                        grname = "filename",
                        data = df,
                        nboot = 1000, npermut = 0)

# Confounding -------------------------------------------------------------

df_avg %>%
  mutate(Region = factor(Region),
         Region = fct_relevel(Region, "Legs"),
         `Normalised severity` = SASSAD / 18) %>%
  glm(MeanQuality ~ `Normalised severity` + Region, data = ., family = "gaussian") %>%
  broom::tidy(.) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = gsub("Region", "Region: ", term),
         term = gsub("`", "", term)) %>%
  ggplot(aes(x = term, y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
  geom_pointrange() +
  coord_flip(ylim = c(-1.1, 1.1)) +
  labs(x = "") +
  theme_bw(base_size = 15)
# ggsave(here("results", "quality_confounders.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 1.5)
