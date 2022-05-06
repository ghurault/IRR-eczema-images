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

p_conf <- df_avg %>%
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
p_conf
# ggsave(here("results", "quality_confounders.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 1.5)

# Distribution out-of-focus -----------------------------------------------

# reorder filename
filename_order <- full_join(
  df %>%
    group_by(filename) %>%
    summarise(N = sum(OutOfFocus)),
  df %>%
    filter(Expert == "Expert_0") %>%
    mutate(N0 = as.numeric(OutOfFocus)) %>%
    select(filename, N0),
  by = "filename"
) %>%
  mutate(filename = fct_reorder2(filename, N0, N)) %>%
  pull(filename) %>%
  levels()

p_oof<- df %>%
  mutate(filename = factor(filename, levels = filename_order),
         Expert = gsub("Expert_", "Rater ", Expert),
         OutOfFocus = case_when(!OutOfFocus ~ "No", TRUE ~ "Yes")) %>%
  ggplot(aes(x = Expert, y = filename, fill = OutOfFocus)) +
  geom_tile() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  labs(x = "", y = "Image", fill = "Is the image out-focus?") +
  theme_classic(base_size = 15) +
  theme(axis.text.y = element_blank(),
        legend.position = "top")
p_oof
# ggsave(here("results", "heatmap_outoffocus.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 2)

# Combine out-of-focus heatmap and confounding estimates ------------------

cowplot::plot_grid(p_oof, p_conf, nrow = 1, rel_widths = c(.46, .54), align = "h", axis = "tb", labels = "AUTO")
# ggsave(here("results", "plot_quality.jpg"), width = 11, height = 5, units = "cm", dpi = 300, scale = 2.5, bg = "white")
