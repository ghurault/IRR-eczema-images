# Notes -------------------------------------------------------------------

# Examples images with different IRR (e.g. poor, moderate, excellent) and from different site
# - very bad irr, legs: '08030/wk12.jpg'
# - average irr, hands: "05039/wk04.jpg"
# - excellent irr, feet: "08045/wk16.jpg"

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear workspace (better to restart the session)

set.seed(2020)

source(here::here("analysis", "00_init.R"))
library(cowplot)

df0 <- load_masks()
img <- unique(df0[["filename"]])

ex_img <- c("08030/wk12.jpg", "05039/wk04.jpg", "08045/wk16.jpg")

# Plot segmentation of different raters -----------------------------------

# Give ICC and KA in figure caption
# Consensus segmentation to illustrate clinician performance

pl <- lapply(ex_img,
             function(x) {
               df0 %>%
                 filter(filename == x,
                        Skin == 1) %>%
                 mutate(Consensus = rowMeans(across(all_of(paste0("rater_", 1:3)))),
                        Consensus = as.numeric(Consensus > 0.5)) %>%
                 pivot_longer(cols = starts_with(c("rater_", "Consensus")), names_to = "Rater", values_to = "Segmentation") %>%
                 mutate(Rater = gsub("rater_", "Rater " , Rater),
                        Rater = factor(Rater),
                        Rater = fct_relevel(Rater, "Consensus", after = Inf),
                        Rater = fct_recode(Rater, "Consensus by 1-2-3" = "Consensus"),
                        Segmentation = factor(Segmentation, levels = c(0, 1), labels = c("Skin", "Eczema"))) %>%
                 ggplot(aes(x = width_coor, y = height_coor, fill = Segmentation)) +
                 facet_grid(cols = vars(Rater)) +
                 geom_tile() +
                 scale_fill_manual(values = c("#999999", "#E69F00")) +
                 labs(x = "", y = "", fill = "") +
                 theme_classic(base_size = 15) +
                 theme(legend.position = "top",
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       axis.line = element_blank())
             })

legend <- get_legend(pl[[1]])

lapply(pl, function(x) {x + theme(legend.position = "none")}) %>%
  plot_grid(plotlist = ., ncol = 1) %>%
  plot_grid(legend, ., ncol = 1, rel_heights = c(.1, .9))
if (FALSE) {
  ggsave(here("results", "example_segmentation.jpg"),
         width = 13, height = 8, units = "cm",
         dpi = 300, scale = 2,
         bg = "white")
}

# Compare masks for pixel-wise and area-wise IRR --------------------------------------------

# NB: we use the masks of rater_1

d_val <- c(10, 15, 20)

# Pixel-wise segmentation
pl1 <- lapply(ex_img,
              function(x) {
                df0 %>%
                  filter(filename == x,
                         Skin == 1) %>%
                  mutate(across(starts_with("rater_"), ~factor(.x, levels = c(0, 1), labels = c("Skin", "Eczema")))) %>%
                  ggplot(aes(x =  width_coor, y = height_coor, fill = rater_1)) +
                  geom_tile() +
                  # scale_fill_viridis_d() +
                  scale_fill_manual(values = c("#999999", "#E69F00")) +
                  labs(x = "", y = "", fill = "") +
                  theme_classic() +
                  theme(legend.position = "none",
                        axis.text = element_blank(),
                        axis.ticks = element_blank(),
                        axis.line = element_blank())
              })


title1 <- ggdraw() +
  draw_label("Pixel-wise",
             fontface = 'bold',
             x = 0.4,
             hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

p1 <- plot_grid(title1,
                plot_grid(plotlist = pl1, ncol = 1),
                ncol = 1,
                rel_heights = c(.1, .9))

# Area-wise segmentation for different d
pl2 <- lapply(d_val,
              function(d) {
                df_agg <- load_masks(d = d)
                lapply(ex_img,
                       function(x) {
                         df_agg %>%
                           filter(filename == x,
                                  Skin > 0) %>%
                           mutate(across(starts_with("rater_"), ~.x / N)) %>%
                           ggplot(aes(x =  BigPixel_x, y = BigPixel_y, fill = rater_1)) +
                           geom_tile() +
                           scale_fill_gradient(low = "#999999", high = "#E69F00") +
                           # scale_fill_viridis_c() +
                           labs(x = "", y = "", fill = "Proportion of eczema") +
                           theme_classic() +
                           theme_classic() +
                           theme(legend.position = "none",
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 axis.line = element_blank())
                       })
              })

legend <- get_legend(pl2[[1]][[1]] + theme(legend.position = "top"))

p2 <- lapply(1:length(d_val),
             function(i) {
               
               title2 <- ggdraw() +
                 draw_label(paste0("d = ", d_val[i]),
                            fontface = 'bold',
                            x = 0.45,
                            hjust = 0) +
                 theme(plot.margin = margin(0, 0, 0, 7))
               
               plot_grid(title2,
                         plot_grid(plotlist = pl2[[i]], ncol = 1),
                         ncol = 1,
                         rel_heights = c(.1, .9))
               
             }) %>%
  plot_grid(plotlist = ., nrow = 1)

plot_grid(
  legend,
  plot_grid(p1, p2, nrow = 1, rel_widths = c(1, 3)),
  ncol = 1,
  rel_heights = c(.1, .9)
)

if (FALSE) {
  ggsave(here("results", "illustration_compression.jpg"),
         width = 13, height = 8, units = "cm",
         dpi = 400, scale = 2,
         bg = "white")
}
