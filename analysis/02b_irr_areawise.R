# Notes -------------------------------------------------------------------

# Area-wise ICC
# - Further "compress" images (aggregate neighbouring pixels) to investigate of IRR of the "main" areas of eczema.
# NB: the original images are already compressed
# - ICC for each image as Proportion data (with mixed effect on Rater)
# - Exclude background pixels

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear workspace (better to restart the session)

set.seed(2020)

source(here::here("analysis", "00_init.R"))

#### OPTIONS
d <- 10 # dimension of compressed image
run <- FALSE
####

# Results files
icc_file <- here("results", paste0("icc_agg", d, "_uni.rds"))

# Data processing --------------------------------------------------------

# Aggregate pixels for area-wise IRR
df <- load_masks(d = d)

img <- unique(df[["filename"]])

# Remove pixels full of background and format dataset
df <- df %>%
  filter(Skin > 0) %>%
  select(-BigPixel_x, -BigPixel_y) %>%
  pivot_longer(cols = starts_with("rater"), names_to = "Rater", values_to = "Eczema") %>%
  mutate(NotEczema = pmax(Skin - Eczema, 0)) %>%
  select(-N, -Skin)

# ICC for each image -----------------------------

if (run) {
  # Should take < 3' for d=10
  rpt_agg_uni <- lapply(1:length(img),
                        function(i) {
                          rptProportion(cbind(Eczema, NotEczema) ~ (1 | Rater) + (1 | ID),
                                        grname = c("ID"),
                                        data = df %>% filter(filename == img[i]),
                                        link = "logit",
                                        nboot = 0, npermut = 0)
                        })
  
  icc_agg_uni <- tibble(filename = img,
                        Model = rpt_agg_uni) %>%
    mutate(ICC = map(Model, ~.x$R["R_link", ]) %>% unlist())
  saveRDS(icc_agg_uni, file = icc_file)
  
} else {
  icc_agg_uni <- readRDS(icc_file)
}
