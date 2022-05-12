# Notes -------------------------------------------------------------------

# 

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear workspace (better to restart the session)

set.seed(2020)

source(here::here("analysis", "00_init.R"))

# Processing --------------------------------------------------------------

ext <- load_masks() %>%
  filter(Skin == 1) %>%
  group_by(filename) %>%
  summarise(across(starts_with("rater_"), ~((sum(.x) + 1) / (length(.x) + 2)))) %>% # avoid extent of exactly 0 or 1
  pivot_longer(!filename, names_to = "Rater", values_to = "Extent") %>%
  mutate(Rater = gsub("rater_", "", Rater),
         LogitExtent = HuraultMisc::logit(Extent))

# Exploration -------------------------------------------------------------

# SD across raters
sd_extent <- ext %>%
  group_by(filename) %>%
  summarise(SD = sd(Extent))

mean(sd_extent$SD)

sd_extent %>%
  pull(SD) %>%
  hist()

# IRR ---------------------------------------------------------------------

# ICC on logit scale for better comparison with pixel-wise and area-wise ICC
rpt_extent <- rptGaussian(LogitExtent ~ (1 | Rater) + (1 | filename),
                          grname = "filename",
                          data = ext,
                          nboot = 1000,
                          npermut = 0)

# IRR of the mean ---------------------------------------------------------

s <- broom.mixed::tidy(rpt_extent$mod) %>%
  filter(effect != "fixed")
vg <- s$estimate[s$group == "filename"]^2
vr <- s$estimate[s$group == "Residual"]^2
vc <- s$estimate[s$group == "Rater"]^2

n <- 1:20
icc <- vg / (vg + (vr + vc) / n)
plot(n, icc, ylim = c(0, 1), pch = 16, xlab = "Number of raters", ylab = "Reliability of average extent")
abline(c(.9, 0), col = "red")

trgt <- 0.9
ceiling((vr + vc) / vg * trgt / (1 - trgt)) # Number of raters required to reach target reliability

# Clinician performance for predicting extent -----------------------------

rmse <- lapply(1:4, function(i) {
  cons <- ext %>%
    filter(Rater != i) %>%
    group_by(filename) %>%
    summarise(Consensus = mean(Extent))
  
  rat <- ext %>%
    filter(Rater == i) %>%
    select(-Rater)
  
  full_join(cons, rat, by = "filename") %>%
    mutate(Error = Extent - Consensus) %>%
    summarise(RMSE = sqrt(mean(Error^2))) %>%
    mutate(TestedRater = i)
}) %>%
  bind_rows() %>%
  summarise(Mean = mean(RMSE), SD = sd(RMSE), SE = SD / sqrt(n()))
