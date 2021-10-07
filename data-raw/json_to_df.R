# Notes -------------------------------------------------------------------

# - Extract data from original JSON file
# - Check whether the labelling order of different experts are correlated.
# If yes, potential to use labelling order as a proxy for average experience.
# In practice, no.

# Initialisation ----------------------------------------------------------

library(tidyverse)
library(rjson)
library(corrplot)

dir_json <- file.path("D:",
                      "Box Sync",
                      "AD ML Project",
                      "SWET DATA - Hywel Williams",
                      "Images",
                      "Labelbox",
                      "LabelBox Exports")
files <- list.files(dir_json, pattern = "^Expert.*\\.json", full.names = TRUE)

# Extract JSON ------------------------------------------------------------

df0 <- lapply(files,
              function(f) {
                
                js <- fromJSON(file = f)
                
                df <- lapply(js,
                             function(x) {
                               
                               cl <- x$Label$classifications
                               eval <- NA
                               bad <- NA
                               for (i in seq_along(cl)) {
                                 if (cl[[i]]$title == "Evaluability (Quality)") {
                                   eval <- cl[[i]]$answer$title
                                 }
                                 # if (cl[[i]]$title == "Bad Image (?)") {
                                 #   bad <- lapply(cl[[i]]$answers, function(ans) {ans$title})
                                 # }
                               }
                               
                               out <- tibble(filename = x[["External ID"]],
                                             Expert = x[["Dataset Name"]], 
                                             StartDate = x[["Created At"]],
                                             LabellingTime = x[["Seconds to Label"]],
                                             Evaluability = eval,
                                             BadImage = bad)
                               
                               return(out)
                             }) %>%
                  bind_rows()
                
                return(df)
                
              }) %>%
  bind_rows()

# Processing --------------------------------------------------------------

df <- df0 %>%
  filter(Expert != "SWET_Test") %>%
  mutate(StartDate = as.POSIXct(gsub("T", " ", StartDate))) %>%
  group_by(Expert) %>%
  mutate(LabellingOrder = order(StartDate)) %>%
  ungroup()

common_set <- df %>%
  group_by(filename) %>%
  summarise(N = n()) %>%
  filter(N == 4) %>%
  pull(filename)

df <- df %>%
  filter(filename %in% common_set)

# Check correlation of labelling order ------------------------------------

df %>%
  select(filename, Expert, LabellingOrder) %>%
  pivot_wider(names_from = "Expert", values_from = "LabellingOrder") %>%
  select(-filename) %>%
  cor(method = "spearman") %>%
  corrplot.mixed()
