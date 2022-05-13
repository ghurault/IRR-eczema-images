# Attach packages ---------------------------------------------------------

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ggplot2)
library(forcats)

library(rptR)
library(here)

lapply(list.files(here("R"), pattern = ".R$", full.names = TRUE), source)
