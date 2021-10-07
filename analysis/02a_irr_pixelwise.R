# Notes -------------------------------------------------------------------

# Pixel-wise IRR
# - For each image, ICC as binary data (mixed effect on Rater)
# - we cannot compute joint ICC for computational reasons
# - Use already compressed images (for speed)
# - Exclude background pixels
# - Sensitivity analysis (leave one rater out) for ICC
# (some iteration may fail because of prevalence=1, not a problem as these images are excluded for the results interpretation)

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear workspace (better to restart the session)

set.seed(2020)

source(here::here("analysis", "00_init.R"))
library(foreach)
library(doParallel)

#### OPTIONS
metric <- "icc"
sensitivity_analysis <- TRUE
run <- FALSE
n_cluster <- 40 # Number of clusters for parallel work # parallel::detectCores() - 2
####

metric <- match.arg(metric, c("icc", "ka"))
stopifnot(is_scalar_logical(sensitivity_analysis),
          is_scalar_logical(run))

res_file <- here("results", paste0(metric, ifelse(sensitivity_analysis, "_sensitivity", ""), "_uni", ".rds"))

# Data processing --------------------------------------------------------

df <- load_masks() %>%
  filter(Skin == 1)

img <- unique(df[["filename"]])
rt <- colnames(df)[grepl("^rater_\\d+$", colnames(df))]

loop_spec <- tibble(filename = img)
if (sensitivity_analysis) {
  loop_spec <- expand_grid(loop_spec, RaterOut = rt)
}

# ICC for each image -----------------------------------------------------

if (metric %in% c("icc") && run) {
  
  dir_uni <- ifelse(!sensitivity_analysis, "rpt_uni", "rpt_uni_sensitivity") %>%
    here("results", .)
  dir.create(dir_uni)
  
  duration <- Sys.time()
  cl <- makeCluster(n_cluster, outfile = "")
  registerDoParallel(cl)
  
  out <- foreach(i = 1:nrow(loop_spec)) %dopar% {
    
    source(here::here("analysis", "00_init.R"))
    
    cat(paste("Starting iteration", i, "\n"))
    
    df_uni <- df %>%
      filter(filename == loop_spec$filename[i])
    if (sensitivity_analysis) {
      df_uni <- df_uni %>% select(!all_of(loop_spec$RaterOut[i]))
    }
    df_uni <- df_uni %>%
      pivot_longer(cols = starts_with("rater"), names_to = "Rater", values_to = "Eczema")
    
    icc <- rptBinary(Eczema ~ (1 | ID) + (1 | Rater),
                     grname = "ID",
                     data = df_uni,
                     link = "logit",
                     nboot = 0, npermut = 0)
    
    saveRDS(icc, file = here(dir_uni, paste0("rpt_", i, ".rds")))
    
    cat(paste("Ending iteration", i, "\n"))
    
    # Return
    NULL
  }
  stopCluster(cl)
  (duration = Sys.time() - duration)
  
  # Combine results
  icc_uni <- lapply(1:nrow(loop_spec),
                    function(i) {
                      cbind(loop_spec[i, ],
                            tibble(Model = here(dir_uni, paste0("rpt_", i, ".rds"))))
                    }) %>%
    bind_rows()
  if (!all(file.exists(icc_uni[["Model"]]))) {
    warning("Some results don't exist: runs may have failed.")
  }
  icc_uni <- icc_uni %>%
    mutate(ICC = map(Model, ~tryCatch({readRDS(.x)$R["R_link", ]}, error = function(e) {NA})) %>% unlist())
  
  saveRDS(icc_uni, file = res_file)
  
} else {
  icc_uni <- readRDS(res_file)
}

# Krippendorff alpha for each image  ---------------------------------------

if (metric == "ka" && run) {
  
  duration <- Sys.time()
  cl <- makeCluster(n_cluster)
  registerDoParallel(cl)
  
  ka_res <- foreach(i = 1:nrow(loop_spec), .packages = "dplyr", .combine = bind_rows) %dopar% {
    
    mat <- df %>%
      filter(filename == loop_spec$filename[i])
    if (sensitivity_analysis) {
      mat <- mat %>% select(!all_of(loop_spec$RaterOut[i]))
    }
    mat <- mat %>%
      select(starts_with("rater_")) %>%
      as.matrix() %>%
      t()
    ka <- irr::kripp.alpha(mat, method = "nominal")
    
    out <- cbind(loop_spec[i, ],
                 tibble(Model = list(ka), KA = ka$value))
    
    return(out)
    
  }
  
  stopCluster(cl)
  (duration = Sys.time() - duration)
  
  saveRDS(ka_res, file = res_file)
  
} else {
  ka_res <- readRDS(res_file)
}
