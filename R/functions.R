# Data processing ---------------------------------------------------------

load_masks <- function(file = here("data/inter-rater_masks-as-ratings(10%).csv"), d = NULL) {
  # Load and clean masks
  # Optionally rescale image by aggregating pixels in a d*d grid
  #
  # Args:
  # file: path to data file
  # d: (optional) dimension of the compressed image
  #
  # Returns
  # Dataframe
  
  library(dplyr)
  
  out <- readr::read_csv(file) %>%
    rename(ID = X1, Skin = background) %>%
    mutate(across(all_of(c("Skin", paste0("rater_", 1:4))), ~as.numeric(.x / 255 > 0.5)))
  
  if (!is.null(d)) {
    stopifnot(HuraultMisc::is_scalar_wholenumber(d),
              d > 0)
    
    out <- out %>%
      group_by(filename) %>%
      mutate(BigPixel_x = cut(width_coor, d, labels = FALSE),
             BigPixel_y = cut(height_coor, d, labels = FALSE),
             ID = paste0(BigPixel_x, "x_", BigPixel_y, "y_", filename)) %>%
      group_by(ID, filename, BigPixel_x, BigPixel_y) %>%
      summarise(across(all_of(c(paste0("rater_", 1:4), "Skin")), sum), N = n()) %>%
      ungroup()
    
  }
  
  return(out)
  
}

load_metasegmentation <- function(file = here("data/interrator_statistics_extended.csv")) {
  # Load segmentation metadata (for each image and each expert)
  # - Quality score
  # - Labelling time
  # - Reasons for bad images
  #
  # Args:
  # file: Path to the data file
  #
  # Returns:
  # Dataframe with columns filename, Expert, Quality, LabellingTime, OutOfFocus, Overexposed, BadImage
  
  library(dplyr)
  library(tidyr)
  
  df0 <- readr::read_csv(file)
  
  eval0 <- df0 %>%
    select(filename, ends_with("_Evaluability")) %>%
    pivot_longer(!filename, names_to = "Expert", values_to = "Quality") %>%
    mutate(Expert = gsub("_Evaluability", "", Expert),
           Quality = recode(Quality,
                            `Low (Difficult)` = -1,
                            `Normal (Fine)` = 0,
                            `High (Great)` = 1))
  
  time0 <- df0 %>%
    select(filename, ends_with("_Time")) %>%
    pivot_longer(!filename, names_to = "Expert", values_to = "LabellingTime") %>%
    mutate(Expert = gsub("_Time", "", Expert))
  
  bad <- df0 %>%
    select(filename, ends_with("Bad-Images")) %>%
    pivot_longer(!filename, names_to = "Expert", values_to = "Reason") %>%
    mutate(Expert = gsub("_Bad-Images", "", Expert),
           Reason = replace(Reason, is.na(Reason), ""), 
           Reason = recode(Reason, "[]" = ""),
           OutOfFocus = str_detect(Reason, "Out-of-focus"),
           Overexposed = str_detect(Reason, "Overexposed"),
           OtherReasons = str_detect(Reason, "Other reasons"),
           BadImage = (OutOfFocus | Overexposed | OtherReasons)) %>%
    select(-Reason)
  
  out <- Reduce(function(x, y) {full_join(x, y, by = c("filename", "Expert"))},
                list(eval0, time0, bad))
  
  return(out)
  
}

load_imagespec <- function(file = here("data/interrator_statistics_extended.csv")) {
  # Load image specifications
  # - Average quality score (across raters)
  # - Average labelling time (across raters)
  # - Any reason for bad quality
  # - Severity
  # - Region (representative site)
  #
  # Args:
  # file: Path to the datafile
  #
  # Returns:
  # Dataframe
  
  library(dplyr)
  
  tmp <- load_metasegmentation(file) %>%
    group_by(filename) %>%
    summarise(MeanQuality = mean(Quality),
              MeanLabellingTime = mean(LabellingTime),
              across(all_of(c("OutOfFocus", "Overexposed", "OtherReasons", "BadImage")), any)) %>%
    ungroup()
  
  sev <- readr::read_csv(file) %>%
    select(filename, repsite, starts_with("sassad_"), starts_with("tiss_")) %>%
    rename(SASSAD = sassad_tot,
           TISS = tiss_tot,
           Region = repsite)
  
  out <- full_join(tmp, sev, by = "filename")
  
  return(out)
  
}

# Other functions ---------------------------------------------------------------

compute_metrics <- function(df) {
  # Compute metrics from TP, TN, FP and FN
  #
  # Args:
  # df: Dataframe with columns TP, TN, FP, FN
  #
  # Returns:
  # Dataframe
  
  library(dplyr)
  
  stopifnot(is.data.frame(df),
            all(c("TP", "FP", "TN", "FN") %in% colnames(df)))
  
  df %>%
    mutate(Accuracy = (TP + TN) / (TP + FP + TN + FN),
           IoU = TP / (TP + FP + FN),
           TPR = TP / (TP + FN),
           PPV = TP / (TP + FP),
           F1 = 2 * TPR * PPV / (TPR + PPV))
}
