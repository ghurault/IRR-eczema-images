# Inter-rater reliability analysis of eczema segmentation from digital images

This repository contains the code developed for the paper by [**Hurault et al. (2021), "Detecting eczema areas in digital images: an impossible task?"**](#) (submitted to publication).

The code is written in the R language for statistical computing.

## File structure

This project is organised as a research compendium, with a similar structure as R packages:

- Functions/helpers are located in the [`R/`](R/) directory
- Analysis scripts are located in the [`analysis/`](analysis/) directory
- [`renv/`](renv/) and `renv.lock` are files created by the renv package to manage package dependencies (see details below).

### Analysis files

- [`01_analyse_quality.R`](analysis/01_analyse_quality.R) contains the code to investigate the quality score given by the raters.
- [`02a_irr_pixelwise.R`](analysis/02a_irr_pixelwise.R), [`02b_irr_areawise.R`](analysis/02b_irr_areawise.R) and [`02c_irr_extent.R`](analysis/02c_irr_extent.R) contain the code to compute the IRR at the pixel-level, area-level and the IRR of extent, respectively.
[`02d_compare_irr.R`](analysis/02d_compare_irr.R) contains the code to compare the IRR metrics.
- [`03_estimate_performance.R`](analysis/03_estimate_performance.R) contains the code to compute the average rater and the naive segmentation performance.
- [`04_plot_segmentation.R`](analysis/04_plot_segmentation.R) contains the code to visualise the segmentation and reproduce Figures 1 and S1.

## Reproducibility

This project is organised to facilitate the reproducibility of the analysis.
To reproduce the analysis, it is recommended to first open the project (`.Rproj` file) in [RStudio IDE](https://www.rstudio.com/products/rstudio/).
The project uses [renv](https://rstudio.github.io/renv/index.html) to manage package dependencies.
To install the packages and their dependencies required to reproduce analysis, first install renv with `install.packages("renv")`, and then call `renv::restore()`.

The analysis scripts are located in the [`analysis/`](analysis/) directory.
It is recommended to run the scripts in the order indicated by their prefix, although some scripts are independent from the others (e.g. [`01_analyse_quality.R`](analysis/01_analyse_quality.R) and [`03_estimate_performance.R`](analysis/03_estimate_performance.R)).

The data should be located in the `data/` directory (it will be made available soon).
Intermediate and output files are saved to a `results/` directory.

## License

This open source version of this project is licensed under the GPLv3 license, which can be seen in the [LICENSE](LICENSE.md) file.
