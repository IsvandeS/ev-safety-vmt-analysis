# One-command entrypoint to reproduce the analysis.
# Run:
#   Rscript scripts/run_all.R
#
# Tip: If you want fully reproducible package versions, use renv:
#   install.packages("renv")
#   renv::init()
#   renv::snapshot()

suppressPackageStartupMessages({
  library(dplyr)
  library(fs)
})

source(file.path("R", "build_panel.R"))
source(file.path("R", "models.R"))
source(file.path("R", "figures.R"))

YEARS <- 2016:2022
DOWNLOAD <- TRUE

dir_create("outputs/figures", recurse = TRUE)
dir_create("outputs/tables", recurse = TRUE)

panel <- build_panel(years = YEARS, download = DOWNLOAD)

res <- estimate_models(panel, output_dir = "outputs")
save_figures(panel, res$models, output_dir = "outputs")

message("Done. Outputs written to ./outputs (tables + figures).")
