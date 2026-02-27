# Build the state-year panel used by all models/figures.

suppressPackageStartupMessages({
  library(dplyr)
  library(fs)
})

source(file.path("R", "helpers.R"))

build_panel <- function(
  years = 2015:2022,
  download = TRUE,
  raw_dir = file.path("data", "raw"),
  cache_dir = file.path("data", "cache")
) {
  dir_create(raw_dir, recurse = TRUE)
  dir_create(cache_dir, recurse = TRUE)

  fars_state_year <- load_fars_state_year(years, download, raw_dir, cache_dir)
  ev_state_year   <- load_ev_state_year(download, raw_dir, cache_dir, years = years)

  # VMT: prefer VM-2 .xls if present, otherwise fall back to a CSV in data/raw/.
  vm2_xls <- dir_ls(raw_dir, regexp = "\\.xls$", recurse = FALSE)

  vmt_state_year <- if (length(vm2_xls) > 0) {
    load_vmt_from_vm2_xls(raw_dir, years)
  } else {
    # Default to the included portfolio dataset
    vmt_path <- file.path(raw_dir, "vmt_state_year_2015_2022_from_vm2.csv")
    load_vmt_state_year_csv(vmt_path)
  }

  panel_raw <- fars_state_year |>
    inner_join(vmt_state_year, by = c("state", "year")) |>
    left_join(ev_state_year, by = c("state", "year")) |>
    mutate(ev_registrations = if_else(is.na(ev_registrations), 0, ev_registrations))

  stopifnot(all(panel_raw$year %in% years))
  stopifnot(all(panel_raw$vmt > 0))
  stopifnot(all(panel_raw$fatalities >= 0))

  panel_raw |>
    mutate(
      fatal_rate = (fatalities / vmt) * 1e8,  # fatalities per 100M VMT
      log_vmt    = log(vmt),
      # Transparent proxy: EV regs per 10,000 VMT
      ev_pen     = (ev_registrations / vmt) * 1e4,
      state_f    = factor(state),
      year_f     = factor(year)
    )
}
