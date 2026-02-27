# - Downloads and loads NHTSA FARS accident data (fatalities)
# - Downloads and loads AFDC/NREL EV registration spreadsheet
# - Loads VMT from either (a) FHWA VM-2 .xls files or (b) a simple CSV

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(readxl)
  library(janitor)
  library(fs)
  library(glue)
})

download_fars_year <- function(year, cache_dir) {
  url <- glue("https://static.nhtsa.gov/nhtsa/downloads/FARS/{year}/National/FARS{year}NationalCSV.zip")
  zip_path <- file.path(cache_dir, glue("FARS{year}NationalCSV.zip"))

  if (!file_exists(zip_path)) {
    message("Downloading FARS ", year, " ...")
    download.file(url, destfile = zip_path, mode = "wb", quiet = TRUE)
  }

  zlist <- unzip(zip_path, list = TRUE)
  accident_name <- zlist$Name[str_detect(tolower(zlist$Name), "accident\\.csv$")][1]
  if (is.na(accident_name)) stop("accident.csv not found inside: ", zip_path)

  acc <- read_csv(unz(zip_path, accident_name), show_col_types = FALSE) |>
    clean_names() |>
    mutate(year = year)

  need <- c("state", "fatals", "year")
  miss <- setdiff(need, names(acc))
  if (length(miss) > 0) {
    stop("FARS accident.csv is missing expected columns: ", paste(miss, collapse = ", "))
  }

  acc |> select(state, year, fatals)
}

load_fars_state_year <- function(years, download, raw_dir, cache_dir) {
  if (download) {
    fars_acc <- map_dfr(years, download_fars_year, cache_dir = cache_dir)
  } else {
    paths <- file.path(raw_dir, glue("fars_accident_{years}.csv"))
    stopifnot(all(file_exists(paths)))
    fars_acc <- map_dfr(paths, ~ read_csv(.x, show_col_types = FALSE) |> clean_names())
    if (!("year" %in% names(fars_acc))) stop("Manual FARS files must include a 'year' column.")
    fars_acc <- fars_acc |> select(state, year, fatals)
  }

  fars_acc |>
    group_by(state, year) |>
    summarise(fatalities = sum(fatals, na.rm = TRUE), .groups = "drop") |>
    mutate(
      state = c(
        `1`="Alabama",`2`="Alaska",`4`="Arizona",`5`="Arkansas",`6`="California",
        `8`="Colorado",`9`="Connecticut",`10`="Delaware",`11`="District of Columbia",
        `12`="Florida",`13`="Georgia",`15`="Hawaii",`16`="Idaho",`17`="Illinois",
        `18`="Indiana",`19`="Iowa",`20`="Kansas",`21`="Kentucky",`22`="Louisiana",
        `23`="Maine",`24`="Maryland",`25`="Massachusetts",`26`="Michigan",
        `27`="Minnesota",`28`="Mississippi",`29`="Missouri",`30`="Montana",
        `31`="Nebraska",`32`="Nevada",`33`="New Hampshire",`34`="New Jersey",
        `35`="New Mexico",`36`="New York",`37`="North Carolina",`38`="North Dakota",
        `39`="Ohio",`40`="Oklahoma",`41`="Oregon",`42`="Pennsylvania",
        `44`="Rhode Island",`45`="South Carolina",`46`="South Dakota",
        `47`="Tennessee",`48`="Texas",`49`="Utah",`50`="Vermont",
        `51`="Virginia",`53`="Washington",`54`="West Virginia",
        `55`="Wisconsin",`56`="Wyoming"
      )[as.character(state)]
    ) |>
    filter(!is.na(state))
}


load_ev_state_year <- function(download, raw_dir, cache_dir, years = 2016:2022) {
  suppressPackageStartupMessages({
    library(rvest)
    library(xml2)
    library(dplyr)
    library(stringr)
    library(readr)
    library(janitor)
    library(tidyr)
    library(purrr)
  })

  # Pull EV counts from AFDC’s yearly table:
  # https://afdc.energy.gov/vehicle-registration?year=YYYY
  fetch_year <- function(yr) {
    url <- paste0("https://afdc.energy.gov/vehicle-registration?year=", yr)
    page <- read_html(url)

    # First table on page is the state-by-fuel-type table
    tab <- page |>
      html_element("table") |>
      html_table(fill = TRUE) |>
      janitor::clean_names()

    # Expect columns like: state, electric_ev, plug_in_hybrid_electric_phev, ...
    if (!("state" %in% names(tab))) stop("Could not find 'state' column for year ", yr)

    ev_col <- names(tab)[str_detect(names(tab), "^electric")]
    if (length(ev_col) == 0) stop("Could not find EV column for year ", yr)

    tab |>
      transmute(
        state = as.character(state),
        year  = as.integer(yr),
        ev_registrations = readr::parse_number(as.character(.data[[ev_col[1]]]))
      )
  }

  purrr::map_dfr(years, fetch_year) |>
    filter(!is.na(state), !is.na(year))
}

# Generic VMT CSV loader (expects: state, year, vmt OR state_name, year, vmt)
load_vmt_state_year_csv <- function(vmt_csv_path) {
  if (!file_exists(vmt_csv_path)) stop("VMT file not found: ", vmt_csv_path)

  vmt <- read_csv(vmt_csv_path, show_col_types = FALSE) |>
    clean_names()

  # Allow either state or state_name; normalize to state
  if ("state_name" %in% names(vmt) && !"state" %in% names(vmt)) {
    vmt <- vmt |> rename(state = state_name)
  }

  if ("vmt_billions" %in% names(vmt) && !"vmt" %in% names(vmt)) {
    vmt <- vmt |> mutate(vmt = vmt_billions * 1e9)
  }

  need <- c("state", "year", "vmt")
  miss <- setdiff(need, names(vmt))
  if (length(miss) > 0) {
    stop("VMT CSV missing columns: ", paste(miss, collapse = ", "),
         "\nExpected columns: state/state_name, year, vmt (miles) or vmt_billions.")
  }

  vmt |>
    transmute(
      state = as.character(state),
      year  = as.integer(year),
      vmt   = as.numeric(vmt)
    ) |>
    filter(!is.na(state), !is.na(year), !is.na(vmt))
}

# VM-2 XLS loader (optional; kept for completeness)
load_vmt_from_vm2_xls <- function(raw_dir, years = NULL) {
  xls_files <- dir_ls(raw_dir, regexp = "\\.xls$", recurse = FALSE)
  if (length(xls_files) == 0) stop("No .xls files found in: ", raw_dir)

  get_year <- function(path) {
    m <- stringr::str_match(path_file(path), "(19|20)\\d{2}")
    as.integer(m[,1])
  }

  valid_states <- c(state.name, "District of Columbia", "Puerto Rico")

  all <- purrr::map_dfr(xls_files, function(fp) {
    yr <- get_year(fp)
    if (!is.null(years) && !(yr %in% years)) return(tibble())

    tab <- readxl::read_excel(fp, sheet = "A", col_names = FALSE)

    state_rows <- tab |>
      mutate(.row_id = row_number(),
             state_name_raw = as.character(...1),
             state_name = stringr::str_replace(state_name_raw, "\\s*\\(.*\\)\\s*$", "")) |>
      filter(!is.na(state_name), state_name %in% valid_states)

    if (nrow(state_rows) == 0) {
      stop("Could not find any state rows in VM-2 file: ", fp,
           "\nTip: open it and confirm sheet 'A' contains the state table.")
    }

    last_numeric <- function(x) {
      nums <- suppressWarnings(as.numeric(x))
      nums <- nums[!is.na(nums)]
      if (length(nums) == 0) return(NA_real_)
      tail(nums, 1)
    }

    totals_millions <- state_rows |>
      rowwise() |>
      mutate(vmt_millions = last_numeric(c_across(everything()))) |>
      ungroup() |>
      select(state_name, vmt_millions)

    totals_millions |>
      filter(!is.na(vmt_millions)) |>
      transmute(
        state = state_name,
        year  = yr,
        vmt   = vmt_millions * 1e6
      )
  })

  all |>
    filter(state != "Puerto Rico") |>
    distinct(state, year, .keep_all = TRUE)
}
