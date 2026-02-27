# Estimate the main fixed-effect models and export tables.

suppressPackageStartupMessages({
  library(fixest)
  library(modelsummary)
  library(fs)
  library(dplyr)
})

estimate_models <- function(panel, output_dir = "outputs") {
  dir_create(file.path(output_dir, "tables"), recurse = TRUE)

  m1 <- feols(fatal_rate ~ ev_pen, data = panel, cluster = ~state)
  m2 <- feols(fatal_rate ~ ev_pen | year_f, data = panel, cluster = ~state)
  m3 <- feols(fatal_rate ~ ev_pen | state_f + year_f, data = panel, cluster = ~state)
  m4 <- feols(fatal_rate ~ ev_pen + log_vmt | state_f + year_f, data = panel, cluster = ~state)

  models <- list(
    "Pooled OLS"                  = m1,
    "Year FE"                     = m2,
    "State + Year FE"             = m3,
    "State + Year FE + log(VMT)"  = m4
  )

  tbl_path <- file.path(output_dir, "tables", "regression_table.html")
  modelsummary(
    models,
    output   = tbl_path,
    stars    = c("*" = .10, "**" = .05, "***" = .01),
    coef_map = c(
      "ev_pen"  = "EV penetration (EV regs per 10,000 VMT)",
      "log_vmt" = "log(VMT)"
    ),
    gof_omit = "IC|RMSE|Within|Pseudo|FE|Std.Errors",
    title    = "EV Penetration and Traffic Fatality Rates (fatalities per 100M VMT)"
  )

  # Optional robustness: Poisson with exposure offset
  m_pois <- fepois(fatalities ~ ev_pen | state_f + year_f,
                   data = panel, offset = ~ log(vmt), cluster = ~state)
  pois_path <- file.path(output_dir, "tables", "poisson_table.html")
  modelsummary(
    list("Poisson (offset log(VMT))" = m_pois),
    output   = pois_path,
    coef_map = c("ev_pen" = "EV penetration (EV regs per 10,000 VMT)"),
    stars    = c("*" = .10, "**" = .05, "***" = .01),
    title    = "Count model robustness: Fatalities with exposure offset"
  )

  list(models = models, poisson = m_pois, paths = list(reg_table = tbl_path, pois_table = pois_path))
}
