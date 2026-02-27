# Create and save figures.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(forcats)
  library(fs)
  library(glue)
  library(fixest)
})

save_figures <- function(panel, models, output_dir = "outputs") {
  fig_dir <- file.path(output_dir, "figures")
  dir_create(fig_dir, recurse = TRUE)

  # Figure 1: FWL residual plot matching the preferred spec (state FE + year FE + log(VMT))
  res_y <- resid(feols(fatal_rate ~ log_vmt | state_f + year_f, data = panel, cluster = ~state))
  res_x <- resid(feols(ev_pen     ~ log_vmt | state_f + year_f, data = panel, cluster = ~state))
  panel_fig <- panel |>
    mutate(fatal_resid = res_y, ev_resid = res_x)

  fig1 <- ggplot(panel_fig, aes(x = ev_resid, y = fatal_resid)) +
    geom_point(alpha = 0.55, size = 1.8) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    labs(
      title    = "Figure 1: EV penetration and traffic fatalities (FWL residuals)",
      subtitle = "Residuals after partialling out state FE, year FE, and log(VMT)",
      x        = "EV penetration residual (EV regs per 10,000 VMT)",
      y        = "Fatality rate residual (fatalities per 100M VMT)",
      caption  = "Sources: NHTSA FARS (fatalities), AFDC (EV registrations), FHWA (VMT)."
    )

  # Figure 2: Trends by EV penetration quartile (quartiles defined by the last year)
  last_year <- max(panel$year, na.rm = TRUE)
  ev_q <- panel |>
    filter(year == last_year) |>
    mutate(q = ntile(ev_pen, 4)) |>
    select(state, q) |>
    mutate(q_lab = factor(q, labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)")))

  panel_trend <- panel |>
    left_join(ev_q, by = "state") |>
    group_by(year, q_lab) |>
    summarise(
      n_obs      = sum(!is.na(fatal_rate)),
      mean_fatal = mean(fatal_rate, na.rm = TRUE),
      se_fatal   = dplyr::if_else(n_obs > 1, sd(fatal_rate, na.rm = TRUE) / sqrt(n_obs), NA_real_),
      .groups    = "drop"
    )

  fig2 <- ggplot(panel_trend, aes(x = year, y = mean_fatal, group = q_lab)) +
    geom_ribbon(aes(ymin = mean_fatal - 1.96 * se_fatal,
                    ymax = mean_fatal + 1.96 * se_fatal),
                alpha = 0.12, colour = NA) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    scale_x_continuous(breaks = sort(unique(panel$year))) +
    scale_y_continuous(labels = number_format(accuracy = 0.01)) +
    facet_wrap(~ q_lab, nrow = 2) +
    labs(
      title    = "Figure 2: Fatality rate trends by EV penetration quartile",
      subtitle = glue("Quartiles defined by EV penetration in {last_year}; ribbons = 95% CI"),
      x        = "Year",
      y        = "Mean fatality rate (per 100M VMT)",
      caption  = "Sources: NHTSA FARS, AFDC, FHWA."
    ) +
    theme(legend.position = "none")

  # Figure 3: Coefficient plot across specs
  coef_ci <- function(mod, term) {
    b  <- coef(mod)[term]
    ci <- confint(mod, parm = term, level = 0.95)
    tibble(
      estimate  = unname(b),
      conf_low  = unname(ci[1, 1]),
      conf_high = unname(ci[1, 2])
    )
  }

  m1 <- models[["Pooled OLS"]]
  m2 <- models[["Year FE"]]
  m3 <- models[["State + Year FE"]]
  m4 <- models[["State + Year FE + log(VMT)"]]

  coef_df <- bind_rows(
    tibble(model = "Pooled OLS")                 |> bind_cols(coef_ci(m1, "ev_pen")),
    tibble(model = "Year FE")                    |> bind_cols(coef_ci(m2, "ev_pen")),
    tibble(model = "State + Year FE")            |> bind_cols(coef_ci(m3, "ev_pen")),
    tibble(model = "State + Year FE + log(VMT)") |> bind_cols(coef_ci(m4, "ev_pen"))
  ) |>
    mutate(model = factor(model, levels = unique(model)))

  fig3 <- ggplot(coef_df, aes(x = estimate, y = forcats::fct_rev(model))) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.2, linewidth = 0.8) +
    geom_point(size = 3.8) +
    labs(
      title    = "Figure 3: Estimated effect of EV penetration across specifications",
      subtitle = "Dependent variable: fatalities per 100M VMT; 95% CI with state-clustered SE",
      x        = "Effect of EV penetration (EV regs per 10,000 VMT)",
      y        = NULL,
      caption  = "OLS with state-clustered standard errors."
    )

  ggsave(file.path(fig_dir, "fig1_fwl.png"), fig1, width = 7.2, height = 5.4, dpi = 300, bg = "white")
  ggsave(file.path(fig_dir, "fig2_trends.png"), fig2, width = 9.6, height = 6.8, dpi = 300, bg = "white")
  ggsave(file.path(fig_dir, "fig3_coefplot.png"), fig3, width = 8.5, height = 4.4, dpi = 300, bg = "white")

  combined <- (fig1 | fig3) / fig2 +
    plot_annotation(
      title   = "EV Penetration and Traffic Safety: State-Level Evidence",
      caption = "All models use state-clustered standard errors."
    )

  ggsave(file.path(fig_dir, "ev_safety_figures_combined.png"),
         combined, width = 12.5, height = 11, dpi = 300, bg = "white")

  invisible(list(fig1 = fig1, fig2 = fig2, fig3 = fig3, combined = combined, fig_dir = fig_dir))
}
