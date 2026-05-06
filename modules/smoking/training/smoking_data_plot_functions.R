###############################################################################
# Smoking Analysis Plotting Functions
# Kaplan-Meier curves and ECDF plots using grid graphics
###############################################################################

library(dplyr)
library(survival)
library(grid)

# ========================================
# KAPLAN-MEIER PLOT
# ========================================
plot_km_survival <- function(fit,
                             gender_labels = c("Male", "Female"),
                             colors = c("darkslateblue", "firebrick"),
                             subplot_label = "",
                             xlim = c(0, 85),
                             x_breaks = seq(0, 80, by = 20)) {
  surv_summary <- summary(fit)
  strata_levels <- levels(factor(surv_summary$strata))
  n_strata <- length(strata_levels)
  ylim <- c(0, 1)

  # ── Extract number at risk at each x_break ───────────────────────────────────
  risk_sum <- summary(fit, times = x_breaks, extend = TRUE)
  n_risk_mat <- matrix(NA_real_, nrow = n_strata, ncol = length(x_breaks))
  for (i in seq_len(n_strata)) {
    s_mask <- as.character(risk_sum$strata) == strata_levels[i]
    for (j in seq_along(x_breaks)) {
      cell <- s_mask & (risk_sum$time == x_breaks[j])
      if (any(cell)) n_risk_mat[i, j] <- risk_sum$n.risk[which(cell)[1]]
    }
  }

  # ── Main plot viewport (shifted up to leave room for risk table) ─────────────
  vp <- viewport(
    x      = 0.15, y      = 0.32,
    width  = 0.75, height = 0.60,
    xscale = xlim, yscale = ylim,
    just   = c("left", "bottom")
  )
  pushViewport(vp)

  # Axes
  grid.lines(
    x  = unit(c(xlim[1], xlim[2]), "native"),
    y  = unit(c(0, 0), "native"),
    gp = gpar(lwd = 2)
  )
  grid.lines(
    x  = unit(c(xlim[1], xlim[1]), "native"),
    y  = unit(c(0, 1.05), "native"),
    gp = gpar(lwd = 2)
  )

  axis_gp <- gpar(fontsize = 20)

  for (x in x_breaks) {
    grid.lines(
      x  = unit(x, "native"),
      y  = unit(c(0, -0.02), "npc"),
      gp = gpar(lwd = 1.5)
    )
    grid.text(
      label = as.character(x),
      x     = unit(x, "native"),
      y     = unit(-0.05, "npc"),
      gp    = axis_gp
    )
  }

  y_at <- seq(0, 1, by = 0.2)
  for (y in y_at) {
    grid.lines(
      x  = unit(c(-0.02, 0), "npc"),
      y  = unit(y, "native"),
      gp = gpar(lwd = 1.5)
    )
    grid.text(
      label = sprintf("%.0f", y * 100),
      x     = unit(-0.03, "npc"),
      y     = unit(y, "native"),
      just  = "right",
      gp    = axis_gp
    )
  }

  # KM step curves + CI bands
  for (i in 1:n_strata) {
    stratum_name <- strata_levels[i]
    idx <- (surv_summary$strata == stratum_name) & (surv_summary$time <= xlim[2])

    times <- c(0, surv_summary$time[idx])
    surv <- c(1, surv_summary$surv[idx])
    upper <- c(1, surv_summary$upper[idx])
    lower <- c(1, surv_summary$lower[idx])

    surv[is.na(surv)] <- 0
    upper[is.na(upper)] <- 0
    lower[is.na(lower)] <- 0

    n <- length(times)
    x_poly_pos <- x_poly_neg <- y_poly_pos <- y_poly_neg <- c()

    for (j in 1:(n - 1)) {
      grid.lines(
        x  = unit(c(times[j], times[j + 1]), "native"),
        y  = unit(c(surv[j], surv[j]), "native"),
        gp = gpar(col = colors[i], lwd = 2.5)
      )
      x_poly_pos <- c(x_poly_pos, times[j], times[j + 1])
      x_poly_neg <- c(times[j + 1], times[j], x_poly_neg)
      y_poly_pos <- c(y_poly_pos, lower[j], lower[j])
      y_poly_neg <- c(upper[j], upper[j], y_poly_neg)

      if (j < n) {
        grid.lines(
          x  = unit(c(times[j + 1], times[j + 1]), "native"),
          y  = unit(c(surv[j], surv[j + 1]), "native"),
          gp = gpar(col = colors[i], lwd = 2.5)
        )
        x_poly_pos <- c(x_poly_pos, times[j + 1], times[j + 1])
        x_poly_neg <- c(times[j + 1], times[j + 1], x_poly_neg)
        y_poly_pos <- c(y_poly_pos, lower[j], lower[j + 1])
        y_poly_neg <- c(upper[j + 1], upper[j], y_poly_neg)
      }
    }

    if (times[n] <= xlim[2]) {
      grid.lines(
        x  = unit(c(times[n], xlim[2]), "native"),
        y  = unit(c(surv[n], surv[n]), "native"),
        gp = gpar(col = colors[i], lwd = 2.5)
      )
      x_poly_pos <- c(x_poly_pos, times[n], xlim[2])
      x_poly_neg <- c(xlim[2], times[n], x_poly_neg)
      y_poly_pos <- c(y_poly_pos, lower[n], lower[n])
      y_poly_neg <- c(upper[n], upper[n], y_poly_neg)
    }

    grid.polygon(
      x  = unit(c(x_poly_pos, x_poly_neg), "native"),
      y  = unit(c(y_poly_pos, y_poly_neg), "native"),
      gp = gpar(fill = colors[i], col = NA, alpha = 0.2)
    )
  }

  # Axis labels
  grid.text(
    label = "Age (years)",
    x     = unit(0.5, "npc"),
    y     = unit(-0.10, "npc"),
    gp    = gpar(fontsize = 22)
  )

  if (subplot_label == "a)") {
    grid.text(
      label = "Probability (%)",
      x     = unit(-0.15, "npc"),
      y     = unit(0.5, "npc"),
      rot   = 90,
      gp    = gpar(fontsize = 22)
    )
  }

  if (subplot_label != "") {
    grid.text(
      label = subplot_label,
      x     = unit(-0.1, "npc"),
      y     = unit(1.1, "npc"),
      just  = c("left", "top"),
      gp    = gpar(fontsize = 26, fontface = "bold")
    )
  }

  if (subplot_label == "b)") {
    legend_x <- 0.75
    legend_y <- 0.95
    for (i in 1:n_strata) {
      y_offset <- -0.03 - 2 * (i - 1.5) * 0.03
      grid.lines(
        x  = unit(c(legend_x - 0.10, legend_x - 0.05), "npc"),
        y  = unit(rep(legend_y + y_offset, 2), "npc"),
        gp = gpar(col = colors[i], lwd = 3)
      )
      grid.text(
        label = gender_labels[i],
        x     = unit(legend_x - 0.03, "npc"),
        y     = unit(legend_y + y_offset, "npc"),
        just  = "left",
        gp    = gpar(fontsize = 18)
      )
    }
  }

  popViewport()

  # ── Number at risk table ─────────────────────────────────────────────────────
  # Column x-positions (page NPC) aligned with x_breaks on the plot.
  col_x <- 0.15 + 0.75 * (x_breaks - xlim[1]) / (xlim[2] - xlim[1])

  # Row y-positions (page NPC).
  header_y <- 0.21
  row_y <- c(0.14, 0.07) # [1] Male, [2] Female

  # "Number at risk" bold header, left-aligned
  grid.text(
    label = "Number at risk",
    x     = unit(0.01, "npc"),
    y     = unit(header_y, "npc"),
    just  = c("left", "center"),
    gp    = gpar(fontsize = 20, fontface = "bold")
  )

  risk_gp <- gpar(fontsize = 20)

  for (i in seq_len(n_strata)) {
    # Row label: left-aligned with the header, black
    grid.text(
      label = gender_labels[i],
      x     = unit(0.01, "npc"),
      y     = unit(row_y[i], "npc"),
      just  = c("left", "center"),
      gp    = gpar(fontsize = 20)
    )

    # Numbers at each time point, centre-aligned under their column tick
    for (j in seq_along(x_breaks)) {
      val <- n_risk_mat[i, j]
      label <- if (!is.na(val)) formatC(as.integer(val), format = "d", big.mark = "\u2009") else "NA"
      grid.text(
        label = label,
        x     = unit(col_x[j], "npc"),
        y     = unit(row_y[i], "npc"),
        just  = c("center", "center"),
        gp    = risk_gp
      )
    }
  }
}

# ========================================
# ECDF PLOT
# ========================================
plot_ecdf_intensity <- function(data,
                                colors = c("darkslateblue", "firebrick"),
                                gender_labels = c("Male", "Female"),
                                xlim = c(0, 60),
                                x_breaks = seq(0, 60, by = 10)) {
  ylim <- c(0, 1)

  vp <- viewport(
    x = 0.15, y = 0.12,
    width = 0.75, height = 0.78,
    xscale = xlim, yscale = ylim,
    just = c("left", "bottom")
  )
  pushViewport(vp)

  grid.lines(
    x = unit(c(xlim[1], xlim[2] + 2), "native"),
    y = unit(c(0, 0), "native"),
    gp = gpar(lwd = 2)
  )
  grid.lines(
    x = unit(c(xlim[1], xlim[1]), "native"),
    y = unit(c(0, 1.05), "native"),
    gp = gpar(lwd = 2)
  )

  axis_gp <- gpar(fontsize = 20)

  for (x in x_breaks) {
    grid.lines(
      x = unit(x, "native"),
      y = unit(c(0, -0.02), "npc"),
      gp = gpar(lwd = 1.5)
    )
    grid.text(
      label = as.character(x / 20),
      x = unit(x, "native"),
      y = unit(-0.05, "npc"),
      gp = axis_gp
    )
  }

  y_at <- seq(0, 1, by = 0.2)
  for (y in y_at) {
    grid.lines(
      x = unit(c(-0.01, 0), "npc"),
      y = unit(y, "native"),
      gp = gpar(lwd = 1.5)
    )
    grid.text(
      label = sprintf("%.0f", y * 100),
      x = unit(-0.02, "npc"),
      y = unit(y, "native"),
      just = "right",
      gp = axis_gp
    )
  }

  grid.text(
    label = "Smoking Intensity (packs)",
    x = unit(0.5, "npc"),
    y = unit(-0.12, "npc"),
    gp = gpar(fontsize = 22)
  )

  grid.text(
    label = "Cumulative Probability (%)",
    x = unit(-0.10, "npc"),
    y = unit(0.5, "npc"),
    rot = 90,
    gp = gpar(fontsize = 22)
  )

  unique_genders <- unique(data$gender)

  for (i in seq_along(unique_genders)) {
    gender_name <- unique_genders[i]
    gender_data <- data %>%
      filter(gender == gender_name) %>%
      arrange(cigarettes_per_day)

    n <- nrow(gender_data)
    for (j in 1:(n - 1)) {
      grid.lines(
        x = unit(c(
          gender_data$cigarettes_per_day[j],
          gender_data$cigarettes_per_day[j + 1]
        ), "native"),
        y = unit(c(
          gender_data$cumulative_prob[j],
          gender_data$cumulative_prob[j]
        ), "native"),
        gp = gpar(col = colors[i], lwd = 2.5)
      )

      if (j < n - 1) {
        grid.lines(
          x = unit(c(
            gender_data$cigarettes_per_day[j + 1],
            gender_data$cigarettes_per_day[j + 1]
          ), "native"),
          y = unit(c(
            gender_data$cumulative_prob[j],
            gender_data$cumulative_prob[j + 1]
          ), "native"),
          gp = gpar(col = colors[i], lwd = 2.5)
        )
      }
    }
  }

  legend_x <- 0.95
  legend_y <- 0.95

  for (i in seq_along(unique_genders)) {
    y_offset <- 0.03 - i * 0.05

    grid.lines(
      x = unit(c(legend_x - 0.10, legend_x - 0.05), "npc"),
      y = unit(rep(legend_y + y_offset, 2), "npc"),
      gp = gpar(col = colors[i], lwd = 3)
    )

    grid.text(
      label = gender_labels[i],
      x = unit(legend_x - 0.03, "npc"),
      y = unit(legend_y + y_offset, "npc"),
      just = "left",
      gp = gpar(fontsize = 20)
    )
  }

  popViewport()
}

# ========================================
# UTILITY FUNCTION: COMPUTE ECDF
# ========================================
compute_ecdf <- function(data, gender_val, gender_label) {
  intensity_values <- data %>%
    filter(intensity > 0, gender == gender_val) %>%
    distinct(id, intensity) %>%
    pull(intensity)

  ecdf_func <- ecdf(intensity_values)
  cig_levels <- seq(0, 60, by = 1)
  cum_probs <- ecdf_func(cig_levels)

  data.frame(
    gender = gender_label,
    gender_code = gender_val,
    cigarettes_per_day = cig_levels,
    cumulative_prob = cum_probs
  )
}

###############################################################################
# END OF SCRIPT
###############################################################################
