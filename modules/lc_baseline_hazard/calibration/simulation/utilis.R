###############################################################################
# Utility Functions for Lung Cancer Baseline Hazard Calibration
###############################################################################

# ========================================
# MISCELLANEOUS
# ========================================
construct <- function(ind, df) {
  if (df$count[ind] > 0) {
    data.frame(
      year_of_birth = df$year_of_birth[ind],
      race = "0",
      gender = "0",
      year_immigrated = 0,
      index = rep(df$index[ind], df$count[ind])
    )
  } else {
    NULL
  }
}

addpopulation <- function(birthmatrix, year_stop = 2030) {
  colnames(birthmatrix) <- 0:7
  df <- as.data.frame(birthmatrix) %>%
    mutate(year_of_birth = 1991:year_stop) %>%
    pivot_longer(
      cols = c(as.character(0:7)),
      names_to = "index",
      values_to = "count"
    )

  popdf <- do.call(rbind, lapply(1:nrow(df), construct, df = df)) %>%
    rows_update(
      mutate(mapping, index = as.character(0:7)),
      by = "index",
      unmatched = "ignore"
    ) %>%
    mutate(index = as.numeric(index))

  popdf
}

clean_matrix <- function(mtx) {
  mtx <- as.matrix(mtx)

  for (i in seq_len(nrow(mtx))) {
    row_vals <- mtx[i, ]

    if (is.na(row_vals[1])) {
      row_vals[1] <- 0
    }

    for (j in 2:length(row_vals)) {
      if (is.na(row_vals[j])) {
        row_vals[j] <- row_vals[j - 1]
      }
    }

    mtx[i, ] <- row_vals
  }

  mtx
}

format_line_box <- function(text, width = 70, border = "|") {
  text <- trimws(text)
  inner_width <- width - nchar(border) * 2

  if (nchar(text) > inner_width) {
    text <- substr(text, 1, inner_width)
  }

  padding_needed <- inner_width - nchar(text)
  left_pad <- floor(padding_needed / 2)
  right_pad <- ceiling(padding_needed / 2)

  paste0(
    border,
    strrep(" ", left_pad), text, strrep(" ", right_pad),
    border
  )
}

# ========================================
# SMOKING FUNCTIONS
# ========================================
micro_smoke_prevalence <- function(populationdata,
                                   minage = 18,
                                   maxage = 74,
                                   year_stop = 2024) {
  interest_yrs <- c(1970, 1992, 1998, 2019:year_stop)
  interest_yrs <- interest_yrs[interest_yrs <= year_stop]
  prevalencetable <- matrix(0, nrow = 12, ncol = length(interest_yrs))

  age_groups <- c("18-29", "30-39", "40-49", "50-59", "60-74")

  for (year in interest_yrs) {
    peoplealive <- populationdata %>%
      filter(
        year - year_of_birth >= minage,
        year - year_of_birth <= maxage,
        year_of_death >= year | year_of_death == 0,
        year_immigrated <= year
      ) %>%
      mutate(age = year - year_of_birth) %>%
      mutate(age = cut(
        age,
        breaks = c(minage, 29, 39, 49, 59, maxage),
        include.lowest = TRUE,
        labels = age_groups
      ))

    peoplealive_with_smoking <- peoplealive %>%
      filter(
        year_start_smoking > 0,
        year_start_smoking <= year,
        (year_start_smoking > year_stop_smoking | year_stop_smoking >= year)
      )

    prevalencetable[1, which(interest_yrs == year)] <-
      nrow(peoplealive_with_smoking) / nrow(peoplealive) * 100

    prevalencetable[2:6, which(interest_yrs == year)] <-
      peoplealive_with_smoking %>%
      group_by(age) %>%
      reframe(n = n()) %>%
      complete(age = age_groups, fill = list(n = 0)) %>%
      mutate(prevalence = n / (
        peoplealive %>%
          group_by(age) %>%
          reframe(n = n()) %>%
          complete(age = age_groups, fill = list(n = 0)) %>%
          pull(n)
      ) * 100) %>%
      pull(prevalence)

    prevalencetable[7:8, which(interest_yrs == year)] <-
      peoplealive_with_smoking %>%
      group_by(gender) %>%
      reframe(n = n()) %>%
      complete(gender = unique(populationdata$gender), fill = list(n = 0)) %>%
      mutate(prevalence = n / (
        peoplealive %>%
          group_by(gender) %>%
          reframe(n = n()) %>%
          complete(gender = unique(populationdata$gender), fill = list(n = 0)) %>%
          pull(n)
      ) * 100) %>%
      pull(prevalence)

    prevalencetable[9:12, which(interest_yrs == year)] <-
      peoplealive_with_smoking %>%
      group_by(race) %>%
      reframe(n = n()) %>%
      complete(race = unique(populationdata$race), fill = list(n = 0)) %>%
      mutate(prevalence = n / (
        peoplealive %>%
          group_by(race) %>%
          reframe(n = n()) %>%
          complete(race = unique(populationdata$race), fill = list(n = 0)) %>%
          pull(n)
      ) * 100) %>%
      pull(prevalence)
  }

  colnames(prevalencetable) <- interest_yrs
  prevalencetable <- prevalencetable %>%
    as.data.frame() %>%
    tibble::add_column(
      Group = c(
        "Total", "18-29", "30-39", "40-49", "50-59", "60-74",
        "Female", "Male", "Chinese", "Indian", "Malay", "Other"
      ),
      .before = 1
    )

  prevalencetable
}

# ========================================
# CANCER BURDEN FUNCTIONS
# ========================================
format_long <- function(matrix) {
  col_map <- expand.grid(
    gender = 0:1,
    age_index = 0:10,
    screen = 0:1,
    stage = 1:4
  )
  col_map$col <- with(
    col_map,
    gender * 88 + age_index * 8 + screen * 4 + (stage - 1) + 1
  )

  years <- 1990 + 0:(nrow(matrix) - 1)
  matrix_long <- as.data.frame(matrix)
  matrix_long$year <- years

  matrix_long <- matrix_long %>%
    pivot_longer(
      -year,
      names_to = "col",
      values_to = "count"
    ) %>%
    mutate(col = as.integer(gsub("V", "", col)))

  end_year <- max(matrix_long$year, na.rm = TRUE)
  k_max <- floor((end_year - 1988) / 5)
  breaks_seq <- seq(1988, 1988 + 5 * (k_max + 1), by = 5)
  labels_seq <- sprintf("%d-%d", breaks_seq[-length(breaks_seq)], breaks_seq[-1] - 1)

  matrix_long <- matrix_long %>%
    left_join(col_map, by = "col") %>%
    mutate(
      period = cut(
        year,
        breaks = breaks_seq,
        right = FALSE,
        labels = labels_seq
      )
    )

  gender_labels <- c("male", "female")
  screen_labels <- c("not_screened", "screened")
  stage_labels <- paste0("stage", 1:4)
  age_labels <- c(
    "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80-84", "85+"
  )

  matrix_long$AgeGroup <- age_labels[matrix_long$age_index + 1]
  matrix_long$Gender <- gender_labels[matrix_long$gender + 1]
  matrix_long$Screen <- screen_labels[matrix_long$screen + 1]
  matrix_long$Stage <- stage_labels[matrix_long$stage]

  matrix_long
}

micro_cancer_burden <- function(incidencematrix, mortalitymatrix) {
  incidencematrix_long <- format_long(incidencematrix)
  mortalitymatrix_long <- format_long(mortalitymatrix)

  list(Incidence = incidencematrix_long, Mortality = mortalitymatrix_long)
}

###############################################################################
# END OF SCRIPT
###############################################################################
