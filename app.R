# HUNT PSA - interaktiv rapport
# Shiny-app med nasjonale referansetall og HUNT-resultater beregnet.


required_packages <- c("shiny", "ggplot2", "survival")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Manglende R-pakker: ", paste(missing_packages, collapse = ", "),
    ". Kjør install.packages(c('shiny', 'ggplot2', 'survival')) og start appen på nytt."
  )
}

library(shiny)
library(ggplot2)

options(stringsAsFactors = FALSE, scipen = 999)

HUNT_DESCRIPTION_START <- 2006L
HUNT_DESCRIPTION_END <- 2025L
HUNT_DESCRIPTION_LABEL <- "HUNT 2006-2025"
TREATMENT_START <- 2021L
TREATMENT_END <- 2024L
FOLLOW_UP_END <- as.Date("2025-12-31")
SURVIVAL_START <- 2006L
SURVIVAL_END <- 2020L
FIVE_YEAR_DAYS <- as.integer(round(5 * 365.25))
FIVE_YEAR_TIME <- FIVE_YEAR_DAYS / 365.25
SURVIVAL_PERIOD_LABEL <- paste0(
  SURVIVAL_START, "-", SURVIVAL_END,
  "; oppfølging til ", format(FOLLOW_UP_END, "%d.%m.%Y")
)

# -----------------------------------------------------------------------------
# Farger og formatering
# -----------------------------------------------------------------------------

COL_HUNT <- "#236B93"
COL_HUNT_DARK <- "#174A6E"
COL_HUNT_LIGHT <- "#8CC3D5"
COL_NORWAY <- "#D85C4A"
COL_ORANGE <- "#E8A33C"
COL_PURPLE <- "#8767A6"
COL_GREY <- "#9AA7AE"
COL_TEXT <- "#243640"

fmt_n <- function(x) {
  format(round(as.numeric(x)), big.mark = " ", scientific = FALSE, trim = TRUE)
}

fmt_1 <- function(x) {
  format(
    round(as.numeric(x), 1),
    nsmall = 1,
    decimal.mark = ",",
    big.mark = " ",
    scientific = FALSE,
    trim = TRUE
  )
}

fmt_2 <- function(x) {
  format(
    round(as.numeric(x), 2),
    nsmall = 2,
    decimal.mark = ",",
    big.mark = " ",
    scientific = FALSE,
    trim = TRUE
  )
}

fmt_pct1 <- function(x) paste0(fmt_1(x), " %")
fmt_pct2 <- function(x) paste0(fmt_2(x), " %")
fmt_n_blank <- function(x) ifelse(is.finite(as.numeric(x)), fmt_n(x), "")
fmt_1_blank <- function(x) ifelse(is.finite(as.numeric(x)), fmt_1(x), "")

theme_clinical <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(
        colour = COL_HUNT_DARK,
        face = "bold",
        size = base_size + 3,
        margin = margin(b = 7)
      ),
      plot.subtitle = element_text(
        colour = "#5B6E78",
        size = base_size - 1,
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        colour = "#667983",
        hjust = 0,
        size = base_size - 3,
        margin = margin(t = 8)
      ),
      axis.title = element_text(colour = COL_TEXT, face = "bold"),
      axis.text = element_text(colour = COL_TEXT),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#DDE5E9", linewidth = 0.45),
      legend.position = "top",
      legend.title = element_blank(),
      strip.text = element_text(colour = COL_HUNT_DARK, face = "bold"),
      plot.margin = margin(12, 24, 12, 12)
    )
}

# -----------------------------------------------------------------------------
# HUNT-individdata for kliniske undergrupper
# -----------------------------------------------------------------------------

safe_character <- function(x) {
  if (is.null(x)) return(character())
  if (inherits(x, c("haven_labelled", "labelled"))) {
    raw_value <- unclass(x)
    raw_value <- as.vector(raw_value)
    out <- as.character(raw_value)
    value_labels <- attr(x, "labels", exact = TRUE)
    if (!is.null(value_labels) && length(value_labels) > 0L) {
      for (label_name in names(value_labels)) {
        hit <- !is.na(raw_value) & raw_value == unname(value_labels[[label_name]])
        out[hit] <- label_name
      }
    }
    return(out)
  }
  as.character(x)
}

normalise_text <- function(x) {
  z <- safe_character(x)
  z <- iconv(z, from = "", to = "UTF-8", sub = "")
  toupper(trimws(z))
}

is_registry_missing <- function(x) {
  z <- normalise_text(x)
  is.na(z) | z == "" | z %in% c(
    "NA", "N/A", "NULL", "<NA>", ".", ".X", ".V", ".M", ".U",
    "UKJENT", "UOPPGITT"
  ) | grepl("^\\.[A-Z0-9]+$", z)
}

parse_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  z <- normalise_text(x)
  z[is_registry_missing(z)] <- NA_character_
  z <- gsub(" ", "", z, fixed = TRUE)
  z <- gsub(",", ".", z, fixed = TRUE)
  z <- sub("^[<>]=?", "", z)
  valid <- grepl("^[-+]?[0-9]+([.][0-9]+)?$", z)
  suppressWarnings(as.numeric(ifelse(valid, z, NA_character_)))
}

as_date_safe <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))
  if (is.numeric(x)) {
    raw_number <- suppressWarnings(as.numeric(x))
    raw_text <- ifelse(is.na(raw_number), NA_character_, sprintf("%.0f", raw_number))
    looks_yyyymmdd <- grepl("^[12][0-9]{7}$", raw_text)
    out <- as.Date(rep(NA_character_, length(raw_number)))
    out[looks_yyyymmdd] <- as.Date(raw_text[looks_yyyymmdd], format = "%Y%m%d")
    use_origin <- !looks_yyyymmdd & is.finite(raw_number)
    out[use_origin] <- as.Date(raw_number[use_origin], origin = "1970-01-01")
    return(out)
  }
  z <- safe_character(x)
  out <- as.Date(rep(NA_character_, length(z)))
  for (date_format in c("%Y-%m-%d", "%d.%m.%Y", "%d%b%Y", "%Y%m%d")) {
    use <- is.na(out) & !is.na(z) & trimws(z) != ""
    if (!any(use)) break
    out[use] <- suppressWarnings(as.Date(z[use], format = date_format))
  }
  out
}

find_column <- function(d, candidates = character(), patterns = character()) {
  if (is.null(d) || ncol(d) == 0L) return(NA_character_)
  lower_names <- tolower(names(d))
  for (candidate in candidates) {
    hit <- which(lower_names == tolower(candidate))
    if (length(hit) > 0L) return(names(d)[hit[1]])
  }
  for (pattern in patterns) {
    hit <- grep(pattern, names(d), ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0L) return(hit[1])
  }
  NA_character_
}

get_vector <- function(d, candidates = character(), patterns = character()) {
  column <- find_column(d, candidates, patterns)
  if (is.na(column)) return(rep(NA, nrow(d)))
  d[[column]]
}

positive_value_present <- function(x) {
  z <- normalise_text(x)
  out <- !is_registry_missing(z) & !z %in% c(
    "0", "0.0", "0,0", "NEI", "NO", "N", "INGEN", "NONE", "FALSE"
  )
  numeric_z <- suppressWarnings(as.numeric(gsub(",", ".", z, fixed = TRUE)))
  numeric_like <- !is.na(numeric_z)
  out[numeric_like] <- numeric_z[numeric_like] > 0
  out[is.na(out)] <- FALSE
  out
}

yes_flag <- function(x) {
  z <- normalise_text(x)
  out <- rep(FALSE, length(z))
  out[grepl("^(JA|YES|Y)([^A-Z]|$)", z)] <- TRUE
  numeric_z <- suppressWarnings(as.numeric(z))
  out[!is.na(numeric_z) & numeric_z == 1] <- TRUE
  out
}

merge_missing_case_columns <- function(target, source) {
  target <- as.data.frame(target, check.names = FALSE)
  source <- as.data.frame(source, check.names = FALSE)
  extra_columns <- setdiff(names(source), names(target))
  if (length(extra_columns) == 0L || nrow(source) == 0L) return(target)
  key_candidates <- c("case_id", "S_sidKrg", "sidKrg", "sid_krg")
  target_key <- find_column(target, key_candidates)
  source_key <- find_column(source, key_candidates)
  if (is.na(target_key) || is.na(source_key)) return(target)
  source_id <- normalise_text(source[[source_key]])
  target_id <- normalise_text(target[[target_key]])
  usable <- !is.na(source_id) & source_id != ""
  if (anyDuplicated(source_id[usable])) return(target)
  index <- match(target_id, source_id)
  for (column in extra_columns) target[[column]] <- source[[column]][index]
  target
}

first_case_per_person <- function(d) {
  first_flag_col <- find_column(d, c("first_prostate_case_for_person"))
  if (!is.na(first_flag_col)) {
    keep <- parse_number(d[[first_flag_col]]) == 1 | yes_flag(d[[first_flag_col]])
    keep[is.na(keep)] <- FALSE
    if (any(keep)) return(d[keep, , drop = FALSE])
  }
  pid_col <- find_column(d, c("pid_std", "PID@115391"), c("^pid"))
  if (is.na(pid_col)) stop("Fant ikke prosjekt-ID i analysefilen.")
  diagnosis_date <- as_date_safe(get_vector(
    d, c("diagnosis_date", "S_diagnosedato"), c("diagnos.*dato", "diagnos.*date")
  ))
  pid <- normalise_text(d[[pid_col]])
  order_index <- order(pid, diagnosis_date, na.last = TRUE)
  d <- d[order_index, , drop = FALSE]
  pid <- pid[order_index]
  d[!duplicated(pid), , drop = FALSE]
}

t_stage_group <- function(x) {
  z <- normalise_text(x)
  blank <- is_registry_missing(z)
  z <- gsub("[^A-Z0-9]", "", z)
  z <- sub("^RAD", "", z)
  z <- sub("^C", "", z)
  out <- rep(NA_character_, length(z))
  out[!blank & grepl("^T?[012]", z)] <- "T0-2"
  out[!blank & grepl("^T?3", z)] <- "T3"
  out[!blank & grepl("^T?4", z)] <- "T4"
  out[!blank & (grepl("^T?X", z) | z %in% c("X", "UKJENT", "UOPPGITT"))] <- "TX"
  out[!blank & is.na(out)] <- "TX"
  factor(out, levels = c("T0-2", "T3", "T4", "TX"))
}

ct_rank <- function(x) {
  z <- gsub("[^A-Z0-9]", "", normalise_text(x))
  z <- sub("^C", "", z)
  out <- rep(NA_real_, length(z))
  out[grepl("^T?0", z)] <- 0
  out[grepl("^T?1", z)] <- 10
  out[grepl("^T?2A", z)] <- 20
  out[grepl("^T?2B", z)] <- 21
  out[grepl("^T?2C", z)] <- 22
  out[grepl("^T?2$", z)] <- 21
  out[grepl("^T?3", z)] <- 30
  out[grepl("^T?4", z)] <- 40
  out
}

positive_stage <- function(x, prefix) {
  z <- gsub("[^A-Z0-9]", "", normalise_text(x))
  grepl(paste0("^(C|RAD)?", prefix, "1"), z)
}

risk_levels <- c(
  "Lav risiko", "Intermediær gunstig", "Intermediær ugunstig",
  "Høyrisiko lokalisert", "Høyrisiko lokalavansert"
)

canonical_risk_group <- function(x) {
  z <- normalise_text(x)
  compact <- gsub("[^A-Z0-9ÆØÅ]", "", z)
  out <- rep(NA_character_, length(z))
  out[grepl("LOKALAVAN|LOCALLYADV|LOCALADV", compact)] <- "Høyrisiko lokalavansert"
  out[is.na(out) & grepl("HØY|HOY|HIGH", compact)] <- "Høyrisiko lokalisert"
  out[grepl("UGUNSTIG|UNFAV", compact)] <- "Intermediær ugunstig"
  out[grepl("GUNSTIG|FAV", compact) & !grepl("UGUNSTIG|UNFAV", compact)] <- "Intermediær gunstig"
  out[is.na(out) & grepl("LAV|LOW", compact)] <- "Lav risiko"
  numeric_code <- suppressWarnings(as.integer(z))
  use_numeric <- is.na(out) & numeric_code %in% 1:5
  out[use_numeric] <- risk_levels[numeric_code[use_numeric]]
  factor(out, levels = risk_levels)
}

derive_risk_group <- function(psa, isup, ct_dre, n_stage, m_stage) {
  psa <- parse_number(psa)
  isup <- parse_number(isup)
  ct <- ct_rank(ct_dre)
  n_positive <- positive_stage(n_stage, "N")
  m_positive <- positive_stage(m_stage, "M")
  out <- rep(NA_character_, length(psa))
  complete <- is.finite(psa) & isup %in% 1:5 & is.finite(ct) & !m_positive
  out[complete & (ct >= 30 | n_positive)] <- "Høyrisiko lokalavansert"
  out[complete & is.na(out) & (ct >= 22 | psa > 20 | isup >= 4)] <- "Høyrisiko lokalisert"
  out[complete & is.na(out) & ct <= 20 & psa < 10 & isup == 1] <- "Lav risiko"
  intermediate <- complete & is.na(out) & ct <= 21 & psa <= 20 & isup <= 3
  intermediate_factors <- (psa >= 10) + (isup == 2) + (ct == 21)
  out[intermediate & isup <= 2 & intermediate_factors == 1] <- "Intermediær gunstig"
  out[intermediate & (isup == 3 | intermediate_factors >= 2)] <- "Intermediær ugunstig"
  factor(out, levels = risk_levels)
}

age_group_treatment <- function(x) {
  cut(
    x,
    breaks = c(-Inf, 69, 74, 79, Inf),
    labels = c("Under 70 år", "70-74 år", "75-79 år", "80 år eller eldre"),
    right = TRUE
  )
}

age_group_description <- function(x) {
  cut(
    x,
    breaks = c(-Inf, 54, 64, 74, 84, Inf),
    labels = c("0-54", "55-64", "65-74", "75-84", "85+"),
    right = TRUE
  )
}

age_group_survival <- function(x) {
  cut(
    x,
    breaks = c(-Inf, 64, 74, Inf),
    labels = c("Under 65 år", "65-74 år", "75 år eller eldre"),
    right = TRUE
  )
}

icd10_is_malignant_cancer <- function(x) {
  z <- gsub("[^A-Z0-9]", "", normalise_text(x))
  out <- grepl("^C([0-8][0-9]|9[0-7])", z)
  out[is.na(out)] <- FALSE
  out
}

# Aalen-Johansen-estimat ved ett fast tidspunkt. Status 0 betyr sensurering,
# mens 1-4 er gjensidig utelukkende registrerte dødsårsaker.
aj_endpoint <- function(time, status, causes = 1:4,
                        time_point = FIVE_YEAR_TIME) {
  keep <- is.finite(time) & time >= 0 & !is.na(status)
  time <- as.numeric(time[keep])
  status <- as.integer(status[keep])
  event_times <- sort(unique(time[status > 0L & time <= time_point]))
  survival_previous <- 1
  cumulative_incidence <- rep(0, length(causes))

  for (event_time in event_times) {
    at_risk <- sum(time >= event_time)
    if (at_risk <= 0L) next
    all_events <- sum(time == event_time & status > 0L)
    cause_events <- vapply(
      causes,
      function(cause) sum(time == event_time & status == cause),
      numeric(1)
    )
    cumulative_incidence <- cumulative_incidence +
      survival_previous * cause_events / at_risk
    survival_previous <- survival_previous * (1 - all_events / at_risk)
  }

  setNames(cumulative_incidence, paste0("cause", causes))
}

build_five_year_outcomes <- function(cases_derived, death_file) {
  empty_result <- function(message) {
    list(
      ok = FALSE,
      message = message,
      observed = NULL,
      at_risk = NULL,
      competing = NULL,
      n_competing = NA_integer_,
      n_competing_5y = NA_integer_
    )
  }

  if (is.null(cases_derived) || nrow(cases_derived) == 0L) {
    return(empty_result("Ingen prostatakrefttilfeller var tilgjengelige."))
  }
  if (!file.exists(death_file)) {
    return(empty_result("Fant ikke core_death_record.rds."))
  }

  death_data <- as.data.frame(readRDS(death_file), check.names = FALSE)
  death_pid_col <- find_column(death_data, c("pid_std", "LOPENR"), c("^pid"))
  death_date_col <- find_column(
    death_data, c("death_date"), c("dod.*dato", "death.*date")
  )
  death_cause_col <- find_column(
    death_data,
    c("DIAGNOSE_UNDERLIGGENDE_K", "underlying_cause"),
    c("underligg.*diagn", "underlying.*cause")
  )
  if (anyNA(c(death_pid_col, death_date_col, death_cause_col))) {
    return(empty_result(
      "Dødsårsaksfilen mangler prosjekt-ID, dødsdato eller underliggende dødsårsak."
    ))
  }

  death_frame <- data.frame(
    pid = normalise_text(death_data[[death_pid_col]]),
    death_date = as_date_safe(death_data[[death_date_col]]),
    underlying_cause = normalise_text(death_data[[death_cause_col]]),
    stringsAsFactors = FALSE
  )
  death_frame <- death_frame[
    !is.na(death_frame$pid) & death_frame$pid != "" &
      !is.na(death_frame$death_date),
    , drop = FALSE
  ]
  death_frame <- death_frame[
    order(death_frame$pid, death_frame$death_date, na.last = TRUE),
    , drop = FALSE
  ]
  death_frame <- death_frame[!duplicated(death_frame$pid), , drop = FALSE]

  outcomes <- merge(
    cases_derived,
    death_frame,
    by = "pid",
    all.x = TRUE,
    sort = FALSE
  )
  outcomes$death_before_diagnosis <-
    !is.na(outcomes$death_date) & !is.na(outcomes$diagnosis_date) &
    outcomes$death_date < outcomes$diagnosis_date

  common_period <- is.finite(outcomes$diagnosis_year) &
    outcomes$diagnosis_year >= SURVIVAL_START &
    outcomes$diagnosis_year <= SURVIVAL_END &
    !is.na(outcomes$diagnosis_date) &
    !(outcomes$death_before_diagnosis %in% TRUE)
  if (!any(common_period)) {
    return(empty_result(paste0(
      "Ingen prostatakrefttilfeller med gyldig diagnosedato i perioden ",
      SURVIVAL_START, "-", SURVIVAL_END, "."
    )))
  }

  # Observert totaloverlevelse etter alder.
  survival_data <- outcomes[
    common_period & is.finite(outcomes$age),
    , drop = FALSE
  ]
  survival_data$age_group <- age_group_survival(survival_data$age)
  survival_data <- survival_data[!is.na(survival_data$age_group), , drop = FALSE]
  survival_data$censor_date <- pmin(
    survival_data$diagnosis_date + FIVE_YEAR_DAYS,
    FOLLOW_UP_END
  )
  survival_data$event <-
    !is.na(survival_data$death_date) &
    survival_data$death_date >= survival_data$diagnosis_date &
    survival_data$death_date <= survival_data$censor_date
  same_day <- survival_data$event &
    survival_data$death_date == survival_data$diagnosis_date
  same_day[is.na(same_day)] <- FALSE
  survival_data$death_date[same_day] <-
    survival_data$diagnosis_date[same_day] + 15L
  survival_data$end_date <- survival_data$censor_date
  survival_data$end_date[survival_data$event] <-
    survival_data$death_date[survival_data$event]
  survival_data$time_years <- as.numeric(
    survival_data$end_date - survival_data$diagnosis_date
  ) / 365.25

  group_levels <- levels(age_group_survival(0))
  risk_times <- c(0:4, FIVE_YEAR_TIME)
  observed_rows <- vector("list", length(group_levels))
  risk_rows <- vector("list", length(group_levels))

  for (index in seq_along(group_levels)) {
    group_name <- group_levels[index]
    group_data <- survival_data[
      as.character(survival_data$age_group) == group_name,
      , drop = FALSE
    ]
    if (nrow(group_data) == 0L) {
      observed_rows[[index]] <- data.frame(
        group = group_name, estimate = NA_real_, lower = NA_real_,
        upper = NA_real_, n_start = 0L, n_5y = 0L
      )
      risk_rows[[index]] <- data.frame(
        group = group_name, year_0 = 0L, year_1 = 0L, year_2 = 0L,
        year_3 = 0L, year_4 = 0L, year_5 = 0L
      )
      next
    }

    fit <- survival::survfit(
      survival::Surv(time_years, event) ~ 1,
      data = group_data
    )
    endpoint <- summary(fit, times = FIVE_YEAR_TIME, extend = FALSE)
    estimate <- if (length(endpoint$surv) > 0L) 100 * endpoint$surv[1] else NA_real_
    lower <- if (length(endpoint$lower) > 0L) 100 * endpoint$lower[1] else NA_real_
    upper <- if (length(endpoint$upper) > 0L) 100 * endpoint$upper[1] else NA_real_
    risk_values <- vapply(risk_times, function(current_time) {
      if (current_time == 0) return(nrow(group_data))
      risk_summary <- summary(fit, times = current_time, extend = TRUE)
      if (length(risk_summary$n.risk) == 0L) 0 else risk_summary$n.risk[1]
    }, numeric(1))

    observed_rows[[index]] <- data.frame(
      group = group_name,
      estimate = estimate,
      lower = lower,
      upper = upper,
      n_start = as.integer(risk_values[1]),
      n_5y = as.integer(risk_values[6]),
      stringsAsFactors = FALSE
    )
    risk_rows[[index]] <- data.frame(
      group = group_name,
      year_0 = as.integer(risk_values[1]),
      year_1 = as.integer(risk_values[2]),
      year_2 = as.integer(risk_values[3]),
      year_3 = as.integer(risk_values[4]),
      year_4 = as.integer(risk_values[5]),
      year_5 = as.integer(risk_values[6]),
      stringsAsFactors = FALSE
    )
  }

  observed <- do.call(rbind, observed_rows)
  observed$group <- factor(observed$group, levels = group_levels)
  at_risk <- do.call(rbind, risk_rows)

  # Kumulativ insidens etter registrert dødsårsak i nøyaktig samme periode.
  cause_data <- outcomes[common_period, , drop = FALSE]
  cause_data$censor_date <- pmin(
    cause_data$diagnosis_date + FIVE_YEAR_DAYS,
    FOLLOW_UP_END
  )
  cause_data$death_in_window <-
    !is.na(cause_data$death_date) &
    cause_data$death_date >= cause_data$diagnosis_date &
    cause_data$death_date <= cause_data$censor_date
  cause_same_day <- cause_data$death_in_window &
    cause_data$death_date == cause_data$diagnosis_date
  cause_same_day[is.na(cause_same_day)] <- FALSE
  cause_data$death_date[cause_same_day] <-
    cause_data$diagnosis_date[cause_same_day] + 15L
  cause_data$outcome_date <- cause_data$censor_date
  cause_data$outcome_date[cause_data$death_in_window] <-
    cause_data$death_date[cause_data$death_in_window]
  cause_data$time_years <- as.numeric(
    cause_data$outcome_date - cause_data$diagnosis_date
  ) / 365.25

  cause_missing <- is_registry_missing(cause_data$underlying_cause)
  c61 <- grepl(
    "^C61",
    gsub("[^A-Z0-9]", "", cause_data$underlying_cause)
  )
  c61[is.na(c61)] <- FALSE
  other_cancer <- icd10_is_malignant_cancer(cause_data$underlying_cause) & !c61
  cause_data$status <- 0L
  cause_data$status[cause_data$death_in_window & c61] <- 1L
  cause_data$status[
    cause_data$death_in_window & !cause_missing & other_cancer
  ] <- 2L
  cause_data$status[
    cause_data$death_in_window & !cause_missing & !c61 & !other_cancer
  ] <- 3L
  cause_data$status[cause_data$death_in_window & cause_missing] <- 4L

  endpoint_cif <- 100 * aj_endpoint(cause_data$time_years, cause_data$status)
  competing <- data.frame(
    cause = factor(
      c("Prostatakreft", "Annen kreft", "Andre årsaker", "Ukjent dødsårsak"),
      levels = c("Prostatakreft", "Annen kreft", "Andre årsaker", "Ukjent dødsårsak")
    ),
    estimate = as.numeric(endpoint_cif),
    stringsAsFactors = FALSE
  )

  list(
    ok = TRUE,
    message = "",
    observed = observed,
    at_risk = at_risk,
    competing = competing,
    n_competing = nrow(cause_data),
    n_competing_5y = sum(cause_data$time_years >= FIVE_YEAR_TIME, na.rm = TRUE)
  )
}

annual_person_years <- function(start_date, end_date, years) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  vapply(years, function(year) {
    year_start <- as.Date(sprintf("%04d-01-01", year))
    next_year_start <- as.Date(sprintf("%04d-01-01", year + 1L))
    interval_start <- pmax(as.numeric(start_date), as.numeric(year_start), na.rm = FALSE)
    interval_end <- pmin(as.numeric(end_date), as.numeric(next_year_start), na.rm = FALSE)
    days <- interval_end - interval_start
    days[!is.finite(days) | days < 0] <- 0
    sum(days, na.rm = TRUE) / 365.25
  }, numeric(1))
}

load_hunt_clinical_data <- function() {
  roots <- unique(c(
    Sys.getenv("HUNT_PSA_WORK_ROOT", unset = ""),
    "/mnt/work/workbench/tingtinf/hunt_psa",
    path.expand("~/work/workbench/tingtinf/hunt_psa")
  ))
  roots <- roots[nzchar(roots)]
  case_candidates <- file.path(roots, "analysis_2026_09", "analysis_prostate_case.rds")
  case_candidates <- case_candidates[file.exists(case_candidates)]
  if (length(case_candidates) == 0L) {
    return(list(ok = FALSE, message = paste0(
      "Fant ikke analysis_prostate_case.rds. Sett HUNT_PSA_WORK_ROOT til prosjektmappen."
    )))
  }

  tryCatch({
    case_file <- case_candidates[1]
    work_root <- dirname(dirname(case_file))
    cases_all <- as.data.frame(readRDS(case_file), check.names = FALSE)
    core_candidates <- file.path(
      work_root,
      "core_2026_09",
      c("core_prostate_linked_case.rds", "core_prostate_detail_case.rds")
    )
    for (core_file in core_candidates[file.exists(core_candidates)]) {
      cases_all <- merge_missing_case_columns(cases_all, readRDS(core_file))
    }
    cases <- first_case_per_person(cases_all)
    case_pid_col <- find_column(cases, c("pid_std", "PID@115391"), c("^pid"))
    if (is.na(case_pid_col)) stop("Fant ikke prosjekt-ID i prostatakreftfilen.")
    case_pid <- normalise_text(cases[[case_pid_col]])

    diagnosis_date <- as_date_safe(get_vector(
      cases, c("diagnosis_date", "S_diagnosedato"), c("diagnos.*dato", "diagnos.*date")
    ))
    diagnosis_year <- parse_number(get_vector(
      cases, c("diagnosis_year", "S_diagnoseAr"), c("diagnos.*year", "diagnos.*ar")
    ))
    diagnosis_year[!is.finite(diagnosis_year) & !is.na(diagnosis_date)] <-
      as.integer(format(diagnosis_date[!is.finite(diagnosis_year) & !is.na(diagnosis_date)], "%Y"))
    age <- parse_number(get_vector(
      cases, c("age_at_diagnosis", "diagnosis_age"), c("age.*diagnos", "alder.*diagnos")
    ))
    psa <- get_vector(cases, c("psa_at_diagnosis", "S_psaDiagnose"), c("psa.*diagnos"))
    isup <- get_vector(cases, c("isup_grade", "S_isupGrad"), c("isup"))
    ct_dre <- get_vector(
      cases, c("S_palpatoriskTstadiumSamlet", "palpatory_t_stage"), c("palpatorisk.*t", "dre.*t")
    )
    rad_t <- get_vector(
      cases, c("S_mrBasertTstadiumSamlet", "mr_based_t_stage"), c("mr.*tstad", "rad.*t")
    )
    n_stage <- get_vector(
      cases,
      c("S_kliniskNstadiumSamlet", "S_mrBasertNstadiumSamlet", "clinical_n_stage"),
      c("nstadium", "n_stage")
    )
    m_stage <- get_vector(
      cases,
      c("S_kliniskMstadiumSamlet", "S_mrBasertMstadiumSamlet", "clinical_m_stage"),
      c("mstadium", "m_stage")
    )
    direct_risk_raw <- get_vector(
      cases,
      c("S_risikogruppeDiagnoseDre", "risk_group_dre", "risk_group_at_diagnosis"),
      c("risikogruppe.*diagnos", "risk.*group.*diagnos")
    )
    direct_risk <- canonical_risk_group(direct_risk_raw)
    derived_risk <- derive_risk_group(psa, isup, ct_dre, n_stage, m_stage)
    risk_group <- direct_risk
    fill_risk <- is.na(risk_group) & !is.na(derived_risk)
    risk_group[fill_risk] <- derived_risk[fill_risk]
    risk_method <- if (sum(!is.na(direct_risk)) > 0L) {
      "registerets risikogruppe, supplert fra PSA, ISUP, cT-DRE og N-/M-stadium"
    } else {
      "beregnet fra PSA, ISUP, cT-DRE og N-/M-stadium"
    }

    cases_derived <- data.frame(
      pid = case_pid,
      diagnosis_date = diagnosis_date,
      diagnosis_year = as.integer(diagnosis_year),
      age = age,
      psa = parse_number(psa),
      isup = parse_number(isup),
      ct_dre = t_stage_group(ct_dre),
      rad_t = t_stage_group(rad_t),
      risk_group = factor(risk_group, levels = risk_levels),
      stringsAsFactors = FALSE
    )

    full_index <- is.finite(diagnosis_year) &
      diagnosis_year >= HUNT_DESCRIPTION_START &
      diagnosis_year <= HUNT_DESCRIPTION_END
    cases_full <- cases_derived[full_index, , drop = FALSE]

    description_years <- HUNT_DESCRIPTION_START:HUNT_DESCRIPTION_END
    annual_hunt <- data.frame(
      year = description_years,
      hunt_new_diagnoses = as.numeric(table(factor(
        cases_full$diagnosis_year,
        levels = description_years
      ))),
      hunt_pc_deaths = NA_real_,
      hunt_incidence_rate = NA_real_,
      stringsAsFactors = FALSE
    )

    # Årlige dødsfall og ustandardiserte insidensrater beregnes fra de samme
    # analysefilene som Quarto-rapporten når de finnes i HUNT Cloud.
    person_file <- file.path(work_root, "analysis_2026_09", "analysis_person_followup.rds")
    death_file <- file.path(work_root, "core_2026_09", "core_death_record.rds")
    if (file.exists(person_file)) {
      analysis_person <- as.data.frame(readRDS(person_file), check.names = FALSE)
      person_pid_col <- find_column(analysis_person, c("pid_std", "PID@115391"), c("^pid"))
      entry_col <- find_column(
        analysis_person,
        c("hunt_entry_date", "entry_date", "baseline_date", "followup_start", "follow_up_start"),
        c("hunt.*entry.*date", "entry.*date", "baseline.*date")
      )
      end_col <- find_column(
        analysis_person,
        c("follow_up_end", "followup_end", "follow_up_end_date", "followup_end_date", "end_of_followup"),
        c("follow.*up.*end", "end.*follow")
      )
      followup_years_col <- find_column(
        analysis_person,
        c("followup_years", "follow_up_years"),
        c("follow.*years")
      )

      if (!is.na(person_pid_col) && !is.na(entry_col)) {
        person_pid <- normalise_text(analysis_person[[person_pid_col]])
        entry_date <- as_date_safe(analysis_person[[entry_col]])
        followup_end <- as.Date(rep(NA_real_, nrow(analysis_person)), origin = "1970-01-01")
        if (!is.na(end_col)) {
          followup_end <- as_date_safe(analysis_person[[end_col]])
        } else if (!is.na(followup_years_col)) {
          followup_years <- parse_number(analysis_person[[followup_years_col]])
          usable_followup <- !is.na(entry_date) & is.finite(followup_years) & followup_years >= 0
          followup_end[usable_followup] <- entry_date[usable_followup] +
            as.integer(round(365.25 * followup_years[usable_followup]))
        }
        followup_end[!is.na(followup_end) & followup_end > FOLLOW_UP_END] <- FOLLOW_UP_END
        valid_followup <- !is.na(person_pid) & person_pid != "" &
          !is.na(entry_date) & !is.na(followup_end) & followup_end > entry_date

        if (any(valid_followup)) {
          incidence_py <- annual_person_years(
            entry_date[valid_followup],
            followup_end[valid_followup],
            description_years
          )
          annual_hunt$hunt_incidence_rate <- ifelse(
            incidence_py > 0,
            100000 * annual_hunt$hunt_new_diagnoses / incidence_py,
            NA_real_
          )
        }

        if (file.exists(death_file)) {
          death_data <- as.data.frame(readRDS(death_file), check.names = FALSE)
          death_pid_col <- find_column(death_data, c("pid_std", "LOPENR"), c("^pid"))
          death_date_col <- find_column(death_data, c("death_date"), c("dod.*dato", "death.*date"))
          death_cause_col <- find_column(
            death_data,
            c("DIAGNOSE_UNDERLIGGENDE_K", "underlying_cause"),
            c("underligg.*diagn", "underlying.*cause")
          )
          if (!anyNA(c(death_pid_col, death_date_col, death_cause_col))) {
            death_frame <- data.frame(
              pid = normalise_text(death_data[[death_pid_col]]),
              date = as_date_safe(death_data[[death_date_col]]),
              cause = normalise_text(death_data[[death_cause_col]]),
              stringsAsFactors = FALSE
            )
            death_frame <- death_frame[
              !is.na(death_frame$pid) & death_frame$pid != "" & !is.na(death_frame$date),
              , drop = FALSE
            ]
            death_frame <- death_frame[order(death_frame$pid, death_frame$date), , drop = FALSE]
            death_frame <- death_frame[!duplicated(death_frame$pid), , drop = FALSE]
            person_index <- match(death_frame$pid, person_pid)
            in_main_followup <- !is.na(person_index) &
              death_frame$date >= entry_date[person_index] &
              death_frame$date <= FOLLOW_UP_END
            in_main_followup[is.na(in_main_followup)] <- FALSE
            c61_cause <- grepl(
              "^C61",
              gsub("[^A-Z0-9]", "", death_frame$cause)
            )
            c61_cause[is.na(c61_cause)] <- FALSE
            c61_deaths <- death_frame[
              in_main_followup & c61_cause,
              , drop = FALSE
            ]
            death_year <- as.integer(format(c61_deaths$date, "%Y"))
            annual_hunt$hunt_pc_deaths <- as.numeric(table(factor(
              death_year,
              levels = description_years
            )))
          }
        }
      }
    }

    management_index <- is.finite(diagnosis_year) &
      diagnosis_year >= TREATMENT_START & diagnosis_year <= TREATMENT_END
    management_raw <- cases[management_index, , drop = FALSE]
    management <- cases_derived[management_index, , drop = FALSE]
    surgery_tech <- get_vector(
      management_raw, c("S_kirurgiskTeknikkPrimar", "S_kir"), c("kirurgisk.*tekn", "^S_kir$")
    )
    surgery_date <- as_date_safe(get_vector(
      management_raw, c("S_datoOprPrimar"), c("dato.*opr", "operas.*dato")
    ))
    radiation <- get_vector(management_raw, c("S_stralebeh"), c("stralebeh"))
    radiation_date <- as_date_safe(get_vector(
      management_raw, c("S_datoOppstartStralebeh"), c("dato.*strale", "strale.*dato")
    ))
    radiation_dose <- parse_number(get_vector(
      management_raw,
      c("S_ekvivalentDoseKurativStralebeh", "S_totDoseKurativStralebeh"),
      c("dose.*strale", "strale.*dose")
    ))
    active <- get_vector(
      management_raw, c("S_aktivOvervaket", "active_surveillance"), c("aktiv.*overvak")
    )

    valid_surgery_date <- !is.na(surgery_date) & surgery_date >= as.Date("1900-01-01") &
      surgery_date <= as.Date("2025-12-31")
    valid_radiation_date <- !is.na(radiation_date) & radiation_date >= as.Date("1900-01-01") &
      radiation_date <= as.Date("2025-12-31")
    surgery_flag <- positive_value_present(surgery_tech) | valid_surgery_date
    radiation_flag <- yes_flag(radiation) | valid_radiation_date |
      (is.finite(radiation_dose) & radiation_dose > 0)
    active_flag <- yes_flag(active)

    treatment <- rep("Annet/ingen vist", nrow(management))
    treatment[active_flag & !surgery_flag & !radiation_flag] <- "Aktiv overvåkning"
    treatment[surgery_flag & !radiation_flag] <- "Prostatektomi"
    treatment[radiation_flag & !surgery_flag] <- "Strålebehandling"
    both <- surgery_flag & radiation_flag
    treatment[both & valid_surgery_date & valid_radiation_date & surgery_date <= radiation_date] <-
      "Prostatektomi"
    treatment[both & valid_surgery_date & valid_radiation_date & radiation_date < surgery_date] <-
      "Strålebehandling"

    management$treatment <- factor(
      treatment,
      levels = c("Prostatektomi", "Strålebehandling", "Aktiv overvåkning", "Annet/ingen vist")
    )
    management$age_group <- age_group_treatment(management$age)
    management$operated <- surgery_flag
    management$valid_surgery_date <- valid_surgery_date
    survival_summary <- build_five_year_outcomes(cases_derived, death_file)

    list(
      ok = TRUE,
      message = "",
      work_root = work_root,
      cases_full = cases_full,
      annual_hunt = annual_hunt,
      cases_analysis = cases_derived[management_index, , drop = FALSE],
      management = management,
      risk_method = risk_method,
      survival_summary = survival_summary
    )
  }, error = function(e) {
    list(ok = FALSE, message = conditionMessage(e))
  })
}

aggregate_distribution <- function(values, levels) {
  known <- !is.na(values)
  counts <- as.numeric(table(factor(values[known], levels = levels)))
  denominator <- sum(counts)
  data.frame(
    stratum = levels,
    count = counts,
    total = denominator,
    percent = if (denominator > 0L) 100 * counts / denominator else NA_real_,
    stringsAsFactors = FALSE
  )
}

aggregate_treatment <- function(stratum, treatment, stratum_levels) {
  treatment_levels <- c(
    "Prostatektomi", "Strålebehandling", "Aktiv overvåkning", "Annet/ingen vist"
  )
  keep <- !is.na(stratum) & !is.na(treatment)
  tab <- table(
    factor(stratum[keep], levels = stratum_levels),
    factor(treatment[keep], levels = treatment_levels)
  )
  out <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(out) <- c("stratum", "treatment", "count")
  out$total <- ave(out$count, out$stratum, FUN = sum)
  out$percent <- ifelse(out$total > 0, 100 * out$count / out$total, NA_real_)
  out
}

build_descriptive_summaries <- function(hunt_data) {
  if (!isTRUE(hunt_data$ok) || is.null(hunt_data$cases_full)) return(NULL)
  d <- hunt_data$cases_full
  n_cases <- nrow(d)
  if (n_cases == 0L) return(NULL)

  age_known <- is.finite(d$age)
  age_summary <- data.frame(
    source = "HUNT",
    period = paste0(HUNT_DESCRIPTION_START, "-", HUNT_DESCRIPTION_END),
    n = sum(age_known),
    median_age = if (any(age_known)) median(d$age[age_known]) else NA_real_,
    pct_under_70 = if (any(age_known)) 100 * mean(d$age[age_known] < 70) else NA_real_,
    stringsAsFactors = FALSE
  )

  psa_known <- is.finite(d$psa) & d$psa >= 0
  psa_category_levels <- c("<4", "4-9,9", "10-19,9", "20-99,9", "≥100")
  psa_category <- cut(
    d$psa[psa_known],
    breaks = c(-Inf, 4, 10, 20, 100, Inf),
    labels = psa_category_levels,
    right = FALSE
  )
  psa_counts <- as.numeric(table(factor(psa_category, levels = psa_category_levels)))
  psa_denominator <- sum(psa_counts)
  psa_categories <- data.frame(
    category = psa_category_levels,
    source = HUNT_DESCRIPTION_LABEL,
    count = psa_counts,
    denominator = psa_denominator,
    percent = if (psa_denominator > 0L) 100 * psa_counts / psa_denominator else NA_real_,
    completeness = 100 * psa_denominator / n_cases,
    stringsAsFactors = FALSE
  )

  age_levels <- levels(age_group_description(0))
  age_group <- age_group_description(d$age)
  psa_age <- do.call(rbind, lapply(age_levels, function(group_name) {
    values <- d$psa[
      as.character(age_group) == group_name & is.finite(d$psa) & d$psa >= 0
    ]
    data.frame(
      age_group = group_name,
      hunt_median = if (length(values) > 0L) median(values) else NA_real_,
      hunt_q1 = if (length(values) > 0L) unname(quantile(values, 0.25)) else NA_real_,
      hunt_q3 = if (length(values) > 0L) unname(quantile(values, 0.75)) else NA_real_,
      hunt_n = length(values),
      stringsAsFactors = FALSE
    )
  }))
  psa_age$age_group <- factor(psa_age$age_group, levels = age_levels)

  isup_known <- d$isup %in% 1:5
  isup_counts <- as.numeric(table(factor(d$isup[isup_known], levels = 1:5)))
  isup_denominator <- sum(isup_counts)
  isup <- data.frame(
    isup = paste("ISUP", 1:5),
    source = HUNT_DESCRIPTION_LABEL,
    count = isup_counts,
    denominator = isup_denominator,
    percent = if (isup_denominator > 0L) 100 * isup_counts / isup_denominator else NA_real_,
    completeness = 100 * isup_denominator / n_cases,
    stringsAsFactors = FALSE
  )

  stage_levels <- c("T0-2", "T3", "T4", "TX")
  make_stage_summary <- function(values, method_name) {
    known <- !is.na(values)
    counts <- as.numeric(table(factor(values[known], levels = stage_levels)))
    denominator <- sum(counts)
    data.frame(
      method = method_name,
      category = stage_levels,
      source = HUNT_DESCRIPTION_LABEL,
      count = counts,
      denominator = denominator,
      percent = if (denominator > 0L) 100 * counts / denominator else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  t_stage <- rbind(
    make_stage_summary(d$ct_dre, "cT-DRE"),
    make_stage_summary(d$rad_t, "radT")
  )

  list(
    n_cases = n_cases,
    annual_hunt = hunt_data$annual_hunt,
    age = age_summary,
    psa_categories = psa_categories,
    psa_by_age = psa_age,
    isup = isup,
    t_stage = t_stage,
    n_ct_dre = sum(!is.na(d$ct_dre)),
    n_rad_t = sum(!is.na(d$rad_t)),
    n_both_stage = sum(!is.na(d$ct_dre) & !is.na(d$rad_t))
  )
}

build_clinical_summaries <- function(hunt_data) {
  if (!isTRUE(hunt_data$ok)) return(NULL)
  analysis_cases <- hunt_data$cases_analysis
  management <- hunt_data$management
  operated <- management[management$operated %in% TRUE, , drop = FALSE]
  list(
    risk_distribution = aggregate_distribution(analysis_cases$risk_group, risk_levels),
    treatment_age = aggregate_treatment(
      management$age_group, management$treatment,
      c("Under 70 år", "70-74 år", "75-79 år", "80 år eller eldre")
    ),
    treatment_risk = aggregate_treatment(
      management$risk_group, management$treatment, risk_levels
    ),
    treatment_ct = aggregate_treatment(
      management$ct_dre, management$treatment, c("T0-2", "T3", "T4", "TX")
    ),
    treatment_radt = aggregate_treatment(
      management$rad_t, management$treatment, c("T0-2", "T3", "T4", "TX")
    ),
    operated_risk = aggregate_distribution(operated$risk_group, risk_levels),
    n_analysis = nrow(analysis_cases),
    n_risk_known = sum(!is.na(analysis_cases$risk_group)),
    n_management = nrow(management)
  )
}

# -----------------------------------------------------------------------------
# Aggregerte HUNT- og nasjonale tall
# -----------------------------------------------------------------------------

annual_stats <- data.frame(
  year = 2016:2025,
  hunt_new_diagnoses = c(106, 101, 94, 88, 106, 92, 111, 102, 99, 114),
  hunt_pc_deaths = c(11, 18, 11, 26, 23, 19, 23, 12, 18, 18),
  hunt_incidence_rate = c(623, 608, 579, 554, 683, 610, 759, 719, 716, 854),
  norway_new_cases = c(5328, 5126, 4966, 5035, 5120, 5273, 5560, 5325, 5245, 5350),
  norway_pc_deaths = c(965, 936, 928, 961, 958, 896, 973, 865, 833, 876),
  norway_incidence_rate = c(211.0, 197.6, 185.5, 184.9, 181.8, 181.9, 187.0, 174.9, 167.8, 168.7),
  norway_prevalence = c(47521, 50045, 52400, 54688, 57001, 59353, 61703, 63842, 65942, 67909)
)

diagnosis_age <- data.frame(
  source = c("HUNT", "HUNT", "Norge"),
  period = c("2006-2025", "2021-2025", "2025"),
  n = c(1737, 518, 5350),
  median_age = c(71, 73, 70),
  pct_under_70 = c(44.0, 33.8, 48.0)
)

lab_psa_thresholds <- data.frame(
  threshold = factor(
    c("PSA ≥3 ng/mL", "PSA ≥4 ng/mL", "PSA ≥10 ng/mL"),
    levels = c("PSA ≥3 ng/mL", "PSA ≥4 ng/mL", "PSA ≥10 ng/mL")
  ),
  count = c(2342, 1562, 351),
  n = c(18723, 18723, 18723),
  percent = c(12.51, 8.34, 1.87),
  ci_low = c(12.04, 7.95, 1.69),
  ci_high = c(12.99, 8.75, 2.08)
)

psa_categories <- data.frame(
  category = rep(c("<4", "4-9,9", "10-19,9", "20-99,9", "≥100"), 2),
  source = rep(c("HUNT 2021-2025", "Norge 2025"), each = 5),
  count = c(21, 202, 145, 93, 38, NA, NA, NA, NA, NA),
  denominator = c(rep(499, 5), rep(4627, 5)),
  percent = c(4.2, 40.5, 29.1, 18.6, 7.6, 9, 46, 23, 16, 6),
  completeness = c(rep(96.3, 5), rep(89.0, 5))
)

psa_by_age <- data.frame(
  age_group = factor(
    c("0-54", "55-64", "65-74", "75-84", "85+"),
    levels = c("0-54", "55-64", "65-74", "75-84", "85+")
  ),
  hunt_median = c(8.1, 8.4, 10.0, 14.6, 66.0),
  hunt_q1 = c(6.0, 5.9, 7.0, 9.4, 25.0),
  hunt_q3 = c(11.8, 13.1, 17.0, 35.9, 161.5),
  hunt_n = c(35, 304, 651, 376, 75),
  norway_median_approx = c(6, 7, 9, 12, 48)
)

isup_data <- data.frame(
  isup = rep(paste("ISUP", 1:5), 2),
  source = rep(c("HUNT 2021-2025", "Norge 2025"), each = 5),
  count = c(52, 151, 120, 52, 75, NA, NA, NA, NA, NA),
  denominator = c(rep(450, 5), rep(4941, 5)),
  percent = c(11.6, 33.6, 26.7, 11.6, 16.7, 22, 34, 18, 13, 13),
  completeness = c(rep(86.9, 5), rep(95.0, 5))
)

t_stage_data <- data.frame(
  method = rep(c("cT-DRE", "radT"), each = 8),
  category = rep(rep(c("T0-2", "T3", "T4", "TX"), 2), 2),
  source = rep(rep(c("HUNT 2021-2025", "Norge 2025"), each = 4), 2),
  count = c(
    268, 56, 8, 18, NA, NA, NA, NA,
    247, 71, 12, 20, NA, NA, NA, NA
  ),
  denominator = c(
    rep(350, 4), rep(NA, 4),
    rep(350, 4), rep(NA, 4)
  ),
  percent = c(
    76.6, 16.0, 2.3, 5.1, 77, 17, 3, 3,
    70.6, 20.3, 3.4, 5.7, 55, 28, 4, 13
  )
)

treatment_hunt <- data.frame(
  age_group = rep(
    c("Under 70 år", "70-74 år", "75-79 år", "80 år eller eldre"),
    each = 5
  ),
  treatment = rep(
    c(
      "Aktiv overvåkning",
      "Prostatektomi",
      "Strålebehandling",
      "Flere radikale behandlinger",
      "Ingen vist behandling registrert"
    ),
    4
  ),
  count = c(
    29, 62, 32, 4, 10,
    13, 25, 41, 3, 13,
    24, 4, 45, 0, 19,
    12, 0, 11, 0, 57
  ),
  total = rep(c(137, 95, 92, 80), each = 5)
)
treatment_hunt$percent <- 100 * treatment_hunt$count / treatment_hunt$total

observed_survival <- data.frame(
  group = factor(
    c("Under 65 år", "65-74 år", "75 år eller eldre"),
    levels = c("Under 65 år", "65-74 år", "75 år eller eldre")
  ),
  estimate = rep(NA_real_, 3),
  lower = rep(NA_real_, 3),
  upper = rep(NA_real_, 3),
  n_start = rep(0L, 3),
  n_5y = rep(0L, 3)
)

survival_at_risk <- data.frame(
  group = c("Under 65 år", "65-74 år", "75 år eller eldre"),
  year_0 = rep(0L, 3),
  year_1 = rep(0L, 3),
  year_2 = rep(0L, 3),
  year_3 = rep(0L, 3),
  year_4 = rep(0L, 3),
  year_5 = rep(0L, 3)
)

competing_mortality <- data.frame(
  cause = factor(
    c("Prostatakreft", "Annen kreft", "Andre årsaker", "Ukjent dødsårsak"),
    levels = c("Prostatakreft", "Annen kreft", "Andre årsaker", "Ukjent dødsårsak")
  ),
  estimate = rep(NA_real_, 4)
)
competing_mortality_n <- NA_integer_
survival_status_message <- paste0(
  "Femårsresultatene beregnes fra HUNT-individdata for ",
  SURVIVAL_PERIOD_LABEL, "."
)

national_risk_distribution <- data.frame(
  stratum = risk_levels,
  percent = c(18, 27, 20, 20, 15),
  source = "Norge 2025",
  stringsAsFactors = FALSE
)

# Årsrapportens figur 2.12 og 2.13 viser glattede kurver uten talltabell.
# Verdiene under er derfor merket som omtrentlige avlesninger fra 2024-punktene.
national_treatment_age <- data.frame(
  stratum = rep(
    c("Under 70 år", "70-74 år", "75-79 år", "80 år eller eldre"),
    each = 4
  ),
  treatment = rep(
    c("Prostatektomi", "Strålebehandling", "Aktiv overvåkning", "Annet/ingen vist"),
    4
  ),
  percent = c(
    46, 10, 33, 11,
    34, 23, 28, 15,
    16, 31, 22, 31,
    2, 18, 11, 69
  ),
  source = "Norge 2024 (ca.)",
  stringsAsFactors = FALSE
)

national_treatment_risk <- data.frame(
  stratum = rep(risk_levels, each = 4),
  treatment = rep(
    c("Prostatektomi", "Strålebehandling", "Aktiv overvåkning", "Annet/ingen vist"),
    length(risk_levels)
  ),
  percent = c(
    6, 0, 83, 11,
    43, 6, 42, 9,
    63, 14, 15, 8,
    62, 24, 8, 6,
    34, 50, 2, 14
  ),
  source = "Norge 2024 (ca.)",
  stringsAsFactors = FALSE
)

national_operated_risk <- data.frame(
  stratum = risk_levels,
  percent = c(3, 32, 33, 25, 7),
  source = "Norge 2025",
  stringsAsFactors = FALSE
)

hunt_clinical <- load_hunt_clinical_data()
hunt_clinical_summary <- build_clinical_summaries(hunt_clinical)
hunt_descriptive_summary <- build_descriptive_summaries(hunt_clinical)

if (isTRUE(hunt_clinical$ok) &&
    isTRUE(hunt_clinical$survival_summary$ok)) {
  observed_survival <- hunt_clinical$survival_summary$observed
  survival_at_risk <- hunt_clinical$survival_summary$at_risk
  competing_mortality <- hunt_clinical$survival_summary$competing
  competing_mortality_n <- hunt_clinical$survival_summary$n_competing
} else {
  survival_error <- if (isTRUE(hunt_clinical$ok)) {
    hunt_clinical$survival_summary$message
  } else {
    hunt_clinical$message
  }
  survival_status_message <- paste(
    "Femårsresultatene kunne ikke beregnes:", survival_error
  )
}

# Når individdata er tilgjengelige, erstattes de tidligere innskrevne HUNT-
# aggregatene med én konsekvent diagnoseperiode: 2006-2025. Nasjonale tall
# beholdes som referanse for 2025 og er derfor tydelig merket med egen periode.
hunt_profile_source <- "HUNT 2021-2025"
annual_hunt_stats <- annual_stats[, c(
  "year", "hunt_new_diagnoses", "hunt_pc_deaths", "hunt_incidence_rate"
)]
if (!is.null(hunt_descriptive_summary)) {
  hunt_profile_source <- HUNT_DESCRIPTION_LABEL

  annual_hunt_stats <- hunt_descriptive_summary$annual_hunt
  recent_index <- match(annual_stats$year, annual_hunt_stats$year)
  for (column in c("hunt_pc_deaths", "hunt_incidence_rate")) {
    missing_recent <- is.na(annual_hunt_stats[[column]][recent_index])
    annual_hunt_stats[[column]][recent_index[missing_recent]] <-
      annual_stats[[column]][missing_recent]
  }

  diagnosis_age <- rbind(
    hunt_descriptive_summary$age,
    diagnosis_age[diagnosis_age$source == "Norge", , drop = FALSE]
  )

  psa_categories <- rbind(
    hunt_descriptive_summary$psa_categories,
    psa_categories[psa_categories$source == "Norge 2025", , drop = FALSE]
  )

  national_psa_age <- psa_by_age[, c("age_group", "norway_median_approx")]
  psa_by_age <- merge(
    hunt_descriptive_summary$psa_by_age,
    national_psa_age,
    by = "age_group",
    all.x = TRUE,
    sort = FALSE
  )
  psa_by_age$age_group <- factor(
    psa_by_age$age_group,
    levels = levels(hunt_descriptive_summary$psa_by_age$age_group)
  )
  psa_by_age <- psa_by_age[order(psa_by_age$age_group), , drop = FALSE]

  isup_data <- rbind(
    hunt_descriptive_summary$isup,
    isup_data[isup_data$source == "Norge 2025", , drop = FALSE]
  )

  t_stage_data <- rbind(
    hunt_descriptive_summary$t_stage,
    t_stage_data[t_stage_data$source == "Norge 2025", , drop = FALSE]
  )
}
hunt_annual_period <- paste0(
  min(annual_hunt_stats$year, na.rm = TRUE), "-",
  max(annual_hunt_stats$year, na.rm = TRUE)
)

empty_clinical_plot <- function(message) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message, size = 5, colour = COL_HUNT_DARK) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void()
}

plot_distribution_comparison <- function(hunt_data, national_data, scale_mode) {
  if (is.null(hunt_data) || nrow(hunt_data) == 0L || sum(hunt_data$count) == 0L) {
    return(empty_clinical_plot("Ingen beregnbare HUNT-resultater"))
  }
  hunt_plot <- hunt_data
  hunt_plot$source <- "HUNT"
  if (identical(scale_mode, "count")) {
    plot_data <- hunt_plot
    plot_data$value <- plot_data$count
    plot_data$label <- fmt_n(plot_data$count)
    y_label <- "Antall menn"
  } else {
    national_plot <- national_data
    national_plot$count <- NA_real_
    national_plot$total <- NA_real_
    plot_data <- rbind(
      hunt_plot[, c("stratum", "count", "total", "percent", "source")],
      national_plot[, c("stratum", "count", "total", "percent", "source")]
    )
    plot_data$value <- plot_data$percent
    plot_data$label <- fmt_pct1(plot_data$percent)
    y_label <- "Andel (%)"
  }
  plot_data$stratum <- factor(plot_data$stratum, levels = rev(risk_levels))
  source_colours <- c(
    "HUNT" = COL_HUNT,
    "Norge 2025" = COL_NORWAY
  )
  ggplot(plot_data, aes(x = stratum, y = value, fill = source)) +
    geom_col(position = position_dodge(width = 0.78), width = 0.68) +
    geom_text(
      aes(label = label),
      position = position_dodge(width = 0.78),
      hjust = -0.08,
      size = 4.0,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_fill_manual(values = source_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.24))) +
    coord_flip(clip = "off") +
    labs(x = NULL, y = y_label) +
    theme_clinical(14) +
    theme(plot.margin = margin(12, 80, 12, 12))
}

plot_treatment_comparison <- function(hunt_data, national_data, scale_mode, facet_ncol) {
  if (is.null(hunt_data) || nrow(hunt_data) == 0L || sum(hunt_data$count) == 0L) {
    return(empty_clinical_plot("Ingen beregnbare HUNT-resultater"))
  }
  hunt_plot <- hunt_data
  hunt_plot$source <- "HUNT"
  if (identical(scale_mode, "count")) {
    plot_data <- hunt_plot
    plot_data$value <- plot_data$count
    plot_data$label <- fmt_n(plot_data$count)
    y_label <- "Antall menn"
    caption <- "Bare HUNT vises når Antall er valgt."
  } else {
    national_plot <- national_data
    national_plot$count <- NA_real_
    national_plot$total <- NA_real_
    plot_data <- rbind(
      hunt_plot[, c("stratum", "treatment", "count", "total", "percent", "source")],
      national_plot[, c("stratum", "treatment", "count", "total", "percent", "source")]
    )
    plot_data$value <- plot_data$percent
    plot_data$label <- ifelse(
      plot_data$source == "HUNT",
      fmt_pct1(plot_data$percent),
      paste0("≈", fmt_pct1(plot_data$percent))
    )
    y_label <- "Andel innen gruppe (%)"
    caption <- "Norge 2024: omtrentlige avlesninger fra årsrapportens glattede kurver."
  }
  treatment_levels <- c(
    "Prostatektomi", "Strålebehandling", "Aktiv overvåkning", "Annet/ingen vist"
  )
  plot_data$stratum <- factor(plot_data$stratum, levels = unique(national_data$stratum))
  plot_data$treatment <- factor(plot_data$treatment, levels = treatment_levels)
  plot_data$treatment_label <- factor(
    plot_data$treatment,
    levels = treatment_levels,
    labels = c("Prostatektomi", "Stråle", "Aktiv overv.", "Annet")
  )
  source_colours <- c(
    "HUNT" = COL_HUNT,
    "Norge 2024 (ca.)" = COL_NORWAY
  )
  ggplot(plot_data, aes(x = treatment_label, y = value, fill = source)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(
      aes(label = label),
      position = position_dodge(width = 0.8),
      vjust = -0.4,
      size = 3.4,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_fill_manual(values = source_colours) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
    facet_wrap(~stratum, ncol = facet_ncol) +
    labs(x = NULL, y = y_label, caption = caption) +
    theme_clinical(13) +
    theme(
      axis.text.x = element_text(angle = 22, hjust = 1),
      panel.spacing = grid::unit(1.1, "lines")
    )
}

plot_treatment_stage <- function(hunt_data, scale_mode) {
  if (is.null(hunt_data) || nrow(hunt_data) == 0L || sum(hunt_data$count) == 0L) {
    return(empty_clinical_plot("Ingen beregnbare HUNT-resultater"))
  }
  plot_data <- hunt_data
  use_percent <- identical(scale_mode, "percent")
  plot_data$value <- if (use_percent) plot_data$percent else plot_data$count
  plot_data$label <- if (use_percent) fmt_pct1(plot_data$percent) else fmt_n(plot_data$count)
  treatment_levels <- c(
    "Prostatektomi", "Strålebehandling", "Aktiv overvåkning", "Annet/ingen vist"
  )
  plot_data$treatment <- factor(plot_data$treatment, levels = treatment_levels)
  plot_data$treatment_label <- factor(
    plot_data$treatment,
    levels = treatment_levels,
    labels = c("Prostatektomi", "Stråle", "Aktiv overv.", "Annet")
  )
  ggplot(plot_data, aes(x = treatment_label, y = value, fill = treatment)) +
    geom_col(width = 0.68) +
    geom_text(aes(label = label), vjust = -0.4, size = 3.7, fontface = "bold") +
    scale_fill_manual(values = c(
      "Prostatektomi" = COL_HUNT,
      "Strålebehandling" = COL_ORANGE,
      "Aktiv overvåkning" = COL_HUNT_LIGHT,
      "Annet/ingen vist" = COL_GREY
    )) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
    facet_wrap(~stratum, ncol = 2) +
    labs(
      x = NULL,
      y = if (use_percent) "Andel innen stadium (%)" else "Antall menn"
    ) +
    theme_clinical(13) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 22, hjust = 1),
      panel.spacing = grid::unit(1.1, "lines")
    )
}

# Kontrollerer at de innebygde aggregerte tabellene er konsistente.
survival_complete <- is.finite(observed_survival$estimate) &
  is.finite(observed_survival$lower) &
  is.finite(observed_survival$upper)
stopifnot(
  nrow(annual_stats) == 10L,
  length(unique(annual_stats$year)) == 10L,
  length(unique(annual_hunt_stats$year)) == nrow(annual_hunt_stats),
  all(lab_psa_thresholds$count <= lab_psa_thresholds$n),
  all(psa_categories$count[!is.na(psa_categories$count)] <=
        psa_categories$denominator[!is.na(psa_categories$count)]),
  all(isup_data$count[!is.na(isup_data$count)] <=
        isup_data$denominator[!is.na(isup_data$count)]),
  all(with(treatment_hunt, ave(count, age_group, FUN = sum) == total)),
  all(observed_survival$lower[survival_complete] <=
        observed_survival$estimate[survival_complete]),
  all(observed_survival$estimate[survival_complete] <=
        observed_survival$upper[survival_complete]),
  identical(survival_at_risk$year_0, observed_survival$n_start),
  identical(survival_at_risk$year_5, observed_survival$n_5y),
  all(tapply(national_treatment_age$percent, national_treatment_age$stratum, sum) == 100),
  all(tapply(national_treatment_risk$percent, national_treatment_risk$stratum, sum) == 100),
  sum(national_risk_distribution$percent) == 100,
  sum(national_operated_risk$percent) == 100
)
if (!is.null(hunt_descriptive_summary)) {
  stopifnot(
    sum(annual_hunt_stats$hunt_new_diagnoses, na.rm = TRUE) ==
      hunt_descriptive_summary$n_cases,
    sum(hunt_descriptive_summary$psa_categories$count) ==
      unique(hunt_descriptive_summary$psa_categories$denominator),
    sum(hunt_descriptive_summary$isup$count) ==
      unique(hunt_descriptive_summary$isup$denominator)
  )
}

# -----------------------------------------------------------------------------
# Brukergrensesnitt
# -----------------------------------------------------------------------------

kpi_card <- function(value, label, detail = NULL, source = "hunt") {
  div(
    class = paste("kpi-card", paste0("kpi-", source)),
    div(class = "kpi-value", value),
    div(class = "kpi-label", label),
    if (!is.null(detail)) div(class = "kpi-detail", detail)
  )
}

source_line <- function(text) {
  div(class = "source-line", text)
}

ui <- navbarPage(
  title = div(
    class = "app-brand",
    span(class = "brand-mark", "HUNT"),
    span("PSA")
  ),
  id = "main_tabs",
  collapsible = TRUE,
  header = tagList(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$style(HTML("
        :root {
          --hunt-blue:#236B93; --hunt-dark:#174A6E; --hunt-light:#8CC3D5;
          --norway-red:#D85C4A; --ink:#243640; --muted:#667983;
          --line:#DCE5E9; --page:#F4F7F8; --paper:#FFFFFF;
        }
        html, body { background:var(--page); color:var(--ink); font-family:Inter,'Segoe UI',Arial,sans-serif; }
        .navbar-default { background:#fff; border:0; border-bottom:1px solid var(--line); box-shadow:0 2px 12px rgba(23,74,110,.07); }
        .navbar-default .navbar-brand, .navbar-default .navbar-nav>li>a { color:var(--hunt-dark); }
        .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover { background:#E9F2F6; color:var(--hunt-dark); font-weight:700; }
        .app-brand { display:inline-flex; align-items:center; gap:9px; font-weight:700; }
        .brand-mark { background:var(--hunt-dark); color:white; border-radius:5px; padding:4px 7px; font-size:12px; letter-spacing:.08em; }
        .report-strip { display:flex; justify-content:space-between; align-items:center; gap:20px; background:var(--hunt-dark); color:white; min-height:48px; padding:8px 5vw; }
        .report-strip small { color:#DCEBF2; margin-left:14px; }
        .fullscreen-btn { color:white; background:transparent; border:1px solid rgba(255,255,255,.65); border-radius:5px; padding:5px 11px; }
        .page-wrap { max-width:1480px; margin:0 auto; padding:28px 24px 52px; }
        .page-title { color:var(--hunt-dark); font-size:33px; font-weight:750; margin:4px 0 20px; }
        .page-subtitle { color:var(--muted); margin:-12px 0 20px; }
        .kpi-card, .panel-card { background:var(--paper); border:1px solid #E7ECEF; border-radius:10px; box-shadow:0 8px 24px rgba(23,74,110,.07); }
        .kpi-card { min-height:135px; margin-bottom:20px; padding:18px; border-top:5px solid var(--hunt-blue); }
        .kpi-norway { border-top-color:var(--norway-red); }
        .kpi-value { color:var(--hunt-dark); font-size:33px; font-weight:800; line-height:1.05; }
        .kpi-norway .kpi-value { color:var(--norway-red); }
        .kpi-label { font-size:14px; font-weight:700; margin-top:9px; line-height:1.3; }
        .kpi-detail { color:var(--muted); font-size:12px; margin-top:4px; }
        .panel-card { margin-bottom:20px; padding:21px 23px; }
        .panel-card h2 { color:var(--hunt-dark); font-size:22px; font-weight:750; margin:0 0 15px; }
        .panel-card h3 { color:var(--hunt-dark); font-size:18px; font-weight:750; margin:0 0 12px; }
        .source-line { color:#6B7B83; font-size:12px; margin:4px 3px 0; }
        .short-note { background:#EDF5F8; border-left:4px solid var(--hunt-light); border-radius:5px; color:#3D5662; font-size:13px; padding:9px 12px; margin:7px 0 14px; }
        .warning-note { background:#FFF4E2; border-left-color:#E8A33C; }
        .metric-row { display:flex; justify-content:space-between; gap:16px; border-top:1px solid var(--line); margin-top:8px; padding-top:11px; font-size:13px; }
        .comparison-table { width:100%; border-collapse:collapse; font-size:13px; }
        .comparison-table th { background:var(--hunt-dark); color:white; text-align:left; }
        .comparison-table th, .comparison-table td { border-bottom:1px solid var(--line); padding:9px 10px; vertical-align:top; }
        .comparison-table tr:nth-child(even) td { background:#F5F8F9; }
        .risk-table { overflow-x:auto; }
        .risk-table table { width:100%; margin:3px 0 0; font-size:12px; }
        .risk-table th, .risk-table td { text-align:center; white-space:nowrap; }
        .risk-table th:first-child, .risk-table td:first-child { text-align:left; }
        details { margin-top:12px; }
        summary { color:var(--hunt-dark); cursor:pointer; font-weight:700; }
        .nav-pills { margin-bottom:16px; }
        .control-label { color:var(--hunt-dark); font-size:13px; font-weight:700; }
        .btn-primary { background:var(--hunt-dark); border-color:var(--hunt-dark); }
        @media(max-width:991px) { .report-strip small { display:none; } }
        @media(max-width:767px) { .page-wrap { padding:20px 12px 38px; } .fullscreen-btn { display:none; } .metric-row { flex-direction:column; } }
      "))
    ),
    div(
      class = "report-strip",
      div(strong("Foreløpige aggregerte resultater")),
      tags$button(
        class = "fullscreen-btn",
        type = "button",
        onclick = "if(document.fullscreenElement){document.exitFullscreen();}else{document.documentElement.requestFullscreen();}",
        "Fullskjerm"
      )
    )
  ),

  tabPanel(
    "Oversikt",
    div(
      class = "page-wrap",
      div(class = "page-title", "Prostatakreft i HUNT3 og Norge"),
      div(
        class = "short-note",
        "HUNT-diagnoser beskrives for 2006-2025."
      ),
      fluidRow(
        column(3, kpi_card("19 402", "menn i HUNT-hovedanalysen", "av 19 802 i HUNT3-utvalget")),
        column(3, kpi_card("1 737", "menn med ny prostatakreft", "etter HUNT3")),
        column(3, kpi_card("17,96 år", "median oppfølging", "til 31.12.2025")),
        column(3, kpi_card("5 350", "nye tilfeller i Norge", "2025", "norway"))
      ),
      fluidRow(
        column(
          8,
          div(
            class = "panel-card",
            tags$h2("Årlige tall"),
            fluidRow(
              column(
                4,
                selectInput(
                  "annual_metric",
                  "Velg mål",
                  choices = c(
                    "Nye diagnoser" = "diagnoses",
                    "Døde av prostatakreft" = "deaths",
                    "Diagnoserate" = "rate"
                  ),
                  selected = "diagnoses"
                )
              ),
              column(8, uiOutput("annual_note"))
            ),
            plotOutput("annual_plot", height = "485px"),
            tags$details(
              tags$summary("Vis årlige tall"),
              tableOutput("annual_table")
            )
          )
        ),
        column(
          4,
          div(
            class = "panel-card",
            tags$h2("Alder ved diagnose"),
            plotOutput("age_plot", height = "345px"),
            uiOutput("age_metric_row")
          ),
          div(
            class = "short-note warning-note",
            ""
          )
        )
      ),
      source_line("Kilder: HUNT release 2026_09; årsrapport 2025, tabell 2.2 og figur 2.1-2.2.")
    )
  ),

  tabPanel(
    "Laboratorie-PSA",
    div(
      class = "page-wrap",
      div(class = "page-title", "PSA i analyserte HUNT3-prøver"),
      div(class = "page-subtitle", "18 723 unike prøver med S-PSA"),
      fluidRow(
        column(
          7,
          div(
            class = "panel-card",
            tags$h2("Andel over kliniske terskler"),
            plotOutput("lab_psa_plot", height = "485px")
          )
        ),
        column(
          5,
          div(
            class = "panel-card",
            tags$h2("Eksakte tall"),
            tableOutput("lab_psa_table"),
            div(
              class = "short-note warning-note",
              "Prøve-PSA og PSA ved kreftdiagnose vises i separate faner."
            )
          )
        )
      ),
      source_line("Kilde: Fürst-resultatfil for HUNT3-prøver; binomiske 95 % konfidensintervall.")
    )
  ),

  tabPanel(
    "PSA ved diagnose",
    div(
      class = "page-wrap",
      div(class = "page-title", "PSA ved prostatakreftdiagnosen"),
      fluidRow(
        column(
          7,
          div(
            class = "panel-card",
            tags$h2("PSA-kategorier"),
            plotOutput("psa_category_plot", height = "490px"),
            uiOutput("psa_metric_row")
          )
        ),
        column(
          5,
          div(
            class = "panel-card",
            tags$h2("Median PSA etter alder"),
            checkboxInput("psa_log", "Logaritmisk y-akse (anbefalt)", value = TRUE),
            plotOutput("psa_age_plot", height = "490px")
          )
        )
      ),
      source_line(paste0(
        "Kilder: ", hunt_profile_source,
        "; årsrapport 2025, figur 2.5-2.6. "
      ))
    )
  ),

  tabPanel(
    "ISUP og T-stadium",
    div(
      class = "page-wrap",
      div(class = "page-title", "Tumorprofil ved diagnose"),
      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            tags$h2("ISUP-grad"),
            plotOutput("isup_plot", height = "540px"),
            uiOutput("isup_metric_row")
          )
        ),
        column(
          6,
          div(
            class = "panel-card",
            tags$h2("T-stadium"),
            radioButtons(
              "stage_method",
              "Velg vurdering",
              choices = c("cT-DRE" = "cT-DRE", "radT" = "radT"),
              selected = "cT-DRE",
              inline = TRUE
            ),
            plotOutput("stage_plot", height = "490px"),
            uiOutput("stage_note")
          )
        )
      ),
      source_line(paste0(
        "Kilder: ", hunt_profile_source,
        "; årsrapport 2025, figur 2.7 og 2.10."
      ))
    )
  ),

  tabPanel(
    "Kliniske undergrupper",
    div(
      class = "page-wrap",
      div(class = "page-title", "Kliniske undergrupper og behandling"),
      uiOutput("clinical_data_status"),
      fluidRow(
        column(
          3,
          div(
            class = "panel-card",
            tags$h2("Vis tall som"),
            radioButtons(
              "clinical_scale",
              NULL,
              choices = c("Andel" = "percent", "Antall" = "count"),
              selected = "percent",
              inline = TRUE
            )
          )
        ),
        column(
          9,
          div(
            class = "panel-card",
            tags$h2("Analyseutvalg"),
            "HUNT: menn diagnostisert i 2021–2024, med behandling registrert til 31.12.2025."
          )
        )
      ),
      tabsetPanel(
        id = "clinical_subtabs",
        type = "pills",
        tabPanel(
          "Etter alder",
          div(
            class = "panel-card",
            tags$h2("Behandlingsvalg etter alder"),
            plotOutput("treatment_age_comparison_plot", height = "720px"),
            div(
              class = "short-note",
              "Aldersgruppene og behandlingskategoriene følger årsrapportens figur 2.13."
            )
          )
        ),
        tabPanel(
          "Etter risikogruppe",
          div(
            class = "panel-card",
            tags$h2("Risikogruppe ved diagnose"),
            plotOutput("risk_distribution_plot", height = "510px"),
            uiOutput("risk_definition_note")
          ),
          div(
            class = "panel-card",
            tags$h2("Behandlingsvalg etter risikogruppe"),
            plotOutput("treatment_risk_comparison_plot", height = "880px"),
            div(
              class = "short-note",
              "Nasjonal referanse følger årsrapportens figur 2.12."
            )
          )
        ),
        tabPanel(
          "Etter T-stadium",
          div(
            class = "panel-card",
            tags$h2("Behandlingsvalg etter tumorutbredelse"),
            radioButtons(
              "stage_treatment_method",
              "Velg stadium",
              choices = c("cT-DRE" = "cT-DRE", "radT" = "radT"),
              selected = "cT-DRE",
              inline = TRUE
            ),
            plotOutput("treatment_stage_plot", height = "720px"),
            div(
              class = "short-note",
              "T0-2, T3, T4 og TX vises separat. Hvert stadium har egen nevner."
            )
          )
        ),
        tabPanel(
          "Kun opererte",
          div(
            class = "panel-card",
            tags$h2("Risikogruppe blant opererte"),
            plotOutput("operated_risk_plot", height = "560px")
          )
        )
      ),
      source_line("Kilder: HUNT individdata; årsrapport 2025, figur 2.11-2.13.")
    )
  ),

  tabPanel(
    "Overlevelse",
    div(
      class = "page-wrap",
      div(class = "page-title", "Overlevelse og dødsårsaker"),
      div(
        class = "short-note",
        "Begge figurene omfatter menn diagnostisert i 2006-2020 og fulgt til 31.12.2025."
      ),
      uiOutput("survival_status"),
      fluidRow(
        column(
          7,
          div(
            class = "panel-card",
            tags$h2("Observert femårsoverlevelse etter alder ved diagnose"),
            plotOutput("observed_survival_plot", height = "390px"),
            tags$h3("Antall i risiko"),
            div(class = "risk-table", tableOutput("survival_risk_table"))
          )
        ),
        column(
          5,
          div(
            class = "panel-card",
            tags$h2("Femårssannsynlighet for død etter registrert dødsårsak"),
            plotOutput("mortality_plot", height = "505px")
          )
        )
      ),
      source_line("Kilde: HUNT overlevelsesanalyser.")
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  output$survival_status <- renderUI({
    if (all(is.finite(observed_survival$estimate)) &&
        all(is.finite(competing_mortality$estimate))) {
      return(NULL)
    }
    div(class = "short-note warning-note", survival_status_message)
  })

  output$annual_note <- renderUI({
    if (identical(input$annual_metric, "rate")) {
      div(
        class = "short-note warning-note",
        "HUNT ", hunt_annual_period,
        ": ustandardisert rate. Norge 2016-2025: aldersstandardisert rate."
      )
    } else {
      div(
        class = "short-note",
        "HUNT vises for ", hunt_annual_period,
        "; nasjonale årlige tall er tilgjengelige her for 2016-2025."
      )
    }
  })

  output$annual_plot <- renderPlot({
    if (identical(input$annual_metric, "diagnoses")) {
      plot_data <- rbind(
        data.frame(year = annual_hunt_stats$year, value = annual_hunt_stats$hunt_new_diagnoses, source = "HUNT3-hovedkohorten"),
        data.frame(year = annual_stats$year, value = annual_stats$norway_new_cases, source = "Norge")
      )
      y_label <- "Nye diagnoser"
      plot_title <- "Nye prostatakreftdiagnoser"
    } else if (identical(input$annual_metric, "deaths")) {
      plot_data <- rbind(
        data.frame(year = annual_hunt_stats$year, value = annual_hunt_stats$hunt_pc_deaths, source = "HUNT3-hovedkohorten"),
        data.frame(year = annual_stats$year, value = annual_stats$norway_pc_deaths, source = "Norge")
      )
      y_label <- "Døde av prostatakreft"
      plot_title <- "Registrerte prostatakreftdødsfall"
    } else {
      plot_data <- rbind(
        data.frame(year = annual_hunt_stats$year, value = annual_hunt_stats$hunt_incidence_rate, source = "HUNT: ustandardisert"),
        data.frame(year = annual_stats$year, value = annual_stats$norway_incidence_rate, source = "Norge: aldersstandardisert")
      )
      y_label <- "Rate per 100 000"
      plot_title <- "Diagnoserater"
    }

    plot_data <- plot_data[is.finite(plot_data$value), , drop = FALSE]
    latest <- do.call(rbind, lapply(split(plot_data, plot_data$source), function(d) {
      d[d$year == max(d$year), , drop = FALSE][1, , drop = FALSE]
    }))
    first_year <- min(plot_data$year)
    last_year <- max(plot_data$year)
    year_breaks <- sort(unique(c(seq(first_year, last_year, by = 2L), last_year)))

    ggplot(plot_data, aes(x = year, y = value, colour = source, group = source)) +
      geom_line(linewidth = 1.25) +
      geom_point(size = 2.8) +
      geom_label(
        data = latest,
        aes(label = fmt_n(value)),
        hjust = 1.05,
        vjust = -0.4,
        size = 4,
        fontface = "bold",
        linewidth = 0,
        fill = "white",
        show.legend = FALSE
      ) +
      facet_wrap(~source, ncol = 1, scales = "free_y") +
      scale_colour_manual(values = c(
        "HUNT3-hovedkohorten" = COL_HUNT,
        "Norge" = COL_NORWAY,
        "HUNT: ustandardisert" = COL_HUNT,
        "Norge: aldersstandardisert" = COL_NORWAY
      )) +
      scale_x_continuous(breaks = year_breaks) +
      labs(title = plot_title, x = "År", y = y_label) +
      theme_clinical(14) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }, res = 110)

  output$annual_table <- renderTable({
    national_index <- match(annual_hunt_stats$year, annual_stats$year)
    if (identical(input$annual_metric, "diagnoses")) {
      data.frame(
        År = annual_hunt_stats$year,
        `HUNT diagnoser` = fmt_n_blank(annual_hunt_stats$hunt_new_diagnoses),
        `Norge diagnoser` = fmt_n_blank(annual_stats$norway_new_cases[national_index]),
        check.names = FALSE
      )
    } else if (identical(input$annual_metric, "deaths")) {
      data.frame(
        År = annual_hunt_stats$year,
        `HUNT dødsfall` = fmt_n_blank(annual_hunt_stats$hunt_pc_deaths),
        `Norge dødsfall` = fmt_n_blank(annual_stats$norway_pc_deaths[national_index]),
        check.names = FALSE
      )
    } else {
      data.frame(
        År = annual_hunt_stats$year,
        `HUNT rate` = fmt_1_blank(annual_hunt_stats$hunt_incidence_rate),
        `Norge rate` = fmt_1_blank(annual_stats$norway_incidence_rate[national_index]),
        check.names = FALSE
      )
    }
  }, striped = TRUE, bordered = FALSE, spacing = "s", width = "100%", rownames = FALSE)

  output$age_plot <- renderPlot({
    plot_data <- diagnosis_age
    plot_data$label_text <- paste0(plot_data$source, "\n", plot_data$period)
    plot_data$label <- factor(plot_data$label_text, levels = plot_data$label_text)

    ggplot(plot_data, aes(x = label, y = median_age, fill = source)) +
      geom_col(width = 0.66) +
      geom_text(
        aes(label = paste0(fmt_1(median_age), " år")),
        vjust = -0.5,
        size = 4.5,
        fontface = "bold"
      ) +
      scale_fill_manual(values = c("HUNT" = COL_HUNT, "Norge" = COL_NORWAY)) +
      scale_y_continuous(limits = c(0, 82), expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = "Median alder (år)") +
      theme_clinical(13) +
      theme(legend.position = "none")
  }, res = 110)

  output$age_metric_row <- renderUI({
    hunt_age <- diagnosis_age[
      diagnosis_age$source == "HUNT" & diagnosis_age$period == "2006-2025",
      , drop = FALSE
    ]
    if (nrow(hunt_age) == 0L) {
      hunt_age <- diagnosis_age[diagnosis_age$source == "HUNT", , drop = FALSE][1, , drop = FALSE]
    }
    norway_age <- diagnosis_age[diagnosis_age$source == "Norge", , drop = FALSE][1, , drop = FALSE]
    div(
      class = "metric-row",
      div(
        strong(paste("HUNT", hunt_age$period)), br(),
        fmt_pct1(hunt_age$pct_under_70), " under 70 år; n=", fmt_n(hunt_age$n)
      ),
      div(
        strong(paste("Norge", norway_age$period)), br(),
        fmt_pct1(norway_age$pct_under_70), " under 70 år"
      )
    )
  })

  output$lab_psa_plot <- renderPlot({
    ggplot(lab_psa_thresholds, aes(x = threshold, y = percent)) +
      geom_col(width = 0.62, fill = COL_HUNT) +
      geom_errorbar(
        aes(ymin = ci_low, ymax = ci_high),
        width = 0.13,
        linewidth = 0.9,
        colour = COL_HUNT_DARK
      ) +
      geom_text(
        aes(label = paste0(fmt_pct2(percent), "\n(n=", fmt_n(count), ")")),
        vjust = -0.8,
        size = 5,
        fontface = "bold",
        lineheight = 0.95
      ) +
      scale_y_continuous(
        limits = c(0, 16.5),
        breaks = seq(0, 15, 3),
        expand = expansion(mult = c(0, 0))
      ) +
      labs(x = NULL, y = "Andel av prøver (%)", caption = "Feilstolper: 95 % KI") +
      theme_clinical(15) +
      theme(plot.margin = margin(18, 24, 18, 12))
  }, res = 110)

  output$lab_psa_table <- renderTable({
    data.frame(
      Terskel = as.character(lab_psa_thresholds$threshold),
      Antall = fmt_n(lab_psa_thresholds$count),
      N = fmt_n(lab_psa_thresholds$n),
      Andel = fmt_pct2(lab_psa_thresholds$percent),
      `95 % KI` = paste0(
        fmt_pct2(lab_psa_thresholds$ci_low),
        " - ",
        fmt_pct2(lab_psa_thresholds$ci_high)
      ),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "m", width = "100%", rownames = FALSE)

  output$psa_category_plot <- renderPlot({
    plot_data <- psa_categories
    plot_data$category <- factor(
      plot_data$category,
      levels = c("<4", "4-9,9", "10-19,9", "20-99,9", "≥100")
    )
    plot_data$bar_label <- ifelse(
      plot_data$source == hunt_profile_source,
      paste0(fmt_pct1(plot_data$percent), "\n(n=", fmt_n(plot_data$count), ")"),
      fmt_pct1(plot_data$percent)
    )

    ggplot(plot_data, aes(x = category, y = percent, fill = source)) +
      geom_col(position = position_dodge(width = 0.82), width = 0.74) +
      geom_text(
        aes(label = bar_label),
        position = position_dodge(width = 0.82),
        vjust = -0.35,
        size = 4,
        fontface = "bold",
        lineheight = 0.9
      ) +
      scale_fill_manual(values = setNames(c(COL_HUNT, COL_NORWAY), c(hunt_profile_source, "Norge 2025"))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
      labs(x = "PSA ved diagnose (ng/mL)", y = "Andel med registrert PSA (%)") +
      theme_clinical(14)
  }, res = 110)

  output$psa_metric_row <- renderUI({
    hunt_psa <- psa_categories[psa_categories$source == hunt_profile_source, , drop = FALSE]
    norway_psa <- psa_categories[psa_categories$source == "Norge 2025", , drop = FALSE]
    hunt_n <- if (nrow(hunt_psa) > 0L) unique(hunt_psa$denominator)[1] else NA_real_
    hunt_total <- if (!is.null(hunt_descriptive_summary)) {
      hunt_descriptive_summary$n_cases
    } else {
      518
    }
    div(
      class = "metric-row",
      div(
        strong(hunt_profile_source), br(),
        fmt_n(hunt_n), "/", fmt_n(hunt_total), " registrert (",
        fmt_pct1(100 * hunt_n / hunt_total), ")"
      ),
      div(
        strong("Norge 2025"), br(),
        fmt_n(unique(norway_psa$denominator)[1]), "/5 200 registrert (89,0 %)"
      )
    )
  })

  output$psa_age_plot <- renderPlot({
    plot_data <- rbind(
      data.frame(
        age_group = psa_by_age$age_group,
        value = psa_by_age$hunt_median,
        q1 = psa_by_age$hunt_q1,
        q3 = psa_by_age$hunt_q3,
        n = psa_by_age$hunt_n,
        source = "HUNT 2006-2025"
      ),
      data.frame(
        age_group = psa_by_age$age_group,
        value = psa_by_age$norway_median_approx,
        q1 = NA_real_,
        q3 = NA_real_,
        n = NA_real_,
        source = "Norge 2025"
      )
    )

    plot_data$x_position <- as.numeric(plot_data$age_group) +
      ifelse(plot_data$source == "HUNT 2006-2025", -0.10, 0.10)

    hunt_points <- plot_data[plot_data$source == "HUNT 2006-2025", , drop = FALSE]
    norway_points <- plot_data[plot_data$source == "Norge 2025", , drop = FALSE]

    p <- ggplot(plot_data, aes(x = x_position, y = value, colour = source, group = source)) +
      geom_errorbar(
        data = hunt_points,
        aes(ymin = q1, ymax = q3),
        width = 0.12,
        linewidth = 0.9,
        colour = COL_HUNT,
        alpha = 0.65
      ) +
      geom_line(linewidth = 1.05) +
      geom_point(size = 3.2) +
      geom_label(
        data = hunt_points,
        aes(label = paste0(fmt_1(value), "\n(n=", fmt_n(n), ")")),
        vjust = -0.65,
        size = 3.8,
        fontface = "bold",
        fill = "white",
        linewidth = 0,
        label.padding = grid::unit(0.10, "lines"),
        show.legend = FALSE
      ) +
      geom_label(
        data = norway_points,
        aes(label = fmt_1(value)),
        vjust = 1.65,
        size = 3.8,
        fontface = "bold",
        fill = "white",
        linewidth = 0,
        label.padding = grid::unit(0.10, "lines"),
        show.legend = FALSE
      ) +
      scale_colour_manual(values = c("HUNT 2006-2025" = COL_HUNT, "Norge 2025" = COL_NORWAY)) +
      scale_x_continuous(
        breaks = seq_along(levels(psa_by_age$age_group)),
        labels = levels(psa_by_age$age_group),
        limits = c(0.55, 5.45)
      ) +
      labs(x = "Alder ved diagnose", y = "Median PSA (ng/mL)") +
      theme_clinical(13) +
      theme(plot.margin = margin(12, 18, 18, 18))

    if (isTRUE(input$psa_log)) {
      p <- p + scale_y_log10(
        breaks = c(5, 10, 20, 50, 100, 200),
        expand = expansion(mult = c(0.10, 0.22))
      )
    } else {
      p <- p + scale_y_continuous(expand = expansion(mult = c(0, 0.22)))
    }
    p
  }, res = 110)

  output$isup_plot <- renderPlot({
    plot_data <- isup_data
    plot_data$isup <- factor(plot_data$isup, levels = paste("ISUP", 1:5))
    plot_data$bar_label <- ifelse(
      plot_data$source == hunt_profile_source,
      paste0(fmt_pct1(plot_data$percent), " (n=", fmt_n(plot_data$count), ")"),
      fmt_pct1(plot_data$percent)
    )

    ggplot(plot_data, aes(x = isup, y = percent, fill = source)) +
      geom_col(position = position_dodge(width = 0.78), width = 0.68) +
      geom_text(
        aes(label = bar_label),
        position = position_dodge(width = 0.78),
        hjust = -0.06,
        size = 3.8,
        fontface = "bold",
        show.legend = FALSE
      ) +
      scale_fill_manual(values = setNames(c(COL_HUNT, COL_NORWAY), c(hunt_profile_source, "Norge 2025"))) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.30))
      ) +
      labs(x = NULL, y = "Andel blant registrerte ISUP-verdier (%)") +
      coord_flip(clip = "off") +
      theme_clinical(14) +
      theme(
        legend.position = "top",
        plot.margin = margin(12, 105, 12, 12)
      )
  }, res = 110)

  output$isup_metric_row <- renderUI({
    hunt_isup <- isup_data[isup_data$source == hunt_profile_source, , drop = FALSE]
    norway_isup <- isup_data[isup_data$source == "Norge 2025", , drop = FALSE]
    hunt_n <- unique(hunt_isup$denominator)[1]
    hunt_total <- if (!is.null(hunt_descriptive_summary)) {
      hunt_descriptive_summary$n_cases
    } else {
      518
    }
    div(
      class = "metric-row",
      div(
        strong(hunt_profile_source), br(),
        fmt_n(hunt_n), "/", fmt_n(hunt_total), " registrert (",
        fmt_pct1(100 * hunt_n / hunt_total), ")"
      ),
      div(
        strong("Norge 2025"), br(),
        fmt_n(unique(norway_isup$denominator)[1]), "/5 200 registrert (95,0 %)"
      )
    )
  })

  output$stage_plot <- renderPlot({
    plot_data <- t_stage_data[t_stage_data$method == input$stage_method, , drop = FALSE]
    plot_data$category <- factor(plot_data$category, levels = c("T0-2", "T3", "T4", "TX"))
    plot_data$bar_label <- ifelse(
      plot_data$source == hunt_profile_source,
      paste0(fmt_pct1(plot_data$percent), " (n=", fmt_n(plot_data$count), ")"),
      fmt_pct1(plot_data$percent)
    )

    ggplot(plot_data, aes(x = category, y = percent, fill = source)) +
      geom_col(position = position_dodge(width = 0.78), width = 0.68) +
      geom_text(
        aes(label = bar_label),
        position = position_dodge(width = 0.78),
        hjust = -0.06,
        size = 3.8,
        fontface = "bold",
        show.legend = FALSE
      ) +
      scale_fill_manual(values = setNames(c(COL_HUNT, COL_NORWAY), c(hunt_profile_source, "Norge 2025"))) +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20),
        expand = expansion(mult = c(0, 0))
      ) +
      labs(x = "Tumorutbredelse", y = "Andel (%)") +
      coord_flip(clip = "off") +
      theme_clinical(14) +
      theme(
        legend.position = "top",
        plot.margin = margin(12, 105, 12, 12)
      )
  }, res = 110)

  output$stage_note <- renderUI({
    if (!is.null(hunt_descriptive_summary)) {
      selected_n <- if (identical(input$stage_method, "cT-DRE")) {
        hunt_descriptive_summary$n_ct_dre
      } else {
        hunt_descriptive_summary$n_rad_t
      }
      national_coverage <- if (identical(input$stage_method, "cT-DRE")) "97 %" else "82 %"
      div(
        class = "short-note",
        HUNT_DESCRIPTION_LABEL, ": ", fmt_n(selected_n), "/",
        fmt_n(hunt_descriptive_summary$n_cases), " har registrert ", input$stage_method,
        "; ", fmt_n(hunt_descriptive_summary$n_both_stage), " har både cT-DRE og radT. ",
        "Norge 2025: dekning ", national_coverage, "."
      )
    } else if (identical(input$stage_method, "cT-DRE")) {
      div(class = "short-note", "HUNT 2021-2025: 350 menn med både cT-DRE og radT. Norge 2025: cT-DRE-dekning 97 %.")
    } else {
      div(class = "short-note", "HUNT 2021-2025: 350 menn med både cT-DRE og radT. Norge 2025: radT-dekning 82 %.")
    }
  })

  output$clinical_data_status <- renderUI({
    if (!isTRUE(hunt_clinical$ok)) {
      return(div(
        class = "short-note warning-note",
        tags$strong("HUNT-individdata ble ikke lastet: "),
        hunt_clinical$message,
        " Aldersfiguren bruker innebygde HUNT-tall; øvrige undergrupper krever individfilene."
      ))
    }
    div(
      class = "short-note",
      tags$strong("HUNT-individdata lastet: "),
      fmt_n(hunt_clinical_summary$n_analysis), " diagnoser i 2021-2024; ",
      fmt_n(hunt_clinical_summary$n_risk_known), " kan risikogrupperes; ",
      fmt_n(hunt_clinical_summary$n_management), " inngår i behandlingsanalysen."
    )
  })

  output$risk_definition_note <- renderUI({
    if (!isTRUE(hunt_clinical$ok)) return(NULL)
    div(
      class = "short-note",
      tags$strong("HUNT-risikogruppe: "),
      hunt_clinical$risk_method,
      ". Norge 2025 følger årsrapportens figur 2.11."
    )
  })

  output$treatment_age_comparison_plot <- renderPlot({
    hunt_age <- if (!is.null(hunt_clinical_summary)) {
      hunt_clinical_summary$treatment_age
    } else {
      fallback <- treatment_hunt
      fallback$treatment_group <- ifelse(
        fallback$treatment %in% c(
          "Flere radikale behandlinger", "Ingen vist behandling registrert"
        ),
        "Annet/ingen vist",
        fallback$treatment
      )
      counts <- aggregate(
        fallback$count,
        by = list(stratum = fallback$age_group, treatment = fallback$treatment_group),
        FUN = sum
      )
      names(counts)[3] <- "count"
      counts$total <- ave(counts$count, counts$stratum, FUN = sum)
      counts$percent <- 100 * counts$count / counts$total
      counts
    }
    plot_treatment_comparison(
      hunt_age,
      national_treatment_age,
      input$clinical_scale,
      facet_ncol = 2
    )
  }, res = 110)

  output$risk_distribution_plot <- renderPlot({
    hunt_risk <- if (!is.null(hunt_clinical_summary)) {
      hunt_clinical_summary$risk_distribution
    } else {
      NULL
    }
    plot_distribution_comparison(
      hunt_risk,
      national_risk_distribution,
      input$clinical_scale
    )
  }, res = 110)

  output$treatment_risk_comparison_plot <- renderPlot({
    hunt_risk_treatment <- if (!is.null(hunt_clinical_summary)) {
      hunt_clinical_summary$treatment_risk
    } else {
      NULL
    }
    plot_treatment_comparison(
      hunt_risk_treatment,
      national_treatment_risk,
      input$clinical_scale,
      facet_ncol = 2
    )
  }, res = 110)

  output$treatment_stage_plot <- renderPlot({
    hunt_stage_treatment <- if (is.null(hunt_clinical_summary)) {
      NULL
    } else if (identical(input$stage_treatment_method, "radT")) {
      hunt_clinical_summary$treatment_radt
    } else {
      hunt_clinical_summary$treatment_ct
    }
    plot_treatment_stage(hunt_stage_treatment, input$clinical_scale)
  }, res = 110)

  output$operated_risk_plot <- renderPlot({
    hunt_operated_risk <- if (!is.null(hunt_clinical_summary)) {
      hunt_clinical_summary$operated_risk
    } else {
      NULL
    }
    plot_distribution_comparison(
      hunt_operated_risk,
      national_operated_risk,
      input$clinical_scale
    )
  }, res = 110)

  output$observed_survival_plot <- renderPlot({
    if (!all(is.finite(observed_survival$estimate)) ||
        !all(is.finite(observed_survival$lower)) ||
        !all(is.finite(observed_survival$upper))) {
      return(empty_clinical_plot(
        "Femårsoverlevelse kunne ikke beregnes fra tilgjengelige HUNT-data"
      ))
    }
    ggplot(observed_survival, aes(x = group, y = estimate, fill = group)) +
      geom_col(width = 0.62) +
      geom_errorbar(
        aes(ymin = lower, ymax = upper),
        width = 0.16,
        linewidth = 0.9
      ) +
      geom_text(
        aes(
          y = 100,
          label = paste0(fmt_pct1(estimate), "\n95 % KI ", fmt_1(lower), "-", fmt_1(upper))
        ),
        hjust = 0,
        size = 4.0,
        fontface = "bold",
        lineheight = 0.95
      ) +
      scale_fill_manual(values = c(COL_HUNT_LIGHT, COL_HUNT, COL_NORWAY)) +
      scale_x_discrete(limits = rev(levels(observed_survival$group))) +
      scale_y_continuous(
        limits = c(0, 138),
        breaks = seq(0, 100, 20),
        expand = expansion(mult = c(0, 0))
      ) +
      coord_flip(clip = "off") +
      labs(
        subtitle = paste0("Diagnoser ", SURVIVAL_PERIOD_LABEL),
        x = NULL,
        y = "Andel i live etter fem år (%)"
      ) +
      theme_clinical(14) +
      theme(
        legend.position = "none",
        plot.margin = margin(12, 58, 12, 12)
      )
  }, res = 110)

  output$survival_risk_table <- renderTable({
    if (!all(is.finite(observed_survival$estimate))) {
      return(data.frame(
        Status = "Antall i risiko beregnes når HUNT-dødsårsaksdata er tilgjengelige."
      ))
    }
    data.frame(
      Aldersgruppe = survival_at_risk$group,
      `0 år` = fmt_n(survival_at_risk$year_0),
      `1 år` = fmt_n(survival_at_risk$year_1),
      `2 år` = fmt_n(survival_at_risk$year_2),
      `3 år` = fmt_n(survival_at_risk$year_3),
      `4 år` = fmt_n(survival_at_risk$year_4),
      `5 år` = fmt_n(survival_at_risk$year_5),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "xs", width = "100%", rownames = FALSE)

  output$mortality_plot <- renderPlot({
    if (!all(is.finite(competing_mortality$estimate))) {
      return(empty_clinical_plot(
        "Dødsårsakssannsynlighet kunne ikke beregnes fra tilgjengelige HUNT-data"
      ))
    }
    mortality_y_max <- max(
      4,
      ceiling(max(competing_mortality$estimate) * 1.28 / 2) * 2
    )
    ggplot(competing_mortality, aes(x = cause, y = estimate, fill = cause)) +
      geom_col(width = 0.62) +
      geom_text(
        aes(label = fmt_pct1(estimate)),
        hjust = -0.08,
        size = 4.8,
        fontface = "bold"
      ) +
      scale_fill_manual(values = c(COL_NORWAY, COL_PURPLE, COL_ORANGE, COL_GREY)) +
      scale_x_discrete(limits = rev(levels(competing_mortality$cause))) +
      scale_y_continuous(
        limits = c(0, mortality_y_max),
        breaks = pretty(c(0, mortality_y_max), n = 6),
        expand = expansion(mult = c(0, 0))
      ) +
      coord_flip(clip = "off") +
      labs(
        subtitle = paste0(
          "Diagnoser ", SURVIVAL_PERIOD_LABEL,
          "; Aalen-Johansen; n=", fmt_n(competing_mortality_n)
        ),
        x = NULL,
        y = "Estimert sannsynlighet innen fem år (%)"
      ) +
      theme_clinical(14) +
      theme(
        legend.position = "none",
        plot.margin = margin(12, 55, 12, 12)
      )
  }, res = 110)

}

shinyApp(ui = ui, server = server)
