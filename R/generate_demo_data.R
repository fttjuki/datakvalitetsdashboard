generate_demo_data <- function(n = 6000, seed = 20260914) {
  set.seed(seed)

  hospital_levels <- c(
    "Fjordvik universitetssykehus",
    "Nordbyen sykehus",
    "Innlandet klinikk",
    "Kysten helseforetak",
    "Sørdal sykehus"
  )

  hospital <- sample(
    hospital_levels,
    n,
    replace = TRUE,
    prob = c(0.27, 0.23, 0.20, 0.17, 0.13)
  )

  age <- sample(45:84, n, replace = TRUE)
  sample_date <- as.Date("2025-01-01") + sample(0:364, n, replace = TRUE)
  transport_days <- pmax(0L, round(rlnorm(n, log(1.2), 0.65)))
  received_date <- sample_date + transport_days

  psa <- exp(
    rnorm(
      n,
      mean = log(0.75) + 0.035 * (age - 55),
      sd = 0.88
    )
  )
  psa <- pmin(psa, 120)

  testosterone <- pmax(
    0.3,
    rnorm(n, mean = 17 - 0.12 * (age - 55), sd = 4.2)
  )
  shbg <- pmax(
    6,
    rlnorm(
      n,
      meanlog = log(38 + 0.30 * (age - 55)),
      sdlog = 0.35
    )
  )
  fti <- 10 * testosterone / shbg + rnorm(n, 0, 0.03)

  psa_unit <- sample(
    c("ug/L", "ng/mL"),
    n,
    replace = TRUE,
    prob = c(0.82, 0.18)
  )
  orig_psa <- format(round(psa, 3), trim = TRUE, scientific = FALSE)

  # Bevisst innførte kvalitetsavvik. Alt er fiktivt.
  missing_prob <- c(
    "Fjordvik universitetssykehus" = 0.004,
    "Nordbyen sykehus" = 0.009,
    "Innlandet klinikk" = 0.015,
    "Kysten helseforetak" = 0.025,
    "Sørdal sykehus" = 0.040
  )

  miss_psa <- runif(n) < unname(missing_prob[hospital])
  miss_t <- runif(n) < (unname(missing_prob[hospital]) + 0.010)
  miss_shbg <- runif(n) < (unname(missing_prob[hospital]) + 0.008)

  psa[miss_psa] <- NA_real_
  orig_psa[miss_psa] <- NA_character_
  testosterone[miss_t] <- NA_real_
  shbg[miss_shbg] <- NA_real_
  fti[is.na(testosterone) | is.na(shbg)] <- NA_real_

  zero_idx <- sample(which(!is.na(psa)), 18)
  psa[zero_idx] <- 0
  orig_psa[zero_idx] <- "0"

  high_idx <- sample(setdiff(which(!is.na(psa)), zero_idx), 3)
  psa[high_idx] <- 1000
  orig_psa[high_idx] <- ">1000"

  li_idx <- sample(
    setdiff(which(!is.na(psa)), c(zero_idx, high_idx)),
    12
  )
  orig_psa[li_idx] <- "LI"

  wrong_unit_idx <- sample(seq_len(n), 24)
  psa_unit[wrong_unit_idx] <- "nmol/L"

  fti_error_pool <- which(
    !is.na(testosterone) & !is.na(shbg) & !is.na(fti)
  )
  fti_error_idx <- sample(fti_error_pool, 30)
  fti[fti_error_idx] <- 100 * testosterone[fti_error_idx] /
    shbg[fti_error_idx]

  dat <- data.frame(
    record_id = sprintf("R%06d", seq_len(n)),
    sample_id = sprintf("S%06d", seq_len(n)),
    hospital = hospital,
    sample_date = sample_date,
    received_date = received_date,
    age = age,
    psa = round(psa, 3),
    psa_unit = psa_unit,
    orig_psa = orig_psa,
    testosterone = round(testosterone, 2),
    shbg = round(shbg, 2),
    fti = round(fti, 2),
    stringsAsFactors = FALSE
  )

  duplicate_idx <- sample(seq_len(n), 55)
  duplicate_rows <- dat[duplicate_idx, ]
  duplicate_rows$record_id <- sprintf(
    "R%06d",
    n + seq_len(nrow(duplicate_rows))
  )

  dat <- rbind(dat, duplicate_rows)
  dat <- dat[sample(seq_len(nrow(dat))), ]
  rownames(dat) <- NULL

  add_quality_flags(dat)
}

add_quality_flags <- function(dat) {
  dat$sample_date <- as.Date(dat$sample_date)
  dat$received_date <- as.Date(dat$received_date)

  dat$expected_fti <- with(
    dat,
    ifelse(
      is.na(testosterone) | is.na(shbg) | shbg == 0,
      NA_real_,
      10 * testosterone / shbg
    )
  )

  dat$flag_duplicate <- duplicated(dat$sample_id) |
    duplicated(dat$sample_id, fromLast = TRUE)
  dat$flag_missing <- is.na(dat$psa) |
    is.na(dat$testosterone) |
    is.na(dat$shbg)
  dat$flag_unit <- !dat$psa_unit %in% c("ug/L", "µg/L", "ng/mL")
  dat$flag_zero <- !is.na(dat$psa) & dat$psa == 0
  dat$flag_high <- (!is.na(dat$psa) & dat$psa >= 1000) |
    (!is.na(dat$orig_psa) & grepl("^>", dat$orig_psa))
  dat$flag_li <- !is.na(dat$orig_psa) &
    toupper(trimws(dat$orig_psa)) == "LI"
  dat$flag_fti <- !is.na(dat$fti) &
    !is.na(dat$expected_fti) &
    abs(dat$fti - dat$expected_fti) > 0.20
  dat$flag_delay <- as.integer(
    dat$received_date - dat$sample_date
  ) > 3

  dat$avvik_any <- with(
    dat,
    flag_duplicate |
      flag_missing |
      flag_unit |
      flag_zero |
      flag_high |
      flag_li |
      flag_fti |
      flag_delay
  )

  dat$avvikstype <- apply(
    dat[
      c(
        "flag_duplicate",
        "flag_missing",
        "flag_unit",
        "flag_zero",
        "flag_high",
        "flag_li",
        "flag_fti",
        "flag_delay"
      )
    ],
    1,
    function(z) {
      labels <- c(
        "Duplikat",
        "Manglende verdi",
        "Ugyldig enhet",
        "PSA = 0",
        "PSA >1000",
        "Kode LI",
        "Avvik i FTI",
        "Forsinket mottak"
      )
      selected <- labels[as.logical(z)]
      if (length(selected) == 0) {
        "Ingen"
      } else {
        paste(selected, collapse = "; ")
      }
    }
  )

  dat
}
