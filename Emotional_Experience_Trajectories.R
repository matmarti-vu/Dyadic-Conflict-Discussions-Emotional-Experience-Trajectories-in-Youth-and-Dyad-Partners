################################################################################
# Emotional Experience Trajectories: manuscript and supplement analyses
# Final GitHub reproducibility version — 2026-09-06
#
# PURPOSE
# -------
# This script reproduces the manuscript and supplemental analyses for:
#   "Dyadic Conflict Discussions: Emotional Experience Trajectories in Youth
#    and Dyad Partners."
#
# It is designed for external researchers to be able to follow the analysis
# from data checks through each manuscript-facing table and figure. Repeated
# operations (centering, model fitting, formatting, plotting, and export) are
# centralized in helper functions so that the exhibit-specific sections remain
# comparatively short and auditable.
#
# REQUIRED INPUT OBJECTS
# ----------------------
# The secure/private data-preparation workflow must create these objects before
# this script is sourced:
#   df_complete : source for descriptive statistics and trajectory figures
#   df_long     : balanced second-level analytic sample for the primary models
#   df_longALT  : all-available-data sample for Table A1
#
# The public repository should NOT contain protected participant-level data.
#
# FINAL EXHIBIT MAP
# -----------------
# Main manuscript
#   Table 1   Sample characteristics
#   Table 2   Pooled linear mixed-effects models (3 specifications)
#   Figure 1  Linear and cross-validated LOESS trajectories
#
# Supplement
#   Table A1  All-available-data sensitivity analysis
#   Table A2  Excluding CHR youth taking antipsychotic medication
#   Table A3  Biological-parent dyads only
#   Table A4  Non-CHR biological-parent vs CHR non-biological-partner dyads
#   Figure A1 CHR trajectories by dyad-partner relationship
#   Table A5  Separate Non-CHR and CHR linear mixed-effects models
#   Table A6  Average rating-dial differences across participant groups
#   Figure A2 Estimated marginal means
#   Table A7  Pairwise differences in estimated marginal means
#   Figure A3 Estimated marginal slopes
#   Table A8  Pairwise differences in estimated marginal slopes
#   Figure A4 Non-CHR LOESS span sensitivity (.25/.50/.75)
#   Figure A5 CHR LOESS span sensitivity (.25/.50/.75)
#   Table A9  GAMMs with group-specific thin-plate smooths and AR(1)
#   Figure A6 Youth-covariate-adjusted GAMM trajectories
#   Figure A7 Dyad-partner-covariate-adjusted GAMM trajectories
#   Table A10 GAMM spline-basis/dimension sensitivity: youth covariates
#   Table A11 GAMM spline-basis/dimension sensitivity: dyad-partner covariates
#
# FINAL MODELING CONVENTIONS
# --------------------------
# * Internal group coding from a prior version is retained for reproducibility,  
#   but reader-facing output uses "Non-CHR" and "dyad partner."
# * Primary linear trajectory models use three specifications:
#     (1) no demographic covariates;
#     (2) mean-centered youth age/sex + age×time and sex×time;
#     (3) mean-centered dyad-partner age/sex + age×time and sex×time.
#   Youth and dyad-partner demographics are modeled separately to reduce the
#   risk of collinearity and overfitting in this modest dyad sample.
# * Mean-level models (Table A6/Figure A2/Table A7) include age/sex main effects
#   only because their estimand is the average rating level rather than change.
# * GAMMs use group-specific smooths of time. Their adjusted specifications add
#   centered age/sex main effects; demographic×time terms are not added to the
#   GAMMs because temporal form is represented by the smooth functions.
# * LOESS curves are descriptive. The GAMMs provide the formal follow-up test of
#   whether trajectories support meaningful nonlinear curvature.
# * Table 1 descriptive slope variables (s10m_Rself_y / s10m_Rself_c) represent
#   participant-specific linear change across the FULL 10-minute conversation;
#   they are not the per-minute coefficients reported in Table 2.
#
# OUTPUT
# ------
# All generated files are written to ./R-script/ under the project root.
################################################################################

required_packages <- c(
  "caret", "cowplot", "dplyr", "emmeans", "gam", "ggplot2", "lme4",
  "lmerTest", "mgcv", "nlme", "openxlsx", "purrr", "rlang", "stringr",
  "tibble", "tidyr"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_packages)) {
  stop("Install the following packages before running the analysis: ", paste(missing_packages, collapse = ", "))
}
if (!all(c("df_complete", "df_long", "df_longALT") %in% ls(envir = .GlobalEnv))) {
  stop("Required objects not found. Create or load df_complete, df_long, and df_longALT before running this script.")
}

# ------------------------------------------------------------------------------
# USER INPUT REQUIRED FOR TABLE A2
#
# Replace the two placeholder values below with the dyad IDs for the two CHR
# participants taking antipsychotic medication before running the script.
#
# IMPORTANT FOR PUBLIC GITHUB REPOSITORIES:
# If these IDs are considered sensitive, keep the placeholders in the public
# version of the script and enter the real values only in your local copy.
# ------------------------------------------------------------------------------
ID1 <- "REPLACE_WITH_DYAD_ID_1"
ID2 <- "REPLACE_WITH_DYAD_ID_2"

results_dir <- "Replace Path to Folder 'R-script'"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
emmeans::emm_options(lmer.df = "asymptotic")

# Consistent manuscript-facing participant-group order used wherever a four-group
# display is possible: Non-CHR dyad partner, Non-CHR youth, CHR dyad partner, CHR youth.
# The Non-CHR dyad partner remains the reference category in pooled models.

GROUP_LEVELS <- c("TD_Caregiver", "TD_Youth", "CHR_Caregiver", "CHR_Youth")
GROUP_LABELS <- c(
  TD_Caregiver = "Non-CHR dyad partner",
  TD_Youth = "Non-CHR youth",
  CHR_Caregiver = "CHR dyad partner",
  CHR_Youth = "CHR youth"
)
PARTICIPANT_LEVELS <- c("Dyad partner", "Youth")
COLORS_ALL <- c(
  TD_Caregiver = "gray50", TD_Youth = "black",
  CHR_Caregiver = "#7EA6D8", CHR_Youth = "#1F5FA8"
)

assert_vars <- function(data, vars, object_name = deparse(substitute(data))) {
  missing <- setdiff(vars, names(data))
  if (length(missing)) stop(object_name, " is missing required variable(s): ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

to_panel <- function(x) {
  z <- trimws(toupper(as.character(x)))
  dplyr::case_when(
    z %in% c("1", "CHR", "TRUE") ~ "CHR",
    z %in% c("0", "TD", "FALSE", "NON-CHR") ~ "TD",
    TRUE ~ NA_character_
  )
}

prepare_long <- function(data) {
  assert_vars(data, c("Dyad_id", "group4", "rating", "second_60"))
  data <- data |>
    dplyr::mutate(
      Dyad_id = factor(Dyad_id),
      group4 = factor(as.character(group4), levels = GROUP_LEVELS)
    )
  if (!"panel" %in% names(data)) {
    data$panel <- ifelse(grepl("^TD_", as.character(data$group4)), "TD",
                         ifelse(grepl("^CHR_", as.character(data$group4)), "CHR", NA_character_))
  }
  if (!"participant" %in% names(data)) {
    data$participant <- ifelse(grepl("Youth", as.character(data$group4)), "Youth",
                               ifelse(grepl("Caregiver", as.character(data$group4)), "Caregiver", NA_character_))
  }
  if (!"second" %in% names(data)) data$second <- 60 * data$second_60
  data
}

center_vars <- function(data, vars, complete_vars, suffix = "_c") {
  assert_vars(data, unique(c(vars, complete_vars)))
  cc <- stats::complete.cases(data[complete_vars])
  if (!any(cc)) stop("No complete observations are available for centering.")
  means <- vapply(vars, function(v) mean(data[[v]][cc]), numeric(1))
  for (v in vars) data[[paste0(v, suffix)]] <- data[[v]] - means[[v]]
  list(data = data, means = means, complete = cc)
}

stars <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
fmt_est <- function(est, p, digits = 3) paste0(sprintf(paste0("%.", digits, "f"), est), stars(p))
fmt_se <- function(se, digits = 3) paste0("(", sprintf(paste0("%.", digits, "f"), se), ")")
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "<.001", sprintf("%.3f", p)))
model_ndyads <- function(model) dplyr::n_distinct(stats::model.frame(model)$Dyad_id)
formula_text <- function(model) paste(deparse(stats::formula(model)), collapse = " ")
canonical_term <- function(x) {
  if (!grepl(":", x, fixed = TRUE)) return(x)
  paste(sort(strsplit(x, ":", fixed = TRUE)[[1]]), collapse = ":")
}

write_workbook <- function(file, sheets, widths = list()) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, sheets[[nm]])
    w <- widths[[nm]]
    if (is.null(w)) w <- "auto"
    openxlsx::setColWidths(wb, nm, cols = seq_len(ncol(sheets[[nm]])), widths = w)
  }
  openxlsx::saveWorkbook(wb, file.path(results_dir, file), overwrite = TRUE)
}

model_audit <- function(models, labels = names(models)) {
  tibble::tibble(
    Specification = labels,
    Formula = vapply(models, formula_text, character(1)),
    N_observations = vapply(models, stats::nobs, numeric(1)),
    N_dyads = vapply(models, model_ndyads, numeric(1))
  )
}

centering_table <- function(means, labels = names(means), source = names(means)) {
  tibble::tibble(Covariate = labels, Source_variable = source, Mean = as.numeric(means))
}

slope_wanted <- c(
  "second_60", "group4TD_Youth:second_60", "group4CHR_Caregiver:second_60",
  "group4CHR_Youth:second_60", "(Intercept)", "group4TD_Youth",
  "group4CHR_Caregiver", "group4CHR_Youth"
)
slope_labels <- c(
  "Time (slope for Non-CHR dyad partner)",
  "Non-CHR youth × time (Δ slope relative to Non-CHR dyad partner)",
  "CHR dyad partner × time (Δ slope relative to Non-CHR dyad partner)",
  "CHR youth × time (Δ slope relative to Non-CHR dyad partner)",
  "Intercept (Non-CHR dyad partner)",
  "Non-CHR youth (Δ intercept relative to Non-CHR dyad partner)",
  "CHR dyad partner (Δ intercept relative to Non-CHR dyad partner)",
  "CHR youth (Δ intercept relative to Non-CHR dyad partner)"
)

extract_fixed_rows <- function(model, wanted, labels, digits = 3) {
  co <- as.data.frame(summary(model)$coefficients) |>
    tibble::rownames_to_column("term") |>
    dplyr::transmute(term, Estimate, SE = `Std. Error`, p = `Pr(>|t|)`)
  missing <- setdiff(wanted, co$term)
  if (length(missing)) stop("Missing expected coefficient(s): ", paste(missing, collapse = ", "))
  co <- co |> dplyr::filter(term %in% wanted) |>
    dplyr::mutate(term = factor(term, levels = wanted)) |>
    dplyr::arrange(term)
  purrr::map2_dfr(labels, seq_along(labels), function(label, i) {
    tibble::tibble(Row = c(label, ""), Value = c(fmt_est(co$Estimate[i], co$p[i], digits), fmt_se(co$SE[i], digits)))
  })
}

build_slope_table <- function(nocov, youth, partner, digits = 3) {
  a <- extract_fixed_rows(nocov, slope_wanted, slope_labels, digits)
  b <- extract_fixed_rows(youth, slope_wanted, slope_labels, digits)
  c <- extract_fixed_rows(partner, slope_wanted, slope_labels, digits)
  stopifnot(identical(a$Row, b$Row), identical(a$Row, c$Row))
  dplyr::bind_rows(
    tibble::tibble(
      Row = a$Row,
      `(1) No covariates` = a$Value,
      `(2) Youth covariates` = b$Value,
      `(3) Dyad-partner covariates` = c$Value
    ),
    tibble::tibble(
      Row = c("N observations", "N dyads"),
      `(1) No covariates` = as.character(c(stats::nobs(nocov), model_ndyads(nocov))),
      `(2) Youth covariates` = as.character(c(stats::nobs(youth), model_ndyads(youth))),
      `(3) Dyad-partner covariates` = as.character(c(stats::nobs(partner), model_ndyads(partner)))
    )
  )
}

fit_slope_models <- function(data) {
  data <- prepare_long(data)
  assert_vars(data, c("X3_Age", "X1_Sex_1", "c_demo_age_1", "c_gender_1"))

  youth_center <- center_vars(
    data, c("X3_Age", "X1_Sex_1"),
    c("rating", "group4", "second_60", "Dyad_id", "X3_Age", "X1_Sex_1")
  )
  data <- youth_center$data
  partner_center <- center_vars(
    data, c("c_demo_age_1", "c_gender_1"),
    c("rating", "group4", "second_60", "Dyad_id", "c_demo_age_1", "c_gender_1")
  )
  data <- partner_center$data

  nocov <- lmerTest::lmer(rating ~ group4 * second_60 + (1 | Dyad_id), data = data)
  youth <- lmerTest::lmer(
    rating ~ group4 * second_60 + X3_Age_c + X1_Sex_1_c +
      X3_Age_c:second_60 + X1_Sex_1_c:second_60 + (1 | Dyad_id),
    data = data
  )
  partner <- lmerTest::lmer(
    rating ~ group4 * second_60 + c_demo_age_1_c + c_gender_1_c +
      c_demo_age_1_c:second_60 + c_gender_1_c:second_60 + (1 | Dyad_id),
    data = data
  )

  list(
    data = data,
    youth_means = youth_center$means,
    partner_means = partner_center$means,
    nocov = nocov,
    youth = youth,
    partner = partner
  )
}

audit_slope_model <- function(model, specification = c("youth", "partner")) {
  specification <- match.arg(specification)
  terms <- attr(stats::terms(model), "term.labels")
  canon <- vapply(terms, canonical_term, character(1))

  if (specification == "youth") {
    required_main <- c("X3_Age_c", "X1_Sex_1_c")
    required_interactions <- c("X3_Age_c:second_60", "X1_Sex_1_c:second_60")
    forbidden <- c("c_demo_age_1", "c_gender_1")
  } else {
    required_main <- c("c_demo_age_1_c", "c_gender_1_c")
    required_interactions <- c("c_demo_age_1_c:second_60", "c_gender_1_c:second_60")
    forbidden <- c("X3_Age", "X1_Sex_1")
  }

  if (!all(required_main %in% terms)) stop("Adjusted slope model is missing expected demographic main effects.")
  req <- vapply(required_interactions, canonical_term, character(1))
  if (!all(req %in% canon)) stop("Adjusted slope model is missing expected demographic × time terms.")
  if (any(vapply(terms, function(z) any(vapply(forbidden, function(token) grepl(token, z, fixed = TRUE), logical(1))), logical(1)))) {
    stop("Adjusted slope model unexpectedly contains covariates from the other demographic specification.")
  }
  invisible(TRUE)
}

combined_centering_table <- function(youth_means, partner_means) {
  tibble::tibble(
    Covariate = c("Youth age", "Youth sex", "Dyad-partner age", "Dyad-partner sex"),
    Source_variable = c("X3_Age", "X1_Sex_1", "c_demo_age_1", "c_gender_1"),
    Mean = c(
      unname(youth_means["X3_Age"]), unname(youth_means["X1_Sex_1"]),
      unname(partner_means["c_demo_age_1"]), unname(partner_means["c_gender_1"])
    )
  )
}

get_emm_columns <- function(data, trend = FALSE) {
  estimate <- if (trend) grep("\\.trend$|trend", names(data), value = TRUE)[1] else "emmean"
  lower <- grep("lower\\.CL|asymp\\.LCL|LCL", names(data), value = TRUE, ignore.case = TRUE)[1]
  upper <- grep("upper\\.CL|asymp\\.UCL|UCL", names(data), value = TRUE, ignore.case = TRUE)[1]
  if (any(is.na(c(estimate, lower, upper)))) stop("Could not identify expected emmeans columns.")
  list(estimate = estimate, lower = lower, upper = upper)
}

trend_summary <- function(trend, label) {
  d <- as.data.frame(summary(trend, infer = c(TRUE, TRUE)))
  cols <- get_emm_columns(d, trend = TRUE)
  pcol <- grep("p\\.value", names(d), value = TRUE, ignore.case = TRUE)[1]
  d |> dplyr::transmute(
    group4 = as.character(group4), Participant = unname(GROUP_LABELS[group4]),
    Slope = .data[[cols$estimate]], SE, lower_CL = .data[[cols$lower]], upper_CL = .data[[cols$upper]],
    p_value = if (!is.na(pcol)) .data[[pcol]] else NA_real_, Model = label
  )
}

pairwise_contrasts <- list(
  "Non-CHR dyad partner - Non-CHR youth" = c(1, -1, 0, 0),
  "Non-CHR dyad partner - CHR dyad partner" = c(1, 0, -1, 0),
  "Non-CHR dyad partner - CHR youth" = c(1, 0, 0, -1),
  "Non-CHR youth - CHR dyad partner" = c(0, 1, -1, 0),
  "Non-CHR youth - CHR youth" = c(0, 1, 0, -1),
  "CHR dyad partner - CHR youth" = c(0, 0, 1, -1)
)

format_contrasts <- function(data, digits = 4) {
  data <- data |> dplyr::mutate(contrast = as.character(contrast), ord = match(contrast, names(pairwise_contrasts))) |> dplyr::arrange(ord)
  purrr::map_dfr(seq_len(nrow(data)), function(i) {
    tibble::tibble(Row = c(data$contrast[i], ""), Value = c(fmt_est(data$estimate[i], data$p.value[i], digits), fmt_se(data$SE[i], digits)))
  })
}

plot_y_range <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  values <- unlist(lapply(built$data, function(x) unlist(x[intersect(c("y", "ymin", "ymax"), names(x))], use.names = FALSE)), use.names = FALSE)
  range(values[is.finite(values)])
}

make_shared_legend <- function(colors, linetypes, levels) {
  d <- tibble::tibble(
    Participant = factor(rep(levels, each = 2), levels = levels),
    x = rep(c(1, 2), length(levels)), y = rep(seq_along(levels), each = 2)
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, group = Participant, color = Participant, linetype = Participant)) +
    ggplot2::geom_line(linewidth = 1.3) +
    ggplot2::scale_color_manual(values = colors, breaks = levels, name = "Participant") +
    ggplot2::scale_linetype_manual(values = linetypes, breaks = levels, guide = "none") +
    ggplot2::theme_void() + ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal")
  cowplot::get_legend(p)
}

# Data preparation

df_long <- prepare_long(df_long)
df_longALT <- prepare_long(df_longALT)
assert_vars(df_complete, c("Dyad_id", "CHR", "second", "CrT_Rscr", "RrT_Rscr", "relationship_dyad"))
df_dyad <- df_complete |> dplyr::distinct(Dyad_id, .keep_all = TRUE) |> dplyr::mutate(panel = to_panel(CHR))

################################################################################
################################################################################
############################## MANUSCRIPT ######################################
################################################################################
################################################################################

########################################################################
### Table 1. Sample Characteristics
#
# Includes:
#
#   - Rating-dial mean (SD)
#   - Linear-slope mean (SD)
#   - Youth age and dyad-partner age
#   - Sex (% women)
#   - Racial identity (% White / Non-White)
#   - Dyad-partner relationship to youth
#   - Symptom measures
#
########################################################################

message("\n--- Table 1. Sample Characteristics ---")

m_sd <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean(x), stats::sd(x))
}

pct1 <- function(x, digits = 0) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  paste0(round(mean(as.numeric(x)) * 100, digits), "%")
}

n_pct <- function(n, denominator, digits = 1) {
  invalid <- is.na(n) | is.na(denominator) | denominator <= 0
  out <- paste0(
    n,
    " (",
    sprintf(paste0("%.", digits, "f"), 100 * n / denominator),
    "%)"
  )
  out[invalid] <- NA_character_
  out
}

first_existing <- function(data, candidates) {
  x <- candidates[candidates %in% names(data)]
  if (length(x)) x[1] else NA_character_
}

get_cell <- function(data, panel_label, participant_label, column) {
  x <- data |>
    dplyr::filter(
      .data$panel == .env$panel_label,
      .data$participant == .env$participant_label
    ) |>
    dplyr::pull(.data[[column]])
  if (length(x)) x[1] else NA_character_
}

get_panel_stat <- function(data, panel_label, column) {
  x <- data |>
    dplyr::filter(.data$panel == .env$panel_label) |>
    dplyr::pull(.data[[column]])
  if (length(x) && !is.na(x[1])) x[1] else NA_character_
}

relationship_stats <- df_dyad |>
  dplyr::mutate(
    relationship_clean = dplyr::na_if(trimws(as.character(relationship_dyad)), "")
  ) |>
  dplyr::group_by(panel) |>
  dplyr::summarise(
    N_dyads = dplyr::n(),
    N_relationship_observed = sum(!is.na(relationship_clean)),
    Biological_parent_n = sum(relationship_clean == "Biological parent", na.rm = TRUE),
    Romantic_partner_n = sum(relationship_clean == "Romantic partner", na.rm = TRUE),
    Other_relationship_n = sum(relationship_clean %in% c("Other caregiver", "Other, namely"), na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    `Biological parent` = n_pct(Biological_parent_n, N_dyads),
    `Romantic partner` = n_pct(Romantic_partner_n, N_dyads),
    `Other relationship` = n_pct(Other_relationship_n, N_dyads)
  )

if (any(relationship_stats$Biological_parent_n + relationship_stats$Romantic_partner_n + relationship_stats$Other_relationship_n != relationship_stats$N_relationship_observed)) {
  warning("Table 1 includes relationship values outside the three displayed categories; inspect relationship_dyad before manuscript use.")
}

rating_stats <- df_complete |>
  dplyr::mutate(panel = to_panel(CHR)) |>
  dplyr::group_by(Dyad_id, panel) |>
  dplyr::summarise(
    Youth = if (all(is.na(CrT_Rscr))) NA_real_ else mean(CrT_Rscr, na.rm = TRUE),
    Caregiver = if (all(is.na(RrT_Rscr))) NA_real_ else mean(RrT_Rscr, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(c(Youth, Caregiver), names_to = "participant", values_to = "mean_rating") |>
  dplyr::group_by(panel, participant) |>
  dplyr::summarise(`Rating dial scores` = m_sd(mean_rating), .groups = "drop")

assert_vars(df_dyad, c("s10m_Rself_y", "s10m_Rself_c", "bdi_total", "tot_BAI"))
slope_stats <- dplyr::bind_rows(
  tibble::tibble(panel = df_dyad$panel, participant = "Youth", slope = df_dyad$s10m_Rself_y),
  tibble::tibble(panel = df_dyad$panel, participant = "Caregiver", slope = df_dyad$s10m_Rself_c)
) |>
  dplyr::filter(!is.na(slope)) |>
  dplyr::group_by(panel, participant) |>
  dplyr::summarise(`Slope of rating dial scores` = m_sd(slope), .groups = "drop")

y_age <- first_existing(df_dyad, c("X3_Age", "y_age", "Y_Age"))
c_age <- first_existing(df_dyad, c("c_demo_age_1", "c_demo_age", "caregiver_age", "C_Age"))
y_sex <- first_existing(df_dyad, "X1_Sex_1")
c_sex <- first_existing(df_dyad, "c_gender_1")
y_race <- first_existing(df_dyad, c("X7_Race.s", "X7_Race"))
c_race_bin <- first_existing(df_dyad, "c_race_1")
c_race_text <- first_existing(df_dyad, "c_race")

age_stats <- dplyr::bind_rows(
  tibble::tibble(panel = df_dyad$panel, participant = "Youth", age = if (!is.na(y_age)) df_dyad[[y_age]] else NA_real_),
  tibble::tibble(panel = df_dyad$panel, participant = "Caregiver", age = if (!is.na(c_age)) df_dyad[[c_age]] else NA_real_)
) |>
  dplyr::group_by(panel, participant) |>
  dplyr::summarise(`Age in years` = m_sd(age), .groups = "drop")

sex_stats <- dplyr::bind_rows(
  tibble::tibble(panel = df_dyad$panel, participant = "Youth", female = if (!is.na(y_sex)) df_dyad[[y_sex]] else NA_real_),
  tibble::tibble(panel = df_dyad$panel, participant = "Caregiver", female = if (!is.na(c_sex)) df_dyad[[c_sex]] else NA_real_)
) |>
  dplyr::group_by(panel, participant) |>
  dplyr::summarise(`Biological sex, % female` = pct1(female), .groups = "drop")

y_white <- if (!is.na(y_race)) as.numeric(df_dyad[[y_race]] == 1) else rep(NA_real_, nrow(df_dyad))
c_white <- if (!is.na(c_race_bin)) {
  as.numeric(df_dyad[[c_race_bin]] == 1)
} else if (!is.na(c_race_text)) {
  as.numeric(df_dyad[[c_race_text]] == "Caucasian - White")
} else {
  rep(NA_real_, nrow(df_dyad))
}

race_stats <- dplyr::bind_rows(
  tibble::tibble(panel = df_dyad$panel, participant = "Youth", white = y_white),
  tibble::tibble(panel = df_dyad$panel, participant = "Caregiver", white = c_white)
) |>
  dplyr::mutate(nonwhite = ifelse(is.na(white), NA_real_, 1 - white)) |>
  dplyr::group_by(panel, participant) |>
  dplyr::summarise(
    White = pct1(white),
    `Non-White` = pct1(nonwhite),
    .groups = "drop"
  )

panel_ms <- function(data, var) {
  if (!var %in% names(data)) return(tibble::tibble(panel = c("TD", "CHR"), value = NA_character_))
  data |>
    dplyr::group_by(panel) |>
    dplyr::summarise(value = m_sd(.data[[var]]), .groups = "drop")
}

symptoms <- list(
  Depression_y = panel_ms(df_dyad, "Y_BDI_total"),
  Anxiety_y = panel_ms(df_dyad, "Y_BAI_total"),
  Pos = panel_ms(df_dyad, "Y_SIPS_PosTotal"),
  Neg = panel_ms(df_dyad, "Y_SIPS_NegTotal"),
  GFS = panel_ms(df_dyad, "GFS_S_Current"),
  Depression_c = panel_ms(df_dyad, "bdi_total"),
  Anxiety_c = panel_ms(df_dyad, "tot_BAI")
)

sym_value <- function(name, panel_label, missing = NA_character_) {
  x <- symptoms[[name]] |>
    dplyr::filter(.data$panel == .env$panel_label) |>
    dplyr::pull(value)
  if (!length(x) || is.na(x[1])) missing else x[1]
}

make_panel_block <- function(panel_label) {
  rows <- c(
    "Rating dial scores", "Slope of rating dial scores", "Age in years", "Biological sex, % female",
    "Racial identity, %", "  White", "  Non-White", "Dyad-partner relationship, n (%)",
    "  Biological parent", "  Romantic partner", "  Other relationship", "Symptoms",
    "  Positive symptoms", "  Negative symptoms", "  Social functioning", "  Depression", "  Anxiety"
  )

  relationship_values <- c(
    get_panel_stat(relationship_stats, panel_label, "Biological parent"),
    get_panel_stat(relationship_stats, panel_label, "Romantic partner"),
    get_panel_stat(relationship_stats, panel_label, "Other relationship")
  )

  youth <- c(
    get_cell(rating_stats, panel_label, "Youth", "Rating dial scores"),
    get_cell(slope_stats, panel_label, "Youth", "Slope of rating dial scores"),
    get_cell(age_stats, panel_label, "Youth", "Age in years"),
    get_cell(sex_stats, panel_label, "Youth", "Biological sex, % female"),
    "",
    get_cell(race_stats, panel_label, "Youth", "White"),
    get_cell(race_stats, panel_label, "Youth", "Non-White"),
    "",
    "", "", "",
    "",
    sym_value("Pos", panel_label, "-"),
    sym_value("Neg", panel_label, "-"),
    sym_value("GFS", panel_label, "-"),
    sym_value("Depression_y", panel_label),
    sym_value("Anxiety_y", panel_label)
  )

  partner <- c(
    get_cell(rating_stats, panel_label, "Caregiver", "Rating dial scores"),
    get_cell(slope_stats, panel_label, "Caregiver", "Slope of rating dial scores"),
    get_cell(age_stats, panel_label, "Caregiver", "Age in years"),
    get_cell(sex_stats, panel_label, "Caregiver", "Biological sex, % female"),
    "",
    get_cell(race_stats, panel_label, "Caregiver", "White"),
    get_cell(race_stats, panel_label, "Caregiver", "Non-White"),
    "",
    relationship_values,
    "",
    "-", "-", "-",
    sym_value("Depression_c", panel_label),
    sym_value("Anxiety_c", panel_label)
  )

  tibble::tibble(Row = rows, Youth = youth, `Dyad partner` = partner)
}

for (obj in list(rating_stats, slope_stats, age_stats, sex_stats, race_stats)) {
  if (any(dplyr::count(obj, panel, participant)$n > 1)) {
    stop("Table 1 source summaries contain duplicate panel-by-participant rows.")
  }
}

Table1 <- dplyr::bind_rows(
  tibble::tibble(Row = "Panel A: Non-CHR dyads", Youth = "", `Dyad partner` = ""),
  make_panel_block("TD"),
  tibble::tibble(Row = "Panel B: CHR dyads", Youth = "", `Dyad partner` = ""),
  make_panel_block("CHR")
)

sample_counts_Table1 <- df_dyad |>
  dplyr::count(panel, name = "N_dyads") |>
  dplyr::mutate(Group = dplyr::recode(panel, TD = "Non-CHR", CHR = "CHR")) |>
  dplyr::select(Group, N_dyads)

wb_Table1 <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb_Table1, "Table1")
openxlsx::writeData(wb_Table1, "Table1", Table1)
header_style_Table1 <- openxlsx::createStyle(textDecoration = "bold")
panel_style_Table1 <- openxlsx::createStyle(textDecoration = "bold")
openxlsx::addStyle(wb_Table1, "Table1", header_style_Table1, rows = 1, cols = 1:3, gridExpand = TRUE)
openxlsx::addStyle(wb_Table1, "Table1", panel_style_Table1, rows = which(grepl("^Panel ", Table1$Row)) + 1, cols = 1:3, gridExpand = TRUE)
# Relationship composition describes the dyad partner, so these values are kept
# explicitly in the Dyad partner column rather than merged across participant columns.
relationship_rows_Table1 <- which(Table1$Row %in% c("  Biological parent", "  Romantic partner", "  Other relationship"))
if (any(Table1$Youth[relationship_rows_Table1] != "") || any(Table1$`Dyad partner`[relationship_rows_Table1] == "")) {
  stop("Table 1 relationship-composition values must appear only in the Dyad partner column.")
}
openxlsx::setColWidths(wb_Table1, "Table1", cols = 1:3, widths = c(45, 20, 20))
openxlsx::addWorksheet(wb_Table1, "Sample counts")
openxlsx::writeData(wb_Table1, "Sample counts", sample_counts_Table1)
openxlsx::setColWidths(wb_Table1, "Sample counts", cols = 1:2, widths = c(18, 12))
openxlsx::addWorksheet(wb_Table1, "Relationship audit")
openxlsx::writeData(wb_Table1, "Relationship audit", relationship_stats)
openxlsx::setColWidths(wb_Table1, "Relationship audit", cols = 1:ncol(relationship_stats), widths = "auto")
openxlsx::addWorksheet(wb_Table1, "Notes")
openxlsx::writeData(
  wb_Table1, "Notes",
  tibble::tibble(
    Item = c("Descriptive slope unit", "Reader-facing terminology"),
    Description = c(
      "s10m_Rself_y and s10m_Rself_c are participant-specific linear change across the full 10-minute conversation; they are not the per-minute coefficients in Table 2.",
      "Internal TD/Caregiver variable names are preserved only for reproducibility; manuscript-facing labels are Non-CHR and Dyad partner."
    )
  )
)
openxlsx::setColWidths(wb_Table1, "Notes", cols = 1:2, widths = c(28, 95))
openxlsx::saveWorkbook(wb_Table1, file.path(results_dir, "Table1.xlsx"), overwrite = TRUE)

################################################################################
### Table 2. Pooled Linear Mixed-Effects Models
#
# Primary Aim 1 model using the balanced second-level sample.
# Outcome: rating-dial score. Time is second_60, so one unit = 60 seconds.
# Non-CHR dyad partner is the reference group.
# Column (1): group × time, no demographic covariates.
# Column (2): mean-centered youth age/sex plus youth age×time and sex×time.
# Column (3): mean-centered dyad-partner age/sex plus partner age×time and sex×time.
# The exported workbook also records the fitted formulas and centering constants.
################################################################################
message("\n--- Table 2. Pooled Linear Mixed-Effects Models ---")

t2 <- fit_slope_models(df_long)
df_long <- t2$data
m2_nocov <- t2$nocov
m2_cov <- t2$youth
m2_partner <- t2$partner
audit_slope_model(m2_cov, "youth")
audit_slope_model(m2_partner, "partner")

table2_youth_age_mean <- unname(t2$youth_means["X3_Age"])
table2_youth_sex_mean <- unname(t2$youth_means["X1_Sex_1"])
table2_partner_age_mean <- unname(t2$partner_means["c_demo_age_1"])
table2_partner_sex_mean <- unname(t2$partner_means["c_gender_1"])

Table2 <- build_slope_table(m2_nocov, m2_cov, m2_partner)
write_workbook(
  "Table2.xlsx",
  list(
    Table2 = Table2,
    `Model audit` = model_audit(
      list(m2_nocov, m2_cov, m2_partner),
      c("(1) No covariates", "(2) Youth covariates", "(3) Dyad-partner covariates")
    ),
    `Centering values` = combined_centering_table(t2$youth_means, t2$partner_means)
  ),
  list(Table2 = c(62, 20, 24, 28), `Model audit` = c(28, 95, 18, 12), `Centering values` = c(24, 24, 18))
)

################################################################################
### Figure 1. Emotional Experience Trajectories During Conflict
#
# Four-panel main-text figure.
# A/C display second-by-second group means with fitted linear trajectories.
# B/D display second-by-second group means with descriptive LOESS trajectories.
# A/B are Non-CHR dyads; C/D are CHR dyads.
# LOESS span is selected separately for each participant group using 5-fold CV,
# local-linear LOESS, and candidate spans .05-.95 by .01.
# The selected spans are exported for reproducibility.
################################################################################
message("\n--- Figure 1. Emotional Experience Trajectories During Conflict ---")

fig1_data <- df_complete |>
  dplyr::mutate(group = ifelse(to_panel(CHR) == "TD", "Non-CHR", "CHR"))
fig1_nonchr <- fig1_data |> dplyr::filter(group == "Non-CHR")
fig1_chr <- fig1_data |> dplyr::filter(group == "CHR")
span_grid <- expand.grid(span = seq(.05, .95, .01), degree = 1)
train_control <- caret::trainControl(method = "cv", number = 5)
select_spans <- function(data) {
  set.seed(20260722)
  y <- caret::train(CrT_Rscr ~ second, data = data, method = "gamLoess", tuneGrid = span_grid, trControl = train_control)
  p <- caret::train(RrT_Rscr ~ second, data = data, method = "gamLoess", tuneGrid = span_grid, trControl = train_control)
  c(youth = y$bestTune$span, partner = p$bestTune$span)
}
spans_nonchr <- select_spans(fig1_nonchr); spans_chr <- select_spans(fig1_chr)
span_nonchr_youth <- spans_nonchr["youth"]; span_nonchr_partner <- spans_nonchr["partner"]
span_chr_youth <- spans_chr["youth"]; span_chr_partner <- spans_chr["partner"]

fig1_long <- fig1_data |>
  dplyr::select(group, second, CrT_Rscr, RrT_Rscr) |>
  tidyr::pivot_longer(c(CrT_Rscr, RrT_Rscr), names_to = "source", values_to = "rating") |>
  dplyr::mutate(
    participant = ifelse(source == "CrT_Rscr", "Youth", "Dyad partner"),
    series = factor(paste(group, tolower(participant)), levels = c("Non-CHR dyad partner", "Non-CHR youth", "CHR dyad partner", "CHR youth"))
  ) |>
  dplyr::filter(!is.na(second), !is.na(rating))
series_colors <- c("Non-CHR dyad partner" = "gray50", "Non-CHR youth" = "black", "CHR dyad partner" = "#7EA6D8", "CHR youth" = "#1F5FA8")
series_linetypes <- c("Non-CHR dyad partner" = "longdash", "Non-CHR youth" = "solid", "CHR dyad partner" = "longdash", "CHR youth" = "solid")
base_trajectory_theme <- ggplot2::theme(
  panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
  panel.grid.major = ggplot2::element_line(linewidth = .25, linetype = "dotted", colour = "#CCCCCC"),
  panel.grid.minor = ggplot2::element_line(linewidth = .25, linetype = "dotted", colour = "#CCCCCC"),
  axis.line = ggplot2::element_line(linewidth = .5, colour = "black"),
  plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = .5),
  plot.tag = ggplot2::element_text(face = "bold", size = 12), plot.tag.position = c(.02, .98)
)
add_series_scales <- function(p) p +
  ggplot2::scale_color_manual(values = series_colors, drop = FALSE, name = "Participant") +
  ggplot2::scale_linetype_manual(values = series_linetypes, drop = FALSE, name = "Participant") + base_trajectory_theme
make_linear_panel <- function(data) add_series_scales(
  ggplot2::ggplot(data, ggplot2::aes(second, rating, color = series)) +
    ggplot2::stat_summary(ggplot2::aes(group = series), geom = "line", fun = mean, linewidth = .4, linetype = "solid", show.legend = FALSE) +
    ggplot2::geom_smooth(ggplot2::aes(group = series, linetype = series), method = "lm", formula = y ~ x, linewidth = 1.3, se = TRUE)
)
make_loess_panel <- function(data, youth_span, partner_span) add_series_scales(
  ggplot2::ggplot(data, ggplot2::aes(second, rating, color = series)) +
    ggplot2::stat_summary(ggplot2::aes(group = series), geom = "line", fun = mean, linewidth = .4, linetype = "solid", show.legend = FALSE) +
    ggplot2::geom_smooth(data = dplyr::filter(data, participant == "Youth"), ggplot2::aes(group = series, linetype = series), method = "loess", span = youth_span, linewidth = 1.3, se = TRUE) +
    ggplot2::geom_smooth(data = dplyr::filter(data, participant == "Dyad partner"), ggplot2::aes(group = series, linetype = series), method = "loess", span = partner_span, linewidth = 1.3, se = TRUE)
)
nonchr_long <- dplyr::filter(fig1_long, group == "Non-CHR"); chr_long <- dplyr::filter(fig1_long, group == "CHR")
raw <- list(
  make_linear_panel(nonchr_long), make_loess_panel(nonchr_long, span_nonchr_youth, span_nonchr_partner),
  make_linear_panel(chr_long), make_loess_panel(chr_long, span_chr_youth, span_chr_partner)
)
yr <- range(unlist(lapply(raw, plot_y_range))); pad <- max(diff(yr) * .05, .1); ylim <- yr + c(-pad, pad); ybreaks <- pretty(ylim, 5)
xbreaks <- pretty(range(fig1_long$second, na.rm = TRUE), 6)
finalize_main_panel <- function(p, title, tag, xlab, ylab) p +
  ggplot2::scale_x_continuous(breaks = xbreaks) + ggplot2::scale_y_continuous(breaks = ybreaks) +
  ggplot2::coord_cartesian(ylim = ylim) + ggplot2::labs(title = title, tag = tag, x = xlab, y = ylab) + ggplot2::theme(legend.position = "none")
figs <- list(
  finalize_main_panel(raw[[1]], "Non-CHR, linear fit", "A", NULL, "Rating dial scores"),
  finalize_main_panel(raw[[2]], "Non-CHR, LOESS fit", "B", NULL, NULL),
  finalize_main_panel(raw[[3]], "CHR, linear fit", "C", "Time (in seconds)", "Rating dial scores"),
  finalize_main_panel(raw[[4]], "CHR, LOESS fit", "D", "Time (in seconds)", NULL)
)
legend_main <- make_shared_legend(
  series_colors,
  series_linetypes,
  names(series_colors)
)
if (is.null(legend_main)) stop("Figure 1 shared legend could not be created.")
Figure1 <- cowplot::plot_grid(
  cowplot::plot_grid(cowplot::plot_grid(figs[[1]], figs[[2]], ncol = 2), cowplot::plot_grid(figs[[3]], figs[[4]], ncol = 2), ncol = 1),
  legend_main, ncol = 1, rel_heights = c(1, .08)
)
ggplot2::ggsave(file.path(results_dir, "Figure1.png"), Figure1, width = 11, height = 10.5, dpi = 300, bg = "white")

Figure1_span_selection <- tibble::tibble(
  Group = c("Non-CHR", "Non-CHR", "CHR", "CHR"),
  Participant = c("Youth", "Dyad partner", "Youth", "Dyad partner"),
  Selected_span = as.numeric(c(span_nonchr_youth, span_nonchr_partner, span_chr_youth, span_chr_partner)),
  CV_method = "5-fold cross-validation",
  LOESS_degree = 1,
  Candidate_span_min = 0.05,
  Candidate_span_max = 0.95,
  Candidate_span_step = 0.01
)
write_workbook("Figure1_span_selection.xlsx", list(`Selected spans` = Figure1_span_selection))

################################################################################
### Table A1. All-Available-Data Sensitivity Analysis
#
# Repeats the Table 2 pooled slope models using df_longALT, which retains all
# dyads/participants contributing usable rating-dial data rather than requiring a
# complete 600-second series for both members of the dyad.
# Uses the same three demographic specifications as Table 2.
################################################################################
message("\n--- Table A1. All-Available-Data Sensitivity Analysis ---")

a1 <- fit_slope_models(df_longALT)
m_A1_nocov <- a1$nocov
m_A1_cov <- a1$youth
m_A1_partner <- a1$partner
audit_slope_model(m_A1_cov, "youth")
audit_slope_model(m_A1_partner, "partner")
TableA1 <- build_slope_table(m_A1_nocov, m_A1_cov, m_A1_partner)
write_workbook("TableA1.xlsx", list(
  TableA1 = TableA1,
  `Model audit` = model_audit(
    list(m_A1_nocov, m_A1_cov, m_A1_partner),
    c("(1) No covariates", "(2) Youth covariates", "(3) Dyad-partner covariates")
  ),
  `Centering values` = combined_centering_table(a1$youth_means, a1$partner_means)
))

################################################################################
### Table A2. Excluding CHR Youth Taking Antipsychotic Medication
#
# Sensitivity analysis excluding the two CHR dyads involving a CHR youth taking
# antipsychotic medication. The two IDs are entered once near the top of this
# script (ID1 and ID2).
# Uses the same three slope-model specifications as Table 2 and exports an audit
# confirming exactly two dyads were removed.
################################################################################
message("\n--- Table A2. Excluding CHR Youth Taking Antipsychotic Medication ---")

placeholder_ids_A2 <- c("REPLACE_WITH_DYAD_ID_1", "REPLACE_WITH_DYAD_ID_2", "", NA_character_)
if (is.na(ID1) || is.na(ID2) || ID1 %in% placeholder_ids_A2 || ID2 %in% placeholder_ids_A2) {
  stop("Before running Table A2, replace ID1 and ID2 near the top of this script with the two dyad IDs to exclude.")
}
excluded_ids_A2 <- unique(as.character(c(ID1, ID2)))
if (length(excluded_ids_A2) != 2) stop("Table A2 requires two distinct exclusion IDs in ID1 and ID2.")
all_ids_A2 <- unique(as.character(df_long$Dyad_id))
if (!all(excluded_ids_A2 %in% all_ids_A2)) stop("At least one Table A2 exclusion ID is not present in df_long.")

n_obs_before_A2 <- nrow(df_long)
n_dyads_before_A2 <- dplyr::n_distinct(df_long$Dyad_id)
df_longM <- df_long[!as.character(df_long$Dyad_id) %in% excluded_ids_A2, , drop = FALSE]
n_obs_after_A2 <- nrow(df_longM)
n_dyads_after_A2 <- dplyr::n_distinct(df_longM$Dyad_id)

if ((n_dyads_before_A2 - n_dyads_after_A2) != 2) stop("Table A2 exclusion audit failed: exactly two dyads were not removed.")
if (any(as.character(df_longM$Dyad_id) %in% excluded_ids_A2)) stop("Table A2 exclusion audit failed: an excluded dyad remains in the analytic dataset.")

a2 <- fit_slope_models(df_longM)
m_A2_nocov <- a2$nocov
m_A2_cov <- a2$youth
m_A2_partner <- a2$partner
audit_slope_model(m_A2_cov, "youth")
audit_slope_model(m_A2_partner, "partner")
TableA2 <- build_slope_table(m_A2_nocov, m_A2_cov, m_A2_partner)

extract_focal_numeric <- function(model, specification, source_table) {
  d <- as.data.frame(summary(model)$coefficients) |>
    tibble::rownames_to_column("term") |>
    dplyr::filter(term %in% slope_wanted) |>
    dplyr::transmute(
      Source_table = source_table,
      Specification = specification,
      term,
      Estimate = Estimate,
      SE = `Std. Error`,
      p_value = `Pr(>|t|)`
    )
  d
}

Table2_A2_comparison <- dplyr::bind_rows(
  extract_focal_numeric(m2_nocov, "No covariates", "Table 2"),
  extract_focal_numeric(m2_cov, "Youth covariates", "Table 2"),
  extract_focal_numeric(m2_partner, "Dyad-partner covariates", "Table 2"),
  extract_focal_numeric(m_A2_nocov, "No covariates", "Table A2"),
  extract_focal_numeric(m_A2_cov, "Youth covariates", "Table A2"),
  extract_focal_numeric(m_A2_partner, "Dyad-partner covariates", "Table A2")
) |>
  tidyr::pivot_wider(
    id_cols = c(Specification, term),
    names_from = Source_table,
    values_from = c(Estimate, SE, p_value),
    names_sep = "__"
  ) |>
  dplyr::mutate(
    Estimate_difference_A2_minus_T2 = .data$`Estimate__Table A2` - .data$`Estimate__Table 2`,
    SE_difference_A2_minus_T2 = .data$`SE__Table A2` - .data$`SE__Table 2`
  ) |>
  dplyr::arrange(factor(Specification, levels = c("No covariates", "Youth covariates", "Dyad-partner covariates")), match(term, slope_wanted))

A2_exclusion_audit <- tibble::tibble(
  Check = c("Observations before exclusion", "Observations after exclusion", "Observations removed", "Dyads before exclusion", "Dyads after exclusion", "Dyads removed"),
  Value = c(n_obs_before_A2, n_obs_after_A2, n_obs_before_A2 - n_obs_after_A2, n_dyads_before_A2, n_dyads_after_A2, n_dyads_before_A2 - n_dyads_after_A2)
)

write_workbook("TableA2.xlsx", list(
  TableA2 = TableA2,
  `Model audit` = model_audit(
    list(m_A2_nocov, m_A2_cov, m_A2_partner),
    c("(1) No covariates", "(2) Youth covariates", "(3) Dyad-partner covariates")
  ),
  `Centering values` = combined_centering_table(a2$youth_means, a2$partner_means),
  `Exclusion audit` = A2_exclusion_audit,
  `Table2 vs A2 audit` = Table2_A2_comparison
))

################################################################################
### Tables A3-A4. Dyad-Partner Relationship-Composition Sensitivity
#
# Reviewer-requested Aim 1 sensitivity analyses using the accepted relationship_dyad coding.
# Table A3 restricts BOTH groups to biological-parent dyads.
# Table A4 compares Non-CHR biological-parent dyads with CHR dyads involving
# non-biological dyad partners (romantic partner or other relationship).
# Both tables use the same three slope-model specifications as Table 2 and export
# participant-group simple slopes for interpretation.
################################################################################
message("\n--- Tables A3-A4. Dyad-Partner Relationship-Composition Sensitivity ---")

simple_slopes_export <- function(nocov, youth, partner) {
  a <- trend_summary(emmeans::emtrends(nocov, ~ group4, var = "second_60"), "No covariates")
  b <- trend_summary(
    emmeans::emtrends(youth, ~ group4, var = "second_60", at = list(X3_Age_c = 0, X1_Sex_1_c = 0)),
    "Youth covariates"
  )
  c <- trend_summary(
    emmeans::emtrends(partner, ~ group4, var = "second_60", at = list(c_demo_age_1_c = 0, c_gender_1_c = 0)),
    "Dyad-partner covariates"
  )
  dplyr::bind_rows(a, b, c) |>
    dplyr::mutate(
      Participant = factor(Participant, levels = unname(GROUP_LABELS)),
      Model = factor(Model, levels = c("No covariates", "Youth covariates", "Dyad-partner covariates")),
      Slope = sprintf("%.3f", Slope), SE = sprintf("%.3f", SE),
      `95% CI` = sprintf("[%.3f, %.3f]", lower_CL, upper_CL),
      `p-value` = ifelse(p_value < .001, "<.001", sprintf("%.3f", p_value))
    ) |>
    dplyr::arrange(Model, Participant) |>
    dplyr::select(Model, Participant, Slope, SE, `95% CI`, `p-value`)
}

################################################################################
### Table A3. Pooled Linear Mixed-Effects Models Using Only Biological Parents
#
# Purpose:
#   Tests whether the primary linear-trajectory findings persist when both
#   Non-CHR and CHR groups are restricted to dyads in which the dyad partner is
#   coded as a biological parent.
#
# Sample:
#   Dyads are selected from the balanced primary sample using relationship_dyad.
#
# Model specifications:
#   (1) no demographic covariates;
#   (2) centered youth age/sex + youth age×time and sex×time;
#   (3) centered dyad-partner age/sex + dyad-partner age×time and sex×time.
#
# Output:
#   TableA3.xlsx, including manuscript rows, participant-group simple slopes,
#   model formulas/sample sizes, and centering values.
################################################################################
message("Creating Table A3: biological-parent dyads only")

bio_ids <- df_dyad |> dplyr::filter(trimws(as.character(relationship_dyad)) == "Biological parent") |> dplyr::pull(Dyad_id) |> as.character()
df_longA3 <- df_long |> dplyr::filter(as.character(Dyad_id) %in% bio_ids)
a3 <- fit_slope_models(df_longA3)
m_A3_nocov <- a3$nocov
m_A3_cov <- a3$youth
m_A3_partner <- a3$partner
audit_slope_model(m_A3_cov, "youth")
audit_slope_model(m_A3_partner, "partner")
TableA3 <- build_slope_table(m_A3_nocov, m_A3_cov, m_A3_partner)
write_workbook("TableA3.xlsx", list(
  TableA3 = TableA3,
  `Simple slopes` = simple_slopes_export(m_A3_nocov, m_A3_cov, m_A3_partner),
  `Model audit` = model_audit(
    list(m_A3_nocov, m_A3_cov, m_A3_partner),
    c("(1) No covariates", "(2) Youth covariates", "(3) Dyad-partner covariates")
  ),
  `Centering values` = combined_centering_table(a3$youth_means, a3$partner_means)
))

################################################################################
### Table A4. Non-CHR Biological-Parent vs CHR Non-Biological-Partner Dyads
#
# Purpose:
#   Provides the complementary relationship-composition sensitivity analysis.
#   The comparison keeps Non-CHR biological-parent dyads and CHR dyads whose
#   partner is romantic or otherwise non-biological.
#
# Interpretation:
#   This is descriptive sensitivity evidence about relationship composition,
#   not a formal moderation test by relationship type.
#
# Model specifications:
#   The same three pooled slope specifications used in Table 2 and Table A3.
#
# Output:
#   TableA4.xlsx, including manuscript rows, participant-group simple slopes,
#   model formulas/sample sizes, and centering values.
################################################################################
message("Creating Table A4: Non-CHR biological-parent vs CHR non-biological-partner dyads")

A4_dyads <- df_dyad |>
  dplyr::mutate(rel = dplyr::na_if(trimws(as.character(relationship_dyad)), "")) |>
  dplyr::filter((panel == "TD" & rel == "Biological parent") | (panel == "CHR" & rel %in% c("Romantic partner", "Other caregiver", "Other, namely")))
A4_ids <- as.character(A4_dyads$Dyad_id)
df_longA4 <- df_long |> dplyr::filter(as.character(Dyad_id) %in% A4_ids)
a4 <- fit_slope_models(df_longA4)
m_A4_nocov <- a4$nocov
m_A4_cov <- a4$youth
m_A4_partner <- a4$partner
audit_slope_model(m_A4_cov, "youth")
audit_slope_model(m_A4_partner, "partner")
TableA4 <- build_slope_table(m_A4_nocov, m_A4_cov, m_A4_partner)
write_workbook("TableA4.xlsx", list(
  TableA4 = TableA4,
  `Simple slopes` = simple_slopes_export(m_A4_nocov, m_A4_cov, m_A4_partner),
  `Model audit` = model_audit(
    list(m_A4_nocov, m_A4_cov, m_A4_partner),
    c("(1) No covariates", "(2) Youth covariates", "(3) Dyad-partner covariates")
  ),
  `Centering values` = combined_centering_table(a4$youth_means, a4$partner_means)
))

################################################################################
### Figure A1. CHR Trajectories by Dyad-Partner Relationship
#
# Reviewer-requested visualization of relationship-composition heterogeneity.
# Panels A/B show CHR dyads with biological-parent partners; C/D show CHR dyads
# with non-biological dyad partners. Each subset is shown with linear and LOESS
# fits. The LOESS spans are the CHR-wide spans selected for main Figure 1, held
# fixed across relationship subsets to facilitate descriptive comparison.
################################################################################
message("\n--- Figure A1. CHR Trajectories by Dyad-Partner Relationship ---")

rel_map_FA1 <- df_dyad |>
  dplyr::mutate(
    rel = dplyr::na_if(trimws(as.character(relationship_dyad)), ""),
    relationship_group = dplyr::case_when(
      rel == "Biological parent" ~ "Biological-parent dyads",
      rel %in% c("Romantic partner", "Other caregiver", "Other, namely") ~ "Dyads with non-biological dyad partners",
      TRUE ~ NA_character_
    ), id = as.character(Dyad_id)
  ) |>
  dplyr::filter(panel == "CHR", !is.na(relationship_group)) |>
  dplyr::distinct(id, relationship_group)
df_FA1 <- df_complete |> dplyr::mutate(id = as.character(Dyad_id), panel2 = to_panel(CHR)) |>
  dplyr::filter(panel2 == "CHR") |> dplyr::inner_join(rel_map_FA1, by = "id") |>
  dplyr::select(id, relationship_group, second, CrT_Rscr, RrT_Rscr) |>
  tidyr::pivot_longer(c(CrT_Rscr, RrT_Rscr), names_to = "source", values_to = "rating") |>
  dplyr::mutate(Participant = factor(ifelse(source == "CrT_Rscr", "Youth", "Dyad partner"), levels = PARTICIPANT_LEVELS)) |>
  dplyr::filter(!is.na(second), !is.na(rating))
n_FA1 <- rel_map_FA1 |> dplyr::count(relationship_group)
get_n_FA1 <- function(x) n_FA1$n[match(x, n_FA1$relationship_group)]
colors_chr <- c(Youth = "#1F5FA8", `Dyad partner` = "#7EA6D8"); ltypes <- c(Youth = "solid", `Dyad partner` = "longdash")
make_FA1_panel <- function(data, loess = FALSE, title, subtitle, xlab, ylab) {
  p <- ggplot2::ggplot(data, ggplot2::aes(second, rating, color = Participant)) +
    ggplot2::stat_summary(ggplot2::aes(group = Participant), geom = "line", fun = mean, linewidth = .4, show.legend = FALSE)
  if (!loess) {
    p <- p + ggplot2::geom_smooth(ggplot2::aes(group = Participant, linetype = Participant), method = "lm", formula = y ~ x, se = FALSE, linewidth = 1.3, show.legend = FALSE)
  } else {
    p <- p +
      ggplot2::geom_smooth(data = dplyr::filter(data, Participant == "Youth"), ggplot2::aes(group = Participant, linetype = Participant), method = "loess", span = span_chr_youth, se = FALSE, linewidth = 1.3, show.legend = FALSE) +
      ggplot2::geom_smooth(data = dplyr::filter(data, Participant == "Dyad partner"), ggplot2::aes(group = Participant, linetype = Participant), method = "loess", span = span_chr_partner, se = FALSE, linewidth = 1.3, show.legend = FALSE)
  }
  p + ggplot2::scale_color_manual(values = colors_chr) + ggplot2::scale_linetype_manual(values = ltypes) +
    ggplot2::scale_x_continuous(breaks = seq(0, 600, 120)) + ggplot2::scale_y_continuous(breaks = c(1.5, 2, 2.5, 3)) +
    ggplot2::coord_cartesian(xlim = c(0, 600), ylim = c(1.5, 3.2)) + ggplot2::labs(x = xlab, y = ylab, title = title, subtitle = subtitle) +
    base_trajectory_theme + ggplot2::theme(legend.position = "none", plot.subtitle = ggplot2::element_text(hjust = .5))
}
bio <- dplyr::filter(df_FA1, relationship_group == "Biological-parent dyads")
nonbio <- dplyr::filter(df_FA1, relationship_group == "Dyads with non-biological dyad partners")
FA1_plots <- list(
  make_FA1_panel(bio, FALSE, "Biological-parent dyads", paste0("Linear fit (n = ", get_n_FA1("Biological-parent dyads"), " dyads)"), NULL, "Rating dial scores"),
  make_FA1_panel(bio, TRUE, "Biological-parent dyads", paste0("LOESS fit (n = ", get_n_FA1("Biological-parent dyads"), " dyads)"), NULL, NULL),
  make_FA1_panel(nonbio, FALSE, "Dyads with non-biological dyad partners", paste0("Linear fit (n = ", get_n_FA1("Dyads with non-biological dyad partners"), " dyads)"), "Time (in seconds)", "Rating dial scores"),
  make_FA1_panel(nonbio, TRUE, "Dyads with non-biological dyad partners", paste0("LOESS fit (n = ", get_n_FA1("Dyads with non-biological dyad partners"), " dyads)"), "Time (in seconds)", NULL)
)
legend_FA1 <- make_shared_legend(colors_chr, ltypes, PARTICIPANT_LEVELS)
FigureA1 <- cowplot::plot_grid(
  cowplot::plot_grid(cowplot::plot_grid(FA1_plots[[1]], FA1_plots[[2]], labels = c("A", "B"), ncol = 2), cowplot::plot_grid(FA1_plots[[3]], FA1_plots[[4]], labels = c("C", "D"), ncol = 2), ncol = 1),
  legend_FA1, ncol = 1, rel_heights = c(1, .065)
)
ggplot2::ggsave(file.path(results_dir, "FigureA1.png"), FigureA1, width = 11, height = 8, dpi = 300, bg = "white")

################################################################################
### Table A5. Separate Linear Mixed-Effects Models by CHR Status
#
# Fits Aim 1 models separately within Non-CHR and CHR dyads.
# Within each stratum, Dyad partner is the participant reference category.
# Therefore second_60 is the dyad-partner slope; Youth×time is the youth-minus-
# partner slope difference; the youth slope is their linear combination.
# All three demographic specifications are reported. Centered covariates use the
# same full-sample centering constants as Table 2 so reference points are common.
# The workbook retains coefficient and pooled-vs-separate slope audit sheets.
################################################################################
message("\n--- Table A5. Separate Linear Mixed-Effects Models by CHR Status ---")

df_A5 <- df_long |>
  dplyr::mutate(
    X3_Age_c = X3_Age - table2_youth_age_mean,
    X1_Sex_1_c = X1_Sex_1 - table2_youth_sex_mean,
    c_demo_age_1_c = c_demo_age_1 - table2_partner_age_mean,
    c_gender_1_c = c_gender_1 - table2_partner_sex_mean
  )
df_nonchr_A5 <- df_A5 |> dplyr::filter(panel == "TD") |>
  dplyr::mutate(participant_A5 = factor(ifelse(grepl("Youth", as.character(group4)), "Youth", "Dyad partner"), levels = c("Dyad partner", "Youth")))
df_chr_A5 <- df_A5 |> dplyr::filter(panel == "CHR") |>
  dplyr::mutate(participant_A5 = factor(ifelse(grepl("Youth", as.character(group4)), "Youth", "Dyad partner"), levels = c("Dyad partner", "Youth")))

fit_A5 <- function(data) list(
  nocov = lmerTest::lmer(rating ~ participant_A5 * second_60 + (1 | Dyad_id), data = data),
  youth = lmerTest::lmer(
    rating ~ participant_A5 * second_60 + X3_Age_c + X1_Sex_1_c + X3_Age_c:second_60 + X1_Sex_1_c:second_60 + (1 | Dyad_id),
    data = data
  ),
  partner = lmerTest::lmer(
    rating ~ participant_A5 * second_60 + c_demo_age_1_c + c_gender_1_c + c_demo_age_1_c:second_60 + c_gender_1_c:second_60 + (1 | Dyad_id),
    data = data
  )
)
A5n <- fit_A5(df_nonchr_A5)
A5c <- fit_A5(df_chr_A5)
m_NonCHR_A5_nocov <- A5n$nocov
m_NonCHR_A5_cov <- A5n$youth
m_NonCHR_A5_partner <- A5n$partner
m_CHR_A5_nocov <- A5c$nocov
m_CHR_A5_cov <- A5c$youth
m_CHR_A5_partner <- A5c$partner

find_A5_term <- function(model, target) {
  coefficient_names <- names(lme4::fixef(model))
  if (target %in% coefficient_names) return(target)
  target_canonical <- canonical_term(target)
  hits <- coefficient_names[
    vapply(coefficient_names, function(x) canonical_term(x) == target_canonical, logical(1))
  ]
  if (length(hits) != 1) {
    stop("Could not uniquely identify Table A5 coefficient: ", target)
  }
  hits
}

extract_A5 <- function(model, panel, adjustment = c("none", "youth", "partner")) {
  adjustment <- match.arg(adjustment)

  reference_levels <- levels(stats::model.frame(model)$participant_A5)
  if (!identical(reference_levels, c("Dyad partner", "Youth"))) {
    stop("Table A5 requires Dyad partner to be the participant_A5 reference category.")
  }

  coefficients <- as.data.frame(summary(model)$coefficients) |>
    tibble::rownames_to_column("term")

  time_term <- find_A5_term(model, "second_60")
  youth_time_term <- find_A5_term(model, "participant_A5Youth:second_60")

  time_row <- coefficients[coefficients$term == time_term, , drop = FALSE]
  youth_time_row <- coefficients[coefficients$term == youth_time_term, , drop = FALSE]

  if (nrow(time_row) != 1 || nrow(youth_time_row) != 1) {
    stop("Could not recover the expected Table A5 time coefficients.")
  }

  L_youth <- stats::setNames(rep(0, length(lme4::fixef(model))), names(lme4::fixef(model)))
  L_youth[time_term] <- 1
  L_youth[youth_time_term] <- 1
  youth_test <- as.data.frame(
    lmerTest::contest1D(model, L = L_youth, ddf = "Satterthwaite")
  )

  p_col_youth <- grep("^Pr\\(", names(youth_test), value = TRUE)[1]
  if (is.na(p_col_youth)) p_col_youth <- grep("p", names(youth_test), value = TRUE, ignore.case = TRUE)[1]
  if (is.na(p_col_youth)) stop("Could not identify the p-value column for the Table A5 youth simple slope.")

  p_col_coef <- "Pr(>|t|)"

  if (panel == "Non-CHR") {
    title <- "Panel A: Non-CHR youth and their dyad partners"
    reference_label <- "Time (slope for Non-CHR dyad partner)"
    interaction_label <- "Non-CHR youth × time (Δ slope relative to Non-CHR dyad partner)"
    combination_label <- "Linear combination (slope for Non-CHR youth)"
  } else {
    title <- "Panel B: CHR youth and their dyad partners"
    reference_label <- "Time (slope for CHR dyad partner)"
    interaction_label <- "CHR youth × time (Δ slope relative to CHR dyad partner)"
    combination_label <- "Linear combination (slope for CHR youth)"
  }

  reference_estimate <- time_row$Estimate[1]
  reference_se <- time_row$`Std. Error`[1]
  reference_p <- time_row[[p_col_coef]][1]

  interaction_estimate <- youth_time_row$Estimate[1]
  interaction_se <- youth_time_row$`Std. Error`[1]
  interaction_p <- youth_time_row[[p_col_coef]][1]

  youth_estimate <- youth_test$Estimate[1]
  youth_se <- youth_test$`Std. Error`[1]
  youth_p <- youth_test[[p_col_youth]][1]

  if (!isTRUE(all.equal(
    unname(youth_estimate),
    unname(reference_estimate + interaction_estimate),
    tolerance = 1e-10
  ))) {
    stop("Table A5 youth simple slope does not equal second_60 + participant_A5Youth:second_60.")
  }

  tibble::tibble(
    Row = c(
      title,
      reference_label, "",
      interaction_label, "",
      combination_label, ""
    ),
    Value = c(
      "",
      fmt_est(reference_estimate, reference_p), fmt_se(reference_se),
      fmt_est(interaction_estimate, interaction_p), fmt_se(interaction_se),
      fmt_est(youth_estimate, youth_p), fmt_se(youth_se)
    )
  )
}

A5_col1 <- dplyr::bind_rows(extract_A5(m_NonCHR_A5_nocov, "Non-CHR"), extract_A5(m_CHR_A5_nocov, "CHR"))
A5_col2 <- dplyr::bind_rows(extract_A5(m_NonCHR_A5_cov, "Non-CHR", "youth"), extract_A5(m_CHR_A5_cov, "CHR", "youth"))
A5_col3 <- dplyr::bind_rows(extract_A5(m_NonCHR_A5_partner, "Non-CHR", "partner"), extract_A5(m_CHR_A5_partner, "CHR", "partner"))
TableA5 <- tibble::tibble(
  Row = A5_col1$Row,
  `(1) Separate models: no covariates` = A5_col1$Value,
  `(2) Separate models: youth covariates` = A5_col2$Value,
  `(3) Separate models: dyad-partner covariates` = A5_col3$Value
)
TableA5 <- dplyr::bind_rows(TableA5, tibble::tibble(
  Row = c("N observations (Non-CHR model)", "N dyads (Non-CHR model)", "N observations (CHR model)", "N dyads (CHR model)"),
  `(1) Separate models: no covariates` = as.character(c(stats::nobs(m_NonCHR_A5_nocov), model_ndyads(m_NonCHR_A5_nocov), stats::nobs(m_CHR_A5_nocov), model_ndyads(m_CHR_A5_nocov))),
  `(2) Separate models: youth covariates` = as.character(c(stats::nobs(m_NonCHR_A5_cov), model_ndyads(m_NonCHR_A5_cov), stats::nobs(m_CHR_A5_cov), model_ndyads(m_CHR_A5_cov))),
  `(3) Separate models: dyad-partner covariates` = as.character(c(stats::nobs(m_NonCHR_A5_partner), model_ndyads(m_NonCHR_A5_partner), stats::nobs(m_CHR_A5_partner), model_ndyads(m_CHR_A5_partner)))
))
A5_separate_slopes <- function(model, panel_label, adjustment_label, at_values = NULL) {
  args <- list(object = model, specs = ~ participant_A5, var = "second_60")
  if (!is.null(at_values)) args$at <- at_values
  tr <- do.call(emmeans::emtrends, args)
  d <- as.data.frame(summary(tr, infer = c(TRUE, TRUE)))
  col <- grep("\\.trend$|trend", names(d), value = TRUE)[1]
  d |>
    dplyr::transmute(
      Specification = adjustment_label,
      Group = paste(panel_label, ifelse(as.character(participant_A5) == "Youth", "youth", "dyad partner")),
      Separate_slope = .data[[col]],
      Separate_SE = SE
    )
}

A5_pooled_slopes <- dplyr::bind_rows(
  trend_summary(emmeans::emtrends(m2_nocov, ~ group4, var = "second_60"), "No covariates"),
  trend_summary(emmeans::emtrends(m2_cov, ~ group4, var = "second_60", at = list(X3_Age_c = 0, X1_Sex_1_c = 0)), "Youth covariates"),
  trend_summary(emmeans::emtrends(m2_partner, ~ group4, var = "second_60", at = list(c_demo_age_1_c = 0, c_gender_1_c = 0)), "Dyad-partner covariates")
) |>
  dplyr::transmute(Specification = as.character(Model), Group = Participant, Pooled_slope = Slope, Pooled_SE = SE)

A5_separate_slopes_all <- dplyr::bind_rows(
  A5_separate_slopes(m_NonCHR_A5_nocov, "Non-CHR", "No covariates"),
  A5_separate_slopes(m_NonCHR_A5_cov, "Non-CHR", "Youth covariates", list(X3_Age_c = 0, X1_Sex_1_c = 0)),
  A5_separate_slopes(m_NonCHR_A5_partner, "Non-CHR", "Dyad-partner covariates", list(c_demo_age_1_c = 0, c_gender_1_c = 0)),
  A5_separate_slopes(m_CHR_A5_nocov, "CHR", "No covariates"),
  A5_separate_slopes(m_CHR_A5_cov, "CHR", "Youth covariates", list(X3_Age_c = 0, X1_Sex_1_c = 0)),
  A5_separate_slopes(m_CHR_A5_partner, "CHR", "Dyad-partner covariates", list(c_demo_age_1_c = 0, c_gender_1_c = 0))
)

A5_pooled_vs_separate <- dplyr::left_join(
  A5_separate_slopes_all,
  A5_pooled_slopes,
  by = c("Specification", "Group")
) |>
  dplyr::mutate(Slope_difference_separate_minus_pooled = Separate_slope - Pooled_slope) |>
  dplyr::arrange(factor(Specification, levels = c("No covariates", "Youth covariates", "Dyad-partner covariates")), factor(Group, levels = unname(GROUP_LABELS)))

A5_coefficient_audit_one <- function(model, panel_label, specification_label) {
  if (!identical(levels(stats::model.frame(model)$participant_A5), c("Dyad partner", "Youth"))) {
    stop("Table A5 coefficient audit requires Dyad partner to be the reference category.")
  }
  time_term <- find_A5_term(model, "second_60")
  youth_time_term <- find_A5_term(model, "participant_A5Youth:second_60")
  beta <- lme4::fixef(model)
  tibble::tibble(
    Panel = panel_label,
    Specification = specification_label,
    Reference_category = "Dyad partner",
    `second_60 (dyad-partner slope)` = unname(beta[time_term]),
    `Youth × second_60 (youth - dyad partner)` = unname(beta[youth_time_term]),
    `Youth simple slope` = unname(beta[time_term] + beta[youth_time_term])
  )
}

A5_coefficient_audit <- dplyr::bind_rows(
  A5_coefficient_audit_one(m_NonCHR_A5_nocov, "Non-CHR", "No covariates"),
  A5_coefficient_audit_one(m_NonCHR_A5_cov, "Non-CHR", "Youth covariates"),
  A5_coefficient_audit_one(m_NonCHR_A5_partner, "Non-CHR", "Dyad-partner covariates"),
  A5_coefficient_audit_one(m_CHR_A5_nocov, "CHR", "No covariates"),
  A5_coefficient_audit_one(m_CHR_A5_cov, "CHR", "Youth covariates"),
  A5_coefficient_audit_one(m_CHR_A5_partner, "CHR", "Dyad-partner covariates")
)

write_workbook("TableA5.xlsx", list(
  TableA5 = TableA5,
  `Model audit` = model_audit(
    list(m_NonCHR_A5_nocov, m_NonCHR_A5_cov, m_NonCHR_A5_partner, m_CHR_A5_nocov, m_CHR_A5_cov, m_CHR_A5_partner),
    c("Non-CHR: no covariates", "Non-CHR: youth covariates", "Non-CHR: dyad-partner covariates", "CHR: no covariates", "CHR: youth covariates", "CHR: dyad-partner covariates")
  ),
  `Centering values` = tibble::tibble(
    Covariate = c("Youth age", "Youth sex", "Dyad-partner age", "Dyad-partner sex"),
    Source_variable = c("X3_Age", "X1_Sex_1", "c_demo_age_1", "c_gender_1"),
    Mean = c(table2_youth_age_mean, table2_youth_sex_mean, table2_partner_age_mean, table2_partner_sex_mean)
  ),
  `Pooled vs separate slopes` = A5_pooled_vs_separate,
  `Coefficient audit` = A5_coefficient_audit
))

################################################################################
### Table A6. Average Differences in Rating-Dial Scores
#
# Estimates average rating levels across the four participant groups over the full
# conflict discussion. These are mean-level models, not trajectory/slope models.
# Column (1): group only; Column (2): centered youth age/sex main effects;
# Column (3): centered dyad-partner age/sex main effects.
# No demographic×time terms are included because time is not part of this estimand.
# The resulting models are reused by Figure A2 and Table A7.
################################################################################
message("\n--- Table A6. Average Differences in Rating-Dial Scores ---")

youth_A6 <- center_vars(df_long, c("X3_Age", "X1_Sex_1"), c("rating", "group4", "Dyad_id", "X3_Age", "X1_Sex_1"))
df_A6 <- youth_A6$data
partner_A6 <- center_vars(df_A6, c("c_demo_age_1", "c_gender_1"), c("rating", "group4", "Dyad_id", "c_demo_age_1", "c_gender_1"))
df_A6 <- partner_A6$data

m_mean_nocov <- lmerTest::lmer(rating ~ group4 + (1 | Dyad_id), data = df_A6)
m_mean_cov <- lmerTest::lmer(rating ~ group4 + X3_Age_c + X1_Sex_1_c + (1 | Dyad_id), data = df_A6)
m_mean_partner <- lmerTest::lmer(rating ~ group4 + c_demo_age_1_c + c_gender_1_c + (1 | Dyad_id), data = df_A6)
mean_wanted <- c("group4TD_Youth", "group4CHR_Caregiver", "group4CHR_Youth", "(Intercept)")
mean_labels <- c("Non-CHR youth", "CHR dyad partner", "CHR youth", "Intercept (Non-CHR dyad partner)")
A6a <- extract_fixed_rows(m_mean_nocov, mean_wanted, mean_labels, 4)
A6b <- extract_fixed_rows(m_mean_cov, mean_wanted, mean_labels, 4)
A6c <- extract_fixed_rows(m_mean_partner, mean_wanted, mean_labels, 4)
TableA6 <- dplyr::bind_rows(
  tibble::tibble(
    Row = A6a$Row,
    `(1) No covariates` = A6a$Value,
    `(2) Youth covariates` = A6b$Value,
    `(3) Dyad-partner covariates` = A6c$Value
  ),
  tibble::tibble(
    Row = c("N observations", "N dyads"),
    `(1) No covariates` = as.character(c(stats::nobs(m_mean_nocov), model_ndyads(m_mean_nocov))),
    `(2) Youth covariates` = as.character(c(stats::nobs(m_mean_cov), model_ndyads(m_mean_cov))),
    `(3) Dyad-partner covariates` = as.character(c(stats::nobs(m_mean_partner), model_ndyads(m_mean_partner)))
  )
)
write_workbook("TableA6.xlsx", list(
  TableA6 = TableA6,
  `Model audit` = model_audit(
    list(m_mean_nocov, m_mean_cov, m_mean_partner),
    c("(1) No covariates", "(2) Youth covariates", "(3) Dyad-partner covariates")
  ),
  `Centering values` = combined_centering_table(youth_A6$means, partner_A6$means)
))

################################################################################
### Figure A2 and Table A7. Estimated Marginal Means and Pairwise Contrasts
#
# Figure A2 plots the estimated marginal mean rating for each participant group
# with 95% confidence intervals under the three Table A6 specifications.
# Table A7 reports all six pairwise EMM contrasts from those same model objects.
# Contrasts use adjust = "none"; the manuscript/Supplement should state that no
# multiple-comparison adjustment was applied.
################################################################################
message("\n--- Figure A2 and Table A7. Estimated Marginal Means and Pairwise Contrasts ---")

emm_nocov_FA2 <- emmeans::emmeans(m_mean_nocov, ~ group4)
emm_cov_FA2 <- emmeans::emmeans(m_mean_cov, ~ group4, at = list(X3_Age_c = 0, X1_Sex_1_c = 0))
emm_partner_FA2 <- emmeans::emmeans(m_mean_partner, ~ group4, at = list(c_demo_age_1_c = 0, c_gender_1_c = 0))
emm_plot <- function(x, model_label) {
  d <- as.data.frame(summary(x, infer = c(TRUE, FALSE)))
  cols <- get_emm_columns(d)
  d |>
    dplyr::transmute(
      model = .env$model_label,
      group4 = as.character(group4),
      emmean,
      lower = .data[[cols$lower]],
      upper = .data[[cols$upper]]
    )
}
plot_df_FA2 <- dplyr::bind_rows(
  emm_plot(emm_nocov_FA2, "No covariates"),
  emm_plot(emm_cov_FA2, "Youth covariates"),
  emm_plot(emm_partner_FA2, "Dyad-partner covariates")
) |>
  dplyr::mutate(
    group4 = factor(unname(GROUP_LABELS[group4]), levels = unname(GROUP_LABELS)),
    model = factor(model, levels = c("No covariates", "Youth covariates", "Dyad-partner covariates"))
  )
figFA2 <- ggplot2::ggplot(plot_df_FA2, ggplot2::aes(group4, emmean, shape = model, group = model)) +
  ggplot2::geom_point(position = ggplot2::position_dodge(.52), size = 2.2) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), position = ggplot2::position_dodge(.52), width = .12) +
  ggplot2::scale_shape_manual(values = c(`No covariates` = 16, `Youth covariates` = 17, `Dyad-partner covariates` = 15)) +
  ggplot2::labs(x = NULL, y = "Estimated marginal mean (rating dial)", shape = NULL) + ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1), legend.position = "bottom")
ggplot2::ggsave(file.path(results_dir, "FigureA2.png"), figFA2, width = 8.5, height = 4.8, dpi = 300, bg = "white")
write_workbook("FigureA2_data.xlsx", list(
  `FigureA2 data` = plot_df_FA2,
  `Model audit` = model_audit(
    list(m_mean_nocov, m_mean_cov, m_mean_partner),
    c("No covariates", "Youth covariates", "Dyad-partner covariates")
  )
))

################################################################################
### Table A7. Pairwise Differences in Estimated Marginal Means
#
# Purpose:
#   Reports all six pairwise comparisons among the four participant-group
#   estimated marginal means displayed in Figure A2.
#
# Source models:
#   Reuses the three Table A6 mean-level model objects exactly.
#
# Multiple comparisons:
#   Contrasts use adjust = "none"; no multiplicity correction is applied.
#
# Output:
#   TableA7.xlsx with the manuscript-facing table, raw contrasts, and model audit.
################################################################################
message("Creating Table A7: pairwise estimated-marginal-mean contrasts")

A7_nocov_raw <- as.data.frame(emmeans::contrast(emm_nocov_FA2, method = pairwise_contrasts, adjust = "none"))
A7_cov_raw <- as.data.frame(emmeans::contrast(emm_cov_FA2, method = pairwise_contrasts, adjust = "none"))
A7_partner_raw <- as.data.frame(emmeans::contrast(emm_partner_FA2, method = pairwise_contrasts, adjust = "none"))
A7a <- format_contrasts(A7_nocov_raw, 4)
A7b <- format_contrasts(A7_cov_raw, 4)
A7c <- format_contrasts(A7_partner_raw, 4)
TableA7 <- dplyr::bind_rows(
  tibble::tibble(
    Row = A7a$Row,
    `(1) No covariates` = A7a$Value,
    `(2) Youth covariates` = A7b$Value,
    `(3) Dyad-partner covariates` = A7c$Value
  ),
  tibble::tibble(
    Row = c("N observations", "N dyads"),
    `(1) No covariates` = as.character(c(stats::nobs(m_mean_nocov), model_ndyads(m_mean_nocov))),
    `(2) Youth covariates` = as.character(c(stats::nobs(m_mean_cov), model_ndyads(m_mean_cov))),
    `(3) Dyad-partner covariates` = as.character(c(stats::nobs(m_mean_partner), model_ndyads(m_mean_partner)))
  )
)
write_workbook("TableA7.xlsx", list(
  TableA7 = TableA7,
  `Raw contrasts` = dplyr::bind_rows(
    dplyr::mutate(A7_nocov_raw, Specification = "No covariates"),
    dplyr::mutate(A7_cov_raw, Specification = "Youth covariates"),
    dplyr::mutate(A7_partner_raw, Specification = "Dyad-partner covariates")
  ),
  `Model audit` = model_audit(
    list(m_mean_nocov, m_mean_cov, m_mean_partner),
    c("No covariates", "Youth covariates", "Dyad-partner covariates")
  )
))

################################################################################
### Figure A3 and Table A8. Estimated Marginal Slopes and Pairwise Contrasts
#
# Uses the exact pooled slope-model objects from main Table 2.
# Figure A3 displays participant-group-specific slopes with 95% confidence
# intervals for all three specifications. Adjusted slopes are evaluated at zero on
# the centered covariates (the analytic-sample centering values).
# Table A8 reports all six pairwise slope contrasts with adjust = "none".
################################################################################
message("\n--- Figure A3 and Table A8. Estimated Marginal Slopes and Pairwise Contrasts ---")

m_slope_nocov <- m2_nocov
m_slope_cov <- m2_cov
m_slope_partner <- m2_partner
trend_nocov <- emmeans::emtrends(m_slope_nocov, ~ group4, var = "second_60")
trend_cov <- emmeans::emtrends(m_slope_cov, ~ group4, var = "second_60", at = list(X3_Age_c = 0, X1_Sex_1_c = 0))
trend_partner <- emmeans::emtrends(m_slope_partner, ~ group4, var = "second_60", at = list(c_demo_age_1_c = 0, c_gender_1_c = 0))
plot_df_FA3 <- dplyr::bind_rows(
  trend_summary(trend_nocov, "No covariates"),
  trend_summary(trend_cov, "Youth covariates"),
  trend_summary(trend_partner, "Dyad-partner covariates")
) |>
  dplyr::mutate(
    group4 = factor(Participant, levels = unname(GROUP_LABELS)),
    model = factor(Model, levels = c("No covariates", "Youth covariates", "Dyad-partner covariates"))
  )
figFA3 <- ggplot2::ggplot(plot_df_FA3, ggplot2::aes(group4, Slope, shape = model, group = model)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = .4) +
  ggplot2::geom_point(position = ggplot2::position_dodge(.52), size = 2.2) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_CL, ymax = upper_CL), position = ggplot2::position_dodge(.52), width = .12) +
  ggplot2::scale_shape_manual(values = c(`No covariates` = 16, `Youth covariates` = 17, `Dyad-partner covariates` = 15)) +
  ggplot2::labs(x = NULL, y = "Estimated marginal slope (per 60 seconds)", shape = NULL) + ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1), legend.position = "bottom")
ggplot2::ggsave(file.path(results_dir, "FigureA3.png"), figFA3, width = 8.5, height = 4.8, dpi = 300, bg = "white")
write_workbook("FigureA3_data.xlsx", list(
  `FigureA3 data` = plot_df_FA3,
  `Model audit` = model_audit(
    list(m_slope_nocov, m_slope_cov, m_slope_partner),
    c("No covariates", "Youth covariates", "Dyad-partner covariates")
  )
))

################################################################################
### Table A8. Pairwise Differences in Estimated Marginal Slopes
#
# Purpose:
#   Reports all six pairwise comparisons among the participant-group-specific
#   linear slopes displayed in Figure A3.
#
# Source models:
#   Reuses the exact Table 2 slope-model objects so the marginal slopes and
#   contrasts are fully synchronized with the primary coefficient table.
#
# Multiple comparisons:
#   Contrasts use adjust = "none"; no multiplicity correction is applied.
#
# Output:
#   TableA8.xlsx with the manuscript-facing table, raw contrasts, and model audit.
################################################################################
message("Creating Table A8: pairwise estimated-marginal-slope contrasts")

A8_nocov_raw <- as.data.frame(emmeans::contrast(trend_nocov, method = pairwise_contrasts, adjust = "none"))
A8_cov_raw <- as.data.frame(emmeans::contrast(trend_cov, method = pairwise_contrasts, adjust = "none"))
A8_partner_raw <- as.data.frame(emmeans::contrast(trend_partner, method = pairwise_contrasts, adjust = "none"))
A8a <- format_contrasts(A8_nocov_raw, 3)
A8b <- format_contrasts(A8_cov_raw, 3)
A8c <- format_contrasts(A8_partner_raw, 3)
TableA8 <- dplyr::bind_rows(
  tibble::tibble(
    Row = A8a$Row,
    `(1) No covariates` = A8a$Value,
    `(2) Youth covariates` = A8b$Value,
    `(3) Dyad-partner covariates` = A8c$Value
  ),
  tibble::tibble(
    Row = c("N observations", "N dyads"),
    `(1) No covariates` = as.character(c(stats::nobs(m_slope_nocov), model_ndyads(m_slope_nocov))),
    `(2) Youth covariates` = as.character(c(stats::nobs(m_slope_cov), model_ndyads(m_slope_cov))),
    `(3) Dyad-partner covariates` = as.character(c(stats::nobs(m_slope_partner), model_ndyads(m_slope_partner)))
  )
)
write_workbook("TableA8.xlsx", list(
  TableA8 = TableA8,
  `Raw contrasts` = dplyr::bind_rows(
    dplyr::mutate(A8_nocov_raw, Specification = "No covariates"),
    dplyr::mutate(A8_cov_raw, Specification = "Youth covariates"),
    dplyr::mutate(A8_partner_raw, Specification = "Dyad-partner covariates")
  ),
  `Model audit` = model_audit(
    list(m_slope_nocov, m_slope_cov, m_slope_partner),
    c("No covariates", "Youth covariates", "Dyad-partner covariates")
  )
))

################################################################################
### Figures A4-A5. LOESS Span Sensitivity
#
# Descriptive smoothing-sensitivity figures using fixed spans .25, .50, and .75.
# Figure A4 is Non-CHR; Figure A5 is CHR.
# These displays show how apparent local shape changes with the smoothing window.
# They are descriptive and are not used as inferential tests of nonlinearity.
################################################################################
message("\n--- Figures A4-A5. LOESS Span Sensitivity ---")

make_loess_sensitivity <- function(panel_code, filename, colors) {
  data <- df_complete |>
    dplyr::mutate(panel2 = to_panel(CHR)) |>
    dplyr::filter(.data$panel2 == .env$panel_code) |>
    dplyr::select(second, CrT_Rscr, RrT_Rscr) |>
    tidyr::pivot_longer(c(CrT_Rscr, RrT_Rscr), names_to = "source", values_to = "rating") |>
    dplyr::mutate(Participant = factor(ifelse(source == "CrT_Rscr", "Youth", "Dyad partner"), levels = PARTICIPANT_LEVELS)) |>
    dplyr::filter(!is.na(second), !is.na(rating))
  spans <- c(.25, .50, .75)
  lt <- c(Youth = "solid", `Dyad partner` = "longdash")
  make_panel <- function(span_value, ylab) ggplot2::ggplot(data, ggplot2::aes(second, rating, color = Participant)) +
    ggplot2::stat_summary(ggplot2::aes(group = Participant), geom = "line", fun = mean, linewidth = .25, show.legend = FALSE) +
    ggplot2::geom_smooth(ggplot2::aes(group = Participant, linetype = Participant), method = "loess", span = span_value, linewidth = 1.3, se = TRUE, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = colors) + ggplot2::scale_linetype_manual(values = lt) +
    ggplot2::scale_x_continuous(breaks = seq(0, 600, 120)) + ggplot2::labs(x = "Time (in seconds)", y = ylab, title = sprintf("Span = %.2f", span_value)) +
    base_trajectory_theme + ggplot2::theme(legend.position = "none")
  raw <- list(make_panel(spans[1], "Rating dial scores"), make_panel(spans[2], NULL), make_panel(spans[3], NULL))
  yr <- range(unlist(lapply(raw, plot_y_range)))
  pad <- max(diff(yr) * .05, .1)
  limits <- yr + c(-pad, pad)
  breaks <- pretty(limits, 5)
  plots <- lapply(raw, function(p) p + ggplot2::scale_y_continuous(breaks = breaks) + ggplot2::coord_cartesian(xlim = c(0, 600), ylim = limits))
  legend <- make_shared_legend(colors, lt, PARTICIPANT_LEVELS)
  fig <- cowplot::plot_grid(cowplot::plot_grid(plotlist = plots, labels = c("A", "B", "C"), ncol = 3), legend, ncol = 1, rel_heights = c(1, .10))
  ggplot2::ggsave(file.path(results_dir, filename), fig, width = 14, height = 5.5, dpi = 300, bg = "white")
  invisible(fig)
}
message("Creating Figure A4: Non-CHR LOESS span sensitivity")
FigureA4 <- make_loess_sensitivity("TD", "FigureA4.png", c(Youth = "black", `Dyad partner` = "gray50"))
message("Creating Figure A5: CHR LOESS span sensitivity")
FigureA5 <- make_loess_sensitivity("CHR", "FigureA5.png", colors_chr)

################################################################################
### Table A9. Generalized Additive Mixed Models (GAMMs)
#
# Formal Aim 2 follow-up assessing whether trajectories support nonlinear shape.
# Each model uses group-specific penalized thin-plate smooths of second (k = 20),
# a dyad random intercept, AR(1) residual correlation within participant, and REML.
# Column (1): no demographics; Column (2): centered youth age/sex main effects;
# Column (3): centered dyad-partner age/sex main effects.
# For each smooth, edf describes curve complexity; edf near 1 is approximately
# linear. The smooth-term p-value is an overall smooth test, NOT a direct test of
# nonlinearity. Raw mgcv tables and model audits are exported alongside Table A9.
################################################################################
message("\n--- Table A9. Generalized Additive Mixed Models (GAMMs) ---")

df_long_A9 <- prepare_long(df_long)
if (!"subj_id" %in% names(df_long_A9)) df_long_A9$subj_id <- interaction(df_long_A9$Dyad_id, df_long_A9$participant, drop = TRUE, lex.order = TRUE)
df_long_A9$subj_id <- factor(df_long_A9$subj_id)
df_model_A9_nocov <- df_long_A9 |>
  dplyr::filter(!is.na(rating), !is.na(second), !is.na(group4), !is.na(Dyad_id), !is.na(subj_id)) |>
  dplyr::group_by(subj_id) |> dplyr::filter(dplyr::n() >= 3) |> dplyr::ungroup() |> dplyr::arrange(subj_id, second) |> droplevels()

a9_youth <- center_vars(
  df_model_A9_nocov, c("X3_Age", "X1_Sex_1"),
  c("rating", "second", "group4", "Dyad_id", "subj_id", "X3_Age", "X1_Sex_1")
)
df_model_A9_cov <- a9_youth$data[a9_youth$complete, , drop = FALSE] |> dplyr::arrange(subj_id, second) |> droplevels()
A9_youth_age_mean <- unname(a9_youth$means["X3_Age"])
A9_youth_sex_mean <- unname(a9_youth$means["X1_Sex_1"])
covars_A9 <- c("X3_Age_c", "X1_Sex_1_c")

a9_partner <- center_vars(
  df_model_A9_nocov, c("c_demo_age_1", "c_gender_1"),
  c("rating", "second", "group4", "Dyad_id", "subj_id", "c_demo_age_1", "c_gender_1")
)
df_model_A9_partner <- a9_partner$data[a9_partner$complete, , drop = FALSE] |> dplyr::arrange(subj_id, second) |> droplevels()
A9_partner_age_mean <- unname(a9_partner$means["c_demo_age_1"])
A9_partner_sex_mean <- unname(a9_partner$means["c_gender_1"])
covars_A9_partner <- c("c_demo_age_1_c", "c_gender_1_c")

fit_gamm <- function(data, covariates = character(), k = 20, bs = "tp") {
  rhs <- paste(c("group4", covariates), collapse = " + ")
  form <- stats::as.formula(paste0("rating ~ ", rhs, " + s(second, by = group4, bs = '", bs, "', k = ", k, ")"))
  mgcv::gamm(formula = form, random = list(Dyad_id = ~ 1), correlation = nlme::corAR1(form = ~ second | subj_id), data = data, method = "REML")
}
m_gamm_A9_nocov <- fit_gamm(df_model_A9_nocov)
m_gamm_A9_cov <- fit_gamm(df_model_A9_cov, covars_A9)
m_gamm_A9_partner <- fit_gamm(df_model_A9_partner, covars_A9_partner)

A9_youth_formula <- formula_text(m_gamm_A9_cov$gam)
A9_partner_formula <- formula_text(m_gamm_A9_partner$gam)
if (!all(vapply(c("X3_Age_c", "X1_Sex_1_c"), function(z) grepl(z, A9_youth_formula, fixed = TRUE), logical(1)))) {
  stop("Table A9 youth-adjusted GAMM is missing an expected youth covariate.")
}
if (any(vapply(c("c_demo_age_1", "c_gender_1"), function(z) grepl(z, A9_youth_formula, fixed = TRUE), logical(1)))) {
  stop("Table A9 youth-adjusted GAMM unexpectedly contains a dyad-partner covariate.")
}
if (!all(vapply(c("c_demo_age_1_c", "c_gender_1_c"), function(z) grepl(z, A9_partner_formula, fixed = TRUE), logical(1)))) {
  stop("Table A9 dyad-partner-adjusted GAMM is missing an expected dyad-partner covariate.")
}
if (any(vapply(c("X3_Age", "X1_Sex_1"), function(z) grepl(z, A9_partner_formula, fixed = TRUE), logical(1)))) {
  stop("Table A9 dyad-partner-adjusted GAMM unexpectedly contains a youth covariate.")
}

extract_gamm_parametric <- function(fit, digits = 3) {
  d <- as.data.frame(summary(fit$gam)$p.table) |>
    tibble::rownames_to_column("term")

  wanted <- c("(Intercept)", "group4TD_Youth", "group4CHR_Caregiver", "group4CHR_Youth")
  labels <- c(
    "Intercept (Non-CHR dyad partner reference)",
    "Non-CHR youth vs Non-CHR dyad partner",
    "CHR dyad partner vs Non-CHR dyad partner",
    "CHR youth vs Non-CHR dyad partner"
  )

  missing <- setdiff(wanted, d$term)
  if (length(missing)) stop("Missing expected GAMM parametric coefficient(s): ", paste(missing, collapse = ", "))

  d <- d |>
    dplyr::filter(term %in% wanted) |>
    dplyr::mutate(term = factor(term, levels = wanted)) |>
    dplyr::arrange(term)

  stopifnot(nrow(d) == 4, identical(as.character(d$term), wanted))

  purrr::map2_dfr(labels, seq_along(labels), function(label, i) {
    tibble::tibble(
      Row = c(label, ""),
      Value = c(
        fmt_est(d$Estimate[i], d$`Pr(>|t|)`[i], digits),
        fmt_se(d$`Std. Error`[i], digits)
      )
    )
  })
}

extract_gamm_smooths <- function(fit) {
  d <- as.data.frame(summary(fit$gam)$s.table) |>
    tibble::rownames_to_column("smooth")

  pcol <- grep("^p-value$|^p\\.value$", names(d), value = TRUE, ignore.case = TRUE)[1]
  if (length(pcol) == 0 || is.na(pcol)) stop("Could not identify the p-value column in the GAMM smooth table.")

  smooth_order <- GROUP_LEVELS
  smooth_labels <- paste0("s(second): ", unname(GROUP_LABELS[smooth_order]))

  identify_group <- function(x) {
    hits <- smooth_order[vapply(smooth_order, function(g) grepl(g, x, fixed = TRUE), logical(1))]
    if (length(hits) != 1) stop("Could not uniquely identify the participant group for smooth term: ", x)
    hits
  }

  d <- d |>
    dplyr::filter(grepl(paste(smooth_order, collapse = "|"), smooth)) |>
    dplyr::mutate(
      group_key = vapply(smooth, identify_group, character(1)),
      group_key = factor(group_key, levels = smooth_order),
      Row = paste0("s(second): ", unname(GROUP_LABELS[as.character(group_key)])),
      p = .data[[pcol]],
      Value = sprintf("edf=%.3f, F=%.3f (p=%s)", edf, F, fmt_p(p))
    ) |>
    dplyr::arrange(group_key) |>
    dplyr::transmute(Row, Value)

  stopifnot(nrow(d) == 4, identical(d$Row, smooth_labels))
  d
}

extract_gamm_summary <- function(fit, n_dyads) {
  s <- summary(fit$gam)
  n <- if (!is.null(fit$gam$n)) fit$gam$n else length(fit$gam$y)
  tibble::tibble(
    Row = c("Adj. R-squared", "Scale estimate", "N observations", "N dyads"),
    Value = c(
      sprintf("%.4f", s$r.sq),
      sprintf("%.4f", s$scale),
      sprintf("%.0f", n),
      as.character(n_dyads)
    )
  )
}

build_gamm_column <- function(fit, n_dyads) dplyr::bind_rows(
  tibble::tibble(Row = "(A) Parametric group coefficients", Value = ""),
  extract_gamm_parametric(fit),
  tibble::tibble(Row = "(B) Group-specific smooth terms", Value = ""),
  extract_gamm_smooths(fit),
  tibble::tibble(Row = "(C) Model summary", Value = ""),
  extract_gamm_summary(fit, n_dyads)
)

A9a <- build_gamm_column(m_gamm_A9_nocov, dplyr::n_distinct(df_model_A9_nocov$Dyad_id))
A9b <- build_gamm_column(m_gamm_A9_cov, dplyr::n_distinct(df_model_A9_cov$Dyad_id))
A9c <- build_gamm_column(m_gamm_A9_partner, dplyr::n_distinct(df_model_A9_partner$Dyad_id))
stopifnot(identical(A9a$Row, A9b$Row), identical(A9a$Row, A9c$Row))
TableA9 <- tibble::tibble(
  Row = A9a$Row,
  `(1) No covariates` = A9a$Value,
  `(2) Youth covariates` = A9b$Value,
  `(3) Dyad-partner covariates` = A9c$Value
)
A9_model_audit <- tibble::tibble(
  Specification = c("No covariates", "Youth covariates", "Dyad-partner covariates"),
  Formula = c(formula_text(m_gamm_A9_nocov$gam), formula_text(m_gamm_A9_cov$gam), formula_text(m_gamm_A9_partner$gam)),
  N_observations = c(nrow(df_model_A9_nocov), nrow(df_model_A9_cov), nrow(df_model_A9_partner)),
  N_dyads = c(dplyr::n_distinct(df_model_A9_nocov$Dyad_id), dplyr::n_distinct(df_model_A9_cov$Dyad_id), dplyr::n_distinct(df_model_A9_partner$Dyad_id)),
  N_participants = c(dplyr::n_distinct(df_model_A9_nocov$subj_id), dplyr::n_distinct(df_model_A9_cov$subj_id), dplyr::n_distinct(df_model_A9_partner$subj_id))
)
A9_centering_values <- combined_centering_table(a9_youth$means, a9_partner$means)
A9_raw_parametric <- dplyr::bind_rows(
  dplyr::mutate(tibble::rownames_to_column(as.data.frame(summary(m_gamm_A9_nocov$gam)$p.table), "term"), Specification = "No covariates"),
  dplyr::mutate(tibble::rownames_to_column(as.data.frame(summary(m_gamm_A9_cov$gam)$p.table), "term"), Specification = "Youth covariates"),
  dplyr::mutate(tibble::rownames_to_column(as.data.frame(summary(m_gamm_A9_partner$gam)$p.table), "term"), Specification = "Dyad-partner covariates")
) |>
  dplyr::select(Specification, dplyr::everything())
A9_raw_smooths <- dplyr::bind_rows(
  dplyr::mutate(tibble::rownames_to_column(as.data.frame(summary(m_gamm_A9_nocov$gam)$s.table), "smooth"), Specification = "No covariates"),
  dplyr::mutate(tibble::rownames_to_column(as.data.frame(summary(m_gamm_A9_cov$gam)$s.table), "smooth"), Specification = "Youth covariates"),
  dplyr::mutate(tibble::rownames_to_column(as.data.frame(summary(m_gamm_A9_partner$gam)$s.table), "smooth"), Specification = "Dyad-partner covariates")
) |>
  dplyr::select(Specification, dplyr::everything())
write_workbook(
  "TableA9.xlsx",
  list(
    TableA9 = TableA9,
    `Model audit` = A9_model_audit,
    `Centering values` = A9_centering_values,
    `Raw parametric` = A9_raw_parametric,
    `Raw smooths` = A9_raw_smooths
  )
)

################################################################################
### Figures A6-A7. Covariate-Adjusted GAMM-Fitted Trajectories
#
# Population-level fitted trajectories from the two adjusted Table A9 GAMMs.
# Figure A6 uses the youth-covariate-adjusted model.
# Figure A7 uses the dyad-partner-covariate-adjusted model.
# Predictions exclude dyad random effects and evaluate centered age/sex covariates
# at zero, corresponding to their analytic-sample means on the centered scale.
################################################################################
message("\n--- Figures A6-A7. Covariate-Adjusted GAMM-Fitted Trajectories ---")

make_gamm_trajectory_figure <- function(fit, data, covariates, figure_name, model_name, covariate_labels) {
  prediction <- expand.grid(
    second = seq(min(data$second, na.rm = TRUE), max(data$second, na.rm = TRUE), length.out = 300),
    group4 = GROUP_LEVELS,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  prediction$group4 <- factor(prediction$group4, levels = GROUP_LEVELS)
  for (v in covariates) prediction[[v]] <- 0

  pred <- stats::predict(fit$gam, newdata = prediction, type = "response", se.fit = TRUE)
  prediction <- prediction |>
    dplyr::mutate(
      fitted_rating = as.numeric(pred$fit),
      fitted_se = as.numeric(pred$se.fit),
      lower_95 = fitted_rating - 1.96 * fitted_se,
      upper_95 = fitted_rating + 1.96 * fitted_se,
      participant = ifelse(grepl("Youth", as.character(group4)), "Youth", "Dyad partner"),
      facet_label = factor(
        as.character(group4),
        levels = c("TD_Caregiver", "TD_Youth", "CHR_Caregiver", "CHR_Youth"),
        labels = c("Non-CHR dyad partner", "Non-CHR youth", "CHR dyad partner", "CHR youth")
      )
    )

  fig <- ggplot2::ggplot(prediction, ggplot2::aes(second, fitted_rating, color = group4, fill = group4, linetype = participant)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_95, ymax = upper_95), alpha = .18, color = NA, show.legend = FALSE) +
    ggplot2::geom_line(linewidth = 1.3, show.legend = FALSE) +
    ggplot2::facet_wrap(~ facet_label, ncol = 2) +
    ggplot2::scale_color_manual(values = COLORS_ALL) +
    ggplot2::scale_fill_manual(values = COLORS_ALL) +
    ggplot2::scale_linetype_manual(values = c(Youth = "solid", `Dyad partner` = "longdash")) +
    ggplot2::labs(x = "Time (in seconds)", y = "Predicted rating dial scores") +
    base_trajectory_theme +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))

  ggplot2::ggsave(file.path(results_dir, paste0(figure_name, ".png")), fig, width = 9, height = 7, dpi = 300, bg = "white")
  settings <- tibble::tibble(
    Setting = c("Underlying model", covariate_labels),
    Value = c(model_name, paste0(covariates, " = 0 (analytic-sample mean on centered scale)"))
  )
  sheets <- list(prediction, settings)
  names(sheets) <- c(paste0(figure_name, " data"), "Prediction settings")
  write_workbook(paste0(figure_name, "_data.xlsx"), sheets)
  invisible(fig)
}

message("Creating Figure A6: youth-covariate-adjusted GAMM trajectories")
FigureA6 <- make_gamm_trajectory_figure(
  m_gamm_A9_cov,
  df_model_A9_cov,
  covars_A9,
  "FigureA6",
  "m_gamm_A9_cov",
  c("Youth age", "Youth sex")
)

message("Creating Figure A7: dyad-partner-covariate-adjusted GAMM trajectories")
FigureA7 <- make_gamm_trajectory_figure(
  m_gamm_A9_partner,
  df_model_A9_partner,
  covars_A9_partner,
  "FigureA7",
  "m_gamm_A9_partner",
  c("Dyad-partner age", "Dyad-partner sex")
)

################################################################################
### Tables A10-A11. GAMM Spline Sensitivity
#
# Tests whether the near-linear GAMM conclusion depends on spline implementation.
# For each adjusted GAMM, cross 3 bases (TP, CR, PS) with k = 10, 20, 40.
# Table A10 uses youth covariates; Table A11 uses dyad-partner covariates.
# The primary TP/k=20 Table A9 model is reused exactly, so each table fits only
# eight new GAMMs. AIC is retained in a dedicated audit sheet rather than the
# manuscript-facing table.
################################################################################
message("\n--- Tables A10-A11. GAMM Spline Sensitivity ---")

run_gamm_sensitivity <- function(primary_fit, data, covariates, table_name, model_name, covariate_description, centering_values) {
  k_grid <- c(10, 20, 40)
  bs_grid <- c("tp", "cr", "ps")
  basis_labels <- c(tp = "TP", cr = "CR", ps = "PS")
  results <- list()

  for (bs_value in bs_grid) {
    for (k_value in k_grid) {
      nm <- paste0("bs_", bs_value, "_k_", k_value)
      message(table_name, ": fitting/collecting ", nm)
      reuse <- identical(bs_value, "tp") && identical(k_value, 20)
      fit <- if (reuse) primary_fit else fit_gamm(data, covariates, k = k_value, bs = bs_value)

      results[[nm]] <- list(
        bs = bs_value,
        k = k_value,
        reused_main_model = reuse,
        AIC = as.numeric(stats::AIC(fit$lme)),
        table = build_gamm_column(fit, dplyr::n_distinct(data$Dyad_id))
      )

      if (!reuse) {
        rm(fit)
        invisible(gc())
      }
    }
  }

  if (length(results) != 9) stop(table_name, " did not produce all nine spline specifications.")
  if (sum(vapply(results, function(x) isTRUE(x$reused_main_model), logical(1))) != 1) {
    stop(table_name, " did not reuse the primary tp/k=20 model exactly once.")
  }

  expected_names <- unlist(lapply(bs_grid, function(bs) paste0("bs_", bs, "_k_", k_grid)), use.names = FALSE)
  if (!identical(names(results), expected_names)) stop(table_name, " sensitivity specifications are not in the expected manuscript order.")

  row_reference <- results[[1]]$table$Row
  if (!all(vapply(results, function(x) identical(x$table$Row, row_reference), logical(1)))) {
    stop(table_name, " sensitivity columns do not have identical row ordering.")
  }

  column_names <- paste0("(", seq_along(results), ")")
  manuscript_table <- tibble::tibble(
    Row = c("Spline basis", "Basis dimension", row_reference)
  )

  for (i in seq_along(results)) {
    x <- results[[i]]
    manuscript_table[[column_names[i]]] <- c(
      unname(basis_labels[x$bs]),
      as.character(x$k),
      x$table$Value
    )
  }

  aic_summary <- tibble::tibble(
    Specification = names(results),
    Column = column_names,
    Basis = vapply(results, function(x) unname(basis_labels[x$bs]), character(1)),
    k = vapply(results, function(x) x$k, numeric(1)),
    Reused_Table_A9_model = vapply(results, function(x) x$reused_main_model, logical(1)),
    AIC = vapply(results, function(x) x$AIC, numeric(1))
  ) |>
    dplyr::arrange(AIC)

  model_setup <- tibble::tibble(
    Feature = c(
      "Analytic sample", "Covariates", "Random effect", "Residual correlation",
      "Estimation", "Primary Table A9 spline", "Primary Table A9 k",
      paste0("New GAMMs fitted for ", table_name)
    ),
    Value = c(
      deparse(substitute(data)), covariate_description, "Random intercept for Dyad_id",
      "AR(1) within participant", "REML", "Thin-plate regression spline (tp)", "20",
      paste0("8; tp/k=20 reuses ", model_name)
    )
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, table_name)
  openxlsx::writeData(wb, table_name, manuscript_table)
  openxlsx::setColWidths(wb, table_name, cols = 1:ncol(manuscript_table), widths = c(48, rep(18, 9)))

  for (nm in names(results)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, results[[nm]]$table)
    openxlsx::setColWidths(wb, nm, cols = 1:2, widths = c(52, 30))
  }

  openxlsx::addWorksheet(wb, "AIC_summary")
  openxlsx::writeData(wb, "AIC_summary", aic_summary)
  openxlsx::setColWidths(wb, "AIC_summary", cols = 1:ncol(aic_summary), widths = "auto")

  openxlsx::addWorksheet(wb, "Model setup")
  openxlsx::writeData(wb, "Model setup", model_setup)
  openxlsx::setColWidths(wb, "Model setup", cols = 1:2, widths = c(28, 65))

  openxlsx::addWorksheet(wb, "Centering values")
  openxlsx::writeData(wb, "Centering values", centering_values)
  openxlsx::setColWidths(wb, "Centering values", cols = 1:ncol(centering_values), widths = "auto")

  openxlsx::saveWorkbook(wb, file.path(results_dir, paste0(table_name, ".xlsx")), overwrite = TRUE)
  invisible(results)
}

A10_centering_values <- tibble::tibble(
  Covariate = c("Youth age", "Youth sex"),
  Source_variable = c("X3_Age", "X1_Sex_1"),
  Mean = c(A9_youth_age_mean, A9_youth_sex_mean)
)
################################################################################
### Table A10. GAMM Spline Sensitivity — Youth Covariates
#
# Purpose:
#   Tests whether the approximately linear youth-adjusted GAMM trajectories are
#   robust to spline basis (TP/CR/PS) and basis dimension (k = 10/20/40).
#
# Reproducibility safeguard:
#   The TP/k=20 column reuses the exact youth-adjusted Table A9 model; the script
#   later asserts that every displayed value matches the corresponding A9 column.
#
# Output:
#   TableA10.xlsx with the manuscript table, one sheet per specification,
#   AIC_summary, model setup, and centering values.
################################################################################
message("Creating Table A10: GAMM spline sensitivity with youth covariates")
sens_results_A10 <- run_gamm_sensitivity(
  m_gamm_A9_cov,
  df_model_A9_cov,
  covars_A9,
  "TableA10",
  "m_gamm_A9_cov",
  "Mean-centered youth age and youth sex",
  A10_centering_values
)

A11_centering_values <- tibble::tibble(
  Covariate = c("Dyad-partner age", "Dyad-partner sex"),
  Source_variable = c("c_demo_age_1", "c_gender_1"),
  Mean = c(A9_partner_age_mean, A9_partner_sex_mean)
)
################################################################################
### Table A11. GAMM Spline Sensitivity — Dyad-Partner Covariates
#
# Purpose:
#   Repeats Table A10 for the dyad-partner-adjusted GAMM to test whether its
#   approximately linear trajectories are robust to spline basis and k.
#
# Reproducibility safeguard:
#   The TP/k=20 column reuses the exact dyad-partner-adjusted Table A9 model; the
#   script later asserts exact equality with the corresponding A9 column.
#
# Output:
#   TableA11.xlsx with the manuscript table, one sheet per specification,
#   AIC_summary, model setup, and centering values.
################################################################################
message("Creating Table A11: GAMM spline sensitivity with dyad-partner covariates")
sens_results_A11 <- run_gamm_sensitivity(
  m_gamm_A9_partner,
  df_model_A9_partner,
  covars_A9_partner,
  "TableA11",
  "m_gamm_A9_partner",
  "Mean-centered dyad-partner age and dyad-partner sex",
  A11_centering_values
)


# Cross-table reproducibility check: the TP/k=20 sensitivity specification is
# literally the Table A9 primary adjusted model, so every displayed value must
# match its Table A9 column exactly (including adjusted R-squared).
if (!identical(sens_results_A10[["bs_tp_k_20"]]$table, A9b)) {
  stop("Table A10 TP/k=20 does not exactly match Table A9 youth-adjusted model.")
}
if (!identical(sens_results_A11[["bs_tp_k_20"]]$table, A9c)) {
  stop("Table A11 TP/k=20 does not exactly match Table A9 dyad-partner-adjusted model.")
}

writeLines(
  c(
    "Primary linear trajectory tables use three specifications: no covariates, mean-centered youth covariates with demographic x time interactions, and mean-centered dyad-partner covariates with demographic x time interactions.",
    "Youth and dyad-partner demographic adjustments are estimated separately to reduce the risk of collinearity and overfitting.",
    "Four-group order: Non-CHR dyad partner, Non-CHR youth, CHR dyad partner, CHR youth.",
    "Table 1 descriptive slope variables represent change across the full 10-minute conversation; Table 2 and marginal-slope outputs report change per 60 seconds.",
    "Mean-level models (Table A6/Figure A2/Table A7) include centered demographic main effects only.",
    "Table A5 uses dyad partner as the within-stratum participant reference; youth slopes are linear combinations of the time and Youth x time coefficients.",
    "Supplement figure sequence: A1 relationship composition; A2 marginal means; A3 marginal slopes; A4 Non-CHR LOESS spans; A5 CHR LOESS spans; A6 youth-adjusted GAMM trajectories; A7 dyad-partner-adjusted GAMM trajectories.",
    "Table A9 reports unadjusted, youth-adjusted, and dyad-partner-adjusted GAMMs with group-specific smooths, dyad random intercepts, within-participant AR(1), and REML.",
    "Table A10 evaluates spline-basis/basis-dimension sensitivity for the youth-adjusted GAMM; Table A11 repeats this for the dyad-partner-adjusted GAMM.",
    "The TP/k=20 specifications in Tables A10/A11 reuse the corresponding Table A9 models exactly and are asserted to match all displayed values.",
    "Tables A7 and A8 report six pairwise contrasts without multiplicity adjustment.",
    "Table A2 requires two private local exclusion IDs but does not print or hard-code those IDs in public output."
  ),
  file.path(results_dir, "analysis_specification.txt")
)

# Reproducibility record

pkg_versions <- tibble::tibble(Package = required_packages, Version = vapply(required_packages, function(x) as.character(utils::packageVersion(x)), character(1)))
utils::write.csv(pkg_versions, file.path(results_dir, "package_versions.csv"), row.names = FALSE)
writeLines(capture.output(utils::sessionInfo()), file.path(results_dir, "sessionInfo.txt"))
message("Analysis complete. Results saved to: ", normalizePath(results_dir, mustWork = FALSE))
