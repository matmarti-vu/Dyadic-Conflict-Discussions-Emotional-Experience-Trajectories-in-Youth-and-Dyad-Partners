################################################################################
##### Setting up and loading packages

pkgs <- c(
  "dplyr","tidyr","tibble","purrr",
  "ggplot2",
  "lmerTest","emmeans",
  "mgcv","nlme",
  "psych",
  "openxlsx", "caret"
)

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

invisible(lapply(pkgs, library, character.only = TRUE))


################################################################################
##### Loading the data

data_dir <- "C:/Users/Path to data"
results_dir <- "C:/Users/Path to results"

if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

df_complete    <- read.csv(file.path(data_dir, "df_complete.csv"))
df_completeALT <- read.csv(file.path(data_dir, "df_completeALT.csv"))

to_panel <- function(chr){
  x <- toupper(trimws(as.character(chr)))
  ifelse(x %in% c("1","CHR","TRUE"), "CHR", "TD")
}

if (!("panel" %in% names(df_complete)) && ("CHR" %in% names(df_complete))) {
  df_complete <- df_complete %>% mutate(panel = to_panel(CHR))
}
if (!("panel" %in% names(df_completeALT)) && ("CHR" %in% names(df_completeALT))) {
  df_completeALT <- df_completeALT %>% mutate(panel = to_panel(CHR))
}

### Description databases
  
### df_complete:
# df_complete is the primary second-by-second analytic dataset used for the 
# main manuscript analyses. It contains one row per dyad × second for dyads 
# with complete rating-dial information for all four dial series across the 
# full interaction (typically 600 seconds): youth self-rating (CrT_Rscr), 
# youth rating of caregiver (CrR_Rscr), caregiver self-rating (RrT_Rscr), 
# and caregiver rating of youth (RrC_Rscr) (and their centered versions if 
# available, e.g., *_Cscr). The dataset includes identifiers 
# (Dyad_id, youth/caregiver IDs if available), diagnostic group 
# (CHR, later recoded into panel = TD vs CHR), the time index (second), 
# and participant covariates used for adjusted models (youth age X3_Age, 
# youth sex indicator X1_Sex_1, caregiver age c_demo_age_1, caregiver 
# gender indicator c_gender_1). It also includes derived summary variables 
# used in follow-up descriptives (e.g., PosResFra and related positivity 
# resonance fractions), and convenience variables created during preprocessing 
# (binary indicators for income and race categories, etc.)

# Intended use: main pooled linear mixed-effects models, LOESS visualizations, 
# GAMM robustness checks, and derived descriptive indices of 
# synchrony/instability.

  
### df_completeALT:
# df_completeALT is an alternative second-by-second dataset used for 
# sensitivity analyses where we relax the strict “all-four-series complete” 
# requirement. It retains dyads/seconds with at least one available rating 
# series (as defined in your earlier filtering logic) and is used to evaluate 
# whether inferences are sensitive to the stricter completeness criterion 
# used in df_complete. It contains the same key identifiers (Dyad_id, second, 
# CHR/panel) and covariates, but may include additional dyads/seconds with 
# partial missingness in some dial series.

# Intended use: robustness checks (e.g., Table A1 using an “uneven” sample.)


################################################################################
##### Creating df_long and df_longALT

# df_long is the participant-level long-format panel created by stacking 
# youth and caregiver self-ratings (CrT_Rscr, RrT_Rscr) from df_complete. 
# Each row is a participant (youth or caregiver) × second observation, 
# with rating as the outcome, participant indicating Youth/Caregiver, 
# panel indicating TD/CHR, group4 as a four-level factor 
# (TD_Caregiver, TD_Youth, CHR_Caregiver, CHR_Youth) with a fixed level order, 
# and second_60 = second/60 used for time scaling in linear models.


### Creating df_long

df_long <- df_complete %>%
  select(
    Dyad_id, second, panel,
    CrT_Rscr, RrT_Rscr,
    X3_Age, X1_Sex_1,
    c_demo_age_1, c_gender_1
  ) %>%
  pivot_longer(
    cols = c(CrT_Rscr, RrT_Rscr),
    names_to = "role",
    values_to = "rating"
  ) %>%
  mutate(
    participant = ifelse(role == "CrT_Rscr", "Youth", "Caregiver"),
    group4 = factor(
      paste0(panel, "_", participant),
      levels = c("TD_Caregiver", "TD_Youth",
                 "CHR_Caregiver", "CHR_Youth")
    ),
    second_60 = second / 60,
    Dyad_id = factor(Dyad_id)
  ) %>%
  filter(!is.na(rating))

describe(df_complete[, c("average_score_c", "average_score")])

df_long %>%
  group_by(panel, participant) %>%
  summarise(
    slope = coef(lm(rating ~ second_60))[2],
    .groups = "drop"
  )


### df_longALT

df_longALT <- df_completeALT %>%
  select(
    Dyad_id, second, panel,
    CrT_Rscr, RrT_Rscr,
    X3_Age, X1_Sex_1,
    c_demo_age_1, c_gender_1
  ) %>%
  pivot_longer(
    cols = c(CrT_Rscr, RrT_Rscr),
    names_to = "role",
    values_to = "rating"
  ) %>%
  mutate(
    participant = ifelse(role == "CrT_Rscr", "Youth", "Caregiver"),
    group4 = factor(
      paste0(panel, "_", participant),
      levels = c("TD_Caregiver", "TD_Youth",
                 "CHR_Caregiver", "CHR_Youth")
    ),
    second_60 = second / 60,
    Dyad_id = factor(Dyad_id)
  ) %>%
  filter(!is.na(rating))

df_longALT %>%
  group_by(panel, participant) %>%
  summarise(
    slope = coef(lm(rating ~ second_60))[2],
    .groups = "drop"
  )

  
################################################################################
##### List of Tables and Figures

##################
##### Manuscript

### Table 1. Sample Characteristics
#                 including:
#                   - Rating dial mean (SD)
#                   - Linear slope mean (SD)
#                   - Youth age, caregiver age
#                   - Sex (% women)
#                   - Race (% White / Non-White)
#                   - Symptoms (CHR only)


### Table 2. Pooled Linear Mixed Model: 2 × 2 Design:
#                   - Column (1): no covariates
#                   - Column (2): with youth & caregiver covariates
# (Directly satisfies Reviewer Comment #4 and 
#   allows formal statistical tests of slope differences across all 4 groups)


### Figure 1. TD Dyads Trajectories:
#                   - (A) Linear trajectories
#                   - (B) Cross-validated optimal span


### Figure 2. CHR Dyads Trajectories:
#                   - (A) Linear trajectories
#                   - (B) Cross-validated optimal span



##################
##### Supplement

### Table A1. Pooled Linear Mixed Model: 2 × 2 Design (sample with uneven caregivers and youths)
#                   - Column (1): no covariates
#                   - Column (2): with youth & caregiver covariates

### Table A2. Pooled Linear Mixed Model: 2 × 2 Design (excluding medicated CHR participants)
#                   - Column (1): no covariates
#                   - Column (2): with youth & caregiver covariates

### Table A3. TD vs CHR Separate Models
#                   - Column (1): no covariates
#                   - Column (2): with youth & caregiver covariates
#                   - Column (3): pooled model slope-equivalent estimates
# Responds to Reviewer Comments #15 and #16
# Separate models and pooled model yield equivalent inferences
# “Linear combinations” now clearly explained in footnote (lincom-style)

### Table A4. Mean Differences from Pooled LMM (not split samples)
#               Group differences relative to TD caregivers
#                   - Column (1): no covariates
#                   - Column (2): with youth & caregiver covariates

### Figure A1. Estimated Marginal Means (based on pooled LMM testing mean differences)
#               Means + 95% CIs for all 4 groups

### Table A5. Pairwise Comparisons from EMMs (after pooled LMM testing mean differences)
#               All 6 pairwise contrasts among:
#                   - TD youth
#                   - TD caregiver
#                   - CHR youth
#                   - CHR caregiver
#                     . Column (1): no covariates
#                     . Column (2): with youth & caregiver covariates

### Figure A2. Estimated Marginal Means (based on pooled LMM testing slope differences)
#               Means + 95% CIs for all 4 groups

### Table A6. Pairwise Comparisons from EMMs (after pooled LMM testing slope differences)
#               All 6 pairwise contrasts among:
#                   - TD youth
#                   - TD caregiver
#                   - CHR youth
#                   - CHR caregiver
#                     . Column (1): no covariates
#                     . Column (2): with youth & caregiver covariates

### Figure A3. Additional LOESS Spans – TD Dyads
#                   - Span = 0.25
#                   - Span = 0.50
#                   - Span = 0.75

### Figure A4. Additional LOESS Spans – CHR Dyads
#                   - Span = 0.25
#                   - Span = 0.50
#                   - Span = 0.75

### Table A7. GAMM Nonlinearity Tests
#                   - Parametric coefficients (group mean differences)
#                   - Smooth term diagnostics:
#                     . edf
#                     . F
#                     . p-values
#                   - Model fit stats (N, adj. R²)

### Table A8. GAMM Sensitivity Analyses (Spline basis × basis dimension)
#               Robustness of GAMM conclusions to alternative spline bases and k values
#                   - Columns vary the spline basis:
#                       . TP = thin-plate regression splines
#                       . CR = cubic regression splines
#                       . PS = P-splines
#                   - Columns also vary the basis dimension (k = 10, 20, 40)
#               Reported components in each specification:
#                   - Panel A: Parametric coefficients (group mean differences; TD caregivers as reference)
#                   - Panel B: Smooth-term diagnostics for each group-specific smooth of time:
#                       . edf (effective degrees of freedom)
#                       . F statistic
#                       . p-value
#                   - Panel C: Model summary (Adj. R², scale estimate, N observations, and AIC)


################################################################################
################################################################################
################################################################################

########################################################################
### Table 1. Sample Characteristics
#                 including:
#                   - Rating dial mean (SD)
#                   - Linear slope mean (SD)
#                   - Youth age, caregiver age
#                   - Sex (% women)
#                   - Race (% White / Non-White)
#                   - Symptoms (CHR only)

# -----------------------------
# Helpers
# -----------------------------
m_sd <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean(x), sd(x))
}

pct1 <- function(x, digits = 0) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  p <- mean(as.numeric(x)) * 100
  paste0(round(p, digits), "%")
}

first_existing <- function(df, candidates) {
  nm <- candidates[candidates %in% names(df)]
  if (length(nm) == 0) return(NA_character_)
  nm[1]
}

# Robust TD/CHR label from CHR that could be 0/1 or "TD"/"CHR"
to_panel <- function(chr) {
  x <- as.character(chr)
  x <- trimws(toupper(x))
  ifelse(x %in% c("1", "CHR", "TRUE"), "CHR", "TD")
}

get_cell <- function(df, panel_label, participant_label, colname) {
  out <- df %>%
    filter(panel == panel_label, participant == participant_label) %>%
    pull(.data[[colname]])
  if (length(out) == 0) return(NA_character_)
  out[1]
}

# -----------------------------
# Dyad-level file + panel
# -----------------------------
stopifnot(all(c("Dyad_id","CHR","second") %in% names(df_complete)))

df_dyad <- df_complete %>%
  distinct(Dyad_id, .keep_all = TRUE) %>%
  mutate(panel = to_panel(CHR))

# Check dyad counts
print(table(df_dyad$panel, useNA = "ifany"))

# -----------------------------
# Rating dial mean (SD): compute within-dyad means over seconds, 
#  then summarize across dyads
# -----------------------------
stopifnot(all(c("CrT_Rscr","RrT_Rscr") %in% names(df_complete)))

df_person_means <- df_complete %>%
  mutate(panel = to_panel(CHR)) %>%
  group_by(Dyad_id, panel) %>%
  summarise(
    mean_rating_y = mean(CrT_Rscr, na.rm = TRUE),
    mean_rating_c = mean(RrT_Rscr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(mean_rating_y, mean_rating_c),
    names_to = "who",
    values_to = "mean_rating"
  ) %>%
  mutate(participant = ifelse(who == "mean_rating_y", "Youth", "Caregiver"))

rating_stats <- df_person_means %>%
  group_by(panel, participant) %>%
  summarise(`Rating dial scores` = m_sd(mean_rating, digits = 2), .groups = "drop")

# -----------------------------
# Slopes mean (SD): dyad-level self-report slopes
# -----------------------------
stopifnot(all(c("s10m_Rself_y","s10m_Rself_c") %in% names(df_dyad)))

df_slopes <- df_dyad %>%
  select(Dyad_id, panel, s10m_Rself_y, s10m_Rself_c) %>%
  pivot_longer(
    cols = c(s10m_Rself_y, s10m_Rself_c),
    names_to = "who",
    values_to = "slope"
  ) %>%
  mutate(participant = ifelse(who == "s10m_Rself_y", "Youth", "Caregiver")) %>%
  filter(!is.na(slope))

slope_stats <- df_slopes %>%
  group_by(panel, participant) %>%
  summarise(`Slope of rating dial scores` = m_sd(slope, digits = 2), .groups = "drop")

# -----------------------------
# Ages
# -----------------------------
y_age_var <- first_existing(df_dyad, c("X3_Age","y_age","Y_Age"))
c_age_var <- first_existing(df_dyad, c("c_demo_age_1","c_demo_age","caregiver_age","C_Age"))

age_long <- df_dyad %>%
  transmute(
    Dyad_id, panel,
    Youth = if (!is.na(y_age_var)) .data[[y_age_var]] else NA_real_,
    Caregiver = if (!is.na(c_age_var)) .data[[c_age_var]] else NA_real_
  ) %>%
  pivot_longer(cols = c(Youth, Caregiver), names_to = "participant", values_to = "age")

age_stats <- age_long %>%
  group_by(panel, participant) %>%
  summarise(`Age in years` = m_sd(age, digits = 2), .groups = "drop")

# -----------------------------
# Sex (% women)
# -----------------------------
y_female_var <- first_existing(df_dyad, c("X1_Sex_1"))
c_female_var <- first_existing(df_dyad, c("c_gender_1"))

sex_long <- df_dyad %>%
  transmute(
    Dyad_id, panel,
    Youth = if (!is.na(y_female_var)) .data[[y_female_var]] else NA_real_,
    Caregiver = if (!is.na(c_female_var)) .data[[c_female_var]] else NA_real_
  ) %>%
  pivot_longer(cols = c(Youth, Caregiver), names_to = "participant", values_to = "female")

sex_stats <- sex_long %>%
  group_by(panel, participant) %>%
  summarise(`Biological Sex (women), %` = pct1(female, digits = 0), .groups = "drop")

# -----------------------------
# Race (% White / Non-White)
# -----------------------------
y_race_code <- first_existing(df_dyad, c("X7_Race.s","X7_Race"))
c_race_bin  <- first_existing(df_dyad, c("c_race_1"))
c_race_text <- first_existing(df_dyad, c("c_race"))

race_long <- df_dyad %>%
  transmute(
    Dyad_id, panel,
    Youth_white = if (!is.na(y_race_code)) as.numeric(.data[[y_race_code]] == 1) else NA_real_,
    Caregiver_white = case_when(
      !is.na(c_race_bin)  ~ as.numeric(.data[[c_race_bin]] == 1),
      !is.na(c_race_text) ~ as.numeric(.data[[c_race_text]] == "Caucasian - White"),
      TRUE ~ NA_real_
    )
  ) %>%
  pivot_longer(
    cols = c(Youth_white, Caregiver_white),
    names_to = "participant",
    values_to = "white"
  ) %>%
  mutate(
    participant = ifelse(participant == "Youth_white", "Youth", "Caregiver"),
    nonwhite = ifelse(is.na(white), NA_real_, 1 - white)
  )

race_white <- race_long %>%
  group_by(panel, participant) %>%
  summarise(White = pct1(white, digits = 0), .groups = "drop")

race_nonwhite <- race_long %>%
  group_by(panel, participant) %>%
  summarise(`Non-White` = pct1(nonwhite, digits = 0), .groups = "drop")

# -----------------------------
# Symptoms (CHR youth) + caregiver dep/anx (both panels)
# -----------------------------

# Youth depression/anxiety by panel (TD and CHR)
sym_youth_by_panel <- df_dyad %>%
  group_by(panel) %>%
  summarise(
    Depression_y = if ("Y_BDI_total" %in% names(.)) m_sd(Y_BDI_total, 2) else NA_character_,
    Anxiety_y    = if ("Y_BAI_total" %in% names(.)) m_sd(Y_BAI_total, 2) else NA_character_,
    .groups = "drop"
  )

# Youth Positive/Negative/GFS by panel (TD and CHR)
sym_png_by_panel <- df_dyad %>%
  group_by(panel) %>%
  summarise(
    Pos = if ("Y_SIPS_PosTotal" %in% names(.)) m_sd(Y_SIPS_PosTotal, 2) else NA_character_,
    Neg = if ("Y_SIPS_NegTotal" %in% names(.)) m_sd(Y_SIPS_NegTotal, 2) else NA_character_,
    GFS = if ("GFS_S_Current"  %in% names(.)) m_sd(GFS_S_Current,  2) else NA_character_,
    .groups = "drop"
  )


# Caregiver depression/anxiety by panel (TD and CHR)

stopifnot(all(c("bdi_total", "tot_BAI") %in% names(df_dyad)))

caregiver_dep_anx <- df_dyad %>%
  group_by(panel) %>%
  summarise(
    Depression_c = m_sd(bdi_total, 2),
    Anxiety_c    = m_sd(tot_BAI,  2),
    .groups = "drop"
  )

# -----------------------------
# Assemble Table 1
# -----------------------------
make_panel_block <- function(panel_label) {
  tibble(
    Row = c("Rating dial scores",
            "Slope of rating dial scores",
            "Age in years",
            "Biological Sex (women), %",
            "Racial Identity, %",
            "  White",
            "  Non-White",
            "Symptoms",
            "  Positive Symptoms",
            "  Negative Symptoms",
            "  Social Functioning",
            "  Depression",
            "  Anxiety"),
    Youth = c(
      get_cell(rating_stats, panel_label, "Youth", "Rating dial scores"),
      get_cell(slope_stats,  panel_label, "Youth", "Slope of rating dial scores"),
      get_cell(age_stats,    panel_label, "Youth", "Age in years"),
      get_cell(sex_stats,    panel_label, "Youth", "Biological Sex (women), %"),
      "",
      get_cell(race_white,   panel_label, "Youth", "White"),
      get_cell(race_nonwhite,panel_label, "Youth", "Non-White"),
      "",
      sym_png_by_panel %>% filter(panel == panel_label) %>% pull(Pos) %>% {ifelse(length(.)==0 || is.na(.[1]), "-", .[1])},
      sym_png_by_panel %>% filter(panel == panel_label) %>% pull(Neg) %>% {ifelse(length(.)==0 || is.na(.[1]), "-", .[1])},
      sym_png_by_panel %>% filter(panel == panel_label) %>% pull(GFS) %>% {ifelse(length(.)==0 || is.na(.[1]), "-", .[1])},
      
      sym_youth_by_panel %>% filter(panel == panel_label) %>% pull(Depression_y) %>% {ifelse(length(.)==0, NA_character_, .[1])},
      sym_youth_by_panel %>% filter(panel == panel_label) %>% pull(Anxiety_y)    %>% {ifelse(length(.)==0, NA_character_, .[1])}
      
    ),
    Caregiver = c(
      get_cell(rating_stats, panel_label, "Caregiver", "Rating dial scores"),
      get_cell(slope_stats,  panel_label, "Caregiver", "Slope of rating dial scores"),
      get_cell(age_stats,    panel_label, "Caregiver", "Age in years"),
      get_cell(sex_stats,    panel_label, "Caregiver", "Biological Sex (women), %"),
      "",
      get_cell(race_white,   panel_label, "Caregiver", "White"),
      get_cell(race_nonwhite,panel_label, "Caregiver", "Non-White"),
      "",
      "-", "-", "-",
      caregiver_dep_anx %>% filter(panel == panel_label) %>% pull(Depression_c) %>% {ifelse(length(.)==0, NA_character_, .[1])},
      caregiver_dep_anx %>% filter(panel == panel_label) %>% pull(Anxiety_c)    %>% {ifelse(length(.)==0, NA_character_, .[1])}
    )
  )
}

Table1 <- bind_rows(
  tibble(Row = "Panel A: Typically developing dyads, M(SD)", Youth = "", Caregiver = ""),
  make_panel_block("TD"),
  tibble(Row = "Panel B: CHR dyads, M(SD)", Youth = "", Caregiver = ""),
  make_panel_block("CHR")
)

Table1

# -----------------------------
# Export to Excel
# -----------------------------
wb <- createWorkbook()
addWorksheet(wb, "Table1")

writeData(wb, "Table1", Table1, startRow = 1, startCol = 1, colNames = TRUE)

# Basic formatting
headerStyle <- createStyle(textDecoration = "bold")
addStyle(wb, "Table1", headerStyle, rows = 1, cols = 1:3, gridExpand = TRUE)

# Make panel title rows bold
panel_rows <- which(grepl("^Panel ", Table1$Row)) + 1  # +1 because header row is row 1 in Excel
addStyle(wb, "Table1", headerStyle, rows = panel_rows, cols = 1:3, gridExpand = TRUE)

setColWidths(wb, "Table1", cols = 1, widths = 55)
setColWidths(wb, "Table1", cols = 2:3, widths = 22)

if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
saveWorkbook(wb, file.path(results_dir, "Table1.xlsx"), overwrite = TRUE)



########################################################################
### Table 2. Pooled Linear Mixed Model: 2 × 2 Design:
#                   - Column (1): no covariates
#                   - Column (2): with youth & caregiver covariates

# ------------------------------------------------------------
# df_long adjustment
# ------------------------------------------------------------

df_long <- df_long %>%
  mutate(
    group4 = factor(group4, levels = c("TD_Caregiver","TD_Youth","CHR_Caregiver","CHR_Youth")),
    Dyad_id = factor(Dyad_id)
  )

# ------------------------------------------------------------
# Fit pooled models
# ------------------------------------------------------------
m2_nocov <- lmerTest::lmer(
  rating ~ group4 * second_60 + (1 | Dyad_id),
  data = df_long
)

covars <- c("X3_Age", "X1_Sex_1", "c_demo_age_1", "c_gender_1")
covars <- covars[covars %in% names(df_long)]

rhs_cov <- paste(c("group4 * second_60", covars), collapse = " + ")

m2_cov <- lmerTest::lmer(
  as.formula(paste0("rating ~ ", rhs_cov, " + (1 | Dyad_id)")),
  data = df_long
)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
stars <- function(p){
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*", ""))))
}
fmt_est <- function(est, p) sprintf("%.3f%s", est, stars(p))
fmt_se  <- function(se) sprintf("(%.3f)", se)

extract_8coef <- function(model){
  
  co <- as.data.frame(summary(model)$coefficients) %>%
    tibble::rownames_to_column("term") %>%
    rename(
      Estimate = Estimate,
      SE = `Std. Error`,
      p = `Pr(>|t|)`
    )
  
  wanted <- c(
    "second_60",
    "group4TD_Youth:second_60",
    "group4CHR_Caregiver:second_60",
    "group4CHR_Youth:second_60",
    "(Intercept)",
    "group4TD_Youth",
    "group4CHR_Caregiver",
    "group4CHR_Youth"
  )
  
  labels <- c(
    "second_60 (slope for TD caregiver)",
    "TD youth × time (Δ slope relative to TD caregiver)",
    "CHR caregiver × time (Δ slope relative to TD caregiver)",
    "CHR youth × time (Δ slope relative to TD caregiver)",
    "Intercept (TD caregiver)",
    "TD youth (Δ intercept relative to TD caregiver)",
    "CHR caregiver (Δ intercept relative to TD caregiver)",
    "CHR youth (Δ intercept relative to TD caregiver)"
  )
  
  co <- co %>%
    filter(term %in% wanted) %>%
    mutate(term = factor(term, levels = wanted)) %>%
    arrange(term)
  
  # Build coefficient + SE rows explicitly
  out <- purrr::map2_dfr(
    labels,
    seq_len(nrow(co)),
    ~ tibble(
      Row = c(.x, ""),
      Value = c(
        fmt_est(co$Estimate[.y], co$p[.y]),
        fmt_se(co$SE[.y])
      )
    )
  )
  
  out
}

tab1 <- extract_8coef(m2_nocov)
tab2 <- extract_8coef(m2_cov)

Table2 <- tibble(
  Row = tab1$Row,
  `(1) No covariates` = tab1$Value,
  `(2) With covariates` = tab2$Value
)

# Add N rows (coerce to character so bind_rows works)
Table2 <- bind_rows(
  Table2,
  tibble(
    Row = c("N observations", "N dyads"),
    `(1) No covariates` = as.character(c(nobs(m2_nocov), length(unique(df_long$Dyad_id)))),
    `(2) With covariates` = as.character(c(nobs(m2_cov), length(unique(m2_cov@frame$Dyad_id))))
  )
)

Table2


wb <- createWorkbook()
addWorksheet(wb, "Table2")

writeData(wb, "Table2", Table2)

# optional: widen columns
setColWidths(wb, "Table2", cols = 1:3, widths = c(52, 20, 20))

saveWorkbook(wb, file.path(results_dir, "Table2.xlsx"), overwrite = TRUE)



########################################################################
### Figure 1. TD Dyads Trajectories:
#                   - (A) Linear trajectories 
#                   - (B) Cross-validated optimal span

### Figure 1A: Linear trajectories, TD

# Filter the data to include only observations where CHR == 0
df_td <- df_complete[df_complete$CHR == 0, ]

# Create the plot with custom legends and ordered factors
fig1A <- ggplot(df_td, aes(x = second)) +
  stat_summary(aes(y = CrT_Rscr, color = "Youth", linetype = "Youth"), geom = "line", fun = mean, size = 0.6, show.legend = TRUE) +
  geom_smooth(aes(y = CrT_Rscr, color = "Youth", linetype = "Youth"), method = 'lm', formula = y ~ x, size = 1.25, show.legend = TRUE) +
  stat_summary(aes(y = RrT_Rscr, color = "Caregiver", linetype = "Caregiver"), geom = "line", fun = mean, size = 0.7, show.legend = TRUE) +
  geom_smooth(aes(y = RrT_Rscr, color = "Caregiver", linetype = "Caregiver"), method = 'lm', formula = y ~ x, size = 1.25, show.legend = TRUE) +
  labs(x = "Time (in seconds)",
       y = "Rating dial scores",
       title = "Youth and caregivers self-ratings",
       subtitle = "Conflict conversation",
       color = "Participant",
       linetype = "Participant") +
  scale_color_manual(
    values = c("Youth" = "black", "Caregiver" = "gray"),
    breaks = c("Youth", "Caregiver")
  ) +
  scale_linetype_manual(
    values = c("Youth" = "solid", "Caregiver" = "dashed"),
    breaks = c("Youth", "Caregiver")
  ) +
  theme(plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))

ggsave(
  filename = file.path(results_dir, "Figure1A.png"),
  plot = fig1A,
  width = 7,
  height = 5,
  dpi = 300
)

### Figure 1B: Cross-validated optimal span

## Youth
span_grid <- expand.grid(span = seq(0.05, 0.95, by = 0.01), degree = c(1))

# Set up cross-validation using caret
train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Fit the LOESS model using different span values
loess_model <- train(CrT_Rscr ~ second, data = df_td,
                     method = "gamLoess",
                     tuneGrid = span_grid,
                     trControl = train_control)

# Check the optimal span
loess_model$bestTune

## After re-running the code five times:
#   Span      Degree
#   0.11      1
#   0.07      1
#   0.08      1
#   0.11      1
#   0.12      1
(.11+.07+.08+.11+.12)/5
# .098 
# ~= 0.10

## Caregiver
span_grid <- expand.grid(span = seq(0.05, 0.95, by = 0.01), degree = c(1))

# Set up cross-validation using caret
train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Fit the LOESS model using different span values
loess_model <- train(RrT_Rscr ~ second, data = df_td,
                     method = "gamLoess",
                     tuneGrid = span_grid,
                     trControl = train_control)

# Check the optimal span
loess_model$bestTune

## After re-running the code five times:
#   Span      Degree
#   0.09      1
#   0.07      1
#   0.06      1
#   0.08      1
#   0.09      1
(.09+.07+.06+.08+.09)/5
# .078 
# ~= 0.08

# Optimal span for TD youth = 0.10 and TD caregivers = 0.08
fig1B <- ggplot(df_td, aes(x = second)) +
  stat_summary(aes(y = CrT_Rscr, color = "Youth", linetype = "Youth"), geom = "line", fun = mean, size = 0.6, show.legend = TRUE) +
  geom_smooth(aes(y = CrT_Rscr, color = "Youth", linetype = "Youth"), method = 'loess', span = 0.10, size = 1.25, show.legend = TRUE) +  # LOESS smoothing with adjustable span
  stat_summary(aes(y = RrT_Rscr, color = "Caregiver", linetype = "Caregiver"), geom = "line", fun = mean, size = 0.7, show.legend = TRUE) +
  geom_smooth(aes(y = RrT_Rscr, color = "Caregiver", linetype = "Caregiver"), method = 'loess', span = 0.08, size = 1.25, show.legend = TRUE) +  # LOESS smoothing with adjustable span
  labs(x = "Time (in seconds)",
       y = "Rating dial scores",
       title = "Youth and caregivers self-ratings",
       subtitle = "Conflict conversation",
       color = "Participant",
       linetype = "Participant") +
  scale_color_manual(
    values = c("Youth" = "black", "Caregiver" = "gray"),
    breaks = c("Youth", "Caregiver")
  ) +
  scale_linetype_manual(
    values = c("Youth" = "solid", "Caregiver" = "dashed"),
    breaks = c("Youth", "Caregiver")
  ) +
  theme(plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))

ggsave(
  filename = file.path(results_dir, "Figure1B.png"),
  plot = fig1B,
  width = 7,
  height = 5,
  dpi = 300
)


########################################################################
### Figure 2. CHR Dyads Trajectories:
#                   - (A) Linear trajectories 
#                   - (B) Cross-validated optimal span

### Figure 2A: Linear trajectories, CHR

# Filter the data to include only observations where CHR == 1
df_chr <- df_complete[df_complete$CHR == 1, ]

# Create the plot with custom legends and ordered factors
fig2A <- ggplot(df_chr, aes(x = second)) +
  stat_summary(aes(y = CrT_Rscr, color = "CHR youth", linetype = "CHR youth"), geom = "line", fun = mean, size = 0.6, show.legend = TRUE) +
  geom_smooth(aes(y = CrT_Rscr, color = "CHR youth", linetype = "CHR youth"), method = 'lm', formula = y ~ x, size = 1.25, show.legend = TRUE) +
  stat_summary(aes(y = RrT_Rscr, color = "CHR caregiver", linetype = "CHR caregiver"), geom = "line", fun = mean, size = 0.7, show.legend = TRUE) +
  geom_smooth(aes(y = RrT_Rscr, color = "CHR caregiver", linetype = "CHR caregiver"), method = 'lm', formula = y ~ x, size = 1.25, show.legend = TRUE) +
  labs(x = "Time (in seconds)",
       y = "Rating dial scores",
       title = "CHR youth and caregivers self-ratings",
       subtitle = "Conflict conversation",
       color = "Participant",
       linetype = "Participant") +
  scale_color_manual(
    values = c("CHR youth" = "#1F77B4", "CHR caregiver" = "#AEC7E8"),
    breaks = c("CHR youth", "CHR caregiver")
  ) +
  scale_linetype_manual(
    values = c("CHR youth" = "solid", "CHR caregiver" = "dashed"),
    breaks = c("CHR youth", "CHR caregiver")
  ) +
  theme(plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))


ggsave(
  filename = file.path(results_dir, "Figure2A.png"),
  plot = fig2A,
  width = 7,
  height = 5,
  dpi = 300
)


### Figure 2B: Cross-validated optimal span

library(caret)

## Youth
span_grid <- expand.grid(span = seq(0.05, 0.95, by = 0.01), degree = c(1))

# Set up cross-validation using caret
train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Fit the LOESS model using different span values
loess_model <- train(CrT_Rscr ~ second, data = df_chr,
                     method = "gamLoess",
                     tuneGrid = span_grid,
                     trControl = train_control)

# Check the optimal span
loess_model$bestTune

## After re-running the code five times:
#   Span      Degree
#   0.08      1
#   0.08      1
#   0.09      1
#   0.09      1
#   0.07      1
(.08+.08+.09+.09+.07)/5
# .082 
# ~= 0.08

## Caregiver
span_grid <- expand.grid(span = seq(0.05, 0.95, by = 0.01), degree = c(1))

# Set up cross-validation using caret
train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Fit the LOESS model using different span values
loess_model <- train(RrT_Rscr ~ second, data = df_chr,
                     method = "gamLoess",
                     tuneGrid = span_grid,
                     trControl = train_control)

# Check the optimal span
loess_model$bestTune

## After re-running the code five times:
#   Span      Degree
#   0.08      1
#   0.08      1
#   0.08      1
#   0.08      1
#   0.08      1
(.08+.08+.08+.08+.08)/5
# .08
# ~= 0.08

# Optimal span for CHR youth  = 0.08 and CHR caregivers = 0.08
fig2B <- ggplot(df_chr, aes(x = second)) +
  stat_summary(aes(y = CrT_Rscr, color = "CHR youth", linetype = "CHR youth"), geom = "line", fun = mean, size = 0.6, show.legend = TRUE) +
  geom_smooth(aes(y = CrT_Rscr, color = "CHR youth", linetype = "CHR youth"), method = 'loess', span = 0.08, size = 1.25, show.legend = TRUE) +
  stat_summary(aes(y = RrT_Rscr, color = "CHR caregiver", linetype = "CHR caregiver"), geom = "line", fun = mean, size = 0.7, show.legend = TRUE) +
  geom_smooth(aes(y = RrT_Rscr, color = "CHR caregiver", linetype = "CHR caregiver"), method = 'loess', span = 0.08, size = 1.25, show.legend = TRUE) +
  labs(x = "Time (in seconds)",
       y = "Rating dial scores",
       title = "CHR youth and caregivers self-ratings",
       subtitle = "Conflict conversation",
       color = "Participant",
       linetype = "Participant") +
  scale_color_manual(
    values = c("CHR youth" = "#1F77B4", "CHR caregiver" = "#AEC7E8"),
    breaks = c("CHR youth", "CHR caregiver")
  ) +
  scale_linetype_manual(
    values = c("CHR youth" = "solid", "CHR caregiver" = "dashed"),
    breaks = c("CHR youth", "CHR caregiver")
  ) +
  theme(plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))


ggsave(
  filename = file.path(results_dir, "Figure2B.png"),
  plot = fig2B,
  width = 7,
  height = 5,
  dpi = 300
)



################################################################################
############################## SUPPLEMENT ######################################

########################################################################
### Table A1. Pooled Linear Mixed Model: 2 × 2 Design 
#               (sample with uneven caregivers and youths)
#                   - Column (1): no covariates
#                   - Column (2): with youth & caregiver covariates

# ------------------------------------------------------------
# df_long adjustment
# ------------------------------------------------------------

df_longALT <- df_longALT %>%
  mutate(
    group4 = factor(group4, levels = c("TD_Caregiver","TD_Youth","CHR_Caregiver","CHR_Youth")),
    Dyad_id = factor(Dyad_id)
  )

# ------------------------------------------------------------
# Fit pooled models
# ------------------------------------------------------------
m2_nocov <- lmerTest::lmer(
  rating ~ group4 * second_60 + (1 | Dyad_id),
  data = df_longALT
)

covars <- c("X3_Age", "X1_Sex_1", "c_demo_age_1", "c_gender_1")
covars <- covars[covars %in% names(df_longALT)]

rhs_cov <- paste(c("group4 * second_60", covars), collapse = " + ")

m2_cov <- lmerTest::lmer(
  as.formula(paste0("rating ~ ", rhs_cov, " + (1 | Dyad_id)")),
  data = df_longALT
)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
stars <- function(p){
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*", ""))))
}
fmt_est <- function(est, p) sprintf("%.3f%s", est, stars(p))
fmt_se  <- function(se) sprintf("(%.3f)", se)

extract_8coef <- function(model){
  
  co <- as.data.frame(summary(model)$coefficients) %>%
    tibble::rownames_to_column("term") %>%
    rename(
      Estimate = Estimate,
      SE = `Std. Error`,
      p = `Pr(>|t|)`
    )
  
  wanted <- c(
    "second_60",
    "group4TD_Youth:second_60",
    "group4CHR_Caregiver:second_60",
    "group4CHR_Youth:second_60",
    "(Intercept)",
    "group4TD_Youth",
    "group4CHR_Caregiver",
    "group4CHR_Youth"
  )
  
  labels <- c(
    "second_60 (slope for TD caregiver)",
    "TD youth × time (Δ slope relative to TD caregiver)",
    "CHR caregiver × time (Δ slope relative to TD caregiver)",
    "CHR youth × time (Δ slope relative to TD caregiver)",
    "Intercept (TD caregiver)",
    "TD youth (Δ intercept relative to TD caregiver)",
    "CHR caregiver (Δ intercept relative to TD caregiver)",
    "CHR youth (Δ intercept relative to TD caregiver)"
  )
  
  co <- co %>%
    filter(term %in% wanted) %>%
    mutate(term = factor(term, levels = wanted)) %>%
    arrange(term)
  
  # Build coefficient + SE rows explicitly
  out <- purrr::map2_dfr(
    labels,
    seq_len(nrow(co)),
    ~ tibble(
      Row = c(.x, ""),
      Value = c(
        fmt_est(co$Estimate[.y], co$p[.y]),
        fmt_se(co$SE[.y])
      )
    )
  )
  
  out
}


tab1 <- extract_8coef(m2_nocov)
tab2 <- extract_8coef(m2_cov)

TableA1 <- tibble(
  Row = tab1$Row,
  `(1) No covariates` = tab1$Value,
  `(2) With covariates` = tab2$Value
)

# Add N rows (coerce to character so bind_rows works)
TableA1 <- bind_rows(
  TableA1,
  tibble(
    Row = c("N observations", "N dyads"),
    `(1) No covariates` = as.character(c(nobs(m2_nocov), length(unique(df_longALT$Dyad_id)))),
    `(2) With covariates` = as.character(c(nobs(m2_cov), length(unique(m2_cov@frame$Dyad_id))))
  )
)

TableA1

wb <- createWorkbook()
addWorksheet(wb, "TableA1")

writeData(wb, "TableA1", TableA1)

# optional: widen columns
setColWidths(wb, "TableA1", cols = 1:3, widths = c(52, 20, 20))

saveWorkbook(wb, file.path(results_dir, "TableA1.xlsx"), overwrite = TRUE)



########################################################################
### Table A2. Pooled Linear Mixed Model: 2 × 2 Design 
#               (excluding medicated CHR participants)
#                   - Column (1): no covariates
#                   - Column (2): with youth & caregiver covariates

# ------------------------------------------------------------
# df_long adjustment
# ------------------------------------------------------------

#ID1 and ID2 are CHR participants excluded for this exercise, you should enter
# corresponding numeric IDs

if (!exists("ID1") || !exists("ID2")) {
  stop("Table A2 requires excluded dyad IDs (ID1, ID2). Request these from the authors.")
}

df_longM <- df_long[!df_long$Dyad_id %in% c(ID1, ID2), ]

# ------------------------------------------------------------
# 1) Fit pooled models
# ------------------------------------------------------------
m2_nocov <- lmerTest::lmer(
  rating ~ group4 * second_60 + (1 | Dyad_id),
  data = df_longM
)

covars <- c("X3_Age", "X1_Sex_1", "c_demo_age_1", "c_gender_1")
covars <- covars[covars %in% names(df_longM)]

rhs_cov <- paste(c("group4 * second_60", covars), collapse = " + ")

m2_cov <- lmerTest::lmer(
  as.formula(paste0("rating ~ ", rhs_cov, " + (1 | Dyad_id)")),
  data = df_longM
)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
stars <- function(p){
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*", ""))))
}
fmt_est <- function(est, p) sprintf("%.3f%s", est, stars(p))
fmt_se  <- function(se) sprintf("(%.3f)", se)

extract_8coef <- function(model){
  
  co <- as.data.frame(summary(model)$coefficients) %>%
    tibble::rownames_to_column("term") %>%
    rename(
      Estimate = Estimate,
      SE = `Std. Error`,
      p = `Pr(>|t|)`
    )
  
  wanted <- c(
    "second_60",
    "group4TD_Youth:second_60",
    "group4CHR_Caregiver:second_60",
    "group4CHR_Youth:second_60",
    "(Intercept)",
    "group4TD_Youth",
    "group4CHR_Caregiver",
    "group4CHR_Youth"
  )
  
  labels <- c(
    "second_60 (slope for TD caregiver)",
    "TD youth × time (Δ slope relative to TD caregiver)",
    "CHR caregiver × time (Δ slope relative to TD caregiver)",
    "CHR youth × time (Δ slope relative to TD caregiver)",
    "Intercept (TD caregiver)",
    "TD youth (Δ intercept relative to TD caregiver)",
    "CHR caregiver (Δ intercept relative to TD caregiver)",
    "CHR youth (Δ intercept relative to TD caregiver)"
  )
  
  co <- co %>%
    filter(term %in% wanted) %>%
    mutate(term = factor(term, levels = wanted)) %>%
    arrange(term)
  
  # Build coefficient + SE rows explicitly
  out <- purrr::map2_dfr(
    labels,
    seq_len(nrow(co)),
    ~ tibble(
      Row = c(.x, ""),
      Value = c(
        fmt_est(co$Estimate[.y], co$p[.y]),
        fmt_se(co$SE[.y])
      )
    )
  )
  
  out
}

tab1 <- extract_8coef(m2_nocov)
tab2 <- extract_8coef(m2_cov)

TableA2 <- tibble(
  Row = tab1$Row,
  `(1) No covariates` = tab1$Value,
  `(2) With covariates` = tab2$Value
)

# Add N rows (coerce to character so bind_rows works)
TableA2 <- bind_rows(
  TableA2,
  tibble(
    Row = c("N observations", "N dyads"),
    `(1) No covariates` = as.character(c(nobs(m2_nocov), length(unique(df_longM$Dyad_id)))),
    `(2) With covariates` = as.character(c(nobs(m2_cov), length(unique(m2_cov@frame$Dyad_id))))
  )
)

TableA2


wb <- createWorkbook()
addWorksheet(wb, "TableA2")

writeData(wb, "TableA2", TableA2)

# optional: widen columns
setColWidths(wb, "TableA2", cols = 1:3, widths = c(52, 20, 20))

saveWorkbook(wb, file.path(results_dir, "TableA2.xlsx"), overwrite = TRUE)



########################################################################
### Table A3. TD vs CHR Separate Models
#                   - Column (1): no covariates
#                   - Column (2): with youth & caregiver covariates

# ---------- helpers ----------
stars <- function(p){
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*", ""))))
}
fmt_est <- function(est, p) sprintf("%.3f%s", est, stars(p))
fmt_se  <- function(se) sprintf("(%.3f)", se)

# compute p-value from estimate, SE, df (df may be Inf)
p_from_est_se_df <- function(est, se, df){
  tval <- est / se
  ifelse(is.finite(df),
         2 * pt(-abs(tval), df = df),
         2 * pnorm(-abs(tval)))
}

# ---------- helper: find estimate column in emmeans outputs ----------
get_est_col <- function(d){
  if ("estimate" %in% names(d)) return("estimate")
  tr <- grep("trend", names(d), value = TRUE)
  if (length(tr) == 1) return(tr)
  # fallback: sometimes it's called something like "second_60.trend"
  tr2 <- grep("\\.trend$", names(d), value = TRUE)
  if (length(tr2) == 1) return(tr2)
  stop("Could not identify estimate/trend column in emmeans output. Names: ",
       paste(names(d), collapse = ", "))
}

# ---------- core extractor for ONE panel model ----------
extract_panel_rows <- function(model, panel_label = c("TD","CHR"), participant_var){
  
  panel_label <- match.arg(panel_label)
  
  # emtrends for slopes by participant
  sl <- emtrends(model, as.formula(paste0("~ ", participant_var)), var = "second_60")
  sl_df <- as.data.frame(sl)
  
  trend_col <- get_est_col(sl_df)
  
  # standardize participant column name
  sl_df <- sl_df %>%
    rename(participant_level = !!participant_var) %>%
    mutate(
      estimate = .data[[trend_col]],
      p = p_from_est_se_df(estimate, SE, df)
    )
  
  youth <- sl_df %>% filter(participant_level == "Youth")
  careg <- sl_df %>% filter(participant_level == "Caregiver")
  
  stopifnot(nrow(youth) == 1, nrow(careg) == 1)
  
  # contrast caregiver - youth
  diff <- contrast(sl, method = list("Caregiver - Youth (slope)" = c(1, -1)))
  diff_df <- as.data.frame(diff)
  
  diff_est_col <- get_est_col(diff_df)
  
  diff_df <- diff_df %>%
    mutate(
      estimate = .data[[diff_est_col]],
      p = p_from_est_se_df(estimate, SE, df)
    )
  
  tibble(
    Row = c(
      paste0("(", ifelse(panel_label=="TD","M1","M2"), ") ",
             ifelse(panel_label=="TD",
                    "Typically developing youth and their caregivers",
                    "CHR youth and their caregivers")),
      "Time (slope for youth)",
      "",
      "Time × caregivers",
      "",
      "Linear combination (slope for caregivers)",
      ""
    ),
    Value = c(
      "",
      fmt_est(youth$estimate[1], youth$p[1]),
      fmt_se(youth$SE[1]),
      fmt_est(diff_df$estimate[1], diff_df$p[1]),
      fmt_se(diff_df$SE[1]),
      fmt_est(careg$estimate[1], careg$p[1]),
      fmt_se(careg$SE[1])
    )
  )
}

# ---------- pooled equivalents extractor (one column) ----------
extract_pooled_equivalents <- function(pooled_model){
  
  sl <- emtrends(pooled_model, ~ group4, var = "second_60")
  sl_df <- as.data.frame(sl)
  
  trend_col <- get_est_col(sl_df)
  
  sl_df <- sl_df %>%
    mutate(
      estimate = .data[[trend_col]],
      p = p_from_est_se_df(estimate, SE, df)
    )
  
  td_youth <- sl_df %>% filter(group4 == "TD_Youth")
  td_care  <- sl_df %>% filter(group4 == "TD_Caregiver")
  chr_youth<- sl_df %>% filter(group4 == "CHR_Youth")
  chr_care <- sl_df %>% filter(group4 == "CHR_Caregiver")
  
  stopifnot(nrow(td_youth)==1, nrow(td_care)==1, nrow(chr_youth)==1, nrow(chr_care)==1)
  
  # contrasts (NOTE: must match the order of factor levels in group4)
  message("Current group4 level order: ", paste(levels(df_long$group4), collapse = ", "))
  td_diff  <- contrast(sl, method = list("TD caregiver - TD youth (slope)" = c( 1, -1, 0, 0)))
  chr_diff <- contrast(sl, method = list("CHR caregiver - CHR youth (slope)" = c( 0, 0, 1, -1)))
  
  td_df <- as.data.frame(td_diff)
  chr_df<- as.data.frame(chr_diff)
  
  td_est_col  <- get_est_col(td_df)
  chr_est_col <- get_est_col(chr_df)
  
  td_df <- td_df %>%
    mutate(estimate = .data[[td_est_col]], p = p_from_est_se_df(estimate, SE, df))
  chr_df <- chr_df %>%
    mutate(estimate = .data[[chr_est_col]], p = p_from_est_se_df(estimate, SE, df))
  
  bind_rows(
    tibble(
      Row = c("(M1) Typically developing youth and their caregivers",
              "Time (slope for youth)","",
              "Time × caregivers","",
              "Linear combination (slope for caregivers)",""),
      Value = c("",
                fmt_est(td_youth$estimate[1], td_youth$p[1]),
                fmt_se(td_youth$SE[1]),
                fmt_est(td_df$estimate[1], td_df$p[1]),
                fmt_se(td_df$SE[1]),
                fmt_est(td_care$estimate[1], td_care$p[1]),
                fmt_se(td_care$SE[1]))
    ),
    tibble(
      Row = c("(M2) CHR youth and their caregivers",
              "Time (slope for youth)","",
              "Time × caregivers","",
              "Linear combination (slope for caregivers)",""),
      Value = c("",
                fmt_est(chr_youth$estimate[1], chr_youth$p[1]),
                fmt_se(chr_youth$SE[1]),
                fmt_est(chr_df$estimate[1], chr_df$p[1]),
                fmt_se(chr_df$SE[1]),
                fmt_est(chr_care$estimate[1], chr_care$p[1]),
                fmt_se(chr_care$SE[1]))
    )
  )
}

# prep data
stopifnot(all(c("rating","second_60","Dyad_id","group4","panel","participant") %in% names(df_long)))

df_long <- df_long %>%
  mutate(
    Dyad_id = factor(Dyad_id),
    group4 = factor(group4, levels = c("TD_Caregiver","TD_Youth","CHR_Caregiver","CHR_Youth")),
    panel  = factor(panel, levels = c("TD","CHR"))
  )

df_TD  <- df_long %>% filter(panel == "TD")  %>% mutate(participant_TD  = factor(participant, levels=c("Caregiver","Youth")))
df_CHR <- df_long %>% filter(panel == "CHR") %>% mutate(participant_CHR = factor(participant, levels=c("Caregiver","Youth")))

covars <- c("X3_Age", "X1_Sex_1", "c_demo_age_1", "c_gender_1")
covars <- covars[covars %in% names(df_long)]


# fit separate models
m_TD_nocov  <- lmerTest::lmer(rating ~ participant_TD  * second_60 + (1|Dyad_id), data=df_TD)
m_CHR_nocov <- lmerTest::lmer(rating ~ participant_CHR * second_60 + (1|Dyad_id), data=df_CHR)

rhs_TD_cov  <- paste(c("participant_TD  * second_60", covars), collapse=" + ")
rhs_CHR_cov <- paste(c("participant_CHR * second_60", covars), collapse=" + ")

m_TD_cov  <- lmerTest::lmer(as.formula(paste0("rating ~ ", rhs_TD_cov,  " + (1|Dyad_id)")), data=df_TD)
m_CHR_cov <- lmerTest::lmer(as.formula(paste0("rating ~ ", rhs_CHR_cov, " + (1|Dyad_id)")), data=df_CHR)

# build columns
col1 <- bind_rows(
  extract_panel_rows(m_TD_nocov,  panel_label="TD",  participant_var="participant_TD"),
  extract_panel_rows(m_CHR_nocov, panel_label="CHR", participant_var="participant_CHR")
)

col2 <- bind_rows(
  extract_panel_rows(m_TD_cov,  panel_label="TD",  participant_var="participant_TD"),
  extract_panel_rows(m_CHR_cov, panel_label="CHR", participant_var="participant_CHR")
)

# assemble Table A3 (2 columns only)
TableA3 <- tibble(
  Row = col1$Row,
  `(1) Separate models: no covariates` = col1$Value,
  `(2) Separate models: with covariates` = col2$Value
)

# Add N lines (observations + dyads)
TableA3 <- bind_rows(
  TableA3,
  tibble(
    Row = c("N observations (TD model)", "N dyads (TD model)",
            "N observations (CHR model)", "N dyads (CHR model)"),
    `(1) Separate models: no covariates` = c(
      as.character(nobs(m_TD_nocov)),
      as.character(length(unique(df_TD$Dyad_id))),
      as.character(nobs(m_CHR_nocov)),
      as.character(length(unique(df_CHR$Dyad_id)))
    ),
    `(2) Separate models: with covariates` = c(
      as.character(nobs(m_TD_cov)),
      as.character(length(unique(m_TD_cov@frame$Dyad_id))),
      as.character(nobs(m_CHR_cov)),
      as.character(length(unique(m_CHR_cov@frame$Dyad_id)))
    )
  )
)

# 4) export to Excel
wb <- createWorkbook()
addWorksheet(wb, "TableA3")
writeData(wb, "TableA3", TableA3)

saveWorkbook(wb, file.path(results_dir, "TableA3.xlsx"), overwrite = TRUE)


########################################################################
### Table A4. Mean Differences from Pooled LMM
#               Group differences relative to TD caregivers
#                   - Column (1): no covariates
#                   - Column (2): with youth & caregiver covariates


# -----------------------------
# Preconditions
# -----------------------------
stopifnot(all(c("Dyad_id","group4","rating") %in% names(df_long)))

df_long <- df_long %>%
  mutate(
    Dyad_id = factor(Dyad_id),
    group4 = factor(group4, levels = c("TD_Caregiver","TD_Youth","CHR_Youth","CHR_Caregiver"))
  )

# Covariates
covars <- c("X3_Age", "X1_Sex_1", "c_demo_age_1", "c_gender_1")
covars <- covars[covars %in% names(df_long)]

# -----------------------------
# Helpers (stars + formatting)
# -----------------------------
stars <- function(p){
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*", ""))))
}
fmt_est <- function(est, p) sprintf("%.4f%s", est, stars(p))
fmt_se  <- function(se) sprintf("(%.4f)", se)

# -----------------------------
# Fit mean-only pooled models
# -----------------------------
m_mean_nocov <- lmerTest::lmer(
  rating ~ group4 + (1 | Dyad_id),
  data = df_long
)

rhs_cov <- paste(c("group4", covars), collapse = " + ")

m_mean_cov <- lmerTest::lmer(
  as.formula(paste0("rating ~ ", rhs_cov, " + (1 | Dyad_id)")),
  data = df_long
)

# TABLE A4: coefficients from linear mixed models
#     Reference: TD caregivers

extract_A4 <- function(mod){
  co <- as.data.frame(summary(mod)$coefficients) %>%
    tibble::rownames_to_column("term") %>%
    rename(Estimate = Estimate, SE = `Std. Error`, p = `Pr(>|t|)`)
  
  wanted <- c("group4TD_Youth", "group4CHR_Youth", "group4CHR_Caregiver", "(Intercept)")
  
  labels <- c(
    "TD youth",
    "CHR youth",
    "CHR caregiver",
    "Intercept"
  )
  
  out <- co %>%
    filter(term %in% wanted) %>%
    mutate(term = factor(term, levels = wanted)) %>%
    arrange(term)
  
  # Build rows with estimate + SE lines
  tibble(
    Row = rep(labels, each = 2),
    line = rep(c("est","se"), times = length(labels)),
    Value = c(rbind(
      fmt_est(out$Estimate, out$p),
      fmt_se(out$SE)
    ))
  ) %>%
    mutate(Row = ifelse(line == "se", "", Row)) %>%
    select(Row, Value)
}

A4_col1 <- extract_A4(m_mean_nocov) %>% rename(`(1)` = Value)
A4_col2 <- extract_A4(m_mean_cov)   %>% rename(`(2)` = Value)

TableA4_means <- bind_cols(
  tibble(Row = A4_col1$Row),
  A4_col1 %>% select(`(1)`),
  A4_col2 %>% select(`(2)`)
)

# Add N info (match your supplement style)
TableA4_means <- bind_rows(
  TableA4_means,
  tibble(
    Row = c("N observations", "N dyads"),
    `(1)` = as.character(c(nobs(m_mean_nocov), length(unique(df_long$Dyad_id)))),
    `(2)` = as.character(c(nobs(m_mean_cov),   length(unique(m_mean_cov@frame$Dyad_id))))
  )
)

# Export Table A4
wb <- createWorkbook()
addWorksheet(wb, "TableA4")
writeData(wb, "TableA4", TableA4_means)
setColWidths(wb, "TableA4", cols = 1:3, widths = c(45, 18, 18))

saveWorkbook(wb, file.path(results_dir, "TableA4.xlsx"), overwrite = TRUE)



########################################################################
### Figure A1. Estimated Marginal Means
#               (based on pooled LMM testing mean differences)
#               Means + 95% CIs for all 4 groups

emm_nocov <- emmeans(m_mean_nocov, ~ group4)
emm_cov   <- emmeans(m_mean_cov,   ~ group4)

emm_to_plot <- function(emm_obj, model_label){
  as.data.frame(emm_obj) %>%
    transmute(
      model = model_label,
      group4 = group4,
      emmean = emmean,
      lower = asymp.LCL,
      upper = asymp.UCL
    )
}

plot_df <- bind_rows(
  emm_to_plot(emm_nocov, "No covariates"),
  emm_to_plot(emm_cov,   "With covariates")
) %>%
  mutate(
    group4 = factor(group4,
                    levels = c("TD_Caregiver","TD_Youth","CHR_Youth","CHR_Caregiver"),
                    labels = c("TD caregiver","TD youth","CHR youth","CHR caregiver")
    )
  )

# Figure A1: two points per group (dodge by model)
figA1 <- 
  ggplot(plot_df, aes(x = group4, y = emmean, shape = model)) +
  geom_point(position = position_dodge(width = 0.4), size = 2.2) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.4),
                width = 0.12) +
  labs(
    x = NULL,
    y = "Estimated marginal mean (rating dial)",
    title = "Figure A1. Mean differences across participant groups from estimated marginal means",
    shape = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

figA1 <- figA1 +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

ggsave(filename = file.path(results_dir, "FigureA1.png"), 
       figA1, width = 8, height = 4.6, dpi = 300, bg = "white")



########################################################################
### Table A5. Pairwise Comparisons from EMMs 
#               (after pooled LMM testing mean differences)
#               All 6 pairwise contrasts among:
#                   - TD youth
#                   - TD caregiver
#                   - CHR youth
#                   - CHR caregiver
#                     . Column (1): no covariates
#                     . Column (2): with youth & caregiver covariates

# Build the 6 contrasts as weight vectors over the 4 means:
# Order: [TD_Caregiver, TD_Youth, CHR_Youth, CHR_Caregiver]
contrast_list <- list(
  "TD caregiver - TD youth"         = c( 1, -1,  0,  0),
  "TD caregiver - CHR youth"        = c( 1,  0, -1,  0),
  "TD caregiver - CHR caregiver"    = c( 1,  0,  0, -1),
  "TD youth - CHR youth"            = c( 0,  1, -1,  0),
  "TD youth - CHR caregiver"        = c( 0,  1,  0, -1),
  "CHR youth - CHR caregiver"       = c( 0,  0,  1, -1)
)


# Compute EMMs
emm_nocov <- emmeans(m_mean_nocov, ~ group4)
emm_cov   <- emmeans(m_mean_cov,   ~ group4)

# Apply contrasts
A5_nocov_raw <- as.data.frame(contrast(emm_nocov, method = contrast_list, adjust = "none"))
A5_cov_raw   <- as.data.frame(contrast(emm_cov,   method = contrast_list, adjust = "none"))

# Force labels
A5_nocov_raw <- A5_nocov_raw %>%
  mutate(contrast = as.character(contrast),
         ord = match(contrast, names(contrast_list))) %>%
  arrange(ord) %>% select(-ord)

A5_cov_raw <- A5_cov_raw %>%
  mutate(contrast = as.character(contrast),
         ord = match(contrast, names(contrast_list))) %>%
  arrange(ord) %>% select(-ord)

# Formatter
format_emm_contrasts <- function(df){
  df %>%
    transmute(
      Row = contrast,
      estimate = estimate,
      SE = SE,
      p = `p.value`
    ) %>%
    tidyr::pivot_longer(cols = c(estimate, SE), names_to = "which", values_to = "val") %>%
    group_by(Row) %>%
    summarise(
      Row = c(first(Row), ""),
      Value = c(
        fmt_est(first(val[which=="estimate"]), first(p)),
        fmt_se(first(val[which=="SE"]))
      ),
      .groups = "drop"
    ) %>%
    tidyr::unnest(c(Row, Value))
}

A5_col1 <- format_emm_contrasts(A5_nocov_raw)
A5_col2 <- format_emm_contrasts(A5_cov_raw)

TableA5_emm <- tibble(
  Row = A5_col1$Row,
  `(1)` = A5_col1$Value,
  `(2)` = A5_col2$Value
)

TableA5_emm <- bind_rows(
  TableA5_emm,
  tibble(
    Row = c("N observations", "N dyads"),
    `(1)` = as.character(c(nobs(m_mean_nocov), length(unique(df_long$Dyad_id)))),
    `(2)` = as.character(c(nobs(m_mean_cov),   length(unique(m_mean_cov@frame$Dyad_id))))
  )
)


TableA5_emm

# Export Table A5
wb <- createWorkbook()
addWorksheet(wb, "TableA5")
writeData(wb, "TableA5", TableA5_emm)
setColWidths(wb, "TableA5", cols = 1:3, widths = c(45, 18, 18))

saveWorkbook(wb, file.path(results_dir, "TableA5.xlsx"), overwrite = TRUE)



########################################################################
### Figure A2. Estimated Marginal Slopes
#               (based on pooled LMM testing slope differences)
#               Means + 95% CIs for all 4 groups


# -----------------------------
# Preconditions
# -----------------------------
stopifnot(all(c("Dyad_id","group4","rating","second_60") %in% names(df_long)))

df_long <- df_long %>%
  mutate(
    Dyad_id = factor(Dyad_id),
    group4  = factor(group4, levels = c("TD_Caregiver","TD_Youth","CHR_Youth","CHR_Caregiver")),
    second_60 = as.numeric(second_60)
  )

# Covariates
covars <- c("X3_Age", "X1_Sex_1", "c_demo_age_1", "c_gender_1")
covars <- covars[covars %in% names(df_long)]

# -----------------------------
# Helpers (stars + formatting)
# -----------------------------
stars <- function(p){
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*", ""))))
}
fmt_est <- function(est, p) sprintf("%.4f%s", est, stars(p))
fmt_se  <- function(se) sprintf("(%.4f)", se)

# -----------------------------
# Fit slope-difference pooled models
# -----------------------------

m_slope_nocov <- lmerTest::lmer(
  rating ~ group4 * second_60 + (1 | Dyad_id),
  data = df_long
)

rhs_cov <- paste(c("group4 * second_60", covars), collapse = " + ")

m_slope_cov <- lmerTest::lmer(
  as.formula(paste0("rating ~ ", rhs_cov, " + (1 | Dyad_id)")),
  data = df_long
)


trend_nocov <- emtrends(m_slope_nocov, ~ group4, var = "second_60")
trend_cov   <- emtrends(m_slope_cov,   ~ group4, var = "second_60")

# -----------------------------
# Plot the coefficients
# -----------------------------
trend_to_plot <- function(tr_obj, model_label){
  d <- as.data.frame(tr_obj)
  
  # identify the slope column (often "second_60.trend")
  slope_col <- grep("trend$", names(d), value = TRUE)[1]
  if (is.na(slope_col)) slope_col <- grep("trend", names(d), value = TRUE)[1]
  if (is.na(slope_col)) stop("Could not find trend column. Names: ", paste(names(d), collapse=", "))
  
  # CI columns may differ across setups
  lcol <- if ("lower.CL" %in% names(d)) "lower.CL" else if ("asymp.LCL" %in% names(d)) "asymp.LCL" else NA
  ucol <- if ("upper.CL" %in% names(d)) "upper.CL" else if ("asymp.UCL" %in% names(d)) "asymp.UCL" else NA
  if (is.na(lcol) | is.na(ucol)) stop("Could not find CI columns. Names: ", paste(names(d), collapse=", "))
  
  d %>%
    transmute(
      model = model_label,
      group4 = group4,
      slope = .data[[slope_col]],
      lower = .data[[lcol]],
      upper = .data[[ucol]]
    )
}

plot_df_slope <- bind_rows(
  trend_to_plot(trend_nocov, "No covariates"),
  trend_to_plot(trend_cov,   "With covariates")
) %>%
  mutate(
    group4 = factor(group4,
                    levels = c("TD_Caregiver","TD_Youth","CHR_Youth","CHR_Caregiver"),
                    labels = c("TD caregiver","TD youth","CHR youth","CHR caregiver"))
  )

figA2 <-
  ggplot(plot_df_slope, aes(x = group4, y = slope, shape = model)) +
  geom_point(position = position_dodge(width = 0.4), size = 2.2) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.4),
                width = 0.12) +
  labs(
    x = NULL,
    y = "Estimated marginal slope (per 60 seconds)",
    title = "Figure A2. Slope differences across participant groups from estimated marginal trends",
    shape = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

ggsave(filename = file.path(results_dir, "FigureA2.png"),
       figA2, width = 8, height = 4.6, dpi = 300, bg = "white")



########################################################################
### Table A6. Pairwise Comparisons from EMMs 
#               (after pooled LMM testing slope differences)
#               All 6 pairwise contrasts among:
#                   - TD youth
#                   - TD caregiver
#                   - CHR youth
#                   - CHR caregiver
#                     . Column (1): no covariates
#                     . Column (2): with youth & caregiver covariates

# Build the 6 contrasts as weight vectors over the 4 means:
# Order: [TD_Caregiver, TD_Youth, CHR_Youth, CHR_Caregiver]
contrast_list_slopes <- list(
  "TD caregiver - TD youth"         = c( 1, -1,  0,  0),
  "TD caregiver - CHR youth"        = c( 1,  0, -1,  0),
  "TD caregiver - CHR caregiver"    = c( 1,  0,  0, -1),
  "TD youth - CHR youth"            = c( 0,  1, -1,  0),
  "TD youth - CHR caregiver"        = c( 0,  1,  0, -1),
  "CHR youth - CHR caregiver"       = c( 0,  0,  1, -1)
)

A6_nocov_raw <- as.data.frame(contrast(trend_nocov, method = contrast_list_slopes, adjust = "none"))
A6_cov_raw   <- as.data.frame(contrast(trend_cov,   method = contrast_list_slopes, adjust = "none"))

A6_nocov_raw <- A6_nocov_raw %>%
  mutate(contrast = as.character(contrast),
         ord = match(contrast, names(contrast_list_slopes))) %>%
  arrange(ord) %>% select(-ord)

A6_cov_raw <- A6_cov_raw %>%
  mutate(contrast = as.character(contrast),
         ord = match(contrast, names(contrast_list_slopes))) %>%
  arrange(ord) %>% select(-ord)

format_slope_contrasts <- function(df){
  df %>%
    transmute(
      Row = contrast,
      estimate = estimate,
      SE = SE,
      p = `p.value`
    ) %>%
    tidyr::pivot_longer(cols = c(estimate, SE), names_to = "which", values_to = "val") %>%
    group_by(Row) %>%
    summarise(
      Row = c(first(Row), ""),
      Value = c(
        fmt_est(first(val[which=="estimate"]), first(p)),
        fmt_se(first(val[which=="SE"]))
      ),
      .groups = "drop"
    ) %>%
    tidyr::unnest(c(Row, Value))
}

A6_col1 <- format_slope_contrasts(A6_nocov_raw)
A6_col2 <- format_slope_contrasts(A6_cov_raw)

TableA6_slopes <- tibble(
  Row = A6_col1$Row,
  `(1)` = A6_col1$Value,
  `(2)` = A6_col2$Value
)

TableA6_slopes <- bind_rows(
  TableA6_slopes,
  tibble(
    Row = c("N observations", "N dyads"),
    `(1)` = as.character(c(nobs(m_slope_nocov), length(unique(df_long$Dyad_id)))),
    `(2)` = as.character(c(nobs(m_slope_cov),   length(unique(m_slope_cov@frame$Dyad_id))))
  )
)

# Export Table A6
wb <- createWorkbook()
addWorksheet(wb, "TableA6")
writeData(wb, "TableA6", TableA6_slopes)
setColWidths(wb, "TableA6", cols = 1:3, widths = c(45, 18, 18))

saveWorkbook(wb, file.path(results_dir, "TableA6.xlsx"), overwrite = TRUE)


########################################################################
### Figure A3. Additional LOESS Spans – TD Dyads
#                   - Span = 0.25
#                   - Span = 0.50
#                   - Span = 0.75

# Filter the data to include only observations where CHR == 0
df_td <- df_complete[df_complete$CHR == 0, ]

# Span = 0.25
figA3a <- ggplot(df_td, aes(x = second)) +
  stat_summary(aes(y = CrT_Rscr, color = "Youth", linetype = "Youth"), geom = "line", fun = mean, size = 0.6, show.legend = TRUE) +
  geom_smooth(aes(y = CrT_Rscr, color = "Youth", linetype = "Youth"), method = 'loess', span = 0.25, size = 1.25, show.legend = TRUE) +  # LOESS smoothing with adjustable span
  stat_summary(aes(y = RrT_Rscr, color = "Caregiver", linetype = "Caregiver"), geom = "line", fun = mean, size = 0.7, show.legend = TRUE) +
  geom_smooth(aes(y = RrT_Rscr, color = "Caregiver", linetype = "Caregiver"), method = 'loess', span = 0.25, size = 1.25, show.legend = TRUE) +  # LOESS smoothing with adjustable span
  labs(x = "Time (in seconds)",
       y = "Rating dial scores",
       title = "Youth and caregivers self-ratings",
       subtitle = "Conflict conversation",
       color = "Participant",
       linetype = "Participant") +
  scale_color_manual(
    values = c("Youth" = "black", "Caregiver" = "gray"),
    breaks = c("Youth", "Caregiver")
  ) +
  scale_linetype_manual(
    values = c("Youth" = "solid", "Caregiver" = "dashed"),
    breaks = c("Youth", "Caregiver")
  ) +
  theme(plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))

ggsave(
  filename = file.path(results_dir, "FigureA3a.png"),
  plot = figA3a,
  width = 7,
  height = 5,
  dpi = 300
)


# Span = 0.5
figA3b <- ggplot(df_td, aes(x = second)) +
  stat_summary(aes(y = CrT_Rscr, color = "Youth", linetype = "Youth"), geom = "line", fun = mean, size = 0.6, show.legend = TRUE) +
  geom_smooth(aes(y = CrT_Rscr, color = "Youth", linetype = "Youth"), method = 'loess', span = 0.5, size = 1.25, show.legend = TRUE) +  # LOESS smoothing with adjustable span
  stat_summary(aes(y = RrT_Rscr, color = "Caregiver", linetype = "Caregiver"), geom = "line", fun = mean, size = 0.7, show.legend = TRUE) +
  geom_smooth(aes(y = RrT_Rscr, color = "Caregiver", linetype = "Caregiver"), method = 'loess', span = 0.5, size = 1.25, show.legend = TRUE) +  # LOESS smoothing with adjustable span
  labs(x = "Time (in seconds)",
       y = "Rating dial scores",
       title = "Youth and caregivers self-ratings",
       subtitle = "Conflict conversation",
       color = "Participant",
       linetype = "Participant") +
  scale_color_manual(
    values = c("Youth" = "black", "Caregiver" = "gray"),
    breaks = c("Youth", "Caregiver")
  ) +
  scale_linetype_manual(
    values = c("Youth" = "solid", "Caregiver" = "dashed"),
    breaks = c("Youth", "Caregiver")
  ) +
  theme(plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))

ggsave(
  filename = file.path(results_dir, "FigureA3b.png"),
  plot = figA3b,
  width = 7,
  height = 5,
  dpi = 300
)


## Span = 0.75
figA3c <- ggplot(df_td, aes(x = second)) +
  stat_summary(aes(y = CrT_Rscr, color = "Youth", linetype = "Youth"), geom = "line", fun = mean, size = 0.6, show.legend = TRUE) +
  geom_smooth(aes(y = CrT_Rscr, color = "Youth", linetype = "Youth"), method = 'loess', span = 0.75, size = 1.25, show.legend = TRUE) +  # LOESS smoothing with adjustable span
  stat_summary(aes(y = RrT_Rscr, color = "Caregiver", linetype = "Caregiver"), geom = "line", fun = mean, size = 0.7, show.legend = TRUE) +
  geom_smooth(aes(y = RrT_Rscr, color = "Caregiver", linetype = "Caregiver"), method = 'loess', span = 0.75, size = 1.25, show.legend = TRUE) +  # LOESS smoothing with adjustable span
  labs(x = "Time (in seconds)",
       y = "Rating dial scores",
       title = "Youth and caregivers self-ratings",
       subtitle = "Conflict conversation",
       color = "Participant",
       linetype = "Participant") +
  scale_color_manual(
    values = c("Youth" = "black", "Caregiver" = "gray"),
    breaks = c("Youth", "Caregiver")
  ) +
  scale_linetype_manual(
    values = c("Youth" = "solid", "Caregiver" = "dashed"),
    breaks = c("Youth", "Caregiver")
  ) +
  theme(plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))

ggsave(
  filename = file.path(results_dir, "FigureA3c.png"),
  plot = figA3c,
  width = 7,
  height = 5,
  dpi = 300
)


########################################################################
### Figure A4. Additional LOESS Spans – CHR Dyads
#                   - Span = 0.25
#                   - Span = 0.50
#                   - Span = 0.75

# Filter the data to include only observations where CHR == 1
df_chr <- df_complete[df_complete$CHR == 1, ]

# Span = 0.25
figA4a <- ggplot(df_chr, aes(x = second)) +
  stat_summary(aes(y = CrT_Rscr, color = "CHR youth", linetype = "CHR youth"), geom = "line", fun = mean, size = 0.6, show.legend = TRUE) +
  geom_smooth(aes(y = CrT_Rscr, color = "CHR youth", linetype = "CHR youth"), method = 'loess', span = 0.25, size = 1.25, show.legend = TRUE) +
  stat_summary(aes(y = RrT_Rscr, color = "CHR caregiver", linetype = "CHR caregiver"), geom = "line", fun = mean, size = 0.7, show.legend = TRUE) +
  geom_smooth(aes(y = RrT_Rscr, color = "CHR caregiver", linetype = "CHR caregiver"), method = 'loess', span = 0.25, size = 1.25, show.legend = TRUE) +
  labs(x = "Time (in seconds)",
       y = "Rating dial scores",
       title = "CHR youth and caregivers self-ratings",
       subtitle = "Conflict conversation",
       color = "Participant",
       linetype = "Participant") +
  scale_color_manual(
    values = c("CHR youth" = "#1F77B4", "CHR caregiver" = "#AEC7E8"),
    breaks = c("CHR youth", "CHR caregiver")
  ) +
  scale_linetype_manual(
    values = c("CHR youth" = "solid", "CHR caregiver" = "dashed"),
    breaks = c("CHR youth", "CHR caregiver")
  ) +
  theme(plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))

ggsave(
  filename = file.path(results_dir, "FigureA4a.png"),
  plot = figA4a,
  width = 7,
  height = 5,
  dpi = 300
)


# Span = 0.5
figA4b <- ggplot(df_chr, aes(x = second)) +
  stat_summary(aes(y = CrT_Rscr, color = "CHR youth", linetype = "CHR youth"), geom = "line", fun = mean, size = 0.6, show.legend = TRUE) +
  geom_smooth(aes(y = CrT_Rscr, color = "CHR youth", linetype = "CHR youth"), method = 'loess', span = 0.5, size = 1.25, show.legend = TRUE) +
  stat_summary(aes(y = RrT_Rscr, color = "CHR caregiver", linetype = "CHR caregiver"), geom = "line", fun = mean, size = 0.7, show.legend = TRUE) +
  geom_smooth(aes(y = RrT_Rscr, color = "CHR caregiver", linetype = "CHR caregiver"), method = 'loess', span = 0.5, size = 1.25, show.legend = TRUE) +
  labs(x = "Time (in seconds)",
       y = "Rating dial scores",
       title = "CHR youth and caregivers self-ratings",
       subtitle = "Conflict conversation",
       color = "Participant",
       linetype = "Participant") +
  scale_color_manual(
    values = c("CHR youth" = "#1F77B4", "CHR caregiver" = "#AEC7E8"),
    breaks = c("CHR youth", "CHR caregiver")
  ) +
  scale_linetype_manual(
    values = c("CHR youth" = "solid", "CHR caregiver" = "dashed"),
    breaks = c("CHR youth", "CHR caregiver")
  ) +
  theme(plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))

ggsave(
  filename = file.path(results_dir, "FigureA4b.png"),
  plot = figA4b,
  width = 7,
  height = 5,
  dpi = 300
)


## Span = 0.75
figA4c <- ggplot(df_chr, aes(x = second)) +
  stat_summary(aes(y = CrT_Rscr, color = "CHR youth", linetype = "CHR youth"), geom = "line", fun = mean, size = 0.6, show.legend = TRUE) +
  geom_smooth(aes(y = CrT_Rscr, color = "CHR youth", linetype = "CHR youth"), method = 'loess', span = 0.75, size = 1.25, show.legend = TRUE) +
  stat_summary(aes(y = RrT_Rscr, color = "CHR caregiver", linetype = "CHR caregiver"), geom = "line", fun = mean, size = 0.7, show.legend = TRUE) +
  geom_smooth(aes(y = RrT_Rscr, color = "CHR caregiver", linetype = "CHR caregiver"), method = 'loess', span = 0.75, size = 1.25, show.legend = TRUE) +
  labs(x = "Time (in seconds)",
       y = "Rating dial scores",
       title = "CHR youth and caregivers self-ratings",
       subtitle = "Conflict conversation",
       color = "Participant",
       linetype = "Participant") +
  scale_color_manual(
    values = c("CHR youth" = "#1F77B4", "CHR caregiver" = "#AEC7E8"),
    breaks = c("CHR youth", "CHR caregiver")
  ) +
  scale_linetype_manual(
    values = c("CHR youth" = "solid", "CHR caregiver" = "dashed"),
    breaks = c("CHR youth", "CHR caregiver")
  ) +
  theme(plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "#CCCCCC"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))

ggsave(
  filename = file.path(results_dir, "FigureA4c.png"),
  plot = figA4c,
  width = 7,
  height = 5,
  dpi = 300
)


########################################################################
### Table A7. GAMM Nonlinearity Tests
#                   - Parametric coefficients (group mean differences)
#                   - Smooth term diagnostics:
#                     . edf
#                     . F
#                     . p-values
#                   - Model fit stats (N, adj. R²)


# ---- ensure a time variable called "second" exists ----
if (!("second" %in% names(df_long))) {
  if ("second_60" %in% names(df_long)) {
    df_long <- df_long %>% mutate(second = 60 * second_60)
  } else {
    stop("I can't find a time variable. Expected `second` or `second_60` in df_long.")
  }
}

# ---- ensure participant exists; derive from group4 if missing ----
if (!("participant" %in% names(df_long))) {
  if ("group4" %in% names(df_long)) {
    df_long <- df_long %>%
      mutate(
        participant = ifelse(grepl("Youth", as.character(group4)), "Youth",
                             ifelse(grepl("Caregiver", as.character(group4)), "Caregiver", NA_character_))
      )
  } else {
    stop("I can't build participant because df_long has no `participant` and no `group4`.")
  }
}

# ---- create subj_id (Dyad × participant) ----
if (!("subj_id" %in% names(df_long))) {
  df_long <- df_long %>%
    mutate(subj_id = interaction(Dyad_id, participant, drop = TRUE))
}

# ---- now enforce factors in the order you want ----
df_long <- df_long %>%
  mutate(
    Dyad_id = factor(Dyad_id),
    subj_id = factor(subj_id),
    group4  = factor(group4, levels = c("TD_Caregiver","TD_Youth","CHR_Youth","CHR_Caregiver")),
    second  = as.numeric(second),
    rating  = as.numeric(rating)
  )

df_long <- df_long %>%
  mutate(group4 = relevel(group4, ref = "TD_Caregiver"))

# -----------------------------
# Preconditions: df_long must exist and contain these variables
# -----------------------------
stopifnot(exists("df_long"))
stopifnot(all(c("Dyad_id","second","rating","group4","subj_id") %in% names(df_long)))

# Covariates (same as you’ve been using)
covars <- c("X3_Age", "X1_Sex_1", "c_demo_age_1", "c_gender_1")
covars <- covars[covars %in% names(df_long)]

df_model_nocov <- df_long %>%
  filter(!is.na(rating), !is.na(second), !is.na(group4), !is.na(Dyad_id), !is.na(subj_id)) %>%
  group_by(subj_id) %>% filter(n() >= 3) %>% ungroup() %>%
  droplevels()

df_model_cov <- df_model_nocov
if (length(covars) > 0) {
  df_model_cov <- df_model_cov %>%
    filter(if_all(all_of(covars), ~ !is.na(.))) %>%
    droplevels()
}


# -----------------------------
# Helpers for stars/formatting
# -----------------------------
stars <- function(p){
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*", ""))))
}
fmt_est <- function(est, p) sprintf("%.3f%s", est, stars(p))
fmt_se  <- function(se) sprintf("(%.3f)", se)
fmt_p   <- function(p)  sprintf("%.3f", p)

# -----------------------------
# Fit GAMMs (REML)
# -----------------------------
fit_gamm <- function(data, include_covars = FALSE, k = 20) {
  rhs <- if (!include_covars || length(covars) == 0) {
    "group4"
  } else {
    paste(c("group4", covars), collapse = " + ")
  }
  
  form <- as.formula(
    paste0("rating ~ ", rhs, " + s(second, by = group4, bs = 'tp', k = ", k, ")")
  )
  
  mgcv::gamm(
    formula = form,
    random = list(Dyad_id = ~ 1),
    correlation = nlme::corAR1(form = ~ second | subj_id),
    data = data,
    method = "REML"
  )
}

m_gamm_nocov <- fit_gamm(df_model_nocov, include_covars = FALSE, k = 20)
m_gamm_cov   <- fit_gamm(df_model_cov,   include_covars = TRUE,  k = 20)


# -----------------------------
# Extractors
# -----------------------------
extract_parametric <- function(gamm_obj){
  s <- summary(gamm_obj$gam)
  
  ptab <- as.data.frame(s$p.table) |>
    tibble::rownames_to_column("term") |>
    dplyr::rename(
      Estimate = Estimate,
      SE = `Std. Error`,
      p = `Pr(>|t|)`
    )
  
  wanted <- c("(Intercept)", "group4TD_Youth", "group4CHR_Youth", "group4CHR_Caregiver")
  
  label_map <- c(
    "(Intercept)"        = "Intercept (TD_Caregiver reference)",
    "group4TD_Youth"     = "TD_Youth vs TD_Caregiver",
    "group4CHR_Youth"    = "CHR_Youth vs TD_Caregiver",
    "group4CHR_Caregiver"= "CHR_Caregiver vs TD_Caregiver"
  )
  
  out <- ptab |>
    dplyr::filter(term %in% wanted) |>
    dplyr::mutate(term = factor(term, levels = wanted)) |>
    dplyr::arrange(term) |>
    dplyr::mutate(label = unname(label_map[as.character(term)]))
  
  # build rows explicitly (prevents any recycling/misalignment)
  res <- dplyr::bind_rows(lapply(seq_len(nrow(out)), function(i){
    dplyr::bind_rows(
      tibble::tibble(Row = out$label[i], Value = fmt_est(out$Estimate[i], out$p[i])),
      tibble::tibble(Row = "",            Value = fmt_se(out$SE[i]))
    )
  }))
  
  res
}


extract_smooths <- function(gamm_obj){
  s <- summary(gamm_obj$gam)
  
  # smooth table: edf, Ref.df, F, p-value
  stab <- as.data.frame(s$s.table) %>%
    tibble::rownames_to_column("smooth") %>%
    rename(p = `p-value`)
  
  # We expect smooth names like: "s(second):group4TD_Caregiver" etc
  smooth_label <- function(x){
    if (grepl("TD_Caregiver", x)) return("s(second): TD_Caregiver")
    if (grepl("TD_Youth", x))     return("s(second): TD_Youth")
    if (grepl("CHR_Youth", x))    return("s(second): CHR_Youth")
    if (grepl("CHR_Caregiver", x))return("s(second): CHR_Caregiver")
    return(paste0("s(second): ", x))
  }
  
  out <- stab %>%
    mutate(
      Row = vapply(smooth, smooth_label, character(1)),
      edf = edf,
      F   = `F`,
      p   = p
    ) %>%
    # keep only the four group smooths
    filter(grepl("TD_Caregiver|TD_Youth|CHR_Youth|CHR_Caregiver", smooth)) %>%
    # enforce desired order
    mutate(Row = factor(
      Row,
      levels = c("s(second): TD_Caregiver","s(second): TD_Youth",
                 "s(second): CHR_Caregiver","s(second): CHR_Youth")
    )) %>%
    arrange(Row) %>%
    mutate(
      Value = sprintf("edf=%.3f, F=%.3f (p=%s)", edf, F, fmt_p(p)),
      Row = as.character(Row)
    ) %>%
    select(Row, Value)
  
  out
}

extract_summary_stats <- function(gamm_obj){
  s <- summary(gamm_obj$gam)
  
  # mgcv stores N in the gam object
  nobs_ <- gamm_obj$gam$n
  if (is.null(nobs_) || is.na(nobs_)) {
    # fallback
    nobs_ <- length(gamm_obj$gam$y)
  }
  
  adj_r2 <- s$r.sq
  scale  <- s$scale
  
  tibble::tibble(
    Row = c("Adj. R-squared", "Scale estimate", "N observations"),
    Value = c(sprintf("%.4f", adj_r2),
              sprintf("%.4f", scale),
              sprintf("%.0f", nobs_))
  )
}


# -----------------------------
# Build Table A7 columns
# -----------------------------
build_A7_column <- function(gamm_obj){
  A <- extract_parametric(gamm_obj)
  B <- extract_smooths(gamm_obj)
  C <- extract_summary_stats(gamm_obj)
  
  # Section headers + combine
  bind_rows(
    tibble::tibble(Row = "(A) Parametric coefficients", Value = ""),
    A,
    tibble::tibble(Row = "(B) Smooth terms (penalized thin-plate)", Value = ""),
    B,
    tibble::tibble(Row = "(C) Model summary", Value = ""),
    C
  )
}

col1 <- build_A7_column(m_gamm_nocov) %>% rename(`(1)` = Value)
col2 <- build_A7_column(m_gamm_cov)   %>% rename(`(2)` = Value)

# Safety check: row structure must match exactly
stopifnot(nrow(col1) == nrow(col2))
stopifnot(identical(col1$Row, col2$Row))

TableA7 <- bind_cols(
  tibble(Row = col1$Row),
  col1 %>% select(`(1)`),
  col2 %>% select(`(2)`)
)


# -----------------------------
# Export to Excel
# -----------------------------
wb <- createWorkbook()
addWorksheet(wb, "TableA7")
writeData(wb, "TableA7", TableA7)

setColWidths(wb, "TableA7", cols = 1:3, widths = c(45, 22, 22))

# Add a footnote (as separate rows)
note <- tibble::tibble(
  Row = "Note:",
  `(1)` = "Column (1) reports GAMM estimates with penalized thin-plate regression splines; SEs in parentheses for parametric terms. Smooth terms report edf, F, and p-values.",
  `(2)` = "Column (2) reports the same GAMM specification adjusted for covariates (X3_Age, X1_Sex_1, c_demo_age_1, c_gender_1, as available)."
)
writeData(wb, "TableA7", note, startRow = nrow(TableA7) + 3, colNames = TRUE)

saveWorkbook(wb, file.path(results_dir, "Table7.xlsx"), overwrite = TRUE)


########################################################################
### Table A8. GAMM Sensitivity Analyses (Spline basis × basis dimension)
#               Robustness of GAMM conclusions to alternative spline bases and k values
#                   - Columns vary the spline basis:
#                       . TP = thin-plate regression splines
#                       . CR = cubic regression splines
#                       . PS = P-splines
#                   - Columns also vary the basis dimension (k = 10, 20, 40)
#               Reported components in each specification:
#                   - Panel A: Parametric coefficients (group mean differences; TD caregivers as reference)
#                   - Panel B: Smooth-term diagnostics for each group-specific smooth of time:
#                       . edf (effective degrees of freedom)
#                       . F statistic
#                       . p-value
#                   - Panel C: Model summary (Adj. R², scale estimate, N observations, and AIC)

fit_gamm_smooth <- function(data, include_covars = FALSE, k = 20, bs = "tp") {
  rhs <- if (!include_covars || length(covars) == 0) {
    "group4"
  } else {
    paste(c("group4", covars), collapse = " + ")
  }
  
  form <- as.formula(
    paste0("rating ~ ", rhs,
           " + s(second, by = group4, bs = '", bs, "', k = ", k, ")")
  )
  
  mgcv::gamm(
    formula = form,
    random = list(Dyad_id = ~ 1),
    correlation = nlme::corAR1(form = ~ second | subj_id),
    data = data,
    method = "REML"
  )
}

# Defining a grid
k_grid  <- c(10, 20, 40)
bs_grid <- c("tp", "cr", "ps")

sens_results <- list()

for (bs in bs_grid) {
  for (k in k_grid) {
    cat("\n--- Fitting bs =", bs, ", k =", k, "---\n")
    fit <- fit_gamm_smooth(df_model_cov, include_covars = TRUE, k = k, bs = bs)
    
    # Store: AIC + smooth table (edf/F/p)
    smtab <- as.data.frame(summary(fit$gam)$s.table)
    smtab$smooth <- rownames(smtab)
    
    sens_results[[paste0("bs_", bs, "_k_", k)]] <- list(
      AIC = AIC(fit$lme),
      s_table = smtab,
      gam = fit$gam
    )
  }
}

# Quick look at AIC across fits
sapply(sens_results, function(x) x$AIC)

# Main model diagnostics
mgcv::gam.check(m_gamm_cov$gam)

# Check one higher-k alternative
mgcv::gam.check(sens_results[["bs_tp_k_40"]]$gam)

#  --- Fitting bs = tp , k = 10 ---
#  --- Fitting bs = tp , k = 20 ---
#  --- Fitting bs = tp , k = 40 ---
# --- Fitting bs = cr , k = 10 ---
#  --- Fitting bs = cr , k = 20 ---
#  --- Fitting bs = cr , k = 40 ---
#  --- Fitting bs = ps , k = 10 ---
#  --- Fitting bs = ps , k = 20 ---
#  --- Fitting bs = ps , k = 40 ---


# -----------------------------
# Formatting helpers
# -----------------------------
stars <- function(p){
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*", ""))))
}
fmt_est <- function(est, p) sprintf("%.3f%s", est, stars(p))
fmt_se  <- function(se) sprintf("(%.3f)", se)
fmt_p   <- function(p)  sprintf("%.3f", p)

# -----------------------------
# Build A8 table from a GAM object
# -----------------------------
build_A8_from_gam <- function(gam_obj, aic_val = NA, nobs_override = NA){
  s <- summary(gam_obj)
  
  # --- Panel A: Parametric coefficients---
  ptab <- as.data.frame(s$p.table) %>%
    rownames_to_column("term") %>%
    rename(Estimate = Estimate, SE = `Std. Error`, p = `Pr(>|t|)`)
  
  wanted <- c("(Intercept)", "group4TD_Youth", "group4CHR_Youth", "group4CHR_Caregiver")
  label_map <- c(
    "(Intercept)"         = "Intercept (TD_Caregiver reference)",
    "group4TD_Youth"      = "TD_Youth vs TD_Caregiver",
    "group4CHR_Youth"     = "CHR_Youth vs TD_Caregiver",
    "group4CHR_Caregiver" = "CHR_Caregiver vs TD_Caregiver"
  )
  
  A <- ptab %>%
    filter(term %in% wanted) %>%
    mutate(term = factor(term, levels = wanted)) %>%
    arrange(term) %>%
    mutate(label = unname(label_map[as.character(term)]))
  
  panelA <- bind_rows(lapply(seq_len(nrow(A)), function(i){
    bind_rows(
      tibble(Row = A$label[i], Value = fmt_est(A$Estimate[i], A$p[i])),
      tibble(Row = "",         Value = fmt_se(A$SE[i]))
    )
  }))
  
  # --- Panel B: Smooth terms (edf/F/p) ---
  stab <- as.data.frame(s$s.table) %>%
    rownames_to_column("smooth")
  
  # find the p-value column robustly across mgcv versions
  pcol <- intersect(c("p-value", "p.value"), names(stab))[1]
  if (is.na(pcol)) stop("Could not find p-value column in s.table")
  
  # group label mapping
  smooth_label <- function(x){
    if (grepl("TD_Caregiver", x)) return("s(second): TD_Caregiver")
    if (grepl("TD_Youth", x))     return("s(second): TD_Youth")
    if (grepl("CHR_Youth", x))    return("s(second): CHR_Youth")
    if (grepl("CHR_Caregiver", x))return("s(second): CHR_Caregiver")
    return(paste0("s(second): ", x))
  }
  
  panelB <- stab %>%
    filter(grepl("TD_Caregiver|TD_Youth|CHR_Youth|CHR_Caregiver", smooth)) %>%
    mutate(
      Row = vapply(smooth, smooth_label, character(1)),
      p = .data[[pcol]],
      Value = sprintf("edf=%.3f, F=%.3f (p=%s)", edf, `F`, fmt_p(p))
    ) %>%
    mutate(Row = factor(Row,
                        levels = c("s(second): TD_Caregiver","s(second): TD_Youth",
                                   "s(second): CHR_Caregiver","s(second): CHR_Youth"))) %>%
    arrange(Row) %>%
    transmute(Row = as.character(Row), Value)
  
  # --- Panel C: Model summary ---
  nobs_ <- if (!is.na(nobs_override)) nobs_override else gam_obj$n
  if (is.null(nobs_) || is.na(nobs_)) nobs_ <- length(gam_obj$y)
  
  panelC <- tibble(
    Row = c("Adj. R-squared", "Scale estimate", "N observations", "AIC (lme)"),
    Value = c(sprintf("%.4f", s$r.sq),
              sprintf("%.4f", s$scale),
              sprintf("%.0f", nobs_),
              ifelse(is.na(aic_val), "", sprintf("%.2f", aic_val)))
  )
  
  # --- Combine panels ---
  out <- bind_rows(
    tibble(Row = "(A) Parametric coefficients", Value = ""),
    panelA,
    tibble(Row = "(B) Smooth terms (penalized)", Value = ""),
    panelB,
    tibble(Row = "(C) Model summary", Value = ""),
    panelC
  )
  
  out
}

# -----------------------------
# Export: one Excel per specification
# -----------------------------
export_A8_sensitivity_tables <- function(sens_results, prefix = "TableA8_sens"){
  for (nm in names(sens_results)) {
    gam_obj <- sens_results[[nm]]$gam
    aic_val <- sens_results[[nm]]$AIC
    
    tab <- build_A8_from_gam(gam_obj, aic_val = aic_val)
    
    wb <- createWorkbook()
    addWorksheet(wb, nm)
    writeData(wb, nm, tab)
    setColWidths(wb, nm, cols = 1:2, widths = c(45, 28))
    
    fname <- paste0(prefix, "_", nm, ".xlsx")
    saveWorkbook(wb, fname, overwrite = TRUE)
    
    message("Saved: ", fname)
  }
}


export_A8 <- function(sens_results, file = "TableA8.xlsx"){
  wb <- createWorkbook()
  
  for (nm in names(sens_results)) {
    gam_obj <- sens_results[[nm]]$gam
    aic_val <- sens_results[[nm]]$AIC
    tab <- build_A8_from_gam(gam_obj, aic_val = aic_val)
    
    addWorksheet(wb, nm)
    writeData(wb, nm, tab)
    setColWidths(wb, nm, cols = 1:2, widths = c(45, 28))
  }
  
  saveWorkbook(wb, file, overwrite = TRUE)
  message("Saved: ", file)
}

export_A8(sens_results, file = file.path(results_dir, "TableA8.xlsx"))