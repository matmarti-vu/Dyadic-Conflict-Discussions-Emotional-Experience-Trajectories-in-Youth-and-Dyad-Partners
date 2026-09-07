# Emotional Experience Trajectories — Reproducibility README

This repository contains the R analysis workflow used to generate the manuscript and supplemental exhibits for **Dyadic Conflict Discussions: Emotional Experience Trajectories in Youth and Dyad Partners**.

The public analysis code is intended to make the statistical workflow transparent while protecting participant-level data. The script assumes that a secure/private data-preparation workflow has already created the analytic R objects described below.

**Validation status.** The complete analysis pipeline was run successfully on September 6, 2026 using the final analytic objects and the two Table A2 exclusion IDs. The generated manuscript and supplemental outputs were reviewed for exhibit numbering, reference-group coding, model specifications, and figure-data consistency.

## Main analysis file

`Emotional_Experience_Trajectories.R`

The script is organized in manuscript order. Before every manuscript-facing table or figure, comments describe the purpose of the exhibit, the analytic sample, the model specification, the reference group, and any important interpretation or reproducibility details. Progress messages are also printed while the script runs.

All generated files are written to a folder called:

```text
R-script/
```

under the project root.

## Required analytic objects

Before conducting the analysis script, the active R session must contain:

- `df_complete` — source for Table 1 and trajectory/LOESS figures.
- `df_long` — balanced second-level analytic sample used by the primary models.
- `df_longALT` — all-available-data long sample used for Table A1.

See the Contact section below to who to email to request access to the data.

Core variables include `Dyad_id`, `group4`, `rating`, `second_60`, youth age/sex (`X3_Age`, `X1_Sex_1`), dyad-partner age/sex (`c_demo_age_1`, `c_gender_1`), and the source variables needed for descriptive statistics and relationship-composition analyses.

## Final exhibit map

### Main manuscript

- **Table 1** — Sample characteristics.
- **Table 2** — Pooled linear mixed-effects models of rating-dial trajectories.
- **Figure 1** — Four-panel linear and cross-validated LOESS trajectory figure.

### Supplement

- **Table A1** — Pooled linear models using all available rating data.
- **Table A2** — Pooled linear models excluding CHR youth taking antipsychotic medication.
- **Table A3** — Biological-parent dyads only.
- **Table A4** — Non-CHR biological-parent dyads versus CHR dyads involving non-biological dyad partners.
- **Figure A1** — CHR trajectories by dyad-partner relationship.
- **Table A5** — Linear mixed-effects models estimated separately within Non-CHR and CHR dyads.
- **Table A6** — Average rating-dial differences across participant groups.
- **Figure A2** — Estimated marginal means.
- **Table A7** — Pairwise differences in estimated marginal means.
- **Figure A3** — Estimated marginal slopes.
- **Table A8** — Pairwise differences in estimated marginal slopes.
- **Figure A4** — Non-CHR LOESS sensitivity at spans 0.25, 0.50, and 0.75.
- **Figure A5** — CHR LOESS sensitivity at spans 0.25, 0.50, and 0.75.
- **Table A9** — Generalized additive mixed models (GAMMs) with group-specific smooths and AR(1) residual correlation.
- **Figure A6** — Youth-covariate-adjusted GAMM-fitted trajectories.
- **Figure A7** — Dyad-partner-covariate-adjusted GAMM-fitted trajectories.
- **Table A10** — GAMM spline-basis/basis-dimension sensitivity with youth covariates.
- **Table A11** — GAMM spline-basis/basis-dimension sensitivity with dyad-partner covariates.

There is **no manuscript-facing Figure A8** in the final sequence.

## Primary linear trajectory models

`second_60` is the linear time variable; one unit corresponds to 60 seconds.

The main slope-model family reports three specifications:

1. **No demographic covariates** — participant group, time, and group × time.
2. **Youth covariates** — mean-centered youth age and sex, plus youth age × time and youth sex × time.
3. **Dyad-partner covariates** — mean-centered dyad-partner age and sex, plus dyad-partner age × time and dyad-partner sex × time.

Youth and dyad-partner demographic adjustments are estimated in separate models to reduce the risk of collinearity and overfitting given the dyad sample size.

With the demographic × time terms, adjusted time slopes are evaluated at zero on the centered demographic variables, corresponding to their analytic-sample centering values.

## Table 1 slope variable

The descriptive slope variables used in Table 1 (`s10m_Rself_y` and `s10m_Rself_c`) represent participant-specific linear **change across the full 10-minute conversation**. They should not be interpreted as the per-minute coefficients reported by the linear mixed-effects models in Table 2.

The Table 1 workbook includes a `Notes` worksheet documenting this distinction.

Relationship-composition counts (biological parent, romantic partner, and other relationship) describe the non-youth member of the dyad and are therefore written in the **Dyad partner** column of Table 1.

## Mean-level models

Table A6, Figure A2, and Table A7 address average rating-dial levels rather than temporal slopes. Their adjusted specifications therefore include centered age and sex as **main effects only**. They do not include demographic × time interactions.

## Table A5 reference coding

Table A5 fits models separately within Non-CHR and CHR dyads. In both strata, **Dyad partner is the participant reference category**.

Therefore:

- the `second_60` coefficient is the dyad-partner slope;
- the `Youth × time` coefficient is the youth-minus-dyad-partner slope difference; and
- the youth slope is the linear combination of those two coefficients.

The exported workbook includes coefficient and pooled-versus-separate-slope audit sheets. These help document why an absolute simple slope from a separately estimated model can differ from the corresponding pooled-model simple slope even when the within-dyad slope difference is stable.

## LOESS analyses

The LOESS curves are treated as **descriptive visualizations**.

For Figure 1, a separate span is selected for each participant group using 5-fold cross-validation, local-linear smoothing (`degree = 1`), and candidate spans from 0.05 to 0.95 in increments of 0.01. The selected spans are written to `Figure1_span_selection.xlsx`.

The selected spans were **0.10 for Non-CHR youth, 0.08 for Non-CHR dyad partners, 0.09 for CHR youth, and 0.09 for CHR dyad partners**.

Figures A4 and A5 use fixed spans of 0.25, 0.50, and 0.75 to show how the displayed group-average trajectory changes with the degree of smoothing.

## GAMMs

Table A9 fits generalized additive mixed models with:

- group-specific penalized thin-plate smooths of `second`;
- `k = 20` for the primary fits;
- a random intercept for `Dyad_id`;
- first-order autoregressive residual correlation within participant;
- REML estimation.

The three Table A9 specifications are:

1. no demographic covariates;
2. mean-centered youth age and sex;
3. mean-centered dyad-partner age and sex.

Demographic × time terms are not added to the GAMMs because temporal form is represented by the group-specific smooth functions.

For each smooth, the estimated degrees of freedom (`edf`) summarize fitted curve complexity; values near 1 indicate an approximately linear fitted trajectory. The smooth-term p-value is an overall test of the smooth term and is **not** a direct test of nonlinearity.

Figures A6 and A7 evaluate the centered demographic covariates at zero, corresponding to their analytic-sample means on the centered scale, and exclude dyad-level random effects from the population-level predictions.

## GAMM sensitivity analyses

Tables A10 and A11 cross:

- spline basis: thin-plate (`tp`), cubic regression (`cr`), and P-spline (`ps`);
- basis dimension: `k = 10`, `20`, and `40`.

The primary thin-plate `k = 20` model from Table A9 is reused exactly. The script contains an explicit cross-table assertion requiring the reused Table A10 and A11 columns to match the corresponding Table A9 adjusted-model columns, including adjusted R-squared. This guards against manual transcription mismatches between the primary and sensitivity tables.

AIC values are retained in dedicated `AIC_summary` worksheets and are not included in the manuscript-facing table body.

## Pairwise contrasts

Tables A7 and A8 report the six pairwise contrasts among the four participant groups with:

```r
adjust = "none"
```

The manuscript/Supplement should therefore state that no multiple-comparison adjustment was applied.

## Table A2 exclusion IDs

Table A2 excludes two dyads associated with CHR youth taking antipsychotic medication. To keep the workflow simple, the two IDs are entered directly near the top of the analysis script:

```r
ID1 <- "REPLACE_WITH_DYAD_ID_1"
ID2 <- "REPLACE_WITH_DYAD_ID_2"
```

Before running the analysis, replace those two placeholders with the appropriate dyad IDs. No additional configuration file is required.

## Software

The manuscript analyses were developed in R 4.5.0. The script checks for these packages at startup:

```r
install.packages(c(
  "caret", "cowplot", "dplyr", "emmeans", "gam", "ggplot2",
  "lme4", "lmerTest", "mgcv", "nlme", "openxlsx", "purrr",
  "rlang", "stringr", "tibble", "tidyr"
))
```

The validated September 6, 2026 run used the following package versions: caret 7.0.1, cowplot 1.2.0, dplyr 1.1.4, emmeans 2.0.1, gam 1.22.7, ggplot2 4.0.3, lme4 1.1.38, lmerTest 3.2.0, mgcv 1.9.4, nlme 3.1.168, openxlsx 4.2.8.1, purrr 1.1.0, rlang 1.1.6, stringr 1.5.1, tibble 3.3.0, and tidyr 1.3.1. The script exports the installed versions automatically so later runs remain auditable.

At the end of a successful run, the script writes:

- `package_versions.csv`
- `sessionInfo.txt`
- `analysis_specification.txt`

These should be retained with the archived reproducibility release.

## Running the analysis

From a clean R session:

```r
setwd("path/to/project-root")

# Run/load the secure data-preparation workflow so that:
# df_complete, df_long, and df_longALT exist.

# Replace ID1 and ID2 near the top of the analysis script with the two Table A2 dyad IDs.

source("Emotional_Experience_Trajectories_analysis.R")
```

The GAMM sensitivity analyses in Tables A10 and A11 can take substantially longer than the linear-model sections.

## Expected principal output files

```text
R-script/
  Table1.xlsx
  Table2.xlsx
  Figure1.png
  Figure1_span_selection.xlsx

  TableA1.xlsx
  TableA2.xlsx
  TableA3.xlsx
  TableA4.xlsx
  FigureA1.png
  TableA5.xlsx
  TableA6.xlsx
  FigureA2.png
  FigureA2_data.xlsx
  TableA7.xlsx
  FigureA3.png
  FigureA3_data.xlsx
  TableA8.xlsx
  FigureA4.png
  FigureA5.png
  TableA9.xlsx
  FigureA6.png
  FigureA6_data.xlsx
  FigureA7.png
  FigureA7_data.xlsx
  TableA10.xlsx
  TableA11.xlsx

  analysis_specification.txt
  package_versions.csv
  sessionInfo.txt
```

Several Excel workbooks contain additional audit worksheets (model formulas, centering values, raw model output, exclusion checks, or pairwise-contrast details). These are intentional and provide a transparent bridge between fitted R objects and the manuscript-facing tables.

## Data privacy

Do not commit protected participant data, real Table A2 exclusion IDs, or local R workspaces that contain participant-level information. The analysis uses relative paths and does not require machine-specific absolute paths.

## Contact
For questions about accessing the database, the code, required variable formats, or to request the medication-exclusion dyad IDs for Table A2, please contact:
Claudia Haase
claudia.haase@northwestern.edu
