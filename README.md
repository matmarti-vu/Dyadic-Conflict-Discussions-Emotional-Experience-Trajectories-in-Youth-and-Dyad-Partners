
This repository contains the R replication script used to reproduce the tables and figures reported in the manuscript and supplement of the study "Dyadic Conflict Discussions: Emotional Experience Trajectories in Youth and Their Caregivers"

**Data availability (important)**
The analytic data used in this project are not publicly available. Public data sharing was not included in the initial consent process given the sensitive nature of the study sample (e.g., clinical high-risk youth). As a result, this repository provides code only.
To run the replication script, you must obtain access to the data through the appropriate channels and prepare the input files in the format described below.

**Expected input files**
The replication script assumes you have access to two CSV files saved locally:
•	df_complete.csv
•	df_completeALT.csv
These are expected to be located in your data_dir (set at the top of the script). The script assumes these files contain second-by-second rating dial data and the variables needed to generate all manuscript/supplement tables and figures.

**Key structure (high-level)**
•	Second-by-second rows indexed by Dyad_id and second
•	A diagnostic indicator CHR (used to derive panel = TD vs CHR)
•	Self-rating dial variables for youth and caregiver:
  o	CrT_Rscr (youth self-rating)
  o	RrT_Rscr (caregiver self-rating)
•	Covariates used in adjusted models (as available):
  o	X3_Age, X1_Sex_1, c_demo_age_1, c_gender_1
•	Some tables (e.g., Table 1) also rely on additional summary variables (e.g., symptom measures and slopes).

**Table A2 medication exclusions (restricted information)**
Table A2 excludes a small set of medicated CHR participants/dyads. The dyad IDs required for this exclusion are not included in the public script to avoid sharing identifying information.
To reproduce Table A2 exactly, you will need to request the excluded dyad IDs from the authors.
The script is written so you can create an “exclusion sample” (e.g., df_longM) once you have the ID list and then run the Table A2 section.

**Outputs written to results_dir**
The script writes all outputs to the folder specified by:
results_dir <- "C:/Users/Path to results"
Outputs include (filenames may match those used in the manuscript/supplement):

**Manuscript outputs**
•	Table1.xlsx
•	Table2.xlsx
•	Figure1A.png
•	Figure1B.png
•	Figure2A.png
•	Figure2B.png

**Supplement outputs**
•	TableA1.xlsx
•	TableA2.xlsx (only if you define the medication-exclusion sample)
•	TableA3.xlsx
•	TableA4.xlsx
•	TableA5.xlsx
•	TableA6.xlsx
•	FigureA1.png
•	FigureA2.png
•	FigureA3a.png, FigureA3b.png, FigureA3c.png
•	FigureA4a.png, FigureA4b.png, FigureA4c.png
•	TableA7.xlsx
•	TableA8.xlsx

**How to run the script end-to-end**
1.	Clone or download this repository.
2.	Open the replication script in RStudio:
3.	Edit paths at the top of the script:
  o	data_dir → folder containing df_complete.csv and df_completeALT.csv
  o	results_dir → folder where outputs should be saved
4.	Run the script from top to bottom.
  o	The script installs/loads required packages, reads the data, constructs long-format datasets, fits models, and exports all tables/figures.
5.	To reproduce Table A2, obtain the restricted exclusion dyad IDs from the authors, create the exclusion sample as indicated in the Table A2 section, and then run that portion of the script.

**Notes on reproducibility**
•	Some procedures (e.g., cross-validation for LOESS span selection) may depend on random folds. If you want fully deterministic output, set a seed (e.g., set.seed(12345)) immediately before the relevant cross-validation code blocks.
•	The script assumes variable naming and coding consistent with the provided CSVs. If your files differ slightly (e.g., variable names or factor encodings), you may need to adapt the corresponding sections.

**Contact**
For questions about the code, required variable formats, or to request the medication-exclusion dyad IDs for Table A2, please contact:
Claudia Haase
claudia.haase@northwestern.edu
