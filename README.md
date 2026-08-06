# CGM Dietary Study: AUS vs MED vs LC

R analysis code for a short-term crossover study comparing the acute glycaemic
effects of three isocaloric diets ( Australian (AUS), Mediterranean (MED), and
low-carbohydrate (LC)) in adults at elevated risk of type 2 diabetes, using
continuous glucose monitoring (CGM).

## Scripts

Run in order:

1. **`01_data_setup.R`** : reads the raw CGM and meal data, calculates the
   glycaemic metrics (mean glucose, CV%, MAGE, time-in-range measures), and
   builds two analysis datasets (`full_data`, `daily_data`). Saves
   `CGM_Study.RData` and `CGM_Descriptives.xlsx`.
2. **`02_statistical_analysis.R`** : linear mixed-effects models for the diet
   comparisons, non-parametric tests for the bounded outcomes, random-slope
   models for inter-individual variability, and exploratory moderation. Saves
   `CGM_Results.xlsx`.
3. **`03_visualisations.R`** : the four figures reported in the thesis
   (compliance, CGM outcomes panel, ambulatory glucose profile, age vs TITR).
4. **`04_individual_response.R`** : per-participant responder classification and
   the composite best-diet ranking, with confirmatory tests. Saves
   `CGM_Individual_Response.xlsx`.

## Requirements

- R (version 4.5.3 or later)
- Packages: `tidyverse`, `lubridate`, `readxl`, `writexl`, `lme4`, `lmerTest`,
  `emmeans`, `performance`, `patchwork`

## Data availability

The raw participant data are **not** included in this repository. Because the
dataset is small (n = 23), even de-identified CGM traces combined with age, sex,
and BMI could be identifying. The data are available from the author on
reasonable request, subject to the study's ethics approval
(Murdoch University Human Research Ethics Committee, Protocol 2020/435).

To reproduce the analysis, place the data files in the working directory set at
the top of `01_data_setup.R`:

- `all_CGM_data.csv`
- `Participants_compliance.xlsx`

## Notes

- AUS is set as the model reference category for statistical purposes only; no
  diet served as a control.
- MAGE is calculated per day and averaged, following Service et al. (1970).
- CGM thresholds follow Battelino et al. (2019): TIR 3.9–10.0, TITR 3.9–7.8,
  TAR >10.0, TBR <3.9 mmol/L.
