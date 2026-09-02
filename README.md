# Death Education Studies: Data and Analysis Materials

This repository contains de-identified public data, reproducible R analyses, result tables, and manuscript figures for two school-based studies of death-related education and intertemporal decision-making.

## Samples

- **Study 1:** 230 participants. Grade 4 includes 104 participants aged 9–10; Grade 8 includes 126 participants, with observed ages 13–16.
- **Study 2:** 184 participants aged 10–16. The item-level file contains 2,576 rows (184 participants × 2 assessment time points × 7 items).

Anonymous participant IDs are stable within each study. Study 2 IDs match across its participant-level and item-level files.

## Repository contents

```text
code/       R analysis and figure-generation scripts
data/       de-identified public datasets
docs/       data-preparation and analysis summaries
figures/    generated manuscript figures
metadata/   public data dictionary
results/    machine-readable statistical results
tests/      reproducibility and release-hygiene checks
```

## Analyses

Study 1 first tests whether pre-post changes differ between the two story conditions. The developmental analysis then compares the pre-adolescent and middle-adolescent cohorts recruited through Grade 4 and Grade 8 classes, reports paired pre-post changes within each cohort, and evaluates the joint cohort difference in the two nonredundant components of resource allocation across temporal accounts. Baseline cohort differences in ln(*k*), pooled pre-post changes, age characteristics, and gender composition are also reported.

Study 2 tests story-condition differences, pooled pre-post changes in the core outcomes, and the joint change in resource allocation across temporal accounts. Item-level analyses describe stated resource-use purposes and use participant-clustered models for sooner-smaller choices and implementation intentions.

All reported component *p* values are unmodified two-sided *p* values.

## Reproduce the analyses

Run these commands from the repository root:

```bash
Rscript code/study1_analysis.R
Rscript code/study1_grade_comparison.R
Rscript code/study2_analysis.R
Rscript code/figure_generation.R
```

The scripts require the R packages `readr`, `dplyr`, `tidyr`, `purrr`, and `ggplot2`. Results are written to `results/`; figures are written to `figures/main/`.

Run the checks with:

```bash
Rscript tests/test_study1_analysis.R
Rscript tests/test_study1_grade_comparison.R
Rscript tests/test_study2_analysis.R
Rscript tests/test_figure_generation.R
Rscript tests/test_release_hygiene.R
```

## Figures

- **Figure 1:** Cohort-specific pre-post changes in Study 1 for the developmental cohorts recruited through Grade 4 and Grade 8 classes, displayed as horizontal dot-whisker estimates.
- **Figure 2:** Pooled pre-post ln(*k*) estimates for Studies 1 and 2.
- **Figure 3:** Pooled pre-post estimates of resource allocation across temporal accounts for Studies 1 and 2.
- **Figure 4:** Pooled pre-post estimates of subjective feeling of time passage for Studies 1 and 2.

PNG files support manuscript use; PDF files are vector formats suitable for editing. Figure 1 is also supplied as EPS.

## Interpretation

The studies used pre-post designs without a neutral or no-session control group. Results therefore describe observed pre-post changes associated with the educational sessions.

## Citation

Jin Zhang and Yangyang Jin contributed equally and share first authorship. Xiaotian Wang is the corresponding author. Machine-readable repository citation metadata are provided in `CITATION.cff`.
