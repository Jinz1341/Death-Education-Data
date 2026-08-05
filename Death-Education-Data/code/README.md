# Reproducing the analyses

The two study scripts are independent. Each reads one de-identified CSV file, validates the input, reproduces the reported analyses, and writes machine-readable result tables. `figure_generation.R` reads both datasets and creates the three combined figures.

## Requirements

- R 4.4 or later
- `readr`
- `dplyr`
- `tidyr`
- `purrr`

Install the packages once in R:

```r
install.packages(c("readr", "dplyr", "tidyr", "purrr"))
```

## Run

Run all commands from the repository root:

```bash
Rscript code/study1_analysis.R
Rscript code/study2_analysis.R
Rscript code/figure_generation.R
```

Study 1 results are written to `results/study1/`, Study 2 results to `results/study2/`, figure statistics to `results/figures/`, and graphics to `figures/main/`.

## Study 1

`study1_analysis.R` reproduces:

- story-version change-score tests for K value, temporal allocation, and subjective time;
- pooled paired-sample tests for the same outcomes;
- baseline Grade 4 versus Grade 8 Welch and Wilcoxon comparisons;
- Hedges' g; and
- the gender- and SES-adjusted baseline model.

## Study 2

`study2_analysis.R` reproduces:

- story-version change-score tests and pooled paired-sample tests;
- pretest and posttest purpose distributions;
- implementation-intention change;
- the conventional item-level Bowker benchmark; and
- participant-level Hotelling T-squared, sign-flip permutation, and Holm-adjusted pairwise net-flow analyses.

The Bowker benchmark treats 497 item pairs as independent and does not account for the seven pairs nested within each participant. The participant-level analyses provide the corresponding cluster-aware checks.

The public rows were randomly reordered during de-identification. Finite Monte Carlo draws may therefore differ slightly from earlier runs while retaining the same substantive interpretation. Deterministic statistics reproduce the reference results.

## Figures

`figure_generation.R` creates:

- Figure 1: Study 1 and Study 2 K-value pretest-posttest panels;
- Figure 2: grouped temporal-allocation bars for both studies; and
- Figure 3: subjective-time pretest-posttest panels for both studies.

The graphics use fixed colors, panel layouts, axis ranges, labels, confidence intervals, canvas dimensions, and 600-dpi PNG/PDF export settings to ensure reproducible presentation.

## Verification

```bash
Rscript tests/test_study1_analysis.R
Rscript tests/test_study2_analysis.R
Rscript tests/test_figure_generation.R
```

The scripts contain numerical assertions and stop if the public data do not reproduce the reference statistics.
