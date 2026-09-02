# Analysis scripts

Run all scripts from the repository root.

| Script | Purpose | Main outputs |
|---|---|---|
| `study1_analysis.R` | Validates Study 1 data; summarizes grade demographics; tests story-condition differences, pooled pre-post changes, and the baseline ln(*k*) grade difference | `results/study1/` |
| `study1_grade_comparison.R` | Compares Grade 4 and Grade 8 change scores, tests paired changes within each grade, and evaluates joint temporal-allocation change | `results/study1_grade_comparison/`; Figure 1 |
| `study2_analysis.R` | Validates Study 2 files; analyzes core pre-post outcomes, stated purposes, SS/LL choices, and implementation intentions | `results/study2/` |
| `figure_generation.R` | Creates pooled pre-post figures for both studies | Figures 2–4; `results/figures/` |

The analysis scripts use simple paired tests, independent-samples change-score tests, one-way ANOVA, a two-component multivariate allocation test, and participant-clustered regression models for repeated item responses. Temporal-allocation analyses use immediate and short-term allocation as the two nonredundant components because immediate, short-term, and long-term percentages sum to 100.

The scripts stop when sample sizes, age bounds, identifiers, item counts, factor levels, missingness, or allocation totals do not match the bundled data.
