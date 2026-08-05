# Death-Education-Data

> **Draft package. Do not make this repository public until the ethics approval and consent materials have been checked for public participant-level data sharing.**

De-identified participant-level data for two school-based studies of death-related narrative and reflection, delay discounting, temporal resource allocation, and subjective time among children and adolescents in Shanghai, China.

## Repository contents

- `data/study1_public.csv` and `data/study1_public.xlsx`: Study 1 analysis-ready public data (`N = 250`).
- `data/study2_public.csv` and `data/study2_public.xlsx`: Study 2 analysis-ready public data (`N = 190`).
- `metadata/public_data_dictionary.csv` and `metadata/public_data_dictionary.xlsx`: variable definitions, coding, provenance, and missing-value counts.
- `docs/privacy_sanitization_report.md`: fields removed or retained and the residual disclosure-risk check.
- `code/README.md`: instructions for adding the final analysis scripts before release.
- `CITATION.cff`: draft dataset citation metadata; replace the team placeholder with the final contributor list before release.

CSV is the recommended machine-readable format. The Excel files contain the same values and are provided for convenient inspection.

## De-identification

The public files exclude school names, original student identifiers, questionnaire names, source row numbers, free-text responses, and variables not required for the analyses reported in the current manuscript. Public participant IDs were generated independently, rows were randomly reordered, and no ID linkage table was retained.

Exact age and gender are retained in both studies. Study 1 also retains exact family socioeconomic-status scores and school cohort because these variables are required to reproduce the reported adjusted grade-cohort analysis. This combination remains potentially identifying in a local school population: 116 of 250 Study 1 records (46.4%) have a unique combination of gender, age, school cohort, and socioeconomic-status score. Public release therefore requires an explicit ethics and consent review. If exact covariates are not authorized for public sharing, use a controlled-access repository or release a further-coarsened dataset with a revised analysis.

## Scoring and coding notes

- `ln_k_pre` and `ln_k_post` are natural-log-transformed delay-discounting parameters. Larger values indicate steeper delay discounting.
- Temporal allocations are percentages assigned to immediate, short-term, and long-term horizons.
- Subjective-time scores are three-item means on a `0-100` metric.
- Study 2 source row 192 had a derived pretest total inconsistent with its three raw ratings. The approved value, `(7 + 6 + 9) / 3 = 7.3333`, is used here; all other Study 2 subjective-time means are direct three-item means.
- Study 2 purpose responses were recoded from fixed Chinese response labels to five English categories. Implementation-intention responses were recoded as `1 = unlikely`, `2 = possible`, and `3 = very_likely`.
- Blank cells represent missing values. Study 1 has 11 missing exact-age values; other retained fields are complete.

## Collection information

Study 1 data were collected on site in school computer classrooms in Qingpu District, Shanghai, during June-July 2025. The verified Study 2 collection dates and the shared IRB number should be added before release.

## Reuse status

No public-use license is granted by this draft package. Add a license only after confirming that the ethics approval, participant/parent consent language, institutional policy, and coauthor agreement permit public reuse.
