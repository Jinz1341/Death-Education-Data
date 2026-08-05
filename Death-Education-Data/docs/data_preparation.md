# Data Preparation and De-identification

## Release status

This repository contains the de-identified public-release datasets and reproducible analysis materials approved by the research team.

## Removed information

The released datasets exclude direct identifiers, school names, original participant identifiers, source row numbers, questionnaire names, free-text responses, and measures not required for the reported analyses. Redundant change scores and derived variables that can be recomputed from retained fields were also excluded.

## Retained information

- Both studies retain public participant IDs, narrative condition, age, gender, primary pretest and posttest outcomes, and variables required for the reported analyses.
- Study 1 retains school cohort and the Hollingshead-type socioeconomic-status score used in the baseline cohort analysis.
- Study 2 retains item-level purpose and implementation-intention responses used in the participant-level analyses.

## Identifier treatment

- Random public IDs were generated separately for each study.
- Row order was randomized before release.
- No original identifier, source row number, school name, or public-to-source mapping is included.

## Data-quality handling

Study 2 subjective-time scores use the three-item mean on a `0-100` metric. One pretest source total was inconsistent with its complete component ratings; the released score uses the component mean, `(7 + 6 + 9) / 3 = 7.3333`, according to the finalized scoring rule.
