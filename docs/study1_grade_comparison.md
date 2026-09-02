# Study 1 grade comparison

## Analysis purpose

This analysis evaluates whether Grade 4 and Grade 8 participants show different pre-post change patterns. It combines three complementary summaries:

1. A two-outcome MANOVA tests the overall grade difference in temporal-allocation change using immediate and short-term allocation. Long-term allocation is determined by the other two percentages because all three sum to 100.
2. Welch independent-samples *t* tests compare each change score between grades.
3. Paired *t* tests describe pre-post change within each grade.

## Main result

The joint allocation-change pattern differed by grade, Pillai's trace = .053, *F*(2, 227) = 6.35, *p* = .002. Grade 8 had a larger immediate-allocation increase and a larger short-term-allocation decline than Grade 4. The grade contrasts for ln(*k*), long-term allocation, and subjective time did not reach .05.

Both grades showed increases in ln(*k*) and immediate allocation, together with decreases in long-term allocation and subjective time. Short-term allocation increased descriptively in Grade 4, *p* = .068, and decreased in Grade 8, *p* = .009.

Figure 1 displays each grade as a separate horizontal dot-whisker estimate within every outcome panel. Points are mean posttest-minus-pretest changes, and whiskers are 95% confidence intervals.

Complete estimates are in:

- `results/study1_grade_comparison/grade_allocation_manova.csv`
- `results/study1_grade_comparison/grade_change_contrasts.csv`
- `results/study1_grade_comparison/grade_simple_effects.csv`
- `results/study1_grade_comparison/grade_prepost_descriptives.csv`
