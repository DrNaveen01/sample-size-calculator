# Implemented and deferred methods

| Feature | Implementation | Numerical verification | Scope |
|:---|:---|:---|:---|
| Single proportion and single mean | Preserved | Existing regression suite | Normal precision planning |
| Two proportions and means | Preserved and audited | 15 independent normal and directional/equivalence fixtures | Equality, superiority, non-inferiority, joint equivalence; fixed-size power |
| Pooled SD and custom names | Preserved | Engine and server checks | Separate, reported pooled, reference-weighted pooled |
| Yamane | Preserved | Regression checks | Fixed approximately 95% confidence and p=.5 |
| Trial planning plots | Added | Values computed by tested engines; infeasible gaps checked | Hypotheses, power, effect, clinical margins |
| AUC estimation and testing | Added | Independent SciPy density integration and count fixtures | Full AUC, Hanley-McNeil variance, normal approximation |
| Independent AUC comparison | Added | Independent count and power fixture | Separate cohorts; unequal cohort and disease-stratum allocation |
| Paired AUC comparison | Added | Independent count and power fixture, covariance sensitivity | Explicit correlation between estimated AUCs, held constant on resizing |
| Sensitivity and specificity estimation | Added | statsmodels Wilson/Wald expected-interval fixtures | Separate strata or expected prevalence-based recruitment |
| Joint diagnostic precision | Added | Quota and maximum recruitment checks | Individual endpoint confidence, not simultaneous coverage |
| Accuracy benchmark testing | Added | Independently integrated normal-score power | Sensitivity or specificity; directional or two-sided |
| Pearson correlation testing | Added | Independent Fisher-z integration fixtures | Zero or non-zero null, fixed-size power |
| Pearson correlation estimation | Added | Independent anticipated-interval fixture | Correlation-scale or Fisher-scale half-width |
| Independent correlation comparison | Added | Independent unequal-allocation fixture | Separate participant populations |
| Manual 2 by 2 measures | Added | statsmodels OR/RR/Newcombe intervals; stats Fisher zero-cell checks | Design-specific suppression; explicit correction; disjoint NNT sets |
| Shared reports and plots | Added to existing pipeline | Export/server checks | Portable self-contained Markdown; native Word equations; embedded PNG plots |
| Dependency preflight | Repaired | Original failure reproduced; new parsing and manifest generation passed | No publication during preflight |
| Exact Pearson t-test or bias-corrected correlation power | Deferred | Not implemented | Current Fisher-z method is explicitly uncorrected |
| Spearman, dependent and repeated-measures correlation | Deferred | Not implemented | Require separate validated methods and inputs |
| Partial AUC, pilot-data DeLong planning, binormal paired-score models | Deferred | Not implemented | Current paired method accepts estimated-AUC correlation, not score correlation |
| Exact binomial diagnostic testing and predictive-value planning | Deferred | Not implemented | Normal-score testing and Se/Sp precision are implemented |
| Cluster and paired group-outcome designs, exact t/Welch planning | Deferred | Not implemented | Existing independent normal methods remain labelled |

Independent computations are numerical implementation checks, not evidence that a planning approximation is adequate for every clinical study. For small strata, extreme probabilities or uncertain covariance, design-specific simulation remains outside this implementation.
