# Additional statistical methods

The application distinguishes estimation precision from hypothesis testing. All R APIs use fractions for probabilities. Interface inputs state whether they use percentages, percentage points, correlation units or AUC units.

## ROC AUC

Full-AUC variance uses Hanley and McNeil's approximation:

$$
V(A;m,n)=\frac{A(1-A)+(m-1)(Q_1-A^2)+(n-1)(Q_2-A^2)}{mn},\quad Q_1=A/(2-A),\quad Q_2=2A^2/(1+A).
$$

Here m is the diseased count and n is the non-diseased count. AUC precision is the normal confidence-interval half-width on the AUC scale. The normal interval is reported without clipping it to claim better precision.

Single-AUC tests allow an arbitrary null AUC. Null variance uses that null value; alternative variance uses the anticipated value. Independent comparisons sum variances from separate cohorts. Cohort 2 counts follow a declared cohort ratio. Paired comparisons use the same participants and variance of the difference `V1 + V2 - 2*rho*sqrt(V1*V2)`. The required rho is correlation of the estimated AUCs, not raw score correlation. The current method holds rho constant during resizing and displays a covariance sensitivity plot. It is not DeLong-based pilot-data planning.

Comparison null variance uses the common mean of the two anticipated AUCs. A normal rejection probability is integrated through its CDF and solved using bounded whole-number bisection. Paired and independent comparisons are different objectives with different participant counts. Fixed-size power uses complete counts without losses.

Population recruitment applies prevalence to the required disease strata. This plans expected case/non-case counts rather than guaranteeing realised quotas. Separate-stratum recruitment directly reports each required quota. Null and alternative differences refer to first AUC minus null or comparator AUC.

Reference: Hanley JA, McNeil BJ. Radiology. 1982;143:29-36. https://doi.org/10.1148/radiology.143.1.7063747

Covariance context: DeLong ER, DeLong DM, Clarke-Pearson DL. Biometrics. 1988;44:837-845. https://doi.org/10.2307/2531595

## Diagnostic accuracy

Sensitivity requires reference-standard diseased participants; specificity requires non-diseased participants. Precision is explicitly interval half-width. Default Wilson planning uses the anticipated proportion and seeks the first count with expected Wilson half-width at most d. Wald planning uses `z^2*p*(1-p)/d^2` with whole-count verification. Joint estimation takes the largest prevalence-based population requirement or reports separate quotas. Both endpoint intervals use individual confidence levels; simultaneous coverage is not claimed.

Testing uses a normal score threshold with null variance `p0*(1-p0)/n`, alternative variance `p1*(1-p1)/n`, and either one rejection tail or both. This is not an exact binomial calculation. The result warns about fewer than ten expected events or non-events. The reference standard, threshold and anticipated disease spectrum must be justified by the study.

Population counts are `max(n_diseased/prevalence, n_non_diseased/(1-prevalence))`, rounded upward, then inflated for expected losses. Expected prevalence is not a probability guarantee for realised strata.

References: Buderer NM. Acad Emerg Med. 1996;3:895-900. https://doi.org/10.1111/j.1553-2712.1996.tb03538.x

Wilson EB. J Am Stat Assoc. 1927;22:209-212. https://doi.org/10.1080/01621459.1927.10502953

## Pearson correlation

The engine uses the uncorrected Fisher transform `atanh(r)` and variance `1/(n-3)`. One-correlation tests compare with a zero or non-zero null; independent comparison adds `1/(n1-3) + 1/(n2-3)`. Power includes both tails for a two-sided test and the declared tail for a directional test. All tests are approximate normal Fisher-z tests, not exact Pearson t-test power.

For estimation, the anticipated interval is `tanh(atanh(r) +/- z/sqrt(n-3))`. Correlation-scale half-width is half its total width, not necessarily the largest distance from r. Fisher-scale half-width is `z/sqrt(n-3)`. The anticipated interval is reported on the correlation scale. Counts represent participants with complete pairs of measurements.

Reference: Fisher RA. On the probable error of a coefficient of correlation deduced from a small sample. Metron. 1921;1:3-32. https://digital.library.adelaide.edu.au/items/002ad8fb-c23c-407b-8a89-f036d8da6030

Official R documentation: https://search.r-project.org/R/refmans/stats/html/cor.test.html

The bias-corrected pwr implementation is a different method: https://search.r-project.org/CRAN/refmans/pwr/html/pwr.r.test.html

## Manual effect measures

Rows are exposed/treatment and unexposed/control; columns are adverse outcome present and absent. Positive-cell OR and RR use log-Wald confidence intervals. Risk difference uses Newcombe's independent Wilson interval (method 10). ARR is negative RD; attributable fraction among exposed is `1 - 1/RR`, with monotone transformation of RR limits. NNT or NNH uses `1/abs(RD)`. Confidence sets crossing RD zero are disjoint and unbounded, not finite intervals spanning both benefit and harm.

By default, no continuity correction is applied. A zero cell switches the OR to R's Fisher conditional estimate and exact interval and labels the change. Undefined RR intervals remain unavailable. An explicit alternative adds 0.5 to all four cells if any cell is zero and labels every corrected result. Case-control designs suppress all risk-based measures; cross-sectional designs display prevalence ratio and difference and suppress benefit measures.

References: Newcombe RG. Stat Med. 1998;17:873-890. https://doi.org/10.1002/(SICI)1097-0258(19980430)17:8%3C873::AID-SIM779%3E3.0.CO;2-I

https://search.r-project.org/R/refmans/stats/html/fisher.test.html

https://www.statsmodels.org/stable/generated/statsmodels.stats.contingency_tables.Table2x2.html

## Rounding and plots

Existing calculators retain their original unrounded-size inflation convention. New modules use minimum whole-number complete stratum/participant quotas, then round each quota divided by `1-loss` upward. This is deliberate: new modules are solved for whole quotas, so inflating those quotas preserves expected analysable quotas. Fixed-size power and observed tables never inflate counts.

Plot data are produced by the calculation engines, with infeasible inputs represented by missing values so lines do not bridge invalid regions. Clinical and rejection boundaries are separate in trial plots. Plot PNG and CSV downloads are available; self-contained Markdown embeds plot data as PNG data URIs, allowing the same figures to appear in the live preview, clipboard, PDF and Word outputs. Data URIs increase Markdown file size.
