# Single proportion sample size method

The app plans a sample for estimating one population proportion under the normal approximation. It does not calculate hypothesis-test power.

## Calculation

$$
n_0=\frac{Z_{1-\alpha/2}^2 p(1-p)}{d^2}
$$

Here $p$ is the anticipated proportion; $d$ is the absolute margin of error; $1-\alpha$ is the two-sided confidence level; and $Z_{1-\alpha/2}$ is the corresponding standard-normal quantile. The R engine obtains this quantile with `qnorm(alpha / 2, lower.tail = FALSE)` and retains its full precision. A custom positive Z value can instead be supplied directly, with its implied confidence shown in the report.

For relative precision $\epsilon$, first compute $d=\epsilon p$. The user interface accepts percentages; the calculation function accepts proportions. For example, 10% relative precision at $p=0.20$ means $d=0.10\times0.20=0.02$, or 2 percentage points.

If the anticipated non-response proportion is $r$:

$$
n_{\mathrm{adj}}=\frac{n_0}{1-r},\qquad
n_{\mathrm{final}}=\left\lceil n_{\mathrm{adj}}\right\rceil.
$$

The complete-observation target is also displayed as $\lceil n_0\rceil$. Non-response inflation is applied to the unrounded base value and the recruitment target is rounded upward once. Inflating the already rounded complete-observation target can yield a different, slightly larger result. Both conventions must be distinguished when comparing calculators.

## Verified example

For $p=0.50$, $d=0.05$, confidence 95%, and $r=0.10$, the full-precision quantile is approximately 1.95996398454005. The base value is approximately 384.145882, the complete-observation target is 385, the adjusted value is approximately 426.828758, and the final recruitment target is 427. If the user explicitly enters Z=1.96, that exact supplied value is used instead.

## Assumptions

Observations are independent, the sample is representative, and the population is large relative to the sample. The app does not apply finite population correction, a cluster design effect, or an adjustment for measurement error. The normal approximation may be inadequate for rare outcomes or small category counts. Input-specific checks flag fewer than 10 expected observations in either category and an anticipated interval extending beyond the proportion bounds. Non-response inflation does not correct non-response bias.

## References

1. Lwanga SK, Lemeshow S. *Sample size determination in health studies a practical manual*. Geneva: World Health Organization; 1991. https://iris.who.int/handle/10665/40062
2. Penn State Department of Statistics. *STAT 500 Confidence intervals*. https://online.stat.psu.edu/stat500/Lesson05
3. Posit. *Shiny file downloads*. https://shiny.posit.co/r/reference/shiny/latest/downloadhandler.html
4. Posit. *Convert a document with Pandoc*. https://rmarkdown.rstudio.com/docs/reference/pandoc_convert.html
