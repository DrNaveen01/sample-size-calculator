# Sample size and power methods

All methods assume independent observations. Planning probabilities are fractions in the R API and percentages in the interface. Calculations retain full machine precision; displayed decimals are abbreviated. Power results are displayed as percentages with at most two decimal places. Editable group names map to the indexed parameters in the formula legends.

## Single population proportion

For expected proportion $p$, absolute margin $d$, and two-sided confidence $C=1-\alpha$:

$$n_0=\frac{Z_{1-\alpha/2}^2p(1-p)}{d^2}.$$

Relative precision $\epsilon$ is converted to $d=\epsilon p$. For example, 10% of $p=0.20$ gives $d=0.02$, or 2 percentage points. A custom positive Z can override confidence; the report shows implied confidence $C=2\Phi(Z)-1$.

## Single population mean

For anticipated population standard deviation $\sigma$ and absolute margin $d$ in the same units:

$$n_0=\frac{Z_{1-\alpha/2}^2\sigma^2}{d^2}.$$

The standard deviation is treated as known for planning. This Z approximation plans estimation precision, not hypothesis-test power. Estimated standard deviations and small samples may require a t-based design.

## Equality test for two independent proportions

Let $p_1,p_2$ be the expected event probabilities, $\delta=|p_2-p_1|$, $\alpha$ the two-sided significance level, $P=1-\beta$ target power, and $k=n_2/n_1$ the group 2 / group 1 allocation ratio.

$$\bar p=\frac{p_1+kp_2}{1+k},\qquad A=\sqrt{(1+1/k)\bar p(1-\bar p)}$$

$$B=\sqrt{p_1(1-p_1)+p_2(1-p_2)/k}$$

$$n_{1,0}=\frac{[Z_{1-\alpha/2}A+Z_{1-\beta}B]^2}{\delta^2},\qquad n_{2,0}=kn_{1,0}.$$

The null variance is pooled; the alternative variance is unpooled. This explicit approximation ignores the far rejection tail in the sizing step. The subsequent power check includes both tails at the rounded complete sizes. No continuity correction or exact binomial test is used.

Group 2 can be supplied directly or derived from an odds ratio or risk ratio for group 2 relative to group 1:

$$p_2=\frac{OR\,p_1}{1-p_1+OR\,p_1},\qquad p_2=RR\,p_1.$$

The derived value must be strictly between 0 and 1. These conversions provide expected probabilities for a difference-in-proportions calculation; they do not change the method to a logistic-regression, matched case-control, or RR-specific design.

## Equality test for two independent means

For anticipated means $\mu_1,\mu_2$, difference $\delta=|\mu_2-\mu_1|$, and standard deviations $\sigma_1,\sigma_2$:

$$n_{1,0}=\frac{(Z_{1-\alpha/2}+Z_{1-\beta})^2(\sigma_1^2+\sigma_2^2/k)}{\delta^2},\qquad n_{2,0}=kn_{1,0}.$$

Different standard deviations are allowed. They are treated as known planning values. This is a Z approximation and does not implement exact t-test or Welch power. The explicit sizing step ignores the far tail; the displayed power check includes both tails.

## Group sizes and approximate power

In sample-size mode, target power must be above 50% and below 100%. The equality objective requires a non-zero expected difference; non-inferiority and equivalence permit equal expected outcomes. $k=1$ gives equal groups; $k=2$ plans twice as many complete observations in group 2.

In power mode, enter complete, analysable integer group sizes $n_1,n_2\ge2$. The ratio is derived from these sizes, and no non-response inflation is applied. For the equality objective, a zero expected difference is permitted in power mode and gives approximate power equal to the significance level. Other objectives calculate power for their selected margins and directional hypothesis.

For proportions, the weighted proportion is recomputed after rounding or from supplied sizes:

$$\bar p_a=\frac{n_1p_1+n_2p_2}{n_1+n_2}$$

$$s_0=\sqrt{\bar p_a(1-\bar p_a)(1/n_1+1/n_2)},\qquad s_1=\sqrt{p_1(1-p_1)/n_1+p_2(1-p_2)/n_2}.$$

For means:

$$s_0=s_1=\sqrt{\sigma_1^2/n_1+\sigma_2^2/n_2}.$$

For both equality comparisons, $s_0$ and $s_1$ are null and alternative standard errors and $\Phi$ is the standard-normal cumulative distribution function:

$$u=\frac{\delta-Z_{1-\alpha/2}s_0}{s_1},\qquad v=\frac{-\delta-Z_{1-\alpha/2}s_0}{s_1}$$

$$P_{\mathrm{approx}}=\Phi(u)+\Phi(v).$$

These are approximate power calculations based on assumed effects and variances. They do not measure the probability that an observed result is true.

## Superiority and non-inferiority

Define the signed expected difference $D=p_2-p_1$ or $D=\mu_2-\mu_1$. Define $t=+1$ if a higher outcome is better and $t=-1$ if a lower outcome is better; $g=tD$ is the expected improvement in group 2.

Superiority tests $H_0:g\le M$ against $H_1:g>M$, with $M\ge0$. A zero superiority margin tests for any improvement. Non-inferiority tests $H_0:g\le-M$ against $H_1:g>-M$, with $M>0$ the largest clinically acceptable loss of benefit. Clinical margins require justification independent of study results.

For superiority, $h=g-M$; for non-inferiority, $h=g+M$. Sample-size mode requires $h>0$.

$$V=p_1(1-p_1)+p_2(1-p_2)/k \quad\text{or}\quad V=\sigma_1^2+\sigma_2^2/k.$$

$$n_{1,0}=\frac{(Z_{1-\alpha}+Z_{1-\beta})^2V}{h^2},\qquad n_{2,0}=kn_{1,0}.$$

Here alpha is **one-sided**. At actual complete sizes, $s$ is the unpooled standard error of the expected difference and approximate power is $\Phi(h/s-Z_{1-\alpha})$. This is a normal Wald approximation with the anticipated variance treated as fixed for planning. The binary method does not implement a constrained score test or exact binomial power.

## Equivalence

Margins $L<0<U$ apply to the signed group 2 minus group 1 difference $D$. The null is $D\le L$ or $D\ge U$; the alternative is $L<D<U$. Both one-sided tests must reject. Alpha is the significance level **for each test**; it is not divided by two. Alpha .05 corresponds to a 90% two-sided confidence interval for the difference.

For a standard error $s$:

$$a=\frac{L-D}{s}+Z_{1-\alpha},\qquad b=\frac{U-D}{s}-Z_{1-\alpha}$$

$$P_{\mathrm{joint}}=\max(0,\Phi(b)-\Phi(a)).$$

For sample size, use $s(n)=\sqrt{V/n}$ with the requested allocation and solve numerically for the unrounded group 1 size at the requested **joint** power. This handles asymmetric margins, unequal allocation, and non-zero expected differences. The expected difference must be strictly inside the margins in sample-size mode. Power mode permits an expected difference outside them and reports the resulting power.

The equality option remains a two-sided test of a zero difference. A non-significant equality test does not establish equivalence or non-inferiority.

## Pooled standard deviation for two means

Separate SDs retain potentially different variances. In pooled modes, set $\sigma_1=\sigma_2=s_p$ and assume a common population variance. A directly reported pooled SD is used unchanged.

For reference SDs $s_{R1},s_{R2}$ and reference sizes $m_1,m_2\ge2$, calculate the **within-group** pooled SD:

$$s_p=\sqrt{\frac{(m_1-1)s_{R1}^2+(m_2-1)s_{R2}^2}{m_1+m_2-2}}.$$

The reference sizes weight the SD by degrees of freedom. They are separate from planned group sizes and allocation. This pools within-group variances; it is not the SD obtained by combining all reference observations across different means. The report includes the reference inputs, generic formula, numerical substitution, and resulting planning SD.

## Taro Yamane finite-population formula

For known finite population size $N$ and precision fraction $e$:

$$n_0=\frac{N}{1+Ne^2}.$$

This is a simplified survey-proportion formula, assuming simple random sampling, approximately 95% confidence, and $p=.5$. The confidence and assumed proportion are fixed and power is not an input. The formula is not used for means, group comparisons, or diagnostic accuracy.

For $N=1000$ and $e=.05$, the unrounded size is approximately 285.714286 and the upward-rounded complete target is 286. With 10% non-response, recruitment is 318. The app flags a recruitment requirement exceeding the available population, rather than silently capping it. IFAS's tabulated sizes may round to the nearest participant; this app consistently rounds upward.

## Non-response and rounding

For a single sample, or separately for each group $i$, apply expected non-response fraction $r$ to the **unrounded** base size:

$$n_{i,\mathrm{adj}}=\frac{n_{i,0}}{1-r}.$$

The complete target $n_i$ and recruitment target $n_{\mathrm{final},i}$ are rounded upward from the respective unrounded values; total recruitment is $n_{\mathrm{final}}=n_{\mathrm{final},1}+n_{\mathrm{final},2}$. The same non-response fraction is applied to both groups. Independent rounding can slightly alter the requested allocation ratio.

For means 100 and 105 with SD 15 in both groups, 80% power, 5% two-sided significance, and equal allocation, each unrounded base size is approximately 141.279835. With 10% non-response, recruitment is 157 per group. Inflating the already rounded complete target of 142 would give 158 instead; the app explicitly uses the former convention.

## Assumptions and limits

No cluster design effect, paired correlation, multiplicity adjustment, exact t-test, or exact binomial method is applied. Only the Yamane calculator incorporates a finite population size. Superiority, non-inferiority, and equivalence calculations use the selected margin inputs. Rare binary outcomes and small expected category counts can make the normal approximation unreliable. Proportion reports flag fewer than 10 expected events or non-events; mean reports flag complete sizes below 30. These flags support review and do not guarantee accuracy above those thresholds. Non-response inflation does not correct bias.

## References

1. Lwanga SK, Lemeshow S. *Sample size determination in health studies a practical manual*. WHO; 1991. https://iris.who.int/handle/10665/40062
2. Penn State Department of Statistics. *STAT 500 Confidence intervals*. https://online.stat.psu.edu/stat500/Lesson05
3. Penn State Department of Statistics. *STAT 507 Power and sample size considerations*. https://online.stat.psu.edu/stat507/Lesson10
4. NCSS. *Tests for two proportions*. Technical details, pooled and unpooled normal approximations. https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/Tests_for_Two_Proportions.pdf
5. statsmodels. *Sample size for two independent proportions using one relevant tail*. https://www.statsmodels.org/stable/generated/statsmodels.stats.proportion.samplesize_proportions_2indep_onetail.html
6. statsmodels. *Normal power with specified null and alternative variances*. https://www.statsmodels.org/stable/generated/statsmodels.stats.power.normal_power_het.html

7. ICH. *E9 Statistical principles for clinical trials*, sections 3.3 and 3.5. https://www.ema.europa.eu/en/documents/scientific-guideline/ich-e-9-statistical-principles-clinical-trials-step-5_en.pdf
8. NCSS. *Non-inferiority tests for the difference between two proportions*. https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/Non-Inferiority_Tests_for_the_Difference_Between_Two_Proportions.pdf
9. NCSS. *Equivalence tests for the difference between two proportions*. https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/Equivalence_Tests_for_the_Difference_Between_Two_Proportions.pdf
10. Penn State Department of Statistics. *STAT 500 Comparing two population parameters*. https://online.stat.psu.edu/stat500/Lesson07
11. Israel GD. *Determining sample size*. University of Florida IFAS Extension, PEOD6. https://ask.ifas.ufl.edu/publication/PD006
