"""Optional developer verification; Python is not an app dependency.

Rebuild reference fixtures by integrating the normal estimator density over
its rejection region. This independently checks the R sizing and CDF paths.
Requires scipy and statsmodels; run from the project root.
"""
from pathlib import Path
from math import sqrt, ceil
import csv
from scipy.stats import norm
from scipy.integrate import quad
from scipy.optimize import brentq
from statsmodels.stats.power import normal_power_het

cases = [
    ('two_means', 'superiority', 0, 5, 15, 20, None, None, 2, .025, 0, None, None),
    ('two_means', 'noninferiority', 0, -1, 10, 12, None, None, 2, .025, 4, None, None),
    ('two_means', 'equivalence', 0, 1, 10, 12, None, None, 2, .05, 0, -3, 5),
    ('two_means', 'equivalence', 0, -.5, 10, 10, None, None, .5, .025, 0, -2, 3),
    ('two_proportions', 'superiority', None, None, None, None, .2, .3, 2, .025, 0, None, None),
    ('two_proportions', 'noninferiority', None, None, None, None, .6, .58, 1, .025, .05, None, None),
    ('two_proportions', 'equivalence', None, None, None, None, .2, .21, 2, .05, 0, -.05, .04),
]

rows = []
for kind, objective, m1, m2, s1, s2, p1, p2, ratio, alpha, margin, lower, upper in cases:
    difference = m2-m1 if kind == 'two_means' else p2-p1

    def variance_factor(k):
        return s1*s1+s2*s2/k if kind == 'two_means' else p1*(1-p1)+p2*(1-p2)/k

    def power(n1, k):
        se = sqrt(variance_factor(k)/n1)
        if objective == 'equivalence':
            lo, hi = lower+norm.isf(alpha)*se, upper-norm.isf(alpha)*se
            if hi <= lo:
                return 0.
        else:
            lo = (-margin if objective == 'noninferiority' else margin)+norm.isf(alpha)*se
            hi = float('inf')
        # Standardise the integral to avoid wide/narrow density numerical problems.
        return quad(norm.pdf, (lo-difference)/se, (hi-difference)/se, epsabs=1e-12)[0]

    bound = 1.
    while power(bound, ratio) < .8:
        bound *= 2
    n10 = brentq(lambda n: power(n, ratio)-.8, 1e-8, bound, xtol=1e-9)
    n1, n2 = ceil(n10), ceil(ratio*n10)
    achieved = power(n1, n2/n1)
    rows.append(dict(type=kind, objective=objective, mean1=m1, mean2=m2, sd1=s1, sd2=s2,
                     p1=p1, p2=p2, ratio=ratio, alpha=alpha, margin=margin, lower=lower,
                     upper=upper, n10=n10, n1=n1, n2=n2, achieved=achieved))
    print(kind, objective, n10, n1, n2, achieved)

fixture = Path(__file__).resolve().parents[1]/'tests/fixtures/independent-objective-planning.csv'
with fixture.open('w', newline='') as stream:
    writer = csv.DictWriter(stream, fieldnames=rows[0])
    writer.writeheader()
    writer.writerows(rows)

print('Statsmodels one-sided reference:', normal_power_het(
    diff=5, nobs=100, alpha=.025, std_null=15*sqrt(1+1/2),
    std_alternative=15*sqrt(1+1/2), alternative='larger'))
