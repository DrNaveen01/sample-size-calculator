"""Independent verification fixtures using numerical integration and SciPy/statsmodels.
Python is a developer dependency only. Does not import or translate R functions.
"""
import csv
from pathlib import Path
import math
from scipy.integrate import quad
from scipy.stats import norm
from statsmodels.stats.contingency_tables import Table2x2
from statsmodels.stats.proportion import proportion_confint, confint_proportions_2indep
ROOT = Path(__file__).resolve().parents[1]
rows = []
def minimum(predicate, start=2):
    lo, hi = start-1, start
    while not predicate(hi):
        lo,hi=hi,hi*2
    while hi-lo>1:
        mid=(lo+hi)//2
        if predicate(mid): hi=mid
        else: lo=mid
    return hi
# Independently integrate the estimator density over rejection regions.
def power(effect, se0, se1, alpha, alternative='two.sided'):
    cutoff=norm.isf(alpha/(2 if alternative=='two.sided' else 1))*se0
    if alternative=='two.sided':
        return quad(norm.pdf,(cutoff-effect)/se1,math.inf)[0]+quad(norm.pdf,-math.inf,(-cutoff-effect)/se1)[0]
    return quad(norm.pdf,(cutoff-effect)/se1,math.inf)[0]
def av(a,m,n):
    q1=a/(2-a);q2=2*a*a/(1+a)
    return (a*(1-a)+(m-1)*(q1-a*a)+(n-1)*(q2-a*a))/(m*n)
for objective,a,a2,rho,k in [('estimate',.8,.5,0,1),('test',.8,.65,0,2),('independent',.8,.7,0,1.5),('paired',.8,.75,.5,2)]:
    def cs(m):
        n=max(2,math.ceil(k*m))
        return m,n
    def se(m,null=False):
        m,n=cs(m)
        a0=(a2 if objective=='test' else (a+a2)/2) if null else a
        v1=av(a0,m,n)
        if objective in ['estimate','test']: return math.sqrt(v1)
        v2=av(a0 if null else a2,m,n)
        return math.sqrt(v1+v2-2*rho*math.sqrt(v1*v2))
    m=minimum(lambda m: norm.isf(.025)*se(m)<=.05 if objective=='estimate' else power(a-a2,se(m,True),se(m),.05)>=.8)
    n=cs(m)[1]
    rows.append(dict(kind='auc',objective=objective,a=a,a2=a2,rho=rho,k=k,n1=m,n2=n,check=norm.isf(.025)*se(m) if objective=='estimate' else power(a-a2,se(m,True),se(m),.05)))
for p,d,method in [(.85,.05,'wilson'),(.9,.05,'wilson'),(.85,.05,'normal')]:
    n=minimum(lambda n: (proportion_confint(n*p,n,alpha=.05,method=method)[1]-proportion_confint(n*p,n,alpha=.05,method=method)[0])/2<=d)
    ci=proportion_confint(n*p,n,alpha=.05,method=method)
    rows.append(dict(kind='diagnostic',objective=method,a=p,a2=d,rho=0,k=1,n1=n,n2=0,check=(ci[1]-ci[0])/2))
p,p0=.85,.7
n=minimum(lambda n: power(p-p0,math.sqrt(p0*(1-p0)/n),math.sqrt(p*(1-p)/n),.05,'greater')>=.8)
rows.append(dict(kind='diagnostic',objective='test',a=p,a2=p0,rho=0,k=1,n1=n,n2=0,check=power(p-p0,math.sqrt(p0*(1-p0)/n),math.sqrt(p*(1-p)/n),.05,'greater')))
for obj,r,r0,k in [('test',.3,0,1),('test',.5,.2,1),('estimate',.3,0,1),('independent',.5,.2,2)]:
    zr=math.atanh(r)
    def ns(n): return n,max(4,math.ceil(k*n))
    def check(n):
        if obj=='estimate':
            h=norm.isf(.025)/math.sqrt(n-3)
            return (math.tanh(zr+h)-math.tanh(zr-h))/2
        n1,n2=ns(n)
        se=math.sqrt(1/(n1-3)+(1/(n2-3) if obj=='independent' else 0))
        return power(zr-math.atanh(r0),se,se,.05)
    n=minimum(lambda n: check(n)<=.1 if obj=='estimate' else check(n)>=.8,4)
    rows.append(dict(kind='correlation',objective=obj,a=r,a2=r0,rho=0,k=k,n1=n,n2=ns(n)[1] if obj=='independent' else 0,check=check(n)))
with (ROOT/'tests/fixtures/independent-advanced.csv').open('w') as f:
    w=csv.DictWriter(f,fieldnames=list(rows[0]),lineterminator="\n");w.writeheader();w.writerows(rows)
table=Table2x2([[20,80],[40,60]],shift_zeros=False)
rd=confint_proportions_2indep(20,100,40,100,method='newcomb',compare='diff')
effects=[('Odds ratio',table.oddsratio,*table.oddsratio_confint()),('Risk ratio',table.riskratio,*table.riskratio_confint()),('Risk difference',-.2,*rd)]
with (ROOT/'tests/fixtures/independent-effects.csv').open('w') as f:
    w=csv.writer(f,lineterminator="\n");w.writerow(['measure','estimate','lower','upper']);w.writerows(effects)
print(f'Wrote {len(rows)} planning and {len(effects)} effect fixtures. SciPy and statsmodels independent numerical checks.')

