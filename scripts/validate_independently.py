# Optional developer verification: Python 3 with statsmodels==0.14.6.
# Run from the project root. This is not a runtime dependency of the Shiny app.
from statsmodels.stats.proportion import samplesize_proportions_2indep_onetail,power_proportions_2indep
from statsmodels.stats.power import normal_sample_size_one_tail,normal_power_het
from pathlib import Path
import math,csv,statsmodels
scenarios=[]
for p1,p2,power,alpha,k in [(.2,.3,.8,.05,1),(.2,.3,.8,.05,2),(.2,.3,.9,.01,.5),(.54,.44,.9,.05,1)]:
 n=samplesize_proportions_2indep_onetail(p1-p2,p2,power,ratio=k,alpha=alpha)
 c1,c2=math.ceil(n),math.ceil(k*n)
 actual=power_proportions_2indep(p1-p2,p2,c1,ratio=c2/c1,alpha=alpha,return_results=False)
 scenarios.append(dict(type='two_proportions',p1=p1,p2=p2,power=power,alpha=alpha,ratio=k,n10=float(n),n1=c1,n2=c2,achieved=float(actual)))
for m1,m2,s1,s2,power,alpha,k in [(100,105,15,15,.8,.05,1),(100,105,15,15,.8,.05,2),(20,25,10,20,.9,.01,2),(-5,0,12,8,.85,.05,.5)]:
 std=math.sqrt(s1*s1+s2*s2/k)
 n=normal_sample_size_one_tail(abs(m2-m1),power,alpha/2,std_null=std,std_alternative=std)
 c1,c2=math.ceil(n),math.ceil(k*n)
 stdactual=math.sqrt(s1*s1+s2*s2*c1/c2)
 actual=normal_power_het(abs(m2-m1),c1,alpha,std_null=stdactual,std_alternative=stdactual,alternative='two-sided')
 scenarios.append(dict(type='two_means',mean1=m1,mean2=m2,sd1=s1,sd2=s2,power=power,alpha=alpha,ratio=k,n10=float(n),n1=c1,n2=c2,achieved=float(actual)))
Path('tests/fixtures').mkdir(exist_ok=True)
with open('tests/fixtures/independent-normal-planning.csv','w') as f:
 writer=csv.DictWriter(f,fieldnames=['type','p1','p2','mean1','mean2','sd1','sd2','power','alpha','ratio','n10','n1','n2','achieved'])
 writer.writeheader();writer.writerows(scenarios)
print('statsmodels',statsmodels.__version__)
for s in scenarios:print(s)
