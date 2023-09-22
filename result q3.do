regress sleep totwrk educ age agesq yngkid male 
predict res, r 
gen ressq = res*res
regress ressq male 
