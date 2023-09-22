cd /Users/reihaneh/Desktop/loanapp2
import delimited loanapp2.raw, delimiters(tab)

/* 1.A summary statistics for variables in A16  */
global A16 approve white hrat obrat loanprc unem male married dep sch cosign chist pubrec mortlat1 mortlat2 vr
describe $A16
summarize $A16

/* 1.B. Finding variable with missing values, finding summary statistics for nomissing */
misstable summarize $A16
summarize $A16 if male != . & dep!=. & married!= .

/* 1.C Regress approve on white , restricting to nonmissing */
regress approve i.white if male != . & dep!=. & married!= .

outreg2 using table0, title(Table 1. Effect of Race on mortgage approval) word label replace

/* 1.D Add other variables regress on approve */
regress approve i.white c.hrat c.obrat c.loanprc c.unem i.male i.married c.dep i.sch i.cosign i.chist i.pubrec i.mortlat1 i.mortlat2 i.vr 

outreg2 using table1, title(Table 2. Determinants of mortgage approval) word label replace

/* 1.E Interact white on obrat */

regress approve i.white##c.obrat c.hrat c.loanprc c.unem i.male i.married c.dep i.sch i.cosign i.chist i.pubrec i.mortlat1 i.mortlat2 i.vr 

outreg2 using table3, title(Table 3. Determinants of mortgage approval with interaction term) word label replace

/* 1.F The effect of white when obrat is equal to 32.339? */
gen obrat2=obrat-32.339
regress approve i.white##c.obrat2 c.hrat c.loanprc c.unem i.male i.married c.dep i.sch i.cosign i.chist i.pubrec i.mortlat1 i.mortlat2 i.vr 
outreg2 using table4, title(Table 3. Determinants of mortgage approval with obrat=32.339) word label replace

/* 1.G Find the estimated probablities of approval, summary statistics */
predict mypred
summarize mypred

/////////////////////////////////////////////////
/* Question 2 */

cd /Users/reihaneh/Desktop/vote1
infile state district democA voteA expendA expendB prtystrA lexpendA lexpendB shareA using vote1.raw
summarize
/* 2.A */
regress voteA prtystrA democA lexpendA lexpendB
predict pred_voteA
predict residual_vote, residuals
summarize residual_vote

/* 2. B */
regress voteA prtystrA democA lexpendA lexpendB
regress voteA prtystrA democA lexpendA lexpendB, vce(robust)

/*2.C*/
hettest, rhs fstat

gen ressq=(residual_vote)^2
gen sqpred_voteA=pred_voteA^2
regress ressq pred_voteA sqpred_voteA

//////////////////////////////////////////
/*Question 3*/
regress sleep totwrk educ age agesq yngkid male 
predict res, r 
gen ressq = res*res
regress ressq male 
