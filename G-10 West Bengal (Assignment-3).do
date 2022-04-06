keep if STATEID == 19
gen food=CO1X+CO2X+CO3X+CO4X+CO5X+CO6X+CO7X+CO8X+CO9X+CO10X+CO11X+CO12X+CO13X+CO14X+CO15+CO16+CO17+CO18+CO19+CO20
gen NADULTM_n= NADULTM- NELDERM
gen NADULTF_n= NADULTF- NELDERF
tabstat SURVEY COTOTAL COPC INCOME INCOMEPC NPERSONS NADULTM_n NADULTF_n NCHILDM NCHILDF NTEENF NELDERM NELDERF NTEENM CO1X CO2X CO3X CO5X CO6X CO7X CO8X CO9X CO10X CO11X CO12X CO14X CO15 CO16 CO17 CO19 CO20, by(URBAN2011) stat(n mean sd min max) long col(stat)
mvencode CO1X CO2X CO3X CO5X CO6X CO7X CO8X CO9X CO10X CO11X CO12X CO14X CO15 CO16 CO17 CO18 CO19 CO20, mv(0) override
tabstat SURVEY COTOTAL COPC INCOME INCOMEPC NPERSONS NADULTM_n NADULTF_n NCHILDM NCHILDF NTEENF NELDERM NELDERF NTEENM CO1X CO2X CO3X CO5X CO6X CO7X CO8X CO9X CO10X CO11X CO12X CO14X CO15 CO16 CO17 CO19 CO20, by(URBAN2011) stat(n mean sd min max) long col(stat)
gen totalfood=food*12
gen foodshare=totalfood/COTOTAL
gen lnincomepc=ln(INCOMEPC) if INCOMEPC>0
gen lnfoodshare=ln(foodshare)
gen lnn=ln(NPERSONS)
gen nn=NADULTM_n+NADULTF_n+NCHILDM+NCHILDF+NTEENM+NTEENF+NELDERM+NELDERF
gen aa=0
replace aa=1 if nn==NPERSONS
tab aa
replace NCHILDF=NCHILDF+1 if aa==0
*this is for West bangal
drop nn
gen nn=NADULTM_n+NADULTF_n+NCHILDM+NCHILDF+NTEENM+NTEENF+NELDERM+NELDERF
drop aa
gen aa=0
replace aa=1 if nn==NPERSONS
tab aa
gen n1=NCHILDF/NPERSONS
gen n2=NCHILDM/NPERSONS
gen n3=NADULTM_n/NPERSONS
gen n4=NADULTF_n/NPERSONS
gen n5=NTEENM/NPERSONS
gen n6=NTEENF/NPERSONS
gen n7=NELDERM/NPERSONS
gen n8=NELDERF/NPERSONS
* Q 1(a) 
reg foodshare lnincomepc lnn n1 n2 n3 n4 n5 n6 n7 n8
predict e1, resid
gen e1sq = e1^2
predict foodsharef, xb
twoway scatter e1sq foodsharef
* Q 1(b)
reg foodshare INCOMEPC lnn n1 n2 n3 n4 n5 n6 n7 n8 if INCOMEPC>0
predict e2, resid
gen e2sq = e2^2
predict foodsharef2, xb
twoway scatter e2sq foodsharef2
* Q 1(c)
*Dummy Variable
tab NPERSONS
gen hhsz=NPERSONS if NPERSONS<=7
tab hhsz
replace hhsz=10 if hhsz==.
tab hhsz
tabulate hhsz, gen(hhszgr)
reg foodshare lnincomepc hhszgr1 hhszgr2 hhszgr3 hhszgr4 hhszgr5 hhszgr6 hhszgr7 n1 n2 n3 n4 n5 n6 n7 n8
predict e3, resid
gen e3sq = e3^2
predict foodsharef3, xb
twoway scatter e3sq foodsharef3
* Q 1(d)
*religious groups
tab ID11
replace ID11=9 if ID11>3
tabulate ID11, gen(relgr)
*Caste groups
tab ID13
replace ID13=6 if ID13==.
tabulate ID13, gen(casgr)
* regression 1(d)
reg foodshare lnincomepc lnn n1 n2 n3 n4 n5 n6 n7 n8 relgr1 relgr2 relgr3 casgr1 casgr2 casgr3 casgr5 casgr6
predict e4, resid
gen e4sq = e4^2
predict foodsharef4, xb
twoway scatter e4sq foodsharef4
*Q 2. Special case of white test
* 1(a)
gen foodsharefsq = foodsharef^2
reg e1sq foodsharef foodsharefsq 
asdoc reg e1sq foodsharef foodsharefsq 
* 1(b)
gen foodsharef2sq = foodsharef2^2
reg e2sq foodsharef2 foodsharef2sq
asdoc reg e2sq foodsharef2 foodsharef2sq
* 1(c)
gen foodsharef3sq = foodsharef3^2
reg e3sq foodsharef3 foodsharef3sq
asdoc reg e3sq foodsharef3 foodsharef3sq
*Q1(d)
gen foodsharef4sq = foodsharef4^2
reg e4sq foodsharef4 foodsharef4sq
asdoc reg e4sq foodsharef4 foodsharef4sq
* Q 3. F-test and LM-test
* F-test for joint significance of caste dummies in model 1(d)
reg foodshare lnincomepc lnn n1 n2 n3 n4 n5 n6 n7 n8 relgr1 relgr2 relgr3 casgr1 casgr2 casgr3 casgr5 casgr6
test casgr1 casgr2 casgr3 casgr5 casgr6
reg foodshare lnincomepc lnn n1 n2 n3 n4 n5 n6 n7 n8 relgr1 relgr2 relgr3 casgr1 casgr2 casgr3 casgr5 casgr6,robust
test casgr1 casgr2 casgr3 casgr5 casgr6
* LM test for the significance of caste dummies in model 1(d)
reg foodshare lnincomepc lnn n1 n2 n3 n4 n5 n6 n7 n8 relgr1 relgr2 relgr3
predict uhat,resid
reg uhat lnincomepc lnn n1 n2 n3 n4 n5 n6 n7 n8 relgr1 relgr2 relgr3 casgr1 casgr2 casgr3 casgr5 casgr6
scalar LM = e(r2)*e(N)
scalar pvalue = chi2tail(e(df_m),LM)
disp "LM = " LM ", p-value = " pvalue
* Heteroscedasticity robust LM statistic
 reg casgr1 lnincomepc lnn n1 n2 n3 n4 n5 n6 n7 n8 relgr1 relgr2 relgr3
predict uhat1,resid
 reg casgr2 lnincomepc lnn n1 n2 n3 n4 n5 n6 n7 n8 relgr1 relgr2 relgr3
predict uhat2,resid
 reg casgr3 lnincomepc lnn n1 n2 n3 n4 n5 n6 n7 n8 relgr1 relgr2 relgr3
predict uhat3,resid
 reg casgr5 lnincomepc lnn n1 n2 n3 n4 n5 n6 n7 n8 relgr1 relgr2 relgr3
predict uhat5,resid
 reg casgr6 lnincomepc lnn n1 n2 n3 n4 n5 n6 n7 n8 relgr1 relgr2 relgr3
predict uhat6,resid
gen p1 = uhat1*uhat 
gen p2 = uhat2*uhat 
gen p3 = uhat3*uhat 
gen p5 = uhat5*uhat 
gen p6 = uhat6*uhat 
gen one = 1
reg one p1 p2 p3 p5 p6,noconstant
*Q4 
reg foodshare lnincomepc lnn n1 n2 n3 n4 n5 n6 n7 n8 relgr1 relgr2 relgr3 casgr1 casgr2 casgr3 casgr5 casgr6
*****************************
