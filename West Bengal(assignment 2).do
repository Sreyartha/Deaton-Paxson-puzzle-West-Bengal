* First remove all data not related to West Bengal
keep if STATEID == 19
*We generate two categories for adult females and adult males(age group:21-60)
gen NADULTM_n=NADULTM-NELDERM
gen NADULTF_n=NADULTF-NELDERF
* Check the summary statistics
tabstat SURVEY COTOTAL COPC INCOME INCOMEPC NPERSONS NADULTM_n NADULTF_n /*
*/ NCHILDM NCHILDF NTEENF NELDERM NELDERF NTEENM CO1X CO2X CO3X CO5X /*
*/ CO6X CO7X CO8X CO9X CO10X CO11X CO12X CO13X CO14X CO15 CO16 CO17 CO19 CO20 CO21, by(URBAN2011) /* 
*/ stat(n mean sd min max) long col(stat)
* Replacing the missing values with 0.
mvencode CO1X CO2X CO3X CO5X CO6X CO7X CO8X CO9X CO10X CO11X CO12X CO13X CO14X CO15/*
*/ CO16 CO17 CO19 CO20, mv(0) override
*Checking the summary statistics again
tabstat SURVEY COTOTAL COPC INCOME INCOMEPC NPERSONS NADULTM_n NADULTF_n /*
*/ NCHILDM NCHILDF NTEENF NELDERM NELDERF NTEENM CO1X CO2X CO3X CO5X /*
*/ CO6X CO7X CO8X CO9X CO10X CO11X CO12X CO13X CO14X CO15 CO16 CO17 CO19 CO20 CO21, by(URBAN2011) /* 
*/ stat(n mean sd min max) long col(stat)
* Dealing with negative values and outliers in income items.
* histogram INCOME and kernel density
histogram INCOME
kdensity INCOME
*summarise non-positive values of INCOME
summ INCOME if INCOME<=0
*generating log variables for consumption and income
gen lnmpce= ln(COPC) if COPC~=.
gen lnapci=ln(INCOMEPC) if (INCOMEPC ~=. & INCOMEPC>0)
*inspecting the density functions again after log transformations
kdensity lnmpce
kdensity lnapci
*inspecting association between lnmpce and lnapci
twoway scatter lnmpce lnapci
* generate food expenditure 
gen food=CO1X+CO2X+CO3X+CO4X+CO5X+CO6X+CO7X+CO8X+CO9X+CO10X+CO11X+CO12X+CO13X+CO14X+CO15+CO16+CO17+CO18+CO19+CO20
gen totalfood = food*12
* generate food share
gen foodshare = totalfood/COTOTAL
* scatter plot between foodshare and log(annual income per capita)
twoway scatter foodshare lnapci
* generate log of household size
gen lnhhsz = ln(NPERSONS)
*The summary statistics again but with lnmpce~=. & lnapci~=. 
*For All India
 tabstat SURVEY COTOTAL COPC lnmpce INCOME INCOMEPC lnapci totalfood foodshare/*
*/ CO1X CO2X CO3X CO5X CO6X CO7X CO8X CO9X CO10X CO11X CO12X CO13X CO14X /*
*/ CO15 CO16 CO17 CO18 CO19 CO20 /* 
*/ NPERSONS lnhhsz NADULTM_n NADULTF_n NCHILDM NCHILDF NTEENF NELDERM NELDERF NTEENM /* 
*/ if lnmpce~=. & lnapci~=. & foodshare~=0, stat(N mean sd min max) 
* This command generates the statistics with sample size-n uniform across all variables
****************************************************************************
* Question 1(a)
reg foodshare lnapci lnhhsz if lnmpce~=. & lnapci~=.
predict e1_23, resid
rvfplot, yline(0)
reg foodshare lnapci
predict e1_2, resid
rvfplot, yline(0)
reg lnhhsz lnapci
predict e3_2, resid
rvfplot, yline(0)
* regress e1_2 on e3_2 to check the slope coefficient
reg e1_2  e3_2
* Question 1(b) - Residual Analysis 
* rvf plot- I have used the same code like in 1(a) where I have also plotted the rvf plots
* Testing if the mean of residuals is 0
ttest e1_23 == 0, level(99)
ttest e3_2 == 0, level(99)
ttest e1_2 == 0, level(99)
*Testing normality of residual terms 
sktest e1_23
sktest e1_2
sktest e3_2
*Question 2
* Checking if nn = NPERSONS
gen nn=NADULTM_n+NADULTF_n+NCHILDM+NCHILDF+NTEENM+NTEENF+NELDERM+NELDERF
gen aa=0
replace aa=1 if nn==NPERSONS
tab aa
replace NCHILDF=NCHILDF+1 if aa==0
drop nn
gen nn=NADULTM_n+NADULTF_n+NCHILDM+NCHILDF+NTEENM+NTEENF+NELDERM+NELDERF
drop aa
gen aa=0
replace aa=1 if nn==NPERSONS
tab aa
* Generating the proportion variables
gen n1=NCHILDF/NPERSONS
gen n2=NCHILDM/NPERSONS
gen n3=NADULTM/NPERSONS
gen n4=NADULTF/NPERSONS
gen n5=NTEENM/NPERSONS
gen n6=NTEENF/NPERSONS
gen n7=NELDERM/NPERSONS
gen n8=NELDERF/NPERSONS
*Multiple regression 
reg foodshare lnapci lnhhsz n1 n2 n3 n4 n5 n6 n7 n8
*Dummy Variable
tab NPERSONS
gen hhsz=NPERSONS if NPERSONS<=7
tab hhsz
replace hhsz=10 if hhsz==.
tab hhsz
tabulate hhsz, gen(hhszgr)
* Chow Test
* Step- 1: Estimate regression assuming no paramter instability and obtain RSS1(Restricted Residual Sum of squares)
reg foodshare lnapci lnhhsz n2 n3 n4 n5 n6 n7 n8
scalar rss1=e(rss)
* Step -2 : Estimate regression for rural area
reg foodshare lnapci lnhhsz n2 n3 n4 n5 n6 n7 n8 if URBAN2011==0
scalar rss2=e(rss)
scalar n_1=e(N)
* Step - 3 : Estimate regression for urban areas
reg foodshare lnapci lnhhsz n2 n3 n4 n5 n6 n7 n8 if URBAN2011==1
scalar rss3=e(rss)
scalar n_2=e(N)
scalar k=10 
* Step -4 : Find the value of the F- statistic = (((rss1 - rss2 -rss3 )/(k))/(((rss2+rss3)/(n_1+n_2-2*k)))
scalar chowtest=(((rss1 - rss2 -rss3 )/(k))/(((rss2+rss3)/(n_1+n_2-2*k))))
display chowtest
*Step - 5 : Find F-critical value and decide whether to reject or accept null at the chosen level of significance
di invFtail(10,2408)





