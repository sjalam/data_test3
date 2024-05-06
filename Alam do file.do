********************************************************************************
*     Stata Excercise 	
* 	  ISI Delhi 
* 	  Written by: SK Jane Alam 
*     Date : 29-11-2023																
********************************************************************************

*stata version 14.1


clear all
set more off 
eststo clear 
matrix drop _all 



* Setting up the path 
global nss "C:\Users\HP\Desktop\Stata Pract\Tanu test\NSS DATA\data"


* Loading the HH level data
use "$nss/level01.dta" , clear 


*********************************Q-1********************************************


/*
Labelling States as there is no state code available only state region codes were 
available so, with state region variable I have tried to make a new variable 
called state but later after searching in google I get to could make state 
variabel by just looking infix to detache from state region variable.
*/
clonevar state_region_clone = state_region 
 

destring state_region_clone, replace 

gen state = 28 if state_region_clone >= 281 & state_region_clone <= 284
replace state = 12 if state_region_clone == 121
replace state = 18 if state_region_clone >= 181 & state_region_clone <= 183
replace state = 10 if state_region_clone >= 101 & state_region_clone <= 102
replace state = 30 if state_region_clone == 301
replace state = 24 if state_region_clone >= 241 & state_region_clone <= 245
replace state = 06 if state_region_clone >= 061 & state_region_clone <= 062
replace state = 02 if state_region_clone == 021
replace state = 01 if state_region_clone >= 011 & state_region_clone <= 013
replace state = 29 if state_region_clone >= 291 & state_region_clone <= 294
replace state = 32 if state_region_clone >= 321 & state_region_clone <= 322
replace state = 23 if state_region_clone >= 231 & state_region_clone <= 236
replace state = 27 if state_region_clone >= 271 & state_region_clone <= 276
replace state = 14 if state_region_clone >= 141 & state_region_clone <= 142
replace state = 17 if state_region_clone == 171
replace state = 15 if state_region_clone == 151
replace state = 13 if state_region_clone == 131
replace state = 21 if state_region_clone >= 211 & state_region_clone <= 213
replace state = 03 if state_region_clone >= 031 & state_region_clone <= 032
replace state = 08 if state_region_clone >= 081 & state_region_clone <= 084
replace state = 11 if state_region_clone == 111
replace state = 33 if state_region_clone >= 331 & state_region_clone <= 334
replace state = 16 if state_region_clone == 161
replace state = 05 if state_region_clone == 051
replace state = 09 if state_region_clone >= 091 & state_region_clone <= 094
replace state = 19 if state_region_clone >= 191 & state_region_clone <= 194
replace state = 35 if state_region_clone == 351
replace state = 04 if state_region_clone == 041
replace state = 26 if state_region_clone == 261
replace state = 25 if state_region_clone == 251
replace state = 07 if state_region_clone == 071
replace state = 31 if state_region_clone == 311
replace state = 34 if state_region_clone == 341
replace state = 22 if state_region_clone == 221
replace state = 20 if state_region_clone == 201

* Labeling each state code with their respective name.
label define statelab 28 "Andhra Pardesh" 12 "Arunachal Pradesh" 18 "Assam" 10 "Bihar" 30 "Goa" ///
					  24 "Gujrat" 06 "Haryana" 02 "Himachal" 01 "Jammy and Kashmir" 29 "Karnataka" ///
					  32 "Kerala" 23 "Madhyapradesh" 27 "Maharashtra" 14 "Manipur" 17 "Meghalaya" ///
					  15 "Mizoram" 13 "Nagaland" 21 "Orissa" 03 "Punjab" 08 "Rajasthan" 11 "Sikkim" ///
					  33 "Tamil Nadu" 16 "Tripura" 09 "Uttar Pradesh" 19 "West Bengal" 35 "Andaman and Nicobar Island" ///
					  04 "Chandigarh" 26 "Dadra and Nagar Haveli" 25 "Daman and Diu" 07 "Delhi" ///
					  31 "Lakshadweep" 34 "Pondicheri" 22 "Chhattisgarh" ///
					  20 "Jharkhand" 05 "Uttranchal", replace
					  
label values state statelab

tab state
drop state_region_clone


/*
Calculating the average monthly per capita consumption expenditure for households 
across the states 
of India?
*/

destring mpce_30_days hh_size , replace 
gen percapita_hh_cons = (mpce_30_days/hh_size)

destring multiplier nss nsc, replace

gen weight = .
replace weight = multiplier / 100 if nss == nsc
replace weight = multiplier / 200 if nss == nsc
replace weight = multiplier / 200 if nss != nsc

* Merging with Individual level information on block 3 

merge 1:m common_id using "$nss/level03.dta" , nogen keep(3)

/*
 Result                           # of obs.
    -----------------------------------------
    not matched                             0
    matched                           602,833  (_merge==3)
    -----------------------------------------
*/

sort common_id

preserve 

/* 
creating state level estimates of per capita income
preserve 
*/

collapse(mean) percapita_hh_cons[aw = weight], by(state)

keep state percapita_hh_cons

keep if _n < 36

estpost tabstat state percapita_hh_cons, by(state)

esttab, cells("state percapita_hh_cons") noobs nomtitle nonumber varlabels(`e(labels)')drop(Total) varwidth(30) collab(, lhs("`:var lab state'")) tex

sort percapita_hh_cons 

/*

Mizoram, Chandigarh, Delhi and Andaman & Nicobar Island are the top four 
per capita household consumption states which are union territories.
Being a union territory might be an advantage for them with more policies by the 
central government.
Whereas Gujrat, Jammu & Kashmr, Maharashtra and Himachal are the leat four 
per capita household consumption states.

*/

restore 



**********************************Q-2*******************************************



* To find decile 
xtile decile = percapita_hh_cons, nq(10)

* For each decile from 10 to 100% of 10% gap
forvalues i = 1/10 {
	sum percapita_hh_cons if decile == `i', detail
}


cap drop _merge

preserve
collapse (mean) percapita_hh_cons [aw=weight] , by(decile) 
estpost tabstat  percapita_hh_cons, by(decile)

esttab, cells("mean") noobs nomtitle nonumber varlabels(`e(labels)')  varwidth(30) collab(, lhs("`:var lab decile'")) tex


/*
Summary statistics: mean
     for variables: percapita_hh_cons
  by categories of: decile

      decile |   e(mean) 
-------------+-----------
           1 |  271.5708 
           2 |  360.3074 
           3 |  420.6985 
           4 |  478.5819 
           5 |  539.7689 
           6 |  610.1359 
           7 |  701.3643 
           8 |  837.6784 
           9 |  1088.603 
          10 |   2251.09 
-------------+-----------
       Total |  755.9799 

*/
********************************Q-3*********************************************


restore
* Merging the individual dtata
merge 1:1 common_id person_srl_no using "$nss/level04.dta"

/*
    Result                           # of obs.
    -----------------------------------------
    not matched                             0
    matched                           602,833  (_merge==3)
    -----------------------------------------
*/

* required variables 
destring age sex pri_activity_status social_group, replace 

* Labeling 1 as male and 2 as female as mentioned in NSS data
label define  sex  1 "Male" 2 "Female"
label values sex sex

* Putting age restriction 
keep if age >= 15 & age <= 59

* tabulating 
tab sex if pri_activity_status >= 11 & pri_activity_status <= 51
* Male with working group are 73.7% and female are 26.3%
preserve


keep if pri_activity_status <= 51
ttest pri_activity_status, by(sex)

/*
Two-sample t test with equal variances
------------------------------------------------------------------------------
   Group |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
---------+--------------------------------------------------------------------
    Male | 149,397    25.35953    .0392126    15.15643    25.28267    25.43638
  Female |  53,686    27.55651    .0617796    14.31448    27.43543     27.6776
---------+--------------------------------------------------------------------
combined | 203,083    25.94031    .0332185    14.96982     25.8752    26.00542
---------+--------------------------------------------------------------------
    diff |           -2.196989    .0751695               -2.344319   -2.049658
------------------------------------------------------------------------------
    diff = mean(Male) - mean(Female)                              t = -29.2271
Ho: diff = 0                                     degrees of freedom =   203081

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.0000         Pr(|T| > |t|) = 0.0000          Pr(T > t) = 1.0000
*/


**********************************Q-4*******************************************


* genrating employment indicator 
restore
gen employed = 0 
replace employed = 1 if pri_activity_status >= 11 & pri_activity_status <= 51

* Employemnt rate by gender
bys sex : sum employed [aw=weight] 

preserve 
	
	collapse (mean) employed [aw=weight] , by(decile sex) 
	
* Generating graph for e employment rate by principalstatusfor 
* females(aged 15-59) differ across expenditure
* deciles
	
	twoway (connected employed decile if sex ==1, lcolor(dknavy) lwidth(medthick) mcolor(dknavy) msymbol(T)) /// male
       (connected employed decile if sex ==2,  lcolor(cranberry) lwidth(medthick) mcolor(cranberry) msymbol(O)), /// female
       xtitle("Deciles")  ///
	   xlab(1(1)10) ///
	   ytitle("Mean employed") ///
	   legend(order(1 "Male" 2 "Female") c(2) position(6)) ///
	   graphr(c(white)) ///
	   note("Sample is restricted for age 15 - 59 years; Source: NSS EUS 2004.")
	   
/*

The graph shows as the employment rate decreases as the women's wealth
increases which I understand rich women does not work often and vice versa
and the gap between male and female also increases as we move along the decile.

*/

	   graph export "$nss/excercise_4 graph.pdf", as(pdf) replace

restore 


*********************************Q-5********************************************


* Graph of principal employement rate by caste and gender 

label define social_group 1 "ST" 2 "SC" 3 "OBC" 9 "Others"
label values social_group social_group

* Normal Stata version
graph bar (mean) employed [aw=weight] , over(sex) over(social_group) /// 
				 asyvars graphr(c(white))
* Colourful one
graph bar (mean) employed [aw=weight], over(sex) over(social_group) ///
   asyvars graphr(c(white)) bar(1, fcolor(blue) lcolor(blue)) bar(2, fcolor(red) lcolor(red)) scheme(vg_s2c)


graph export "$nss/excercise_5 graph.pdf", as(pdf) replace
