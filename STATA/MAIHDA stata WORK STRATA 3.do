* MAIHDA Stata code
* Diana Juanita Mora
* March 8th 2026

**# Setup

* Set command interpreter to version 18
version 18.0

* Change the working directory
cd "C:\Users\juani\OneDrive\Documents\UAH\PhD Documents\INEdatos\Analysis\STATA"

* Load data
use dt_stata3.dta, clear

* Set output format of coefficients, SEs, and confidence limits to 2dp
set cformat %9.2f

* Log analysis and save log
capture log close
log using "C:\Users\juani\OneDrive\Documents\UAH\PhD Documents\INEdatos\Analysis\STATA/TutorialLog" , replace

* Describe data
describe
summarize

**# Model 0
* Fit the two-level logistic regression with no covariates
melogit sedentarismo2 || stratum:, or baselevels

* Save the model results
estimates save "m0.ster", replace

* Store the between-stratum variance
scalar m0sigma2 = _b[/var(_cons[stratum])]

* Predict the fitted linear predictor
predict m0_total, eta

* Predict the linear predictor for the fixed portion of the model only
predict m0_fixed, xb

**# Model 1
* Fit the two-level logistic regression with covariates
melogit sedentarismo2 i.sexo i.edad_cat i.clase_2 i.NUTS1 i.survey2 || stratum:, ///
	or baselevels

* Save the model results
estimates save "m1.ster", replace

* Store the between-stratum variance
scalar m1sigma2 = _b[/var(_cons[stratum])]

* Predict the fitted linear predictor
predict m1_total, eta

* Predict the linear predictor for the fixed portion of the model only
predict m1_fixed, xb

* Predict the standard error of the fixed-portion linear prediction
predict m1_fixed_se, stdp

* Predict the stratum random effect and its standard error
predict m1_random, reffect reses(m1_random_se)

**# Save data at individual-level

* Compress the data
compress

* Save the data
save "individual.dta", replace

**# Save data at stratum-level

* Collapse the data down to a stratum-level dataset
collapse (count) n = sedentarismo2 (mean) sedentarismo2, ///
	by(stratum sexo edad_cat clase_2 NUTS1 survey2 ///
	 m0_total m0_fixed m1_total m1_fixed m1_fixed_se m1_random_se m1_random)

* Move the list of variables to the beginning of the dataset
order stratum sexo edad_cat clase_2 NUTS1 survey2 n sedentarismo2

* Convert the outcome from a proportion to a percentage
replace sedentarismo2 = 100*sedentarismo2

* Set the display format to 1dp
format %9.1f sedentarismo2

* Set the display format to 2dp
format %9.2f sedentarismo2 ///
	m0_total m0_fixed m1_total m1_fixed m1_fixed_se m1_random_se m1_random

* Compress the data
compress

* Save the data
save "stratum.dta", replace

**# Individual-level descriptive statistics

* Load the individual-level data
use "individual.dta", clear

* Total sample size
codebook stratum, compact

* Tabulate each individual characteristic
tabulate sexo 
tabulate edad_cat
tabulate clase_2
tabulate NUTS1
tabulate survey2

* Summarize the individual outcome
tabstat sedentarismo2, statistics(n mean sd min max) format(%3.1f)
ci proportions sedentarismo2
//[mean of the individual outcomes = "sample mean"]

**# Stratum-level descriptive statistics
* Load the stratum-level data
use "stratum.dta", clear

* Generate binary indicators for whether each stratum has more than X 
* individuals
generate n100plus = (n >= 100)
generate n50plus = (n >= 50)
generate n30plus = (n >= 30)
generate n20plus = (n >= 20)
generate n10plus = (n >= 10)
generate nlessthan10 = (n < 10)

* Tabulate the binary indicators
tabulate n100plus
tabulate n50plus
tabulate n30plus
tabulate n20plus
tabulate n10plus
tabulate nlessthan10

**# Table 3
* Load the individual-level data
use "individual.dta", clear

* Model 0 

* Load the estimation results and make them the current (active) results
estimates use "m0.ster"

* Replay the estimation results
estimates replay, or baselevels

* Calculate the variance partition coefficient (VPC)
estat icc

* Calculate the area under the receiver operating characteristic (ROC) curve
* based on fitted linear predictor for the fixed portion of the model only 
* (only the intercept)
roctab sedentarismo2 m0_fixed

* Calculate the area under the receiver operating characteristic (ROC) curve
* based on fitted linear predictor (intercept and stratum random effect)
roctab sedentarismo2 m0_total

* Model 1 
* Load the estimation results and make them the current (active) results
estimates use "m1.ster"

* Replay the estimation results
estimates replay, or baselevels

* Calculate the variance partition coefficient (VPC)
estat icc

* Calculate the Proportional Change in Variance (PCV) (as a percentage)
display	%3.1f 100*(m0sigma2 - m1sigma2) / m0sigma2

* Calculate the area under the receiver operating characteristic (ROC) curve
* based on linear predictor for the fixed portion of the model only (main 
* effects only)
roctab sedentarismo2 m1_fixed

* Calculate the area under the receiver operating characteristic (ROC) curve
* based on fitted linear predictor (main effects and interactions)
roctab sedentarismo2 m1_total

**# Figure 2 - Predicted Probability
* Load the stratum-level data
use "stratum.dta", clear

* Generate the predicted stratum percentages
generate m1_prob_total = 100 * invlogit(m1_total)

* Rank the predicted stratum percentages
egen m1_prob_total_rank = rank(m1_prob_total)

* Generate the lower and upper limits of the approximate 95% confidence 
* intervals for the predicted stratum percentages
generate m1_total_lo = 100 * invlogit(m1_fixed + m1_random ///
	- 1.96 * sqrt(m1_fixed_se^2 + m1_random_se^2))
generate m1_total_hi = 100 * invlogit(m1_fixed + m1_random ///
	+ 1.96 * sqrt(m1_fixed_se^2 + m1_random_se^2))
// Approximate as the model assumes no sampling covariability between the 
// regression coefficients and the stratum random effect

* Plot the caterpillar plot of the predicted stratum means

* label height 
generate lab_y = 48 

twoway ///
    (rspike m1_total_hi m1_total_lo m1_prob_total_rank, lcolor(gs4)) ///
    (scatter m1_prob_total m1_prob_total_rank, mcolor(black) msymbol(smcircle)) ///
	(scatter lab_y m1_prob_total_rank, msymbol(none) ///
	mlabel(stratum) mlabsize(*1) mlabcolor(black) ///
    mlabangle(90) mlabposition(12) mlabgap(*1)) ///)
    , ///
    ytitle("Predicted Percent Sedentarism" " " "Model 2", size(*1.5)) ///
    ylabel(0(10)50, angle(horizontal) labsize(*1.5)) ///
    yline(10 20 30 40 50, lwidth(vthin) lcolor(gray)) ///
    xtitle("Stratum rank", size(*1.5)) ///
    xlabel(0(20)112, labsize(*1.5)) ///
    legend(off) ///
    scheme(s1mono) ///
    name(Figure2B, replace) ///
    xsize(10)
	
* Export the graph as a portable network graphics (PNG) file
graph export "Figure2_predictedprobability_sedentarism.png", replace width(1000)

* Generate list of 5 highest/lowest predicted stratum percentages (for Table 4)
sort m1_prob_total_rank
list stratum sexo edad_cat clase_2 NUTS1 survey2 n m1_prob_total_rank m1_prob_total m1_total_lo m1_total_hi in f/5
list stratum  sexo edad_cat clase_2 NUTS1 survey2 n m1_prob_total_rank m1_prob_total m1_total_lo m1_total_hi ///
	in -5/-1
	
save "strata3_predicted_probabilities.dta", replace

**# Figure 3 - Residual Effects
* Load the stratum-level data
use "stratum.dta", clear

* Replaces each observation in the dataset with 1000 copies of the observation
expand 1000
// We do this as for this plot we follow a simulation-based approach to 
// calculating the limits of the approximate 95% confidence intervals of our 
// predictions. This involves simulating 1000 values for each predicted value.
//
// Approximate as the model assumes no sampling covariability between the 
// regression coefficients and the stratum random effect

* Generate the approximate standard error for the linear predictor
generate m1_total_se = sqrt(m1_fixed_se^2 + m1_random_se^2)

* Specify initial value of random-number seed
set seed 354612

* Generate the predicted stratum percentages based on the regression 
* coefficients and the predicted stratum random effect and factoring in 
* prediction uncertainty
generate m1_prob_total_sim = 100 * invlogit(m1_fixed + m1_random + rnormal(0, m1_total_se))

* Generate the predicted stratum percentages ignoring the predicted stratum 
* effect
generate m1_prob_fixed_sim = 100 * invlogit(m1_fixed)

* Generate the difference in the predicted stratum percentages due to the 
* predicted stratum effect
generate m1_diff = m1_prob_total_sim - m1_prob_fixed_sim

* Generate the lower and upper limits of the approximate 95% confidence 
* intervals for the difference in predicted stratum percentages due to 
* interaction
bysort stratum: egen m1_diff_lo = pctile(m1_diff), p(2.5)
bysort stratum: egen m1_diff_hi = pctile(m1_diff), p(97.5)

* ── SAVE original data with stratum-level vars BEFORE collapsing ──
preserve
keep stratum sexo edad_cat clase_2 NUTS1 survey2
save "stratum_lookup.dta", replace
restore

* Convert the data into a stratum-level dataset
collapse (mean) m1_diff m1_diff_lo m1_diff_hi ///
         (first) sexo edad_cat clase_2 NUTS1 survey2, ///
         by(stratum)

* Rank the predicted stratum percentage differences
egen m1_diff_rank = rank(m1_diff)

* Save stratum-level results
save "strata3.dta", replace

* Re-plot the caterpillar of the predicted stratum percentage differences
generate lab_y = 14

twoway ///
	(rspike m1_diff_hi m1_diff_lo m1_diff_rank, lcolor(gs4)) ///
	(scatter m1_diff m1_diff_rank, mcolor(black) msymbol(smcircle)) ///
	(scatter lab_y m1_diff_rank, msymbol(none) ///
	mlabel(stratum) mlabsize(*1) mlabcolor(black) ///
    mlabangle(90) mlabposition(12) mlabgap(*1)) ///)
	, ///
	ytitle("Difference in Predicted Percent" ///
		" " "Sedentarism due to Interactions", size(*1.5)) ///
	yline(0) ///
	yline(-10 -5 5 10 15, lcolor(gray)) ///
	ylabel(-10(5)15, angle(horizontal) labsize(*1.5)) ///
	xtitle("Stratum rank", size(*1.5)) ///
	xlabel(0(12)112, labsize(*1.5)) ///
	legend(off) ///
	scheme(s1mono) ///
	name(Figure3C, replace) ///
	xsize(10)

* Export the graph as a portable network graphics (PNG) file
graph export "Figure 3_residualeffects.png", replace width(1000)

* ── RESTORE original data and merge in the new stratum-level vars ──
restore

* Merge in the stratum-level results
merge 1:1 stratum using "stratum_lookup.dta"
drop _merge

* Generate list of 5 highest/lowest residual effects strata

sort m1_diff_rank
list stratum sexo edad_cat clase_2 NUTS1 survey2 n m1_diff_rank m1_diff m1_diff_lo m1_diff_hi in f/5
list stratum sexo edad_cat clase_2 NUTS1 survey2 n m1_diff_rank m1_diff m1_diff_lo m1_diff_hi ///
	in -5/-1
	
**# Close log file
capture log close

translate "C:\Users\juani\OneDrive\Documents\UAH\PhD Documents\INEdatos\Analysis/TutorialLog.smcl" "C:\Users\juani\OneDrive\Documents\UAH\PhD Documents\INEdatos\Analysis/TutorialLog.pdf", replace
// Log will be converted and saved as a PDF.







