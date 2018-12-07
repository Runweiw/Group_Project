* Stats 506 - Group Project, Stata Part
* Group : #7
* Author: Songkai Xue

*------------------------*
* 0. General information *
*------------------------*

version 15.0
clear


*-------------------------------------------------------------------*
* 1. Download Wine dataset from the UCI Machine Learning Repository *
*-------------------------------------------------------------------*

import delimited https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data


*------------------------------------------------------*
* 2. Save the dataset to the current working directory *
*------------------------------------------------------*

save Wine, replace


*--------------------------------------*
* 3. Pattern of the selected variables *
*--------------------------------------*

* Response: "Color_intensity"
rename v11 Color_intensity
kdensity Color_intensity

* Predictor: "Total_phenols"
rename v7 Total_phenols
kdensity Total_phenols

* Ground truth of class (cluster): "Class"
rename v1 Class
histogram Class, frequency


*-----------------------------------------*
* 4. Set initial values for EM iterations *
*-----------------------------------------*

* Show the default initial values, given by factor analysis
fmm 3, emopts(iterate(0)) noestimate: regress Color_intensity Total_phenols
matrix list e(b)

* If you want to give your own initial values, do something like this
matrix b = e(b)
matrix b[1,2] = 0
matrix b[1,3] = 0  // set the initial pi_1 = pi_2 = pi_3 = 1/3


*-----------------------------*
* 5. Results of model fitting *
*-----------------------------*

fmm 3, from(b) emopts(iterate(100)): regress Color_intensity Total_phenols
estat lcprob      // pi_hat
matrix list e(b)  // all estimated parameters
estat ic          // AIC


*---------------*
* 6. Clustering *
*---------------*

* Posterior Probabilities: pr1, pr2, pr3
predict pr*, classposteriorpr
format %4.3f pr*
list Class pr* in 1/10

* Assign Cluster number: Cluster
gen Cluster = 0
replace Cluster = 1 if pr1 > pr2 & pr1 > pr3  // (50 real changes made)
replace Cluster = 2 if pr2 > pr1 & pr2 > pr3  // (78 real changes made)
replace Cluster = 3 if pr3 > pr1 & pr3 > pr2  // (50 real changes made)


*-------------------*
* 7. Classification *
*-------------------*

* The aggregation table of medians of posterior probabilities, by Class
preserve
collapse (median)pr*, by(Class)
list
restore

* Assign Predicted Class number: Class_pred
recode Cluster (2 = 1) (3 = 2) (1 = 3), gen(Class_pred)


*-----------------------------------*
* 8. Accuracy of the classification *
*-----------------------------------*

* Error rate
gen True_pred = 0
replace True_pred = 1 if Class == Class_pred

preserve
collapse (sum)True_pred
local Accuracy = True_pred / 178
restore

display `Accuracy'  // Out: 0.764

* Confusion matrix
matrix C_mat = (0,0,0\0,0,0\0,0,0)

local i = 0

while `i' < _N {

	local ++i
	
	local b = Class[`i']
	local a = Class_pred[`i']
	
	matrix C_mat[`a', `b'] = C_mat[`a', `b'] + 1
}

matrix list C_mat

