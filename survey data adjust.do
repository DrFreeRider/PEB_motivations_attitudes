******************************************************
** Paper: PEB Motivations and Social Norms
**  Survey Data
** Author: Jose David Lopez-Rivas  
******************************************************

clear all
cd "~/Documents/Research/PEB Drivers"
import excel "survey_data_adjusted.xlsx", sheet("resultados") firstrow
describe 

************************************
* Household identificator 
gen hh=_n 
label var hh "household"
drop id

************************************
* Village Identificator
egen villageid = group(village), label
drop village
rename villageid village
label define villages 1 "Mo 75" 2 "Bi 50" 3 "Bi 75" 4 "Bi 0" 5 "Mo 0" 6 "Bi 25" 7 "Mo 50" 8 "Mo 25"
label values village villages
label values village "Village"

************************************
* Housing Variables
* Inhabitants
label var inhabitants "Inhabitants (\#)"

* Type of housing
label var tipo_viv "Type of housing"
tabulate tipo_viv, generate(type)
label var type1 "Apartment"
label var type2 "House"
label var type3 "Other"

label var prop_viv "Housing"
tabulate prop_viv, generate(property)
label var property1 "Owner"
label var property2 "Tenant"
label var property3 "Family"
label var property4 "Other"

* Area
label var area_viv "Area of housing (m2)"
replace area_viv=. if area_viv==0

* Bathrooms
label var bath_viv "Bathrooms (\#)"

* Economic activity in house
label var econ_viv "Economic activity"

************************************
*** Head of the Household HHH

label var chief "HHH was interviewed"
label var rol "Role in family"
label var age "HHH Age (years)"

* Sex
tabulate sex, gen(sexhhh)
label var sexhhh1 "Female"
label var sexhhh2 "Male"

* Education
label var educ "HHH Level of education"
tabulate educ, gen(educ_levelhhh)
label var educ_levelhhh1 "Elementary or less"
label var educ_levelhhh2 "High school"
label var educ_levelhhh3 "Tertiary or College"


************************************
* Motivations or Attitudes
************************************
** Actions (PEBs)
label var ac1 "Energy saving (ES)"
rename ac1 beh_ES
label var ac2 "Waste recycling (WR)"
rename ac2 beh_WR
label var ac3 "Water saving (WS)"
rename ac3 beh_WS

** PCB (Perceived Control of Behavior)
label var cpc1 "PCB ES"
rename cpc1 pcb_ES
label var cpc2 "PCB WR"
rename cpc2 pcb_WR
label var cpc3 "PCB WS"
rename cpc3 pcb_WS

** Social Norms closest people
label var ns11 "SNC ES"
rename ns11 snc_ES
label var ns12 "SNC  WR"
rename ns12 snc_WR
label var ns13 "SNC  WS"
rename ns13 snc_WS

** Social Norms neighbors
label var ns21 "SNN ES"
rename ns21 snn_ES
label var ns22 "SNN WR"
rename ns22 snn_WR
label var ns23 "SNN WS"
rename ns23 snn_WS

** Anticipated Guilt
label var cul1 "AG ES"
rename cul1 gui_ES
label var cul2 "AG WR"
rename cul2 gui_WR
label var cul3 "AG WS"
rename cul3 gui_WS

** Anticipated Embarrassment
label var ver1 "AE ES"
rename ver1 emb_ES
label var ver2 "AE WR"
rename ver2 emb_WR
label var ver3 "AE WS"
rename ver3 emb_WS

** Anticipated Pride
label var org1 "AP ES"
rename org1 pri_ES
label var org2 "AP WR"
rename org2 pri_WR
label var org3 "AP WS"
rename org3 pri_WS

** Extrinsic Incentive: Monetary payment
label var mon1 "MP ES"
rename mon1 mon_ES
label var mon2 "MP WR"
rename mon2 mon_WR
label var mon3 "MP WS"
rename mon3 mon_WS

**  Intention of behavior
label var int1 "IB ES"
rename int1 int_ES
label var int2 "IB WR"
rename int2 int_WR
label var int3 "IB WS"
rename int3 int_WS

************************************
** How they see the others
************************************

** Others are trustworthy
replace  turst_other =. if  turst_other==99
rename turst_other trust_others
label var trust_others "Others are trustworthy"

** Others are helpful
replace  help_others =. if  help_others==99
label var help_others "Others are helpful"

** Others are fair
replace  justice_others =. if  justice_others==99
rename justice_others fair_others
label var fair_others "Others are fair"

************************************
** Trust in institutions
************************************

replace  trust_osp =. if  trust_osp==99
label var trust_osp "Trust: water utility"

replace  trust_gob =. if  trust_gob==99
label var trust_gob "Trust: local government"

replace  trust_energy =. if  trust_energy==99
label var trust_energy "Trust: energy utility"

replace  trust_envi =. if  trust_envi==99
label var trust_envi "Trust: environmental agency"

* Discrete variables of Trust
gen h_trust_water =0
replace h_trust_water=1 if trust_osp<=2
label var h_trust_water "Trust: water utility"

gen h_trust_gov =0
replace h_trust_gov=1 if trust_gob<=2
label var h_trust_gov "Trust: local government"

gen h_trust_energy =0
replace h_trust_energy=1 if trust_energy<=2
label var h_trust_energy "Trust: energy utility"

gen h_trust_envi =0
replace h_trust_envi=1 if trust_envi<=2
label var h_trust_envi "Trust: environmental agency"


************************************
** Reciprocity Variables
************************************

replace  inter_1 =. if  inter_1==99
label var inter_1 "My consumption affects others"

replace  inter_2 =. if  inter_2==99
label var inter_2  "Others' consumption affects me"

************************************
* Reliable Neighbors
************************************
label var trust_neigh "Reliable neighbors (\#)"
sum trust_neigh, detail
replace trust_neigh=. if trust_neigh>=`r(p99)'

************************************
* Recalling messages 
************************************
label var image_1 "Approval"
label var image_2 "Disapproval"

************************************
* Where heard about the messages?
************************************
label var net_neigh "From neighbors"
label var net_family "From relatives" 
label var net_public "At public spaces"

************************************
* Socioceconomic stratification
************************************
rename strata ses
label var ses "Socioeconomic Strata"

************************************
* Treatment Status
************************************
rename treated d_treated
label var d_treated "Directly"
label define d_treated 0 "Not Directly" 1 "Direclty"
label values d_treated d_treated

rename spill i_treated
label var i_treated "Indirectly"
label define i_treated 0 "Not Indirectly" 1 "Indirectly"
label values i_treated i_treated 

label var control "Control"
label var status "Treatment status"

label define status 1 "Control" 2 "Indirectly" 3 "Directly"
label values status status


*************************************************************
* Summary statistics 
*************************************************************

global motiv beh_ES beh_WR beh_WS pcb_ES pcb_WS pcb_WR gui_ES gui_WS gui_WR  int_ES int_WR int_WS mon_ES mon_WR mon_WS snc_ES snc_WR snc_WS snn_WS snn_ES snn_WR  emb_WR emb_WS emb_ES pri_ES pri_WS pri_WR inter_1 inter_2 trust_others fair_others help_others trust_neigh  h_trust_water h_trust_gov h_trust_energy h_trust_envi   

** Summary stats characteristics of households and hhh
eststo clear
estpost sum inhabitants type1 type2 type3 property1 property2 property3 property4 area_viv bath_viv econ_viv chief age sexhhh1 educ_levelhhh1 educ_levelhhh2 educ_levelhhh3 

esttab using "/Users/mrfreerider/Documents/Research/PEB Drivers/paper drivers/summary_survey_hh.tex", label replace noobs nonumber booktabs compress nogaps cells("mean(label(Mean) fmt(a2)) sd(label(Std. Dev.)) min(label(Min.)) max(label(Max.)) count(label(Obs.))") refcat(inhabitants "\textbf{\emph{Housing}}" chief "\textbf{\emph{Head of Household}}", nolabel) 

** Summary stats peb-mot-att
eststo clear
estpost sum $motiv 
esttab using "/Users/mrfreerider/Documents/Research/PEB Drivers/paper drivers/summary_survey_peb.tex", label replace noobs nonumber booktabs compress nogap cells("mean(label(Mean) fmt(a2)) sd(label(Std. Dev.)) min(label(Min.)) max(label(Max.)) count(label(Obs.))") refcat( pcb_ES "\textbf{\emph{Intrinsic Drivers}}"  mon_ES "\textbf{\emph{Extrinsic Drivers}}" snc_ES "\textbf{\emph{Social influence}" inter_1 "\textbf{\emph{Reciprocity}}"  trust_others "\textbf{\emph{Trust}}", nolabel) 

*************************************************************
* Differences of means by treatment groups
*************************************************************

gen d_sample=0 // directly versus control
replace d_sample=1 if i_treated != 1

gen i_sample=0 // indirectly versus control
replace i_sample=1 if d_treated != 1

** Households and HHH characteristics
eststo clear
estpost ttest inhabitants type1 type2 type3 property1 property2 property3 property4 area_viv bath_viv econ_viv chief age sexhhh1 educ_levelhhh1 educ_levelhhh2 educ_levelhhh3  if d_sample==1, by(status) unequal
eststo D

estpost ttest inhabitants type1 type2 type3 property1 property2 property3 property4 area_viv bath_viv econ_viv chief age sexhhh1 educ_levelhhh1 educ_levelhhh2 educ_levelhhh3 if i_sample==1, by(status) unequal
eststo I

estpost ttest inhabitants type1 type2 type3 property1 property2 property3 property4 area_viv bath_viv econ_viv chief age sexhhh1 educ_levelhhh1 educ_levelhhh2 educ_levelhhh3 if status != 1, by(status) unequal
eststo DIFF


esttab D I DIFF using "/Users/mrfreerider/Documents/Research/PEB Drivers/paper drivers/ttest_survey_hh.tex", label replace noobs nonumber booktabs compress nogap cells("t(fmt(a2)) b(label(Diff.) star fmt(a2))") mgroups("Directly" "Indirectly" "Difference", pattern( 1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) refcat(inhabitants "\textbf{\emph{Housing}}" chief "\textbf{\emph{Head of Household}}", nolabel) 

** PEB-MOT-ATT
eststo clear
estpost ttest $motiv if d_sample==1, by(status) unequal
eststo D
estpost ttest $motiv if i_sample==1, by(status) unequal
eststo I
estpost ttest $motiv if status != 1, by(status) unequal
eststo DIFF

esttab D I DIFF using"/Users/mrfreerider/Documents/Research/PEB Drivers/paper drivers/ttest_survey_peb.tex", label replace noobs nonumber booktabs compress nogap cells("t(fmt(%9.3f)) b(label(Diff.) star fmt(%9.3f))") mgroups("Directly" "Indirectly" "Difference", pattern( 1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) refcat( pcb_ES "\textbf{\emph{Intrinsic Drivers}}"  mon_ES "\textbf{\emph{Extrinsic Drivers}}" snc_ES "\textbf{\emph{Social influence}" inter_1 "\textbf{\emph{Reciprocity}}"  trust_others "\textbf{\emph{Trust}}", nolabel) 




******************************************************
** Saving data
save data_survey_fixed.dta, replace
save data_survey.csv, replace

******************************************************




******************************************************
** Correlations
*****************************************************

** Table of correlations For ALL The PEBs
/*
eststo clear
estpost corr $motiv, matrix listwise
esttab using correlations.tex,replace  unstack noobs nonum compress collabels(none) booktabs varwidth(5) b(%9.2f) not nomtitles star(* 0.10 ** 0.05 *** 0.01) 

** Correlations for ES
global motiv_ES beh_ES beh_ES pcb_ES snc_ES snn_ES gui_ES emb_ES pri_ES mon_ES int_ES trust_others fair_others help_others trust_neigh trust_osp trust_gob trust_energy trust_envi inter_1 inter_2

eststo clear
estpost corr $motiv_ES, matrix listwise
esttab using correlations_ES.tex,replace  unstack noobs nonum compress collabels(none) booktabs varwidth(5) b(%9.2f) not nomtitles star(* 0.10 ** 0.05 *** 0.01) 

** Correlations for WS
global motiv_WS beh_WS beh_WS pcb_WS snc_WS snn_WS gui_WS emb_WS pri_WS mon_WS int_WS trust_others fair_others help_others trust_neigh trust_osp trust_gob trust_energy trust_envi inter_1 inter_2

eststo clear
estpost corr $motiv_WS, matrix listwise
esttab using correlations_WS.tex,replace  unstack noobs nonum compress collabels(none) booktabs varwidth(5) b(%9.2f) not nomtitles star(* 0.10 ** 0.05 *** 0.01) 

** Correlations for WR
global motiv_WR beh_WR beh_WR pcb_WR snc_WR snn_WR gui_WR emb_WR pri_WR mon_WR int_WR trust_others fair_others help_others trust_neigh trust_osp trust_gob trust_energy trust_envi inter_1 inter_2

eststo clear
estpost corr $motiv_WR, matrix listwise
esttab using correlations_WR.tex,replace  unstack noobs nonum compress collabels(none) booktabs varwidth(5) b(%9.2f) not nomtitles star(* 0.10 ** 0.05 *** 0.01) 
*/
******************************************************
** New Aggregate Variables
*****************************************************
** Changing Variables Labels
**PEB
label var beh_ES "Energy saving"
label var beh_WR "Waste recycling"
label var beh_WS "Water saving"
** PCB
label var pcb_ES "PCB ES"
label var pcb_WR "PCB WR"
label var pcb_WS "PCB WS"
** Social Norms
label var snc_ES "SN closest ES"
label var snc_WR "SN closest WR" 
label var snc_WS "SN closest WS"
label var snn_ES "SN neighbors ES" 
label var snn_WR "SN neighbors WR"
label var snn_WS "SN neighbors WS"
**Anticipated Guilt
label var gui_ES "A. guilt ES"
label var gui_WR "A. guilt WR"
label var gui_WS "A. guilt WS"
**Anticipated Embarrassment
label var emb_ES "A. embarrassment ES" 
label var emb_WR "A. embarrassment WR"
label var emb_WS "A. embarrassment WS"
** Anticipated Pride
label var pri_ES "A. pride ES"
label var pri_WR "A. pride WR"
label var pri_WS "A. pride WS"
** Extrinsic payment
label var mon_ES "Monetary dis. ES"
label var mon_WR "Monetary dis. WR"
label var mon_WS "Monetary dis. WS"
**  Intention of behavior
label var int_ES "Intention ES"
label var int_WR "Intention WR"
label var int_WS "Intention WS"

***********************************************************
** Factor anaylisys
***********************************************************

**  Bartlett's test for sphericity: Calculates the determinant of the matrix of the sums of products and cross-products (S) from which the intercorrelations matrix is derived. The determinant of the matrix S is converted to a chi-square statistic and tested for significance. The null hypothesis is that the intercorrelation matrix comes from a population in which the variables are noncollinear (i.e. an identity matrix), and that the non-zero correlations in the sample matrix are due to sampling error. Kaiser-Meyer-Olkin Measure of Sampling Adequacy: is an index for comparing the magnitudes of the observed correlation coefficients to the magnitudes of the partial correlation coefficients. Large values for the KMO measure indicate that a factor analysis of the variables is a good idea. Better with >0.5

factor pcb_ES-int_ES if treated==1, pcf  blanks(0.3)
rotate,  quartimin blanks(0.3)
predict f_treat*, bartlett

factor pcb_ES-int_ES if spill==1, pcf  blanks(0.3)
rotate,  quartimin blanks(0.3)
predict f_spill* , bartlett

factor pcb_ES-int_ES if control==1, pcf  blanks(0.3)
rotate,  quartimin blanks(0.3)
predict f_control* , bartlett


gen f1=(treated*f_treat1)+(spill*f_spill1)+(control*f_control1)



factor pcb_ES-int_ES, ml blanks(0.3)
estat kmo
screeplot
screeplot, yline(1)
graph save scree_all.eps, replace

** Rotations two approach (varimax and promax)
rotate,  quartimin blanks(0.3)
//rotate, clear

** Scatter plots of the loading and the score factors
loadingplot, yline(0) xline(0) 
scoreplot, yline(0) xline(0)

** Factor Scores // With quartimin rotation
predict f*

monetary pride embarra snneigh snclose guilt pcb, bartlett

predict f* 
		



** Generate aggregate measures of pro-environmental behaviors
gen index_beh_avg = (beh_ES+beh_WR+beh_WS)/15 //aggregate
label var index_beh_avg "Index of PEB (avg)"

** Generate Aggregate measures of Motivations
gen index_pcb_avg = (pcb_ES+pcb_WR+pcb_WS)/15 //aggregate
label var index_pcb_avg "Index of PCB (avg)"
gen index_gui_avg = (gui_ES+gui_WR+gui_WS)/15
label var index_gui_avg "Index of A. Guilt (avg)"
gen index_emb_avg = (emb_ES+emb_WR+emb_WS)/15
label var index_emb_avg "Index of A. Embarrassament (avg)"
gen index_pri_avg = (pri_ES+pri_WR+pri_WS)/15
label var index_pri_avg "Index of A. Pride (avg)"
gen index_snc_avg = (snc_ES+snc_WR+snc_WS)/15
label var index_snc_avg "Index of SN from closest (avg)"
gen index_snn_avg = (snn_ES+snn_WR+snn_WS)/15
label var index_snn_avg "Index of SN Neighbors (avg)"
gen index_mon_avg = (mon_ES+mon_WR+mon_WS)/15
label var index_mon_avg "Index of Monetary Displacement (avg)"
gen index_int_avg = (int_ES+int_WR+int_WS)/15
label var index_int_avg "Index of Intention of perform (avg)"

gen trust_othe = (trust_others+fair_others+help_others)/3
label var trust_othe "Index of Trust in Neighbors"
gen trust_inst = (trust_osp+ trust_gob+ trust_energy+ trust_envi)/4
label var trust_inst "Index of Trust in Institutions"

** Generate Polychoric Score Measures
local measures beh pcb gui mon int snc snn emb pri
foreach Y in `measures'{
	polychoricpca (`Y'_WS `Y'_WR `Y'_ES), score(index_`Y'_pol) nscore(1)
}

label var index_beh_pol1 "Index of PEB (poly)"
label var index_pcb_pol1 "Index of PCB (poly)"
label var index_gui_pol1 "Index of A. Guilt (poly)"
label var index_emb_pol1 "Index of A. Embarrassament (poly)"
label var index_pri_pol1 "Index of A. Pride (poly)"
label var index_snc_pol1 "Index of SN from closest (poly)"
label var index_snn_pol1 "Index of SN Neighbors (poly)"
label var index_mon_pol1 "Index of Monetary Displacement (poly)"
label var index_int_pol1 "Index of Intention of perform (poly)"


** Conbranch's Alpha of reliability measure

alpha pcb_ES pcb_WS pcb_WR
alpha snc_ES snc_WR snc_WS
alpha snn_WS snn_ES snn_WR
alpha gui_ES gui_WS gui_WR
alpha emb_WR emb_WS emb_ES
alpha pri_ES pri_WS pri_WR
alpha mon_WR mon_ES mon_WS 
alpha int_ES int_WR int_WS
alpha trust_others fair_others help_others
alpha trust_osp trust_gob trust_energy trust_envi
alpha inter_1 inter_2


** Correlations for Aggregate motivations
global motiv_corr index_pcb_pol1 index_gui_pol1 index_mon_pol1 index_int_pol1 index_emb_pol1 index_pri_pol1 index_snc_pol1 index_snn_pol1 index_pcb_avg index_gui_avg index_mon_avg index_int_avg index_emb_avg index_pri_avg index_snc_avg index_snn_avg 

eststo clear
estpost corr $motiv_corr, matrix listwise
esttab using correlations_agg.tex,replace  unstack noobs nonum compress collabels(none) booktabs varwidth(5) b(%9.2f) not nomtitles star(* 0.10 ** 0.05 *** 0.01) 


** Variable for aggregate treatment
gen treatment=1 if status!=1
replace treatment=0 if treatment==.
label var treatment "Received SN activation"



******************************************************
*** DIFFERENCES BETWEEN GRUOPS
******************************************************

use data_survey_fixed.dta, replace

** treatment status group indicators
gen treat_g1 =1 if status==3
replace treat_g1=0 if status==1
gen treat_g2 =1 if status==2
replace treat_g2=0 if status==1

label define exposed 1 "Control" 2 "Indireclty" 3 "Directly"
label values status exposed

** Histogram 
local all_var beh pcb gui mon int snc snn emb pri
local status Control Indireclty Directly
local exposed 1 2 3


foreach var in `all_var'{
 foreach st in `status'{
 foreach exp in `exposed'{
	histogram index_`var'_avg if status==`exp', bin(5) color(none) lcolor(*1) title(`st') name(`var'`st')
	histogram index_`var'_avg, bin(5) color(none) lcolor(*1) title(Overall) name(`var'all)
	graph combine `var'`st' `var'all, ycommon
	graph drop 
	

 
 
}
}}

	histogram index_`var'_pol1 if !`st', bin(5) color(*.25) lcolor(*1) title(`st')
	
	
stripplot index_beh_pol1, over(status) cumul cumprob box centre vertical refline yla(, ang(h)) xla(, noticks) xsc(titlegap(*5))



** Kernel Density Plots
local motiv pcb gui mon int snc snn emb pri
foreach mot in `motiv'{
kdensity index_`mot'_pol1 if treat_g1==0, gen(den_con_`mot' ind_con_`mot')
kdensity index_`mot'_pol1 if treat_g1==1, gen(den_tre_`mot' ind_tre_`mot')
kdensity index_`mot'_pol1 if treat_g2==1, gen(den_spi_`mot' ind_spi_`mot')

kdensity index_`mot'_avg if treat_g1==0, gen(den_con_`mot'avg ind_con_`mot'avg)
kdensity index_`mot'_avg if treat_g1==1, gen(den_tre_`mot'avg ind_tre_`mot'avg)
kdensity index_`mot'_avg if treat_g2==1, gen(den_spi_`mot'avg ind_spi_`mot'avg)

summarize index_`mot'_pol1 if treat_g1==0, mean
local con_mean_`mot' = r(mean)
summarize index_`mot'_pol1 if treat_g1==1, mean
local tre_mean_`mot' = r(mean)

line ind_con_`mot' den_con_`mot'  || line ind_tre_`mot' den_tre_`mot' || line ind_spi_`mot' den_spi_`mot'
graph export `mot'_poly_group.eps, replace
}




, xline(`con_mean', lcolor(blue))
 
 
 
 
 , xline(`tre_mean', lcolor(blue))
 
 
 , || xline h x if  x == `con_mean'



separate index_beh_pol1, by(status)
label var index_beh_pol11 "Control"
label var index_beh_pol12 "Spillover"
label var index_beh_pol13 "Treated"

twoway__histogram_gen index_beh_pol1 if status==1, gen(den_control x1)  density
twoway__histogram_gen index_beh_pol1 if status==2, gen(den_spill x2) density
twoway__histogram_gen index_beh_pol1 if status==3, gen(den_treat x3) density

qui gen den = den_control +  den_spill +  den_treat

twoway bar den_spill x1 ,  bcolor(blue) xsc(r(10 .)) || ///
rbar den_spill  x2 ,  bcolor(red) 



///
legend(order(1 "Domestic" 2 "Foreign") pos(11) col(1) ring(0)) ///
xtitle("`: var label status'") ytitle(Frequency) xla(10(5)40) yla(, ang(h)) ///
xsc(r(10 .)) 



******************************************************
** Data from experimental setting
******************************************************

**Setting the  panel
reshape long cons above norm, i(HHid) j(m) string
gen year = substr(m,1,4)
gen month = substr(m,5,6)
destring year, replace
destring month, replace
drop m
gen period=ym(year, month)
format period %tm

xtset HHid period
sort HHid period 
label var cons "Water consumption (m3/period)"
label var period "Period"

label var villageid "Village"

label define status 1 "Control" 2 "Indirectly" 3 "Directly"
label values status status

bys i: generate time=_n

** norm activation
label var norm "Social norm"
replace norm=. if norm==0

** Post variable
gen post=1 if period>=692 // after August
replace post=0 if post==.
replace post=0 if period>=697 & bill==0

** Approval message
label var above "Above SN"
replace above=. if norm==.
gen above_treated=above if treated==1
label var above_treated "Received an approval"

**Disapproval message
gen below=1-above
label var below "Below SN"
gen below_treated=below if treated==1
label var below_treated "Received a disapproval"

** Consumption Variable
replace cons=. if cons==0
bys period: egen cons_prom=mean(cons)
bys period: egen cons_sd=sd(cons)
gen cons_std=(cons-cons_prom)/cons_sd
label var cons_std "Standardize water consumption"

bys HHid: egen cons_bt=mean(cons) if period<692
label var cons_bt "Consumption Before T" 
bys HHid: egen cons_at=mean(cons) if period>=692
label var cons_at "Consumption After T"

** Number of deliverys 
gen delivery=1 if post==1 & treated==1
replace delivery=. if cons==. & bill==1 & period>=692
bys HHid: egen delv=sum(delivery)
label var delv "Message deliveries (\#)"

** Distance to norm
sort HHid period
gen dist_norm = cons-L.norm
label var dist_norm "Distance to SN (m3)"

** Compliance to norm
gen comply =1 if dist_norm<=0
replace comply=0 if dist_norm>0
replace comply=. if control==1
label var comply "Compliance with SN"

** gen change in behavior

bys HHid: egen prom_cons_bt = mean(cons_bt) 
label var prom_cons_bt "Consumption Before T"
bys HHid: egen prom_cons_at = mean(cons_at)
label var prom_cons_at "Consumption After T"
bys HHid: gen ch_cons =  prom_cons_at-prom_cons_bt
label var ch_cons "Change in consumption"


// Create Distribution Before treatment
bys HHid: egen cons_prom_before = mean(cons) if period<692
bys HHid:  replace cons_prom_before=cons_prom_before[_n-1] if cons_prom_before==.
// Create new indicator variable of the median
egen cons_pctil=xtile(cons_prom_before), by(villageid) nq(2) 
gen cons_change=1 if ch_cons<=0
replace cons_change=2 if ch_cons>0
gen altruist=1 if cons_pctil==1 & cons_change==1
gen individualist=1 if cons_pctil==2 & cons_change==2
gen competitive=1 if cons_pctil==1 & cons_change==2
gen cond_coop=1 if cons_pctil==2 & cons_change==1

** Summary Statistics BY TREATMENT STATUS
eststo clear
estpost su cons prom_cons_bt prom_cons_at ch_cons delv dist_norm above below comply if status==1
est store A
estpost su cons prom_cons_bt prom_cons_at ch_cons delv dist_norm above below comply if status==2
est store B
estpost su cons prom_cons_bt prom_cons_at ch_cons delv dist_norm above below comply if status==3
est store C
esttab A B C using sum_mot&pe_exp.tex, replace label mtitle("Control" "Directly" "Indirectly") ///
cells("mean(label(Mean) fmt(a2)) sd(label(Std. Dev.)) count(label(Obs.))") ///
nostar nonum booktabs  gaps varwidth(20)  

** Summary Statistics BY VILLAGES
eststo clear
estpost su cons  prom_cons_bt prom_cons_at ch_cons treated spill control delv dist_norm above below comply villageid if villageid==1
est store A
estpost su cons prom_cons_bt prom_cons_at ch_cons treated spill control delv dist_norm above below comply villageid if villageid==2
est store B
estpost su cons  prom_cons_bt prom_cons_at ch_cons treated spill control delv dist_norm above below comply villageid if villageid==3
est store C
estpost su cons  prom_cons_bt prom_cons_at ch_cons treated spill control delv dist_norm above below comply villageid if villageid==4
est store D
estpost su cons  prom_cons_bt prom_cons_at ch_cons treated spill control delv dist_norm above below comply villageid if villageid==5
est store E
estpost su cons  prom_cons_bt prom_cons_at ch_cons treated spill control delv dist_norm above below comply villageid if villageid==6
est store F
estpost su cons prom_cons_bt prom_cons_at ch_cons treated spill control delv dist_norm above below comply villageid if villageid==7
est store G
estpost su cons prom_cons_bt prom_cons_at ch_cons treated spill control delv dist_norm above below comply villageid if villageid==8
est store H
esttab A B C D E F G H using sum_mot&pe_exp_villages.tex, replace label mtitle("A" "C" "D" "E" "F" "G" "H") ///
cells("mean(label(Mean) fmt(a2)) sd(label(Std. Dev.)) count(label(Obs.))") ///
nostar nonum booktabs collabels(none) gaps unstack

******************************************************
** Mean Differences Between Groups
*****************************************************


global aggmotiv index_pcb_pol1 index_gui_pol1 index_mon_pol1 index_int_pol1 index_emb_pol1 index_pri_pol1  index_snc_pol1 index_snn_pol1 index_pcb_avg index_gui_avg index_mon_avg index_int_avg index_emb_avg index_pri_avg index_snc_avg index_snn_avg inter_1 inter_2  trust_othe trust_inst 

global actmotiv pcb_ES snc_ES snn_ES gui_ES emb_ES pri_ES mon_ES int_ES pcb_WR snc_WR snn_WR gui_WR emb_WR pri_WR mon_WR int_WR pcb_WS snc_WS snn_WS gui_WS emb_WS pri_WS mon_WS int_WS trust_others fair_others help_others trust_neigh trust_osp trust_gob trust_energy 

//Control-Treated
eststo clear
estpost ttest $aggmotiv, by(treat_g1)
eststo tc
//Control-Spill
estpost ttest $aggmotiv, by(treat_g2)
eststo sc

esttab tc sc using mean_diff_treated_control.tex, replace label mtitle("Control-Directly" "Control-Indirectly") refcat(index_pcb_pol1 "\textbf{\emph{Index polychoric}}" index_pcb_avg "\textbf{\emph{Index Average}}" beh_ES "\textbf{\emph{Behavior}}" inter_1 "\textbf{\emph{Interdependency}}" trust_othe "\textbf{\emph{Trust Measures}}", nolabel) noobs nonumber booktabs  star(* 0.10 ** 0.05 *** 0.01) 


//Control-Treated
eststo clear
estpost ttest $actmotiv, by(treat_g1)
eststo tc
//Control-Spill
estpost ttest $actmotiv, by(treat_g2)
eststo sc
esttab tc sc using mean_diff_treated_control_k.tex, replace label mtitle("Control-Directly" "Control-Indirectly") refcat(pcb_ES "\textbf{\emph{Energy related}}" pcb_WR "\textbf{\emph{Recycling related}}" pcb_WS "\textbf{\emph{Water related}}" trust_others "\textbf{\emph{Trust Measures}}", nolabel) noobs nonumber booktabs  star(* 0.10 ** 0.05 *** 0.01) 




corr gui_ES gui_WS gui_WR 
polychoric gui_ES gui_WS gui_WR
display r(sum_w)
global N = r(sum_w)
matrix r_gui=r(R)
matrix list r_gui
factormat r_gui, n($N) factors(3)



global N = r(sum_w)
matrix r = r(R)
factormat r, n( N ) factors(3)
predict f1 f2 f3


******************************************************
** Regressions 
*****************************************************


bys status: egen PEB_st=mean(PEB)

bys status: egen PCB_st=mean(PCB)

bys status: egen EMB_st=mean(EMB)

global $controlvar inhabitants tipo_viv prop_viv bath_viv econ_viv age educ sex_num ses

** Regressions NAIVE
do reg_naive.do
** Regressions cross Effects 
do reg_cross.do







global covariates tipo_viv prop_viv area_viv bath_viv econ_viv  inhabitants educ age sex_num

pscore treatment $covariates, logit pscore(mypscore1)

psmatch2 treatment, outcome(PEB2 ) pscore(mypscore1) neighbor(1)

pstest age tipo_viv prop_viv area_viv bath_viv econ_viv  inhabitants educ sexhhh1, both graph

psmatch2 treated, outcome(INT2) pscore(mypscore) radius caliper(0.03)

pstest age tipo_viv prop_viv area_viv bath_viv econ_viv  inhabitants educ sexhhh1, both graph
 
 
eststo clear
reg PEB2 i.treated##c.PCB2 i.treated##c.GUI2 EMB2 PRI2 SNC2 SNN2 MON2 INT2  TRU_N TRU_I trust_neigh   ///
inter_1 inter_2 ///
i.villageid i.ses ///
inhabitants tipo_viv prop_viv bath_viv econ_viv age educ
estadd local "Village" Y
estadd local "HH Controls" Y
eststo K

estadd local "Village" Y
estadd local "HH" Controls
eststo A

reg PEB2 inter_1 inter_2 ///
trust_others fair_others help_others trust_neigh ///
trust_osp trust_gob trust_energy trust_envi ///
i.villageid i.ses ///
inhabitants tipo_viv prop_viv bath_viv econ_viv age educ, beta







psmatch2 treated $covariates 

psmatch2 compl 1.treated##c.gui $controlvars i.ses i.villageid

psmatch2 compl i._treated##i.image_2  $controlvars i.ses i.villageid


,neighbor(integer k>1) caliper(real)


