*create log file
capture log close
set more off
clear


log using "/Users/audrie/Desktop/stata/WBB-EED-analysis/log/bangladesh-dm-ee-ffq.log", text replace

*------------------------------------------------
* 5-bangladesh-dm-ffq.do
*
* Audrie Lin (audrielin@berkeley.edu)
*
* Description:
*
* calculate rates of exclusive breastfeeding (exclusively breastfed yesterday & no to other foods or drinks or liquids consumed within 24 hour period)
* calculate rates of complementary feeding if the child had any non-breastmilk liquid or solids on the previous day (24 hour period)
* Use 24-hour definition of exclusive breastfeeding
* Denominator is number of women who answered the FFQ
* 
*
* version 1 (2018-07-20) created
* 
*-----------------------------------------------
*-----------------------------------------------
* 
* input files: 
* 
* FFQ Form data file:
* 
* "~/Desktop/stata/WBB-EED-analysis/data/untouched/Baseline/EE_Baseline_FFQ_Raw_data_13Sep2017.dta (raw)
*
* output files:
* None - % exclusive breastfeeding calculated for manuscript 
*
*------------------------------------------------
*------------------------------------------------



global indir   "~/Desktop/stata/WBB-EED-analysis/data/untouched/"
global tempdir "~/Desktop/stata/WBB-EED-analysis/data/temp/"
global outdir  "~/Desktop/stata/WBB-EED-analysis/data/final"
global output  "~/Desktop/stata/WBB-EED-analysis/analysis/tables/rawoutput"


global pgm "5-bangladesh-dm-ffq.do"

cd $tempdir

clear
set more off


use "~/Desktop/stata/WBB-EED-analysis/data/untouched/Baseline/EE_Baseline_FFQ_Raw_data_13Sep2017.dta"

rename childId childno
egen childid = concat(dataid childno)
order childid

save "~/Desktop/stata/WBB-EED-analysis/data/temp/EE_Baseline_FFQ_Raw_data_13Sep2017.dta", replace

clear


*check Raw FFQ IDs against the main EE dataset to ensure that the IDs all match kids that are actually in the dataset

use "~/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/washb-bangladesh-anthro-diar-ee-med-blind-tr-stool-urine-age-lab.dta"

merge 1:1 childid using "~/Desktop/stata/WBB-EED-analysis/data/temp/EE_Baseline_FFQ_Raw_data_13Sep2017.dta"

*total EBF for raw dataset
count if c607>0 & c607a==5 & c608_1_3==2 & c608_2_3==2 & c608_3_3==2 & c608_4_3==2 & c608_5_3==2 & c608_6_3==2 & c608_7_3==2 & c608_8_3==2 & c608_9_3==2 & c608_10_3==2 & c608_11_3==2 & c608_12_3==2 & c609_1_1==2 & c609_2_1==2 & c609_2_2==2 & c609_2_3==2 & c609_2_4==2 & c609_2_5==2 & c609_3_1==2 & c609_3_2==2 & c609_3_3==2 & c609_4_1==2 & c609_4_2==2 & c609_5_1==2 & c609_5_2==2 & c609_5_3==2 & c609_5_4==2 & c609_5_5==2 & c609_6_1==2 & c609_6_2==2 & c609_7_1==2 & c609_7_2==2 & c609_7_3==2 & c609_7_4==2 & c609_7_5==2 & c609_7_6==2 & c609_7_7==2 & c609_8_1==2 & c609_8_2==2 & c609_8_3==2 & c609_8_4==2 & c609_8_5==2 & c609_8_6==2 & c609_9_1==2 & c609_9_2==2 & c609_9_3==2 & c609_10_1==2 & c609_11_1==2 & c609_12_1==2 & c609_12_2==2 & c609_12_3==2 & c609_13_1==2 & c609_13_2==2 & c609_13_3==2 & c609_13_4==2 & c609_13_5==2 & c609_13_6==2 & c609_13_7==2 & c609_14_1==2 & c609_14_2==2 & c609_14_3==2 & c609_15_1==2 & c609_15_2==2 & c609_15_3==2 & c609_16_1==2 & c609_16_2==2 & c609_16_3==2 & c609_16_4==2 & c609_16_5==2 & c609_17_1==2 & c609_17_2==2 & c609_17_3==2 & c609_17_4==2 & c609_18_1==2 & c609_18_2==2     


*calculate ffq if have urine or stool outcome (n=1090)

drop if t1_aat==. & t1_mpo==. & t1_neo==. & Lact1==. & Mann1==.

*c605 = Is the baby still breastfeeding, or is he/she completely weaned?
*count if completely weaned (n=11)
*count if c605==2
*list childid c605 c606months c606days c607 if c605==2

*count if c605 is missing, no ffq data collected 
*count if c605==.
*list childid c605 c606months c606days c607 if c605==.

*drop those with no ffq breastfeeding data collected

drop if c605==. & c607==.




*c608_#_2 and c608_#_3 are flipped. 
*c608_#_2 = How many times
*c608_#_3 = Yes/No
*Example: c608_1_1 = Water
*         c608_1_3 = Yes/No
*         c608_1_2 = How many times eaten/drank?
*
*Example: c608_2_1 = Sugar Water
*         c608_2_3 = Yes/No
*         c608_2_2 = How many times eaten/drank?


*count if say "no" to c608 series of questions
*count if say "no" to c609 series of questions
*manually checked c609_19_1, whenever there was an non-no entry, scrolled through and checked if yes to any c608 or c609 series questions. All c609_19_1 other food entries had a yes to another c608 or c609 series question. Can't include c619_1_1 because it's not numeric.

count if c607>0 & c607a==5 & tr==1 & c608_1_3==2 & c608_2_3==2 & c608_3_3==2 & c608_4_3==2 & c608_5_3==2 & c608_6_3==2 & c608_7_3==2 & c608_8_3==2 & c608_9_3==2 & c608_10_3==2 & c608_11_3==2 & c608_12_3==2 & c609_1_1==2 & c609_2_1==2 & c609_2_2==2 & c609_2_3==2 & c609_2_4==2 & c609_2_5==2 & c609_3_1==2 & c609_3_2==2 & c609_3_3==2 & c609_4_1==2 & c609_4_2==2 & c609_5_1==2 & c609_5_2==2 & c609_5_3==2 & c609_5_4==2 & c609_5_5==2 & c609_6_1==2 & c609_6_2==2 & c609_7_1==2 & c609_7_2==2 & c609_7_3==2 & c609_7_4==2 & c609_7_5==2 & c609_7_6==2 & c609_7_7==2 & c609_8_1==2 & c609_8_2==2 & c609_8_3==2 & c609_8_4==2 & c609_8_5==2 & c609_8_6==2 & c609_9_1==2 & c609_9_2==2 & c609_9_3==2 & c609_10_1==2 & c609_11_1==2 & c609_12_1==2 & c609_12_2==2 & c609_12_3==2 & c609_13_1==2 & c609_13_2==2 & c609_13_3==2 & c609_13_4==2 & c609_13_5==2 & c609_13_6==2 & c609_13_7==2 & c609_14_1==2 & c609_14_2==2 & c609_14_3==2 & c609_15_1==2 & c609_15_2==2 & c609_15_3==2 & c609_16_1==2 & c609_16_2==2 & c609_16_3==2 & c609_16_4==2 & c609_16_5==2 & c609_17_1==2 & c609_17_2==2 & c609_17_3==2 & c609_17_4==2 & c609_18_1==2 & c609_18_2==2     
count if c607>0 & c607a==5 & tr==5 & c608_1_3==2 & c608_2_3==2 & c608_3_3==2 & c608_4_3==2 & c608_5_3==2 & c608_6_3==2 & c608_7_3==2 & c608_8_3==2 & c608_9_3==2 & c608_10_3==2 & c608_11_3==2 & c608_12_3==2 & c609_1_1==2 & c609_2_1==2 & c609_2_2==2 & c609_2_3==2 & c609_2_4==2 & c609_2_5==2 & c609_3_1==2 & c609_3_2==2 & c609_3_3==2 & c609_4_1==2 & c609_4_2==2 & c609_5_1==2 & c609_5_2==2 & c609_5_3==2 & c609_5_4==2 & c609_5_5==2 & c609_6_1==2 & c609_6_2==2 & c609_7_1==2 & c609_7_2==2 & c609_7_3==2 & c609_7_4==2 & c609_7_5==2 & c609_7_6==2 & c609_7_7==2 & c609_8_1==2 & c609_8_2==2 & c609_8_3==2 & c609_8_4==2 & c609_8_5==2 & c609_8_6==2 & c609_9_1==2 & c609_9_2==2 & c609_9_3==2 & c609_10_1==2 & c609_11_1==2 & c609_12_1==2 & c609_12_2==2 & c609_12_3==2 & c609_13_1==2 & c609_13_2==2 & c609_13_3==2 & c609_13_4==2 & c609_13_5==2 & c609_13_6==2 & c609_13_7==2 & c609_14_1==2 & c609_14_2==2 & c609_14_3==2 & c609_15_1==2 & c609_15_2==2 & c609_15_3==2 & c609_16_1==2 & c609_16_2==2 & c609_16_3==2 & c609_16_4==2 & c609_16_5==2 & c609_17_1==2 & c609_17_2==2 & c609_17_3==2 & c609_17_4==2 & c609_18_1==2 & c609_18_2==2 
count if c607>0 & c607a==5 & tr==6 & c608_1_3==2 & c608_2_3==2 & c608_3_3==2 & c608_4_3==2 & c608_5_3==2 & c608_6_3==2 & c608_7_3==2 & c608_8_3==2 & c608_9_3==2 & c608_10_3==2 & c608_11_3==2 & c608_12_3==2 & c609_1_1==2 & c609_2_1==2 & c609_2_2==2 & c609_2_3==2 & c609_2_4==2 & c609_2_5==2 & c609_3_1==2 & c609_3_2==2 & c609_3_3==2 & c609_4_1==2 & c609_4_2==2 & c609_5_1==2 & c609_5_2==2 & c609_5_3==2 & c609_5_4==2 & c609_5_5==2 & c609_6_1==2 & c609_6_2==2 & c609_7_1==2 & c609_7_2==2 & c609_7_3==2 & c609_7_4==2 & c609_7_5==2 & c609_7_6==2 & c609_7_7==2 & c609_8_1==2 & c609_8_2==2 & c609_8_3==2 & c609_8_4==2 & c609_8_5==2 & c609_8_6==2 & c609_9_1==2 & c609_9_2==2 & c609_9_3==2 & c609_10_1==2 & c609_11_1==2 & c609_12_1==2 & c609_12_2==2 & c609_12_3==2 & c609_13_1==2 & c609_13_2==2 & c609_13_3==2 & c609_13_4==2 & c609_13_5==2 & c609_13_6==2 & c609_13_7==2 & c609_14_1==2 & c609_14_2==2 & c609_14_3==2 & c609_15_1==2 & c609_15_2==2 & c609_15_3==2 & c609_16_1==2 & c609_16_2==2 & c609_16_3==2 & c609_16_4==2 & c609_16_5==2 & c609_17_1==2 & c609_17_2==2 & c609_17_3==2 & c609_17_4==2 & c609_18_1==2 & c609_18_2==2  
count if c607>0 & c607a==5 & tr==7 & c608_1_3==2 & c608_2_3==2 & c608_3_3==2 & c608_4_3==2 & c608_5_3==2 & c608_6_3==2 & c608_7_3==2 & c608_8_3==2 & c608_9_3==2 & c608_10_3==2 & c608_11_3==2 & c608_12_3==2 & c609_1_1==2 & c609_2_1==2 & c609_2_2==2 & c609_2_3==2 & c609_2_4==2 & c609_2_5==2 & c609_3_1==2 & c609_3_2==2 & c609_3_3==2 & c609_4_1==2 & c609_4_2==2 & c609_5_1==2 & c609_5_2==2 & c609_5_3==2 & c609_5_4==2 & c609_5_5==2 & c609_6_1==2 & c609_6_2==2 & c609_7_1==2 & c609_7_2==2 & c609_7_3==2 & c609_7_4==2 & c609_7_5==2 & c609_7_6==2 & c609_7_7==2 & c609_8_1==2 & c609_8_2==2 & c609_8_3==2 & c609_8_4==2 & c609_8_5==2 & c609_8_6==2 & c609_9_1==2 & c609_9_2==2 & c609_9_3==2 & c609_10_1==2 & c609_11_1==2 & c609_12_1==2 & c609_12_2==2 & c609_12_3==2 & c609_13_1==2 & c609_13_2==2 & c609_13_3==2 & c609_13_4==2 & c609_13_5==2 & c609_13_6==2 & c609_13_7==2 & c609_14_1==2 & c609_14_2==2 & c609_14_3==2 & c609_15_1==2 & c609_15_2==2 & c609_15_3==2 & c609_16_1==2 & c609_16_2==2 & c609_16_3==2 & c609_16_4==2 & c609_16_5==2 & c609_17_1==2 & c609_17_2==2 & c609_17_3==2 & c609_17_4==2 & c609_18_1==2 & c609_18_2==2 

gen EBF = c607>0 & c607a==5 & c608_1_3==2 & c608_2_3==2 & c608_3_3==2 & c608_4_3==2 & c608_5_3==2 & c608_6_3==2 & c608_7_3==2 & c608_8_3==2 & c608_9_3==2 & c608_10_3==2 & c608_11_3==2 & c608_12_3==2 & c609_1_1==2 & c609_2_1==2 & c609_2_2==2 & c609_2_3==2 & c609_2_4==2 & c609_2_5==2 & c609_3_1==2 & c609_3_2==2 & c609_3_3==2 & c609_4_1==2 & c609_4_2==2 & c609_5_1==2 & c609_5_2==2 & c609_5_3==2 & c609_5_4==2 & c609_5_5==2 & c609_6_1==2 & c609_6_2==2 & c609_7_1==2 & c609_7_2==2 & c609_7_3==2 & c609_7_4==2 & c609_7_5==2 & c609_7_6==2 & c609_7_7==2 & c609_8_1==2 & c609_8_2==2 & c609_8_3==2 & c609_8_4==2 & c609_8_5==2 & c609_8_6==2 & c609_9_1==2 & c609_9_2==2 & c609_9_3==2 & c609_10_1==2 & c609_11_1==2 & c609_12_1==2 & c609_12_2==2 & c609_12_3==2 & c609_13_1==2 & c609_13_2==2 & c609_13_3==2 & c609_13_4==2 & c609_13_5==2 & c609_13_6==2 & c609_13_7==2 & c609_14_1==2 & c609_14_2==2 & c609_14_3==2 & c609_15_1==2 & c609_15_2==2 & c609_15_3==2 & c609_16_1==2 & c609_16_2==2 & c609_16_3==2 & c609_16_4==2 & c609_16_5==2 & c609_17_1==2 & c609_17_2==2 & c609_17_3==2 & c609_17_4==2 & c609_18_1==2 & c609_18_2==2 

tab EBF

count if tr==1
count if tr==5
count if tr==6
count if tr==7

save "/Users/audrie/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/FFQ.dta", replace

outsheet using "/Users/audrie/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/Aud_FFQ.csv", comma replace

*end analysis.
*rates above used in EED manuscript.























******* NOT Needed*******IGNORE*************************************************

*of those receiving breastmilk, how many were breastfed yesterday? (c607) (n=998)
count if c607>0 & c607!=. & c605!=2

*missing
*count if c607==.

*count if breastfeeding yes, but 0 times breastfed yesterday
*count if c605==1 & c607==.

*----------------------------------------------*
* If count all who have samples even if no ffq
* Control: 96.3%
* WSH: 92.9%
* N: 93.7%
* N+WSH: 83.5%
*----------------------------------------------*

*N+WSH==7, N==6, WSH==5, Control==1
*count denominators by arm
count if tr==1
count if tr==5
count if tr==6
count if tr==7

*------------
* EE Endline
*------------

clear
set more off

use "~/Desktop/stata/WBB-EED-analysis/data/untouched/Endline/EE_Endline_FFQ_raw_data_07March2017.dta"

rename childNo childno
egen childid = concat(dataid childno)
order childid

save "~/Desktop/stata/WBB-EED-analysis/data/temp/EE_Endline_FFQ_raw_data_07March2017.dta", replace

clear

use "~/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/washb-bangladesh-anthro-diar-ee-med-blind-tr-stool-urine-age-lab.dta"

merge 1:1 childid using "~/Desktop/stata/WBB-EED-analysis/data/temp/EE_Endline_FFQ_raw_data_07March2017.dta"


*calculate ffq if have urine or stool outcome (n=1090)

drop if t3_aat==. & t3_mpo==. & t3_neo==. & Lact3==. & Mann3==.

*count if completely weaned (n=11)
count if c605==2
list childid c605 c606months c606days if c605==2

*count if c605 is missing, no ffq data collected 
count if c605==.
list childid c605 c606months c606days if c605==.
*drop those with no ffq data collected
drop if c605==. 

*count if say "no" to c608 series of questions
count if c605!=2 & tr==1 & c608_1_1==2 & c608_2_1==2 & c608_3_1==2 & c608_4_1==2 & c608_5_1==2 & c608_6_1==2 & c608_7_1==2 & c608_8_1==2 & c608_9_1==2 & c608_10_1==2 & c608_11_1==2 & c608_12_1==2
count if c605!=2 & tr==5 & c608_1_1==2 & c608_2_1==2 & c608_3_1==2 & c608_4_1==2 & c608_5_1==2 & c608_6_1==2 & c608_7_1==2 & c608_8_1==2 & c608_9_1==2 & c608_10_1==2 & c608_11_1==2 & c608_12_1==2
count if c605!=2 & tr==6 & c608_1_1==2 & c608_2_1==2 & c608_3_1==2 & c608_4_1==2 & c608_5_1==2 & c608_6_1==2 & c608_7_1==2 & c608_8_1==2 & c608_9_1==2 & c608_10_1==2 & c608_11_1==2 & c608_12_1==2
count if c605!=2 & tr==7 & c608_1_1==2 & c608_2_1==2 & c608_3_1==2 & c608_4_1==2 & c608_5_1==2 & c608_6_1==2 & c608_7_1==2 & c608_8_1==2 & c608_9_1==2 & c608_10_1==2 & c608_11_1==2 & c608_12_1==2
















*-----------


* Exclusive breastfeeding

count if c607>0 & c607a==5 & tr==1
count if c607>0 & c607a==5 & tr==5
count if c607>0 & c607a==5 & tr==6
count if c607>0 & c607a==5 & tr==7

*----------------------------------------------*
* If count all who have samples and who answered ffq c607 
* Control: 
* WSH: 
* N: 
* N+WSH: 
*----------------------------------------------*

drop if c607==.

*N+WSH==7, N==6, WSH==5, Control==1
*count denominators by arm
count if tr==1
count if tr==5
count if tr==6
count if tr==7

*count numerator by arm
count if c607>0 & c607!=. & c605!=2 & tr==1
count if c607>0 & c607!=. & c605!=2 & tr==5
count if c607>0 & c607!=. & c605!=2 & tr==6
count if c607>0 & c607!=. & c605!=2 & tr==7

*-------------------
* Exclusive breastfeeding
* Control: 19.7%
* WSH: 29.9%
* N: 55%
* N+WSH: 61.2%
*-------------------


count if c607>0 & c607a==5 & tr==1
count if c607>0 & c607a==5 & tr==5
count if c607>0 & c607a==5 & tr==6
count if c607>0 & c607a==5 & tr==7

