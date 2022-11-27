
// use each dataset saperately

import delimited "C:\Users\proka\OneDrive - Auburn University\Auburn\Research\Collaboration\TxState_BAU\R_Codes\Compiled_Data_Descriptive_Statistics\HIES2005_AER.csv" 

rename q02_1a s1aq01
rename q04_1a s1aq03


generate ae = .
replace ae = 0.20 if inrange(s1aq03,00,00) & s1aq01==2
replace ae = 0.29 if inrange(s1aq03,1.0,1.9) & s1aq01==2
replace ae = 0.36 if inrange(s1aq03,2.0,2.9) & s1aq01==2
replace ae = 0.40 if inrange(s1aq03,3.0,3.9) & s1aq01==2
replace ae = 0.44 if inrange(s1aq03,4.0,4.9) & s1aq01==2
replace ae = 0.47 if inrange(s1aq03,5.0,5.9) & s1aq01==2
replace ae = 0.51 if inrange(s1aq03,6.0,6.9) & s1aq01==2
replace ae = 0.55 if inrange(s1aq03,7.0,7.9) & s1aq01==2
replace ae = 0.59 if inrange(s1aq03,8.0,8.9) & s1aq01==2
replace ae = 0.64 if inrange(s1aq03,9.0,9.9) & s1aq01==2
replace ae = 0.67 if inrange(s1aq03,10.0,10.9) & s1aq01==2
replace ae = 0.72 if inrange(s1aq03,11.0,11.9) & s1aq01==2
replace ae = 0.75 if inrange(s1aq03,12.0,12.9) & s1aq01==2
replace ae = 0.78 if inrange(s1aq03,13.0,13.9) & s1aq01==2
replace ae = 0.80 if inrange(s1aq03,14.0,14.9) & s1aq01==2
replace ae = 0.81 if inrange(s1aq03,15.0,15.9) & s1aq01==2
replace ae = 0.82 if inrange(s1aq03,16.0,16.9) & s1aq01==2
replace ae = 0.82 if inrange(s1aq03,17.0,17.9) & s1aq01==2
replace ae = 0.83 if inrange(s1aq03,18.0,18.9) & s1aq01==2
replace ae = 0.78 if inrange(s1aq03,19.0,29.9) & s1aq01==2
replace ae = 0.80 if inrange(s1aq03,30.0,59.9) & s1aq01==2
replace ae = 0.71 if inrange(s1aq03,60.0,150) & s1aq01==2

replace ae = 0.22 if inrange(s1aq03,00,00) & s1aq01==1
replace ae = 0.32 if inrange(s1aq03,1.0,1.9) & s1aq01==1
replace ae = 0.39 if inrange(s1aq03,2.0,2.9) & s1aq01==1
replace ae = 0.43 if inrange(s1aq03,3.0,3.9) & s1aq01==1
replace ae = 0.47 if inrange(s1aq03,4.0,4.9) & s1aq01==1
replace ae = 0.51 if inrange(s1aq03,5.0,5.9) & s1aq01==1
replace ae = 0.55 if inrange(s1aq03,6.0,6.9) & s1aq01==1
replace ae = 0.59 if inrange(s1aq03,7.0,7.9) & s1aq01==1
replace ae = 0.64 if inrange(s1aq03,8.0,8.9) & s1aq01==1
replace ae = 0.69 if inrange(s1aq03,9.0,9.9) & s1aq01==1
replace ae = 0.71 if inrange(s1aq03,10.0,10.9) & s1aq01==1
replace ae = 0.76 if inrange(s1aq03,11.0,11.9) & s1aq01==1
replace ae = 0.83 if inrange(s1aq03,12.0,12.9) & s1aq01==1
replace ae = 0.90 if inrange(s1aq03,13.0,13.9) & s1aq01==1
replace ae = 0.96 if inrange(s1aq03,14.0,14.9) & s1aq01==1
replace ae = 1.00 if inrange(s1aq03,15.0,15.9) & s1aq01==1
replace ae = 1.04 if inrange(s1aq03,16.0,16.9) & s1aq01==1
replace ae = 1.07 if inrange(s1aq03,17.0,17.9) & s1aq01==1
replace ae = 1.08 if inrange(s1aq03,18.0,18.9) & s1aq01==1
replace ae = 1.02 if inrange(s1aq03,19.0,29.9) & s1aq01==1
replace ae = 1.00 if inrange(s1aq03,30.0,59.9) & s1aq01==1
replace ae = 0.82 if inrange(s1aq03,60.0,150) & s1aq01==1

assert ae!=.
generate seq=_n
bysort hhold: egen nad=total(ae)
sort seq
drop seq

keep hhold nad
sort hhold
quietly by hhold: gen dup = cond(_N==1,0,_n)
drop if dup>1

keep hhold nad









