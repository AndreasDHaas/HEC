	
	***************************************************************************************************************************************************************
	***HIV status among HIV exposed children: a) observed, b) perfect testing & perfect retention
	***************************************************************************************************************************************************************
				
		/////////////////////////////////////////////////////////
		/// Prepare HIV testing data  
		////////////////////////////////////////////////////////
		
		*** Number of children in analysis 
			use $hec/hcc_states, clear
			count
			global N = `r(N)'
			di $N
		
		*** Combine Rapid antibody test and PCR tests 
		
			** Rapid antibody test 
				
				*use date 
					use $hec/rat1, clear
			
				*drop tests taken under age of 1 year -> likely to be false positive due to maternal antibodies
					*spikeplot rat_age 
					drop if rat_age <367.5
					
				*age_d
					rename rat_age age_d
				
				*age_w 
					gen age_w = round(age_d/7)
				
				*ratres
					tab ratres
					drop if ratres ==3
					gen HIV = ratres-1 
					
				*ratdate
					rename ratdate tdate
					
				*list 
					*list id dob tdate age_d age_w ratres HIV, sepby(id) header(20)
				
				*test id 
					gen test_id = 2
					lab define test_id 1 "PCR" 2 "RAT", replace 
					lab val test_id test_id 
					tab test_id
				
				*Clean 
					keep id tdate age_d age_w test_id HIV
					
				*Save 
					save $hec/rat_cleaned, replace 	
					
			** PCR 
				
				*use date 
					use $hec/pcr1, clear
								
				*age_d
					rename pcr_age age_d
				
				*age_w 
					gen age_w = round(age_d/7)
				
				*pcrres
					tab pcrres
					gen HIV = pcrres-1 
					
				*pcrdate
					rename pcrdate tdate
					
				*List 
					*list id tdate age_d age_w pcrres HIV, sepby(id) header(20)
				
				*Test id 
					gen test_id = 1
					lab define test_id 1 "PCR" 2 "RAT", replace 
					lab val test_id test_id 
					tab test_id
					
				*Drop after week 156
					drop if age_w >156
				
				*Clean 
					keep id tdate age_d age_w test_id HIV
					
				*Save 
					save $hec/pcr_cleaned, replace 	
					
			**PHSD
			
				*Open baseline date 
					use $hec/analyse1, clear

				*Keep only patients on ART with PSHD or confirmed positive accoriding to visits table 
					keep if pshd_d !=. 
					
				*Merge HIV diagnosis table 
					mmerge id using $hec/HIV, unmatched(master)
					
				*Merk children diagnosed with HIV according to testing data 
					bysort id (day): egen temp = max(HIV)
					
				*Keep only children not diagnosed according to testing date 
					keep if temp !=1
					drop temp

				*Age at PSHD diagnosis 
					gen pshd_day = pshd_d - dob

				*List 
					sort id day
					*list id day HIV test_id pshd_day, sepby(id)
					
				*Patient with PSHD but tested HIV- thereafter -> not HIV+ (eg. "ST_HEC_11_183_0159")
					gen temp = 1 if pshd_day !=. & HIV ==0 & day >= pshd_day 
					bysort id (day): egen temp1 = max(temp)
					*list id day HIV test_id pshd_day temp temp1 , sepby(id)
					drop if temp1==1
					drop temp*
					
				*Keep only patients with PSHD
					keep if pshd_day !=. 
					
				*List 
					*list id day HIV test_id pshd_day, sepby(id)
				
				*Generate dataset 
					replace HIV =1
					replace test_id = 3
					rename pshd_day age_d
					keep id age_d HIV test_id 
					gen age_w = round(age_d /7)
				
				*Save 
					save $hec/pshd, replace 
					list if id =="ST_HEC_13_243_00685", sepby(id)
		
			** Combine 
			
				*Append
					append using $hec/rat_cleaned
					append using $hec/pcr_cleaned
				
				*Lab 
					lab def test_id 3 "PSHD", modify
					tab test_id
				
				*Keep PCR or positive RAT if several tests per week
					gen sort = HIV *-1
					bysort id age_w (test_id sort): gen temp = _N
					*list if temp >1, sepby(id)
					bysort id age_w (test_id sort): keep if _n ==1
					
				*Clean 
					drop sort temp
					rename age_d day
					rename age_w week
					
				*Drop second HIV+ tests to avoid double counting of children 
					bysort id HIV (day): gen temp =_N if HIV ==1
					bysort id HIV (day): gen temp1 =_n if HIV ==1
					drop if temp >1 & temp !=. & temp1 >1 & temp1 !=. 
					*list if temp >1 & temp !=. , sepby(id) 
					drop temp*
					
				*List 
					sort id day
					*list in 1/30, sepby(id)
					
				*keep only tests children who are included in study 
					mmerge id using $hec/hcc_states, ukeep(id) unmatched(none)
					unique id
					
				*Drop test after 2.5 years 
					drop if week > 130 & week !=. 
					
				*Clean 
					drop _merge 
					
				*Checks 
					assert id !="" & week !=. & inlist(HIV, 0, 1) & inlist(test_id, 1, 2, 3)
					assert week >= 0 & week <=130
					
				*List 
					*list in 1/10, sepby(id)
					
				*Time  
					gen T = 1 if week <=52
					replace T = 2 if week >52 & week <=78
					replace T = 3 if week >78 & week <=130
					
				*Median test time
					bysort T: sum week, de
					
				*Keep only one per time // keep positive results 
					gen sort = HIV * -1
					bysort id T (sort test_id day): gen temp = _N
					*list if temp >1, sepby(id)
					bysort id T (sort test_id day): keep if _n ==1
					bysort id (week): gen temp3 = _N 
					assert temp3 <=3 
					drop temp* sort T
													
				*Save 
					save $hec/HIV, replace
					list if id =="ST_HEC_13_243_00685", sepby(id)
								
		*** Generate matrix (one line per child) with indicator for testing in at at T1, T2, and T3 
								
				*Add observations for children without test result  
					mmerge id using $hec/hcc_states, ukeep(id) unmatched(using)
					drop _merge
				
				*Assert that all children are in data 
					preserve 
					bysort id (week): keep if _n ==1
					count 
					assert `r(N)' == $N
					restore 
				
				*List 
					sort id day 
					list id day week test_id HIV if id =="ST_HEC_13_243_00685", sepby(id)
				
				** Mark tests that were taken in T1, T2, and T3
				
					*T1
						gen temp1 = 1 if week <=52 
						bysort id (day): egen T1 = max(temp1)
						replace T1 = 0 if T1 ==. 
						
					*T2 
						gen temp2 = 1 if week >52 & week <=78 
						bysort id (day): egen T2 = max(temp2)
						replace T2 = 0 if T2 ==. 
						
					*T3 
						gen temp3 = 1 if week >78 & week <=130 
						bysort id (day): egen T3 = max(temp3)
						replace T3 = 0 if T3 ==. 
					
				*List 
					list if id =="ST_HEC_13_243_00685", sepby(id) 
									
				*Keep only one row per child 
					bysort id (day): keep if _n ==1
				
				*Clean 
					keep id T* 
					
				*List 
					*list in 1/10, sepby(id)
					
				*Checks 
					count
					assert `r(N)' == $N
					assert T1 !=. & T2 !=. & T3 !=. 
					bysort id: gen temp =_N 
					assert temp ==1
					drop temp
					
				*Save
					save $hec/tested, replace 	
								
		*** Generate wide dataseet (one row per child) with time since last HIV (e.g. tst1) test and week of testing (e.g. week1).  
				// One variable per HIV test 
				// Children who have not been tested in each time interval (T1-T3) have artificial testing times (set to week 6, 53, and 105 for T1, T2, and T3, respectively): 
				// One dummy variable at the end of follow-up (week 130) 

				   // +--------------------------------------------------------------------------------------------------------------------------------------------+
				   // |                 id   week0   week1   tst0   tst1   week2   tst2   week3   tst3   week4   tst4   week5   tst5   week6   tst6   T1   T2   T3 |
				   // |--------------------------------------------------------------------------------------------------------------------------------------------|
				   // | MH_HEC_12_213_1018       0       6      0      6      53     47     105     52     130     25       .      .       .      .    0    0    0 |
				   // +--------------------------------------------------------------------------------------------------------------------------------------------+
				
				*Merge test results
					mmerge id using $hec/HIV, unmatched(master)
				
				*Assert that all children are in data 
					preserve 
					bysort id (week): keep if _n ==1
					count 
					assert `r(N)' == $N
					restore 
					
				*Clean 
					keep id T* week 
				
				*Ensure that children who have died have an observation in the week of death (end of their follow-up)
					
					*Row number
						bysort id (week): gen temp =_n 

					*merge week of death
						mmerge id using $hec/hcc_states, ukeep(dead_t) uif(dead_s ==1)
						replace dead_t = round(dead_t * 30/7)
						
					*Add row for death week 
						list if id =="ZA_HEC_13_421_3618", sepby(id)
						expand 2 if dead_t !=. & temp ==1, gen(d)
						replace week = dead_t if d ==1
						bysort id week: keep if _n ==1
						drop d _merge temp
						
						
					** Mark rows were in T1, T2, T3
				
						*T1
							gen temp1 = 1 if week <=52 
							bysort id (week): egen R1 = max(temp1)
							replace R1 = 0 if R1 ==. 
							
						*T2 
							gen temp2 = 1 if week >52 & week <=78 
							bysort id (week): egen R2 = max(temp2)
							replace R2 = 0 if R2 ==. 
							
						*T3 
							gen temp3 = 1 if week >78 & week <=130 
							bysort id (week): egen R3 = max(temp3)
							replace R3 = 0 if R3 ==. 
									
				*Add dummy rows if testing was not done 
					
					*Row number
						bysort id (week): gen temp =_n 
					
					*At week 6 if patients were not tested in T1
						expand 2 if R1 ==0 & temp ==1, gen(d)
						replace week = 6 if d ==1 
						replace temp = . if d ==1
						drop d
						
					*At week 52 if patients were not tested in T2
						expand 2 if R2 ==0 & temp ==1, gen(d)
						replace week = 53 if d ==1 
						replace temp = . if d ==1
						drop d
						
					*At week 104 if patients were not tested in T3
						expand 2 if R3 ==0 & temp ==1, gen(d)
						replace week = 105 if d ==1 
						replace temp = . if d ==1
						drop d 
						sort id week
						*list in 1/30, sepby(id) 
						
					*At week 130 for all
						expand 2 if temp ==1, gen(d)
						replace week = 130 if d ==1 
						drop d temp
						
					*Clean 
						drop if week ==. 
						
					*Drop duplicates
						bysort id week: keep if _n ==1
						
					*Assert that all patients have at least 4 rows
						bysort id (week): gen temp = _N 
						assert temp >=3 
						sum temp
						drop temp
						sum week
						assert week >=0 & week <=130
					
				*Spikeplot 
					*spikeplot week
										
				*List 
					sort id week 
					list if id =="MH_HEC_12_213_1018", sepby(id)	
					
				*Clean 
					drop R* temp*
					
				*Time since last test 
					bysort id (week): gen tst = week-week[_n-1] 
					bysort id (week): replace tst = week if tst ==. & _n ==1
					
				*Test number 
					bysort id (week): gen no = _n 
					
				*Wide 
					reshape wide week tst, i(id) j(no) 
					
				*tst0 dummy
					gen tst0 = 0, after(week1)
						
				*week0 dummy
					gen week0 = 0, before(week1) 
				
				*Assert that all children are in data 
					count 
					assert `r(N)' == $N
										
				*Save
					save $hec/tst, replace 
								
		/////////////////////////////////////////////////////////
		/// Prepare dataset for ART treatment status of mother
		////////////////////////////////////////////////////////
		
		*** Generate a long dataset with treatment status of mother observed at each visit 
		
			*Baseline data 
				use $hec/baseline1, clear
						
			*Clean 
				keep id dob mstatus enraged
				
			*Merge baseline covariates  
				mmerge id using $hec/hcc_states, ukeep(EA) unmatched(none)
			
			***Merge time-varying covariates: Treatment status of mother 
			
				*Merge
					mmerge id using $hec/mstatus, unmatched(master)
			
				*Vist date missing 
					assert visdate !=. 
								
				*Visit week 
				
					*Generate variable 
						gen week = round((visdate - dob)/7)
									
					*Make visit week unique 
						bysort id week: gen temp = _N
						sort id visdate
						tab temp
						*list id week dob visdate mstatus mstatusv if temp >1, sepby(id)
						bysort id week (mstatusv): keep if _n ==1
						
					*Assert that visit week is unique 
						capture drop temp
						bysort id week: gen temp = _N
						assert temp ==1
						drop temp
			
				*Treatment status at enrolment 
					
					*EA in weeks 
						replace EA = round(EA*30/7)
					
					*List 
						sort id week
						list id week mstatusv EA mstatus if id =="NE_HEC_13_093_01755", sepby(id) 
						
					*List patients without visit at enrolment 
						bysort id (week): gen temp=1 if EA != week & _n ==1
						bysort id (week): egen temp1=max(temp) 
						sort id week
						*list id week mstatusv EA mstatus temp temp1 if temp1==1, sepby(id)
						
					*Generate visit at enrolment 
						expand 2 if temp ==1, gen(temp2)
						sort id week
						*list id week mstatusv EA mstatus temp temp1 temp2 if temp1==1, sepby(id)
						replace week = EA if temp2 ==1 
						replace mstatusv = mstatus if temp2 ==1
						drop temp*
						
					*Assert that visit week is unique 
						capture drop temp*
						bysort id week (mstatusv): keep if _n ==1
						
					*Check week
						sum week
						drop if week <0
						drop if week >130
					
					*Clean 
						drop EA mstatus _merge enraged dob visdate
					
				*Treatment status
					
					*Unknown 
						replace mstatus = 4 if mstatus ==. 
					
					*List 
						*list id week mstatusv, sepby(id)
					
					*visit 
						bysort id (week): gen visit = _n
					
				*Checks 
					assert id !="" 
					assert mstatus !=.
					tab ms
					assert week !=. 
				
								
				*Assert that all children are in data 
					preserve 
					bysort id (week): keep if _n ==1
					count 
					assert `r(N)' == $N
					restore 
										
				*save
					save $hec/mstatus2, replace 
				
								
		/////////////////////////////////////////////////////////
		/// Prepare final analysis dataset 
		////////////////////////////////////////////////////////
		
		*** Prepare long dataset: 1 row per child and week
			// Treatment status of mother is carried forward
			// Indicators for HCC outcomes (LTF, death of child, not enrolled, discharged) and administrative censoring 
			
			*Baseline data 
				use $hec/hcc_states, clear
				
			*Assert that all children are in the dataset
				count 
				assert `r(N)' == $N
							
			*Clean 
				keep id AP AL AB ABC LOC SEX BW EA LTF60_s LTF60_t dis_s dis_t dead_s dead_t enrol_t ART_t ART_s	 	
			
			*Time in weeks
				foreach var in LTF60_t dis_t dead_t enrol_t ART_t { 
					replace `var' = round(`var'*30/7)
				}
				
			*Administrative censoring 
				gen admc_s = 0, after(dead_t)
				replace admc_s =1 if (LTF60_s ==0 & dead_s ==0 & dis_s ==0)
				gen admc_t = round(LTF60_t + 60/7), after(admc_s)
						
			*Expand (one row per week and child)
				expand 131
				
			*Week
				bysort id: gen week = _n -1
							
			*Week squared
				gen weeksq = week*week
				order id week weeksq
				*list id week weeksq, sepby(id)
			
		*** Treatment status of mother 
				
			*Merge treatment status of mother
				mmerge id week using $hec/mstatus2, unmatched(master)
				assert inlist(_merge, 1, 3)
				drop _merge
				
			*List 
				sort id week
				*list id week visit AP AL mstatusv, sepby(id)
				
			*Status at day 0 -> ART at delivery 
				replace mstatusv = 1 if (week == 0 & mstatusv ==. & inlist(AL, 0,1)) // no ART
				replace mstatusv = 2 if week == 0 & mstatusv ==. & AL ==2 // ART
				replace mstatusv = 4 if week == 0 & mstatusv ==. & AL ==3 // Unknown
				assert mstat !=. if week ==0
				tab mstatusv if week ==0, mi
				
			*Last observation carried forward
				bysort id (week): replace mstatusv = mstatusv[_n-1] if mstatusv ==. & week !=. 
			
			*List 
				sort id week
				*list id week visit LOC SEX BW AB ABC AP AL mstatusv if id =="BE_HEC_12_947_0744", sepby(id)
			
			*Died
				
				*Mark 
					gen temp = 1 if mstatus ==3
					bysort id (week): egen temp1 = max(temp)
					*list id week visit LOC SEX BW AB ABC AP AL mstatusv temp temp1 if temp1==1, sepby(id)
					
				*Set status after death to status before death (carry forward last observed status). 
					bysort id (week): egen temp2 = min(week) if temp ==1
					bysort id (week): egen temp3 = min(temp2)
					list id week visit LOC SEX BW AB ABC AP AL mstatusv temp* if id =="ZA_HEC_14_161_4303", sepby(id)
					gen temp4 = mstatusv if week == temp3-1
					bysort id (week): egen temp5 = max(temp4)
					replace mstatusv = temp5 if week > temp3
					
				*Clean 
					capture drop temp*
		
		*** Status of child 
		
			*LTF
				
				*List 
					*list id week visit LOC SEX BW AB ABC AP AL mstatusv LTF60_s LTF60_t if id =="BE_HEC_12_433_0893", sepby(id)
				
				*No missings 
					assert week !=. & LTF60_t !=. & LTF60_s !=. 
				
				*Gen indicator 
					gen LTF = 0
					replace LTF =1 if (week > LTF60_t & LTF60_s ==1) 
				
				*List 
					list id week visit LOC SEX BW AB ABC AP AL mstatusv LTF60_s LTF60_t LTF, sepby(id)
				
			*Discharged 
				
				*List 
					*list id week visit LOC SEX BW AB ABC AP AL mstatusv dis_s dis_t if id =="ZA_HEC_13_465_3478", sepby(id)
				
				*No missings 
					assert week !=. & dis_t !=. & dis_s !=. 
				
				*Gen indicator 
					gen DIS = 0
					replace DIS =1 if (week > dis_t & dis_s ==1) 
				
				*List 
					*list id week visit LOC SEX BW AB ABC AP AL mstatusv dis_s dis_t DIS if id =="ZA_HEC_13_465_3478", sepby(id)
					*list id week visit LOC SEX BW AB ABC AP AL mstatusv dis_s dis_t DIS, sepby(id)
			
			*Dead 
				
				*List 
					*list id week visit LOC SEX BW AB ABC AP AL mstatusv dead_s dead_t if id =="MG_HEC_11_459_0246", sepby(id)
				
				*No missings 
					assert week !=. & dead_t !=. & dead_s !=. 
				
				*Gen indicator 
					gen DD = 0
					replace DD =1 if (week > dead_t & dead_s ==1) 
				
				*List 
					list id week visit LOC SEX BW AB ABC AP AL mstatusv dead_s dead_t DD if id =="MG_HEC_11_459_0246", sepby(id)
					*list id week visit LOC SEX BW AB ABC AP AL mstatusv dis_s dis_t DIS, sepby(id)
					
			*Not enrolled
				*List 
					*list id week visit LOC SEX BW AB ABC AP AL mstatusv enrol_t if id =="MG_HEC_11_459_0246", sepby(id)
				
				*No missings 
					assert week !=. & enrol_t !=.  
				
				*Gen indicator 
					gen NE = 1
					replace NE =0 if (week >= enrol_t) 
				
				*List 
					*list id week visit LOC SEX BW AB ABC AP AL mstatusv enrol_t NE, sepby(id)
			
			*Administrative censoring 
				
				*List 
					list id week visit enrol_t LTF60_s LTF60_t dis_s dis_t dead_s dead_t admc_s admc_t admc_t admc_s NE DD DIS LTF if id =="MG_HEC_11_459_0246", sepby(id)
				
				*No missings 
					assert week !=. & admc_t !=. & admc_s !=. 
				
				*Gen indicator 
					gen AC = 0
					replace AC =1 if (week >= admc_t & admc_s ==1) 
				
				*List 
					list id week visit LOC SEX BW AB ABC AP AL mstatusv LTF* if id =="ZA_HEC_14_193_4122", sepby(id)
					*list id week visit LOC SEX BW AB ABC AP AL mstatusv dis_s dis_t DIS, sepby(id)
					
		*** Treatment status of mother (assuming that mothers of children LTF are off ART). 
				
				*Women are LTF 4 or 8 weeks after last visit of child (assuming they refilled medication at last visit and took all collected ARVs)
					gen temp = LTF60_t + 4 if LTF60_t < 26
					replace temp = LTF60_t + 8 if LTF60_t >= 26
					list id week visit LOC SEX BW AB ABC AP AL mstatusv LTF* temp if id =="ZA_HEC_14_193_4122", sepby(id)
					gen mstatusv1 = mstatusv
					lab val mstatusv1 mstatusv_
					tab mstatusv1
					replace mstatusv1 = 1 if LTF60_s ==1 & week >=temp & week !=. 
					list id week visit LOC SEX BW AB ABC AP AL mstatusv* LTF* temp if id =="ZA_HEC_14_193_4122", sepby(id)
					drop temp		

		*** HIV status of child 
				
				*Merge
					mmerge id week using $hec/HIV, unmatched(master) ukeep(HIV)
					
				*List 
					list id week visit AB ABC AP AL mstatusv NE LTF DIS DD AC HIV if id =="BE_HEC_12_221_9103" , sepby(id) header(157)
					*list id week visit AB ABC AP AL mstatusv NE LTF DIS DD AC HIV, sepby(id) header(157)
			
				*HIV missing if not tested 
					tab HIV, mi
					tab HIV
				
				*HIV_free (yes / no)
					gen HIV_free = 1-HIV
					tab HIV_f
						
				*hiv (0/1) no missings 
					gen hiv = 0 
					replace hiv =1 if HIV ==1
					tab hiv, mi
			
				*hiv_free (0/1) no missings 
					gen hiv_free = 1-hiv
					tab hiv_free, mi
								
		*** Tested at t1, t2, t3
			mmerge id using $hec/tested, unmatched(master)
			assert _merge ==3
			drop _merge 
				
		*** Censor patients at the first HIV diagnosis and at death
				
			*Children who have died 
				list id week visit AB ABC AP AL mstatusv NE LTF DIS DD AC HIV if id =="MG_HEC_13_293_1950" , sepby(id) header(157)
				gen elig = 1
				replace elig = 0 if DD ==1 // dead 
					
			*HIV+
				bysort id (week): gen temp = week if HIV ==1
				bysort id (week): egen temp1 = min(temp) 
				replace elig = 0 if week > temp1 & temp1 !=.  
				list id week visit HIV temp temp1 elig if id =="SA_HEC_12_141_0794" , sepby(id) header(157) 
				*list id week visit HIV temp temp1 elig, sepby(id) header(157) 
				drop temp temp1
					
		*** Treatment status of mother in the time since last HIV test  
					
			*Merge time since last test 
				mmerge id using $hec/tst, unmatched(master)
				
			*** Set week to max week a child was eligible 
	
			*List 
				list id week visit week mstatusv HIV week1 week2 week3 week4 tst1 tst2 tst3 tst4 elig if id =="ZA_HEC_13_341_3530", sepby(id)         // week 59 
			
			*Dummy with 1 
				gen dummy = 1
											
			** Percentage of weeks on ART: last status recorded in mstatusv carried forward for women LTF 
					
				*Generate variables (ms2_1 = percentage of weeks with mstatusv ==2 between birth and the first test (tst1)
					forval i = 1/4 { // loop over tests `i'
						local k = `i' - 1 
						di "Begin test `i'"
						di "Previous test `k'" 
						bysort id (week): egen m2_`i' = total(dummy) if mstatusv == 2 & week >= week`k' & week < week`i' // N with mstatusv == x 
						replace m2_`i' = m2_`i'/tst`i'*100 // percentage of weeks with mstatusv ==x 
						bysort id (week): egen ms2_`i' = max(m2_`i') // copy to long
						replace ms2_`i' = . if week != week`i' // keep only in observation where test was done 
						drop m2_`i' // clean 
					}
							
				*Check 
					list id week visit HIV mstatusv week1 week2 week3 week4 ms* if id =="BE_HEC_11_003_0677", sepby(id) // time1
					list id week visit HIV mstatusv week1 week2 week3 week4 ms* if id =="MH_HEC_12_213_1018", sepby(id) // time1
													
				*Use ART status at labour for children who were tested in week 0 
					replace ms2_1 = 100 if week == 0 & tst1 == 0 & AL == 2 // on ART
								
				*Missings to 0					
					forval i = 1/4 {
							replace ms2_`i' = 0 if ms2_`i' ==. & week ==week`i'
					}
				
				*Pool dummy variable for tests in one variable: maternal status 
					egen temp = rownonmiss(ms2_1 ms2_2 ms2_3 ms2_4)
					assert temp <2
					drop temp 
					egen ms2 = rowmin(ms2_1 ms2_2 ms2_3 ms2_4)
					drop ms2_*
												
				*List 
					list id week AP AL AB ABC LOC ms2 visit HIV mstatusv if id =="QE_HEC_12_629_0856", sepby(id) 
					list id week HIV mstatusv elig if id =="ZA_HEC_13_465_3478", sepby(id) 

		
			** Percentage of weeks on ART: maternal ART status set to off ART when child is LTF 
					
				*Generate variables (ms2_1 = percentage of weeks with mstatusv ==2 between birth and the first test (tst1)
					forval i = 1/4 { // loop over tests `i'
						local k = `i' - 1 
						di "Begin test `i'"
						di "Previous test `k'" 
						bysort id (week): egen m2_`i' = total(dummy) if mstatusv1 == 2 & week >= week`k' & week < week`i' // N with mstatusv == x 
						replace m2_`i' = m2_`i'/tst`i'*100 // percentage of weeks with mstatusv ==x 
						bysort id (week): egen ms2_`i' = max(m2_`i') // copy to long
						replace ms2_`i' = . if week != week`i' // keep only in observation where test was done 
						drop m2_`i' // clean 
					}
							
				*Check 
					list id week visit HIV mstatusv1 week1 week2 week3 week4 ms* if id =="BE_HEC_11_003_0677", sepby(id) // time1
					list id week visit HIV mstatusv1 week1 week2 week3 week4 ms* if id =="MH_HEC_12_213_1018", sepby(id) // time1
													
				*Use ART status at labour for children who were tested in week 0 
					replace ms2_1 = 100 if week == 0 & tst1 == 0 & AL == 2 // on ART
								
				*Missings to 0					
					forval i = 1/4 {
							replace ms2_`i' = 0 if ms2_`i' ==. & week ==week`i'
					}
				
				*Pool dummy variable for tests in one variable: maternal status 
					egen temp = rownonmiss(ms2_1 ms2_2 ms2_3 ms2_4)
					assert temp <2
					drop temp 
					egen ms2_ltf = rowmin(ms2_1 ms2_2 ms2_3 ms2_4)
												
				*List 
					list id week week1-week4 visit HIV mstatusv1 ms2_l if id =="QE_HEC_12_629_0856", sepby(id) 
				
				*Clean 
					drop ms2_1 ms2_2 ms2_3 ms2_4
					
			** Indicator for death of mother prior to or at testing
				gen temp = 1 if mstatusv ==3
				bysort id (week): egen temp2 = min(week) if temp ==1
				bysort id (week): egen temp3 = min(temp2)
				bysort id (week): gen md = 1 if week >= temp3 & temp3 !=. 
				drop temp*	
				replace md = 0 if md ==. 
				
			** Save
				save $hec/temp, replace
				use $hec/temp, clear
				
			** Add restricted cubic splines: spline location = testing date - 2 weeks to testing date + visit frequency + 2 weeks
				mkspline weeksp = week, cubic displayknots knots(0 4 10 50 64 102 116)
					
			*Save temp
				save $hec/HIV_long, replace 
		
		///////////////////////////////////////////////////////////
		///Cumulative number of HIV+ children 
		///////////////////////////////////////////////////////////
		
		*** Use
			use $hec/HIV_long, clear 
			keep if HIV ==1
			keep id week HIV
			unique id
			collapse (sum)HIV, by(week)
			sort week
			gen cum_N = HIV if _n ==1
			replace cum_N = HIV + cum_N[_n-1] if _n !=1
			rename HIV N
			save $hec/cum_N, replace
					
		//////////////////////////////////////////////////////////////////////////////
		/// Calcualte inverse probability weights  (adjusted for ltf & not testing
		//////////////////////////////////////////////////////////////////////////////
		
		*** Use
			use $hec/HIV_long, clear
		
		*** Prepare long dataset: 1 row per test or dummy test (each child has a least 3 rows)
		
		*** Predicting HIV status 
				*logistic HIV i.AP i.AB i.LOC ms2_ltf i.T1 i.T2 
				*logistic HIV week weeksp* i.AP i.AL i.AB i.ABC i.LOC ms2_ltf i.md i.T1 i.T2
					
			*Keep only rows per test or dummy test 
				keep if ms2_ltf !=.
			
			*Observatoin included T 
				gen T = 1 if elig ==1 & week <=52
				replace T = 2 if elig ==1 & week >52 & week <=78
				replace T = 3 if elig ==1 & week >78 & week <=130
				bysort id T (week): gen temp = _n
		
			*List 
				list id week AP AL ms2_ltf md AB ABC LOC T1 T2 T3 HIV elig DD T temp if id =="ZA_HEC_13_341_3530" // inelig in T3
				list id week T HIV, sepby(id)
								
			*List 
				list id week AP AL ms2_ltf md AB ABC LOC T1 T2 T3 HIV elig T if id =="QE_HEC_12_629_0856"
				
		/// Inverse prbability weights for T1
											
			*Estimate probability of being tested at T1
				logistic T1 i.AP i.AL ms2_ltf i.md i.AB i.ABC i.LOC if T==1 
				*logistic T1 i.AP ms2_ltf i.AB i.LOC if T==1 
				predict pT1 if e(sample)
				
			*Calculate IPCW for HIV test among children in care 
				gen wT1 = 1/pT1 				// calculate IPCW
				replace wT1 = 0 if T1 !=1 		// weights are 0 for children who were not tested 
				replace wT1 = . if T !=1		// weights are . for obs in period 2 and 3 
				list id week T1 AP AL AB ABC LOC HIV pT1 wT1 if id =="NE_HEC_13_411_01756", sepby(id) // tested in T1
				list id week T1 AP AL AB ABC LOC HIV pT1 wT1 if id =="BE_HEC_12_053_1296", sepby(id)  // not tested in T1
				sum wT1	// mean of weights 
				assert `r(mean)' > 0.99 &  `r(mean)' < 1.01  // assert mean is close to 1 
				replace wT1 = wT1 * 1/`r(mean)' // correct 
				sum wT1 // mean is 1 
				
		/// Inverse prbability weights for T2
				
			*Estimate probability of being tested at T2
				logistic T2 i.AP i.AL ms2_ltf i.md T1 i.AB i.ABC i.LOC if T==2
				*logistic T2 i.AP ms2_ltf i.AB i.LOC T1 if T==2
				predict pT2 if e(sample)
				
			*Calculate IPCW for HIV test among children in care 
				gen wT2 = 1/pT2 				// calculate IPCW
				replace wT2 = 0 if T2 !=1 		// weights are 0 for children who were not tested 
				replace wT2 = . if T !=2		// weights are . for obs in period 2 and 3 
				list id week T2 AP AL AB ABC LOC HIV pT2 wT2 if id =="NE_HEC_13_411_01756", sepby(id) // tested in T2
				list id week T2 AP AL AB ABC LOC HIV pT2 wT2 if id =="BE_HEC_12_053_1296", sepby(id)  // not tested in T2
				sum wT2	// mean of weights 
				assert `r(mean)' > 0.99 &  `r(mean)' < 1.01  // assert mean is close to 1 
				replace wT2 = wT2 * 1/`r(mean)' // correct 
				sum wT2 // mean is 1 
								
		/// Inverse prbability weights for T3
				
			*Estimate probability of being tested at T3
				logistic T3 i.AP i.AL ms2_ltf i.md T1 T2 i.AB i.ABC i.LOC if T ==3
				*logistic T3 i.AP ms2_ltf i.AB i.LOC T1 T2 if T ==3 
				predict pT3 if e(sample)
				
			*Calculate IPCW for HIV test among children in care 
				gen wT3 = 1/pT3 				// calculate IPCW
				replace wT3 = 0 if T3 !=1 		// weights are 0 for children who were not tested 
				replace wT3 = . if T !=3		// weights are . for obs in period 2 and 3 
				list id week T3 AP AL AB ABC LOC HIV ms2_ltf pT3 wT3 if id =="ZA_HEC_14_177_4064", sepby(id) // tested in T1
				list id week T3 AP AL AB ABC LOC HIV ms2_ltf pT3 wT3 if id =="BE_HEC_12_053_1296", sepby(id)  // not tested in T1
				sum wT3	 // mean of weights 
				assert `r(mean)' > 0.92 &  `r(mean)' < 1.01  // assert mean is close to 1 
				replace wT3 = wT3 * 1/`r(mean)' // correct 
				sum wT3 // mean is 1 
								
		/// Final weights 
		
			*List 
				list id week visit T1 T2 T3 wT1 wT2 wT3 if id =="ZA_HEC_14_087_3565" , sepby(id) header(157)
			
			*Truncnate at 99ths percentile 
				sum wT1 if wT1 !=0, de
				sum wT2 if wT2 !=0, de
				sum wT3 if wT3 !=0, de
				replace wT3 = `r(p99)' if wT3 > `r(p99)' & wT3 !=. 
			
			*Weights
				gen wALL = wT1 if week <=52
				replace wALL = wT2 if week >52 & week <=78
				replace wALL = wT3 if week >78 & week <=130 
	
			*List 
				list id week visit T1 T2 T3 wT1 wT2 wT3 wALL if id =="ZA_HEC_13_341_3530" , sepby(id) header(157)
				list id week visit T1 T2 T3 wT1 wT2 wT3 wALL if id =="BE_HEC_12_053_1296" , sepby(id) header(157)
			
			*clean 
				keep id week wALL T elig
				
			*missings 
				gen temp =1 if wALL ==. 
				bysort id: egen temp1 = max(temp)
				*list if temp1 ==1, sepby(id)  header(40)
				assert wALL !=. if elig ==1
				
			*Clean 
				drop elig temp*
								
			***Save 
				save $hec/weights, replace
				

				
				
		/// At risk 
		
			*T1
				count if T ==1			// total at risk 
				global arT1 = `r(N)' 
				total wALL if T==1     // total over weights 
				
			*T2
				count if T ==2			// total at risk 
				global arT2 = `r(N)' 
				total wALL if T==2     // total over weights 
				
			*T2
				count if T ==3			// total at risk 
				global arT3 = round(`r(N)'/2) 
				di $arT3
				total wALL if T==3     // total over weights / 2 becasue T3 has 2 rows for each patient
														

						
		//////////////////////////////////////////////////////////////////////////////
		/// Calcualte inverse probability weights  (adjusted for not testing)
		//////////////////////////////////////////////////////////////////////////////
		
		*** Use
			use $hec/HIV_long, clear
		
		*** Prepare long dataset: 1 row per test or dummy test (each child has a least 3 rows)
		
		*** Predicting HIV status 
				*logistic HIV i.AP i.AB i.LOC ms2_ltf i.T1 i.T2 
				*logistic HIV week weeksp* i.AP i.AL i.AB i.ABC i.LOC ms2_ltf i.md i.T1 i.T2
					
			*Keep only rows per test or dummy test 
				keep if ms2_ltf !=.
			
			*Observatoin included T 
				gen T = 1 if elig ==1 & week <=52 
				replace T = 2 if elig ==1 & week >52 & week <=78 
				replace T = 3 if elig ==1 & week >78 & week <=130 
				bysort id T (week): gen temp = _n
				
			*Under follow-up
			
				*Fup1
					gen fup1 = 0 if elig == 1 & week <= 52
					replace fup1 = 1 if (elig == 1 & week <= 52 & enrol_t <=52 & LTF60_s ==0) | (elig == 1 & week <= 52 & enrol_t <=52 & LTF60_s ==1 & LTF60_t >6) | (T1 ==1 & elig == 1 & week <= 52)
					tab fup1
				
				*Fup2
					gen fup2 = 0 if elig == 1 & week >52 & week <=78 & enrol_t <=78
					replace fup2 = 1 if (elig == 1 & week >52 & week <=78 & enrol_t <=78 & LTF60_s ==0) | (elig == 1 & week >52 & week <=78 & enrol_t <=78 & LTF60_s ==1 & LTF60_t >52) | (T2 ==1 & elig == 1 & week >52 & week <=78)
					tab fup2
				
				*Fup3
					gen fup3 = 0 if elig == 1 & week >78 & week <=130  & enrol_t <=130
					replace fup3 = 1 if (elig == 1 & week >78 & week <=130 & enrol_t <=130 & LTF60_s ==0) | (elig == 1 & week >78 & week <=130 & enrol_t <=130 & LTF60_s ==1 & LTF60_t >104) | (T3 ==1 & elig == 1 & week >78 & week <=130)
					tab fup3 if week !=130
				
			*Uptake of testing among children under follow-up
				tab fup1 T1, row
				tab fup2 T2, row
				tab fup3 T3, row
				
			*Percentage tested 
				count if T1 ==1 & elig ==1 & T ==1
				count if T1 ==1 & elig ==1 & T ==.
		
			*List 
				list id week AP AL ms2_ltf md AB ABC LOC T1 T2 T3 HIV elig DD T temp enrol_t LTF60_s LTF60_t LTF if id =="ZA_HEC_14_177_4064" // inelig in T3
				list id week T HIV, sepby(id)
								
			*List 
				list id week AP AL ms2_ltf md AB ABC LOC T1 T2 T3 HIV elig T if id =="QE_HEC_12_629_0856"
				
		/// Inverse prbability weights for T1
											
			*Estimate probability of being tested at T1
				logistic T1 i.AP i.AL ms2_ltf i.md i.AB i.ABC i.LOC if T==1 
				*logistic T1 i.AP ms2_ltf i.AB i.LOC if T==1 
				predict pT1 if e(sample)
				
			*Calculate IPCW for HIV test among children in care 
				gen wT1 = 1/pT1 				// calculate IPCW
				replace wT1 = 0 if T1 !=1 		// weights are 0 for children who were not tested 
				replace wT1 = . if T !=1		// weights are . for obs in period 2 and 3 
				list id week T1 AP AL AB ABC LOC HIV pT1 wT1 if id =="NE_HEC_13_411_01756", sepby(id) // tested in T1
				list id week T1 AP AL AB ABC LOC HIV pT1 wT1 if id =="BE_HEC_12_053_1296", sepby(id)  // not tested in T1
				sum wT1	// mean of weights 
				assert `r(mean)' > 0.99 &  `r(mean)' < 1.01  // assert mean is close to 1 
				replace wT1 = wT1 * 1/`r(mean)' // correct 
				sum wT1 // mean is 1 
				
		/// Inverse prbability weights for T2
				
			*Estimate probability of being tested at T2
				logistic T2 i.AP i.AL ms2_ltf i.md T1 i.AB i.ABC i.LOC if T==2
				*logistic T2 i.AP ms2_ltf i.AB i.LOC T1 if T==2
				predict pT2 if e(sample)
				
			*Calculate IPCW for HIV test among children in care 
				gen wT2 = 1/pT2 				// calculate IPCW
				replace wT2 = 0 if T2 !=1 		// weights are 0 for children who were not tested 
				replace wT2 = . if T !=2		// weights are . for obs in period 2 and 3 
				list id week T2 AP AL AB ABC LOC HIV pT2 wT2 if id =="NE_HEC_13_411_01756", sepby(id) // tested in T2
				list id week T2 AP AL AB ABC LOC HIV pT2 wT2 if id =="BE_HEC_12_053_1296", sepby(id)  // not tested in T2
				sum wT2	// mean of weights 
				assert `r(mean)' > 0.99 &  `r(mean)' < 1.01  // assert mean is close to 1 
				replace wT2 = wT2 * 1/`r(mean)' // correct 
				sum wT2 // mean is 1 
								
		/// Inverse prbability weights for T3
				
			*Estimate probability of being tested at T3
				logistic T3 i.AP i.AL ms2_ltf i.md T1 T2 i.AB i.ABC i.LOC if T ==3
				*logistic T3 i.AP ms2_ltf i.AB i.LOC T1 T2 if T ==3 
				predict pT3 if e(sample)
				
			*Calculate IPCW for HIV test among children in care 
				gen wT3 = 1/pT3 				// calculate IPCW
				replace wT3 = 0 if T3 !=1 		// weights are 0 for children who were not tested 
				replace wT3 = . if T !=3		// weights are . for obs in period 2 and 3 
				list id week T3 AP AL AB ABC LOC HIV ms2_ltf pT3 wT3 if id =="ZA_HEC_14_177_4064", sepby(id) // tested in T1
				list id week T3 AP AL AB ABC LOC HIV ms2_ltf pT3 wT3 if id =="BE_HEC_12_053_1296", sepby(id)  // not tested in T1
				sum wT3	 // mean of weights 
				assert `r(mean)' > 0.92 &  `r(mean)' < 1.01  // assert mean is close to 1 
				replace wT3 = wT3 * 1/`r(mean)' // correct 
				sum wT3 // mean is 1 
								
		/// Final weights 
		
			*List 
				list id week visit T1 T2 T3 wT1 wT2 wT3 if id =="ZA_HEC_14_087_3565" , sepby(id) header(157)
			
			*Truncnate at 99ths percentile 
				sum wT1 if wT1 !=0, de
				sum wT2 if wT2 !=0, de
				sum wT3 if wT3 !=0, de
				replace wT3 = `r(p99)' if wT3 > `r(p99)' & wT3 !=. 
			
			*Weights
				gen wALL = wT1 if week <=52
				replace wALL = wT2 if week >52 & week <=78
				replace wALL = wT3 if week >78 & week <=130 
	
			*List 
				list id week visit T1 T2 T3 wT1 wT2 wT3 wALL if id =="ZA_HEC_13_341_3530" , sepby(id) header(157)
				list id week visit T1 T2 T3 wT1 wT2 wT3 wALL if id =="BE_HEC_12_053_1296" , sepby(id) header(157)
			
			*clean 
				keep id week wALL T elig
				
			*missings 
				gen temp =1 if wALL ==. 
				bysort id: egen temp1 = max(temp)
				sort id week
				list if temp1 ==1, sepby(id)  header(40)
				replace wALL =1 if wALL ==. 
				assert wALL !=. if elig ==1 & T !=. 
				
			*Clean 
				drop elig temp*
								
			***Save 
				save $hec/weights_adj_for_testing, replace		
	
	
	
	
	
	
	
	
