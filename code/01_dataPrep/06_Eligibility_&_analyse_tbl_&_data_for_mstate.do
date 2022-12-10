	
	**********************************
	***VARIABLES AND ELIGIBILITY 
				
	use $hec/baseline2, clear
									
		***VARIABLES  
					
			*date of fisrt visit 
				mmerge id using $hec/followup1, unmatched(master) ukeep(visdate)
				bysort id (visdate): keep if _n ==1
				rename visdate fvis_d
						
			*date of last visit 
				mmerge id using $hec/followup1, unmatched(master) ukeep(visdate)
				replace visdate = . if visdate > close_d
				sort id visdate
					*list id visdate, sepby(id)
				replace visdate = 0 if visdate ==. 
				bysort id (visdate): keep if _n ==_N
				replace visdate = . if visdate == 0 
				rename visdate lastvis_d
				bysort facility: tab lastvis_d
								
			*date of first pcr
				mmerge id using $hec/pcr1, unmatched(master) ukeep(pcrdate)
				bysort id (pcrdate): keep if _n ==1
				rename pcrdate fpcr_d
						
			*date of last pcr
				mmerge id using $hec/pcr1, unmatched(master) ukeep(pcrdate)
				replace pcrdate = . if pcrdate > close_d
				replace pcrdate = 0 if pcrdate == . 
				bysort id (pcrdate): keep if _n ==_N
				replace pcrdate = . if pcrdate == 0 
				rename pcrdate lpcr_d
					
			*date of first rapid test
				mmerge id using $hec/rat1, unmatched(master) ukeep(ratdate ratres)
				bysort id (ratdate): keep if _n ==1
				rename ratdate frat_d
						
			*date of last rapid test
				mmerge id using $hec/rat1, unmatched(master) ukeep(ratdate ratres)
				replace ratdate = . if ratdate > close_d
				replace ratdate = 0 if ratdate == .  
				bysort id (ratdate): keep if _n ==_N
				replace ratdate = . if ratdate == 0 
				rename ratdate lrat_d
				rename ratres lrat_res
						
			*enrol_d 
				egen enrol_d = rowmin(fvis_d fpcr_d frat_d) 
				format enrol_d %td
				       *list id enrol_d fvis_d fpcr_d frat_d in 1/100
						
			*replace with enr_age if missing
				count if enrol_d ==. 
				replace enrol_d = dob + enraged if enrol_d ==.
								
			*Enrol_age
				gen enrol_age = enrol_d - dob   
				       *list enrol_d dob enrol_age enraged in 1/200
				drop enraged 
				tab enrol_age, mi
				recode enrol_age (0/42 = 1 "<6 weeks") (43/84 = 2 "6-12 weeks") (85/180 =3 "3-6 months") (181/max =4 ">6 months"), test gen(enrol_age_c)
			
			*Sex
				recode sex (1=1 "male") (2 = 2 "femal") (. = 99 "unknown"), gen(temp)
				tab sex temp, mi
				drop sex 
				rename temp sex
				assert sex !=.
			
			*year of birth  
				gen year = year(dob)
				tab year
				
			*birth weight
				recode bweight (min/2.5 =1 "<2.5") (2.5/max =2 ">=2.5") (.=3 "missing"), gen(b_weight)
				tab b_weight, mi
			
			*arvs during pregnancy
				tab arvpreg, nolab mi
				replace arvpreg = 6 if arvpreg ==. 
				assert arvpreg !=. 
				tab arvpreg
								
			*ARV during labour 
				tab arvlab, mi 
				tab arvlab, nolab mi
				recode arvlab (4=5) (.=6)
				tab arvlab, mi
									
			*Baby ARV at birth
				tab barvbirth, nolab mi
				tab barvbirth
				recode barvbirth (1=1 "None")  (2/4=2 "ARVs") (5=3 "Unknown") (.=3), gen(barv_birth)
				tab barv_birth, mi
									
			*Baby ARVs continued
				tab barvcont
				tab barvcont, nolab mi
				recode barvcont (1=1 "None")  (2/3=2 "ARVs") (4=3 "Unknown") (.=3), gen(barv_cont)
				tab barv_cont, mi	
				
			*N
				gen count =1
				
			*Birth cohort 
			
				*generating month 
					gen bc1 = mofd(dob)
					format bc1 %tm 
					tab bc1
				
				*lab bc1 
					lab define mth ///
					600 "2008 Jan" 577 "Feb" 578 "Mar" 579 "Apr" 580 "May" 581 "Jun" 582 "Jul" 583 "Aug" 584 "Sep" 585 "Oct" 586 "Nov" 587 "Dec" ///
					588 "2009 Jan" 589 "Feb" 590 "Mar" 591 "Apr" 592 "May" 593 "Jun" 594 "Jul" 595 "Aug" 596 "Sep" 597 "Oct" 598 "Nov" 599 "Dec" ///
					600 "2010 Jan" 601 "Feb" 602 "Mar" 603 "Apr" 604 "May" 605 "Jun" 606 "Jul" 607 "Aug" 608 "Sep" 609 "Oct" 610 "Nov" 611 "Dec" ///
					612 "2011 Jan" 613 "Feb" 614 "Mar" 615 "Apr" 616 "May" 617 "Jun" 618 "Jul" 619 "Aug" 620 "Sep" 621 "Oct" 622 "Nov" 623 "Dec" ///
					624 "2012 Jan" 625 "Feb" 626 "Mar" 627 "Apr" 628 "May" 629 "Jun" 630 "Jul" 631 "Aug" 632 "Sep" 633 "Oct" 634 "Nov" 635 "Dec" ///
					636 "2013 Jan" 637 "Feb" 638 "Mar" 639 "Apr" 640 "May" 641 "Jun" 642 "Jul" 643 "Aug" 644 "Sep" 645 "Oct" 646 "Nov" 647 "Dec" ///
					648 "2014 Jan" 649 "Feb" 650 "Mar" 651 "Apr" 652 "May" 653 "Jun" 654 "Jul" 655 "Aug" 656 "Sep" 657 "Oct" 658 "Nov" 659 "Dec" ///
					660 "2015 Jan" 661 "Feb" 662 "Mar" 663 "Apr" 664 "May" 665 "Jun" 666 "Jul" ///
					, replace 
					lab val bc1 mth 
					
				*lab bc1 
				lab define year ///
					600 "2008" 577 " " 578 " " 579 " " 580 " " 581 " " 582 " " 583 " " 584 " " 585 " " 586 " " 587 " " ///
					588 "2009" 589 " " 590 " " 591 " " 592 " " 593 " " 594 " " 595 " " 596 " " 597 " " 598 " " 599 " " ///
					600 "2010" 601 " " 602 " " 603 " " 604 " " 605 " " 606 " " 607 " " 608 " " 609 " " 610 " " 611 " " ///
					612 "2011" 613 " " 614 " " 615 " " 616 " " 617 " " 618 " " 619 " " 620 " " 621 " " 622 " " 623 " " ///
					624 "2012" 625 " " 626 " " 627 " " 628 " " 629 " " 630 " " 631 " " 632 " " 633 " " 634 " " 635 " " ///
					636 "2013" 637 " " 638 " " 639 " " 640 " " 641 " " 642 " " 643 " " 644 " " 645 " " 646 " " 647 " " ///
					648 "2014" 649 " " 650 " " 651 " " 652 " " 653 " " 654 " " 655 " " 656 " " 657 " " 658 " " 659 " " ///
					660 "2015" 661 " " 662 " " 663 " " 664 " " 665 " " 666 " " ///
					, replace 
					lab val bc1 year
			
				
			*Month of enrollment 
				gen em = mofd(enrol_d)
				format em %tm 
				lab val em mth 
				
			*Enrolment qrt
				gen eq = qofd(enrol_d)
				format eq %tq 
			
				*Lab 
					levelsof eq, separate( ) 
					di "`r(levels)'"
					foreach value in `r(levels)' {
					di "`value'"
					local lab: di %tq `value'
					di "`lab'"
					lab define eq `value' "`lab'", add
					}
				
					lab list eq
					lab val eq eq
				 				
			*Last visit
				replace lastvis_d = enrol_d if lastvis_d ==. 
				
			
			*Facility type
				gen loc=.
				lab var loc "Facility Type"
				replace loc=1 if facility=="BE" 
				replace loc=1 if facility=="LE"
				replace loc=2 if facility=="ML" 
				replace loc=2 if facility=="PE" 
				replace loc=2 if facility=="ST" 
				replace loc=3 if facility=="DZ" 
				replace loc=3 if facility=="KU"
				replace loc=3 if facility=="DA"
				replace loc=3 if facility=="MC"
				replace loc=3 if facility=="SA"
				replace loc=3 if facility=="NS"
				replace loc=3 if facility=="MJ" 
				replace loc=3 if facility=="MG"
				replace loc=3 if facility=="MH"
				replace loc=3 if facility=="NU"
				replace loc=3 if facility=="NE"
				replace loc=4 if facility=="LH"
				replace loc=3 if facility=="CK"
				replace loc=4 if facility=="QE"
				replace loc=4 if facility=="ZA"
				replace loc=3 if facility=="LL"
				replace loc=2 if facility=="HF"
				recode loc (2=3) (3=2)
				lab define  loc 1"Health Centre" 3"Mission Hosp" 2"District Hosp" 4"Central Hosp"
				lab val loc loc
				assert loc !=. 
				
			*Enrolment age in months 
				gen enrol_age_m = enrol_age / 30
										
		***ELIGIBILITY 
		
			*Dob missing 
				*see 01_baseline 
			
			*Dob > closing 
				drop if dob > close_d & dob !=. 
				
			*Dob > first visit 
				drop if  dob > fvis & dob !=. 
				
			*Dob > first rapid antibody test 
				drop if dob > frat_d & dob !=.
				
			*Drop > first PCR date 
				drop if dob > fpcr_d & dob !=. 
		
			*Born after B+ was implemented 
				drop if dob < date("01/09/2011", "DMY")
			
			*Enrolement date missing 
				drop if enrol_d ==. 
			
			*Enrolment <2010 or >2014
				drop if enrol_d > date("30/06/2014", "DMY") | enrol_d < date("01/09/2011", "DMY")
			
			*AZT during pregnancy 
				drop if inlist(arvpreg, 2, 3) | arvlab == 3
				
			*Drop patients with missing visit dates 
				*see below
				
		***ASSERT
			*Enrolment age
				assert enrol_age !=. 
			
			*Last visit 
				assert lastvis_d !=. 
				
			*Dob
				assert dob !=. 
	
		***GENERATE DATASET FOR STATUS PLOT 
		
			/*Follow-up from birth to 30 months (up to week 156)
			*Possible stages: 
				Not enrolled (event on day of enrolment but not censored)
				Dead (censored on day of death or last visit)
				Discharged (censored on day of discharge) 
				Transfer to other facility or ART clinic (censored) 
				Loss to follow-up (90 days after missed appointment, censored on day of last visit)
				Under follow-up  */ 
				
		***Patients are followed from birth (Time 0) 
			assert dob !=. 
			
		***Enter into HCC on date of enrollment (failure but no censoring) 
			assert enrol_d !=. 

		***Date of death (outcome ==6)
					
			*Merge visits 
				mmerge id using $hec/followup1, unmatched(master) ukeep(id visit visdate outcome appdate)
			
			*List 
				sort id visit visdate 
				       list id visit visdate outcome appdate if id =="ZA_HEC_13_371_3491", sepby(id) 
				
			*Mark all patients who died
				gen d = 1 if outcome ==6 
				bysort id (visit visdate): egen d1 = max(d)
				
			*List 
				       *list id visit visdate outcome appdate if d1 ==1, sepby(id) 
			
			*Date of death
				gen int death_d = appdate if outcome ==6
				format death_d %td
		
			*List visits of all children who did
				       *list id visit visdate outcome appdate death_d d lastvis_d if d1 ==1, sepby(id) 
				
				       *list id visit visdate outcome appdate death_d d lastvis_d if id== "ST_HEC_12_037_0463", sepby(id)
				
			*Death date < lastvis_d -> use lastvis_d as death date
				assert lastvis_d !=. 
				count if death_d < lastvis_d 
				replace death_d = lastvis_d if death_d < lastvis_d
				replace death_d = lastvis_d if death_d ==. & d ==1
				assert death_d !=. if d ==1
				assert death_d > dob
				
			*One row per patient
				bysort id (death_d): keep if _n ==1
				
			*Clean 
				drop visit visdate outcome appdate _merge d1
	
		***Discharged (outcome ==2) 
			
			*Merge visits 
				mmerge id using $hec/followup1, unmatched(master) ukeep(visit visdate outcome appdate)
			
			*List 
				sort id visit visdate 
				*list id visit visdate outcome appdate, sepby(id) 
				
			*Mark all patients who died
				gen dis = 1 if outcome ==2 
				bysort id (visit visdate): egen dis1 = max(dis)
				
			*List 
				sort id visit visdate
				       *list id visit visdate outcome appdate if dis1 ==1, sepby(id) 
			
			*Date of discahrge
				gen int dis_d = visdate if outcome ==2
				replace dis_d = appdate if outcome ==2 & dis_d ==. 
				replace dis_d = lastvis_d if outcome ==2 & dis_d ==. 
				format dis_d %td
		
			*List visits of all children who were discharged
				*list id visit visdate outcome appdate dis_d dis if dis1 ==1, sepby(id) 
				
			*Dis date < lastvis_d -> use lastvis_d as dis date
				assert dob < dis_d 
							
			*One row per patient
				bysort id (dis_d): keep if _n ==1
				
			*Clean 
				drop visit visdate outcome appdate _merge dis1
				
			*Define infants with negative HIV rapid test after end of breastfeeding as discharged  (visdate if inlist(bfeed, 4, 5)
	
				*Merge visits 
					mmerge id using $hec/followup1, unmatched(master) ukeep(visit visdate outcome appdate bfeed)
				
				*Mark women who have stopped breastfeeding  
					gen bf = 1 if inlist(bfeed, 4, 5)  
					bysort id (visit visdate): egen bf1 = max(bf)
				
				*List 
					sort id visit visdate
					*       *list id visit visdate bfeed outcome appdate if bf1 ==1, sepby(id) 
				
				*Date of discahrge
					gen int bf_d = visdate if inlist(bfeed, 4, 5)  
					count if bf_d ==. & inlist(bfeed, 4, 5)  
					count if bf_d !=.
					format bf_d %td
								
				*Bf_d > dob 
					replace bf_d =. if bf_d <= dob
					assert dob < bf_d if bf_d !=. 
							
				*One row per patient
					bysort id (bf_d): keep if _n ==1
					
				*Clean 
					drop visit visdate outcome appdate bf1
										
				*Discharged if negative after end of bf (assuming all rapid antibody tests taken after end of bf are valid, HCWs will only take rat test after end of bf if this is a valid measure.
					gen dis1 = 1 if lrat_res ==1 & lrat_d >= bf_d & lrat_d !=. 
					
				*List 
					*list id bf_d lrat_res dis dis_d dis1 lrat_d bfeed if bf_d !=., sepby(id) header(10)
								
				*Dis_d -> take lrat_d if missing 
					replace dis_d = lrat_d if dis_d ==. & dis1 ==1
					replace dis = 1 if dis1 ==1
					assert dis_d !=. if dis ==1
					
				*Clean 
					drop dis1 _merge
				
		***Referred to ART (outcome ==3) 
			
			*Merge visits 
				mmerge id using $hec/followup1, unmatched(master) ukeep(visit visdate outcome appdate)
			
			*List 
				sort id visit visdate 
				*list id visit visdate outcome appdate, sepby(id) 
				
			*Mark all patients who started ART
				gen ART = 1 if outcome ==3 
				bysort id (visit visdate): egen ART1 = max(ART)
				
			*List 
				sort id visit visdate
				     *  list id visit visdate outcome appdate if ART1 ==1, sepby(id) 
					   
			*Overwrite ART if children continue follow-up therater ART thereafter
				 list id visit visdate outcome appdate if id =="SA_HEC_12_497_1142", sepby(id)
				 bysort id (visdate): egen temp = max(visdate) if inlist(outcome, 1,2)
				 bysort id (visdate): egen temp1 = min(temp)
				 format temp* %td
					*list id visit visdate outcome appdate temp* if id =="SA_HEC_12_497_1142", sepby(id)
				 gen temp2 = 1 if visdate < temp1 & temp1 !=. & outcome ==3
				 *list id visit visdate outcome appdate temp* if ART1 ==1, sepby(id) 
				 replace outcome = 1 if temp2 ==1	
				 drop temp*
				
			*Date of referral to ART clinic 
				gen int ART_d = appdate if outcome ==3
				replace ART_d = visdate if outcome ==3 & ART_d ==. 
				replace ART_d = lastvis_d if outcome ==3 & ART_d ==. 
				format ART_d %td
		
			*List visits of all children who were discharged
				  *list id visit visdate outcome appdate ART_d ART if ART1 ==1, sepby(id) header(20)
							
			*Dis date < lastvis_d -> use lastvis_d as dis date
				assert dob < ART_d 
							
			*One row per patient
				bysort id (ART_d): keep if _n ==1
				
			*Clean 
				drop visit visdate outcome appdate _merge ART1 
				
		***Transferred out (outcome ==4) 
			
			*Merge visits 
				mmerge id using $hec/followup1, unmatched(master) ukeep(visit visdate outcome appdate)
			
			*List 
				sort id visit visdate 
				       *list id visit visdate outcome appdate, sepby(id) header(20)
				
			*Mark all patients who died
				gen trans = 1 if outcome ==4 
				bysort id (visit visdate): egen trans1 = max(trans)
				
			*List 
				sort id visit visdate
				       *list id visit visdate outcome appdate if trans1 ==1, sepby(id) 
			
			*First date of transfer
				gen int trans_d = visdate if outcome ==4
				replace trans_d = appdate if outcome ==4 & trans_d ==. 
				replace trans_d = lastvis_d if outcome ==4 & trans_d ==. 
				format trans_d %td
				
			*List visits of all children who were discharged
				       *list id visit visdate outcome appdate trans_d trans if trans1 ==1, sepby(id) header(20)
							
			*Dis date < lastvis_d -> use lastvis_d as dis date
				assert dob < trans_d 
							
			*One row per patient
				bysort id (trans_d): keep if _n ==1
				
			*Clean 
				drop visit visdate outcome appdate _merge trans1

		
		***LTF (prospective definition)
			
			*Merge visits 
				mmerge id using $hec/followup1, unmatched(master) 
			
			*No entry in visit table -> visit = enrol_d 
				replace visdate = enrol_d if _merge ==1 & visdate ==. 
				replace visit = 1 if _merge ==1 & visit ==.
				
			*Visit date missing on first visit 
				count if visit ==1 & visdate ==. 
				
				*Mark 
					gen temp = 1 if visit ==1 & visdate ==. 
					bysort id (visit visdate): egen temp1 = max(temp)
					
				*List 
					* list id dob visit visdate outcome appdate if temp1==1, sepby(id) header(20)
				
				*Replace with enrolment date
					replace visdate = enrol_d if visit ==1 & visdate ==. 
					
				*Clean 
					drop temp temp1
				
			*Default entries 
			
				*Drop obs if observations was created to indecate defaulting 
				
					*#1) Mark (missing visdate) 
						gen temp = 1 if outcome ==5 & visdate ==. 
						bysort id (visit visdate): egen temp1 = max(temp)
						
					*List 
						*list id dob visit visdate outcome appdate temp if temp1==1, sepby(id) header(20)
						
					*Drop if not in first visit
						drop if temp ==1 & visit !=1
						
					*Clean 
						drop temp temp1	
						
					*#2) Mark (missing appdate & appdate in visdate)
						gen temp = 1 if outcome ==5 & visdate !=. & appdate ==.  
						bysort id (visit visdate): egen temp1 = max(temp)
						
					*List 
						* list id dob visit visdate outcome appdate temp if temp1==1, sepby(id) header(20)
						
					*Drop if not in first visit
						drop if temp ==1 & visit !=1
						
					*Clean 
						drop temp temp1	
						
						
					*#3) Mark (appdate == visdate)
						gen temp = 1 if outcome ==5 & visdate == appdate & appdate !=.    
						bysort id (visit visdate): egen temp1 = max(temp)
						
					*List 
						* list id dob visit visdate outcome appdate temp if temp1==1, sepby(id) header(20)
						
					*Drop if not in first visit
						drop if temp ==1 & visit !=1
						
					*Clean 
						drop temp temp1	
						
				*Overwrite outcome if default was recorded in observation of a visit 
								
					*Mark (missing visdate) 
						gen temp = 1 if outcome ==5 & appdate != visdate & appdate !=. & visdate !=.  
						bysort id (visit visdate): egen temp1 = max(temp)
						
					*List 
						 list id dob visit visdate outcome appdate temp if temp1==1, sepby(id) header(20)
						
					*Drop if not in first visit
						replace outcome = 1 if temp ==1
						replace appdate = . if temp ==1
						
					*Clean 
						drop temp temp1	
				
				*Remaining 
					
					*Mark remaining 
						gen temp = 1 if outcome ==5
						bysort id (visit visdate): egen temp1 = max(temp)
						
					*List 
						*list id dob visit visdate outcome appdate enrol_d if temp1==1, sepby(id) header(20)
					
					*Def in first visit -> overwrite outcome
						replace outcome = 1 if temp ==1 & visit ==1
					
					*Clean 
						drop temp temp1
						
				*Assert non remaining 
					
					*Mark remaining 
						gen temp = 1 if outcome ==5
						bysort id (visit visdate): egen temp1 = max(temp)
						
					*List 
						 list id dob visit visdate outcome appdate enrol_d if temp1==1, sepby(id) header(20)
					
					*Assert 
						assert temp !=1
					
					*Clean 
						drop temp temp1	
						
						
			*Visits wihout dates
			
					*Missing outcome date -> outcome date (app date) = lastvis_d if visdate ==. & appdate ==. & patient died, transferred out, transferred to ART, or discharged
						replace appdate = lastvis_d if appdate ==. & visdate ==. & inlist(outcome, 2, 3, 4, 6)  
							
					*Missing visit dates  
					
						*#1) Mark 
							gen temp = 1 if visdate ==. & !inlist(outcome, 2, 3, 4, 6)  
							bysort id (visit visdate): egen temp1 = max(temp)
							
						*List 
							list id dob visit visdate outcome appdate temp if temp1==1, sepby(id) header(20)
						
						*Take appointment date from previous visit as visit date
							bysort id (visit visdate): replace visdate = appdate[_n-1] if temp == 1
						
						*Clean 
							drop temp temp1	
							
						*#2) Mark 
							gen temp = 1 if visdate ==. & !inlist(outcome, 2, 3, 4, 6)  
							bysort id (visit visdate): egen temp1 = max(temp)
							
						*List 
							list id dob visit visdate outcome appdate temp if temp1==1, sepby(id) header(20)
						
						*Between two visit dates -> take midpoint
							bysort id (visit visdate): replace visdate = visdate[_n-1] + ((visdate[_n+1]-visdate[_n-1])/2) if temp == 1 & visdate[_n+1]!=. & visdate[_n-1] !=. 
						
						*Clean 
							drop temp temp1	
							
						*#3) Mark 
							gen temp = 1 if visdate ==. & !inlist(outcome, 2, 3, 4, 6)  
							bysort id (visit visdate): egen temp1 = max(temp)
							
						*List 
							 list id dob visit visdate outcome appdate temp if temp1==1, sepby(id) 
						
						*Add 3 month
							bysort id (visit visdate): replace visdate = visdate[_n-1] + 90 if visdate ==. & temp ==1
							
						*Clean 
							drop temp temp1	
							
						*#4) Mark 
							gen temp = 1 if visdate ==.  
							bysort id (visit visdate): egen temp1 = max(temp)
							
						*List 
							 list id dob visit visdate outcome appdate temp if temp1==1, sepby(id) 
						
						*Take appdate if not missing
							 replace visdate = appdate if  visdate ==. & appdate !=. & temp ==1 & inlist(outcome, 2, 3, 4, 6)
							
						*Clean 
							drop temp temp1	
							
						*#6) Mark 
							gen temp = 1 if visdate ==.  
							bysort id (visit visdate): egen temp1 = max(temp)
							
						*List 
							 list id dob visit visdate outcome appdate temp if temp1==1, sepby(id) 
						
						*Check
							assert temp ==.
							drop temp*
							
						*Assert
							assert visdate !=. 
																	
				*Save table with treatment status of mother 
					preserve 
					keep id visdate mstatusv 
					save $hec/mstatus, replace 
					restore 

											
		***GENERATE ANALYSE TABLE 
			
			*preserve 
				preserve 
				bysort id (visit visdate): keep if _n ==1
				drop visit visdate height weight muac malnutr bfeed mstatusv tbstatus clinmon hiv cpt outcome appdate _merge
			
			*save 
				save $hec/analyse00, replace	
					
			*restore 
				restore 
			
		***GENERATE CLEAN DATASETS WITH VISITS 

			*Clean 
				keep id visit visdate height weight muac malnutr bfeed mstatusv tbstatus clinmon hiv cpt outcome appdate dob close
			
			*Missing 
				egen mis = rowmiss(id visit visdate height weight muac malnutr bfeed mstatusv tbstatus clinmon hiv cpt outcome appdate)
			
			*Unique visit date
				bysort id visdate (mis): gen temp = _N
				
			*Mark 
				bysort id (visdate mis): egen temp1 = max(temp)
					
			*List 
				       *list if temp1>1, sepby(id) header(20)
				
			*Drop 
				bysort id visdate (mis): keep if _n ==1 
				
			*Drop observations with recording of death
				drop if outcome == 6
				
			*Clean 
				drop temp temp1 mis visit
				
			*Assert unique of visit 
				bysort id visdate: gen temp = _N 
				assert temp ==1
				drop temp
				
			*Visit number
				bysort id (visdate): gen vis = _n 
				
			*Clean 
				keep id id visdate vis 
				
			*Drop censored visits (after closing date or after 900 days from dob) 
				
				*Merge closing date and dob
					mmerge id using $hec/analyse00, unmatched(master) ukeep(close dob)
					
				*Drop visit after closing 
					*list id vis visdate close dob if visdate > close & visdate !=. , sepby(id) header(20)
					gen drop =1  if visdate > close & visdat !=. 
					
				*Drop if visit was after > 900 days
					gen int temp = visdate - dob 
					       *list id vis visdate close dob temp if temp >900 & temp !=., sepby(id) header(20)
					replace drop = 1 if temp >900 & temp !=.
					list id vis visdate close dob temp if id =="LL_HEC_11_131_0190", sepby(id) header(20)
					
				*save
					save $hec/visits1, replace	
				
			*Ensure that each patient has at least one visit 
				
				*Use baseline
					use $hec/analyse00, clear
				
				*Clean 
					keep id enrol_d dob close
					
				*Merge visits
					mmerge id using $hec/visits1
					
				*List
					sort id visdate
					       *list in 1/10, sepby(id)
					
				*Patients without visit -> enrol_d = visdate
					unique id if visdate ==.
					count if visdate ==. 
					replace vis = 1 if visdate ==. 
					replace visdate = enrol_d if visdate ==.  
					
				*save
					save $hec/visits2, replace
							
			*Age at visit 
					
				*List 
					sort id visdate 
					       *list id dob vis visdate, sepby(id) header(20)
				
				*Generate age at visit 
					gen age_v = (visdate-dob)/30
					assert age_v !=. 
					
				*list 
					       *list id dob vis visdate age, sepby(id) header(20)
					 
				*next appointment date
					gen int app_d = visdate + 30 if age <= 5
					replace app_d = visdate + 90 if age > 5 & age !=. 
					format app_d %td
					assert app_d !=.
					
				*list 
					       *list id vis visdate app_d close if id =="ZA_HEC_14_171_4084", sepby(id) header(20)
				
				*assert 
					assert dob !=. 
					assert close_d !=. 
					assert visdate !=. 
					assert vis !=. 
					
				*clean 
					drop temp
				
			*save
				save $hec/visits3, replace
				
			*exclude censored visits 
				drop if drop ==1
			
		*LTF (prospective)
			
			*Days overdue 
				bysort id (visdate): gen diff = visdate[_n+1]-app_d
				
			*Days overdue for last row
				bysort id (visdate): replace diff = close_d - app_d if _n ==_N 
			
			*List 
				       *list id vis visdate app_d close diff if id =="ZA_HEC_14_171_4084", sepby(id) header(20)
				       *list id vis visdate app_d close diff age, sepby(id) header(20)
			
			*Assert 
				assert diff !=. 
				
			*Define LTF
				* >60, 90, 180 days too late
					foreach j in 60 90 180 {
						di "`j' days"
						preserve 
						gen def`j' = 1 if diff > `j'
						keep if def`j' ==1
						bysort id (visdate): keep if _n ==1
						gen int ltf`j'_d = visdate
						format ltf`j'_d %td
						keep id ltf`j'_d
						save $hec/ltf`j'_d, replace
						count
						restore
					}
					
		*LTF (retrospective) 
				
			*Use data
				use $hec/visits3, clear
				
			*Clean 
				drop _merge 
											
			*List 
				list if id =="LL_HEC_11_131_0190", sepby(id) header(20)
				
			*Last visit 
				bysort id (visdate): keep if _n ==_N
				
			*Default date
				foreach j in 60 90 180 { 
					gen int def`j'_d = app_d + `j'
					format def`j'_d %td
				}
				
			*List 
				list id vis visdate app_d close def* if id =="LL_HEC_11_131_0190", sepby(id) header(20)
				
			*LTF date 
				foreach j in 60 90 180 { 
					gen int LTF`j'_d = visdate if def`j'_d < close_d 
					format LTF`j'_d %td
				}
				
			*List 
				*list id vis visdate app_d close def* LTF* if id =="ZA_HEC_14_171_4084", sepby(id) header(20)
				*list id vis visdate app_d close def* LTF*, sepby(id) header(20)
				
			*Clean 
				keep id LTF*
				
			*Save 
				save $hec/LTF, replace 

		***Final analysis table 		
			
			*Analyse
				use $hec/analyse00, clear 
			
			*Merge LTF
				mmerge id using $hec/LTF
				assert _merge ==3
				
			*Define LTF
				foreach j in 60 90 180 {
					gen LTF`j' = 1 if LTF`j'_d !=. 
				}
				
			*Check consistency of LTF_d
				foreach j in 60 90 180 {
					count if LTF`j'_d < enrol_d & LTF`j'_d !=. 
					*list id enrol_d LTF`j'_d if LTF`j'_d < enrol_d & LTF`j'_d !=.
					replace LTF`j'_d = enrol_d if LTF`j'_d < enrol_d & LTF`j'_d !=. 
					assert LTF`j'_d >= enrol_d 
					assert LTF`j'_d >= dob
				}
			
			*Outcomes: Patients LTF are allowed to re-enter the cohort. Follow-up of patients ends on day of LTF, DIS, Dead, ART start or are censored administratievely on the day of last visit or censored on day of transfer.  
			
				foreach j in 60 90 180 {
				
					*Outcome 
						gen int out`j'_d = . 
						format out`j'_d %td
						
					*Date 
						gen out`j' = . 
						lab define out`j' 0 "Enrolled" 1 "RIC" 2 "TO" 3 "DIS" 4 "LTF" 5 "Dead" 6 "ART", replace 
						lab val out`j' out`j'
				
					*Minimum of death dis trans ltf ART
						egen int min`j'_d = rowmin(LTF`j'_d death_d dis_d trans_d ART_d) 
						format min`j'_d %td
					
					*Outcomes
						
						*LTF
							replace out`j' = 4 if LTF`j'_d !=. & (LTF`j'_d - dob) < 900  
						
						*DIS
							replace out`j' = 3 if dis_d !=. & (dis_d - dob) < 900 
								
						*Dead
							replace out`j' = 5 if death_d !=. & (death_d - dob) < 900 
							
						*TO 
							replace out`j' = 2 if trans_d !=. & (trans_d - dob) < 900 
							
						*ART
							replace out`j' = 6 if ART_d !=. & (ART_d - dob) < 900 
							
						*RIC
							replace out`j' = 1 if out`j' ==. 
							
					*Dates 
					
						*LTF
							replace out`j'_d = LTF`j'_d if out`j' == 4  
						
						*DIS
							replace out`j'_d = dis_d if out`j' == 3  
								
						*Dead
							replace out`j'_d = death_d if out`j' ==5  
							
						*TO 
							replace out`j'_d = trans_d if out`j' ==2 
							
						*ART
							replace out`j'_d = ART_d if out`j' ==6
							
						*RIC
							replace out`j'_d = lastvis_d if out`j' ==1 
							
							
							
					*Checks
						assert out`j'_d !=. 
						assert out`j' !=. 
						*assert enrol_d <= out`j'_d  
						replace out`j'_d = enrol_d if enrol_d > out`j'_d
						tab out`j'
														
					*List 
						list id enrol_d out60 out60_d death_d dis_d trans_d LTF60_d lastvis_d if id =="LL_HEC_11_131_0190", header(20) sepby(id)
						
					*Add one day
						replace out`j'_d = out`j'_d + 1 if out`j'_d == enrol_d
				}						
			
			*exit date 
				* the latest time under which the subject is and at risk: patients are not at risk -`j' days before closing date 
				* maximum follow-up time 30 months
				
				*Maximum follwo-up time
					gen int max_fup = dob + 30*30
					format %td max_fup
				
				*Closing - delay
					foreach j in 60 90 180 {
						gen int close_`j' = close_d - `j'
						format %td close_`j'
						*list close_d close_`j'
					}
					
				*exit dates 
					foreach j in 60 90 180 {
						egen int exit`j'_d = rowmin(max_fup close_`j')
						format exit`j'_d %td
					}
					
				*PSHD 
					
					*Merge visits 
						mmerge id using $hec/followup1, unmatched(master) ukeep(visit visdate hiv appdate) uif(hiv ==4)
					
					*PSHD_d
						gen pshd_d = visdate if hiv==4
						format %td pshd_d
						replace pshd_d = appdate if hiv ==4 & pshd_d ==. 
						assert pshd_d !=. if hiv ==4
						
					*List 
						list id visit visdate hiv appdate pshd_d if hiv ==4, sepby(id)
						
					*Clean 
						drop visit visdate hiv appdate
						
					*Unique id 
						bysort id: keep if _n ==1
									
				*Save 
					save $hec/analyse02, replace
										
				*Use 
					use $hec/analyse02, clear
									
				***Eligibility 
					gen elig = 1
					
				*Enrolment 
					gen enrol = 0 if enrol_d >=exit60_d
					replace enrol =1 if enrol ==. 
														
				***Outcome = entry
				
					*Enrol_d 
						replace dob = dob - 1 if enrol_d == dob
						assert dob < enrol_d 
						
					*Other outcome dates 
						foreach var in out60 LTF60 trans dis death {
							di "`var'"
							di "outcome before enrolment"
							count if  `var'_d < enrol_d & enrol_d !=.
							replace `var'_d = enrol_d + 1 if `var'_d < enrol_d & `=`r(N)'' < 5 
							di "outcome on enrolment"
							count if `var'_d ==enrol_d 
							replace `var'_d = `var'_d + 1 if  `var'_d ==enrol_d 
							assert `var'_d > enrol_d 
						}
														
				***States 
										
						*stset 				
							stset enrol_d, failure(enrol == 1) origin(dob) exit(time exit60_d) id(id)
							list id dob enrol_d out60 out60_d _t0 _t _d _st exit60_d trans_d LTF60_d death_d if id =="LL_HEC_11_131_0190", header(20) sepby(id)
							
						*Enrolment after or on exit -> exculde 
							replace elig = 0 if enrol_d >= exit60_d  // on exit 
														
						*_t 
							rename _t enrol_t 
							replace enrol_t = . if elig == 0
							assert enrol_t !=. if elig ==1
							sum enrol_t if elig ==1, de
													
						*_s 
							rename _d enrol_s 
							replace enrol_s = . if elig == 0
							assert enrol_s !=. if elig ==1
							
							
						*list 
							*list id dob enrol_d enrol_t enrol_s elig exit60_d if id =="ZA_HEC_11_127_2299", header(20)
					
					*LTF 
					
						*stset 				
							stset out60_d, failure(out60 == 4) origin(dob) exit(time exit60_d) id(id) 
							assert _st ==1 if elig ==1
							
						*list 
							*list id dob enrol_d out60 out60_d _t0 _t _d _st exit60_d trans_d LTF60_d death_d elig if elig ==1, header(20) sepby(id)
							
						*_t 
							rename _t LTF60_t 
							replace LTF60_t = . if elig == 0
							assert LTF60_t !=. if elig ==1
							sum LTF60_t if elig ==1 & _d ==1, de
														
						*_s 
							rename _d LTF60_s 
							replace LTF60_s = . if elig == 0
							assert LTF60_s !=. if elig ==1
						
						*list 
							*list id dob enrol_d out60 out60_d _st exit60_d trans_d LTF60_d death_d LTF60_t LTF60_s, header(20) sepby(id)
				
				
					*Discharged 
					
						*stset 				
							stset out60_d, failure(out60 == 3) origin(dob) exit(time exit60_d) id(id) 
							assert _st ==1 if elig ==1
							
						*_t 
							rename _t dis_t 
							replace dis_t = . if elig == 0
							assert dis_t !=. if elig ==1
							sum dis_t if elig ==1 & _d ==1, de
														
						*_s 
							rename _d dis_s 
							replace dis_s = . if elig == 0
							assert dis_s !=. if elig ==1
						
						*list 
							*list id dob enrol_d out60 out60_d _st exit60_d trans_d dis_d death_d dis_t dis_s, header(20) sepby(id)
				
					*Dead 
					
						*stset 				
							stset out60_d, failure(out60 == 5) origin(dob) exit(time exit60_d) id(id) 
							assert _st ==1 if elig ==1
							
						*_t 
							rename _t dead_t 
							replace dead_t = . if elig == 0
							assert dead_t !=. if elig ==1
							sum dead_t if elig ==1 & _d ==1, de
														
						*_s 
							rename _d dead_s 
							replace dead_s = . if elig == 0
							assert dead_s !=. if elig ==1
						
						*list 
							*list id dob enrol_d out60 out60_d _st exit60_d trans_d death_d death_d dead_t dead_s, header(20) sepby(id)
							*list id dob enrol_d out60 out60_d _st exit60_d trans_d death_d death_d dead_t dead_s if out60 ==5, header(20) sepby(id)
							
					*ART start
					
						*stset 				
							stset out60_d, failure(out60 == 6) origin(dob) exit(time exit60_d) id(id) 
							assert _st ==1 if elig ==1
							
						*_t 
							rename _t ART_t 
							replace ART_t = . if elig == 0
							assert ART_t !=. if elig ==1
							sum ART_t if elig ==1 & _d ==1, de
														
						*_s 
							rename _d ART_s 
							replace ART_s = . if elig == 0
							assert ART_s !=. if elig ==1
						
						*list 
							*list id dob enrol_d out60 out60_d _st exit60_d trans_d death_d death_d dead_t dead_s, header(20) sepby(id)
							*list id dob enrol_d out60 out60_d _st exit60_d trans_d death_d death_d dead_t dead_s if out60 ==5, header(20) sepby(id)
							
						*** Patients who died after start of ART
						
							list id dob ART_d death_d enrol_s enrol_t LTF60_s LTF60_t dis_s dis_t dead_s dead_t ART_s ART_t if  ART_d !=. & death_d !=.
							replace dead_t = death_d - dob if ART_d !=. & death_d !=.
							replace dead_s = 1 if ART_d !=. & death_d !=.
							replace LTF60_t = death_d - dob if ART_d !=. & death_d !=.
							replace dis_t = death_d - dob if ART_d !=. & death_d !=.
							
					*Dataset prepared for mstate (Wreede, LC et al: An R Package for the Analysis of Competing Risks and Multi-State. Jstatsoft 2011. URL: http://www.jstatsoft.org/v38/i07. p4)
						
						*Model
							*			 [1]						 |----------------Loss to follow-up 	[2]
								*Birth----------Enrolled & retained -|----------------Dead 					[3]
							*										 |----------------Discharged 			[4]
						
						*Dataset
							*list id dob enrol_d out60 out60_d exit60_d enrol_t enrol_s LTF60_t LTF60_s dis_t dis_s dead_t dead_s elig if id =="DZ_HEC_12_137_60011"
							*list id dob enrol_d out60 out60_d exit60_d enrol_t enrol_s LTF60_t LTF60_s dis_t dis_s dead_t dead_s if elig ==1 & out60==4 & exit60_d <LTF60_d, sepby(id)  
							*list id enrol_t enrol_s LTF60_t LTF60_s dis_t dis_s dead_t dead_s in 1/20, sepby(id)  
							
						*Time in months 
							foreach var in enrol_t LTF60_t dis_t dead_t ART_t {
								replace `var' = `var'/30 
							}
						
						*Elig 
							keep if elig ==1
		
							
						*Save 
							save $hec/analyse1, replace	
							use  $hec/analyse1, clear
						
						*Clean 
							keep id ART_t ART_s enrol_t enrol_s LTF60_t LTF60_s dis_t dis_s dead_t dead_s sex enrol_age_m year b_weight arvpreg arvlab barv_birth loc barv_cont
							
						***Revode predictors 
							
							*sex
								tab sex, mi
								tab sex, nolab
								*rcdlb sex 1
								*lab list SEX
								
							*enrol_age_m
								sum enrol_age_m
								rename enrol_age_m EA
								
							*EA rounded
								gen EA_r = round(EA, 0.1)
									
							*year
								tab year, mi
								tab year, nolab 
								egen y = group(year), lab
								tab y, nolab mi
								drop year 
								*rcdlb y 1
								
							*birth weight 
								tab b_weight, mi
								rename b_weight bw
								tab bw, nolab
								*rcdlb bw 2
																					
							*ARVs during pregnancy
								tab arvpreg, mi
								tab arvpreg, mi nolab
								rename arvpreg ap
								*rcdlb ap 1
								*lab list AP
								
							*ARVs during labour 
								tab arvlab, mi
								tab arvlab, mi nolab
								rename arvlab al
								*rcdlb al 1
								*lab list AL
							
							*Baby ARVs at birth
								tab barv_birth, mi
								tab barv_birth, mi nolab
								rename barv_b ab
								*rcdlb ab 1
								*lab list AB
								
							*Baby ARVs at continued
								tab barv_cont, mi
								tab barv_cont, mi nolab
								rename barv_c abc
								*rcdlb abc 1
								*lab list ABC
							
							*Loc
								tab loc, nolab
								tab loc
								*rcdlb loc 3
								*lab list LOC
															
						*Assert
							assert enrol_s ==1 
							
						*Order 
							order SEX EA Y BW AP AL AB LOC EA_r ABC, last
														
						*Save dataset
							saveold $hec/hcc_states, replace version(12)
					
					
		use $hec/hcc_states, clear
		ed if id =="BE_HEC_12_039_1263"
		
		list in 1/5

