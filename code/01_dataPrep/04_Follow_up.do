
	use $hec/followup_merged, clear
					
		*clean 
			*dropmiss, force
	
		*dates
			foreach var in visdate appdate {
			gen temp = date(`var', "DMY", 2020)
			format temp %td
			list `var' temp 
			drop `var'
			rename temp `var'
			}
			
		*visit date
			replace visdate = . if visdate < date("01/01/2010", "DMY")
		
		*Several visits at one day 
			
			*Number of visits per day
				bysort id visdate: gen temp = _N if visdate !=. 
				tab temp
				
			*Drop visit 3 and visit 4 at one day (only few)
				bysort id visdate: drop if _n >2 & temp !=. & inlist(temp, 3, 4)
				
			*Number of visits per day
				drop temp
				bysort id visdate: gen temp = _N if visdate !=. 
				tab temp
				
			*List 
				list id visit height weight muac malnutr bfeed mstatusv tbstatus clinmon hiv cpt outcome visdate appdate if temp >1 & temp !=., sepby(id visdate)
			
			*Clean 
				drop _merge
			
			*Define master records (record with fewer missings)
				
					*Number of missings 
						egen missing = rowmiss(id - appdate)
					
					*Mark records with fewer missings as record 1 other as recrod 2 
						bysort id visdate (missing): gen recno = _n if visdate !=. 
						
					*List 
						list id visit height weight muac malnutr bfeed mstatusv tbstatus clinmon hiv cpt outcome visdate appdate missing recno if temp >1 & temp !=., sepby(id visdate) header(26)
					
					*Assert id not missing
						assert id !="" 
					
					*Keep outcome if absorbing state (Discharged, ART, TO, Death) was recorded in 2nd record e.g. "CK_HEC_14_457_2026" 
						list id visdate height weight muac outcome if id =="CK_HEC_14_457_2026", sepby(id visdate)
						bysort id visdate (recno): replace outcome = outcome[_n+1] if inlist(outcome[_n+1], 2, 3, 4, 6) & temp >1 & temp !=.
										
				*If data in record 1 is missing replace with record 2 
					
					*List example case 
						list id recno visdate weight temp if id =="SA_HEC_11_015_0513", sepby(id visdate) // example case 
										
					*loop over all variables 
						foreach var of varlist _all {
						
						*check if variable is numeric 
							capture confirm numeric variable `var'
						
						*If numeric 
							if !_rc {
								bysort id visdate (recno): replace `var' = `var'[_n+1] if `var' ==. & `var'[_n+1] !=. & temp >1 & temp !=. // replace numeric variables 
							}
							else {
								bysort id visdate (recno): replace `var' = `var'[_n+1] if `var' =="" & `var'[_n+1] !="" & temp >1 & temp !=. // replace string variables 
							}
						}
											
					*List example cases 
						list id recno visdate weight temp if id =="SA_HEC_11_015_0513", sepby(id visdate) // example case 
						
					*Keep only 1st record 
						bysort id visdate (recno): drop if recno > 1 & recno !=. & visdate !=. 
						
					*Clean 
						drop temp recno missing
					
		*visit 
			drop visit // incorrect after merging of visits
			bysort id (visdate): gen visit = _n
		
		*current age 
			drop curr*
			
		*height
			tab height, mi
			destring height, force replace
			replace height = . if height < 15
			
		*weight
			tab weight, mi
			destring weight, force replace
			replace weight = . if weight < 1 
			replace weight = . if weight > 50
			
		*muac 
			tab muac
			destring muac, force replace
			replace muac = . if muac < 1 
			replace weight = . if weight > 50

		*malnutr
			tab malnutr, mi
			
		*bfeed
			tab bfeed, mi
			
		*tb status
			tab tbstatus, mi
			
		*Clinical monitoring
			tab clinmon, mi
			
		*HIV
			tab hiv, mi
			
		*CTP
			tab cpt, mi
			
		*Outcome	
			tab outcome
		
		*current age
			mmerge id using $hec/baseline1, unmatched(master) ukeep(dob)
			gen cage = visdate - dob
			drop _merge dob
			tab cage
			replace cage = . if cage <0
			
		*closing date 
			sort visdate
			list id visdate
			
		*Clean 
			order id cage visit visdate height weight muac malnutr bfeed mstatusv tbstatus clinmon hiv cpt outcome appdate	
			
		save $hec/followup1, replace
		use $hec/followup1, clear
		
	
	list if id =="ZA_HEC_13_371_3491", sepby(id)
