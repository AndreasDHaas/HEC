	
	use $hec/rapid_antibody_merged, clear
	
	/***Enrolment***/
		tab event
		keep if inlist(event, "enrolment", "from 12 months", "from 24 months") 
		
		*clean 
			*dropmiss, force
		
		*date of birth
			local var = "ratdate"
			gen temp = date(`var', "DMY", 2020)
			format temp %td
			list `var' temp 
			drop `var'
			rename temp `var'
			
		*Multiple results at one day 
			
			*Drop true duplicates
				drop _merge 
				duplicates drop id ratres ratdate, force
			
			*Number of visits per day
				bysort id ratdate: gen temp = _N if ratdate !=. 
				tab temp
						
			*Keep the record with the most information on selected variables 
				
				*Number of missings 
					egen missing = rowmiss(id ratres ratdate)
					
				*Keep 
					bysort id ratdate (missing): keep if _n ==1
														
			*Clean 
				drop temp missing		
		
		*Rapid antibody test at enrolment
		
			*Result
				tab ratres, mi
				drop if ratres ==. 
		 	
			*Age at rapid antibody test in days (final)
				gen age = . 
			
			*Age at rapid antibody (days)
				tab rataged, mi
				destring rataged, replace force
				tab rataged, mi
				replace rataged = . if rataged > 3650
				tab rataged, mi
				replace age = rataged if age ==. 
				tab age
			
			*Age at rapid antibody (weeks)
				tab ratagew, mi
				destring ratagew, replace force
				replace ratagew = . if ratagew >520
				tab ratagew, mi
				replace age = ratagew * 7 if age ==. 
			
			*Age at rapid antibody (month)
				tab ratagem, mi
				destring ratagem, replace force
				tab ratagem, mi
				replace ratagem = . if ratagem >120
				tab ratagem, mi
				replace age = ratagem * 30 if age ==. 
			
			*Age at rapid antibody  (years)
				tab ratagey, mi
				destring ratagey, replace force
				replace ratagey = . if ratagey >10
				tab ratagey, mi
				replace age = ratagey * 365 if age ==. 
			
				replace rataged = age 
				drop ratagey ratagem ratagew age
				
			*HTCno
				codebook htcno 
				
			*Dob 
				mmerge id using $hec/baseline1, unmatched(master) ukeep(dob)
			
			*Rataged1 
				gen rataged1 = ratdate - dob
				replace rataged1 = rataged if rataged1 ==. 
				drop if rataged1 ==. 
				drop rataged
				rename rataged1 rat_age
				lab var rat_age "Age at rapid test (in days)"
				drop if rat_age <0
				drop if ratres ==.
				spikeplot rat_age 
			
		save $hec/rat1, replace
						
		