	
	use $hec/pcr_merged, clear
	
	/***Enrolment***/
		tab event
		keep if inlist(event, "pcr 1", "pcr 2", "pcr 3") 
		drop event
		
		*clean 
			*dropmiss, force
	
		*date of birth
			foreach var in pcrdate pcrdtlabres pcrdtgiv {
			gen temp = date(`var', "DMY", 2020)
			format temp %td
			list `var' temp 
			drop `var'
			rename temp `var'
			}
			
		*Multiple results at one day 
			
			*Drop true duplicates
				drop _merge 
				duplicates drop id pcrres pcrdate, force
			
			*Number of visits per day
				bysort id pcrdate: gen temp = _N if pcrdate !=. 
				tab temp
			
			*List 
				list if temp >1 & temp !=., sepby(id pcrdate)
		
			
			*Keep the record with the most information on selected variables 
				
				*Number of missings 
					egen missing = rowmiss(id pcrres pcrdate)
					
				*Keep 
					bysort id pcrdate (missing): keep if _n ==1
														
			*Clean 
				drop temp missing
				
		****PCR
			*Result
				tab pcrresult, mi
		 	
			*Age at PCR in days (final)
				gen age = . 
			
			*Age at PCR (days)
				tab pcrtaged, mi
				destring pcrtaged, replace force
				tab pcrtaged, mi
				replace pcrtaged = . if pcrtaged > 1095
				tab pcrtaged, mi
				replace age = pcrtaged if age ==. 
			
			*Age at PCR (weeks)
				tab pcrtagew, mi
				destring pcrtagew, replace force
				replace pcrtagew = . if pcrtagew >156
				tab pcrtagew, mi
				replace age = pcrtagew * 7 if age ==. 
			
			*Age at PCR (month)
				tab pcrtagem, mi
				destring pcrtagem, replace force
				tab pcrtagem, mi
				replace pcrtagem = . if pcrtagem >36
				tab pcrtagem, mi
				replace age = pcrtagem * 30 if age ==. 
			
			*Age at PCR (years)
				tab pcrtagey, mi
				destring pcrtagey, replace force
				replace pcrtagey = . if pcrtagey >3
				tab pcrtagey, mi
				replace age = pcrtagey * 365 if age ==. 
			
				replace pcrtaged = age 
				drop pcrtagey pcrtagem pcrtagew age
				
			*Clean
				order id fac pcrdate pcrdtlabres pcrresult pcrdtgiv pcrtaged sampleid 
				
			*PCR
				mmerge id using $hec/baseline1, unmatched(master) ukeep(dob)
				
			*Rataged1 
				gen pcrtaged1 = pcrdate - dob
				replace pcrtaged1 = pcrtaged if pcrtaged1 ==. 
				drop if pcrtaged1 ==. 
				drop pcrtaged
				rename pcrtaged1 pcr_age
				lab var pcr_age "Age at pcr_test (in days)"
				drop if pcr_age <0
				drop if pcrres ==.
				spikeplot pcr_age 
				
				
			*Clean
				drop _merge dob
				order id fac pcrdate pcr_age 
				
				
		save $hec/pcr1, replace
						