
use $hec/analyse1, clear

	mmerge id using $hec/HIV, unmatched(master) ukeep(HIV)
	bysort id (HIV): keep if _n ==_N
	tab HIV, mi
	replace HIV = 9 if HIV ==. 
	
	*********************************************************************************
	***Remove old files 
		foreach var in count sex enrol_age_c enrol_age_m year b_weight bweight arvpreg arvlab barv_birth barv_cont loc {
		capture erase $hec/`var'.dta
		}
	
	*********************************************************************************
	***DEFINE GROUP VARIABLE (BY VAR) 
	 /* each level of byvar will be a column in table */
	 /* maximum number of levels in byvar = 6 */
	
	 ***Define byvar
		capture macro drop global byvar
		global byvar HIV
		di "$byvar"
	 
	 ****BYVAR: Number of levels in byvar (global x)
		capture macro drop global x
		tab $byvar 
		global x = `r(r)'
		di $x // number of levels in byvar
		assert $x <= 6  // byvar not valid
		assert $x >= 2 // byvar not valid

	*********************************************************************************
	***DATA MANAGEMENT
		
		*Number of patients
			tab count $byvar 
	
		*Sex
			tab sex $byvar, col chi

		*Age at enrollment 
			tab enrol_age_c $byvar, col chi
		
		*Median age at enrollment 
			bysort $byvar: sum enrol_age_m, de
		
		*Year of birth
			tab year $byvar, col chi
			
		*Birth weight
			tab b_weight $byvar, col chi
					
		*Median birth weight
			replace bweight = bweight * 1000
			bysort $byvar: sum bweight, de
			
		*ARV during pregnancy
			tab arvpreg $byvar, col chi mi
				
		*ARV during labour 
			tab arvlab $byvar, col chi
									
		*Baby ARV at birth
			tab barv_birth $byvar, col chi
								
		*Baby ARVs continued
			tab barv_cont $byvar, col chi	
			
		*Baby ARVs continued
			tab loc $byvar, col chi	
						
	*********************************************************************************
	***VALUE LABELS
	
		lab define count 1 "Number of patients (%)", replace
		
		lab define sex -888 "Gender (%)" ///
			1 "   Male" ///
			2 "   Female" ///
			99 "   Unknown" , replace
		
		lab define enrol_age_c -888 "Age at enrolment (%)" ///
			1 "   <6 weeks" ///
			2 "   6-12 weeks" ///
			3 "   3-6 months" ///
			4 "   >6 months",  replace
		lab define enrol_age_m 1 "Median age at enrolment (IQR)", replace
		
		lab define year -888 "Year of birth (%)" ///
			2010 "   2010" ///
			2011 "   2011" ///
			2012 "   2012" ///
			2013 "   2013" ///
			2014 "   2014",  replace
		
		lab define b_weight -888 "Birth weight (%)" ///
			1 "   <2500g" ///
			2 "   >=2500g" ///
			3 "   Missing", replace
		lab define bweight 1 "Median birth weight (IQR)", replace
		
		replace arvpreg = 4 if arvpreg ==5
		
		lab define arvpreg -888 "Antepartum ARV exposure (%)" ///
			1 "   None" ///
			2 "   AZT 0-3 weeks" ///
			3 "   AZT 4+ weeks" ///
			4 "   ART" ///
			5 "   ART" ///
			6 "   Unknown", replace 
		
		replace arvlab = 5 if arvlab ==2
		
		lab define arvlab -888 "Intrapartum ARV exposure (%)" ///
			1 "   None" ///
			2 "   sdNVP" ///
			3 "   AZT" ///
			5 "   ARVs" ///
			6 "   Unknown", replace 
		
		lab define barv_birth -888 "Prophylactic ARVs at birth (%)" ///
			1 "   None" ///
			2 "   Yes" ///
			3 "   Unknown", replace 
			
		lab define barv_cont -888 "Prophylactic ARVs after birth (%)" ///
			1 "   None" ///
			2 "   Yes" ///
			3 "   Unknown", replace 
			
		lab define loc -888 "Facility type (%)" ///
			1 "   Health centre" ///
			2 "   District hospital" ///
			3 "   Mission hospital" ///
			4 "   Central hospital",  replace
		
		foreach var in count sex enrol_age_c enrol_age_m year b_weight bweight arvpreg arvlab barv_birth barv_cont loc {
			lab val `var' `var' 
			label save `var'  using $hec/`var'.do , replace
		}
		
		*Weight
			replace bweight = bweight / 1000

save $hec/t1_hec, replace
	
	*********************************************************************************************
	***GENERATE DATASETS WITH SUMMARY STATISTICS
	
	
	*************************************
	***ROW PERCENTAGES
		
		use $hec/t1_hec, clear
			
			***Number of patients
				
				*Write N in dataset
					tab count $byvar, col matcell(N) matrow(levels) 
					matrix list N
					matrix list levels
					matrix colnames levels = levels
					matrix tmp = levels , N
					matrix list tmp
					clear
					svmat2 tmp, names(col)
						
				* N in total 
					egen c`=$x+1' = rowtotal(c1-c$x)
										
				* Labels 
					do $hec/count.do
					lab val levels count
						
				*Column totals & column percent
					forval y = 1/`=$x+1' {
					gen prc_c`y' = c`y' / c`=$x+1' * 100 // column percent
					order prc_c`y', after(c`y')
					}
								
			***Format	
					
					*N
					forval y = 1/`=$x+1' {
					format %8.0f c`y'	// %8.0fc with , to seperate thousands 
					tostring c`y', replace usedisplay force
					replace c`y' = "" if c`y' =="."
					}
					
					*PRC
					forval y = 1/`=$x+1' {
					format %3.1fc prc_c`y'
					tostring prc_c`y', replace usedisplay force
					replace prc_c`y' = "(" + prc_c`y' + "%)" 
					replace prc_c`y' = "" if prc_c`y' =="(.%)"
					*replace prc_c`y' = regexr(prc_c`y', "\." ,"·") // middle point
					rename prc_c`y' e`y' 
					}
														
					*levels to string
						decode levels, gen(desc)
						order desc
						drop levels
					
					*Headline 
						format %-30s desc
						
				list, separator(`=_N')    
								
				*Save 
					save $hec/count, replace 
		
	*************************************
	***MEDIAN (IQR)	
			
		***Loop over variables 
			foreach var in enrol_age_m bweight { // 
			use $hec/t1_hec, clear
			
			***post sumary statistics 
				estpost tabstat `var', by($byvar) statistics(median p25 p75) columns(statistics) 
				matrix median = e(p50)
				matrix p25 = e(p25)
				matrix p75 = e(p75)
			
			***compose final matrix
				matrix mat = median[1,1], p25[1,1], p75[1,1] 
				forval y = 2/`=$x+1' {
				matrix mat = mat, median[1,`y'], p25[1,`y'], p75[1,`y'] 
				}
			
			***matrix to data
				matrix list mat
				clear
				svmat2 mat, names(col)
	
			***levels & lables
				gen levels = 1 
				do $hec/`var'.do
				lab val levels `var'
				order levels
				
			***Max
				local max = `=($x+1)*3' // number of levels in byvar + 1 for total column 1* 3 statistics per column (3+1)*4 = 12
				
			***Round columns 
				forval y = 1/`max' {
				replace c`y' = round(c`y', 0.1)
				}
			
				list 
				
			***Combine IQRs and rename colums 
				tostring c1-c`max', replace force usedisplay
								
				local level = 1 
				local p25 = 2
				local p75 = 3 
				local median = 1
				
				while `p75' <= `max' {
				replace c`p25' = "(" + c`p25' + "-" + c`p75' + ")"
				rename c`p25' e`level'
				drop c`p75'
				rename c`median' c`level'
				local level = `=`level' + 1' 
				local p25 = `=`p25' + 3'
				local p75 = `=`p75' + 3'
				local median = `=`median' + 3'
				di `level'
				di `p25'
				di `p75'
				di `median'
				}
			
			***Add Headline
				set obs `=_N+1'
				replace levels = -888 if _n ==_N
				sort levels
			
			
			***Format
			
				*levels to string
					decode levels, gen(desc)
					order desc
					drop levels
				
				*Headline 
					format %-30s desc
							
			list, separator(`=_N')  
				
			*save
				save $hec/`var', replace 
			}
		
	*************************************
	***N & COLUMN PERCENT
	
		***Loop over variables 
				
				foreach var in count sex enrol_age_c year b_weight arvpreg arvlab barv_birth barv_cont loc { // 
					use $hec/t1_hec, clear
									
				***tabulate variable 
					tab `var' $byvar, col
					
				*** N in subgroups
					tab `var' $byvar, col matcell(N) matrow(levels) 
					matrix list N
					matrix list levels
					matrix colnames levels = levels
					matrix tmp = levels , N
					matrix list tmp
					clear
					svmat2 tmp, names(col)
					
				*** N in total 
					egen c`=$x+1' = rowtotal(c1-c$x)
				
				*** Labels 
					do $hec/`var'.do
					lab val levels `var'
				
				***Column totals & column percent
					forval y = 1/`=$x+1' {
					egen total_c`y' = sum(c`y')  // column totals
					gen prc_c`y' = c`y' / total_c`y' * 100 // column percent
					order prc_c`y', after(c`y')
					}
					
					drop total*
				
				***Headline
					set obs `=_N+1'
					replace levels = -888 if _n ==_N
					sort levels
					
				***Format	
					
					*N
					forval y = 1/`=$x+1' {
					format %8.0f c`y'	// %8.0fc with , to seperate thousands 
					tostring c`y', replace usedisplay force
					replace c`y' = "" if c`y' =="."
					}
					
					*PRC
					forval y = 1/`=$x+1' {
					format %3.1fc prc_c`y'
					tostring prc_c`y', replace usedisplay force
					replace prc_c`y' = "(" + prc_c`y' + "%)" 
					replace prc_c`y' = "" if prc_c`y' =="(.%)"
					*replace prc_c`y' = regexr(prc_c`y', "\." ,"·") // middle point
					rename prc_c`y' e`y' 
					}
														
					*levels to string
						decode levels, gen(desc)
						order desc
						drop levels
					
					*Headline 
						format %-30s desc
						
				list, separator(`=_N')    
				
				save $hec/`var', replace 
			}		
	
	*************************************
	*** APPEND
		clear 
		foreach var in count sex enrol_age_m bweight arvpreg arvlab barv_birth {
		append using $hec/`var' 
		}
				
		drop if desc ==""
		
		forval y = 1/$x {
		gen str1 b`y' =""
		order b`y', after(e`y')
		format %30s b`y'
		}
		
		list, separator(`=_N') 
		
		export excel using "$tbl/Table1_ppt", firstrow(variables) replace
		
	
	
		
