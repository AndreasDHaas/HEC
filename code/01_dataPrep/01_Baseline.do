	
**********************************************************
*** Datamanagement
	
	*Deduplicated dataset from Adrian with merged follow-up data for infants who had 2 cards 
		use $hec/baseline_merged_unique, clear
		
	/***Baseline***/
				
		*Facility
			tab fac
		
		*Gender
			tab sex, mi
			
		*date of birth
			gen temp = date(dob, "DMY", 2050)
			format temp %td
			*list dob temp
			drop dob
			rename temp dob
			drop if dob ==. 
			
		*birth cohort 
			gen temp = month(dob)
			gen temp1 = year(dob)
			format temp %02.0f
			tostring temp temp1, replace usedi
			*list temp temp1
			gen bc = temp1 + "/" + temp if temp != "."
			tab bc
		
		*birth weight
			tab bweight, mi
			destring bweight, replace force
			tab bweight
			replace bweight = . if bweight > 7
			tab bweight
			
		*clean
			drop grelation 
			
		*ARV_prg
			tab arvpreg, mi
		
		*ARV_lab
			tab arvlab, mi
		
		*Baby ARV at birth
			tab barvbirth, mi
			
		*Baby ARV cont? 
			tab barvcont, mi
			
		*Baby ARV adherence 
			tab barvadh, mi
			
		*** Age at enrollment 	
			*Age at entrolment in days (final)
				gen age = . 
			
			*Age at entrollment (days)
				tab enraged, mi
				replace age = 0 if enraged =="at birth" & age ==. 
				destring enraged, replace force
				tab enraged, mi
				replace enraged = . if enraged > 3650
				tab enragem, mi
				replace age = enraged if age ==. 
			
			*Age at entrollment (weeks)
				tab enragew, mi
				destring enragew, replace force
				replace enragew = . if enragew >520
				tab enragew, mi
				replace age = enragew * 7 if age ==. 
			
			*Age at entrollment (month)
				tab enragem, mi
				replace age = 0 if enragem =="10 hrs" & age ==. 
				destring enragem, replace force
				tab enragem, mi
				replace enragem = . if enragem >120
				tab enragem, mi
				replace age = enragem * 30 if age ==. 
			
			*Age at enrollment (years)
				tab enragey, mi
				replace age = 0 if enragey =="at birth" & age ==. 
				replace age = 0 if enragey =="0" & age ==. 
				destring enragey, replace force
				replace enragey = . if enragey >10
				tab enragey, mi
				replace age = enragey * 365 if age ==. 
			
				replace enraged = age 
				drop enragey enragem enragew 
		
		list id dob rap*  
		
		***Rapid test at enrolment
			*Rapid test baby 
				tab raptest, mi
				drop age
		
			*Age at rapid test in days (final)
				gen age = . 
			
			*Age at rapid test (days)
				tab rapaged, mi
				destring rapaged, replace force
				tab rapaged, mi
				replace rapaged = . if rapaged > 3650
				tab rapagem, mi
				replace age = rapaged if age ==. 
				tab rapaged
			
			*Age at rapid test (weeks)
				tab rapagew, mi
				destring rapagew, replace force
				replace rapagew = . if rapagew >520
				tab rapagew, mi
				replace age = rapagew * 7 if age ==. 
			
			*Age at rapid test  (month)
				tab rapagem, mi
				destring rapagem, replace force
				tab rapagem, mi
				replace rapagem = . if rapagem >120
				tab rapagem, mi
				replace age = rapagem * 30 if age ==. 
			
			*Age at rapid test (years)
				tab rapagey, mi
				destring rapagey, replace force
				replace rapagey = . if rapagey >10
				tab rapagey, mi
				replace age = rapagey * 365 if age ==. 
			
				replace rapaged = age 
				tab rapaged, mi
				drop rapagey rapagem rapagew age
			
		***PCR at enrolment
			*Child PCR 
				tab pcr, mi nolab
			
			*Age 
				gen age = . 
			
			*Age at pcr (days)
				tab pcraged, mi
				destring pcraged, replace force
				tab pcraged, mi
				replace pcraged = . if pcraged > 3650
				tab pcragem, mi
				replace age = pcraged if age ==. 
			
			*Age at pcr (weeks)
				tab pcragew, mi
				destring pcragew, replace force
				replace pcragew = . if pcragew >520
				tab pcragew, mi
				replace age = pcragew * 7 if age ==. 
			
			*Age at pcr  (month)
				tab pcragem, mi
				destring pcragem, replace force
				replace pcragem = . if pcragem >120
				tab pcragem, mi
				replace age = pcragem * 30 if age ==. 
			
			*Age at pcr (years)
				tab pcragey, mi
				destring pcragey, replace force
				replace pcragey = . if pcragey >10
				tab pcragey, mi
				replace age = pcragey * 365 if age ==. 
			
				replace pcraged = age 
				tab pcraged, mi
				drop pcragey pcragem pcragew age
				count if inlist(pcr, 2, 3) & pcraged !=.
				
		*HIV confirmed
			tab hivinf, mi
		
		*Mother HIV status
			tab mstatus, mi 
		
		*Mat ART number
			codebook martregno
		
		*Clean 
			drop event temp*
		
		*Order
			order id facility bc dob sex enraged bweight arvpreg arvlab barvbirth barvcont barvadh raptest rapaged pcr pcraged hivinf mstatus martregno
		
		*transferdate
			gen temp = date(transdate, "DMY", 2050)
			format temp %td
			list trans temp
			drop trans
			rename temp transdate
		
		save $hec/baseline1, replace
