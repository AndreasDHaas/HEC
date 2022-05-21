
**********************************
***Closing dates 
						
	*use follow-up records 
		use	$hec/followup1, clear
	
	*generate facility 
		gen fac = substr(id, 1, 2)
			
	*vis
		tab fac
		tab visd if fac =="ZA" 
		
	*use baseline 	
		use $hec/baseline1, clear
	
	***clean  
		drop raptest rapaged pcr pcraged hivinf
		
	***closing date
		gen int close_d =.
		format close_d %td
		replace close_d = date("04jun2014", "DMY") if fac =="BE"
		replace close_d = date("01jun2015", "DMY") if fac =="CK"
		replace close_d = date("01jul2014", "DMY") if fac =="DA"
		replace close_d = date("05may2014", "DMY") if fac =="DZ"
		replace close_d = date("26jul2015", "DMY") if fac =="HF"
		replace close_d = date("16jul2014", "DMY") if fac =="KU"
		replace close_d = date("05sep2014", "DMY") if fac =="LE"
		replace close_d = date("13jun2014", "DMY") if fac =="LL"
		replace close_d = date("26jun2014", "DMY") if fac =="MC"
		replace close_d = date("31oct2014", "DMY") if fac =="MG"
		replace close_d = date("23oct2014", "DMY") if fac =="MH"
		replace close_d = date("18feb2015", "DMY") if fac =="MJ"
		replace close_d = date("02sep2014", "DMY") if fac =="ML"
		replace close_d = date("15may2015", "DMY") if fac =="NE"
		replace close_d = date("25aug2014", "DMY") if fac =="NS"
		replace close_d = date("21may2014", "DMY") if fac =="NU"
		replace close_d = date("18jul2015", "DMY") if fac =="PE"
		replace close_d = date("29sep2014", "DMY") if fac =="QE"
		replace close_d = date("30jun2015", "DMY") if fac =="SA"
		replace close_d = date("24jun2014", "DMY") if fac =="ST"
		replace close_d = date("04may2015", "DMY") if fac =="ZA"
	
	*Save
		save $hec/baseline2, replace
