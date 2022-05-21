	/////////////////////////////////////////////////////////
	/// Analysis
	////////////////////////////////////////////////////////						
		
	*************************************************************************************************
	***HIV status among HIV exposed children: a) observed
			
			** data
				use $hec/HIV_long, replace
				
			** merge weights
				mmerge id week using $hec/weights_adj_for_testing, unmatched(master)
				
			** List 
				sort id week
				list id week HIV hiv_free DD elig wALL if id =="ZA_HEC_13_341_3530", sepby(id)
				
			** Drop elig
				drop if elig ==0
				
			** Carry-backward 
				
				*Sort by week * -1 
					gen temp = week * -1
					assert temp !=.
					sort id temp
					
				*Treatment status of mother 
					list id hiv_free week elig wALL if id =="ZA_HEC_13_341_3530", sepby(id)
					bysort id (temp): replace ms2_ltf = ms2_ltf[_n-1] if ms2_ltf ==.
										
				*Weight 
					bysort id (temp): replace wALL = wALL[_n-1] if wALL ==.
					assert wALL !=. if T !=. 
									
				*Death of mother 
					assert md !=. 
				
				*List  
					sort id week
					list id hiv_free week ms2_ltf mstatusv wALL md if id =="SA_HEC_12_023_0653", sepby(id)
					
			** Missings 
				assert hiv_free !=. 
				assert week !=. 
				forval j = 1/6 {
					assert weeksp`j' !=.
				}
									
			** logistic regression on testing data 
				logistic hiv_free week weeksp*, vce(cluster id) 

			** predict HIV free survival on dummy data 
				predict p_hiv_free  
				sum p_hiv_free 
					
			** Kaplan-Meier for HIV free survival
				bysort id (week): generate surv=1 if _n==1
				bysort id (week): replace surv= surv[_n-1]* p_hiv_free if _n!=1
				
			** Cumulative incidence of HIV transmssion 
				sort id week
				gen cif_obs = (1-surv) if _n < 132 
			
			** List 
				list id week HIV HIV_free hiv hiv_free p_hiv_free surv cif_ob if _n <132, sepby(id) header(157)
			
			** Plot 
				twoway (line cif_obs week, sort), xlab(0(8)130) ///
				ytitle("Cumulative risk of HIV transmission") xtitle("Age (in weeks)") ///
				scheme(s1mono)
				
			** Save estimates 
				preserve 
				keep week cif_obs
				keep if cif_obs !=. 
				save $hec/cif_obs, replace 
				restore 
				
	*************************************************************************************************
	***HIV status among HIV exposed children: b) perfect testing & perfect retention
		
				
			** Missings 
				
				foreach var in hiv_free week LOC ms2_ltf AB ABC md {
					assert `var' !=. 
				}
				forval j = 1/6 {
					assert weeksp`j' !=.
				}
			
			** logistic regression on testing data 
				logistic hiv_free week weeksp* i.AP i.AL ms2_ltf i.md i.AB i.ABC i.LOC [pweight=wALL], vce(cluster id) 
				*logistic hiv_free week weeksp* i.AP ms2_ltf i.LOC [pweight=wALL], vce(cluster id) coef

			** predict HIV free survival on dummy data 
				capture drop p_hiv_free
				predict p_hiv_free  
				sum p_hiv_free 
					
			** Kaplan-Meier for HIV free survival
				capture drop surv
				bysort id (week): generate surv=1 if _n==1
				bysort id (week): replace surv= surv[_n-1]* p_hiv_free if _n!=1
				
			** Mean
				collapse (mean) surv, by(week)
				
			** Cumulative incidence of HIV transmssion 
				sort week
				gen cif_adj = 1-surv 
			
			** List 
				*list
												
			** Merge unadjusted estimates
				mmerge week using $hec/cif_obs
				
			** Confidence intervals 
				
				*Merge cumulative number of events
					mmerge week using $hec/cum_N, ukeep(cum_N)
					
				*Fill in missings
					replace cum_N = 0 if week ==0
					sort week
					replace cum_N = cum_N[_n-1] if cum_N ==. 
					assert cum_N !=. 
					
				*N
					gen N = $N
					
				* Estimate error factor for log risk
					gen ef=exp(1.96*sqrt(1/cum_N - 1/N))
					
				* CIs adjusted
					gen cif_adj_l=cif_adj*ef^-1
					gen cif_adj_u=cif_adj*ef
					replace cif_adj_l =0 if week ==0
					replace cif_adj_u =0 if week ==0
					
				* CIs unadjusted
					gen cif_obs_l=cif_obs*ef^-1
					gen cif_obs_u=cif_obs*ef
					replace cif_obs_l =0 if week ==0
					replace cif_obs_u =0 if week ==0
					
							
			** Plot 
				twoway 	(rarea cif_obs_l cif_obs_u week, sort color(gs14)) || ///
						(rarea cif_adj_l cif_adj_u week, sort color(gs14)) || ///
						(line cif_obs week, sort lpattern(solid)) || /// 
						(line cif_adj week, sort lpattern(dash))  ///
						, xlab(0(8)130) ///
						ytitle("Percentage of children diagnosed with HIV") xtitle("Age (in weeks)") ///
						scheme(s1mono) ///
						legend(order(4 "IPW-adjusted" 3 "Observed") rows(2) region(lcolor(none) lpattern(blank))) ///
						legend(ring(0) position(10) bmargin(medium)) 
						
			** Prepare dataset to plot data in R
				
				*Months 
					replace week = week-1
					drop if week == -1
					gen month = week*7/30
				
				*Long
					preserve 
					keep month cif_obs*
					rename cif_obs cif 
					rename cif_obs_l cif_l
					rename cif_obs_u cif_u
					gen est = "Unweighted"
					list
					save $hec/crude, replace
					restore 
					keep month cif_adj*
					rename cif_adj cif 
					rename cif_adj_l cif_l
					rename cif_adj_u cif_u
					gen est = "Weighted"
					list 
					append using $hec/crude
											
				*Order 
					order month est cif* 
					
				*Sort 
					gen sort = 1 if est =="Unweighted"
					replace sort = 2 if est =="Weighted"
					sort month sort
																	
				*Save dataset
					saveold $hec/HIV_est, replace version(12)
					
				*Use
					use $hec/HIV_est, clear
					
				*Weeks
					gen weeks = month*30/7
				
				*List 
					sort est week
					foreach var in cif cif_l cif_u {
						replace `var' = (`var'* 100)
						format `var' %3.2f
						}
										
					list month week est cif* if inlist(weeks, 8, 52, 129), sepby(est)
			
				*Save dataset
					saveold $hec/HIV_est_prc, replace version(12)
