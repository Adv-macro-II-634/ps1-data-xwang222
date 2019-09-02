 *************************Advanced Macro HW1**********************
 clear all
 use "I:\SUNY\2019 fall\SCF\ps1-data-xwang222\build_data\input\rscfp2007.dta", clear
 ***Take the weight as consideration instead of delete the duplication
 ***Based on summary extract public data
 ***Generate EARNINGS INCOME WEALTH
 gen INF=1.176 
	***As instructed on the SCF website "https://www.federalreserve.gov/econres/scf_2007.htm", the data in summary extract public data
	***is inflation-adjusted to 2016 dollars. Therefore, I use the calculator on https://www.bls.gov/data/inflation_calculator.htm
	***to get the inflation adjustment factor.
	***expressed in '000
 gen EARNING= (wageinc+0.863*bussefarminc)/(INF*1000)
 gen WEALTH=networth/(INF*1000)
 gen INCOME=(wageinc+bussefarminc+intdivinc+kginc+ssretinc+transfothinc)/(INF*1000)
 gen EARNING_LOG=log(EARNING)
 gen WEALTH_LOG=log(WEALTH)
 gen INCOME_LOG=log(INCOME)
 
 ************Table 1 percentile********
 tabstat EARNING INCOME WEALTH [aweight=wgt], statistics(min p1 p5 p10 p90 p95 p99 max) format(%12.2fc) column(statistics)
 ***plus more percentile 20 40 60 80
 pctile EARNING_PC=EARNING [aweight=wgt],nq(5) genp(earnpct)
 pctile INCOME_PC=INCOME [aweight=wgt],nq(5) genp(incpct)
 pctile WEALTH_PC=WEALTH [aweight=wgt],nq(5) genp(wlthpct)
 list EARNING_PC earnpct in 1/4
 list INCOME_PC incpct in 1/4
 list WEALTH_PC wlthpct in 1/4
 
 ***********Table 2 Concentration and Skewness**********
 *coeffient of variation, mean, media, top 1%
 tabstat EARNING INCOME WEALTH [aweight=wgt], statistics(mean cv median) column(statistics)
 
 *location of mean: by comparing mean with ._PC2 value to find corresponding .pct2
 pctile EARNING_PC2=EARNING [aweight=wgt],nq(100) genp(earnpct2)
 pctile INCOME_PC2=INCOME [aweight=wgt],nq(100) genp(incpct2)
 pctile WEALTH_PC2=WEALTH [aweight=wgt],nq(100) genp(wlthpct2)
 tabstat EARNING[aweight=wgt], statistics(mean cv median p99) column(statistics)
 list EARNING_PC2 earnpct2 in 40/99
 tabstat INCOME [aweight=wgt], statistics(mean cv median p99) column(statistics)
 list INCOME_PC2 incpct2 in 40/81
 tabstat WEALTH [aweight=wgt], statistics(mean cv median p99) column(statistics)
 list WEALTH_PC2 wlthpct2 in 40/85
 
 *** earning ***
 *top 1%
 egen sum_wgt=sum(wgt)
 gen one_pctile_wgt=sum_wgt*0.01
 gen neg_earning = -EARNING
 sort neg_earning
 gen cum_wgt = sum(wgt)
 gen one_pct_earn_idt=.
 replace one_pct_earn_idt=1 if cum_wgt<=one_pctile_wgt
 gen earning_wgt=EARNING*wgt
 egen sum_one_pct_earn= sum(earning_wgt) if one_pct_earn_idt==1
 
 *bottom 40% 
 sort EARNING
 gen forty_pctile_wgt=sum_wgt*0.4
 gen cum_wgt_40 = sum(wgt)
 gen forty_pct_earn_idt=.
 replace forty_pct_earn_idt=1 if cum_wgt_40<=forty_pctile_wgt
 egen sum_40_pct_earn= sum(earning_wgt) if forty_pct_earn_idt==1
 
 egen sum_one_pct_earn_ready = max(sum_one_pct_earn)
 egen one_pctile_wgt_ready = max(one_pctile_wgt)
 egen sum_40_pct_earn_ready = max(sum_40_pct_earn)
 egen forty_pctile_wgt_ready = max(forty_pctile_wgt)
 gen ratio_earning=(sum_one_pct_earn_ready/one_pctile_wgt_ready)/(sum_40_pct_earn_ready/forty_pctile_wgt_ready)
 drop cum_wgt_40 cum_wgt
 
 *** income ***
 *top 1%
 *egen sum_wgt=sum(wgt)
 *gen one_pctile_wgt=sum_wgt*0.01
 gen neg_income = -INCOME
 sort neg_income
 gen cum_wgt = sum(wgt)
 gen one_pct_inc_idt=.
 replace one_pct_inc_idt=1 if cum_wgt<=one_pctile_wgt
 gen income_wgt=INCOME*wgt
 egen sum_one_pct_income= sum(income_wgt) if one_pct_inc_idt==1
 
 *bottom 40% 
 sort INCOME
 *gen forty_pctile_wgt=sum_wgt*0.4
 gen cum_wgt_40 = sum(wgt)
 gen forty_pct_inc_idt=.
 replace forty_pct_inc_idt=1 if cum_wgt_40<=forty_pctile_wgt
 egen sum_40_pct_income= sum(income_wgt) if forty_pct_inc_idt==1
 
 egen sum_one_pct_income_ready = max(sum_one_pct_income)
 *egen one_pctile_wgt_ready = max(one_pctile_wgt)
 egen sum_40_pct_income_ready = max(sum_40_pct_income)
 *egen forty_pctile_wgt_ready = max(forty_pctile_wgt)
 gen ratio_inc=(sum_one_pct_income_ready/one_pctile_wgt_ready)/(sum_40_pct_income_ready/forty_pctile_wgt_ready)
 drop cum_wgt_40 cum_wgt
 
  *** wealth ***
 *top 1%
 *egen sum_wgt=sum(wgt)
 *gen one_pctile_wgt=sum_wgt*0.01
 gen neg_wealth = -WEALTH
 sort neg_wealth
 gen cum_wgt = sum(wgt)
 gen one_pct_wth_idt=.
 replace one_pct_wth_idt=1 if cum_wgt<=one_pctile_wgt
 gen wth_wgt=WEALTH*wgt
 egen sum_one_pct_wth= sum(wth_wgt) if one_pct_wth_idt==1
 
 *bottom 40% 
 sort WEALTH
 *gen forty_pctile_wgt=sum_wgt*0.4
 gen cum_wgt_40 = sum(wgt)
 gen forty_pct_wth_idt=.
 replace forty_pct_wth_idt=1 if cum_wgt_40<=forty_pctile_wgt
 egen sum_40_pct_wth= sum(wth_wgt) if forty_pct_wth_idt==1
 
 egen sum_one_pct_wth_ready = max(sum_one_pct_wth)
 *egen one_pctile_wgt_ready = max(one_pctile_wgt)
 egen sum_40_pct_wth_ready = max(sum_40_pct_wth)
 *egen forty_pctile_wgt_ready = max(forty_pctile_wgt)
 gen ratio_wth=(sum_one_pct_wth_ready/one_pctile_wgt_ready)/(sum_40_pct_wth_ready/forty_pctile_wgt_ready)
 drop cum_wgt_40 cum_wgt
 
 *ratio_earning, ratio_income, ratio_wth are the top1%/lower40% ratio.
  
 *Variance of the logs 
 tabstat EARNING_LOG INCOME_LOG WEALTH_LOG[aweight=wgt], statistics(variance)
 
 *mean/median calculated by calculator
 *Top 1%/ lowest 40% calculated by calculator
 
 *Gini index
 ineqdeco EARNING [aweight=wgt]
 ineqdeco INCOME [aweight=wgt]
 ineqdeco WEALTH [aweight=wgt]

 *******************Lorenz Curve**************************
 glcurve7 EARNING [aweight=wgt] if EARNING>=0, gl(gl_e) p(p_e) lorenz nograph
 twoway line gl_e p_e , sort || line p_e p_e ,xlabel(0(.1)1) ylabel(0(.1)1) xline(0(.2)1) yline(0(.2)1) title("Lorenz curve of Earning") legend(label(1 "Lorenz curve") label(2 "Line of perfect equality")) plotregion(margin(zero)) aspectratio(1) scheme(economist)
 graph export lZ_curve_earning.png,replace
 
 glcurve7 INCOME [aweight=wgt] if INCOME>=0, gl(gl_i) p(p_i) lorenz nograph
 twoway line gl_i p_i , sort || line p_i p_i ,xlabel(0(.1)1) ylabel(0(.1)1) xline(0(.2)1) yline(0(.2)1) title("Lorenz curve of Income") legend(label(1 "Lorenz curve") label(2 "Line of perfect equality")) plotregion(margin(zero)) aspectratio(1) scheme(economist)
 graph export lZ_curve_income.png,replace

 
 glcurve7 WEALTH [aweight=wgt] if WEALTH>=0, gl(gl_w) p(p_w) lorenz nograph
 twoway line gl_w p_w , sort || line p_w p_w ,xlabel(0(.1)1) ylabel(0(.1)1) xline(0(.2)1) yline(0(.2)1) title("Lorenz curve of Wealth") legend(label(1 "Lorenz curve") label(2 "Line of perfect equality")) plotregion(margin(zero)) aspectratio(1) scheme(economist)
 graph export lZ_curve_wealth.png,replace
