clear
infile intnr cage pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_nonlinear_pspline_effect_of_cage.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 cage, bcolor(gs13) || rarea pqu10 pqu90 cage , bcolor(gs10) || /*
 */ scatter pmean cage, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of cage") xtitle("cage") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_nonlinear_pspline_effect_of_cage_statagraph.eps, replace

sleep 1000

clear
infile intnr breastfeeding pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_nonlinear_pspline_effect_of_breastfeeding.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 breastfeeding, bcolor(gs13) || rarea pqu10 pqu90 breastfeeding , bcolor(gs10) || /*
 */ scatter pmean breastfeeding, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of breastfeeding") xtitle("breastfeeding") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_nonlinear_pspline_effect_of_breastfeeding_statagraph.eps, replace

sleep 1000

clear
infile intnr mbmi pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_nonlinear_pspline_effect_of_mbmi.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 mbmi, bcolor(gs13) || rarea pqu10 pqu90 mbmi , bcolor(gs10) || /*
 */ scatter pmean mbmi, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of mbmi") xtitle("mbmi") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_nonlinear_pspline_effect_of_mbmi_statagraph.eps, replace

sleep 1000

clear
infile intnr mage pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_nonlinear_pspline_effect_of_mage.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 mage, bcolor(gs13) || rarea pqu10 pqu90 mage , bcolor(gs10) || /*
 */ scatter pmean mage, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of mage") xtitle("mage") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_nonlinear_pspline_effect_of_mage_statagraph.eps, replace

sleep 1000

clear
infile intnr medu pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_nonlinear_pspline_effect_of_medu.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 medu, bcolor(gs13) || rarea pqu10 pqu90 medu , bcolor(gs10) || /*
 */ scatter pmean medu, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of medu") xtitle("medu") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_nonlinear_pspline_effect_of_medu_statagraph.eps, replace

sleep 1000

clear
infile intnr edupartner pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_nonlinear_pspline_effect_of_edupartner.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 edupartner, bcolor(gs13) || rarea pqu10 pqu90 edupartner , bcolor(gs10) || /*
 */ scatter pmean edupartner, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of edupartner") xtitle("edupartner") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_nonlinear_pspline_effect_of_edupartner_statagraph.eps, replace

sleep 1000

clear
infile intnr distH pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_spatial_MRF_effect_of_distH.res
drop in 1
kdensity pmean
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_stunting2_spatial_MRF_effect_of_distH_statagraph.eps, replace

sleep 1000

clear
infile intnr cage pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_nonlinear_pspline_effect_of_cage.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 cage, bcolor(gs13) || rarea pqu10 pqu90 cage , bcolor(gs10) || /*
 */ scatter pmean cage, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of cage") xtitle("cage") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_nonlinear_pspline_effect_of_cage_statagraph.eps, replace

sleep 1000

clear
infile intnr breastfeeding pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_nonlinear_pspline_effect_of_breastfeeding.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 breastfeeding, bcolor(gs13) || rarea pqu10 pqu90 breastfeeding , bcolor(gs10) || /*
 */ scatter pmean breastfeeding, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of breastfeeding") xtitle("breastfeeding") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_nonlinear_pspline_effect_of_breastfeeding_statagraph.eps, replace

sleep 1000

clear
infile intnr mbmi pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_nonlinear_pspline_effect_of_mbmi.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 mbmi, bcolor(gs13) || rarea pqu10 pqu90 mbmi , bcolor(gs10) || /*
 */ scatter pmean mbmi, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of mbmi") xtitle("mbmi") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_nonlinear_pspline_effect_of_mbmi_statagraph.eps, replace

sleep 1000

clear
infile intnr mage pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_nonlinear_pspline_effect_of_mage.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 mage, bcolor(gs13) || rarea pqu10 pqu90 mage , bcolor(gs10) || /*
 */ scatter pmean mage, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of mage") xtitle("mage") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_nonlinear_pspline_effect_of_mage_statagraph.eps, replace

sleep 1000

clear
infile intnr medu pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_nonlinear_pspline_effect_of_medu.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 medu, bcolor(gs13) || rarea pqu10 pqu90 medu , bcolor(gs10) || /*
 */ scatter pmean medu, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of medu") xtitle("medu") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_nonlinear_pspline_effect_of_medu_statagraph.eps, replace

sleep 1000

clear
infile intnr edupartner pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_nonlinear_pspline_effect_of_edupartner.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 edupartner, bcolor(gs13) || rarea pqu10 pqu90 edupartner , bcolor(gs10) || /*
 */ scatter pmean edupartner, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of edupartner") xtitle("edupartner") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_nonlinear_pspline_effect_of_edupartner_statagraph.eps, replace

sleep 1000

clear
infile intnr distH3 pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_spatial_MRF_effect_of_distH3.res
drop in 1
kdensity pmean
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_mu_REGRESSION_wasting2_spatial_MRF_effect_of_distH3_statagraph.eps, replace

sleep 1000

clear
infile intnr distH5 pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_stunting2_spatial_MRF_effect_of_distH5.res
drop in 1
kdensity pmean
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_stunting2_spatial_MRF_effect_of_distH5_statagraph.eps, replace

sleep 1000

clear
infile intnr cage pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_stunting2_nonlinear_pspline_effect_of_cage.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 cage, bcolor(gs13) || rarea pqu10 pqu90 cage , bcolor(gs10) || /*
 */ scatter pmean cage, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of cage") xtitle("cage") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_stunting2_nonlinear_pspline_effect_of_cage_statagraph.eps, replace

sleep 1000

clear
infile intnr medu pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_stunting2_nonlinear_pspline_effect_of_medu.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 medu, bcolor(gs13) || rarea pqu10 pqu90 medu , bcolor(gs10) || /*
 */ scatter pmean medu, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of medu") xtitle("medu") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_stunting2_nonlinear_pspline_effect_of_medu_statagraph.eps, replace

sleep 1000

clear
infile intnr breastfeeding pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_stunting2_nonlinear_pspline_effect_of_breastfeeding.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 breastfeeding, bcolor(gs13) || rarea pqu10 pqu90 breastfeeding , bcolor(gs10) || /*
 */ scatter pmean breastfeeding, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of breastfeeding") xtitle("breastfeeding") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_stunting2_nonlinear_pspline_effect_of_breastfeeding_statagraph.eps, replace

sleep 1000

clear
infile intnr mage pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_stunting2_nonlinear_pspline_effect_of_mage.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 mage, bcolor(gs13) || rarea pqu10 pqu90 mage , bcolor(gs10) || /*
 */ scatter pmean mage, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of mage") xtitle("mage") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_stunting2_nonlinear_pspline_effect_of_mage_statagraph.eps, replace

sleep 1000

clear
infile intnr mbmi pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_stunting2_nonlinear_pspline_effect_of_mbmi.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 mbmi, bcolor(gs13) || rarea pqu10 pqu90 mbmi , bcolor(gs10) || /*
 */ scatter pmean mbmi, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of mbmi") xtitle("mbmi") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_stunting2_nonlinear_pspline_effect_of_mbmi_statagraph.eps, replace

sleep 1000

clear
infile intnr distH7 pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_wasting2_spatial_MRF_effect_of_distH7.res
drop in 1
kdensity pmean
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_wasting2_spatial_MRF_effect_of_distH7_statagraph.eps, replace

sleep 1000

clear
infile intnr cage pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_wasting2_nonlinear_pspline_effect_of_cage.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 cage, bcolor(gs13) || rarea pqu10 pqu90 cage , bcolor(gs10) || /*
 */ scatter pmean cage, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of cage") xtitle("cage") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_wasting2_nonlinear_pspline_effect_of_cage_statagraph.eps, replace

sleep 1000

clear
infile intnr mbmi pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_wasting2_nonlinear_pspline_effect_of_mbmi.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 mbmi, bcolor(gs13) || rarea pqu10 pqu90 mbmi , bcolor(gs10) || /*
 */ scatter pmean mbmi, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of mbmi") xtitle("mbmi") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_sigma_REGRESSION_wasting2_nonlinear_pspline_effect_of_mbmi_statagraph.eps, replace

sleep 1000

clear
infile intnr cage pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_rho_REGRESSION_stunting2_nonlinear_pspline_effect_of_cage.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 cage, bcolor(gs13) || rarea pqu10 pqu90 cage , bcolor(gs10) || /*
 */ scatter pmean cage, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of cage") xtitle("cage") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_rho_REGRESSION_stunting2_nonlinear_pspline_effect_of_cage_statagraph.eps, replace

sleep 1000

clear
infile intnr distH9 pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_rho_REGRESSION_stunting2_spatial_MRF_effect_of_distH9.res
drop in 1
kdensity pmean
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_rho_REGRESSION_stunting2_spatial_MRF_effect_of_distH9_statagraph.eps, replace

sleep 1000

clear
infile intnr mage pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_rho_REGRESSION_stunting2_nonlinear_pspline_effect_of_mage.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 mage, bcolor(gs13) || rarea pqu10 pqu90 mage , bcolor(gs10) || /*
 */ scatter pmean mage, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of mage") xtitle("mage") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_rho_REGRESSION_stunting2_nonlinear_pspline_effect_of_mage_statagraph.eps, replace

sleep 1000

clear
infile intnr breastfeeding pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_rho_REGRESSION_stunting2_nonlinear_pspline_effect_of_breastfeeding.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 breastfeeding, bcolor(gs13) || rarea pqu10 pqu90 breastfeeding , bcolor(gs10) || /*
 */ scatter pmean breastfeeding, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of breastfeeding") xtitle("breastfeeding") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/template_bivariate/results/NV_s1_di_ca_me_mb_br_ma_s2_di_ca_mb_br_rho_ca_di_ma_br_MAIN_rho_REGRESSION_stunting2_nonlinear_pspline_effect_of_breastfeeding_statagraph.eps, replace

sleep 1000

