clear
infile intnr x2 pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/BayesXShiny_test/output/yreg_MAIN_a_REGRESSION_y_nonlinear_pspline_effect_of_x2.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 x2, bcolor(gs13) || rarea pqu10 pqu90 x2 , bcolor(gs10) || /*
 */ scatter pmean x2, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of x2") xtitle("x2") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/BayesXShiny_test/output/yreg_MAIN_a_REGRESSION_y_nonlinear_pspline_effect_of_x2_statagraph.eps, replace

sleep 1000

clear
infile intnr x2 pmean pstd pqu2p5 pqu10 pmed pqu90 pqu97p5 pcat95 pcat80 sim_pqu2p5 sim_pqu10 sim_pqu90 sim_pqu97p5 sim_pcat95 sim_pcat80 using /Users/aleksandarspasojevic/Desktop/BayesXShiny_test/output/yreg_MAIN_b_REGRESSION_y_nonlinear_pspline_effect_of_x2.res
drop in 1
graph twoway rarea pqu2p5 pqu97p5 x2, bcolor(gs13) || rarea pqu10 pqu90 x2 , bcolor(gs10) || /*
 */ scatter pmean x2, c(l) m(i) clpattern(l) clcolor(gs0) /* 
 */ ytitle("Effect of x2") xtitle("x2") xlab(,grid) ylab(,grid) legend(off)
graph export /Users/aleksandarspasojevic/Desktop/BayesXShiny_test/output/yreg_MAIN_b_REGRESSION_y_nonlinear_pspline_effect_of_x2_statagraph.eps, replace

sleep 1000

