
dataset y
y.infile using /Users/aleksandarspasojevic/Desktop/BayesXShiny_test/data.raw

mcmcreg yreg

yreg.hregress y = const, family=dagum iterations=12000 burnin=2000 step=10 equationtype=p using y

yreg.hregress y = const+x1+x2(pspline,lambda=100) , family=dagum equationtype=b using y

yreg.hregress y = const+x1+x2(pspline,lambda=100) , setseed =250 predict = light family=dagum equationtype=a using y


yreg.getsample
drop y
drop yreg

logclose
