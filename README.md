# Option-pricing
Estimates of the theoretical Call and Put options prices, using R language.

 I will estimate the theoretical prices of the Europian options of Goldman Sachs company, and confront them with the real prices available in the market. In order to do so, I will
start considering the Black&Scholes model, and I will test the assumptions which this model implies.
Even if the assumptions should not be fulfilled, however, I will estimate the theoretical price, using the B&S framework, and I will use these as benchmarks, for evaluating others models. Considering
the market price, I want to know which is the volatility implied in a B&S framework, and confront it, with the historical volatility.
I’ll move further, tring to estimate a stochastic process which fits better the dynamics of our data, and I will try to estimate the theoretical options price, using it.
I will end with the confront of the estimated prices using B&S framework, the estimated stochastic proces, and the prices available in the market.

The data set is the time series of the Goldman Sachs’ prices stock (from 17/12/2015 to 15/12/2016 ).
