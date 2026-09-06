# Modelling Healthcare Expenditure and Utilisation Using MEPS Data

Used Medical Expenditure Panel Survey (MEPS) data (10,638 individuals) to model two healthcare demand outcomes: expenditure on doctor visits and number of visits. Since expenditure had ~46% zero values and strong right-skew, applied a two-part model (logistic regression for any spending + Gamma GLM for spending amount among users). For visit counts, tested Poisson regression, found significant overdispersion (dispersion test p<0.001), and switched to Negative Binomial regression, which fit substantially better (AIC dropped from ~51,957 to ~37,796). Found that poor health, chronic conditions, age, and income consistently predicted higher healthcare use and spending, with clear policy implications for need-based resource allocation.

**Tools:** R (MASS, AER, broom, dplyr, ggplot2)
