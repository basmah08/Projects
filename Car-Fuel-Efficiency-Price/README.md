# What Properties Determine a Car's Fuel Efficiency and Value

Group project analysing a 1985 automotive dataset to determine which car characteristics drive fuel efficiency (defined as highway MPG per dollar of price) and price separately. Built two harmonised multiple linear regression models on log-transformed response variables (to correct skew and stabilise variance), selected via stepwise AIC from an initial 16 candidate predictors including engine specs, body type, weight, and manufacturer. Both models achieved strong fit (adjusted R² of 0.965 and 0.950). Key findings: turbocharged engines reduce mpg-per-dollar by ~19% while raising price ~13%; heavier/longer cars are less efficient and pricier; and brand effects are substantial (e.g. BMW priced ~31% above the reference brand). Includes full residual diagnostics and a discussion of dataset limitations (small sample, high-leverage points, no causal inference).

**Repository:** https://github.com/ytterbiu/smm634-AS-g4-assignment

**Tools:** R (stepwise regression, diagnostic plots)
