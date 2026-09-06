# Shrinkage Estimation in Finance and Insurance: Portfolio Construction and Claim Severity Modelling

Group coursework for SMM069 (Advanced Predictive Analytics), Bayes Business School — two linked studies applying shrinkage estimation to real-world financial and actuarial problems.

**Part A — Portfolio construction:** Using 24 years of weekly excess returns for 100 S&P 500 companies, benchmarked eight mean-vector estimators — four distribution-free shrinkage estimators (Asimit et al., 2026) alongside Wang, BOP, Jorion, and the raw sample mean — each paired with the Ledoit-Wolf `cov1Para` covariance shrinkage estimator. Portfolios were evaluated out-of-sample using a rolling 3-year training / 1-month testing window (273 windows, no look-ahead bias), covering the full 2000-2023 period plus the 2008 financial crisis and COVID-19 sub-periods. Results showed shrinkage-based mean estimators (notably O-LSh and D-MSh) roughly doubled the raw sample mean's Sharpe ratio, but a naive equal-weighted (1/N) portfolio remained very hard to beat once leverage and turnover costs were accounted for — consistent with the classic DeMiguel et al. (2009) result.

**Part B — Insurance claim severity:** Modelled individual claim severity on 1,000 policies from the French MTPL dataset using nine Gamma GLM variants — a standard IRLS baseline, ridge/elastic net (glmnet), and six shrinkage GLMs from the `savvyGLM` package (Asimit et al., 2025). Evaluated via 5-fold cross-validation repeated 100 times (500 runs). Ridge regression was selected as best overall (lowest deviance, best winsorised RMSE), reflecting the near-total absence of rankable severity signal at this sample size — a finding that in-sample diagnostics on the full sample independently confirmed (the fitted model compressed to a near-constant intercept).

Includes extensive robustness/sensitivity appendices (variable caps, spline flexibility, target-encoding smoother, covariance conditioning, tangency sign-flip diagnostics) and an interactive R Shiny dashboard letting users re-run the Part A analysis under different group seeds, sample sizes, window lengths, and covariance/mean-estimator choices.

**Live dashboard:** https://3enji-apps.shinyapps.io/smm069-g12-part-a-dashboard/
**Report site:** https://ytterbiu.github.io/smm069-assignment_g12/

**Tools:** R (glmnet, savvyGLM, shiny, plotly, ggplot2), R Markdown
