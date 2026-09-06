# Pricing US Flood Risk at Underwriting: Can Property-Level Features Beat Flood-Zone-Band Rating?

Group coursework for SMM284 (Applied Machine Learning), Bayes Business School — framed as a pricing review for a Lloyd's syndicate writing US property-catastrophe flood reinsurance. The question: using only information knowable *before* a flood occurs, can a model beat the industry-standard practice of pricing purely off FEMA flood-zone band?

Built on ~2.7M FEMA NFIP claims records (National Flood Insurance Program, OpenFEMA bulk data), modelling **claim severity** (the dataset is claims-conditional, so frequency is out of scope and explicitly flagged as a follow-on workstream). Compared five approaches:

- Trivial baselines (global mean/median) and a **flood-zone mean-payout proxy** (the incumbent approach)
- **Gamma GLM** with log link (the industry-standard severity model)
- **Gradient boosting** (`HistGradientBoostingRegressor`, gamma loss)
- **Random Forest** on a smeared log target (bagging, as a second ensemble family)

Rigour was a central theme: a strict underwriting-time-only feature set (machine-checked leakage audit), inflation-adjusted (CPI-deflated to constant 2024 USD) severities, predictions clipped at NFIP's statutory coverage limits, and — critically — **out-of-time validation** (train pre-2020, test 2020+, year-grouped CV) rather than a random split, since claims from the same flood event must never straddle train/test. This exposed a large "optimism gap": a naive random split suggested a ~29% MAE improvement over the zone baseline, which collapsed to a robust ~5.3% once evaluated out-of-time — the number the report treats as decision-relevant. Model interpretation used SHAP values, permutation importance (computed out-of-time to filter out memorised rather than transferable signal), GLM coefficients as multiplicative severity effects, and segmented error analysis by flood zone and state.

**Key findings:** the property-level model ranks risk meaningfully better than the zone baseline (Gini 0.33 vs 0.09) and reveals a ~3.3x within-zone severity spread the flat rate erases — useful as an independent read on a ceded book's risk composition — but cannot reliably price the extreme tail, and the learning curve flattens well before the full dataset is used (more *features*, not more *history*, is the constraint). The report closes with a structured set of committee recommendations (commission a frequency model, treat this as a discrimination check rather than a rating tool, invest in richer features, monitor for drift) and a discussion of ethical/practical limitations spanning selection bias, claim immaturity, climate non-stationarity, and fairness/disparate-impact concerns tied to NFIP's documented geographic and demographic disparities.

The notebook uses "Reflection" boxes throughout to transparently document points where an earlier modelling or business-framing decision was found to be wrong, and what was learned from it.

**Presentation video:** https://youtu.be/B8rsYvt4Pxk (unlisted)

**Tools:** Python (scikit-learn, SHAP, pandas/polars, plotly, matplotlib), Jupyter
