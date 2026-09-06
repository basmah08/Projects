# Tree-Based Methods for Heart Disease Classification: An Interpretability Case Study

Group coursework for SMM636 (Machine Learning), Bayes Business School — framed as a consulting pitch demonstrating tree-based ML methods to non-technical clients, using heart disease classification as an illustrative case study transferable to actuarial applications like claims triage and fraud detection.

Compared three tree-based approaches on the Cleveland Heart Disease dataset (303 patients, 13 clinical predictors, 54%/46% class balance) using a 70/30 train-test split:

- **Decision Tree** — single pruned tree (rpart, 10-fold CV), fully interpretable as a flowchart
- **Random Forest** — bootstrap-aggregated ensemble with majority voting
- **XGBoost** — sequentially boosted shallow trees

On unseen test data, the ensembles outperformed the single tree on discrimination (Random Forest: 82.6% accuracy, 0.878 specificity; XGBoost: 81.5% accuracy, 0.880 specificity) versus the Decision Tree's 79.3% accuracy — but at the cost of direct interpretability. To bridge this gap, SHAP (SHapley Additive exPlanations) values were used to decompose XGBoost's predictions into additive per-feature contributions, with summary and beeswarm plots showing which clinical features (chest pain type, thalassemia status, sex) drove predictions and in which direction.

Two interactive R Shiny applications were built: an educational multi-tab walkthrough of the full modelling pipeline (problem framing → data → tree building → ensembles → takeaways) for teaching/presentation audiences, and a standalone clinical decision tool letting practitioners input a patient's profile and trace their predicted path through the decision tree in real time.

**Live apps:**
- Educational walkthrough: https://3enji.shinyapps.io/smm636-a01-tree-based-methods/
- Clinical decision tool: https://3enji.shinyapps.io/smm636-a01-tree-based-methods-standalone-tab4/

**Repository:** https://github.com/ytterbiu/SMM636-ML-group-coursework-01-g01

**Tools:** R (rpart, randomForest, xgboost, SHAPforxgboost, shiny, bslib, ggplot2)
