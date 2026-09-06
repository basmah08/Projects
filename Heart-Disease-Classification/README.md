# Heart Disease Classification Using Statistical and Machine Learning Methods

Analysed a 462-patient clinical dataset to classify coronary heart disease (CHD) status using nine health and lifestyle predictors (age, tobacco use, cholesterol, family history, etc.). Built a ridge logistic regression model (handling multicollinearity between predictors) and benchmarked it against KNN, LDA, decision tree, random forest, and SVM using 5-fold cross-validation. Ridge regression and LDA achieved the best discrimination (AUC ≈ 0.75), while KNN had the highest raw accuracy. Includes exploratory data analysis, correlation diagnostics, and a discussion of the sensitivity/specificity trade-off relevant to clinical use.

**Tools:** R (glmnet, caret, randomForest, e1071, pROC)
