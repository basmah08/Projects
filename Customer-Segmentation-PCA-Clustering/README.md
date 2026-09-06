# Beyond the Binary: Uncovering Hidden Customer Segments in Wholesale Purchasing Data

Group coursework for SMM636 (Machine Learning), Bayes Business School. Analysed annual spending data from 440 clients of a Portuguese wholesale distributor across six product categories (Fresh, Milk, Grocery, Frozen, Detergents & Paper, Delicatessen), investigating whether the distributor's existing binary HoReCa/Retail channel label captures the real structure of customer spending.

**PCA:** After log-transforming and standardising the (heavily right-skewed) spending variables, the first two principal components captured 71.3% of variance. PC1 emerged as a "channel axis" (driven by Grocery, Detergents_Paper, Milk) and PC2 as a "perishable volume axis" (Fresh, Frozen, Delicatessen), visually separating HoReCa from Retail customers.

**Clustering:** K-Means silhouette analysis favoured k=2 (recovering the known channel split) but k=3 remained strong and yielded a materially richer segmentation, isolating a distinct "Mixed Basket" segment (18% of customers) whose spending cuts across both channels. Ward's hierarchical clustering (robustness check) agreed closely with K-Means, while a Gaussian Mixture Model (BIC-selected 5-component VVE) split both channels into finer sub-segments (e.g. Heavy vs. Diversified Retail, High-volume vs. Balanced HoReCa) — all derived without access to the Channel/Region labels, later validated by cross-tabulating against them.

**LLM comparison:** As a third strand, Claude (Sonnet 4.6) and ChatGPT (GPT-5.3) were given the same four-prompt sequence and asked to independently reproduce the PCA/clustering analysis in R — without being given the raw data. Both models converged on the correct preprocessing steps and cluster count (k=3) using default reasoning alone, but neither interpreted the PCA loadings with real specificity, and both only surfaced robustness checks and limitations once explicitly prompted. The exercise highlights a practical risk: both models acknowledged the dataset is a well-known public one whose typical results likely appear in their training data, raising the question of whether their outputs reflected genuine analysis or memorised pattern-matching.

**Repository:** https://github.com/ytterbiu/SMM636-ML-group-coursework-03-g01

**Tools:** R (tidyverse, factoextra, mclust, corrplot, ggsankeyfier, kableExtra), Claude (Sonnet 4.6), ChatGPT (GPT-5.3)
