# ============================================================================ #
# Key Information ====
# SMM636 Machine Learning
# Group Coursework 2025-26
# Group:        Group 01
# Authors (in alphabetical order):
#   - Abdulrahman Alolyan
#   - Benjamin Evans
#   - Basmah Khan
#   - Ardi Wira Sudarmo
# Professor:    Dr. Rui Zhu
# Institution:  Bayes Business School - City St George's, University of London
# Date:         03 Apr 2026
#
# Description:  Term 2 group project for SMM636 Machine Learning
#               (25% of coursework grade - 25% of module grade). The R code
#               below has been exported directly from an R Markdown (.rmd) file.
#               Hence the knitr settings.
#
# Dependencies:
#   - tidyverse:     dplyr, tidyr, ggplot2, purrr, stringr, readr, forcats, tibble
#   - patchwork:     stacking/combining ggplot objects (/ and | operators)
#   - corrplot:      correlation matrix heatmaps
#   - factoextra:    PCA scree plots, silhouette plots, fviz_* family
#   - mclust:        Gaussian Mixture Modelling (Mclust, BIC selection)
#   - ggsankeyfier:  Sankey diagram geoms for ggplot2
#   - Polychrome:    automatic palette generation (createPalette)
#   - kableExtra:    LaTeX/HTML table formatting (kable_styling, pack_rows)
#   - callr:         isolated R subprocesses for LLM script execution
#   - e1071:         skewness function (called via e1071::skewness)
#
# NOT loaded (namespace-only to avoid masking dplyr::select):
#   - cluster:       called as cluster::daisy(), cluster::silhouette()
#   - MASS:          called as MASS::isoMDS()
# ============================================================================ #

# Setup & configuration ====
## Knitr settings ----
dir.create("fig", showWarnings = FALSE)

# Defaults common to all outputs
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.align = "center",
  out.width = "100%",
  fig.path = "fig/",
  dpi = 300
)

# Output-specific settings
if (knitr::is_latex_output()) {
  knitr::opts_chunk$set(
    fig.width = 6,
    fig.height = 4,
    dev = "pdf",
    fig.pos = "!ht",
    out.extra = ""
  )
} else {
  knitr::opts_chunk$set(
    fig.width = 6,
    fig.height = 4,
    dev = "svglite" # or "png"
  )
}

## Clean environment ----
rm(list = ls()) # Remove all objects
graphics.off() # Close all graphical devices
cat("\014") # Clean console

## Load dependencies ----
library(tidyverse) # dplyr, tidyr, ggplot2, purrr, stringr, readr, forcats
library(patchwork) # stacking/combining ggplot objects (/ and | operators)
library(corrplot) # correlation matrix heatmaps
library(factoextra) # PCA scree plots, silhouette plots, fviz_* family
library(mclust) # Gaussian Mixture Modelling (Mclust, BIC selection)
library(ggsankeyfier) # Sankey diagram geoms for ggplot2
library(Polychrome) # automatic palette generation (createPalette)
library(kableExtra) # LaTeX/HTML table formatting (kable_styling, pack_rows)
library(callr) # isolated R subprocesses for LLM script execution
library(ggpubr) #table in ggplot quad figure

## Custom helper functions ----
# ***********************************
# function: banner comments (used to to section up code)
# Usage: banner_comment("Element 1: data cleaning") -> then ctrl + v (or cmd+v)
# ***********************************
banner_comment <- function(text, width = 80, border = "#", fill = "-") {
  txt <- paste0(" ", text, " ")
  inner_width <- width - 2 * nchar(border)
  banner_string <- ""

  if (inner_width <= nchar(txt)) {
    banner_string <- paste0(border, txt, border)
  } else {
    pad_total <- inner_width - nchar(txt)
    pad_left <- pad_total %/% 2
    pad_right <- pad_total - pad_left

    banner_string <- paste0(
      border,
      strrep(fill, pad_left),
      txt,
      strrep(fill, pad_right),
      border
    )
  }

  cat(banner_string, "\n")
  # copy banner to allow direct pasting (requires clipr)
  clipr::write_clip(banner_string)
  # avoid [1] when printing if want to manually copy
  invisible(banner_string)
}
# ***********************************
# function: format p-values for text
# Usage (in-line): `r format_p_vals(ad_test_result$p.value)`
# Usage (console): format_p_vals(ad_test_result$p.value)
# ***********************************
format_p_vals <- function(p) {
  if (length(p) != 1L || is.na(p)) {
    stop("Error! p must be a single non-missing value")
  }
  if (p > 1) {
    stop("Error! Value greater than 1")
  }
  if (p < 0) {
    stop("Error! Value less than 0")
  }

  if (p >= 0.01) {
    paste0("= ", formatC(p, format = "f", digits = 2))
  } else if (p >= 0.001) {
    paste0("= ", formatC(p, format = "f", digits = 3))
  } else {
    "< 0.001"
  }
}
# ***********************************
# function: format confidence intervals for tables & text
# Usage (in-line): `r format_interval(el2_ci_normal_95[1], el2_ci_normal_95[2])`
# Usage (console): format_interval(el2_ci_normal_95[1], el2_ci_normal_95[2])
# ***********************************
format_interval <- function(lower, upper, digits = 3, small_interval = 5L) {
  paste0(
    "[",
    formatC(
      lower,
      format = "f",
      digits = digits,
      small.interval = small_interval,
      small.mark = " "
    ),
    ", ",
    formatC(
      upper,
      format = "f",
      digits = digits,
      small.interval = small_interval,
      small.mark = " "
    ),
    "]"
  )
}
# ***********************************
# function: format truncated ellipses
# Usage (in-line): `r tbi`
# Usage (console): tbi
# ***********************************
fmt_trunc_ellip <- function(
  x,
  digits = 7,
  ellip = "...",
  tol = 1e-12,
  trim_zeros_if_exact = TRUE
) {
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  scale <- 10^digits
  xt <- trunc(x[ok] * scale) / scale
  s <- formatC(xt, format = "f", digits = digits)
  add <- abs(x[ok] - xt) > tol * pmax(1, abs(x[ok]))
  if (trim_zeros_if_exact) {
    s_trim <- sub("0+$", "", s) # drop trailing zeros
    s_trim <- sub("\\.$", "", s_trim) # drop trailing decimal point
  } else {
    s_trim <- s
  }
  out[ok] <- ifelse(add, paste0(s, ellip), s_trim)
  out[is.na(x)] <- NA_character_
  out
}
# ***********************************
# function: function to format 6dp numbers with a space after the 3rd decimal
# digit
# Usage (in-line): TBI
# Usage (console): TBI
# ***********************************
format_spaced_6dp <- function(x) {
  # Format to 6 decimal places
  s <- sprintf("%.6f", x)
  # Insert space between the first 3 and last 3 decimal digits
  sub("(\\.[0-9]{3})([0-9]{3})$", "\\1 \\2", s)
}
# ***********************************
# function: format variable names in green in latex (& normal code in html)
# Usage (in-line): TBI `r format_var_name("dvisit")`
# ***********************************
format_var_name <- function(x) {
  if (knitr::is_latex_output()) {
    # replace all "_" with "\_" in va name
    paste0("\\greentt{", gsub("_", "\\\\_", x), "}")
  } else {
    # change to paste0("<code style='color:green'>", x, "</code>") for green
    # paste0("`", x, "`")
    paste0("<code style='color:green'>", x, "</code>")
  }
}
#
# ## Word count ----
# rmd_lines <- readLines("../01SMM363-assessment-03.rmd", warn = FALSE)
#
# # Remove YAML front matter
# yaml_bounds <- which(rmd_lines == "---")
# if (length(yaml_bounds) >= 2) {
#   rmd_lines <- rmd_lines[-(yaml_bounds[1]:yaml_bounds[2])]
# }
#
# # Remove code chunks
# in_chunk <- FALSE
# keep <- rep(TRUE, length(rmd_lines))
# for (i in seq_along(rmd_lines)) {
#   if (grepl("^```\\{", rmd_lines[i])) {
#     in_chunk <- TRUE
#     keep[i] <- FALSE
#     next
#   }
#   if (in_chunk && grepl("^```\\s*$", rmd_lines[i])) {
#     in_chunk <- FALSE
#     keep[i] <- FALSE
#     next
#   }
#   if (in_chunk) keep[i] <- FALSE
# }
# rmd_lines <- rmd_lines[keep]
#
# # Identify which lines fall inside counted sections
# # Sections to include (level-1 headers only)
# counted_ids <- c("intro", "qone", "qtwo", "qthr")
# in_counted_section <- FALSE
# include <- rep(FALSE, length(rmd_lines))
#
# for (i in seq_along(rmd_lines)) {
#   line <- rmd_lines[i]
#   # Detect any level-1 header (# but not ##)
#   if (grepl("^#\\s+[^#]", line)) {
#     # Check if this header belongs to a counted section
#     in_counted_section <- any(sapply(counted_ids, function(id) {
#       # [^}]* allows any characters (like spaces, hyphens, classes) before the
#       # closing }
#       # this is required to count the words in the unnumbered introduction
#       grepl(paste0("\\{\\s*#", id, "[^}]*\\}"), line)
#     }))
#     next # skip the header line itself
#   }
#   if (in_counted_section) include[i] <- TRUE
# }
#
# counted_lines <- rmd_lines[include]
#
# # Remove sub-headers (##, ###, etc.)
# counted_lines <- counted_lines[!grepl("^#{2,}", counted_lines)]
#
# # Remove table lines (markdown pipes)
# counted_lines <- counted_lines[!grepl("^\\|", counted_lines)]
#
# # Remove figure captions: ![caption](...)
# counted_lines <- counted_lines[!grepl("^!\\[", counted_lines)]
#
# # Remove fenced div markers (::: name)
# counted_lines <- counted_lines[!grepl("^:::", counted_lines)]
#
# # Remove LaTeX commands
# counted_lines <- counted_lines[!grepl("^\\\\", counted_lines)]
#
# # Remove HTML comments
# counted_lines <- counted_lines[!grepl("^<!--", counted_lines)]
#
# # Remove inline R references like \@ref()
# counted_lines <- gsub("\\\\@ref\\([^)]*\\)", "", counted_lines)
#
# # Count
# word_count <- sum(stringi::stri_count_words(counted_lines), na.rm = TRUE)
#
# message("Word count (sections qone, qtwo, qthr, body text only): ", word_count)
#
# # Write for LaTeX preamble
# writeLines(
#   paste0(
#     "\\newcommand{\\wordcount}{",
#     formatC(word_count, big.mark = ","),
#     "}"
#   ),
#   "wordcount.tex"
# )

## Load data ----
set.seed(1234)

data <- read.csv("data/Wholesale_customers_data.csv")
str(data)
summary(data)

# just get the $ amounts spent
spending <- data[, c(
  "Fresh",
  "Milk",
  "Grocery",
  "Frozen",
  "Detergents_Paper",
  "Delicassen"
)]

spending_log <- log1p(spending)

rescale01 <- function(x) (x - min(x)) / (max(x) - min(x))

raw_df <- spending %>%
  pivot_longer(everything(), names_to = "Category", values_to = "Spend") %>%
  mutate(Transform = "Raw")

log_df <- spending_log %>%
  pivot_longer(everything(), names_to = "Category", values_to = "Spend") %>%
  mutate(Transform = "Log-transformed")

# add data to import into in-line text
# standardise log-transformed data
scaled_data <- scale(spending_log)

# PCA
pca_result <- prcomp(spending_log, center = TRUE, scale. = TRUE)

# Skewness (raw)
skew_vals <- apply(spending, 2, e1071::skewness)
min_skew <- min(skew_vals)

DT::datatable(
  data,
  filter = "top",
  options = list(pageLength = 15, scrollX = TRUE),
  caption = "Wholesale customers"
)


# ******************************************************************************
# Q1: Principal Component Analysis ====
# ******************************************************************************
## PCA computation ----
pca_result <- prcomp(spending_log, center = TRUE, scale. = TRUE)
summary(pca_result)

cumvar <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
cat("Cumulative variance explained:\n")
print(round(cumvar, 3))

loadings <- pca_result$rotation[, 1:3]
print(round(loadings, 3))

## Individual plots (exploratory) ----
fviz_eig(
  pca_result,
  addlabels = TRUE,
  ylim = c(0, 60),
  main = "Scree Plot - Variance Explained by Each Principle Component (PC)"
)

fviz_pca_biplot(
  pca_result,
  label = "var",
  col.var = "firebrick",
  col.ind = alpha("steelblue", 0.3),
  repel = TRUE,
  title = "PCA Biplot (log-transformed, standardised data)"
)

# Opted to hide these for now
# (don't think we have space and they don't add too much)
# Even including in appendix is limited - easier to include table
fviz_contrib(
  pca_result,
  choice = "var",
  axes = 1,
  title = "Variable Contributions to PC1"
)

fviz_contrib(
  pca_result,
  choice = "var",
  axes = 2,
  title = "Variable Contributions to PC2"
)

loadings_df <- as.data.frame(round(pca_result$rotation[, 1:3], 3))
colnames(loadings_df) <- c("PC1", "PC2", "PC3")

kable(
  loadings_df,
  caption = "PCA loadings (first three components). Values over 0.4 are highlighted in bold.",
  linesep = "",
  booktabs = TRUE
) %>%
  kable_styling(
    full_width = FALSE,
    position = "center"
  ) %>%
  column_spec(2, bold = abs(loadings_df$PC1) >= 0.4) %>%
  column_spec(3, bold = abs(loadings_df$PC2) >= 0.4) %>%
  column_spec(4, bold = abs(loadings_df$PC3) >= 0.4)

fviz_pca_ind(
  pca_result,
  geom.ind = "point",
  col.ind = factor(data$Channel),
  palette = c("#E64B35", "#4DBBD5"),
  addEllipses = TRUE,
  legend.title = "Channel",
  title = "PCA - Individuals Coloured by Channel (1=HoReCa, 2=Retail)"
)

## Combined 2x2 figure (for report) ----
# --- Top left: Scree plot ---
p1 <- fviz_eig(
  pca_result,
  addlabels = TRUE,
  ylim = c(0, 60),
  main = "(a) Scree Plot - Variance Explained by Principle Component (PC)"
)

# --- Top right: Loadings table as a grob ---
loadings_df <- as.data.frame(round(pca_result$rotation[, 1:3], 3))
colnames(loadings_df) <- c("PC1", "PC2", "PC3")

# --- Top right: Loadings table ---
loadings_df <- as.data.frame(pca_result$rotation[, 1:4])
colnames(loadings_df) <- c("PC1", "PC2", "PC3", "PC4")
# loadings_df <- as.data.frame(pca_result$rotation[, 1:3])
# colnames(loadings_df) <- c("PC1", "PC2", "PC3")
loadings_df[] <- lapply(loadings_df, function(x) {
  format(round(x, 3), nsmall = 3)
})

p2 <- ggtexttable(
  loadings_df,
  theme = ttheme(
    "blank",
    tbody.style = tbody_style(
      size = 13,
      hjust = 1.00, #controls alignment {0.00, 0.5, 1.00}
      x = 0.95, #controls alignment {0.05, 0.5, 0.95}
      fill = "white"
    ),
    colnames.style = colnames_style(
      size = 13,
      face = "bold",
      hjust = 1.00,
      x = 0.95,
      fill = "white"
    ),
    rownames.style = rownames_style(
      size = 13,
      face = "plain",
      hjust = 0,
      x = 0.05,
      fill = "white"
    )
  )
) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>%
  tab_add_hline(
    at.row = nrow(loadings_df) + 1,
    row.side = "bottom",
    linewidth = 2
  ) +
  labs(title = "(b) PCA Loadings") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# --- Bottom left: Biplot ---
p3 <- fviz_pca_biplot(
  pca_result,
  label = "none",
  col.var = "firebrick",
  col.ind = alpha("steelblue", 0.3),
  title = "(c) PCA Biplot (log-transformed, standardised data)"
)

# Pull arrow endpoints from layer 4 of the built plot
arrows <- ggplot_build(p3)$data[[4]]

var_df <- data.frame(
  x = arrows$xend,
  y = arrows$yend,
  label = rownames(pca_result$rotation)
)

p3 <- p3 +
  ggrepel::geom_text_repel(
    data = var_df,
    aes(x = x, y = y, label = label),
    colour = "firebrick",
    size = 3.5,
    bg.color = "white",
    bg.r = 0.15,
    inherit.aes = FALSE
  )

# --- Bottom right: Channel plot ---
p4 <- fviz_pca_ind(
  pca_result,
  geom.ind = "point",
  col.ind = factor(data$Channel),
  palette = c("#E64B35", "#4DBBD5"),
  addEllipses = TRUE,
  legend.title = "Channel",
  title = "(d) PCA - Individuals Coloured by Channel (1=HoReCa, 2=Retail)"
)

# BE note: qol to get bottom two plots to align vertically
# ggplot_build(p3)$layout$panel_params[[1]]$y.range
# ggplot_build(p4)$layout$panel_params[[1]]$y.range

p3 <- p3 + coord_cartesian(ylim = c(-5.58, 5.24))
p4 <- p4 + coord_cartesian(ylim = c(-5.58, 5.24))

# --- Combine 2x2 ---
(p1 | p2) / (p3 | p4)

# ******************************************************************************
# Q2: Customer Clustering ====
# ******************************************************************************

## K-Means & cluster labelling ----
spending.cols <- c(
  "Fresh",
  "Milk",
  "Grocery",
  "Frozen",
  "Detergents_Paper",
  "Delicassen"
)

# K-Means for k=2 through 5
KMeans.results <- list()
for (k in 2:5) {
  KMeans.results[[k]] <- kmeans(scaled_data, centers = k, nstart = 25)
}

# Assign K-Means labels programmatically
km3 <- KMeans.results[[3]]
km3_ch_tab <- table(km3$cluster, data$Channel)
horeca_pct <- km3_ch_tab[, 1] / rowSums(km3_ch_tab)

horeca_cluster <- as.character(which.max(horeca_pct))
retail_cluster <- as.character(which.min(horeca_pct))
mixed_cluster <- as.character(setdiff(1:3, c(horeca_cluster, retail_cluster)))

km3_names <- setNames(
  c("HoReCa Dominant", "Retail Dominant", "Mixed Basket"),
  c(horeca_cluster, retail_cluster, mixed_cluster)
)
km3_colours <- setNames(
  c("#B2182B", "#2166AC", "#FDB863"),
  c(horeca_cluster, retail_cluster, mixed_cluster)
)
km3_order <- c(retail_cluster, mixed_cluster, horeca_cluster)

## Silhouette scores ----
# (referenced inline)
sil_scores <- sapply(2:10, function(k) {
  km <- kmeans(scaled_data, centers = k, nstart = 25)
  sil <- cluster::silhouette(km$cluster, dist(scaled_data))
  mean(sil[, 3])
})
names(sil_scores) <- 2:10

## Gaussian Mixture Model ----
# GMM on spending data (referenced inline for $G, $modelName, $bic)
gmm_spending <- Mclust(data = scaled_data)

# BIC comparison table (referenced inline)
bic_vals <- gmm_spending$BIC
best_per_k <- apply(bic_vals, 1, max, na.rm = TRUE)

## Ward's hierarchical clustering ----
# Ward's hierarchical clustering (referenced in main text)
dist_mat <- dist(scaled_data, method = "euclidean")
hc <- hclust(dist_mat, method = "ward.D2")
hc_clusters <- cutree(hc, k = 3)

# Shorthand references used in main text ====
km3 <- KMeans.results[[3]]
km2 <- KMeans.results[[2]]
data$km3_cluster <- km3$cluster
data$km2_cluster <- km2$cluster
data$gmm_cluster <- as.integer(gmm_spending$classification)

## Cluster profile statistics ----
### K-Means medians & revenue ----
km3_medians <- aggregate(
  cbind(
    Fresh,
    Milk,
    Grocery,
    Frozen,
    Detergents_Paper,
    Delicassen
  ) ~ km3_cluster,
  data = data,
  FUN = median
)
km3_medians$total <- rowSums(km3_medians[, spending.cols])
for (col in spending.cols) {
  km3_medians[[paste0(col, "_pct")]] <- round(
    km3_medians[[col]] / km3_medians$total * 100,
    1
  )
}

# Revenue concentration % per cluster (K-Means)
km3_sums_all <- aggregate(
  cbind(
    Fresh,
    Milk,
    Grocery,
    Frozen,
    Detergents_Paper,
    Delicassen
  ) ~ km3_cluster,
  data = data,
  FUN = sum
)
for (col in spending.cols) {
  km3_sums_all[[paste0(col, "_rev_pct")]] <- round(
    km3_sums_all[[col]] / sum(km3_sums_all[[col]]) * 100,
    1
  )
}

### GMM medians & revenue ----
# Same for GMM
gmm_medians <- aggregate(
  cbind(
    Fresh,
    Milk,
    Grocery,
    Frozen,
    Detergents_Paper,
    Delicassen
  ) ~ gmm_cluster,
  data = data,
  FUN = median
)
gmm_medians$total <- rowSums(gmm_medians[, spending.cols])
for (col in spending.cols) {
  gmm_medians[[paste0(col, "_pct")]] <- round(
    gmm_medians[[col]] / gmm_medians$total * 100,
    1
  )
}

gmm_sums_all <- aggregate(
  cbind(
    Fresh,
    Milk,
    Grocery,
    Frozen,
    Detergents_Paper,
    Delicassen
  ) ~ gmm_cluster,
  data = data,
  FUN = sum
)
for (col in spending.cols) {
  gmm_sums_all[[paste0(col, "_rev_pct")]] <- round(
    gmm_sums_all[[col]] / sum(gmm_sums_all[[col]]) * 100,
    1
  )
}

### Lookup helper functions ----
# Helper: look up a value by cluster ID
km3_stat <- function(cluster_id, col) {
  row <- km3_medians$km3_cluster == as.integer(cluster_id)
  km3_medians[row, col]
}
km3_rev <- function(cluster_id, col) {
  row <- km3_sums_all$km3_cluster == as.integer(cluster_id)
  km3_sums_all[row, col]
}
gmm_stat <- function(cluster_id, col) {
  row <- gmm_medians$gmm_cluster == cluster_id
  gmm_medians[row, col]
}
gmm_rev <- function(cluster_id, col) {
  row <- gmm_sums_all$gmm_cluster == cluster_id
  gmm_sums_all[row, col]
}
gmm_n <- function(cluster_id) {
  sum(data$gmm_cluster == cluster_id)
}
gmm_pct_customers <- function(cluster_id) {
  round(sum(data$gmm_cluster == cluster_id) / nrow(data) * 100)
}


n_total <- nrow(data)
spend_cols <- c(
  "Fresh",
  "Milk",
  "Grocery",
  "Frozen",
  "Detergents_Paper",
  "Delicassen"
)
cat_levels <- c(spend_cols, "Total")

# ******************************************************************************
# Helper: from a grouping vector, produce a revenue-concentration data frame
# ******************************************************************************
build_shares <- function(group_vec, group_name = "cluster") {
  # Sums per group
  sums <- aggregate(
    cbind(Fresh, Milk, Grocery, Frozen, Detergents_Paper, Delicassen) ~ grp,
    data = cbind(data[, spend_cols], grp = group_vec),
    FUN = sum
  )

  # Column percentages
  pcts <- sums
  for (col in spend_cols) {
    pcts[[col]] <- sums[[col]] / sum(sums[[col]]) * 100
  }

  # Pivot long
  pcts_long <- reshape(
    pcts,
    direction = "long",
    varying = spend_cols,
    v.names = "Pct",
    timevar = "Category",
    times = spend_cols
  )
  pcts_long$Cluster <- factor(pcts_long$grp)
  row.names(pcts_long) <- NULL

  # Total column
  sums$total <- sums$Fresh +
    sums$Milk +
    sums$Grocery +
    sums$Frozen +
    sums$Detergents_Paper +
    sums$Delicassen
  totals <- data.frame(
    grp = sums$grp,
    Category = "Total",
    Pct = sums$total / sum(sums$total) * 100,
    Cluster = factor(sums$grp)
  )

  # Combine
  out <- rbind(pcts_long[, c("grp", "Category", "Pct", "Cluster")], totals)
  out$Category <- factor(out$Category, levels = cat_levels)
  out
}

# ******************************************************************************
# Helper: from a grouping vector, produce legend labels "Label-X (n=Y, Z%)"
# ******************************************************************************
build_labels <- function(group_vec, names_dict = NULL, prefix = "Cluster") {
  counts <- as.data.frame(table(group_vec))
  colnames(counts) <- c("grp", "n")

  # Calculate percentages
  counts$pct <- (counts$n / n_total) * 100

  # Construct the label string
  if (!is.null(names_dict)) {
    # If a dictionary is provided, map the group number to your custom name
    counts$desc <- names_dict[as.character(counts$grp)]
    counts$label <- sprintf(
      "%s (n=%d, %.0f%%)",
      counts$desc,
      counts$n,
      counts$pct
    )
  } else {
    # Fallback to the original Prefix-X format if no dictionary is supplied
    counts$label <- sprintf(
      "%s-%s (n=%d, %.0f%%)",
      prefix,
      counts$grp,
      counts$n,
      counts$pct
    )
  }

  # Return a named vector required by scale_fill_manual / scale_fill_brewer
  setNames(counts$label, as.character(counts$grp))
}

# ******************************************************************************
# Helper: standard revenue concentration plot
# ******************************************************************************
plot_shares <- function(
  share_data,
  fill_labels,
  title,
  palette = "Set1",
  manual_colours = NULL
) {
  p <- ggplot(share_data, aes(x = Category, y = Pct, fill = Cluster)) +
    geom_col(position = "stack", width = 0.7) +
    geom_text(
      aes(label = sprintf("%.0f%%", Pct)),
      position = position_stack(vjust = 0.5),
      size = 3,
      colour = "white"
    ) +
    geom_vline(xintercept = 6.5, linetype = "dashed", colour = "grey40") +
    labs(
      title = title,
      y = "Share of total category spend (%)",
      x = "",
      fill = "Segment"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  if (!is.null(manual_colours)) {
    p <- p + scale_fill_manual(values = manual_colours, labels = fill_labels)
  } else {
    p <- p + scale_fill_brewer(palette = palette, labels = fill_labels)
  }
  p
}

## Revenue triptych figure ----
# ******************************************************************************
# Build all three plots
# ******************************************************************************

# Define the descriptive segment names based on the spending profiles
channel_names <- c(
  "1" = "HoReCa",
  "2" = "Retail"
)

# km3_names <- c(
#   "1" = "Mixed Basket",
#   "2" = "HoReCa Dominant",
#   "3" = "Retail Dominant"
# )

gmm_names <- c(
  "1" = "Heavy Retail",
  "2" = "Diversified Retail",
  "3" = "High-volume HoReCa",
  "4" = "Balanced HoReCa",
  "5" = "Low-spend / Mixed"
)

# factor ordering: HoReCa-type first (bottom), Retail-type last (top)
channel_order <- c("2", "1")
# HoReCa, Retail

# km3_order <- c("3", "1", "2")
# # HoReCa Dominant, Mixed, Retail Dominant

gmm_order <- c("1", "2", "5", "4", "3")
# High-vol HoReCa, Balanced HoReCa, Low-spend, Diversified Retail, Heavy Retail

gmm_colours <- c(
  "3" = "#B2182B", # deep red - high-vol HoReCa
  "4" = "#E66101", # orange - balanced HoReCa
  "5" = "#FDB863", # amber - low-spend mixed
  "2" = "#92C5DE", # light blue - diversified Retail
  "1" = "#2166AC" # deep blue - heavy Retail
)
# km3_colours <- c(
#   "2" = "#B2182B",   # HoReCa Dominant
#   "1" = "#FDB863",   # Mixed Basket
#   "3" = "#2166AC"    # Retail Dominant
# )
ch_colours <- c("1" = "#E64B35", "2" = "#4DBBD5")

palettes <- list(
  rdylbu = list(
    gmm = c(
      "3" = "#B2182B",
      "4" = "#E66101",
      "5" = "#FDB863",
      "2" = "#92C5DE",
      "1" = "#2166AC"
    ),
    km3 = c("2" = "#B2182B", "1" = "#FDB863", "3" = "#2166AC"),
    ch = c("1" = "#E64B35", "2" = "#4DBBD5")
  ),
  okabe_ito = list(
    gmm = c(
      "3" = "#D55E00",
      "4" = "#E69F00",
      "5" = "#F0E442",
      "2" = "#56B4E9",
      "1" = "#0072B2"
    ),
    km3 = c("2" = "#D55E00", "1" = "#F0E442", "3" = "#0072B2"),
    ch = c("1" = "#D55E00", "2" = "#0072B2")
  ),
  viridis = list(
    gmm = c(
      "3" = "#440154",
      "4" = "#3B528B",
      "5" = "#21918C",
      "2" = "#5EC962",
      "1" = "#FDE725"
    ),
    km3 = c("2" = "#440154", "1" = "#21918C", "3" = "#FDE725"),
    ch = c("1" = "#440154", "2" = "#FDE725")
  )
)

active <- palettes$rdylbu

gmm_colours <- active$gmm
km3_colours <- active$km3
ch_colours <- active$ch

# (a) Channel baseline
ch_vec <- as.integer(data$Channel)
ch_shares <- build_shares(ch_vec)
ch_shares$Cluster <- factor(ch_shares$Cluster, levels = channel_order)
ch_labels <- build_labels(ch_vec, names_dict = channel_names)

p_channel <- plot_shares(
  ch_shares,
  ch_labels,
  title = "(a) Baseline: Channel",
  manual_colours = ch_colours
) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

# (b) K-Means k=3
km3_vec <- as.integer(KMeans.results[[3]]$cluster)
km3_shares <- build_shares(km3_vec)
km3_shares$Cluster <- factor(km3_shares$Cluster, levels = km3_order)
km3_labels <- build_labels(km3_vec, names_dict = km3_names)

p_km3 <- plot_shares(
  km3_shares,
  km3_labels,
  title = "(b) K-Means (k = 3)",
  manual_colours = km3_colours
) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

# (c) Gaussian Mixture Modelling k=5
gmm_vec <- as.integer(gmm_spending$classification)
gmm_shares <- build_shares(gmm_vec)
gmm_shares$Cluster <- factor(gmm_shares$Cluster, levels = gmm_order)
gmm_labels <- build_labels(gmm_vec, names_dict = gmm_names)

p_gmm <- plot_shares(
  gmm_shares,
  gmm_labels,
  title = "(c) Gaussian Mixture Modelling (k = 5)",
  manual_colours = gmm_colours
)

### Build & stack plots ----
# ******************************************************************************
# Stack
# ******************************************************************************
p_channel / p_km3 / p_gmm + plot_layout(heights = c(1, 1, 1.15))


## Sankey diagram ----

categories <- c(
  "Fresh",
  "Milk",
  "Grocery",
  "Frozen",
  "Detergents_Paper",
  "Delicassen"
)

sk_wide <- data %>%
  pivot_longer(
    cols = all_of(categories),
    names_to = "category",
    values_to = "spend"
  ) %>%
  group_by(category, km3_cluster, gmm_cluster) %>%
  summarise(total_spend = sum(spend), .groups = "drop") %>%
  mutate(
    km3_cluster = paste0("KM-", km3_cluster),
    gmm_cluster = paste0("GMM-", gmm_cluster)
  ) %>%
  mutate(perc = total_spend / sum(total_spend))

# Remap cluster IDs to descriptive labels
km3_label_map <- setNames(
  paste0("KM-", names(km3_names), "\n", km3_names),
  paste0("KM-", names(km3_names))
)
gmm_label_map <- setNames(
  paste0("GMM-", names(gmm_names), "\n", gmm_names),
  paste0("GMM-", names(gmm_names))
)

sk_wide <- sk_wide %>%
  mutate(
    km3_cluster = km3_label_map[km3_cluster],
    gmm_cluster = gmm_label_map[gmm_cluster]
  )

# In case we want to set color based on a column
sk_color_lookup <- sk_wide %>%
  select(category, km3_cluster, gmm_cluster) %>%
  #mutate(edge_color = km3_cluster)
  mutate(edge_color = gmm_cluster)

sankeyfier_df <- sk_wide %>%
  left_join(
    sk_color_lookup,
    by = c("category", "km3_cluster", "gmm_cluster")
  ) %>%
  pivot_stages_longer(
    #stages      = c("category", "km3_cluster", "gmm_cluster"),
    #stages      = c("km3_cluster", "category", "gmm_cluster"), # category in the middle
    stages = c("category", "gmm_cluster", "km3_cluster"), # gmm_cluster in the middle
    values_from = "perc",
    additional_aes_from = c('edge_color')
  )

sk_palette <- createPalette(
  N = length(unique(sankeyfier_df$node)),
  seedcolors = c("#B22222", "#228B22", "#4682B4")
)

names(sk_palette) <- unique(sankeyfier_df$node)

sk_pos <- position_sankey(width = 0.5, nudge_x = -0.15) # was 0.3
sk_pos_text_node <- position_sankey(text = TRUE, nudge_x = -0.38) #was -0.28
sk_pos_text_edge <- position_sankey(text = TRUE, nudge_x = 0.15) # was 0.1

sk_label_threshold <- 0.03
text_size <- 4

ggplot(
  sankeyfier_df,
  aes(
    x = stage,
    y = perc,
    group = node,
    connector = connector,
    edge_id = edge_id
  )
) +
  # nodes
  geom_sankeynode(
    aes(fill = node),
    position = sk_pos,
    color = "black"
  ) +
  # node labels
  geom_label(
    aes(label = node),
    stat = "sankeynode",
    position = sk_pos_text_node,
    hjust = 0,
    size = text_size,
    fill = alpha("white", 0.8),
    label.size = 0,
    label.padding = unit(2, "pt")
  ) +
  # edges (flows)
  geom_sankeyedge(
    aes(fill = edge_color),
    #aes(fill = node),
    position = sk_pos,
    alpha = 0.8
  ) +
  # edge-level percentage labels
  geom_label(
    stat = "sankeyedge",
    aes(
      label = ifelse(
        after_stat(y) > sk_label_threshold,
        scales::percent(after_stat(y), accuracy = 0.1),
        NA
      )
    ),
    position = sk_pos_text_edge,
    size = text_size,
    color = "black",
    fill = alpha("white", 0.7),
    label.size = 0,
    label.padding = unit(1, "pt"),
    na.rm = TRUE
  ) +
  scale_fill_manual(values = sk_palette) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(
    labels = c(
      category = "Category",
      km3_cluster = "KMeans Cluster",
      gmm_cluster = "GMM Cluster"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank()
  ) +
  labs(
    title = "",
    y = "Percentage of total spending",
  )

# Q3: LLM Comparison ====
## Comparison table ----
# define symbols
sym <- list(
  Y = "$\\checkmark$", # ● addressed unprompted
  P = "$\\circ$", # ○ partial / after prompting
  N = "\\texttimes" # ✗ missed entirely
)

sym_legend <- paste0(
  sym$Y,
  "~= addressed unprompted; ",
  sym$P,
  "~= partial; ",
  sym$N,
  "~= missed entirely."
)

# data structure
# > can replace "?" with Y/P/N after running the LLM experiment

rows <- tribble(
  ~group           , ~label                                            , ~ours , ~chatgpt , ~claude ,
  "Preprocessing"  , "Excluded Channel \\& Region from PCA/clustering" , "Y"   , "Y"      , "Y"     ,
  "Preprocessing"  , "Identified skewness / recommended log transform" , "Y"   , "Y"      , "Y"     ,
  "Preprocessing"  , "Standardised variables before PCA"               , "Y"   , "Y"      , "Y"     ,
  "PCA"            , "Produced interpretable scree plot / variance"    , "Y"   , "Y"      , "Y"     ,
  "PCA"            , "Interpreted PC loadings with specificity"        , "Y"   , "N"      , "P"     ,
  "PCA"            , "Validated PCA against Channel label"             , "Y"   , "Y"      , "Y"     ,
  "Clustering"     , "Justified choice of clustering method"           , "Y"   , "P"      , "Y"     ,
  "Clustering"     , "Used formal $k$ selection (silhouette / elbow)"  , "Y"   , "Y"      , "P"     ,
  "Clustering"     , "Identified structure beyond $k{=}2$"             , "Y"   , "Y"      , "Y"     ,
  "Clustering"     , "Applied robustness checks unprompted"            , "Y"   , "N"      , "N"     ,
  "Interpretation" , "Commercial framing of segments"                  , "Y"   , "P"      , "Y"     ,
  "Interpretation" , "Acknowledged limitations unprompted"             , "Y"   , "N"      , "N"     ,
  "Interpretation" , "Generated code executed without errors"          , "Y"   , "Y"      , "N"
)

# map codes to symbols
map_sym <- function(x) {
  dplyr::case_match(x, "Y" ~ sym$Y, "P" ~ sym$P, "N" ~ sym$N, .default = x)
}

# compute totals pre symbol mapping
score_col <- function(col) {
  n_total <- sum(col %in% c("Y", "P", "N"))
  if (n_total == 0) {
    return("---")
  }
  n_y <- sum(col == "Y")
  n_p <- sum(col == "P")
  # format: "10/14 (2P)" or "14/14" if no partials
  if (n_p > 0) {
    paste0(n_y, "/", n_total, " (", n_p, " partial)")
  } else {
    paste0(n_y, "/", n_total)
  }
}

totals <- tibble(
  group = "Totals",
  label = "\\textbf{Score}",
  ours = score_col(rows$ours),
  chatgpt = score_col(rows$chatgpt),
  claude = score_col(rows$claude)
)

# combine rows + totals, then map symbols
all_rows <- bind_rows(rows, totals) %>%
  mutate(across(c(ours, chatgpt, claude), map_sym))

#compute pack_rows indices from group column
group_indices <- all_rows %>%
  mutate(row = row_number()) %>%
  group_by(group) %>%
  summarise(start = min(row), end = max(row), .groups = "drop")

group_indices <- group_indices %>%
  mutate(group = factor(group, levels = unique(all_rows$group))) %>%
  arrange(group)

# build kable
kable_data <- all_rows %>%
  dplyr::select(label, ours, chatgpt, claude)

colnames(kable_data) <- c(
  "Analysis Step",
  "Our Analysis",
  "ChatGPT (GPT-5.3)",
  "Claude (Sonnet 4.6)"
)

caption_text <- paste0(
  "Comparison of analytical steps: our pipeline vs \\ two LLMs. ",
  sym_legend
)

tbl <- kable(
  kable_data,
  booktabs = TRUE,
  escape = FALSE,
  align = c("l", "c", "c", "c"),
  caption = caption_text,
  linesep = ""
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 10
  )

# -apply pack_rows
for (i in seq_len(nrow(group_indices))) {
  grp <- as.character(group_indices$group[i])
  # skip the totals row - it stands alone with a rule above it
  if (grp == "Totals") {
    next
  }
  tbl <- tbl %>%
    pack_rows(
      grp,
      group_indices$start[i],
      group_indices$end[i],
      bold = TRUE,
      italic = FALSE,
      hline_before = (i > 1)
    )
}

tbl


# Appendix ====
## A: Raw data exploration ----
box_data <- reshape(
  data[, spending.cols],
  direction = "long",
  varying = spending.cols,
  v.names = "Spend",
  timevar = "Category",
  times = spending.cols
)

cat_totals <- data.frame(
  Category = spending.cols,
  Total = colSums(data[, spending.cols]),
  Median = apply(data[, spending.cols], 2, median)
)

# option to sort by median vs total;
# BE note: have chosen median here to highlight the difference in median spend
# despite large total gap between bottom two categories
cat_order <- levels(reorder(cat_totals$Category, cat_totals$Median))
# cat_order <- levels(reorder(cat_totals$Category, cat_totals$Total))
box_data$Category <- factor(box_data$Category, levels = cat_order)
cat_totals$Category <- factor(cat_totals$Category, levels = cat_order)
row.names(box_data) <- NULL

### Boxplots & total spend ----
p_box <- ggplot(
  box_data,
  aes(x = Category, y = Spend)
) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "(a) Spending Distribution per Customer",
    y = "Annual Spend (m.u.)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

p_bar <- ggplot(cat_totals, aes(x = Category, y = Total)) +
  geom_col(fill = "steelblue", alpha = 0.7, width = 0.7) +
  geom_text(
    aes(label = format(round(Total), big.mark = ",")),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "(b) Total Category Spend",
    y = "Total Spend (m.u.)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

p_box / p_bar

### Histograms: raw vs log ----
bind_rows(raw_df, log_df) %>%
  mutate(
    Transform = factor(Transform, levels = c("Raw", "Log-transformed"))
  ) %>%
  group_by(Category, Transform) %>%
  mutate(Spend_scaled = rescale01(Spend)) %>%
  ungroup() %>%
  ggplot(aes(x = Spend_scaled, fill = Transform)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    colour = "white",
    alpha = 0.5,
    position = "identity"
  ) +
  scale_fill_manual(
    values = c("Raw" = "steelblue", "Log-transformed" = "firebrick")
  ) +
  facet_wrap(~Category, scales = "free_y") +
  labs(
    title = "Shape Comparison: Raw vs Log-transformed Spending",
    x = "Normalised Spend (0-1 scaled within each group)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


### Correlation matrices ----
par(mfrow = c(1, 2))

soft_palette <- colorRampPalette(c(
  "#D73027",
  "#F46D43",
  "#FFFFFF",
  "#74ADD1",
  "#4575B4"
))(200)

corrplot(
  cor(spending),
  method = "color",
  type = "upper",
  col = soft_palette,
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  title = "Raw spending",
  mar = c(0, 0, 2, 0)
)

corrplot(
  cor(spending_log),
  method = "color",
  type = "upper",
  col = soft_palette,
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  title = "Log-transformed spending",
  mar = c(0, 0, 2, 0)
)

## B: Multi-dimensional scaling ----
# Gower distance on all 8 variables
mds_data <- data.frame(
  Channel = factor(data$Channel, labels = c("HoReCa", "Retail")),
  Region = factor(data$Region, labels = c("Lisbon", "Oporto", "Other")),
  spending_log
)

gow <- cluster::daisy(mds_data, metric = "gower")
mds_fit <- MASS::isoMDS(gow, k = 2, trace = FALSE)
mds_pts <- as.data.frame(mds_fit$points)
colnames(mds_pts) <- c("MDS1", "MDS2")

mds_pts$Channel <- mds_data$Channel
mds_pts$Region <- mds_data$Region

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Left: coloured by Channel
plot(
  mds_pts$MDS1,
  mds_pts$MDS2,
  col = ifelse(mds_pts$Channel == "HoReCa", "#E64B35", "#000000"),
  # 19 = Circle, 17 = Triangle
  pch = ifelse(mds_pts$Channel == "HoReCa", 19, 17),
  cex = 0.8,
  xlab = "MDS1",
  ylab = "MDS2",
  main = "(a) Coloured by Channel"
)
legend(
  "topleft",
  legend = c("HoReCa", "Retail"),
  col = c("#E64B35", "#000000"),
  pch = c(19, 17),
  cex = 0.9,
  bty = "n"
)

# Right: coloured by Region
region_cols <- c(Lisbon = "#E69F00", Oporto = "#56B4E9", Other = "#CC79A7")
plot(
  mds_pts$MDS1,
  mds_pts$MDS2,
  col = region_cols[mds_pts$Region],
  pch = 19,
  cex = 0.8,
  xlab = "MDS1",
  ylab = "MDS2",
  main = "(b) Coloured by Region"
)
legend(
  "topleft",
  legend = names(region_cols),
  col = region_cols,
  pch = 19,
  cex = 0.9,
  bty = "n"
)
# mds_fit$stress

## C: Clustering diagnostics ----

### Silhouette plot ----
fviz_nbclust(scaled_data, kmeans, method = 'silhouette', nstart = 25)

### BIC plot ----
plot(
  gmm_spending,
  what = "BIC",
  legendArgs = list(x = "bottomright", ncol = 4, cex = 0.8)
)

### K-Means cross-tabulation ----
km3_3way <- table(
  KM = km3$cluster,
  Channel = data$Channel,
  Region = data$Region
)

km_rows <- list()
for (k in 1:3) {
  for (ch in 1:2) {
    km_rows[[length(km_rows) + 1]] <- data.frame(
      KM = k,
      Channel = ch,
      Lisbon = km3_3way[k, ch, 1],
      Oporto = km3_3way[k, ch, 2],
      Other = km3_3way[k, ch, 3]
    )
  }
}
km_region_df <- do.call(rbind, km_rows)

# Build pack_rows labels programmatically
km3_pack_labels <- sapply(1:3, function(k) {
  paste0("KM-", k, " (", km3_names[as.character(k)], ")")
})

kable(
  km_region_df[, c("Channel", "Lisbon", "Oporto", "Other")],
  booktabs = TRUE,
  row.names = FALSE,
  caption = "K-Means (k=3) cluster assignments vs \\ Channel (1=HoReCa, 2=Retail) and Region.",
  align = c("l", "r", "r", "r")
) %>%
  kable_styling(latex_options = "hold_position", font_size = 10) %>%
  pack_rows(km3_pack_labels[1], 1, 2, bold = TRUE, hline_before = FALSE) %>%
  pack_rows(km3_pack_labels[2], 3, 4, bold = TRUE, hline_before = TRUE) %>%
  pack_rows(km3_pack_labels[3], 5, 6, bold = TRUE, hline_before = TRUE)

### GMM cross-tabulation ----
gmm_3way <- table(
  GMM = data$gmm_cluster,
  Channel = data$Channel,
  Region = data$Region
)

rows_list <- list()
for (g in 1:5) {
  for (ch in 1:2) {
    rows_list[[length(rows_list) + 1]] <- data.frame(
      GMM = g,
      Channel = ch,
      Lisbon = gmm_3way[g, ch, 1],
      Oporto = gmm_3way[g, ch, 2],
      Other = gmm_3way[g, ch, 3]
    )
  }
}
region_df <- do.call(rbind, rows_list)

gmm_pack_labels <- sapply(1:5, function(g) {
  paste0("GMM-", g, " (", gmm_names[as.character(g)], ")")
})

kable(
  region_df[, c("Channel", "Lisbon", "Oporto", "Other")],
  booktabs = TRUE,
  row.names = FALSE,
  caption = "GMM (k=5) cluster assignments vs \\ Channel (1=HoReCa, 2=Retail) and Region.",
  align = c("l", "r", "r", "r")
) %>%
  kable_styling(latex_options = "hold_position", font_size = 10) %>%
  pack_rows(gmm_pack_labels[1], 1, 2, bold = TRUE, hline_before = FALSE) %>%
  pack_rows(gmm_pack_labels[2], 3, 4, bold = TRUE, hline_before = TRUE) %>%
  pack_rows(gmm_pack_labels[3], 5, 6, bold = TRUE, hline_before = TRUE) %>%
  pack_rows(gmm_pack_labels[4], 7, 8, bold = TRUE, hline_before = TRUE) %>%
  pack_rows(gmm_pack_labels[5], 9, 10, bold = TRUE, hline_before = TRUE)

### Cluster spending profiles ----

#### Median spending table ----

# Build from precomputed km3_medians and gmm_medians
km3_raw_table <- km3_medians[, c("km3_cluster", spending.cols)]
km3_raw_table$n <- sapply(km3_raw_table$km3_cluster, function(k) {
  sum(data$km3_cluster == k)
})
km3_raw_table$Cluster <- paste0("KM-", km3_raw_table$km3_cluster)
km3_raw_table$Label <- km3_names[as.character(km3_raw_table$km3_cluster)]

gmm_raw_table <- gmm_medians[, c("gmm_cluster", spending.cols)]
gmm_raw_table$n <- sapply(gmm_raw_table$gmm_cluster, function(g) {
  sum(data$gmm_cluster == g)
})
gmm_raw_table$Cluster <- paste0("GMM-", gmm_raw_table$gmm_cluster)
gmm_raw_table$Label <- gmm_names[as.character(gmm_raw_table$gmm_cluster)]

combined_raw <- rbind(
  km3_raw_table[, c("Cluster", "Label", "n", spending.cols)],
  gmm_raw_table[, c("Cluster", "Label", "n", spending.cols)]
)
for (col in spending.cols) {
  combined_raw[[col]] <- format(round(combined_raw[[col]]), big.mark = ",")
}

kable(
  combined_raw,
  booktabs = TRUE,
  escape = FALSE,
  col.names = c(
    "Cluster",
    "Label",
    "$n$",
    "Fresh",
    "Milk",
    "Grocery",
    "Frozen",
    "Detergents\\_Paper",
    "Delicassen"
  ),
  align = c("l", "l", "r", rep("r", 6)),
  caption = "Cluster spending profiles (median annual spending, m.u.)",
  linesep = ""
) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  pack_rows("K-Means (k = 3)", 1, 3, bold = TRUE, hline_before = FALSE) %>%
  pack_rows(
    "Gaussian Mixture Model (k = 5)",
    4,
    8,
    bold = TRUE,
    hline_before = TRUE
  )

#### Basket percentage table ----

km3_pct_table <- km3_medians[, c("km3_cluster", paste0(spending.cols, "_pct"))]
km3_pct_table$Cluster <- paste0("KM-", km3_pct_table$km3_cluster)
km3_pct_table$Label <- km3_names[as.character(km3_pct_table$km3_cluster)]
colnames(km3_pct_table)[2:7] <- spending.cols

gmm_pct_table <- gmm_medians[, c("gmm_cluster", paste0(spending.cols, "_pct"))]
gmm_pct_table$Cluster <- paste0("GMM-", gmm_pct_table$gmm_cluster)
gmm_pct_table$Label <- gmm_names[as.character(gmm_pct_table$gmm_cluster)]
colnames(gmm_pct_table)[2:7] <- spending.cols

combined_pct <- rbind(
  km3_pct_table[, c("Cluster", "Label", spending.cols)],
  gmm_pct_table[, c("Cluster", "Label", spending.cols)]
)

kable(
  combined_pct,
  booktabs = TRUE,
  escape = FALSE,
  col.names = c(
    "Cluster",
    "Label",
    "Fresh",
    "Milk",
    "Grocery",
    "Frozen",
    "Detergents\\_Paper",
    "Delicassen"
  ),
  align = c("l", "l", rep("r", 6)),
  caption = "Cluster spending profiles (\\% of median basket)",
  linesep = ""
) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  pack_rows("K-Means (k = 3)", 1, 3, bold = TRUE, hline_before = FALSE) %>%
  pack_rows(
    "Gaussian Mixture Model (k = 5)",
    4,
    8,
    bold = TRUE,
    hline_before = TRUE
  )

#### Revenue concentration table ----

km3_rev_table <- km3_sums_all[, c(
  "km3_cluster",
  paste0(spending.cols, "_rev_pct")
)]
km3_rev_table$n <- sapply(km3_rev_table$km3_cluster, function(k) {
  sum(data$km3_cluster == k)
})
km3_rev_table$Cluster <- paste0("KM-", km3_rev_table$km3_cluster)
km3_rev_table$Label <- km3_names[as.character(km3_rev_table$km3_cluster)]
colnames(km3_rev_table)[2:7] <- spending.cols

gmm_rev_table <- gmm_sums_all[, c(
  "gmm_cluster",
  paste0(spending.cols, "_rev_pct")
)]
gmm_rev_table$n <- sapply(gmm_rev_table$gmm_cluster, function(g) {
  sum(data$gmm_cluster == g)
})
gmm_rev_table$Cluster <- paste0("GMM-", gmm_rev_table$gmm_cluster)
gmm_rev_table$Label <- gmm_names[as.character(gmm_rev_table$gmm_cluster)]
colnames(gmm_rev_table)[2:7] <- spending.cols

combined_rev <- rbind(
  km3_rev_table[, c("Cluster", "Label", spending.cols)],
  gmm_rev_table[, c("Cluster", "Label", spending.cols)]
)

kable(
  combined_rev,
  booktabs = TRUE,
  escape = FALSE,
  col.names = c(
    "Cluster",
    "Label",
    "Fresh",
    "Milk",
    "Grocery",
    "Frozen",
    "Detergents\\_Paper",
    "Delicassen"
  ),
  align = c("l", "l", rep("r", 6)),
  caption = "Revenue concentration: each cluster's share (\\%) of total
             category spend across all 440 customers.",
  linesep = ""
) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 10) %>%
  pack_rows("K-Means (k = 3)", 1, 3, bold = TRUE, hline_before = FALSE) %>%
  pack_rows(
    "Gaussian Mixture Model (k = 5)",
    4,
    8,
    bold = TRUE,
    hline_before = TRUE
  )

### Cluster assignments in PCA space ----

pca_scores <- as.data.frame(pca_result$x[, 1:2])
colnames(pca_scores) <- c("PC1", "PC2")

pc1_var <- summary(pca_result)$importance[2, 1] * 100
pc2_var <- summary(pca_result)$importance[2, 2] * 100
pc1_lab <- sprintf("PC1 (%.1f%%)", pc1_var)
pc2_lab <- sprintf("PC2 (%.1f%%)", pc2_var)

pca_scores$KM3 <- factor(km3$cluster)
pca_scores$GMM5 <- factor(data$gmm_cluster)

p_km <- ggplot(pca_scores, aes(x = PC1, y = PC2, colour = KM3, shape = KM3)) +
  geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.3) +
  geom_point(alpha = 0.7, size = 1.5) +
  scale_colour_brewer(palette = "Set1") +
  labs(
    title = "(a) K-Means (k = 3)",
    x = pc1_lab,
    y = pc2_lab,
    colour = "Cluster",
    shape = "Cluster"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_gmm_pca <- ggplot(
  pca_scores,
  aes(x = PC1, y = PC2, colour = GMM5, shape = GMM5)
) +
  geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.3) +
  geom_point(alpha = 0.99, size = 1.5) +
  scale_colour_brewer(palette = "Set2") +
  scale_shape_manual(values = c(16, 17, 15, 18, 3)) +
  labs(
    title = "(b) GMM (k = 5)",
    x = pc1_lab,
    y = pc2_lab,
    colour = "Cluster",
    shape = "Cluster"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_km / p_gmm_pca

## D: Ward's hierarchical clustering ----
# Ward's hierarchical clustering
# this code was moved up to Q2 (commented out here for transparency)
# dist_mat <- dist(scaled_data, method = "euclidean")
# hc <- hclust(dist_mat, method = "ward.D2")
# hc_clusters <- cutree(hc, k = 3)

# table(KMeans = KMeans.results[[3]]$cluster, Ward = hc_clusters)
# cross-tabulation
cross_tab <- table(KMeans.results[[3]]$cluster, Ward = hc_clusters)

# Ename the rows and columns
rownames(cross_tab) <- paste("K-Means", 1:3)
colnames(cross_tab) <- paste("Ward", 1:3)

# format
kable(
  cross_tab,
  booktabs = TRUE,
  caption = "K-Means vs Ward's hierarchical clustering assignments (k=3)."
) %>%
  kable_styling(latex_options = "hold_position")

## E: LLM script execution ----

# We have commented out the below code because we did not attach the raw R files
# produced by each LLM.
# BE note: The persistent session per LLM was interesting to code - it allowed
# us to run the individual code from each LLM by linking out to the R file using
# a separate instance of R. This meant that we could see the actual outputs in
# our pdf and html files without having to manually import the figures or text
# printed to the console across.

# llm_plot_dir <- "LLM-output/figures"
# dir.create(llm_plot_dir, showWarnings = FALSE, recursive = TRUE)
#
# # One persistent session per LLM
# sessions <- list(
#   claude = callr::r_session$new(),
#   chatgpt = callr::r_session$new()
# )
#
#
# # Set working directory for each
# for (s in sessions) {
#   s$run(function(wd) setwd(wd), args = list(wd = normalizePath("LLM-output")))
# }
#
# run_llm_script <- function(script_path, plot_prefix, llm = "claude") {
#   session <- sessions[[llm]]
#
#   plot_dir <- file.path(llm_plot_dir, plot_prefix)
#   dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
#
#   old_plots <- list.files(plot_dir, pattern = "\\.png$", full.names = TRUE)
#   if (length(old_plots) > 0) {
#     file.remove(old_plots)
#   }
#
#   result <- session$run(
#     function(sp, pd) {
#       png(file.path(pd, "plot_%03d.png"), width = 900, height = 600, res = 150)
#
#       output <- capture.output(
#         {
#           tryCatch(
#             source(sp, print.eval = TRUE),
#             error = function(e) cat("\n== SOURCE ERROR ==\n", e$message, "\n")
#           )
#         },
#         type = "output"
#       )
#
#       dev.off()
#       list(output = output)
#     },
#     args = list(
#       sp = normalizePath(script_path),
#       pd = normalizePath(plot_dir)
#     )
#   )
#
#   if (length(result$output) > 0) {
#     cat("\n```\n")
#     cat(result$output, sep = "\n")
#     cat("\n```\n\n")
#   }
#
#   plots <- sort(list.files(plot_dir, pattern = "\\.png$", full.names = TRUE))
#   for (p in plots) {
#     cat(paste0("![](", p, ")\n\n"))
#   }
# }
#
# # NA
#
# try({
#   run_llm_script(
#     "LLM-output/LLM-code-claude-prompt-01.R",
#     "claude_prompt01",
#     llm = "claude"
#   )
# })
#
# # NA
#
# try({
#   run_llm_script(
#     "LLM-output/LLM-code-chatgpt-prompt-01.R",
#     "chatgpt_prompt01",
#     llm = "chatgpt"
#   )
# })
#
# # NA
#
# try({
#   run_llm_script(
#     "LLM-output/LLM-code-claude-prompt-02.R",
#     "claude_prompt02",
#     llm = "claude"
#   )
# })
#
# # NA
#
# try({
#   run_llm_script(
#     "LLM-output/LLM-code-chatgpt-prompt-02.R",
#     "chatgpt_prompt02",
#     llm = "chatgpt"
#   )
# })
#
# # NA
#
# try({
#   run_llm_script(
#     "LLM-output/LLM-code-chatgpt-prompt-03.R",
#     "chatgpt_prompt03",
#     llm = "chatgpt"
#   )
# })
#
# for (s in sessions) {
#   s$close()
# }
