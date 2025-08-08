#' @details
#' data.frame.X provides custom functions to simplify common analysis and visualization
#' tasks against a data.frame.  Many analysis tools require a fully numeric data.frame,
#' and so this provides `as_numeric_df`.  And for quick, initial looks at Principal
#' Component Analysis, correlation, and variance, this provides `pca_plot`, `corr_plot`,
#' `aov_plot` and `nzv_plot`.
#'
#' @importFrom tibble rownames_to_column tribble
#' @importFrom dplyr select mutate filter
#' @importFrom tidyr pivot_longer
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob gpar
#' @importFrom stats prcomp aov cor as.formula
#' @importFrom caret nearZeroVar
#' @importFrom ggcorrplot ggcorrplot cor_pmat
#' @importFrom rstatix is_outlier is_extreme
#' @importFrom kableExtra kable_styling
#' @importFrom knitr kable
#' @references
#' A portion of `pca_plot` was derived from this [Statistical Tools for High-Throughput Data Analysis](https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp) article.
"_PACKAGE"
