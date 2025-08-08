#' data.frame.X-eg
#'
#' This script loads 'data.frame.X' and calls many of its functions with
#' widely available data sets.
#'
#' Currently this exhibits what seems to be a problem, that the package does
#' not load without 'devtools::load_all'.
devtools::load_all()
library(data.frame.X)
pca_plot(USArrests)
nzv_plot(airquality)
corr_plot(trees)
any_NA_dup_outliers(iris)
