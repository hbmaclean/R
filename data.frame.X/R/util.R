#' install_packages
#'
#' Wrapper around the common !require/install/library pattern,
#' to install and load a given vector of packages.
#'
#' Stops with error if packages are not provided as a character vector.
#'
#' @param   packages vector of packages
#' @returns implicit result of installations
#' @export
install_packages <- function(packages) {
    if (! (is.vector(packages) && is.character(packages) ) ) {
        stop('Usage: .install_packages(packages)')
    }
    repo <- "http://cran.us.r-project.org"

    for (package in packages) {
        if (!require(package, character.only = TRUE)) {
            install.packages(package, repos = repo)
            library(package, character.only = TRUE)
        }
    }
}

#' is_numeric_df
#'
#' Checks whether the given df is indeed a data.frame,
#' and that all columns are numeric.
#'
#' @param   df data.frame object
#' @returns TRUE/FALSE whether df is a fully numeric data.frame
#' @export
is_numeric_df <- function(df) {
    return(
        is.data.frame(df) &&
        all(sapply(df, is.numeric, simplify = TRUE))
    )
}

#' as_numeric_df
#'
#' For a given data.frame, this mutates each non-numeric column to instead contain
#' the indices of that column's vectorized unique values, which are then scaled without
#' centering, to minimize variance while remaining positive.
#'
#' @param   df data.frame object
#' @returns df mutated
#' @export
as_numeric_df <- function(df) {
    df |> mutate(
        across(
            ! where(is.numeric),
            ~ scale(match(., as.vector(unique(.))), center = FALSE)
        )
    )
}

#' any_NA_dup_outliers
#'
#' Inspects the given data.frame for NAs, duplicates, outliers, and extreme values.
#' If the data.frame is not fully numeric, this attempts to make it so in order to
#' run 'rstatix::is_outlier' and 'is_extreme'.
#'
#' @param   df data.frame object
#' @param   tabular = FALSE
#' @returns sum of each metric, as a tribble or kable-styled table if tabular=TRUE
#' @export
any_NA_dup_outliers <- function(df, tabular = FALSE) {
    if (!is_numeric_df(df)) {
        warning('Mutating df to numeric')
        df <- as_numeric_df(df)
    }

    moe <- tribble(
        ~missing, ~duplicate, ~outlying, ~extreme,
        sum(is.na(df)),
        sum(duplicated(df)),
        sum(rstatix::is_outlier(df |> as.matrix())),
        sum(rstatix::is_extreme(df |> as.matrix()))
    )

    if (tabular) {
        moe <- moe |> knitr::kable(
            caption = 'Missing, Duplicate, Outlying, and Extreme Values'
        ) |>
        kableExtra::kable_styling(
            bootstrap_options = c('bordered', 'striped'),
            full_width = FALSE,
            position = 'left'
        )
    }
    moe
}
