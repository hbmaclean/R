#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
#    NAME:         leaf.R
#
#    SYNOPSIS:     Rscript leaf.R
#
#    DESCRIPTION:  Downloads leaf data from UCI.org.
#                  Wrangles the data into a data.frame.
#                  Fits a classification model to it with nnet::multinom.
#                  Calculates and reports Accuracy.
#
#    NOTE:         This intentionally reduced to JUST the final solution.
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


packages <- c(
    'stringr', 'readr', 'dplyr', 'caret', 'utils', 'stats', 'nnet'
)
for (package in packages) {
    if (!require(package, character.only = TRUE)) {
        install.packages(package, repos = "http://cran.us.r-project.org")
        library(package, character.only = TRUE)
    }
}

fetch_data <- function(verbose=FALSE) {
    options(timeout = 120)
    tmp   <- tempfile()
    zip   <- 'https://archive.ics.uci.edu/static/public/288/leaf.zip'

    download.file(zip, tmp, quiet = !verbose)
    data <- as.data.frame(
        str_split(
            read_lines(
                unz(tmp, 'leaf.csv')
            ),
            ',',
            simplify = TRUE
        )
    ) |> mutate(across(everything(), ~ as.double(.x)))

    colnames(data) <- c(
        'Species', 'SpecimenNumber', 'Eccentricity', 'AspectRatio',
        'Elongation', 'Solidity', 'StochasticConvexity',
        'IsoperimetricFactor', 'MaxIndentationDepth', 'Lobedness',
        'AverageIntensity', 'AverageContrast', 'Smoothness',
        'ThirdMoment', 'Uniformity', 'Entropy'
    )

    data$SpecimenNumber <- NULL

    return(data)
}
set.seed(1)
leaf_df <- fetch_data()
index   <- createDataPartition(y = leaf_df$Species, times = 1, p = 0.5, list = FALSE)
train   <- leaf_df[index,]
test    <- leaf_df[-index,]
f       <- as.formula(
    factor(Species) ~ IsoperimetricFactor * Solidity
        + Lobedness * Solidity
        + Elongation * IsoperimetricFactor
        + I(Eccentricity^3)
        + .
)
fit  <- multinom(f, train, trace = FALSE, decay = .0011)
pred <- predict(fit, test)
cm   <- confusionMatrix(pred, factor(test$Species))

sprintf(
    'nnet::multinom accuracy against UCI Leaf data: %.02f%%',
    round(100*cm$overall[['Accuracy']], 3)
)
