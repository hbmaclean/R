#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
#    NAME:         mushroom.R
#
#    SYNOPSIS:     Rscript mushroom.R
#
#    DESCRIPTION:  Downloads mushroom data from UCI.org.
#                  Wrangles the data into a data.frame.
#                  Fits a classification model to it with ranger.
#                  Calculates and reports Accuracy.
#
#    NOTE:         This intentionally reduced to JUST the final solution.
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

packages <- c(
    'stringr', 'readr', 'dplyr', 'caret', 'ranger'
)
for (package in packages) {
    if (!require(package, character.only = TRUE)) {
        install.packages(package, repos = 'http://cran.us.r-project.org')
        library(package, character.only = TRUE)
    }
}

# Download mushroom.zip from UCI.edu and render it as a data.frame
fetch_data <- function(verbose = FALSE) {
    options(timeout = 120)
    tmp   <- tempfile()
    zip   <- 'https://archive.ics.uci.edu/static/public/73/mushroom.zip'

    download.file(zip, tmp, quiet = !verbose)
    data <- as.data.frame(
        str_split(
            read_lines(
                unz(tmp, 'agaricus-lepiota.data')
            ),
            ',',
            simplify = TRUE
        )
    )

    colnames(data) <- c(
        'poisonous', 'cap_shape', 'cap_surface', 'cap_color', 'bruises', 'odor',
        'gill_attachment', 'gill_spacing', 'gill_size', 'gill_color',
        'stalk_shape', 'stalk_root', 'stalk_surface_above_ring', 'stalk_surface_below_ring',
        'stalk_color_above_ring', 'stalk_color_below_ring', 'veil_type', 'veil_color',
        'ring_number', 'ring_type', 'spore_print_color', 'population', 'habitat'
    )
    return(data)
}

mushrooms <- fetch_data()
fit       <- ranger(factor(poisonous) ~ ., mushrooms)
pred      <- predict(fit, mushrooms)
cm        <- confusionMatrix(pred$predictions, factor(mushrooms$poisonous))

sprintf('Accuracy: %%%.2f', cm$overall[['Accuracy']]*100)
