#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
#    NAME:         plant.R
#
#    SYNOPSIS:     Rscript plant.R
#
#    DESCRIPTION:  Downloads plant disease data from Kaggle.
#                  Wrangles the data into a data.frame.
#                  Fits a classification model to it with caret::train and xgbTree.
#                  Calculates and reports Accuracy.
#
#    NOTE:         This intentionally reduced to the minimum solution.
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
set.seed(5345)
repo <- "http://cran.us.r-project.org"
packages <- c(
    'curl', 'stringr', 'dplyr', 'tidyr', 'caret', 'tibble'
)
for (package in packages) {
    if (!require(package, character.only = TRUE)) {
        install.packages(package, repos = repo)
        library(package, character.only = TRUE)
    }
}

fetch_data <- function() {
    options(timeout = 120)
    url <- paste0(
        'https://www.kaggle.com/api/v1/datasets/download/',
        'turakut/plant-disease-classification'
    )
    tmp <- tempfile()
    curl_download(url, tmp)
    data <- as.data.frame(
        read.csv(
            unz(tmp, 'plant_disease_dataset.csv'),
            header = FALSE,   # first row has column names
            skip = 1          # skip them
        )
    )
    colnames(data) <- c(
        'temperature', 'humidity', 'rainfall', 'soil_ph', 'disease_present'
    )
    return(data)
}

plant_df <- fetch_data()
index    <- createDataPartition(y = plant_df$disease_present, times = 1, p = 0.5, list = FALSE)
train_df <- plant_df[index,]
test_df  <- plant_df[-index,]

fit <- caret::train(
    factor(disease_present) ~ .,
    method        = 'xgbTree',
    data          = train_df,
    trControl     = trainControl(method = 'optimism_boot'),
    tuneGrid      = data.frame(
        nrounds          = 10,
        max_depth        = 2,
        eta              = .7,
        gamma            = 7,
        colsample_bytree = .9,
        min_child_weight = 15,
        subsample        = 1
    ),
    preProcess    = c('center')
)
pred    <- predict(fit, test_df)
cm      <- confusionMatrix(pred, factor(test_df$disease_present))
metrics <- c('Prevalence', 'Sensitivity', 'Specificity', 'Precision', 'F1')
cm$byClass |>
    as.data.frame() |>
    rownames_to_column() |>
    pivot_wider(names_from = rowname, values_from = `cm$byClass`) |>
    select(all_of(metrics)) |>
    mutate(
        Accuracy = cm$overall[['Accuracy']]*100,
        F_meas   = caret::F_meas(
            data      = factor(train_df$disease_present),
            reference = factor(test_df$disease_present)
        )
    )