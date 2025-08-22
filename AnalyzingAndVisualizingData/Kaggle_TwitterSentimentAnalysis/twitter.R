#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
#    NAME:         twitter.R
#
#    SYNOPSIS:     Rscript twitter.R
#
#    DESCRIPTION:  Downloads twitter data from Kaggle.
#                  Wrangles the data into a data.frame.
#                  Fits a classification model to it with ranger.
#                  Calculates and reports accuracy.
#
#    NOTE:         This intentionally reduced to a minimal working solution.
#                  For performance/speed, it is about 2% off top accuracy in
#                  the corresponding .Rmd.  May take about 3.5 minutes to run.
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(data.frame.X)
install_packages(c(
    'curl', 'stringr', 'dplyr', 'tidyr', 'caret', 'ranger', 'syuzhet'
))
set.seed(45245)

fetch_data <- function() {
    options(timeout = 120)
    url <- paste0(
        'https://www.kaggle.com/api/v1/datasets/download/',
        'jp797498e/twitter-entity-sentiment-analysis'
    )
    tmp <- tempfile()
    curl_download(url, tmp)
    cols <- c('id', 'entity', 'sentiment', 'content')
    train_df <<- as.data.frame(
        read.csv(
            unz(tmp, 'twitter_training.csv'),
            header = FALSE,
            col.names = cols
        )
    ) |>
        mutate(sentiment = ifelse(sentiment == 'Irrelevant', 'Neutral', sentiment))

    test_df <<- as.data.frame(
        read.csv(
            unz(tmp, 'twitter_validation.csv'),
            header = FALSE,
            col.names = cols
        )
    ) |>
        mutate(sentiment = ifelse(sentiment == 'Irrelevant', 'Neutral', sentiment))

}

fetch_data()

# add sentiments
train_df <- train_df |>
    mutate(
        bing    = syuzhet::get_sentiment(content, method = 'bing'),
        afinn   = syuzhet::get_sentiment(content, method = 'afinn'),
        nrc     = syuzhet::get_sentiment(content, method = 'nrc'),
        syuzhet = syuzhet::get_sentiment(content, method = 'syuzhet'),
        content = NULL,  # unnecessary with above sentiments
        id      = NULL   # no intrinsic value

    )

test_df <- test_df |>
    mutate(
        bing    = syuzhet::get_sentiment(content, method = 'bing'),
        afinn   = syuzhet::get_sentiment(content, method = 'afinn'),
        nrc     = syuzhet::get_sentiment(content, method = 'nrc'),
        syuzhet = syuzhet::get_sentiment(content, method = 'syuzhet'),
        content = NULL,
        id      = NULL
    )

# Note: The data is now ~50% duplicated, but making it distinct lowers accuracy ~10%!

fit  <- ranger(factor(sentiment) ~ ., train_df)
pred <- predict(fit, test_df)
cm   <- confusionMatrix(pred$predictions, factor(test_df$sentiment))
acc  <- round(100*cm$overall[['Accuracy']], 2)
print(c('ranger Accuracy' = acc))
