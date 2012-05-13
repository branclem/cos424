
# Run k-way cross validation on the provided data set. The API is as
# follows: fit(training data) should fit return a model, and then
# pred(model, test data element) should predict the response for the new data.
cross.validate <- function(k, data, fit, pred, folds=NULL, verbose=F) {
    predictions <- rep(0, nrow(data));

    # Choose the folds, if they were not prespecified.
    if (is.null(folds)) {
        folds <- sample(rep(1:k, length=nrow(data)));
    }

    for (i in 1:k) {

	  if (verbose) {
            cat('Fold ', i, '...\n');
            cat('  splitting...\n');
            flush.console();
        }

        # Split into out-of-fold and in-fold data.
        train <- data[folds != i,];
        test  <- data[folds == i,];

	  if (verbose) {
            cat('  training...\n');
            flush.console();
        }

        # Train on the other k-1 folds.
        model <- fit(train);

	  if (verbose) {
            cat('  recording...\n');
            flush.console();
        }

        predictions[which(folds == i)] <- pred(model, test);

	  if (verbose) {
            cat('  done...\n');
            flush.console();
        }
    }

    return(predictions);
}
