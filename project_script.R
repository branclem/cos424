
# Run k-way cross validation on the provided data set. The API is as
# follows: fit(training data) should fit return a model, and then
# pred(model, test data element) should predict the response for the new data.
cross.validate <- function(k, data, fit, pred) {
    predictions <- rep(0, nrow(data));

    # Choose the folds.
    folds <- sample(rep(1:k, length=nrow(data)));

    for (i in 1:k) {
        # Split into out-of-fold and in-fold data.
        train <- data[folds != i,];
        test  <- data[folds == i,];

        # Train on the other k-1 folds.
        model <- fit(train);

        for (j in 1:nrow(test)) {
	      row <- as.integer(row.names(test[j,]));
            predictions[row] <- pred(model, test[j,]);
        }
    }

    return(predictions);
}


# Read in the dataset.
library(foreign);
raw.data <- read.dta(file="alcohol_data.dta");

head(raw.data);
validation.set <- raw.data[9000:10904, ];
data <- raw.data[1:8999,];

# Sample invocation
#lm.pred <- cross.validate(5,
#           oct,
#           function(oct) { return(lm(octane ~ ., data=oct)) },
#	     function(model, new.data) { return(predict(model, newdata=new.data)) } );

