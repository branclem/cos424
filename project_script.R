
# Read in the dataset.
library(foreign);
raw.data <- read.dta(file="alcohol_data.dta");

head(raw.data);
validation.set <- raw.data[9000:10904, ];
data <- raw.data[1:8999,];

# Sample invocation syntax of cross-validation.
#lm.pred <- cross.validate(5,
#           oct,
#           function(oct) { return(lm(octane ~ ., data=oct)) },
#	     function(model, new.data) { return(predict(model, newdata=new.data)) } );

