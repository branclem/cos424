
# Read in the dataset.
library(foreign);
raw.data <- read.dta(file="alcohol_data.dta");

head(raw.data);
validation.set <- raw.data[9000:10904, ];
data <- raw.data[1:8999,];