
library(mixtools);

source('data_prep.R');
source('validation.R');

data <- get.data.numeric();

raws<-cbind(data$C1, data$A1, data$A2, data$A5, data$D3A, data$D3B)
#multmix.data<-makemultdata(raws, cuts = median(c(data$C1, data$A1, data$A2, data$A5, data$D3A, data$D3B)))
fit.multmix<-multmixEM(raws, k = 6)
summary(fit.multmix)
