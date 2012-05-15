library(MASS);
library(ggplot2);
source('data_prep.R');
source('validation.R');
source('linear.R');

test.glm.pca <- function(data, formula, family, floor=T, folds=NULL) {
  
  #cat('Running glm -', family$family, '- on', nrow(data), 'rows of data.\n');
  N <- nrow(data);
  actual <- data$C1[1:N];
  pred <- cross.validate(5,
                         data[1:N,],
                         function(data) { return(glm(formula, family, data=data)) },
                         function(model, new.data) {return(predict(model, newdata=new.data)) },
                         folds=folds,
                         verbose=T );
  #pred <- family$linkinv(pred);
  
  # Try different rounding modes to predict integer values.
  rounded.pred <- round(pred);
  rounded.pred[rounded.pred == 0] = 1;
  rounded.pred[rounded.pred > 6] = 6;
  
  floor.pred <- floor(pred);
  floor.pred[floor.pred <= 0] = 1;
  floor.pred[floor.pred > 6] = 6;
  
  cat('Base predictions: avg offset =', mean(abs(pred - actual)), '\n');
  cat('Rounded predictions: avg offset =', mean(abs(rounded.pred - actual)), ',', 100*length(which(rounded.pred == actual)) / length(actual), '%\n');
  cat('Floored predictions: avg offset =', mean(abs(floor.pred - actual)), ',', 100*length(which(floor.pred == actual)) / length(actual), '%\n');
  cat('All 1: avg offset =', mean(abs(rep(1, N) - actual)), ',', 100*length(which(1 == actual)) / length(actual), '%\n');
  
  if(floor) {
    r <- list();
    r$dev <- mean(abs(floor.pred - actual));
    r$pct <- 100*length(which(floor.pred == actual)) / length(actual);
    return(r);
  }
  else {
    r <- list();
    r$dev <- mean(abs(rounded.pred - actual));
    r$pct <- 100*length(which(rounded.pred == actual)) / length(actual);
    return(r);
  }
  
}



#prep the data
data <- get.data.numeric()[,4:400];
C1.col.num <- which(colnames(data) == "C1");
data.without.C1<-data[, -C1.col.num];
data.C1<-data[,C1.col.num];
nvar = 397;
nobs = 8965;
data.pca <- prcomp(data.without.C1, tol = 0.005);
plot(data.pca);
summary(data.pca);
#looking at individual components
comp1<-data.pca$rotation[,1];
comp2<-data.pca$rotation[,2];
comp3<-data.pca$rotation[,3];
comp4<-data.pca$rotation[,4];

comp1.sorted<-comp1[sort.list(abs(comp1), decreasing = TRUE)];
comp2.sorted<-comp2[sort.list(abs(comp2), decreasing = TRUE)];
comp3.sorted<-comp3[sort.list(abs(comp3), decreasing = TRUE)];
comp4.sorted<-comp4[sort.list(abs(comp4), decreasing = TRUE)];

center <- data.pca$center
scale <- data.pca$scale

scores1<-data.matrix(scale(as.matrix(data.without.C1), center = center, scale = scale))%*%t(t(comp1));
scores2<-data.matrix(scale(as.matrix(data.without.C1), center = center, scale = scale))%*%t(t(comp2));
scores3<-data.matrix(scale(as.matrix(data.without.C1), center = center, scale = scale))%*%t(t(comp3));
scores4<-data.matrix(scale(as.matrix(data.without.C1), center = center, scale = scale))%*%t(t(comp4));
scores<-data.frame('scores1'=scores1, 'scores2'=scores2, 'scores3'=scores3, 'scores4'=scores4, 'C1'=data.C1);

test.glm.pca( data=scores, formula = 'data.C1~scores1+scores2+scores3+scores4', family = gaussian, floor = T, folds = NULL)

