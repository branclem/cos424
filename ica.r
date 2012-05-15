library(MASS);
library(ggplot2);
library(fastICA);
source('data_prep.R');


#-------------------------------------------------------------
test.ica <- function(data) {
  cat('Running pca -', '- on', nrow(data), 'rows of data.\n');
  N <- nrow(data);
  actual <- data$C1[1:N];
  actual.mean<-mean(actual);
  actual.centered<-actual - actual.mean;
  #get rid of the first three columns
  pred <- cross.validate.ica(5,
                             data[1:N,],
                             function(data) { return(regression.ica(data)) },
                             function(model, new.data) {return(predict.ica(model, newdata=new.data, A)) },
                             verbose=F );
  # pred <- family$linkinv(pred);
  #pred<-pred$C1
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
  
  return(mean(abs(floor.pred - actual)));  
}
#----------------------------------------------------------------------
regression.ica<-function(data)
{
  ica<-fastICA(data, n.comp = 5, fun = "logcosh", maxit = 200, row.norm = F);
  S<-ica$S; #S containts mixing proportions of the components in each observation
  #add C1 to S for regression purposes
  A<-ica$A;
  #X<-ica$X;
  #K<-ica$K;
  #X.pred<-S%*%A;
  S<-data.frame(S);
  S$C1 <- data$C1;
  lm.ica<-glm(formula = as.formula('C1~X1+X2+X3+X4+X5'), data = S, family = gaussian);
  return(list("model"=lm.ica, "A"=A))
}
#----------------------------------------------------------------------
predict.ica<-function(model, newdata, A =A)
{
  #newdata is already centered
  #decompose newdata into 
  newica<-fastICA(newdata, n.comp = 5, fun = "logcosh", maxit = 200, row.norm = F);
  S<-newica$A;
  S<-data.frame(S);
  S$C1 <- data$C1;
  predictions<-predict(model = model, newdata =S);
  return(predictions%*%A)
}
#----------------------------------------------------------------------
cross.validate.ica <- function(k, data, fit, pred, verbose=F) {
  predictions<-data.frame(data)
  #predictions <- rep(0, nrow(data));
  # Choose the folds.
  folds <- sample(rep(1:k, length=nrow(data)));
  
  for (i in 1:k) {
    
    if (verbose) {
      cat('Fold ', i, '...\n');
      cat('  splitting...\n');
      flush.console();
    }
    
    # Split into out-of-fold and in-fold data.
    train <- data[folds != i,];
    test  <- data[folds == i,];
    #Center everything, because ica will do it anyway
    train.means<-colMeans(train);
    train.centered <- train-train.means;
    test.means<-colMeans(test);
    test.centered <- test-test.means;
    if (verbose) {
      cat('  training...\n');
      flush.console();
    }   
    # Train on the other k-1 folds.
    model.and.A<- fit(train);
    model<-model.and.A$model;
    A<-model.and.A$A;
    if (verbose) {
      cat('  recording...\n');
      flush.console();
    }  
    predictions[which(folds == i),] <- pred(model, test); 
    if (verbose) {
      cat('  done...\n');
      flush.console();
    }
  }
  return(predictions);
}

#data <- get.data.numeric()[,4:400];
test.ica(data);