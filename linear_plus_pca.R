
library(MASS);
library(rms);
library(ggplot2);

source('data_prep.R');
source('validation.R');



get.scores<-function(data)
{
  C1.col.num <- which(colnames(data) == "C1");
  data.without.C1<-data[, -C1.col.num];
  data.C1<-data[,C1.col.num];
  data.pca <- prcomp(data.without.C1, tol = 0.001);
  comp1<-data.pca$rotation[,1];
  comp2<-data.pca$rotation[,2];
  comp3<-data.pca$rotation[,3];
  comp4<-data.pca$rotation[,4];
  
  center <- data.pca$center
  scale <- data.pca$scale
  
  scores1<-data.matrix(scale(as.matrix(data.without.C1), center = center, scale = scale))%*%t(t(comp1));
  scores2<-data.matrix(scale(as.matrix(data.without.C1), center = center, scale = scale))%*%t(t(comp2));
  scores3<-data.matrix(scale(as.matrix(data.without.C1), center = center, scale = scale))%*%t(t(comp3));
  scores4<-data.matrix(scale(as.matrix(data.without.C1), center = center, scale = scale))%*%t(t(comp4));
  scores<-data.frame('scores1'=scores1, 'scores2'=scores2, 'scores3'=scores3, 'scores4'=scores4, 'C1'=data$C1);
  cat("  Principal components\n")
  return(list('scores'=scores, 'data.pca'=data.pca ))
}

get.scores.from.old<-function(data, data.pca)
{
  C1.col.num <- which(colnames(data) == "C1");
  data.without.C1<-data[, -C1.col.num];
  data.C1<-data[,C1.col.num];
  #data.pca <- prcomp(data.without.C1, tol = 0.001);
  comp1<-data.pca$rotation[,1];
  comp2<-data.pca$rotation[,2];
  comp3<-data.pca$rotation[,3];
  comp4<-data.pca$rotation[,4];
  
  data.pca.new<-prcomp(data.without.C1, tol = 0.001)
  
  center <- data.pca.new$center
  scale <- data.pca.new$scale
  
  scores1<-data.matrix(scale(as.matrix(data.without.C1), center = center, scale = scale))%*%t(t(comp1));
  scores2<-data.matrix(scale(as.matrix(data.without.C1), center = center, scale = scale))%*%t(t(comp2));
  scores3<-data.matrix(scale(as.matrix(data.without.C1), center = center, scale = scale))%*%t(t(comp3));
  scores4<-data.matrix(scale(as.matrix(data.without.C1), center = center, scale = scale))%*%t(t(comp4));
  scores<-data.frame('scores1'=scores1, 'scores2'=scores2, 'scores3'=scores3, 'scores4'=scores4, 'C1'=data$C1);
  cat("  Principal components\n")
  return(scores)
}
  
  
test.glm <- function(data, formula, family, floor=T, folds=NULL) {

    #cat('Running glm -', family$family, '- on', nrow(data), 'rows of data.\n');
    N <- nrow(data);
    actual <- data$C1[1:N];
    pred <- cross.validate(5,
               data[1:N,],
               function(data) {scores<-get.scores(data); 
                               cat("  proceeding to fit\n"); 
                               return(list('glm'=glm(formula=formula, family=family, data=scores$scores), 'data.pca'=scores$data.pca) )},
	         function(model, new.data) {scores<-get.scores.from.old(new.data, model$data.pca); return(predict(model$glm, newdata=scores)) },
		   folds=NULL,
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

test.lrm.mle <- function(data, formula, folds=NULL) {

    data$C1 <- as.factor(data$C1);

    # Run it on this stuff.
    cat('Running lrm on', nrow(data), 'rows of data.\n');
    N <- nrow(data);
    actual <- as.numeric(data$C1[1:N]);
    pred <- cross.validate(5,
                           data[1:N,],
                           function(data) { scores<-get.scores(data); 
                                            return(list('glm'=lrm(formula, data=scores$scores), 
                                                        'data.pca'=scores$data.pca))},
                           function(model, new.data) {
                             scores<-get.scores.from.old(new.data, model$data.pca)
                             p <- predict(model$glm, newdata=scores, type="fitted.ind");
                             return(apply(p, 1, which.max));
                           },
               folds=folds,
               verbose=T );
    pred <- as.numeric(pred);    

    cat('Base predictions: avg offset =', mean(abs(pred - actual)), '\n');
    cat('All 1: avg offset =', mean(abs(rep(1, N) - actual)), ',', 100*length(which(1 == actual)) / length(actual), '%\n');

    r <- list();
    r$dev <- mean(abs(pred - actual));
    r$pct <- 100*length(which(pred == actual)) / length(actual);
    return(r);

}

build.regularized.plot <- function() {

	library(glmnet);

	train <- data[1:6000,];
	test <- data[6001:8965,];
	form <- formula4;

	mm <- model.matrix(form, data=train);
	jim <- seq(from=0.0001, to=1, by=0.0001);
	fit.glmnet <- glmnet(x=mm, y=train$C1, family='poisson');

	coef(fit.glmnet, s=0.01) # extract coefficients at a single value of lambda

	mm <- model.matrix(form, data=test);
	pred <- predict(fit.glmnet,newx=mm);
	pred <- exp(pred);
	round.pred <- round(pred);
	round.pred[round.pred <= 0] <- 1;
	round.pred[round.pred > 6] <- 6;
	floor.pred <- floor(pred);
	floor.pred[floor.pred <= 0] <- 1;
	floor.pred[floor.pred > 6] <- 6;

	plot(x=fit.glmnet$lambda, y=colMeans(abs(floor(pred) - test$C1)), ylim=c(0,1), type="l");
	lines(x=fit.glmnet$lambda, y=colMeans(abs(round(pred) - test$C1)), col="red");
	lines(x=fit.glmnet$lambda, y=colMeans(abs(pred - test$C1)), col="green");
	lines(x=fit.glmnet$lambda, y=colMeans(abs(floor.pred - test$C1)), ylim=c(0,1), col="grey");
	lines(x=fit.glmnet$lambda, y=colMeans(abs(round.pred - test$C1)), col="pink");
	lines(x=fit.glmnet$lambda, y=colMeans(abs(pred - test$C1)), col="yellow");

	pred <- as.vector(predict(fit.glmnet, newx=mm, s=0.2));

	fit.glm <- glm(form, data=train, family=poisson());
	pred2 <- as.vector(predict(fit.glm, newdata=test));
	pred2 <- exp(pred2);
	floor.pred2 <- floor(pred2);
	floor.pred2[floor.pred2 <= 0] <- 1;
	floor.pred2[floor.pred2 > 6] <- 6;
	lines(x=fit.glmnet$lambda, y=rep(mean(abs(floor.pred2 - test$C1)), length(fit.glmnet$lambda)), col="orange");
	lines(x=fit.glmnet$lambda, y=rep(mean(abs(pred2 - test$C1)), length(fit.glmnet$lambda)), col="brown");
}

#----------------------------------------------------------------------------------
formula<-as.formula('C1 ~ scores1 + scores2 + scores3 + scores4');
data <- get.data.numeric()[,4:400];
#data.backup<-data;
#data<-scores; #scores obtained in pca_individual_components.R 
# Generate a fixed set of folds that everyone will use.
folds <- sample(rep(1:5, length=nrow(data)));
options(stringsAsFactors = FALSE);
results <- data.frame(Algorithm=character(40), 'Features Used'=character(40), 'Input Mode'=character(40), Error=numeric(40), pct=numeric(40), stringsAsFactors=F);

N <- nrow(data);
pred <- rep(1, N);
actual <- as.numeric(data$data.C1[1:N]);
baseline <- mean(abs(pred - actual));
base.pct <- 100*length(which(pred == actual)) / length(actual);
cat('Category 1\n');
results[1,] <- c('GLM - Gaussian', 'Feature Class 1', 'Numeric', test.glm(data, formula, gaussian(), folds=folds));
results[2,] <- c('GLM - Poisson', 'Feature Class 1', 'Numeric', test.glm(data, formula, poisson(), folds=folds));
results[3,] <- c('Ordered Logistic', 'Feature Class 1', 'Numeric', test.lrm.mle(data, formula, folds=folds));

# All done!
results$Error <- as.numeric(results$Error);
results$pct <- as.numeric(results$pct);

ggplot(results[1:20,], aes(x=Features.Used, y=Error, fill=Features.Used)) +
	geom_bar() + facet_grid(. ~ Algorithm) +
	scale_y_continuous(name='Mean Error') +
	scale_x_discrete(name=NA) +
	#scale_fill_discrete(name='Features Used') +
	opts(axis.text.x = theme_blank(), axis.title.x = theme_blank(), axis.ticks = theme_blank());
