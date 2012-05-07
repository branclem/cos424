
source('data_prep.R');
source('validation.R');

#data <- get.data.numeric();

# special prediction function for k-means cluster assignment
kmeans.predict <- function(model, new.data, data, clusters) {
  c1vals = rep(0, clusters)
  for (i in 1:dim(data)[1]) {
    c1vals[model$cluster[i]] = c1vals[model$cluster[i]] + data[i, 77]
  }
  for (i in 1:length(c1vals)) {
    c1vals[i] = c1vals[i]/model$size[i]
  }
  
  predictions = rep(0, dim(new.data)[1])
  for (i in 1:dim(new.data)[1]) {
    row = new.data[i,]
    closest = 1
    closest.dist = dist(rbind(row, model$centers[1,]))
    for (j in 2:dim(model$centers)[1]) {
      distance = dist(rbind(row, model$centers[j,]))
      if (distance < closest.dist) {
        closest = j
        closest.dist = distance
      }
    }
    
    predictions[i] = c1vals[closest]
  }
  return(predictions)
}

test.kmeans <- function(base, data, clusters) {
  #multmix.data<-makemultdata(raws, cuts = median(c(data$C1, data$A1, data$A2, data$A5, data$D3A, data$D3B)))
  #kmeans <- kmeans(raws, 6)
  #summary(kmeans) 
  #survey <- cbind(data[,1:76], data[,142:265])
  survey <- cbind(base$A1, base$A2, base$A3, base$A4, base$A5, base$A6, base$A7A,
                  base$A7B, base$A7C, base$A7D, base$G1, base$G2, base$G3A, base$G3B,
                  base$G3C, base$G3D, base$G3E, base$G4)
  
  
  # Run it on this stuff.
  cat('Running kmeans on', nrow(data), 'rows of data.\n');    N <- nrow(data);
  actual <- as.numeric(data[1:N,77]);
  pred <- cross.validate(5,
                         survey[1:N,],
                         function(d) { return(kmeans(d, clusters)) },
                         function(model, new.data) {return(kmeans.predict(model, new.data, data, clusters)) },
                         verbose=F );
  pred <- as.numeric(pred);
  
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
}

raws<-as.matrix(data)[,4:268]
test.kmeans(data, raws, 6)