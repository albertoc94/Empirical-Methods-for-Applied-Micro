cross_val = function(data,model,df,K){
  
  #df = degrees of freedom (eg degrees of the polynomial or knots)
  #set.seed(1)
  data.shuffled <- data[sample(nrow(data)),]
  folds <- cut(seq(1,nrow(data.shuffled)),breaks=K,labels=FALSE)
  #Creating empty object to hold fit information
  rmse = matrix(NA,nrow=K,ncol=length(df))
  for(i in 1:K){
    #Segement data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- data.shuffled[testIndexes, ]
    trainData <- data.shuffled[-testIndexes, ]
    #Use the test and train data partitions
    #Model fitting and evaluation
    for (j in 1:length(df)){
      d = df[[j]]
      #training regression model on training folds
      if(model == "polyreg"){
        fit.train = lm(lmp ~ poly(temp,d), data = trainData)
      }
      if(model == "spline"){
        fit.train = lm(lmp ~ ns(temp, df = d+2), data = trainData)
      }
      if(model == "loess"){
        fit.train = loess(lmp ~ temp, span = d, data = trainData)
      }
      #evaluating fit on the test fold
      fit.test = predict(fit.train, newdata=testData)
      rmse[i,j] = sqrt(mean(na.omit(fit.test - testData$lmp)^2))
    }
  }
  return(rmse)
}