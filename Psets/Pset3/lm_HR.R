lm_HR = function(data,testIndexes){
  data_test = data[testIndexes,]
  data_train = data[-testIndexes,]
  x_col = setdiff(names(data_test),"medv")
  x_test = as.matrix(data_test[,x_col])
  y_test = as.matrix(data_test[,"medv"])
  x_train = as.matrix(data_train[,x_col])
  y_train = as.matrix(data_train[,"medv"])
  df_lr = as.data.frame(cbind(x_train,y_train))
  names(df_lr)[ncol(df_lr)]<-paste("medv")
  output_lr = lm(log(medv) ~., data=df_lr)
  mse_train = sqrt(mean(output_lr$residuals^2))
  y_pred = predict(output_lr, newdata=as.data.frame(x_test))
  mse_test = sqrt(mean((y_pred-y_test)^2))
  df_lr = as.data.frame(cbind(mse_train,mse_test))
  lm_HR = list(output_lr,y_pred,df_lr)
  names(lm_HR) = c("output_lr","y_pred","df_lr")
  return(lm_HR)
}