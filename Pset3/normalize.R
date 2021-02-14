normalize = function(data, type){
  M = as.matrix(data)
  if(type == 1){ # divide by the mean
    N = M/colMeans(M)
  }
  if(type == 2){ # rescale to 0-1
    N = (M - apply(M,2,min))/(apply(M,2,max) - apply(M,2,min))
  }
  if(type == 3){ # subtract mean, divide by std
    N = (M - colMeans(M))/(apply(M, 2, sd))
  }
  data = as.data.frame(round(N,3))
  return(data)
}