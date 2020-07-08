data_transformation<-function(dataset, num_par, n_lettere, iterazioni, transformation_type){
  
  x <- split(dataset, rep(1:num_par, each = nrow(dataset)/num_par))
  x <- lapply(x, function(df) split(df, rep(1:n_lettere, each = nrow(df)/n_lettere)))
  x <- lapply(x, function(df) {
    df<-lapply(df, function(df2) {
    #raggruppamento delle prove rispetto alla stessa riga/colonna dell'esperimento
      df2<-df2[order(df2$C), ]
    })
    #ricostruzione del dataframe
    df <- do.call(rbind.data.frame, df)
  })
  #ricostruzione del dataframe
  x <- do.call(rbind.data.frame, x)
  
  #esecuzione della detrendizzazione del segnale per riga
  if(strcmp(transformation_type, 'DIFF')){
    return(apply_diff(x,num_par, n_lettere, iterazioni))
  }
  #esecuzione della media aritmetica in funzione del valore C
  else if(strcmp(transformation_type, 'MEDIA')){
    return(apply_media(x,iterazioni))
  }
  #esecuzione dell'estrazione del trend del segnale
  else if(strcmp(transformation_type, 'MEDIA_MOBILE')){
    return(apply_media_mobile(x,num_par, n_lettere, iterazioni))
  } 
  #esecuzione della detrendizzazione del segnale per sensore
  else if(strcmp(transformation_type, 'DIFF_1')){
    return(apply_diff_per_sensore(x,num_par, n_lettere, iterazioni))
  }
  
}




apply_diff <- function(x, num_par, n_lettere, iterazioni){

  Y <- split(x$Y, rep(1:(num_par*n_lettere*12), each = nrow(x)/(num_par*n_lettere*12)))
  Y <- sapply(Y, function(df) unique(df))
  C <- split(x$C, rep(1:(num_par*n_lettere*12), each = nrow(x)/(num_par*n_lettere*12)))
  C <- sapply(C, function(df) unique(df))
  
  x$C <- NULL
  x$Y <- NULL
  y <- ts(x)
  y <- split(x, rep(1:(num_par*n_lettere*12), each = nrow(x)/(num_par*n_lettere*12)))
  y <- lapply(y, function(df){
      row<-ts(df)
      row<-diff(row,lag = 1, differences = 1)
      row<-data.frame(row)
      return(row)
  })
  
  x <- transpose(data.frame(sapply(y, function(df) colMeans(df))))
  x <- data.frame(x, Y=Y, C=C)
  return(x)
}


apply_media <- function(x, iterazioni){
  x <- aggregate(x,list(rep(1:(nrow(x)%/%iterazioni+1),each=iterazioni,len=nrow(x))),mean)[-1]
  return(x)
}


apply_media_mobile <- function(x, num_par, n_lettere, iterazioni){
  Y <- split(x$Y, rep(1:(num_par*n_lettere*12), each = nrow(x)/(num_par*n_lettere*12)))
  Y <- sapply(Y, function(df) unique(df))
  C <- split(x$C, rep(1:(num_par*n_lettere*12), each = nrow(x)/(num_par*n_lettere*12)))
  C <- sapply(C, function(df) unique(df))
  
  y <- ts(x)
  y$C <- NULL
  y <- split(x, rep(1:(num_par*n_lettere*12), each = nrow(x)/(num_par*n_lettere*12)))#TODO: 20parametro 4*5*10
  y <- lapply(y, function(df){
    if(length(which(df$Y == -1))==iterazioni){
      row<-ts(df)
      detrend<-diff(row,lag = 1, differences = 1)
      ma <- (row - detrend )
      row<-data.frame(row)
    }else{
      return(data.frame(df))
    }
  })
  
  x <- transpose(data.frame(sapply(y, function(df) colMeans(df))))
  colnames(x)[c( (ncol(x)-1), ncol(x))] <- c("Y", "C")
  x$Y[which(x$Y==0)]<- -1
  x$C <- C
  return(x)
}


apply_diff_per_sensore<- function(x, num_par, n_lettere, iterazioni){
 
  Y <- split(x$Y, rep(1:(num_par*n_lettere*12), each = nrow(x)/(num_par*n_lettere*12)))
  Y <- sapply(Y, function(df) unique(df))
  C <- split(x$C, rep(1:(num_par*n_lettere*12), each = nrow(x)/(num_par*n_lettere*12)))
  C <- sapply(C, function(df) unique(df))
  
  data_sensori<-list(
    X = NULL,
    Y = Y,
    C = C
  )
  df<-x
  df[,c((ncol(df)-1), ncol(df))]<-NULL
  
  data_sensori$X <- setNames(split.default(df, rep(1:length(nomi_sensori) , each = (ncol(df)/length(nomi_sensori)))), nomi_sensori)
  
  data_sensori$X <- lapply(data_sensori$X, function(sensore){
    y <- split(sensore, rep(1:(6*5*12), each = nrow(sensore)/(6*5*12)))
    y <- lapply(y, function(df){
      
      row<-ts(df)
      row<-diff(ts(df),lag = 1, differences = 1)
      row<-data.frame(row)
      return(row)
    })
    y <- lapply(y, function(df) colMeans(df))
    y <- transpose(as.data.frame(y))
  })
  
  data_sensori$X <- as.data.frame(data_sensori$X)
  data_sensori <- as.data.frame(data_sensori)
  return(data_sensori)
}

  