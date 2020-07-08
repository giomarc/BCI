LIB_predict <- function(model, test){
#script per la manipolazione delle uscite ottenute in fase di predict per il classificatore LibLinear
  
  test_y <- transpose(test[c(ncol(test)-1)])
  test <- test[-c(ncol(test)-1, ncol(test))]
  
  test_prediction <- predict(model,test,decisionValues = TRUE)
  
  #selezione del valore massimo dei decision values ogni gruppo di righe o colonne (in funzione del valore C)
  temp <- setNames(data.frame(test_prediction$decisionValues[,1], test_prediction$predictions), c('dec','Y'))
  temp <- split(temp, rep(1:(nrow(temp)/6), each=6))
  temp<- lapply(temp, function(df){

    df[which.max(df$dec),]$Y<-1
    df[-which.max(df$dec),]$Y<- -1
    df<-as.data.frame(df)
    return(df)
  })
  test_prediction <- do.call(rbind.data.frame, temp)

  #calcolo e stampa su terminale della matrice di confusione, dell'accuracy e della precision
  cat("\n","--------LIB-------","\n","matrice di confusione: ", "\n")
  confusion_matrix <- table(predicted=test_prediction$Y, actual = test_y)
  print(confusion_matrix)
  accuracy <- round(((confusion_matrix["-1", "-1"] + confusion_matrix["1","1"])/nrow(test)),4)
  cat('accuracy: ', accuracy,'\n')
  precision <- round((confusion_matrix["1","1"]/(confusion_matrix["1", "1"] + confusion_matrix["-1","1"])),4)
  cat('precision: ', precision,'\n\n')
  x<- list("new_Y" = test_prediction$Y, "accuracy" = accuracy, "precision" = precision, "model" = model)
  
  return(x)
}