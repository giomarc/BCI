NB_predict <- function(model, test){
#script per la manipolazione delle uscite ottenute in fase di predict per il classificatore Naive Bayes

  test_y <- transpose(test[c(ncol(test)-1)])
  test <- test[-c(ncol(test)-1, ncol(test))]
  
  predict_raw <- predict(model,test[,- c((ncol(test)-1),ncol(test))], type = "raw")
  predict_n <- predict(model,test[,- c((ncol(test)-1),ncol(test))], type = "class")
  
  predict<-data.frame(predict_raw, new_Y = predict_n)
  colnames(predict) <- c("-1", "1", "new_Y")
  
  #selezione del valore corretto dei raw values ogni gruppo di righe o colonne (in funzione del valore C)
  #in base diffenti ai casi limite
  predict <- split(predict, rep(1:(nrow(predict)/6), each=6))
  predict <- lapply(predict, function(df){
    if(length(which(df$`1`== 1))>1){
      df[intersect(which.min(df$`-1`) , which(df$`1`== 1)),]$new_Y <- 1
      df[-intersect(which.min(df$`-1`) , which(df$`1`== 1)),]$new_Y <- -1
    }else if( length(which(df$`1`== 1)) < 1) {
      df[which.max(df$`1`),]$new_Y <- 1
      df[-which.max(df$`1`),]$new_Y <- -1
    }
    else{
      df[-intersect(which.min(df$`-1`) , which.max(df$`1`)),]$new_Y <- -1
    }
    return(df)
  })
  predict <- do.call(rbind.data.frame, predict)

  #calcolo e stampa su terminale della matrice di confusione, dell'accuracy e della precision
  cat("\n","--------NAIVE-------","\n","matrice di confusione: ", "\n")
  confusion_matrix <- table(predicted = predict$new_Y, actual = test_y)
  print(confusion_matrix)
  accuracy <- round(((confusion_matrix["-1", "-1"] + confusion_matrix["1","1"])/nrow(test)),4)
  cat('accuracy: ', accuracy,'\n')
  precision <- round((confusion_matrix["1","1"]/(confusion_matrix["1", "1"] + confusion_matrix["-1","1"])),4)
  cat('precision: ', precision,'\n\n')
  x<- list("new_Y" = predict$new_Y, "accuracy" = accuracy, "precision" = precision, "model" = model)
  
  return(x)
}