LDA_predict <- function(model, test, parole_valid_set, n_lettere){
#script per la manipolazione delle uscite ottenute in fase di predict per il classificatore LDA

  options(warn = -1)
  
  test_y <- transpose(test[c(ncol(test)-1)])
  test <- test[-c(ncol(test)-1, ncol(test))]

  predict <- predict(model, newdata = test)
  
  table_results <- data.frame(predict$posterior, predict$x, result = c(1:nrow(predict$posterior)))
  div <- (2 * (parole_valid_set) * n_lettere)
  table_results <- split(table_results, rep(1:div, each = nrow(table_results)/div))

  #selezione del valore corretto dei posterior values ogni gruppo di righe o colonne (in funzione del valore C)
  #in base diffenti ai casi limite
  table_results <- lapply(table_results, function(df){
    if(length(which(df$X.1 == 1))==6){
      #caso: tutti i -1 hanno probabilit? 1-> min tra le probabilit? di X.1
      df$result[which.max(df$X1)]<- 1
      df$result[-which.max(df$X1)]<- -1
    }else{
      #caso:  i -1 hanno probabilit? differenti-> max tra le probabilit? di X1
      df$result[which.min(df$X.1)]<- 1
      df$result[-which.min(df$X.1)]<- -1
    }
    return(df)
  })
  table_results<-do.call(rbind.data.frame, table_results)
  
  #calcolo e stampa su terminale della matrice di confusione, dell'accuracy e della precision
  cat("\n","--------LDA-------","\n","matrice di confusione: ", "\n")
  confusion_matrix <- table(predicted = table_results$result, actual = test_y)
  print(confusion_matrix)
  accuracy <- round(((confusion_matrix["-1", "-1"] + confusion_matrix["1","1"])/nrow(test)),4)
  cat('accuracy: ', accuracy,'\n')
  precision <- round((confusion_matrix["1","1"]/(confusion_matrix["1", "1"] + confusion_matrix["-1","1"])),4)
  cat('precision: ', precision,'\n\n')
  x <- list("new_Y" = table_results$result, "accuracy" = accuracy, "precision" = precision, "model" = model)
  
  return(x)
}
