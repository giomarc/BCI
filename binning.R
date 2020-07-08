binning<-function(datasetX, bin_dim){
#split del datasetX per sensori e per colonne di dimensione 'bin_dim' 

  data_bin <-setNames(split.default(datasetX, rep(1:length(nomi_sensori) , each = (ncol(datasetX)/length(nomi_sensori)))), nomi_sensori)
  data_bin<-lapply(data_bin, function(df){
    
    df<-apply(df, 1, function(row){
      row <- split.default(row, rep(1:(length(row)/bin_dim), each = (bin_dim) ))
      #calcolo della media dei 'bin_dim' valori selezionati
      row <- sapply(row, function(df) mean(df))
      row <- transpose(as.data.frame(row))
    })
    df<-do.call(rbind.data.frame, df)
    return(df)
  })
  data_bin <- as.data.frame(data_bin)
  return(data_bin)
  
}