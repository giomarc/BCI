get_right_words <- function(subdf, n_lettere){
#traduzione in caratteri delle parole utilizzate (orginali o 'predicted') per stampa su terminale della parola  
  
  #matrice dei caratteri
  result_panel <- matrix(c("A", "B", "C", "D", "E", "F", 
                           "G", "H", "I", "J", "K", "L",
                           "M", "N", "O", "P", "Q", "R",
                           "S", "T", "U", "V", "W", "X",
                           "Y", "Z", "1", "2", "3", "4",
                           "5", "6", "7", "8", "9", "_"), nrow = 6, ncol = 6, byrow = TRUE)
  
  #accesso alla matrice dei caratteri in base al valore di C corrispondente all'uscita corretta (Y=1)
  subdf <- subdf[subdf$Y != -1, ] 
  if(nrow(subdf) < (n_lettere*2)-1){
    print("Errore nella classificazione delle parole")
    words <-NULL
  }
  else{
    subdf$Y <- NULL
    subdf <- split(subdf, rep(1:(nrow(subdf)/2), each=2))
    words <- lapply(subdf, function(fd){
      if(fd$C[1]>6)
        fd$C[1]<- (fd$C[1]+1)%%6
      result_panel[fd$C[1], (fd$C[2]-6)]
    })
    words <- split(words, rep(1:(length(words)/n_lettere), each=n_lettere))
  }
  return(words)
}