source("NB_predict.R")
source("LDA_predict.R")
source("LIB_predict.R")

classifiers <- function(trainer, test, model_type, n_parole_valid_set, n_lettere, features_weight){
#addestramento effettuato in funzione del valore 'model_type' 
#utilizzando il valore 'features_weight' per pesare le features
  
  set.seed(123)
  
  #macchina di addestramento LDA
  if(strcmp(model_type, 'LDA')){
    model <- lda(Y ~ ., data = trainer, na.action = "na.omit", weight = features_weight)
    predict <- LDA_predict(model, test, n_parole_valid_set, n_lettere)
  }
  
  #macchina di addestramento NAIVE
  else if(strcmp(model_type, 'NAIVE')){
    model <- naiveBayes(as.factor(Y) ~ .,
                        trainer,
                        laplace = 1, useKernel = T,
                        type = "raw", weights = features_weight)
    
    predict <- NB_predict(model, test)
  }
  
  #macchina di addestramento LIB
  else if(strcmp(model_type, 'LIB')){
  
    model <- LiblineaR(data = trainer[-ncol(trainer)],
                       target= trainer$Y,
                       type=1, cost=1, bias=TRUE,
                       verbose=FALSE, WI = features_weight)
    predict <- LIB_predict(model, test)
  }
  return(predict)
}