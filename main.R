library(klaR)
library(e1071)
library(caret)
library(pracma)
library(data.table)
library(dplyr)
library(scales)
library(smooth)
library(greybox)
library(LiblineaR)
library(CORElearn)
source("data_transformation.R")
source("classifiers.R")
source("get_right_words.R")
source("binning.R")
options(warn = -1)

#_______________________________CONFIG____________________________________
setwd("./MOBD_BCI")
n_parole = 6
n_lettere = 5
parole_training_set = 5
parole_valid_set = 1
iterazioni = 10
nomi_sensori <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "PO7", "PO8")


#___________________________LOAD DATASET_______________________________
#import dei file dei dati
datasetX <- read.csv("dataset/X.txt", header = FALSE, sep = " ")
datasetY <- read.csv("dataset/Y.txt", header = FALSE, sep = " ")
datasetC <- read.csv("dataset/C.txt", header = FALSE, sep = " ")


#___________________________FEATURE TRANSFORMATION__________________________
#binning del dataset completo nel dominio del tempo in funzione del valore di 'bin_dim=[4,6,12]'
bin_dim = 12
datasetX_binned <- binning(datasetX, bin_dim)

#ricostruzione del dataset
dataset_completo <- data.frame(datasetX_binned, datasetY, datasetC)
colnames(dataset_completo)[c((ncol(dataset_completo)-1), ncol(dataset_completo))] <- c("Y", "C")
remove(datasetX_binned, datasetX, datasetY, datasetC)

#feature transformation effettuata in funzione del valore di 'transformation_type=['MEDIA','MEDIA_MOBILE', 'DIFF', 'DIFF_1']'
transformation_type = 'MEDIA'
dataset_trasformato <- data_transformation(dataset_completo, n_parole, n_lettere, iterazioni,transformation_type)


#___________________________SPLIT DATASET_______________________________
#split del dataset completo in training_set e test_set, selezionando l'ultima parola come test_set
lista_parole <- split(dataset_trasformato, rep(1:n_parole, each = nrow(dataset_trasformato)/n_parole))
test_set <- lista_parole[[length(lista_parole)]]
lista_parole[length(lista_parole)]<-NULL
training_set <- do.call(rbind.data.frame, lista_parole)


#___________________________FEATURE SELECTION__________________________
#feature selection effettuata in funzione del valore 'estimator=['ReliefFbestK]'
features_weight <- attrEval(formula = training_set$Y ~ .,
                         data = training_set[-c((ncol(training_set)-1), ncol(training_set))],
                         estimator = 'ReliefFbestK',
                         costMatrix = NULL,
                         outputNumericSplits=FALSE)


#___________________________CROSS-VALIDATION_______________________________
#cross-validation effettuata utilizzando 4 parole per il training_set e 1 parola per il test_set 
#utilizzando differenti classificatori in funzione del valore di 'model_type=['NAIVE','LDA','LIB']' 
lista_parole_train <- split(training_set, rep(1:parole_training_set, each = nrow(training_set)/parole_training_set))
n_run  = length(lista_parole_train) / parole_valid_set
classification_res <- vector(mode = "list", length = n_run)
#model_type = "NAIVE"
model_type = "LDA"
#model_type = "LIB"
for (k in 1:n_run) {
  
  # per ogni run associamo al test_i il k-esimo valid_set
  valid_set <- do.call(rbind.data.frame, lista_parole_train[k])
  training_set <- do.call(rbind.data.frame, lista_parole_train[-k])
  cat('\n',k,'\n')
  trainer <- training_set[-c(ncol(training_set))]

  #addestramento del training_set
  classification_res[[k]] <- classifiers(trainer, valid_set, model_type, parole_valid_set, n_lettere, features_weight)
 
  #confronto tra la conversione in caratteri del risultato ottenuto dalla 'predict' 
  #e la versione originale della parola del valid_test
  words_cross_original <- get_right_words(data.frame(Y = valid_set$Y, C=valid_set$C), 5 )
  words_cross_predicted <- get_right_words(data.frame(Y = classification_res[[k]]$new_Y, C= valid_set$C), 5 )
  if(!is.null(words_cross_predicted))
     cat("original: ", unlist(words_cross_original),"\npredicted: ", unlist(words_cross_predicted))

}

#stampa su terminale di 'accuracy' e 'precision' per la k-esima combinazione di training_set e valid_set
mean_classification_res <- data.frame(accuracy = numeric(0), precision = numeric(0) )
for(k in 1:length(classification_res)){
  mean_classification_res<-rbind(mean_classification_res, data.frame(accuracy = classification_res[[k]]$accuracy, precision = classification_res[[k]]$precision))
}
print(mean_classification_res)

#stampa su terminale dei valori medi di 'accuracy' e 'precision' 
mean_classification_res<- colMeans(mean_classification_res)
print(mean_classification_res)

remove(trainer, valid_set, words_cross_original, words_cross_predicted, k,  mean_classification_res)

#_______________________________________FINAL TEST______________________________________
#esecuzione del test finale sulla parola del test_set utilizzando
#il training_set completo (5 parole) per addestrare il classificatore scelto precedentemente
lista_parole_train <- lapply(lista_parole_train, function(df) {
  df$C <- NULL
  return(df)})
training_set_f <- do.call(rbind.data.frame, lista_parole_train)

classification_res_f <- classifiers(training_set_f, test_set, model_type,  parole_valid_set, n_lettere)

#confronto tra la conversione in caratteri del risultato ottenuto dalla 'predict'
#e la versione originale della parola del test_set
original_final_word <- unlist(get_right_words(data.frame(Y= test_set$Y, C=test_set$C), 5 ))
predicted_final_word <- unlist(get_right_words(data.frame(Y= classification_res_f$new_Y, C=test_set$C), 5 ))
cat("\noriginal: ", original_final_word,"\npredicted: ", predicted_final_word)

#_______________________________________MODEL RE-PREPARETION______________________________________
training_set_v <- dataset_trasformato
training_set_v$C<-NULL
features_weight_v <- attrEval(formula = training_set_v$Y ~ .,
                              data = training_set_v[-c(ncol(training_set_v))],
                              estimator = 'ReliefFbestK',
                              costMatrix = NULL,
                              outputNumericSplits=FALSE)


#
#
#
#_______________________________________TEST PER LA VALUTAZIONE______________________________________
#Caricamento dei dati di addestramento del modello
load("./BCImodel_data.RData")

#inserire il path dei files per il test di valutazione
X_test <- read.csv("path_to_test_file_X.txt", header = FALSE, sep = " ")
Y_test <- read.csv("path_to_test_file_Y.txt", header = FALSE, sep = " ")
C_test <- read.csv("path_to_test_file_C.txt", header = FALSE, sep = " ")

#PARAMETRI CONFIGURAZIONE
bin_dim = 12
transformation_type = 'MEDIA'
model_type = 'LDA'


#binning del test
X_test <- binning(X_test, bin_dim)
test_set_v<- data.frame(X_test, Y = Y_test, C = C_test)
#media del test
test_set_v <- data_transformation(test_set_v, 1, 5, 10, transformation_type)

#addestramento
classification_res_v <- classifiers(training_set_v, test_set_v, model_type, 1, 5, features_weight_v) 


original_final_word_v <- unlist(get_right_words(data.frame(Y= test_set_v$Y, C=test_set_v$C), 5 ))
predicted_final_word_v <- unlist(get_right_words(data.frame(Y= classification_res_v$new_Y, C=test_set_v$C), 5 ))
cat("\noriginal: ", original_final_word_v,"\npredicted: ", predicted_final_word_v)

