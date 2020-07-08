# BCI
P300 Speller with patient with ALS

### ESECUZIONE COMPLETA DEL CODICE

Per avviare correttamente il progetto intero è necessario specificare i seguenti parametri all'interno del 
file "main.R", eventualmente modificandoli in base al test che si vuole eseguire:

1. bin_dim (line 37): i valori proposti che hanno mostrato un buon adattamento al modello sono [6, 12] 
		      ma è possibile scegliere un qualunque divisore di 204 per effettuare il binning 
                      sul dataset originale a discapito delle prestazioni;

2. transformation_type (line 46): parametro che serve per selezionare il tipo di trasformazione che si vuole applicare sul 
				  dataset modificato in fase di binning (dataset_completo).
				  I valori possibili sono [MEDIA, MEDIA_MOBILE, DIFF, DIFF_1]; 
				

3. estimator (line 62): il valore impostato è ['ReliefFbestK'], ma se ne possono scegliere altri guardando la documentazione 
			del package CORELearn sulla funzione attreval;

4. model_type (line 73-75): serve per impostare il classificatore desiderato.
			 I valori possibili sono [NAIVE, LDA, LIB];

La configurazione di default prevede [bin_dim = 12, transformation_type = MEDIA, estimator = ReliefFbestK, model_type = LDA],
scelta a seguito dei vari test descritti nella relazione. 

Nota: estrarre il file dataset/X.zip prima dell'avvio del progetto.

#################################################################################################################################


### ESECUZIONE TEST DI VALUTAZIONE

Per effettuare il test per la valutazione del progetto:

1)Importare tutte le librerie (eventualmente scaricandole) e i files sorgente

2)Andare nella sezione #Test per la valutazione#

3)Sostituire i path fittizi dei files corrispondenti ai dataset X,Y,C della parola per il test di valutazione all'interno 
della funzione read.csv (line 145-147) 

4)Eseguire il run delle linee 140-162
