###################################
#### CESTI DI NATALE 2019-2020 ####
#### Analysis                  ####
####  - Ambrosi  Andrea        ####
####  - Balliu   Bertiana      ####
####  - Marchesi Raffaele      ####
####  - Parolin  Irene         ####
###################################

# libreria per multinomial logit models 
library(mlogit)

# importiamo il dataset delle risposte al questionario, gi? pulito e in formato long
risposte_df <- read.csv("risposte_df.csv", na.strings = c("NaN")) # specifo le stringhe NA per non perdere la provincia di napoli

# statistiche descrittive
summary(risposte_df)

# variazione degli attributi del questionario in base alla scelta
xtabs(scelta ~ dolce,        data = risposte_df)
xtabs(scelta ~ bottiglia,    data = risposte_df)
xtabs(scelta ~ salame,       data = risposte_df)
xtabs(scelta ~ torrone,      data = risposte_df)
xtabs(scelta ~ cioccolatini, data = risposte_df)
xtabs(scelta ~ miele,        data = risposte_df)
xtabs(scelta ~ grana,        data = risposte_df)
xtabs(scelta ~ prezzo,       data = risposte_df)

# recode delle variabili. 
# quelle che descrivono la presenza o assenza di un prodotto sono logical. 
# consideriamo inizialmente il prezzo come categorico e non come numerico, visto che ? composto da solo 4 livelli 

risposte_df$salame       <- as.logical(risposte_df$salame      )
risposte_df$torrone      <- as.logical(risposte_df$torrone     )
risposte_df$cioccolatini <- as.logical(risposte_df$cioccolatini)
risposte_df$miele        <- as.logical(risposte_df$miele       )
risposte_df$grana        <- as.logical(risposte_df$grana       )
risposte_df$prezzo       <- as.factor( risposte_df$prezzo      )

# modifichiamo le variabili demografiche in modo che rappresentino categorie sufficientemente grandi
risposte_df$classeEta <- ifelse(risposte_df$anni < 25, "<25",
                                ifelse(risposte_df$anni >= 25 & risposte_df$anni < 40, "25-39",
                                       ifelse(risposte_df$anni >= 40 & risposte_df$anni < 55, "39-54", 
                                              ">54")))
risposte_df$classeEta <- as.factor(risposte_df$classeEta) 
levels(risposte_df$occupazione)[levels(risposte_df$occupazione) == "Disoccupato"] <- "Disoccupato/Altro"
levels(risposte_df$occupazione)[levels(risposte_df$occupazione) == "Altro"]       <- "Disoccupato/Altro"

risposte_df$ID_domanda <- ifelse(risposte_df$blocco == 2, 
                                 risposte_df$ID_domanda + 8, 
                                 risposte_df$ID_domanda) #cambiamo l'id delle domande del secondo blocco


# per usare le funzioni della libreria "mlogit" occorre trasformare i nostri dati in un oggetto mlogit.data

cesti.mlogit <- mlogit.data(data    = risposte_df     , # il nostro dataset
                            choice  = "scelta"        , # la variabile che rappresenta le scelte dei rispondenti
                            shape   = "long"          , # il formato del dataset in input
                            varying = 6:13            , # le colonne degli attributi del questionario
                            alt.var = "alternativa"   , # la colonna delle alternative
                            id.var  = "ID_rispondente") # la colonna degli id dei rispondenti


#################################
########## MNL models ###########
#################################

# il primo modello ? una regressione semplice con tutte le variabili del questionario e una intercetta diversa per ogni alternativa
m1 <- mlogit(scelta ~ dolce + bottiglia + salame + torrone + cioccolatini 
                    + miele + grana + prezzo, data = cesti.mlogit)
summary(m1)

# il secondo modello ha una intercetta fissa, quindi non distingue tra le posizioni delle alternative. Nel nostro questionario erano casuali e non rappresentano nulla
# per farlo usiamo il simbolo "|", che separa le variabili specifiche dell'alternativa da quelle che non lo sono
m2 <- mlogit(scelta ~ dolce + bottiglia + salame + torrone + cioccolatini 
                    + miele + grana + prezzo | -1, data = cesti.mlogit)
summary(m2)


# per confrontare i modelli usiamo un likelihood ratio test
# il likelihood ratio test mette a confronto due modelli costruiti per predirre gli stessi dati.
# la likelihood dei due modelli rappresenta la bont? del fit.
# se il test ? significativo significa che il modello con la likelihood pi? alta rappresenta i dati significativamente meglio dell'altro 
#  (https://stats.stackexchange.com/questions/254643/model-selection-with-lr-test-how-to-interpret-the-result)

lrtest(m2, m1)    # anche se controintuitivo a noi viene un test significativo: 
                  # il modello alternativo (m1) ha un fit migliore di quello usato come null hypothesis (m2). 
                  # Ma  in questo caso sappiamo che si tratta di un caso e che le intercette non devono essere tenute in considerazione per la nostra analisi

# visto che procederemo senza considerare l'influenza delle intercette, mostriamo che nessuno abbia risposto a caso scegliendo sempre la stessa alternativa
# per controllare che nessuno abbia risposto a caso  conto il numero di volte che ogni rispondente a scelto la stessa alternativa
# (per eseguire il controllo occorre caricare il file del preprocessing e decommentare le righe seguenti)
# table(apply(risposte[3:10], 1, function(x) length(which(x==1))))
# table(apply(risposte[3:10], 1, function(x) length(which(x==2))))
# table(apply(risposte[3:10], 1, function(x) length(which(x==3))))
# ci sono casi in cui un rispondente ha scelto fino a 7 volte su 8 la stessa alternativa, ma mai nessuno per 8 su 8
# continuiamo assumendo che lo sbilanciamento nella scelta delle alternative sia semplicemente un caso

# proviamo a considerare il prezzo come una variabile numerica
m3 <- mlogit(scelta ~ dolce + bottiglia + salame + torrone + cioccolatini 
                    + miele + grana + as.numeric(as.character(prezzo)) 
                    | -1, data = cesti.mlogit)
summary(m3)

# confrontiamo i modelli con prezzo categorico e numerico
lrtest(m3, m2)    # a noi viene un test significativo: 
                  # significa che m2 riesce a rappresentare i dati meglio di m3.  
                  # questo dovrebbe dipendere dal fatto che la classe 20euro non segue un'andamento lineare rispetto alle altre


###################################
####### willingness to pay ########
###################################

# la willingness to pay serve a dare una rappresentazione concreta dei coefficienti della regressione,
# rappresentandoli come differenze nel prezzo che i rispondenti sono disposti a spendere per avere uno specifico prodotto
# Per calcolarla siamo costretti ad utilizzare i coefficienti del modello m3, anche se ? peggiore di m2. 
# Questo perche ? necessario avere il prezzo come variabile quantitativa per poter stimare la willingness to pay


# il valore che si ottiene ? la differenza che separa mediamente i rispondenti tra scegliere 
# l'alternativa rappresentata dal coefficiente oppure quello alternativa con l'aggiunta della differenza
# noi abbiiamo quasi sempre valori negativi, visto che le nostre variabili rappresentano la presenza o assenza di un prodotto
listawtp <- list()
listawtp <- append(listawtp, coef(m3)["dolcepanettone"]   /(coef(m3)["as.numeric(as.character(prezzo))"])) #la gente vuole il pandoro
listawtp <- append(listawtp, coef(m3)["bottigliaspumante"]/(coef(m3)["as.numeric(as.character(prezzo))"]))
listawtp <- append(listawtp, coef(m3)["salameTRUE"]       /(coef(m3)["as.numeric(as.character(prezzo))"]))
listawtp <- append(listawtp, coef(m3)["cioccolatiniTRUE"] /(coef(m3)["as.numeric(as.character(prezzo))"]))
listawtp <- append(listawtp, coef(m3)["torroneTRUE"]      /(coef(m3)["as.numeric(as.character(prezzo))"]))
listawtp <- append(listawtp, coef(m3)["mieleTRUE"]        /(coef(m3)["as.numeric(as.character(prezzo))"]))
listawtp <- append(listawtp, coef(m3)["granaTRUE"]        /(coef(m3)["as.numeric(as.character(prezzo))"])) 



#abbiamo creato una lista ordinata con i coefficienti  
listawtp<-listawtp[order(-abs(unlist(listawtp)))]
listawtp


######################################################
########## Simulating Preference Shares ##############
######################################################

# definiamo la funzione "predict.mnl" per simulare le quote di preferenza 

predict.mnl <- function(model, data) {
  # Function for predicting prefernce shares from a MNL model 
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to 
  #       predict shares.  Same format at the data used to estimate model. 
  data.model   <- model.matrix(update(model$formula, 0 ~ .), data = data)[ , -1]
  logitUtility <- data.model%*%model$coef
  share        <- exp(logitUtility) / sum(exp(logitUtility))
  cbind(share, data)
}

# per simulare le quote di preferenza dobbiamo generare un insieme di design si prodotti
# che rappresentano il mercato. uno di questi prodotti rappresenter? quello che vogliamo
# inserire nel mercato, mentre tutti gli altri saranno i concorrenti gi? presenti.
# per evitare il problema della IIA Property ? importante evitare di scegliere design troppo simili tra loro


# generiamo la lista di tutti i possibili design che si possono ottenere dai nostri atributi
attributes <- list(dolce        = names(table(cesti.mlogit$dolce))       ,
                   bottiglia    = names(table(cesti.mlogit$bottiglia))   ,
                   salame       = names(table(cesti.mlogit$salame))      ,
                   torrone      = names(table(cesti.mlogit$torrone))     ,
                   cioccolatini = names(table(cesti.mlogit$cioccolatini)),
                   miele        = names(table(cesti.mlogit$miele))       ,
                   grana        = names(table(cesti.mlogit$grana))       ,
                   prezzo       = names(table(cesti.mlogit$prezzo)))
allDesign <- expand.grid(attributes) 
allDesign 

# tra tutti i design segliamo un sottoinsieme non casuale di prodotti con caratteristiche sensate e diverse tra loro,
# per rappresentare il mercato (il numero di prodotti si puo cambiare)
# uno dei prodotti sar? quello che vogliamo introdurre nel mercato. 

#new.data <- allDesign[c(7,  65, 172, 209, 302, 343, 445, 508), ] # versione bilanciata con 8 cesti
#new.data <- allDesign[c(364, 7, 209, 226, 302, 343, 445), ]      # versione meno bilanciata con 7 cesti
new.data <- allDesign[c(364, 7, 209, 226, 302, 343, 445), ]       #16 euro
#new.data <- allDesign[c(243, 7, 209, 226, 302, 343, 445), ]      #13 euro


new.data

# prevediamo come si dividerebbero le quote di preferenza sull'insieme dei design che rappresentano il nostro mercato.
# ovvero cosa sceglierebbero i rispondenti se dovessero scegliere tra queste alternative
# in particolare ci interessa che quota guadagnerebbe il nostro design (il primo della lista)

predict.mnl(m2, new.data) 

# possiamo anche vedere quali sarebbero le quote senza il nostro design
predict.mnl(m2, new.data[-1, ]) 

# calcoliamo il confidence interval per le quote di preferenza 
source('support_functions.R') #file esterno con le funzioni di bootstrap
predIC <- BootCI.predict.mnl(m2, new.data, nsim = 100)
predIC
predIC[ , 1:3] <- round(predIC[ , 1:3], 3)
predIC

# con un sensitivity chart possiamo visualizzare come varierebbe la previsione di quota di preferenza
# per il nostro design, al variare di ogni suo attributo, mantenendo inalterato il resto del mercato

# la funzione "sensitivity.mnl" cicla su tutti i livelli degli attributi calcolando ogni volta le quote di preferenza
sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
  # Function for creating data for a preference share-sensitivity chart
  # model: mlogit object returned by mlogit() function
  # attrib: list of vectors with attribute levels to be used in sensitivity
  # base.data: data frame containing baseline design of target product
  # competitor.data: data frame contining design of competitive set
  data       <- rbind(base.data, competitor.data)
  base.share <- predict.mnl(model, data)[1, 1]
  share      <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,  ] <- base.data
      data[1, a] <- i
      share      <- c(share, predict.mnl(model, data)[1, 1])
    }
  }
  data.frame(level = unlist(attrib), share = share, increase = share - base.share)
} 

# calcoliamo i tradeoff per per ogni livello degli attributi del nostro design
base.data       <- new.data[1, ]
competitor.data <- new.data[-1, ]
tradeoff        <- sensitivity.mnl(m2, attributes, base.data, competitor.data)

# visualizziamo il Sensitivity Chart per il modello fixed effects MNL 
barplot(tradeoff$increase, 
        horiz     = FALSE, 
        names.arg = c("pand"  , "panett", "pass"  , "spum"  , "salNO" ,
                      "salSI" , "torrNO", "torrSI", "ciocNO", "ciocSI",
                      "mielNO", "mielSI", "graNO" , "graSI" ,
                      "p10"   , "p13"   , "p16"   , "p20"),
        ylab      = "Change in Share for the Planned Product Design", 
        ylim      = c(-0.1, 0.11))
grid(nx = NA, ny = NULL)

# individuiamo il design ideale da inserire nel mercato, viste le informazioni a nostra disposizione, 
#per il modello fixed effects MNL
allDesign[  allDesign$dolce        == "pandoro"  &
            allDesign$bottiglia    == "spumante" &
            allDesign$salame       == "FALSE"    & 
            allDesign$torrone      == "FALSE"    & 
            allDesign$cioccolatini == "TRUE"     & 
            allDesign$miele        == "TRUE"     & 
            allDesign$grana        == "TRUE"     & 
            allDesign$prezzo       == "16"     , ]
# numero 371
# ? il pacco che vorremmo mettere in vendita perch? racchiuderebbe le preferenze dei consumatori con la massimizzazione del prezzo 

new.data2 <- allDesign[c(371, 7, 209, 226, 302, 343, 445), ]

predict.mnl(m2, new.data2)

################################################
######## Individual level predictors ###########
################################################

# possiamo calcolare un modello che tiene conto anche di variabili a livello di rispondente (variabili demografiche)

#  proviamo ad aggiungere la interazione di una variabile alla volta

# destinatario
m4_dest <- mlogit(scelta ~ dolce + bottiglia + salame + torrone + cioccolatini + miele + grana + prezzo | 
                           dolce * destinatario + bottiglia * destinatario + salame * destinatario 
                         + torrone * destinatario + cioccolatini * destinatario + miele * destinatario 
                         + grana * destinatario + prezzo * destinatario , data = cesti.mlogit)
summary(m4_dest)

lrtest(m2, m4_dest)

# genere
m4_genere <- mlogit(scelta ~ dolce + bottiglia + salame + torrone + cioccolatini + miele + grana + prezzo | 
                             dolce * genere + bottiglia * genere + salame * genere + torrone * genere 
                           + cioccolatini * genere + miele * genere + grana * genere + prezzo * genere, 
                           data = cesti.mlogit)
summary(m4_genere)

lrtest(m2, m4_genere)

# occupazione
m4_occ <- mlogit(scelta ~ dolce + bottiglia + salame + torrone + cioccolatini + miele + grana + prezzo | 
                          dolce * occupazione + bottiglia * occupazione + salame * occupazione 
                        + torrone * occupazione + cioccolatini * occupazione + miele * occupazione 
                        + grana * occupazione + prezzo * occupazione, data = cesti.mlogit)
summary(m4_occ)

lrtest(m2, m4_occ)

# anni
m4_anni <- mlogit(scelta ~ dolce + bottiglia + salame + torrone + cioccolatini + miele + grana + prezzo | 
                           dolce * anni + bottiglia * anni + salame * anni + torrone * anni 
                         + cioccolatini * anni + miele * anni + grana * anni + prezzo * anni, 
                         data = cesti.mlogit)
summary(m4_anni) 

lrtest(m2, m4_anni)

# classe eta'
m4_etaclass <- mlogit(scelta ~ dolce + bottiglia + salame + torrone + cioccolatini + miele + grana + prezzo | 
                               dolce * classeEta + bottiglia * classeEta + salame * classeEta 
                             + torrone * classeEta + cioccolatini * classeEta + miele * classeEta 
                             + grana * classeEta + prezzo * classeEta, data = cesti.mlogit)
summary(m4_etaclass)

lrtest(m2, m4_etaclass)



# completo
m4 <- mlogit(scelta ~ dolce + bottiglia + salame + torrone + cioccolatini + miele + grana + prezzo | 
                      dolce * destinatario + bottiglia * destinatario + salame * destinatario 
                    + torrone * destinatario + cioccolatini * destinatario + miele * destinatario 
                    + grana * destinatario + prezzo * destinatario + dolce * genere + bottiglia * genere 
                    + salame * genere + torrone * genere + cioccolatini * genere + miele * genere 
                    + grana * genere + prezzo * genere + dolce * occupazione + bottiglia * occupazione 
                    + salame * occupazione + torrone * occupazione + cioccolatini * occupazione 
                    + miele * occupazione + grana * occupazione + prezzo * occupazione + dolce * classeEta 
                    + bottiglia * classeEta + salame * classeEta + torrone * classeEta + cioccolatini * classeEta 
                    + miele * classeEta + grana * classeEta + prezzo * classeEta, data = cesti.mlogit)
summary(m4)

lrtest(m2, m4) # bisogna considerare che il numero di degrees of freedom ? molto alto

lrtest(m4_etaclass, m4)

# completo con prezzo numerico
m5 <- mlogit(scelta ~ dolce + bottiglia + salame + torrone + cioccolatini + miele + grana + as.numeric(as.character(prezzo)) | 
                      dolce * destinatario + bottiglia * destinatario + salame * destinatario + torrone * destinatario 
                    + cioccolatini * destinatario + miele * destinatario + grana * destinatario + as.numeric(as.character(prezzo)) * destinatario 
                    + dolce * genere + bottiglia * genere + salame * genere + torrone * genere + cioccolatini * genere + miele * genere 
                    + grana * genere + as.numeric(as.character(prezzo)) * genere + dolce * occupazione + bottiglia * occupazione + salame * occupazione 
                    + torrone * occupazione + cioccolatini * occupazione + miele * occupazione + grana * occupazione + as.numeric(as.character(prezzo)) * occupazione 
                    + dolce * classeEta + bottiglia * classeEta + salame * classeEta + torrone * classeEta + cioccolatini * classeEta + miele * classeEta 
                    + grana * classeEta + as.numeric(as.character(prezzo)) * classeEta, data = cesti.mlogit)

summary(m5)

lrtest(m5, m4) # anche in questo caso si ha un risultato peggiore con il prezzo numerico


###############################################################################
############ Controlling for consumer heterogeneity - mixed MNL model##########
###############################################################################

# per lavorare anche con i random coefficients dobbiamo passare alla funzione 'mlogit' un vettore che 
# specifica quali sono le variabili che variano a livello di rispondente e la loro distribuzione.
# noi assumiamo che tutti i coefficienti seguano una distribuzione normale ('n')
# (https://cran.r-project.org/web/packages/mlogit/vignettes/c5.mxl.html)

m2.rpar <- rep("n", length = length(m2$coef))
names(m2.rpar) <- names(m2$coef)
m2.rpar # vettore con nomi e distribuzioni dei random coefficients

# ricalcoliamo m2 aggiungendo i random effects
m2.mixed <- mlogit(scelta ~ dolce + bottiglia + salame + torrone + cioccolatini + miele + grana + prezzo | -1, 
                  data        = cesti.mlogit, 
                  panel       = TRUE        , #panel dice che abbiamo pi? scelte dello stesso rispondente 
                  rpar        = m2.rpar     , #random parameters
                  correlation = FALSE)        #per ora assumiamo che non ci sia correlazione tra i random parameters

summary(m2.mixed) #oltre alla stima dei coefficienti otteniamo anche la loro standard deviation
# analizzando i random coefficients si capisce che esistono due distinte categorie di rispondenti per pandoro e panettone.
# anche per la variabile bottiglia c'? alta std dev. prezzo13 ? praticamente uguale a prezzo10

# possiamo visualizzare il livello di heterogenity per ogni random coefficient. i grafici ci permettono di capire
# la percentuale di rispondenti che non ? rappresentata dal coefficiente stimato (perch? ha un individual level coef del segno opposto)
par(mfrow = c(1 ,1))
plot(m2.mixed)

par(mfrow = c(2, 5)) # stampiamo i grafici in un unico panel (lo spazio per i plot deve essere grande per non avere errore di margini)
plot(rpar(m2.mixed, "dolcepanettone"   ))
plot(rpar(m2.mixed, "bottigliaspumante"))
plot(rpar(m2.mixed, "salameTRUE"       ))
plot(rpar(m2.mixed, "torroneTRUE"      ))
plot(rpar(m2.mixed, "cioccolatiniTRUE" ))
plot(rpar(m2.mixed, "mieleTRUE"        ))
plot(rpar(m2.mixed, "granaTRUE"        ))
plot(rpar(m2.mixed, "prezzo13"         ))
plot(rpar(m2.mixed, "prezzo16"         ))
plot(rpar(m2.mixed, "prezzo20"         ))
par(mfrow = c(1, 1))

# possiamo salvare e visualizzare la distribuzione di un singolo random coefficient. quella della variabile 'dolce' ? la piu interessante
dolcepanettone.distr <- rpar(m2.mixed, "dolcepanettone")
summary(dolcepanettone.distr)
mean(   dolcepanettone.distr)
med(    dolcepanettone.distr)
plot(   dolcepanettone.distr)


# se ricalcoliamo il modello con il prezzo numerico, poi possiamo avere la willingness to pay per i random coef
m3.rpar        <- rep("n", length = length(m3$coef))
names(m3.rpar) <- names(m3$coef)
m3.rpar # vettore con nomi e distribuzioni dei random coefficients

m3.mixed <- mlogit(scelta ~ dolce + bottiglia + salame + torrone + cioccolatini + miele + grana + as.numeric(as.character(prezzo)) | -1, 
                   data        = cesti.mlogit, 
                   panel       = TRUE        , #panel dice che abbiamo pi? scelte dello stesso rispondente 
                   rpar        = m3.rpar     , #random parameters
                   correlation = FALSE)        #per ora assumiamo che non ci sia correlazione tra i random parameters

summary(m3.mixed)
wtp.panettone <- rpar(m3.mixed, "dolcepanettone", norm = "as.numeric(as.character(prezzo))")
summary(wtp.panettone) # distribuzione della wtp per panettone
stdev(  wtp.panettone) # std deviation nella willingness to pay della variabile 'dolce'

# calcoliamo il modello che ammette correlazione tra i random coefficients. si pu? fare con un update del modello precedente
# questo modello oltre a media e std dev dei random coef calcola anche la covariance tra loro
m2.mixed2 <- update(m2.mixed, correlation = TRUE)
summary(m2.mixed2)

m3.mixed2     <- update(m3.mixed , correlation = TRUE) # lo facciamo anche per il modello con prezzo numerico (per la wtp)
wtp.panettone <- rpar(  m3.mixed2, "dolcepanettone", norm = "as.numeric(as.character(prezzo))")
summary(wtp.panettone) # distribuzione della wtp per panettone


listawtp2<-summary(rpar(m3.mixed2, "dolcepanettone", norm = "as.numeric(as.character(prezzo))")) #la gente vuole il pandoro
listawtp2<-rbind(listawtp2,summary(rpar(m3.mixed2, "bottigliaspumante", norm = "as.numeric(as.character(prezzo))")))
listawtp2<-rbind(listawtp2,summary(rpar(m3.mixed2, "salameTRUE"       , norm = "as.numeric(as.character(prezzo))")))
listawtp2<-rbind(listawtp2,summary(rpar(m3.mixed2, "cioccolatiniTRUE" , norm = "as.numeric(as.character(prezzo))")))
listawtp2<-rbind(listawtp2,summary(rpar(m3.mixed2, "torroneTRUE"      , norm = "as.numeric(as.character(prezzo))")))
listawtp2<-rbind(listawtp2,summary(rpar(m3.mixed2, "mieleTRUE"        , norm = "as.numeric(as.character(prezzo))")))
listawtp2<-rbind(listawtp2,summary(rpar(m3.mixed2, "granaTRUE"        , norm = "as.numeric(as.character(prezzo))")))
listawtp2 
# mostriamo la correlazione tra i random coefficients. 
# 'cov.mlogit' restituisce la matrice di covarianza, che 'cov2cor' trasforma in correlazione (covarianza normalizzata)
cor_matrix <- cov2cor(cov.mlogit(m2.mixed2))
round(cor_matrix, 4) 

# notiamo che alcuni coefficienti hanno una correlazione >0.5 in valore assoluto.
# per esempio il prezzo, come ci si pu? aspettare. 
# ma anche tra cioccolatini e torrone, grana e miele, e negativa tra cioccolatini e spumante

# mostriamo anche gli std errors delle correlazioni, e la loro significativit? 
summary(vcov(m2.mixed2, what = "rpar", type = "cor"))



# "We may restrict the correlation to only random parameters with significant association"
# qui non ? chiaro come scegliere i coefficienti da tenere. io ho scelto quelli con almeno una correlazione >0.5
m2.mixed3 <- update(m2.mixed2, correlation = c("bottigliaspumante", "cioccolatiniTRUE", "granaTRUE", 
                                               "torroneTRUE"      , "mieleTRUE"       , "prezzo13" , 
                                               "prezzo16"         , "prezzo20"))

summary(m2.mixed3)


# usiamo il likelihood ratio test per confrontare i nuovi modelli
lrtest(m2       , m2.mixed ) #Fixed effects vs. uncorrelated random effects
lrtest(m2       , m2.mixed2)
lrtest(m2.mixed , m2.mixed2) #Uncorrelated random effects vs. all correlated random effects
lrtest(m2.mixed3, m2.mixed2) #partially correlated random effects vs. all correlated random effects


#############################################################
############ Simulating shares with a mixed MNL model #######
#############################################################

# la funzione 'predict.mixed.mnl' calcola le quote di preferenza come 'predict.mnl'. 
# questa volta per? le risposte dei rispondenti vengono simulate con una distribuzione normale multivariata
# costruita con i coefficienti, le std dev e le correlazioni del mixed model
library(MASS)
predict.mixed.mnl <- function(model, data, nresp = 1000) {
  # Function for predicting shares from a mixed MNL model 
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to 
  #       predict shares. Same format at the data used to estimate model. 
  # Note that this code assumes all model parameters are random
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[ , -1]
  coef.Sigma <- cov.mlogit(model)
  coef.mu    <- model$coef[1:dim(coef.Sigma)[1]]
  draws      <- mvrnorm(n = nresp, coef.mu, coef.Sigma)
  shares     <- matrix(NA, nrow = nresp, ncol = nrow(data))
  for (i in 1:nresp) {
    utility <- data.model%*%draws[i, ]
    share = exp(utility)/sum(exp(utility))
    shares[i, ] <- share
  }
  cbind(colMeans(shares), data)
}

set.seed(1111)
predict.mixed.mnl(m2.mixed2, data = new.data) # calcoliamo le quote di preferenza sullo stesso set di design
# questa volta si pu? notare che le nicchie di mercato vengono premiate leggermente di pi?

# calcoliamo i tradeoff per per ogni livello degli attributi del nostro design Mixed MNL
base.data       <- new.data[ 1, ]
competitor.data <- new.data[-1, ]
tradeoff        <- sensitivity.mnl(m2, attributes, base.data, competitor.data)


# visualizziamo il Sensitivity Chart per il modello mixed MNL
barplot(tradeoff$increase, 
        horiz     = FALSE, 
        names.arg = c("pand"  , "panett", "pass"  , "spum"  , "salNO" ,
                      "salSI" , "torrNO", "torrSI", "ciocNO", "ciocSI",
                      "mielNO", "mielSI", "graNO" , "graSI" ,
                      "p10"   , "p13"   , "p16"   , "p20"),
        ylab      = "Change in Share for the Planned Product Design- Mixed MNL", 
        ylim      = c(-0.1, 0.11))
grid(nx = NA, ny = NULL)

# ricalcoliamo le quote, con il confidence interval
source("BootCI.predict.mixed.mnl.R")
BootCI.predict.mixed.mnl(m2.mixed2, data = new.data, nsim = 20) # questo comando ? molto lento perche esegue il bootstrap a livello di singolo rispondente per tenere conto dei random effect. 
                                                                #per testarlo meglio un nsim piu piccolo
#visto che il prodotto che racchiuderebbe le preference dei consumatori ? lo stesso 
#del modello fixed, lo usiamo per simulare il mercato  
#? il pacco che vorremmo mettere in vendita perch? racchiuderebbe le preferenze 
#dei consumatori con la massimizzazione del prezzo per il modello mixed MNL 

predict.mixed.mnl(m2.mixed2, data = new.data2)


##################################################
############# Consumer Heterogeneity #############
##################################################

# cerchiamo di capire se la consumer heterogeneity pu? essere spiegata da una caratteristica demografica (individual characteristics).
# lo facciamo analizzando la relazione tra individual part worth e individual-level variables.

#estraiamo gli individual part worth dal modello con 'fitted()'
PW.ind <- fitted(m2.mixed2, type = "parameters") #restituisce i parametri di ogni rispondenti

# aggiungiamo le individual-level variables con 'merge()' dopo averle estratte dal dataset originale associate a id_rispondente
destinatario.data <- unique(risposte_df[ , c("ID_rispondente", "destinatario")])
genere.data       <- unique(risposte_df[ , c("ID_rispondente", "genere"      )])
occupazione.data  <- unique(risposte_df[ , c("ID_rispondente", "occupazione" )])
classeEta.data    <- unique(risposte_df[ , c("ID_rispondente", "classeEta"   )])
preferenza.data   <- unique(risposte_df[ , c("ID_rispondente", "team"        )])

names(PW.ind)[1] <- "ID_rispondente"
PW.ind <- merge(PW.ind, destinatario.data, by = "ID_rispondente")
PW.ind <- merge(PW.ind, genere.data      , by = "ID_rispondente")
PW.ind <- merge(PW.ind, occupazione.data , by = "ID_rispondente")
PW.ind <- merge(PW.ind, classeEta.data   , by = "ID_rispondente")
PW.ind <- merge(PW.ind, preferenza.data  , by = "ID_rispondente")


PW.ind$classeEta = factor(PW.ind$classeEta, levels(PW.ind$classeEta)[c(1, 3, 4, 2)]) # le classi di eta in ordine crescente

colnames(PW.ind)[names(PW.ind) == "team"] <- "preferenza"
levels(  PW.ind$preferenza)[levels(PW.ind$preferenza) == "Team Pandoro"  ] <- "Pandoro"
levels(  PW.ind$preferenza)[levels(PW.ind$preferenza) == "Team Panettone"] <- "Panettone"

# possiamo concentrarci su una interazione alla volta tra un random effect e una variabile demografica
library(lattice)
library(ggplot2)


histogram(~ dolcepanettone | genere, data = PW.ind)
boxplot(    dolcepanettone ~ genere, data = PW.ind)
by(PW.ind$dolcepanettone, PW.ind$genere, mean)

# possiamo anche testare se la variabile demografica riesce a spiegare in modo significativo il random effect
t.test(dolcepanettone ~ genere, data = PW.ind) # per fare questo test occorre una variabile demografica a due livelli 


histogram(~ dolcepanettone | classeEta, data = PW.ind)
boxplot(    dolcepanettone ~ classeEta, data = PW.ind)
by(PW.ind$dolcepanettone, PW.ind$classeEta, mean)

histogram(~ prezzo20 | classeEta, data = PW.ind)
boxplot(    prezzo20 ~ classeEta, data = PW.ind)
by(PW.ind$prezzo20, PW.ind$classeEta, mean)

# proviamo alcune relazioni
by(PW.ind$prezzo20, PW.ind$classeEta   , mean)
by(PW.ind$prezzo20, PW.ind$destinatario, mean)
by(PW.ind$prezzo20, PW.ind$genere      , mean)
by(PW.ind$prezzo20, PW.ind$occupazione , mean)
by(PW.ind$prezzo20, PW.ind$preferenza  , mean)


ggplot(PW.ind, aes(x = classeEta  , y = prezzo20)) + geom_boxplot()
ggplot(PW.ind, aes(x = occupazione, y = prezzo20)) + geom_boxplot()

by(PW.ind$dolcepanettone, PW.ind$classeEta   , mean)
boxplot(dolcepanettone ~ classeEta, data = PW.ind)
by(PW.ind$dolcepanettone, PW.ind$destinatario, mean)
by(PW.ind$dolcepanettone, PW.ind$genere      , mean)
by(PW.ind$dolcepanettone, PW.ind$occupazione , mean)
by(PW.ind$dolcepanettone, PW.ind$preferenza  , mean)
boxplot(dolcepanettone ~ preferenza, data = PW.ind)

ggplot(PW.ind, aes(x = classeEta, y = dolcepanettone)) + geom_boxplot()
ggplot(PW.ind, aes(x = genere   , y = dolcepanettone)) + geom_boxplot()

by(PW.ind$granaTRUE, PW.ind$classeEta   , mean)
by(PW.ind$granaTRUE, PW.ind$destinatario, mean)
by(PW.ind$granaTRUE, PW.ind$genere      , mean)
by(PW.ind$granaTRUE, PW.ind$occupazione , mean)

by(PW.ind$cioccolatiniTRUE, PW.ind$classeEta   , mean)
by(PW.ind$cioccolatiniTRUE, PW.ind$destinatario, mean)
by(PW.ind$cioccolatiniTRUE, PW.ind$genere      , mean)
by(PW.ind$cioccolatiniTRUE, PW.ind$occupazione , mean)

by(PW.ind$bottigliaspumante, PW.ind$classeEta   , mean)
by(PW.ind$bottigliaspumante, PW.ind$destinatario, mean)
by(PW.ind$bottigliaspumante, PW.ind$genere      , mean)
by(PW.ind$bottigliaspumante, PW.ind$occupazione , mean)

by(PW.ind$prezzo13, PW.ind$classeEta   , mean)
by(PW.ind$prezzo13, PW.ind$destinatario, mean)
by(PW.ind$prezzo13, PW.ind$genere      , mean)
by(PW.ind$prezzo13, PW.ind$occupazione , mean)
