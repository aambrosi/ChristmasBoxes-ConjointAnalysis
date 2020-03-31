####################################################################
#### CESTI DI NATALE 2019-2020                                  ####
#### Costruzione del questionario - Fractional Factorial design ####
####  - Ambrosi  Andrea                                         ####
####  - Balliu   Bertiana                                       ####
####  - Marchesi Raffaele                                       ####
####  - Parolin  Irene                                          ####
####################################################################

library(support.CEs) 

# definiamo la lista di attributi
# aggiungiamo gi? dei segni di punteggiatura che ci aiuteranno a scrivere le domande nel google form
attrib <- list(pan          = c("Pandoro,"     , "Panettone, "),
               bottiglia    = c("Spumante,"    , "Passito, "  ),
               salame       = c("Salame,"      , ""),
               torrone      = c("Torrone,"     , ""),
               cioccolatini = c("Cioccolatini,", ""),
               miele        = c("Miele,"       , ""),
               grana        = c("Grana"        , ""),
               prezzo       = c("- 10 euro"    , "- 13 euro", 
                                "- 16 euro"    , "- 20 euro"))

# Mix and match design method
RotDesSurvey <- rotation.design(attribute.names = attrib, # quali attributi usiamo
                                nalternatives   = 3,      # numero di alternative per domanda
                                nblocks         = 2,      # 2 blocchi per ridurre il carico cognitivo per i rispondenti
                                randomize       = TRUE,   #'FALSE' = rotation design - 'TRUE' = mix-and-match design
                                seed            = 999) 
RotDesSurvey 

questionnaire(RotDesSurvey) #domande in formato 'questionario'

# con 'sink' salviamo il questionario in un file txt
# decommentare le righe seguenti per salvare un nuovo file
# sink("questions_for_survey_rotation.txt")  
# questionnaire(RotDesSurvey)
# sink() 

# questionario in formato dataframe
RotDesSurveyDF <- NULL
nalt <- RotDesSurvey$design.information$nalternatives
for(i in seq(1 : nalt)) {
  RotDesSurveyDF <- rbind(RotDesSurveyDF, RotDesSurvey$alternatives[[i]])
}
RotDesSurveyDF <- RotDesSurveyDF[order(RotDesSurveyDF$BLOCK, RotDesSurveyDF$QES), ] 

# decommentare le righe seguenti per salvare il questionario in formato wide (utile per trascrivere le domande)
# sink("questionario_wide.txt")  
# RotDesSurveyDF
# sink() 


# decommentare la riga seguente per salvare la matrice delle domande (serve per mettere le risposte in formato long)
# write.csv(RotDesSurveyDF, "matriceDomande.csv")


# controllo di ortogonalit? (si pu? fare con le varie combinazioni di attributi)
cor(apply(RotDesSurveyDF[,-1:-3], 2, rank)) 
table(RotDesSurveyDF$pan,       RotDesSurveyDF$prezzo )
table(RotDesSurveyDF$bottiglia, RotDesSurveyDF$torrone)
