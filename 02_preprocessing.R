###################################
#### CESTI DI NATALE 2019-2020 ####
#### Preprocessing             ####
####  - Ambrosi  Andrea        ####
####  - Balliu   Bertiana      ####
####  - Marchesi Raffaele      ####
####  - Parolin  Irene         ####
###################################


# Leggiamo i 2 dataset dai rispettivi file csv
risposte1 <- read.csv("risposte_blocco1.csv", sep = ",")
risposte2 <- read.csv("risposte_blocco2.csv", sep = ",")

# Teniamo traccia del questionario di provenienza aggiungendo la colonna "blocco"
risposte1$blocco <- as.factor("1")
risposte2$blocco <- as.factor("2")

# Ora possiamo fare il merge dei 2 dataset
risposte <- rbind(risposte1, risposte2)

# Rinominiamo le colonne per semplicita'
colnames(risposte) <- c("dateTime", "destinatario", "domanda1", "domanda2"    , 
                        "domanda3", "domanda4"    , "domanda5", "domanda6"    , 
                        "domanda7", "domanda8"    , "anni"    , "occupazione" ,
                        "genere"  , "provincia"   , "team"    , "blocco")

############################
####   DATA CLEANING    ####
############################

# Sistemiamo il genere: abbiamo piu' livelli in quanto ci siamo accorti (dopo aver distribuito
# il questionario) di aver erroneamente chiesto di scegliere il "sesso" specificando anche l'opzione
# "altro" quando invece volevamo sapere il "genere". Per questo in corso d'opera abbiamo modificato
# in modo da chiedere il "genere" lasciando come terza opzione una non-risposta.
levels(risposte$genere)[levels(risposte$genere) == "Uomo"                     ] <- "M"
levels(risposte$genere)[levels(risposte$genere) == "Donna"                    ] <- "F"
levels(risposte$genere)[levels(risposte$genere) == "Altro"                    ] <- "NoRisposta"
levels(risposte$genere)[levels(risposte$genere) == "Preferisco non rispondere"] <- "NoRisposta"


# Sistemiamo ora le provincie mettendole nel formato corretto e correggendo eventuali errori
# dettati principalmente da spazi di troppo e formato non rispettato
levels(risposte$provincia)[levels(risposte$provincia) == "Bari (non troppo anonimizzata :*)"]<- "BA"
levels(risposte$provincia)[levels(risposte$provincia) == "Bari"       ] <- "BA"
levels(risposte$provincia)[levels(risposte$provincia) == "Bo"         ] <- "BO"
levels(risposte$provincia)[levels(risposte$provincia) == "Bologna"    ] <- "BO"
levels(risposte$provincia)[levels(risposte$provincia) == "Br"         ] <- "BR"
levels(risposte$provincia)[levels(risposte$provincia) == "bz"         ] <- "BZ"
levels(risposte$provincia)[levels(risposte$provincia) == "Bolzano"    ] <- "BZ"
levels(risposte$provincia)[levels(risposte$provincia) == "Bz"         ] <- "BZ"
levels(risposte$provincia)[levels(risposte$provincia) == "BZ "        ] <- "BZ"
levels(risposte$provincia)[levels(risposte$provincia) == "Como"       ] <- "CO"
levels(risposte$provincia)[levels(risposte$provincia) == "Cremona"    ] <- "CR"
levels(risposte$provincia)[levels(risposte$provincia) == "Cs"         ] <- "CS"
levels(risposte$provincia)[levels(risposte$provincia) == "Cz"         ] <- "CZ"
levels(risposte$provincia)[levels(risposte$provincia) == "Fi"         ] <- "FI"
levels(risposte$provincia)[levels(risposte$provincia) == "fi"         ] <- "FI"
levels(risposte$provincia)[levels(risposte$provincia) == "Ge"         ] <- "GE"
levels(risposte$provincia)[levels(risposte$provincia) == "GE "        ] <- "GE"
levels(risposte$provincia)[levels(risposte$provincia) == "Lecce"      ] <- "LE"
levels(risposte$provincia)[levels(risposte$provincia) == "Mb"         ] <- "MB"
levels(risposte$provincia)[levels(risposte$provincia) == "Mi"         ] <- "MI"
levels(risposte$provincia)[levels(risposte$provincia) == "Mi "        ] <- "MI"
levels(risposte$provincia)[levels(risposte$provincia) == "Milano"     ] <- "MI"
levels(risposte$provincia)[levels(risposte$provincia) == "Mantova"    ] <- "MN"
levels(risposte$provincia)[levels(risposte$provincia) == "MN "        ] <- "MN"
levels(risposte$provincia)[levels(risposte$provincia) == "Mo"         ] <- "MO"
levels(risposte$provincia)[levels(risposte$provincia) == "Na"         ] <- "NA"
levels(risposte$provincia)[levels(risposte$provincia) == "No"         ] <- "NO"
levels(risposte$provincia)[levels(risposte$provincia) == "Palermo"    ] <- "PA"
levels(risposte$provincia)[levels(risposte$provincia) == "Padova"     ] <- "PD"
levels(risposte$provincia)[levels(risposte$provincia) == "Padova "    ] <- "PD"
levels(risposte$provincia)[levels(risposte$provincia) == "Pd"         ] <- "PD"
levels(risposte$provincia)[levels(risposte$provincia) == "Pe"         ] <- "PE"
levels(risposte$provincia)[levels(risposte$provincia) == "Pg"         ] <- "PG"
levels(risposte$provincia)[levels(risposte$provincia) == "Pz"         ] <- "PZ"
levels(risposte$provincia)[levels(risposte$provincia) == "Rm"         ] <- "RM"
levels(risposte$provincia)[levels(risposte$provincia) == "Roma"       ] <- "RM"
levels(risposte$provincia)[levels(risposte$provincia) == "Sa"         ] <- "SA"
levels(risposte$provincia)[levels(risposte$provincia) == "So"         ] <- "SO"
levels(risposte$provincia)[levels(risposte$provincia) == "Sassari"    ] <- "SS"
levels(risposte$provincia)[levels(risposte$provincia) == "Tn "        ] <- "TN"
levels(risposte$provincia)[levels(risposte$provincia) == "tn"         ] <- "TN"
levels(risposte$provincia)[levels(risposte$provincia) == "Tn"         ] <- "TN"
levels(risposte$provincia)[levels(risposte$provincia) == "TN "        ] <- "TN"
levels(risposte$provincia)[levels(risposte$provincia) == "Trento"     ] <- "TN"
levels(risposte$provincia)[levels(risposte$provincia) == "Trento "    ] <- "TN"
levels(risposte$provincia)[levels(risposte$provincia) == "Tr"         ] <- "TR"
levels(risposte$provincia)[levels(risposte$provincia) == "Treviso"    ] <- "TV"
levels(risposte$provincia)[levels(risposte$provincia) == "Treviso "   ] <- "TV"
levels(risposte$provincia)[levels(risposte$provincia) == "tv"         ] <- "TV"
levels(risposte$provincia)[levels(risposte$provincia) == "Tv"         ] <- "TV"
levels(risposte$provincia)[levels(risposte$provincia) == "TV "        ] <- "TV"
levels(risposte$provincia)[levels(risposte$provincia) == "To"         ] <- "TO"
levels(risposte$provincia)[levels(risposte$provincia) == "Ts"         ] <- "TS"
levels(risposte$provincia)[levels(risposte$provincia) == "Va"         ] <- "VA"
levels(risposte$provincia)[levels(risposte$provincia) == "Ve"         ] <- "VE"
levels(risposte$provincia)[levels(risposte$provincia) == "Ca' D'ORO"  ] <- "VE"
levels(risposte$provincia)[levels(risposte$provincia) == "Vi"         ] <- "VI"
levels(risposte$provincia)[levels(risposte$provincia) == "Vi "        ] <- "VI"
levels(risposte$provincia)[levels(risposte$provincia) == "vi"         ] <- "VI"
levels(risposte$provincia)[levels(risposte$provincia) == "Vicenza "   ] <- "VI"
levels(risposte$provincia)[levels(risposte$provincia) == "vicenza"    ] <- "VI"
levels(risposte$provincia)[levels(risposte$provincia) == "Vicenza"    ] <- "VI"
levels(risposte$provincia)[levels(risposte$provincia) == "VICENZA"    ] <- "VI"
levels(risposte$provincia)[levels(risposte$provincia) == "Mussolente "] <- "VI"
levels(risposte$provincia)[levels(risposte$provincia) == "Veroa"      ] <- "VR"
levels(risposte$provincia)[levels(risposte$provincia) == "Verona"     ] <- "VR"
levels(risposte$provincia)[levels(risposte$provincia) == " VR"        ] <- "VR"
levels(risposte$provincia)[levels(risposte$provincia) == "Vr"         ] <- "VR"
levels(risposte$provincia)[levels(risposte$provincia) == "VR "        ] <- "VR"
levels(risposte$provincia)[levels(risposte$provincia) == "Ud"         ] <- "UD"
levels(risposte$provincia)[levels(risposte$provincia) == ""           ] <- "NoRisposta"

# Tutti gli esteri vanno nella categoria ESTERO per semplicita'
levels(risposte$provincia)[levels(risposte$provincia) == "Cj (Cluj- Romania)"] <- "ESTERO"
levels(risposte$provincia)[levels(risposte$provincia) == "Dublin"            ] <- "ESTERO"
levels(risposte$provincia)[levels(risposte$provincia) == "Dublino, Irlanda"  ] <- "ESTERO"
levels(risposte$provincia)[levels(risposte$provincia) == "Estero"            ] <- "ESTERO"
levels(risposte$provincia)[levels(risposte$provincia) == "Estero "           ] <- "ESTERO"
levels(risposte$provincia)[levels(risposte$provincia) == "Irlanda"           ] <- "ESTERO"
levels(risposte$provincia)[levels(risposte$provincia) == "Zurigo, Svizzera " ] <- "ESTERO"
levels(risposte$provincia)[levels(risposte$provincia) == "TR"                ] <- "ESTERO"
#Siamo sicuri che nessuno sia di Terni; TR e' stato indicato da abitanti di Tirana


# Sistemiamo l'eta'
levels(risposte$anni)[levels(risposte$anni) == "24 "    ] <- "24"
levels(risposte$anni)[levels(risposte$anni) == "125"    ] <- "25"
levels(risposte$anni)[levels(risposte$anni) == "49 "    ] <- "49"
levels(risposte$anni)[levels(risposte$anni) == "54 "    ] <- "54"
levels(risposte$anni)[levels(risposte$anni) == "58 anni"] <- "58"
levels(risposte$anni)[levels(risposte$anni) == "5 9"    ] <- "59"
levels(risposte$anni)[levels(risposte$anni) == "73 "    ] <- "73"
risposte$anni <- as.numeric(as.character(risposte$anni))


# Sistemiamo l'occupazione: abbiamo piu' livelli in quanto ci siamo accorti (dopo aver distribuito
# il questionario) che non rispettava la parita' di genere. In un questionario avevamo scritto per esteso
# mentre nell'altro in forma concisa quindi riuniamo i 2 livelli superflui.
levels(risposte$occupazione)[levels(risposte$occupazione) == "Disoccupato/Disoccupata"] <- "Disoccupato"
levels(risposte$occupazione)[levels(risposte$occupazione) == "Disoccupato/a"          ] <- "Disoccupato"
levels(risposte$occupazione)[levels(risposte$occupazione) == "Lavoratore/Lavoratrice" ] <- "Lavoratore"
levels(risposte$occupazione)[levels(risposte$occupazione) == "Pensionato/a"           ] <- "Pensionato"
levels(risposte$occupazione)[levels(risposte$occupazione) == "Pensionato/Pensionata"  ] <- "Pensionato"
levels(risposte$occupazione)[levels(risposte$occupazione) == "Studente/Studentessa"   ] <- "Studente"


# Eliminiamo i rispondenti che non hanno voluto specificare il genere perche' sono in numero esiguo e difficili da trattare
risposte <- risposte[risposte$genere != "NoRisposta", ]

summary(risposte$blocco)


############################
# DATASET IN FORMATO LONG  #
############################

matriceDomande <- read.csv("matriceDomande.csv")

# Dimensione nuovo dataframe
nrisposte <- 237

# Dataframe per blocco 1
data1 <- data.frame(matrix(ncol = 19, 
                           nrow = nrisposte * 24)) #Per ogni rispondente avremo 24 righe nel dataframe, 8 domande * 3 alternative
names(data1) <- c("ID_rispondente", "blocco"   , "ID_domanda" , "alternativa", 
                  "dolce"         , "bottiglia", "salame"     , "torrone"    , 
                  "cioccolatini"  , "miele"    , "grana"      , "prezzo"     , 
                  "destinatario"  , "anni"     , "occupazione", "genere"     ,
                  "provincia"     , "team"     , "scelta")

data1$alternativa <- 1:3
data1$ID_domanda  <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 
                       5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8)
data1$blocco      <- 1

data1$dolce        <- matriceDomande[c(1:24), ]$dolce
data1$bottiglia    <- matriceDomande[c(1:24), ]$bottiglia
data1$salame       <- matriceDomande[c(1:24), ]$salame
data1$torrone      <- matriceDomande[c(1:24), ]$torrone
data1$cioccolatini <- matriceDomande[c(1:24), ]$cioccolatini
data1$miele        <- matriceDomande[c(1:24), ]$miele
data1$grana        <- matriceDomande[c(1:24), ]$grana
data1$prezzo       <- matriceDomande[c(1:24), ]$prezzo

# Dataframe per blocco 2
data2 <- data.frame(matrix(ncol = 19, 
                           nrow = nrisposte * 24))
names(data2) <- c("ID_rispondente", "blocco"   , "ID_domanda" , "alternativa", 
                  "dolce"         , "bottiglia", "salame"     , "torrone"    , 
                  "cioccolatini"  , "miele"    , "grana"      , "prezzo"     , 
                  "destinatario"  , "anni"     , "occupazione", "genere"     , 
                  "provincia"     , "team"     , "scelta")

data2$alternativa <- 1:3
data2$ID_domanda  <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 
                       5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8)
data2$blocco      <- 2

data2$dolce        <- matriceDomande[c(25:48), ]$dolce
data2$bottiglia    <- matriceDomande[c(25:48), ]$bottiglia
data2$salame       <- matriceDomande[c(25:48), ]$salame
data2$torrone      <- matriceDomande[c(25:48), ]$torrone
data2$cioccolatini <- matriceDomande[c(25:48), ]$cioccolatini
data2$miele        <- matriceDomande[c(25:48), ]$miele
data2$grana        <- matriceDomande[c(25:48), ]$grana
data2$prezzo       <- matriceDomande[c(25:48), ]$prezzo

# Merge  
risposte_df <- rbind(data1, data2)

# Cleaning
risposte_df[] <- lapply(risposte_df, as.character) #tratto tutte le variabili come stringhe

risposte_df[risposte_df == "Panettone, "  ] <- "panettone"
risposte_df[risposte_df == "Pandoro,"     ] <- "pandoro"
risposte_df[risposte_df == "Passito, "    ] <- "passito"
risposte_df[risposte_df == "Spumante,"    ] <- "spumante"
risposte_df[risposte_df == "Salame,"      ] <- 1
risposte_df[risposte_df == "Torrone,"     ] <- 1
risposte_df[risposte_df == "Cioccolatini,"] <- 1
risposte_df[risposte_df == "Miele,"       ] <- 1
risposte_df[risposte_df == "Grana"        ] <- 1
risposte_df[risposte_df == ""             ] <- 0
risposte_df[risposte_df == "- 10 euro"    ] <- 10
risposte_df[risposte_df == "- 13 euro"    ] <- 13
risposte_df[risposte_df == "- 16 euro"    ] <- 16
risposte_df[risposte_df == "- 20 euro"    ] <- 20

# Aggiungiamo un ID univoco per ogni risposta del dataframe
risposte$ID <- seq.int(nrow(risposte))

# Riempiamo i questionari con le risposte ottenute
tempoinizio<- Sys.time()
for (r in 1:nrow(risposte_df)){
  risposte_df[r, ]$ID_rispondente <- ( (r-1) %/% 24) + 1 #ripete ogni id 24 volte (8 domande * 3 alternative)
  risposte_df[r, ]$destinatario   <- as.character(risposte[risposte$ID == risposte_df[r, ]$ID_rispondente, ]$destinatario)
  risposte_df[r, ]$anni           <- as.character(risposte[risposte$ID == risposte_df[r, ]$ID_rispondente, ]$anni)
  risposte_df[r, ]$genere         <- as.character(risposte[risposte$ID == risposte_df[r, ]$ID_rispondente, ]$genere)
  risposte_df[r, ]$occupazione    <- as.character(risposte[risposte$ID == risposte_df[r, ]$ID_rispondente, ]$occupazione)
  risposte_df[r, ]$provincia      <- as.character(risposte[risposte$ID == risposte_df[r, ]$ID_rispondente, ]$provincia)
  risposte_df[r, ]$team           <- as.character(risposte[risposte$ID == risposte_df[r, ]$ID_rispondente, ]$team)
}

tempofine<-Sys.time()
tempo<- tempofine - tempoinizio
tempo

# Cambio contenuto scelte da stringhe a numeri corrispondenti (inseriti a mano): 
# invece di avere la scelta per esteso usiamo 3 interi per indicare l'alternativa
levels(risposte$domanda1) <- c("2", "1", "3", "3", "1", "2")
levels(risposte$domanda2) <- c("3", "2", "1", "1", "3", "2")
levels(risposte$domanda3) <- c("1", "3", "2", "2", "1", "3")
levels(risposte$domanda4) <- c("2", "3", "1", "2", "1", "3")
levels(risposte$domanda5) <- c("3", "2", "1", "3", "2", "1")
levels(risposte$domanda6) <- c("1", "3", "2", "2", "3", "1")
levels(risposte$domanda7) <- c("3", "2", "1", "1", "2", "3")
levels(risposte$domanda8) <- c("1", "3", "2", "3", "1", "2")

# Vengono resettate le scelte
risposte_df$scelta <- 0
# Verra' poi aggiunto un 1 quando si incontra l'alternativa scelta
for (r in 1:nrow(risposte)){
  risposte_df[risposte_df$ID_rispondente == risposte[r, ]$ID & 
              risposte_df$ID_domanda     == 1                & 
              risposte_df$alternativa    == risposte[r, ]$domanda1, "scelta"] <- 1
  risposte_df[risposte_df$ID_rispondente == risposte[r, ]$ID & 
              risposte_df$ID_domanda     == 2                & 
              risposte_df$alternativa    == risposte[r, ]$domanda2, "scelta"] <- 1
  risposte_df[risposte_df$ID_rispondente == risposte[r, ]$ID & 
              risposte_df$ID_domanda     == 3                & 
              risposte_df$alternativa    == risposte[r, ]$domanda3, "scelta"] <- 1
  risposte_df[risposte_df$ID_rispondente == risposte[r, ]$ID & 
              risposte_df$ID_domanda     == 4                & 
              risposte_df$alternativa    == risposte[r, ]$domanda4, "scelta"] <- 1
  risposte_df[risposte_df$ID_rispondente == risposte[r, ]$ID & 
              risposte_df$ID_domanda     == 5                & 
              risposte_df$alternativa    == risposte[r, ]$domanda5, "scelta"] <- 1
  risposte_df[risposte_df$ID_rispondente == risposte[r, ]$ID & 
              risposte_df$ID_domanda     == 6                & 
              risposte_df$alternativa    == risposte[r, ]$domanda6, "scelta"] <- 1
  risposte_df[risposte_df$ID_rispondente == risposte[r, ]$ID & 
              risposte_df$ID_domanda     == 7                & 
              risposte_df$alternativa    == risposte[r, ]$domanda7, "scelta"] <- 1
  risposte_df[risposte_df$ID_rispondente == risposte[r, ]$ID & 
              risposte_df$ID_domanda     == 8                & 
              risposte_df$alternativa    == risposte[r, ]$domanda8, "scelta"] <- 1
}

# Correzione tipo del dato
risposte_df$ID_rispondente <- as.integer  (risposte_df$ID_rispondente)
risposte_df$blocco         <- as.factor   (risposte_df$blocco        )
risposte_df$ID_domanda     <- as.integer  (risposte_df$ID_domanda    )
risposte_df$alternativa    <- as.factor   (risposte_df$alternativa   )
risposte_df$dolce          <- as.factor   (risposte_df$dolce         )
risposte_df$bottiglia      <- as.factor   (risposte_df$bottiglia     )
risposte_df$salame         <- as.factor   (risposte_df$salame        )
risposte_df$torrone        <- as.factor   (risposte_df$torrone       )
risposte_df$cioccolatini   <- as.factor   (risposte_df$cioccolatini  ) 
risposte_df$miele          <- as.factor   (risposte_df$miele         )
risposte_df$grana          <- as.factor   (risposte_df$grana         )
risposte_df$prezzo         <- as.integer  (risposte_df$prezzo        )
risposte_df$destinatario   <- as.character(risposte_df$destinatario  )
risposte_df$anni           <- as.integer  (risposte_df$anni          )
risposte_df$occupazione    <- as.factor   (risposte_df$occupazione   )
risposte_df$genere         <- as.factor   (risposte_df$genere        )
risposte_df$provincia      <- as.factor   (risposte_df$provincia     )
risposte_df$team           <- as.factor   (risposte_df$team          )
risposte_df$scelta         <- as.factor   (risposte_df$scelta        )



summary(risposte_df)

#decommentare questa riga per salvare un nuovo file delle risposte
#write.csv(risposte_df, "risposte_df.csv")


#############################################
############# Descriptive Analysis ##########
#############################################

library(ggplot2)

# Distribuzione dei rispondenti in base alle classi d'eta'
ggplot(risposte, aes(x    = risposte$classe, 
                     fill = risposte$classe)) + 
  labs(title  = "Respondents distribution by age",
       x      = NULL,
       y      = "Frequency") + 
  guides(fill = FALSE) + 
  geom_bar() +
  theme(panel.grid.minor = element_blank())

# Distribuzione dei riceventi per classe d'eta
ggplot(risposte, aes(x    = risposte$classe, 
                     fill = risposte$destinatario)) + 
  labs(title = "Receiver distribution by age",
       x     = NULL,
       y     = "Respondents",
       fill  = "Receiver") + 
  geom_bar() +
  theme(panel.grid.minor = element_blank())

# Distribuzione percentuale pandoro vs. panettone per eta'
ggplot(risposte, aes(x    = risposte$classe, 
                     fill = risposte$team)) + 
  labs(title = "Team preference by age",
       x     = NULL,
       y     = NULL,
       fill  = "Team") + 
  geom_bar(position = "fill") +
  theme(panel.grid.minor    = element_blank())+
  scale_y_continuous(labels = scales::percent_format())

# Distribuzione percentuale pandoro vs. panettone per genere
ggplot(risposte[risposte$genere != "NoRisposta",], 
       aes(x    = risposte$genere[risposte$genere != "NoRisposta"],
           fill = risposte$team[  risposte$genere != "NoRisposta"])) + 
  labs(title = "Team preference by gender",
       x     = NULL,
       y     = NULL,
       fill  = "Team") + 
  geom_bar(position = "fill") +
  theme(panel.grid.minor    = element_blank())+
  scale_y_continuous(labels = scales::percent_format())

library(dplyr)

# Gender comparison between classes
prova <- count(x = risposte[risposte$genere != "NoRisposta",], team, classe, genere, name = "count")
ggplot(prova, 
       aes(x     = prova$classe, 
           y     = prova$count,
           fill  = prova$team)) + 
  labs(title = "Preference",
       x     = NULL,
       y     = "Frequency",
       fill  = "Team") + 
  geom_col(position = "dodge2")+
  geom_text(aes(label = prova$genere), position = position_dodge(width = 0.9, preserve = "total"))

# Per generare le tabelle da importare in qgis per la stampa della mappa cloropletica
pandoro   <- count(x = risposte[  risposte$provincia != "NoRisposta"      &  
                                  risposte$provincia != "ESTERO"          &
                                  risposte$team      == "Team Pandoro",   ], 
                   provincia, 
                   name = "count")

panettone <- count(x = risposte[  risposte$provincia != "NoRisposta"      & 
                                  risposte$provincia != "ESTERO"          &
                                  risposte$team      == "Team Panettone", ], 
                   provincia, 
                   name = "count")

# Nella colonna count poi ci sara'
# >0 se la preferenza per provincia e' pandoro
# <0 se la preferenza e' panettone
for (i in 1:nrow(panettone)){
  c <- pandoro$count[pandoro$provincia == panettone[i, ]$provincia] - panettone[i, ]$count
  pandoro$count[     pandoro$provincia == panettone[i, ]$provincia] <- c
}
# Per salvare decommentare
#write.csv(pandoro, file = "./pandoro.csv")
