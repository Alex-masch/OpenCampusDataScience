###################################################
### Preparation of the Environment ####

# Clear environment
remove(list = ls())

# Create list with needed libraries
pkgs <- c("readr", "fastDummies", "jsonlite", "tidyverse", "psych", "lubridate", "DataExplorer")

# Load each listed library and check if it is installed and install if necessary
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


###################################################
### Data Import ####

# Reading the data file
umsatzdaten <- read_csv(file.path('Daten','umsatzdaten_gekuerzt.csv',fsep = .Platform$file.sep))
ordentlicheWoche <- c('Montag','Dienstag','Mittwoch','Donnerstag','Freitag','Samstag','Sonntag')
umsatzdaten$Wochentag <- factor(weekdays(umsatzdaten$Datum),
                                levels=ordentlicheWoche)
Warengruppen = c("Brot", "Broetchen", "Croissant","Konditorei","Kuchen","Saisonbrote")
umsatzdaten$Warengruppe <- factor(umsatzdaten$Warengruppe,
                                  levels = c(1,2,3,4,5,6),
                                  labels = Warengruppen)
kiwo <- read_csv(file.path('Daten','kiwo.csv',fsep = .Platform$file.sep))
wetter <- read_csv(file.path('Daten','wetter.csv',fsep = .Platform$file.sep))

url1 <- "https://feiertage-api.de/api/?jahr=2013&nur_land=SH"
url2 <- "https://feiertage-api.de/api/?jahr=2014&nur_land=SH"
url3 <- "https://feiertage-api.de/api/?jahr=2015&nur_land=SH"
url4 <- "https://feiertage-api.de/api/?jahr=2016&nur_land=SH"
url5 <- "https://feiertage-api.de/api/?jahr=2017&nur_land=SH"
url6 <- "https://feiertage-api.de/api/?jahr=2018&nur_land=SH"
url7 <- "https://feiertage-api.de/api/?jahr=2019&nur_land=SH"

# url lesen und data.frame erzeugen
feiertage13 <- fromJSON(txt=url1)
feiertage14 <- fromJSON(txt=url2)
feiertage15 <- fromJSON(txt=url3)
feiertage16 <- fromJSON(txt=url4)
feiertage17 <- fromJSON(txt=url5)
feiertage18 <- fromJSON(txt=url6)
feiertage19 <- fromJSON(txt=url7)
feiertage13_tbl <- as_data_frame(feiertage13) #data frame erzeugen
feiertage14_tbl <- as_data_frame(feiertage14) #data frame erzeugen
feiertage15_tbl <- as_data_frame(feiertage15) #data frame erzeugen
feiertage16_tbl <- as_data_frame(feiertage16) #data frame erzeugen
feiertage17_tbl <- as_data_frame(feiertage17) #data frame erzeugen
feiertage18_tbl <- as_data_frame(feiertage18) #data frame erzeugen
feiertage19_tbl <- as_data_frame(feiertage19) #data frame erzeugen

#führt die Datentabellen zusammen
feiertage13_14_tbl<-full_join(feiertage13_tbl, feiertage14_tbl) 
feiertage13_14_15_tbl<-full_join(feiertage13_14_tbl, feiertage15_tbl)
feiertage13_14_15_16_tbl<-full_join(feiertage13_14_15_tbl, feiertage16_tbl)
feiertage13_14_15_16_17_tbl<-full_join(feiertage13_14_15_16_tbl, 
                                       feiertage17_tbl)
feiertage13_14_15_16_17_18_tbl<-full_join(feiertage13_14_15_16_17_tbl, 
                                          feiertage18_tbl)
feiertage13_14_15_16_17_18_19_tbl<-full_join(feiertage13_14_15_16_17_18_tbl, 
                                             feiertage19_tbl)

# Datensatz umstrukturiert von "wide" mit mehreren Spalten auf "long", um die Daten konform zu unseren anderen Daten zu strukturieren. Umformung mit tidyverse-package. Zur Übergabe des Ergebnisses neue Variable feiertage_neu geschrieben
feiertage <- feiertage13_14_15_16_17_18_19_tbl %>%
  pivot_longer(everything(), names_to = "Feiertage", values_to = "Datum")
feiertage$Datum<- ymd(feiertage$Datum) #strings als Datum interpretieren
feiertage<- feiertage%>% 
  filter(!is.na(Datum)) #wo kein Datum produziert wurde oder eh missings auftreten Zeile löschen (es gab da komische leere Zellen beim Einlesen, die werden hier rausgeworfen)
feiertage$istFeiertag<-1 #dichotome Variable als Index: ist ein Feiertag? ---> setzen
write.csv(feiertage, file.path('Daten','feiertage.csv',fsep = .Platform$file.sep), row.names = TRUE) #sichere den Datensatz
rm(feiertage13_14_15_16_17_18_19_tbl,feiertage13_14_15_16_17_18_tbl,feiertage13_14_15_16_17_tbl,feiertage13_14_15_16_tbl,feiertage13_14_15_tbl,feiertage13_14_tbl,url1, url2, url3, url4, url5, url6, url7, feiertage13, feiertage14, feiertage15, feiertage16, feiertage17, feiertage18, feiertage19, feiertage13_tbl, feiertage14_tbl, feiertage15_tbl, feiertage16_tbl, feiertage17_tbl, feiertage18_tbl, feiertage19_tbl) #aufräumen

dataset<-left_join(umsatzdaten,kiwo, by="Datum")
dataset<-left_join(dataset , wetter, by="Datum")
dataset<-left_join(dataset , feiertage, by="Datum")

### Data Cleaning ###
dataset$KielerWoche[is.na(dataset$KielerWoche)] <- 0
dataset$Feiertage[is.na(dataset$Feiertage)] <- 0
dataset$istFeiertag[is.na(dataset$istFeiertag)] <- 0
dataset$Bewoelkung[is.na(dataset$Bewoelkung)] <- mean(dataset$Bewoelkung, na.rm=TRUE)
dataset$Temperatur[is.na(dataset$Temperatur)] <- mean(dataset$Temperatur, na.rm=TRUE)
dataset$Windgeschwindigkeit[is.na(dataset$Windgeschwindigkeit)] <- mean(dataset$Windgeschwindigkeit, na.rm=TRUE)
dataset<-subset(dataset, select = -Wettercode)
dataset$Wochentag<- as.numeric(as.integer(dataset$Wochentag))
dataset$Warengruppe<-as.numeric(as.integer(dataset$Warengruppe))

#Ausreißer deckeln auf 3x Standardabweichung (Umsatz und Windgeschwindigkeit sind betroffen.)
#Bei 2x Standardabweichung werden 5% der Daten beeinflusst, das halte ich für zu viel. NUN 30.05.2021
#Standardabweichung berechnen (sd(x))
sd_Umsatz<-sd(dataset$Umsatz)
sd_Bewoelkung<-sd(dataset$Bewoelkung)
sd_Temperatur<-sd(dataset$Temperatur)
sd_Windgeschwindigkeit<-sd(dataset$Windgeschwindigkeit)

#Gefundene Werte auf 3x Standardabweichung setzen
dataset$Windgeschwindigkeit[dataset$Windgeschwindigkeit > (mean(dataset$Windgeschwindigkeit) + sd_Windgeschwindigkeit*3)]<- (mean(dataset$Windgeschwindigkeit) + sd_Windgeschwindigkeit*3)
dataset$Windgeschwindigkeit[dataset$Windgeschwindigkeit < (mean(dataset$Windgeschwindigkeit) - sd_Windgeschwindigkeit*3)]<- (mean(dataset$Windgeschwindigkeit) - sd_Windgeschwindigkeit*3)
dataset$Umsatz[dataset$Umsatz > (mean(dataset$Umsatz) + sd_Umsatz*3)]<- (mean(dataset$Umsatz) + sd_Umsatz*3)
dataset$Umsatz[dataset$Umsatz < (mean(dataset$Umsatz) - sd_Umsatz*3)]<- (mean(dataset$Umsatz) - sd_Umsatz*3)

###################################################
### Data Preparation ####

# Recoding of the variables into one-hot encoded (dummy) variables
dummy_list <- c("Warengruppe", "KielerWoche")
dataset_dummy= dummy_cols(dataset, dummy_list)

# Definition of lists for each one-hot encoded variable (just to make the handling easier)
warengruppe_dummies = c('Warengruppe_1', 'Warengruppe_2', 'Warengruppe_3', 'Warengruppe_4', 'Warengruppe_5', 'Warengruppe_6')
kielerWoche_dummies = c('KielerWoche_0', 'KielerWoche_1')


###################################################
### Selection of the Feature Variables and the Label Variable ####

# Selection of the features (the independent variables used to predict the dependent)
features <- c('Wochentag', 'Bewoelkung', 'Temperatur', 'Windgeschwindigkeit', warengruppe_dummies, kielerWoche_dummies)
# Selection of the label (the dependent variable)
labels <- 'Umsatz'


###################################################
### Selection of Training, Validation and Test Data ####

# Look at the data
str(dataset_dummy)

# Setting the random counter to a fixed value, so the random initialization stays the same (the random split is always the same)
set.seed(1)

# Shuffling the dataset (to get random orders within each dataset as well)
new_row_order <- sample(nrow(dataset_dummy))
house_pricing_dummy <- dataset_dummy[new_row_order, ]

# Assign each row number in the full dataset randomly to one of the three groups of datasets
# The probability of being in one of the groups results then in crresponding group sizes
assignment <- sample(1:3, size = nrow(dataset_dummy), prob = c(.7, .2, .1), replace = TRUE)

# Create training, validation and test data for the features and the labels
training_features <- dataset_dummy[assignment == 1, features]    # subset house_pricing to training indices only
training_labels <- dataset_dummy[assignment == 1, labels]    # subset house_pricing to training indices only

validation_features <- dataset_dummy[assignment == 2, features]  # subset house_pricing to validation indices only
validation_labels <- dataset_dummy[assignment == 2, labels]  # subset house_pricing to validation indices only

test_features <- dataset_dummy[assignment == 3, features]   # subset house_pricing to test indices only
test_labels <- dataset_dummy[assignment == 3, labels]   # subset house_pricing to test indices only