### Preparation of the environment

# clear entvironment
remove(list = ls())

#Lade benöätige library über Liste und for-Schleife
pkgs <- c('readr', 'fastDummies')

for(pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


### Data import

# Lese CSV Datei ein
data <- read_csv("Daten/umsatzdaten_kiwo_wetter_feiertage.csv")


### Data preparation

# entcoding der variablen in dummy variablen
dummy_list <- c("Warengruppe","Wochentag")
data_dummy = dummy_cols(data, dummy_list)

#Definition der Listen fuer dummy entcodedete varivablen (damit einfacher zu handeln)
condition_dummies = c('condition_1', 'condition_2', 'condition_3', 'condition_4', 'condition_5')
view_dummies = c('view_0', 'view_1', 'view_2', 'view_3','view_4')


### Selection of the Feature Variables and the Label Variable

#Auswahl der features (die unabhängigen Var wird zur Vorhersage der abhänigen genutzt)

features <-  c("Warengruppe", "Wochentag", "KielerWoche", "Bewoelkung", "Temperatur", "Windgeschwindigkeit", condition_dummies, view_dummies)    # unabhängige Variablen zur Vorhersage
labels <- "Umsatz"                                                                                                                                # zu vorhersgende Variable


### Selecion of Training, Validation and Test Data

#gebe Datendummy aus

str(data_dummy)

# setze Zufallsgenerator auf festen Wert, damit random split is always the same
set.seed(1)

# Dataset durcheinander würfeln um eine Random-Reihnfolge zu erhalten
new_row_order <- sample(nrow(data))
data_dummy <- data_dummy[new_row_order, ]

# erstelle Hilfs-Zufalls-Variable assignment (Besitzt Wert 1, 2 oder 3)
assignment <- sample(1:3, size = nrow(data_dummy), prob = c(.7, .2, .1), replace = TRUE)

# erstelle training-, validation- & test data fuer features und labels
training_features <- data_dummy[assignment == 1, features]  #subset data to training indices only
training_labels <- data_dummy[assignmen == 1, labels]       #subset data to training indices only

validation_features <- data_dummy[assignment == 2, features]  # subset house_pricing to validation indices only
validation_labels <- data_dummy[assignment == 2, labels]  # subset house_pricing to validation indices only

test_features <- data_dummy[assignment == 3, features]   # subset house_pricing to test indices only
test_labels <- data_dummy[assignment == 3, labels]   # subset house_pricing to test indices only
