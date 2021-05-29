##Reihenfolge festlegen
#Warengruppen
#Wochentage
#Temperatur
#Kieler Woche
#Bewölkung
#Windgeschwindigkeit

library(ggplot2)
library(dplyr)

#Import Data
umsatzdaten <- umsatzdaten <- read_csv(file.path('Daten','umsatzdaten_gekuerzt.csv',fsep = .Platform$file.sep))
kiwo <- read_csv(file.path('Daten','kiwo.csv',fsep = .Platform$file.sep))
wetter <- read_csv(file.path('Daten','wetter.csv',fsep = .Platform$file.sep))

#Add columns
umsatzdaten$Wochentag <- factor(weekdays(umsatzdaten$Datum))

#Join datasets
data <- left_join(umsatzdaten, kiwo)
data <- left_join(data,wetter)

#Clean Data
data$KielerWoche[is.na(data$KielerWoche)] <- 0
data$Bewoelkung[is.na(data$Bewoelkung)] <- mean(data$Bewoelkung, na.rm=TRUE)
data$Temperatur[is.na(data$Temperatur)] <- mean(data$Temperatur, na.rm=TRUE)
data$Windgeschwindigkeit[is.na(data$Windgeschwindigkeit)] <- mean(data$Windgeschwindigkeit, na.rm=TRUE)
data<-subset(data, select = -Wettercode)
data$Wochentag<- as.numeric(as.integer(data$Wochentag))

M1<-Umsatz ~ as.factor(Warengruppe)
model_M1 <- lm(M1,data,na.action=na.exclude)
summary(model_M1)
#0.645

M2<-update(M1,.~.+Wochentag)
model_M2 <- lm(M2,data,na.action=na.exclude)
summary(model_M2)
#0.673

M3<-update(M2,.~.+Temperatur)
model_M3 <- lm(M3,data,na.action=na.exclude)
summary(model_M3)
#0.673

M4<-update(M3,.~.+KielerWoche)
model_M4 <- lm(M4,data,na.action=na.exclude)
summary(model_M4)
#0.7126

M5<-update(M4,.~.+Bewoelkung)
model_M5 <- lm(M5,data,na.action=na.exclude)
summary(model_M5)
#0.7128

M6<-update(M5,.~.+Windgeschwindigkeit)
model_M6 <- lm(M6,data,na.action=na.exclude)
summary(model_M6)
#0.7128

plot_temperatur <- ggplot(data = data, aes(x = Temperatur , y = Umsatz)) +
  geom_point() + 
  geom_smooth(method = lm) +
  xlim(0, 35) + ylim(0, 2000) + 
  ggtitle("Temperatur")

plot_bewoelkung <- ggplot(data = data, aes(x = Bewoelkung , y = Umsatz)) +
  geom_point() + 
  geom_smooth(method = lm) +
  xlim(0, 8) + ylim(0, 2000) + 
  ggtitle("Bewölkung")

plot_KielerWoche <- ggplot(data = data, aes(x = KielerWoche , y = Umsatz)) +
  geom_point() + 
  geom_smooth(method = lm) +
  xlim(0, 1) + ylim(0, 2000) + 
  ggtitle("Kieler Woche")

plot_Wochentag <- ggplot(data = data, aes(x = Wochentag , y = Umsatz)) +
  geom_point() + 
  geom_smooth(method = lm) +
  xlim(0, 7) + ylim(0, 2000) + 
  ggtitle("Wochentag")

plot_Warengruppe <- ggplot(data = data, aes(x = Warengruppe , y = Umsatz)) +
  geom_point() + 
  geom_smooth(method = lm) +
  xlim(0, 6) + ylim(0, 2000) + 
  ggtitle("Warengruppe")

plot_Windgeschwindigkeit <- ggplot(data = data, aes(x = Windgeschwindigkeit , y = Umsatz)) +
  geom_point() + 
  geom_smooth(method = lm) +
  xlim(3, 35) + ylim(0, 2000) + 
  ggtitle("Windgeschwindigkeit")



z_scores <- as.data.frame(sapply(data, function(data) (abs(data-mean(data))/sd(data))))
head(z_scores)
no_outliners<-z_scores[!rowSums(z_scores>3),]
dim(no_outliners)


# Relevante Spalten: Umsatz, Bewölkung, Temperatur und Windgeschwindigkeit
#Standardabweichung berechnen (sd(x))
sd_Umsatz<-sd(data$Umsatz)
sd_Bewoelkung<-sd(data$Bewoelkung)
sd_Temperatur<-sd(data$Temperatur)
sd_Windgeschwindigkeit<-sd(data$Windgeschwindigkeit)
#Menge an Werten über 3x Standardabweichung
sub_Umsatz<-subset(data, Umsatz > (mean(data$Umsatz) + sd_Umsatz*3) | Umsatz < (mean(data$Umsatz) - sd_Umsatz*3))
sub_Bewoelkung<-subset(data, Bewoelkung > (mean(data$Bewoelkung) + sd_Bewoelkung*3) | Bewoelkung < (mean(data$Bewoelkung) - sd_Umsatz*3))
sub_Temperatur<-subset(data, Temperatur > (mean(data$Temperatur) + sd_Temperatur*3) | Temperatur < (mean(data$Temperatur) - sd_Temperatur*3))
sub_Windgeschwindigkeit<-subset(data, Windgeschwindigkeit > (mean(data$Windgeschwindigkeit) + sd_Windgeschwindigkeit*3) | Windgeschwindigkeit < (mean(data$Windgeschwindigkeit) - sd_Windgeschwindigkeit*3))
#Gefundene Werte auf 3x Standardabweichung setzen
data$Umsatz[data$Umsatz > (mean(data$Umsatz) + sd_Umsatz*3)]<- (mean(data$Umsatz) + sd_Umsatz*3)
data$Umsatz[data$Umsatz < (mean(data$Umsatz) - sd_Umsatz*3)]<- (mean(data$Umsatz) - sd_Umsatz*3)
data$Temperatur[data$Temperatur > (mean(data$Temperatur) + sd_Temperatur*3)]<- (mean(data$Temperatur) + sd_Temperatur*3)
data$Temperatur[data$Temperatur < (mean(data$Temperatur) - sd_Temperatur*3)]<- (mean(data$Temperatur) - sd_Temperatur*3)
data$Windgeschwindigkeit[data$Windgeschwindigkeit > (mean(data$Windgeschwindigkeit) + sd_Windgeschwindigkeit*3)]<- (mean(data$Windgeschwindigkeit) + sd_Windgeschwindigkeit*3)
data$Windgeschwindigkeit[data$Windgeschwindigkeit < (mean(data$Windgeschwindigkeit) - sd_Windgeschwindigkeit*3)]<- (mean(data$Windgeschwindigkeit) - sd_Windgeschwindigkeit*3)
#R-Squared prüfen
M1<-Umsatz ~ as.factor(Warengruppe)
model_M1 <- lm(M1,data,na.action=na.exclude)
summary(model_M1)
#0.645 -> 0.6949

M2<-update(M1,.~.+Wochentag)
model_M2 <- lm(M2,data,na.action=na.exclude)
summary(model_M2)
#0.673 -> 0.7134

M3<-update(M2,.~.+Temperatur)
model_M3 <- lm(M3,data,na.action=na.exclude)
summary(model_M3)
#0.673 -> 0.7559

M4<-update(M3,.~.+KielerWoche)
model_M4 <- lm(M4,data,na.action=na.exclude)
summary(model_M4)
#0.7126 -> 0.7562

M5<-update(M4,.~.+Bewoelkung)
model_M5 <- lm(M5,data,na.action=na.exclude)
summary(model_M5)
#0.7128 -> 0.7562

M6<-update(M5,.~.+Windgeschwindigkeit)
model_M6 <- lm(M6,data,na.action=na.exclude)
summary(model_M6)
#0.7128 -> 0.7562

#Optional: Faktor anpassen
data$Umsatz[data$Umsatz > (mean(data$Umsatz) + sd_Umsatz)]<- (mean(data$Umsatz) + sd_Umsatz)
data$Umsatz[data$Umsatz < (mean(data$Umsatz) - sd_Umsatz)]<- (mean(data$Umsatz) - sd_Umsatz)
data$Temperatur[data$Temperatur > (mean(data$Temperatur) + sd_Temperatur)]<- (mean(data$Temperatur) + sd_Temperatur)
data$Temperatur[data$Temperatur < (mean(data$Temperatur) - sd_Temperatur)]<- (mean(data$Temperatur) - sd_Temperatur)
data$Windgeschwindigkeit[data$Windgeschwindigkeit > (mean(data$Windgeschwindigkeit) + sd_Windgeschwindigkeit)]<- (mean(data$Windgeschwindigkeit) + sd_Windgeschwindigkeit)
data$Windgeschwindigkeit[data$Windgeschwindigkeit < (mean(data$Windgeschwindigkeit) - sd_Windgeschwindigkeit)]<- (mean(data$Windgeschwindigkeit) - sd_Windgeschwindigkeit)

M1<-Umsatz ~ as.factor(Warengruppe)
model_M1 <- lm(M1,data,na.action=na.exclude)
summary(model_M1)
#0.645 -> 0.6949->0.7279

M2<-update(M1,.~.+Wochentag)
model_M2 <- lm(M2,data,na.action=na.exclude)
summary(model_M2)
#0.673 -> 0.7134 -> 0.7445

M3<-update(M2,.~.+Temperatur)
model_M3 <- lm(M3,data,na.action=na.exclude)
summary(model_M3)
#0.673 -> 0.7559 -> 0.7835

M4<-update(M3,.~.+KielerWoche)
model_M4 <- lm(M4,data,na.action=na.exclude)
summary(model_M4)
#0.7126 -> 0.7562 -> 0.7837

M5<-update(M4,.~.+Bewoelkung)
model_M5 <- lm(M5,data,na.action=na.exclude)
summary(model_M5)
#0.7128 -> 0.7562 -> 0.7837

M6<-update(M5,.~.+Windgeschwindigkeit)
model_M6 <- lm(M6,data,na.action=na.exclude)
summary(model_M6)
#0.7128 -> 0.7562 -> 0.7838 -> 80,34