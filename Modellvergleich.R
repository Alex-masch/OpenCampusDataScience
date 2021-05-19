##Reihenfolge festlegen
#Warengruppen
#Wochentage
#Temperatur
#Kieler Woche
#Bewölkung
#Windgeschwindigkeit


data<-umsatzdaten_kiwo_wetter

M1<-Umsatz ~ as.factor(Warengruppe)
model_M1 <- lm(M1,data,na.action=na.exclude)
summary(model_M1)
#0.645

M2<-update(M1,.~.+Wochentag)
model_M2 <- lm(M2,data,na.action=na.exclude)
summary(model_M2)
#0.673

M3<-update(M2,.~.+Temperatur)
model_M3 <- lm(M2,data,na.action=na.exclude)
summary(model_M3)
#0.673

M4<-update(M3,.~.+KielerWoche)
model_M4 <- lm(M3,data,na.action=na.exclude)
summary(model_M4)
#0.7126

M5<-update(M4,.~.+Bewoelkung)
model_M5 <- lm(M4,data,na.action=na.exclude)
summary(model_M5)
#0.7128

M6<-update(M5,.~.+Windgeschwindigkeit)
model_M6 <- lm(M5,data,na.action=na.exclude)
summary(model_M6)
#0.7128

##Reihenfolge festlegen
#Warengruppen
#Wochentage
#Kieler Woche
#Temperatur
#Bewölkung
#Windgeschwindigkeit

M1<-Umsatz ~ as.factor(Warengruppe)
model_M1 <- lm(M1,data,na.action=na.exclude)
summary(model_M1)
#0.645

M2<-update(M1,.~.+Wochentag)
model_M2 <- lm(M2,data,na.action=na.exclude)
summary(model_M2)
#0.673

M3<-update(M2,.~.+KielerWoche)
model_M3 <- lm(M2,data,na.action=na.exclude)
summary(model_M3)
#0.673

M4<-update(M3,.~.+Temperatur)
model_M4 <- lm(M3,data,na.action=na.exclude)
summary(model_M4)
#0.6747

M5<-update(M4,.~.+Bewoelkung)
model_M5 <- lm(M4,data,na.action=na.exclude)
summary(model_M5)
#0.7128

M6<-update(M5,.~.+Windgeschwindigkeit)
model_M6 <- lm(M5,data,na.action=na.exclude)
summary(model_M6)
#0.7128

##Modellreihenfolge von Anke
#Temperatur+KielerWoche
#Bewölkung
#Wochentag
#Warengruppe
#Windgeschwindigkeit
M1<-Umsatz ~ Temperatur
model_M1 <- lm(M1,data,na.action=na.exclude)
summary(model_M1)
#0.04824

M2<-update(M1,.~.+KielerWoche)
model_M2 <- lm(M2,data,na.action=na.exclude)
summary(model_M2)
#0.0488

M3<-update(M2,.~.+Bewoelkung)
model_M3 <- lm(M2,data,na.action=na.exclude)
summary(model_M3)
#0.0488

M4<-update(M3,.~.+Wochentag)
model_M4 <- lm(M3,data,na.action=na.exclude)
summary(model_M4)
#0.0488

M5<-update(M4,.~.+as.factor(Warengruppe))
model_M5 <- lm(M4,data,na.action=na.exclude)
summary(model_M5)
#0.07639

M6<-update(M5,.~.+Windgeschwindigkeit)
model_M6 <- lm(M5,data,na.action=na.exclude)
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

grid.arrange(plot_temperatur, plot_KielerWoche, plot_bewoelkung, plot_Wochentag, plot_Warengruppe, plot_Windgeschwindigkeit, nrow=2)
