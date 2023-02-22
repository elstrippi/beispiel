library(MASS)
library(ggplot2)
library(ggcorrplot)
library(olsrr)
library(rstudioapi)
library(moments)


#Daten einlesen
setwd(dirname(getSourceEditorContext()$path))
daten <- read.csv("mietspiegel2015.csv", sep = " ")


hist(daten$nmqm, xlab = "Nettomiete in Euro pro Quadratmeter", ylab = "Häufigkeit",  main = "Verteilung der Nettomiete pro Quadratmeter")
hist(daten$wfl, xlab = "Wohnfläche in Quadratmeter", ylab = "Häufigkeit", main = "Verteilung der Wohnfläche" )
barplot(table(daten$rooms), ylab = "Häufigkeit", xlab = "Anzahl der Zimmer", main = "Verteilung der Zimmer")
hist(2015 - daten$bj, main = "Verteilung des Alters", xlab="Alter in Jahren", ylab="Häufigkeit")
barplot(table(daten$bez), xlab = "Bezirk", ylab = "Häufigkeit", main = "Verteilung der Bezirke")
barplot(table(daten$wohngut), xlab = "Gute Lage laut Makler", ylab = "Häufigkeit", main = "Verteilung der guten Lage")
barplot(table(daten$wohnbest), xlab = "Beste Lage laut Makler", ylab = "Häufigkeit", main = "Verteilung er besten Lage")
barplot(table(1 - daten$ww0), xlab = "Wasserversorgung vorhanden", ylab = "Häufigkeit", main = "Verteilung der Wasserversorgung")
barplot(table(1 - daten$zh0), xlab = "Zentralheizung vorhanden", ylab = "Häufigkeit", main = "Verteilug der Zentralheizungen")
barplot(table(1 - daten$badkach0), xlab="Bad gefliest", ylab = "Häufigkeit", main ="Verteilung der gefliesten Bäder")
barplot(table(daten$badextra), xlab ="Sonderausstattung im Bad vorhanden", ylab = "Häufigkeit", main = "Verteilung der Sonderausstattungen im Bad")
barplot(table(daten$kueche), xlab = "gehobene Ausstattung er Küche vorhanden", ylab = "Häufigkeit", main = "Verteilung der besseren Küchen")


skewness(daten$nmqm)
kurtosis(daten$nmqm)
shapiro.test(daten$nmqm)

skewness(daten$wfl)
kurtosis(daten$wfl)




#Bezirke umkodieren
bezirke <- names(table(daten$bez))
for(i in 1:25){
  daten <- cbind(daten, bezirke[i] == daten$bez)
  colnames(daten)[13 + i] <- bezirke[i]
}




#Zielvarialen und die alte Bezirksspalte entfernen
fullData <- daten[, c(-1, -6, -38)]*1







#Andere Daten umkodieren
fullData$bj <- 2015 - fullData$bj
fullData$ww0 <- 1 - fullData$ww0
fullData$zh0 <- 1 - fullData$zh0
fullData$badkach0 <- 1 - fullData$badkach0
names(fullData)[4] <- "Alter"


#Erstes Modell
model <- lm(nmqm~.,data=fullData)
sum(model$residuals^2)/3065

model$coefficients





#Korrelationsmatrix
ggcorrplot(cor(fullData[, 1:11]))


#Shapiro-Test
hist(daten$nmqm)
shapiro.test(daten$nmqm)
#Preise nicht Normalverteilt





#AIC
stepAIC(lm(nmqm~., data = fullData), direction = "both", k = 2)

#BIC
stepAIC(lm(nmqm~., data = fullData), direction = "both", k = log(nrow(fullData)))


#Bestes AIC Modell
AIC.modell <- lm(formula = nmqm ~ rooms + Alter + wohngut + wohnbest + ww0 + 
                   zh0 + badkach0 + badextra + kueche + `Allach-Untermenzing` + 
                   `Au-Haidhausen` + Aubing... + `Berg am Laim` + Bogenhausen + 
                   `Fledmoching-Hasenbergel` + Hadern + Laim + `Ludwigvorstadt-Isarvorstadt` + 
                   Maxvorstadt + `Milbersthofen-Am Hart` + Moosach + Obergiesing + 
                   `Pasing-Obermenzing` + `Ramersdorf-Perlach` + `Schwabing-Freimann` + 
                   `Sendling-Westpark` + Thalkirchen... + `Trudering-Riem`, 
                 data = fullData)

#Bestes BIC Modell
BIC.modell <- lm(formula = nmqm ~ rooms + Alter + wohngut + wohnbest + ww0 + 
                   zh0 + badkach0 + badextra + kueche + Aubing... + `Berg am Laim` + 
                   Bogenhausen + `Fledmoching-Hasenbergel` + Hadern + Laim + 
                   `Ludwigvorstadt-Isarvorstadt` + Maxvorstadt + `Milbersthofen-Am Hart` + 
                   Moosach + Obergiesing + `Pasing-Obermenzing` + `Ramersdorf-Perlach` + 
                   `Sendling-Westpark` + Thalkirchen... + `Trudering-Riem`, 
                 data = fullData)
sum(AIC.modell$residuals^2)/3065
#[1] 5.09981
length(AIC.modell$coefficients)
#[1] 29

sum(BIC.modell$residuals^2)/3065
#[1] 5.158828
length(BIC.modell$coefficients)
#[1] 26





BIC.modell2 <- lm(formula = nmqm ~ rooms + Alter + wohngut + wohnbest + ww0 + zh0 + 
                                  badkach0 + badextra + kueche + Aubing... + `Berg am Laim` + 
                                  Bogenhausen + `Fledmoching-Hasenbergel` + Hadern + Laim + 
                                  `Ludwigvorstadt-Isarvorstadt` + Maxvorstadt + `Milbersthofen-Am Hart` + 
                                  Moosach + Obergiesing + `Pasing-Obermenzing` + `Ramersdorf-Perlach` + 
                                  `Sendling-Westpark` + Thalkirchen... + `Trudering-Riem`, 
                                data = fullData[which(cooks.distance(BIC.modell)<0.0013),])

sum(BIC.modell2$residuals^2)/nrow(fullData[which(cooks.distance(BIC.modell)<0.0013),])
#[1] 3.936664
round(BIC.modell2$coefficients, 3)














