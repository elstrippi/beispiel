install.packages("moments")
library(moments)

Alter <- rnorm(100, 25, 4)
Studienfach <- sample(c(rep("Statistik", 3), rep("Data Science", 3), rep("Mathe", 2), "Informatik"), 100, replace = TRUE)
InteresseAnMathe <- sample(1:7, 100, replace = TRUE)
InteresseAnProgrammieren <- sample(1:7, 100, replace = TRUE)
MatheLK <- sample(c(TRUE, FALSE), 100, replace=TRUE)

Daten <- data.frame(Alter, Studienfach, InteresseAnMathe, InteresseAnProgrammieren, MatheLK)

Daten$InteresseAnMathe[which(Daten$Studienfach=="Mathe")] <- Daten$InteresseAnMathe[which(Daten$Studienfach=="Mathe")] + 2
Daten$InteresseAnMathe[which(Daten$InteresseAnMathe > 7)] <- 7

Daten$InteresseAnProgrammieren[which(Daten$Studienfach=="Informatik")] <- Daten$InteresseAnProgrammieren[which(Daten$Studienfach=="Informatik")] + 2
Daten$InteresseAnProgrammieren[which(Daten$InteresseAnMathe > 7)] <- 7

mean(Daten$InteresseAnMathe[which(Daten$Studienfach=="Mathe")])
mean(Daten$InteresseAnMathe[which(Daten$Studienfach=="Informatik")])
mean(Daten$InteresseAnMathe[which(Daten$Studienfach=="Data Science")])
mean(Daten$InteresseAnMathe[which(Daten$Studienfach=="Statistik")])


mean(Daten$InteresseAnProgrammieren[which(Daten$Studienfach=="Mathe")])
mean(Daten$InteresseAnProgrammieren[which(Daten$Studienfach=="Informatik")])
mean(Daten$InteresseAnProgrammieren[which(Daten$Studienfach=="Data Science")])
mean(Daten$InteresseAnProgrammieren[which(Daten$Studienfach=="Statistik")])


write.csv(Daten, "githubDaten.csv")


#a)
functiona <- function(vector){
  Durchschnitt <- mean(vector)
  Varianz <- var(vector)
  Standardabweichung <- sqrt(Varianz)
  Median <- median(vector)
  Minimum <- min(vector)
  Maximum <- max(vector)
  Schiefe <- skewness(vector)
  Wölbung <- kurtosis(vector)
  
  ausgabe <- data.frame(Durchschnitt, Varianz, Standardabweichung, 
                        Median, Minimum, Maximum, Schiefe, Wölbung)
  print(ausgabe)
  }

functiona(Daten$Alter)

functionb <- function(vector){
  table(vector)/100
}


functionc <- function(vector1, vector2){
  n <- length(table(vector1))
  m <- length(table(vector2))
  namen1 <- names(table(vector1))
  namen2 <- names(table(vetor2))
  Kreuztabelle <- matrix(0, n, m)
  colnames(Kreuztabelle) <- names(table(vector2))
  rownames(Kreuztabelle) <- names(table(vector1))
  for(i in 1:n){
    for(j in 1:m){
      Kreuztabelle[i, j] <- subset(Daten[i,], )
    }
  }
  
  return(Kreuztabelle)
}



functionc(Daten$Studienfach, Daten$InteresseAnMathe)
Daten






































