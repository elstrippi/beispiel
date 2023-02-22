## projekt 4
library("MASS")
library("pscl")
library("mpath")

print(Gesundheitszustand$female[which(Gesundheitszustand$female!=0 & Gesundheitszustand$female != 1)])
print(Gesundheitszustand$hhkids[which(Gesundheitszustand$hhkids!=0 & Gesundheitszustand$hhkids != 1)])
print(Gesundheitszustand$handdum[which(Gesundheitszustand$handdum!=0 & Gesundheitszustand$handdum != 1)])
print(Gesundheitszustand$married[which(Gesundheitszustand$married!=0 & Gesundheitszustand$married != 1)])
print(Gesundheitszustand$haupts[which(Gesundheitszustand$haupts!=0 & Gesundheitszustand$haupts != 1)])
print(Gesundheitszustand$reals[which(Gesundheitszustand$reals!=0 & Gesundheitszustand$reals != 1)])
print(Gesundheitszustand$abitur[which(Gesundheitszustand$abitur!=0 & Gesundheitszustand$abitur != 1)])
print(Gesundheitszustand$univ[which(Gesundheitszustand$univ!=0 & Gesundheitszustand$univ != 1)])
print(Gesundheitszustand$working[which(Gesundheitszustand$working!=0 & Gesundheitszustand$working != 1)])
print(Gesundheitszustand$bluec[which(Gesundheitszustand$bluec!=0 & Gesundheitszustand$bluec != 1)])
print(Gesundheitszustand$whitec[which(Gesundheitszustand$whitec!=0 & Gesundheitszustand$whitec != 1)])
print(Gesundheitszustand$self[which(Gesundheitszustand$self!=0 & Gesundheitszustand$self != 1)])
print(Gesundheitszustand$beamt[which(Gesundheitszustand$beamt!=0 & Gesundheitszustand$beamt != 1)])
print(Gesundheitszustand$public[which(Gesundheitszustand$public!=0 & Gesundheitszustand$public != 1)])
print(Gesundheitszustand$addon[which(Gesundheitszustand$addon!=0 & Gesundheitszustand$addon != 1)])

which((Gesundheitszustand$haupts+Gesundheitszustand$reals+Gesundheitszustand$fachhs
          +Gesundheitszustand$abitur+Gesundheitszustand$univ)>1)

for(i in 1:length(Gesundheitszustand$id)){
  if(Gesundheitszustand$univ[i]==1){
    Gesundheitszustand$haupts[i] = 0
    Gesundheitszustand$reals[i] = 0
    Gesundheitszustand$fachhs[i] = 0
    Gesundheitszustand$abitur[i] = 0
  }
  if(Gesundheitszustand$abitur[i]==1){
    Gesundheitszustand$haupts[i] = 0
    Gesundheitszustand$reals[i] = 0
    Gesundheitszustand$fachhs[i] = 0
  }
  if(Gesundheitszustand$fachhs[i]==1){
    Gesundheitszustand$haupts[i] = 0
    Gesundheitszustand$reals[i] = 0
  }
  if(Gesundheitszustand$reals[i]==1){
    Gesundheitszustand$haupts[i] = 0
  }
}

Gesundheitszustand = Gesundheitszustand[-which(Gesundheitszustand$year != "1987") ,]


## poisson regression
m1 <- glm(docvis ~ . - hospvis - id - hsat2 - newhsat -year -Column1, family="poisson", data = Gesundheitszustand)
m1
summary(m1)
AIC(m1)



## es liegt overdispersion vor
mean(Gesundheitszustand$docvis)
var(Gesundheitszustand$docvis)

set.seed(121)
mmean <- NULL
mvar <- NULL

for(i in 1:100){
  tmp <- sample(1:3666,20)
  mvar <- c(mvar,var(Gesundheitszustand$docvis[tmp]))  
  mmean <- c(mmean,mean(Gesundheitszustand$docvis[tmp]))  
}
plot(mmean,mvar)

##wegen overdispersion und quadratischem zusammenhang zwischen
##Erwartungswert und Varianz jetzt negative binomial regression
m2 <- glm.nb(docvis ~ . - hospvis - id - hsat2 - newhsat -year -Column1, data = Gesundheitszustand)
m2
summary(m2)
AIC(m2)

barplot(table(Gesundheitszustand$docvis))

## wegen zeroinflation wird nun ein zeroinfinflated model verwendet
m3 <- zeroinfl(docvis ~ . - hospvis - id - hsat2 - newhsat -year -Column1
               | . - hospvis - id - hsat2 - newhsat -year-Column1-hhinc, data = Gesundheitszustand, dist = "negbin")
summary(m3)
AIC(m3)

## nun backwards selection der variablen
m4 <- be.zeroinfl(m3, data=Gesundheitszustand, dist="negbin", alpha=0.05, trace = FALSE)
summary(m4)
AIC(m4)
sum(m4$residuals^2)/m4$df.residual



## poisson regression
m1n <- glm(hospvis ~ . - docvis - id - hsat2 - newhsat -year -Column1, family="poisson", data = Gesundheitszustand)
m1n
summary(m1n)

## es liegt keine overdispersion vor
mean(Gesundheitszustand$hospvis)
var(Gesundheitszustand$hospvis)


barplot(table(Gesundheitszustand$hospvis))

## wegen zeroinfation wird ein zeroinflated model verwendet
m3n <- zeroinfl(hospvis ~ . - docvis - id - hsat2 - newhsat -year -Column1-hhinc 
               | . - docvis - id - hsat2 - newhsat -year-Column1-hhinc , data = Gesundheitszustand, dist = "poisson")
summary(m3n)

sum(m3n$residuals^2)/m3n$df.residual
sum(m1n$residuals^2)/m1n$df.residual
AIC(m1n)
AIC(m3n)

## backwards selection für die variablen
m4n <- be.zeroinfl(m3n, data=Gesundheitszustand, dist="poisson", alpha=0.05, trace = FALSE)
summary(m4n)

sum(m4n$residuals^2)/m5n$df.residual
AIC(m4n)


sum(Gesundheitszustand$docvis[which(Gesundheitszustand$female==1)])/length(Gesundheitszustand$docvis[which(Gesundheitszustand$female==1)])
sum(Gesundheitszustand$docvis[which(Gesundheitszustand$female==0)])/length(Gesundheitszustand$docvis[which(Gesundheitszustand$female==0)])

sum(Gesundheitszustand$hospvis[which(Gesundheitszustand$female==1)])/length(Gesundheitszustand$hospvis[which(Gesundheitszustand$female==1)])
sum(Gesundheitszustand$hospvis[which(Gesundheitszustand$female==0)])/length(Gesundheitszustand$hospvis[which(Gesundheitszustand$female==0)])

gesf = Gesundheitszustand[which(Gesundheitszustand$female==1) ,]
gesm = Gesundheitszustand[which(Gesundheitszustand$female==1) ,]

## poisson regression
m1f <- glm(docvis ~ . -female - hospvis - id - hsat2 - newhsat -year -Column1, family="poisson", data = gesf)
m1f
summary(m1f)
AIC(m1f)



## es liegt overdispersion vor
mean(gesf$docvis)
var(gesf$docvis)

##wegen overdispersion jetzt negative binomial regression
m2f <- glm.nb(docvis ~ .-female - hospvis - id - hsat2 - newhsat -year -Column1, data = gesf)
m2f
summary(m2f)
AIC(m2f)

barplot(table(Gesundheitszustand$docvis))

## wegen zeroinflation wird nun ein zeroinfinflated model verwendet
m3f <- zeroinfl(docvis ~ .-female - hospvis - id - hsat2 - newhsat -year -Column1
               | .-female - hospvis - id - hsat2 - newhsat -year-Column1, data = gesf, dist = "negbin")
summary(m3f)
AIC(m3f)

## nun backwards selection der variablen
m4f <- be.zeroinfl(m3f, data=Gesundheitszustand, dist="negbin", alpha=0.05, trace = FALSE)
summary(m4f)
AIC(m4f)
sum(m4f$residuals^2)/m4f$df.residual






