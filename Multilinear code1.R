ToyotaCorolla_1_<- read.csv(file.choose()) # choose the Cars.csv data set

Corolla <- ToyotaCorolla_1_[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

View(Corolla)
attach(Corolla)
summary(Corolla)

boxplot(Price,horizontal =TRUE)
boxplot(Age_08_04,horizontal = TRUE)
boxplot(KM,horizontal = TRUE)
boxplot(HP,horizontal = TRUE)
boxplot(cc,horizontal = TRUE)
boxplot(Doors,horizontal = TRUE)
boxplot(Gears,horizontal = TRUE)
boxplot(Quarterly_Tax,horizontal = TRUE)
boxplot(Weight,horizontal = TRUE)
plot(Corolla)

pairs(Corolla)  #find the correleation b/w output


# 8. Correlation Coefficient matrix - Strength & Direction of Correlation##always take and check down of the digonal
cor(Corolla)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)

cor2pcor(cor(Corolla))

# The Linear Model of interest with all the columns
model.Price <- lm (Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)

summary(model.Price)

# Predection based on only Doors 
model.price1<-lm(Price~Doors) #It became significant 
summary(model.price1) 

#predection based on only cc
model.price2 <-lm(Price~cc)  #It became significant
summary(model.price2)

# Predecxtion based on Doors and cc 
model.price3 <- lm(Price~Doors + cc)  # both are significant
summary(model.price3)

# It is better to delete influential observations not entire column for to check influentia install library(car)

library(car) 

influence.measures(model.Price)#Influence model function enter 1st lm function model

influenceIndexPlot(model.Price,id.n=3) #Index plot for data measure as per thumb rule 81 is more than 0.05 so we can delete it(no.). 
influencePlot(model.Price,id.n=3)   

model.price4<-lm (Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=Corolla[-81,])
summary(model.price4)  # still Doors value is insignicant 


##  check the collinearity we can use vif>10 then there exists collinearity among all the variables (Vif-variance inflation factor)
library(MASS)

vif(model.Price)

avPlots(model.Price,id.n=3,id.cex=0.7) #added variable plot check correlation b/w  variables and o/p variables

model.pricefinal  <- lm (Price ~ Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data=Corolla[-81,]) # remove Doors from the model 
summary(model.pricefinal)
#Evaluate LINE model assumptions ( linear assumptions- error are independly disributed)
plot(model.pricefinal)
#################################################################
