Corolla <- read.csv(file.choose())
View(Corolla)
Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)
#EDA
attach(Corolla)
summary(Corolla)
hist(Price)
qqnorm(Price)
qqline(Price)
hist(Age_08_04)
qqnorm(Age_08_04)
qqline(Age_08_04)
hist(KM)
qqnorm(KM)
qqline(KM)
hist(HP)
qqnorm(HP)
qqline(HP)
hist(Quarterly_Tax)
qqnorm(Quarterly_Tax)
qqline(Quarterly_Tax)
hist(Weight)
qqnorm(Weight)
qqline(Weight)
# Scatter Plot for all variables
pairs(Corolla)
cor(Corolla)# Correlation matrix
#Multiple Linear Model
toyo_mod <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=Corolla)
summary(toyo_mod)

toyo_mod1 <- lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight,data=Corolla)
summary(toyo_mod1)

#######Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Corolla, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

# Partial Correlation Matrix
library(corpcor)
cor(Corolla)
cor2pcor(cor(Corolla))

# Diagnostic Plots
library(car)
#Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
plot(toyo_mod)
#Deletion Diagnostics for identifying influential variable
influence.measures(toyo_mod)
influenceIndexPlot(toyo_mod) 
influencePlot(toyo_mod)
#Regression after deleting the 81st observation
toyo_mod3 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=Corolla[-81,])
summary(toyo_mod3)

### Variance Inflation Factors
vif(toyo_mod)
summary(toyo_mod)
#### Added Variable Plots ######
avPlots(toyo_mod, id.n=5, id.cex=100, col="blue")

libray("MASS")
toyo_model <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=Corolla)
summary(toyo_model)
stepAIC(toyo_model)

toyo_model_Final <- lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data=Corolla[-81,])
summary(toyo_model_Final)
confint(toyo_model_Final,level=0.95)
pred1 <- predict(toyo_model_Final,interval="predict")
pred1 <- as.data.frame(pred1)
View(pred1)
