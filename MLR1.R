comp_data <- read.csv(file.choose())
View(comp_data)
comp_data$cd1 <- ifelse(comp_data$cd=="yes",1,0)
comp_data$multi1 <- ifelse(comp_data$multi=="yes",1,0)
comp_data$premi <- ifelse(comp_data$premium=="yes",1,0)
View(comp_data)
com_data <- comp_data[-c(1,7,8,9)]
View(com_data)
#EDA
attach(com_data)
summary(com_data)
hist(price)
qqnorm(price)
qqline(price)
hist(speed)
qqnorm(speed)
qqline(speed)
hist(hd)
qqnorm(hd)
qqline(hd)
hist(ram)
qqnorm(ram)
qqline(ram)
hist(ads)
qqnorm(ads)
qqline(ads)
# Scatter Plot for all variables
pairs(com_data)
# Correlation between variables
cor(com_data)
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
pairs(com_data, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

# Multiple Linear Model
com_mod1 <- lm(price~speed+hd+ram+screen+ads+trend+cd1+multi1+premi,data=com_data)
summary(com_mod1)

# Partial Correlation Matrix
library(corpcor)
cor(com_data)
cor2pcor(cor(com_data))

# Diagnostic Plots
#Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
plot(com_mod1)

influence.measures(com_mod1)
influenceIndexPlot(com_mod1)
influencePlot(com_mod1)

#Regression after deleting the 1441st and 1701st observation
com_mod2 <- lm(price~speed+hd+ram+screen+ads+trend+cd1+multi1+premi,data=com_data[-c(1441,1701)])
summary(com_mod2)

### Variance Inflation Factors
vif(com_mod1)
summary(com_mod1)
#### Added Variable Plots ######
avPlots(com_mod1, id.n=5, id.cex=100, col="red")

library("MASS")
stepAIC(com_mod1)


com_mod_final <- lm(price~speed+hd+ram+screen+ads+trend+cd1+multi1+premi,data=com_data)
summary(com_mod_final)
confint(com_mod_final,level=0.95)
pred <- predict(com_mod_final,interval="predict")
pred <- as.data.frame(pred)
View(pred)
