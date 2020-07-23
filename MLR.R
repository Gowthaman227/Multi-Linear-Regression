# 50_Startups
startups <- read.csv(file.choose())
View(startups)
colnames(startups) <- c("RD","Admin","Market","State","Profit")
startups <- startups[-4]
View(startups)
#EDA
attach(startups)
summary(startups)
hist(Admin)
qqnorm(Admin)
qqline(Admin)
hist(Market)
qqnorm(Market)
qqline(Market)
hist(Profit)
qqnorm(Profit)
qqline(Profit)
hist(RD)
qqnorm(RD)
qqline(RD)
plot(RD,Profit)
plot(Admin,Profit)
plot(Market,Profit)
# Combined Scatter Plot for all variables
pairs(startups)
cor(RD,Profit)
cor(Admin,Profit)
cor(Market,Profit)
cor(Admin,Market)
cor(Admin,RD)
cor(Market,RD)
cor(startups)
# Multiple Linear Model
mod_startup <- lm(Profit~RD+Admin+Market,data=startups)
summary(mod_startup)


mod_staruptAM <- lm(Profit~Admin+Market,data=startups)
summary(mod_staruptAM)

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
pairs(startups, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

#Partial Correlation matrix
library(corpcor)
cor(startups)
View(startups)

cor2pcor(cor(startups))

# Diagnostic Plots
#install.packages(car)
library(car)
#Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
plot(mod_startup)

#Deletion Diagnostics for identifying influential variable
influence.measures(mod_startup)
influenceIndexPlot(mod_startup) 
influencePlot(mod_startup)

#Regression after deleting the 49th and 50th observation
mod1_startup <- lm(Profit~Admin+RD+Market,data=startups[-c(49,50),])
summary(mod1_startup)



### Variance Inflation Factors
vif(mod_startup)
summary(mod_startup)
VIFA <- lm(Admin~Profit+Market+RD)
VIFR <- lm(RD~Profit+Market+Admin)
VIFM <- lm(Market~Profit+RD+Admin)
summary(VIFA)
summary(VIFR)
summary(VIFM)
#### Added Variable Plots ######
avPlots(mod_startup, id.n=5, id.cex=100, col="red")

library("MASS")
Profit_mod <- lm(Profit~RD+Admin+Market)
stepAIC(Profit_mod)


PredModel_Final <- lm(Profit~RD+Market,data=startups)
summary(PredModel_Final)
confint(PredModel_Final,level=0.95)
pred_model <- predict(PredModel_Final,interval="predict")
pred_model <- as.data.frame(pred_model)
View(pred_model)
plot(PredModel_Final)

R_squared <- matrix(c(0.9507,0.9627,0.9485),ncol=1,nrow=3,byrow=T,dimnames=list(c("Model 1","Model 2","Model 3"),c("R_Squared")))
View(R_squared)

