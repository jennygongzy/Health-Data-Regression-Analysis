######################################
## Class Project- Statistics Method ##
##        Disease in the U.S        ##
######################################

# Read Data

y = read.csv('Disease.csv',stringsAsFactors = FALSE)
dim(y)
summary(y)

# divide explanatory variables into two groups
colnames(y)
exp.vars.1 <- c("Drinking","HighBP","MedCol_HighBP","Smoking","HighChol",
                "LackWorkOut","PoorMentalHealth","PoorPhyHealth","TeethLost","SleepLess")
exp.vars.2 <- c("NoInsurance","Reg_Doc_Visit","Tests","Preventive_M","Preventive_W",
                "DentalVisit","Mammo","PapaniTest","Chol_Screening")

#scatter plots
col.vector <- rep('black',times=nrow(y))
col.vector[y$Region=='West'] <- 'green'
col.vector[y$Region=='Northeast'] <- 'red'
col.vector[y$Region=='South'] <- 'blue'

pairs(y[,c('Asthma',exp.vars.1)],las=TRUE,pch =19 ,col = col.vector)

# VIF check
require(usdm)
vif(y[,c(exp.vars.1,exp.vars.2)])
# Delete variables
exp.vars.1 <- c("Drinking","HighBP","MedCol_HighBP","Smoking","HighChol",
                "PoorPhyHealth","SleepLess")
exp.vars.2 <- c("NoInsurance","Reg_Doc_Visit","Tests","Preventive_M","Preventive_W",
                "Mammo","PapaniTest","Chol_Screening")

vif(y[,c(exp.vars.1,exp.vars.2)])

cor(y[,c('Cancer','Obesity','Diabetes','Heart',exp.vars.1,exp.vars.2)])

############## Model - 'Cancer' ##############
## step wise select variables 
require(leaps)
step.1 <- regsubsets(Cancer ~ Region+Drinking+HighBP+MedCol_HighBP+Smoking+HighChol
                           +PoorPhyHealth+SleepLess+NoInsurance+Reg_Doc_Visit+Tests
                           +Preventive_M+Preventive_W+Mammo+PapaniTest+Chol_Screening,
                           data=y, method="forward")
summary(step.1)

# compare C_p values
round(summary(step.1)$cp, digits=1)

# Build a linear model
lm.1 <- lm(Cancer ~ Region+Drinking+HighBP+MedCol_HighBP+Smoking+HighChol
           +PoorPhyHealth+SleepLess+NoInsurance+Reg_Doc_Visit+Tests
           +Preventive_M+Preventive_W+Mammo+PapaniTest+Chol_Screening,
           data=y)
summary(lm.1)

#delete insignificant variables
lm.1 <- lm(Cancer ~ Region+Drinking+HighBP+HighChol+PoorPhyHealth+SleepLess+NoInsurance+Tests
           +Mammo+PapaniTest,data=y)
summary(lm.1)

#delete not linear variables
#log transform NoInsurance
y$l_NoInsurance = log(y$NoInsurance)

par(mfrow=c(1,2))
hist(y$NoInsurance, main='No Insurance',col ='blue',xlab='NoInsurance'
     ,cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20 )
hist(y$l_NoInsurance, main='Log(NoInsurance)',col ='blue',xlab='Log(NoInsurance)'
     ,cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20 )

#Delete Region variables
## Cancer vs. Region 
boxplot(y$Cancer~y$Region,main='Cancer Distribution among Regions',
        ylab='Cancer(%)',cex.main=1.4, cex.lab=1.4, cex.axis=1.4)
oneway.test(y$Cancer~ y$Region, var.equal=TRUE)


###Final Model for Cancer###
lm.1 <- lm(Cancer ~ HighBP+HighChol+log(NoInsurance)+Tests,data=y)
summary(lm.1)


######### Check Assumptions #########

#1. Linearity 
pairs(y[,c("Cancer","HighBP","HighChol",'NoInsurance',"l_NoInsurance","Tests")],
      col ="#0080ff70",pch=19,las=TRUE)

#2. Multicolinearity
vif(y[,c("HighBP","HighChol","NoInsurance","Tests")])


par(mfrow=c(1,2))
#3.Constant Variance - Residual vs. Fitted
plot(lm.1$fitted.values,lm.1$residuals,pch = 19, main="Cancer Linear Model\nResiduals vs. Fitted Values"
     ,xlab="fitted values", ylab="residuals",col='#df202070',
     cex.main=1.0, cex.lab=0.8, cex.axis=0.8)
abline(h=c(0, summary(lm.1)$sigma*2, -summary(lm.1)$sigma*2) , lty=2, lwd=2,
       col=c("gray50", "darkolivegreen4", "darkolivegreen4")) 
#4. Normality of Residuals - Normal QQ plot
qqnorm(lm.1$residuals, pch=19, las=TRUE, cex.main=1.0, cex.lab=0.8, cex.axis=0.8,
       main="Normal Quantile Plot of Residuals",col='darkolivegreen')
qqline(lm.1$residuals)	

#5. Check outliers 
influence.measures(lm.1)


############ Model - 'Obesity'############
#1st Model
lm.2<- lm(Obesity ~ Region+Drinking+HighBP+MedCol_HighBP+Smoking+HighChol
           +PoorPhyHealth+SleepLess+NoInsurance+Reg_Doc_Visit+Tests
           +Preventive_M+Preventive_W+Mammo+PapaniTest+Chol_Screening,
           data=y)
summary(lm.2)

#2nd Model - Delete not siginificant variables
lm.2<- lm(Obesity ~ Region+HighBP+Smoking+HighChol+SleepLess+NoInsurance
          +PapaniTest+Chol_Screening,data=y)
summary(lm.2)

#Final Model - Delete not linear variables
lm.2 <- lm(Obesity ~ Region+HighBP+Smoking+SleepLess+NoInsurance,data=y)
summary(lm.2)


######### Check Assumptions #########

#1. Linearity 
col.vector <- rep('black',times=nrow(y))
col.vector[y$Region=='West'] <- 'green'
col.vector[y$Region=='Northeast'] <- 'red'
col.vector[y$Region=='South'] <- 'blue'

pairs(y[,c("Obesity","HighBP","Smoking",'NoInsurance',"SleepLess")],
      col =col.vector,pch=19,las=TRUE)

#2. Multicolinearity
vif(y[,c("HighBP","Smoking",'NoInsurance',"SleepLess")])

#3. Is Region significant? 
boxplot(y$Obesity~y$Region,main='Obesity Distribution among Regions',
        ylab='Obesity(%)',cex.main=1.4, cex.lab=1.4, cex.axis=1.4)
par(mfrow=c(1,2))
# one-way ANOVA
oneway.test(y$Obesity~ y$Region, var.equal=TRUE)

#4. Residual plots 
par(mfrow=c(2,2))
col.vector <- rep('black',times=nrow(y))
col.vector[y$Region=='West'] <- 'green'
col.vector[y$Region=='Northeast'] <- 'red'
col.vector[y$Region=='South'] <- 'blue'

# Obesity vs. NoInsurance 
plot(y$NoInsurance,y$Obesity,pch = 19, main="(A)Obesity vs. NoInsurance"
     ,xlab="NoInsurance", ylab="Obesity",col=col.vector,
     cex.main=1.0, cex.lab=0.8, cex.axis=0.8)

# Obesity vs. Smoking 
plot(y$Smoking,y$Obesity,pch = 19, main="(B)Obesity vs. Smoking"
     ,xlab="Smoking", ylab="Obesity",col=col.vector,
     cex.main=1.0, cex.lab=0.8, cex.axis=0.8)


#Constant Variance - Residual vs. Fitted
plot(lm.2$fitted.values,lm.2$residuals,pch=19, main="(C)Obesity Linear Model\nResiduals vs. Fitted Values"
     ,xlab="fitted values", ylab="residuals",col=col.vector,
     cex.main=1.0, cex.lab=0.8, cex.axis=0.8)
abline(h=c(0, summary(lm.2)$sigma*2, -summary(lm.2)$sigma*2) , lty=2, lwd=2,
       col=c("gray50", "darkolivegreen4", "darkolivegreen4")) 

#Normality of Residuals - Normal QQ plot
qqnorm(lm.2$residuals, pch=19, las=TRUE, cex.main=1.0, cex.lab=0.8, cex.axis=0.8,
       main="(D)Normal Quantile Plot of Residuals",col='firebrick')
qqline(lm.2$residuals)	

##
legend("topleft", legend=c("MidWest", "West","Northeast","South"), 
       col=c("black", "green",'red','blue'), pch=19,bty="n", cex=1.0)

#5. Check outliers 
influence.measures(lm.2)



## Model - 'Diabetes'
#1st Model
lm.3<- lm(Diabetes ~ Region+Drinking+HighBP+MedCol_HighBP+Smoking+HighChol
          +PoorPhyHealth+SleepLess+NoInsurance+Reg_Doc_Visit+Tests
          +Preventive_M+Preventive_W+Mammo+PapaniTest+Chol_Screening,
          data=y)
summary(lm.3)

#2nd Model Delete not sigificant variables
lm.3<- lm(Diabetes ~ Region+HighBP+Smoking+HighChol
          +SleepLess+NoInsurance+Reg_Doc_Visit+Tests
          +Preventive_M,
          data=y)
summary(lm.3)


######### Check Assumptions #########

#1. Linearity 
col.vector <- rep('black',times=nrow(y))
col.vector[y$Region=='West'] <- 'green'
col.vector[y$Region=='Northeast'] <- 'red'
col.vector[y$Region=='South'] <- 'blue'

pairs(y[,c("Diabetes","HighBP","Smoking","HighChol",
           "SleepLess","NoInsurance","Reg_Doc_Visit","Tests",
           "Preventive_M")],
      col =col.vector,pch=19,las=TRUE)

#2. Multicolinearity
vif(y[,c("HighBP","Smoking","HighChol","SleepLess","NoInsurance","Reg_Doc_Visit","Tests","Preventive_M")])

#3. Is Region significant? 
boxplot(y$Diabetes~y$Region,main='Diabetes Distribution among Regions',
        ylab='Diabetes(%)',cex.main=1.4, cex.lab=1.4, cex.axis=1.4)
par(mfrow=c(1,2))
# one-way ANOVA
oneway.test(y$Diabetes~ y$Region, var.equal=TRUE)

#4. Residual plots 
par(mfrow=c(2,2))
col.vector <- rep('black',times=nrow(y))
col.vector[y$Region=='West'] <- 'green'
col.vector[y$Region=='Northeast'] <- 'red'
col.vector[y$Region=='South'] <- 'blue'

# Diabetes vs. HighBP 
plot(y$HighBP,y$Diabetes,pch = 19, main="(A)Diabetes vs. HighBP"
     ,xlab="HighBP", ylab="Diabetes",col=col.vector,
     cex.main=1.0, cex.lab=0.8, cex.axis=0.8)

# Diabetes vs. NoInsurance
plot(y$NoInsurance,y$Diabetes,pch = 19, main="(B)Diabetes vs. NoInsurance"
     ,xlab="NoInsurance", ylab="Diabetes",col=col.vector,
     cex.main=1.0, cex.lab=0.8, cex.axis=0.8)


#Constant Variance - Residual vs. Fitted
plot(lm.3$fitted.values,lm.3$residuals,pch=19, main="(C)Diabetes Linear Model\nResiduals vs. Fitted Values"
     ,xlab="fitted values", ylab="residuals",col=col.vector,
     cex.main=1.0, cex.lab=0.8, cex.axis=0.8)
abline(h=c(0, summary(lm.3)$sigma*2, -summary(lm.3)$sigma*2) , lty=2, lwd=2,
       col=c("gray50", "darkolivegreen4", "darkolivegreen4")) 

#Normality of Residuals - Normal QQ plot
qqnorm(lm.3$residuals, pch=19, las=TRUE, cex.main=1.0, cex.lab=0.8, cex.axis=0.8,
       main="(D)Normal Quantile Plot of Residuals",col='firebrick')
qqline(lm.3$residuals)	

##
legend("topleft", legend=c("MidWest", "West","Northeast","South"), 
       col=c("black", "green",'red','blue'), pch=19,bty="n", cex=1.0)

#5. Check outliers 
influence.measures(lm.3)



