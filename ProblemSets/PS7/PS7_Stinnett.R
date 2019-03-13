library(magrittr)
library(dplyr)
library(stargazer)
library(mice)
# loading wage data
Wages<-as.data.frame(read.csv("wages for DFE.csv"))
# dropping rows with na's in either tenure or hgc 
Wages%<>%drop_na(tenure,hgc)
# printing LaTeX table of summary statistics for Wages
stargazer::stargazer(Wages)
# rate at which logwages are missing
values_logWages<-as.data.frame(table(Wages$logwage,useNA = "always"))
560/2229
#imputation methods
# complete cases only 
Wages_na_omit<-na.omit(Wages)
Wages_na_omit[,7]<-Wages_na_omit$tenure^2
lm(Wages_na_omit$logwage~Wages_na_omit$hgc+Wages_na_omit$college+Wages_na_omit$tenure+Wages_na_omit$V7+Wages_na_omit$age+Wages_na_omit$married)
# mean value imputation
Wages2<-Wages
Wages2[,7]<-Wages2$tenure^2
Wages2$logwage[is.na(Wages2$logwage)]<-mean(Wages_na_omit$logwage)
lm(Wages2$logwage~Wages2$hgc+Wages2$college+Wages2$tenure+Wages2$V7+Wages2$age+Wages2$married)
# imputing missing wages from the regression 
Wages3<-Wages
Wages3[,7]<-Wages3$tenure^2
Wages3$logwage[is.na(Wages3$logwage)]<-0.0623931*Wages3$hgc[is.na(Wages3$logwage)]+0.5335692
lm(Wages3$logwage~Wages3$hgc+Wages3$college+Wages3$tenure+Wages3$V7+Wages3$age+Wages3$married)
# using mice
wages4<-Wages
wages4[,7]<-wages4$tenure^2
wages4.imp = mice(wages4, seed = 12345)
summary(wages4.imp)
fit = with(wages4.imp, lm(logwage~hgc+college+tenure+V7+age+married))
round(summary(pool(fit)),5)
