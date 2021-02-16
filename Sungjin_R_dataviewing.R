# Clear workspace
rm(list = ls())
# Set working directory
setwd("J:/Sungjin_Pig_Data/Sungjin_Pig_Data_analysis")
# load data files
#Phenotype file
phenotypes <- read.table("Pheno.txt")
colnames(phenotypes) <- c("ARN","SSIRE", "TBATCH", "RBATCH","SEX","PAR", "MPAR",
                          "TAGE","BWT","ADGW","ADGP","BF2","D90kg","BF1","BF3",
                          "BFM","DP","EMA","NBA","LWT","ABW","WEI","TNB","HNB",
                          "SBP","LSD","RFI")

# Count unique
sapply(phenotypes, function(x) length(unique(x)))
#change zero values to missing NA
phenotypes[, 7:26][phenotypes[, 7:26] == 0] <- NA
#Histograms
hist(phenotypes$MPAR)
hist(phenotypes$TAGE)
hist(phenotypes$MPAR)
hist(phenotypes$BWT)
hist(phenotypes$ADGW)
hist(phenotypes$BF1)
hist(phenotypes$BF2)
hist(phenotypes$BF3)
hist(phenotypes$D90kg)
hist(phenotypes$BFM)
hist(phenotypes$DP)
hist(phenotypes$EMA)
hist(phenotypes$NBA)
hist(phenotypes$LWT)
hist(phenotypes$ABW)
hist(phenotypes$WEI)
hist(phenotypes$TNB)
hist(phenotypes$HNB)
hist(phenotypes$SBP)
hist(phenotypes$LSD)
hist(phenotypes$RFI)

#Statistics
summary(phenotypes$MPAR)
summary(phenotypes$TAGE)
summary(phenotypes$BWT)
summary(phenotypes$BF1)
summary(phenotypes$BF2)
summary(phenotypes$BF3)
summary(phenotypes$D90kg)
summary(phenotypes$BFM)
summary(phenotypes$DP)
summary(phenotypes$EMA)
summary(phenotypes$NBA)
summary(phenotypes$LWT)
summary(phenotypes$ABW)
summary(phenotypes$WEI)
summary(phenotypes$TNB)
summary(phenotypes$HNB)
summary(phenotypes$SBP)
summary(phenotypes$LSD)
summary(phenotypes$RFI)

#Count distinct values
table(phenotypes$MPAR)
table(phenotypes$TAGE)
table(phenotypes$BWT)
table(phenotypes$BF1)
table(phenotypes$BF2)
table(phenotypes$BF3)
table(phenotypes$D90kg)
table(phenotypes$BFM)
table(phenotypes$DP)
table(phenotypes$EMA)
table(phenotypes$NBA)
table(phenotypes$LWT)
table(phenotypes$ABW)
table(phenotypes$WEI)
table(phenotypes$TNB)
table(phenotypes$HNB)
table(phenotypes$SBP)
table(phenotypes$LSD)
table(phenotypes$RFI)

#count percentages
#install.packages("qwraps2")
library(qwraps2)
n_perc(phenotypes$MPAR==0)
n_perc(phenotypes$MPAR==8)
n_perc(phenotypes$MPAR==6)

#Descriptive statistics
#install.packages("table1")
library(table1)
phenotypes$SEX <- 
  factor(phenotypes$SEX, 
         levels=c(2,1),
         labels=c("Male", # Reference
                  "Female"))
table1::label(phenotypes$MPAR) <- "MPAR"
table1::label(phenotypes$TAGE) <- "TAGE"
table1::label(phenotypes$BWT) <- "BWT"
table1::label(phenotypes$BF1) <- "BF1"
table1::label(phenotypes$BF2) <- "BF2"
table1::label(phenotypes$BF3) <- "BF3"
table1::label(phenotypes$BF1) <- "BF1"
table1::label(phenotypes$D90kg) <- "D90kg"
table1::label(phenotypes$BFM) <- "BFM"
table1::label(phenotypes$DP)<-"DP"
table1::label(phenotypes$EMA)<-"EMA"
table1::label(phenotypes$NBA)<-"NBA"
table1::label(phenotypes$LWT)<-"LWT"
table1::label(phenotypes$ABW)<-"ABW"
table1::label(phenotypes$WEI)<- "WEI"
table1::label(phenotypes$TNB)<-"TNB"
table1::label(phenotypes$HNB)<-"HNB"
table1::label(phenotypes$SBP)<-"SBP"
table1::label(phenotypes$LSD)<-"LSD"
table1::label(phenotypes$RFI)<-"RFI"
table1::table1(~ MPAR+ TAGE + BWT+BF1+BF2+BF3+BWT+ADGW+ADGP+D90kg+BFM+DP+EMA+NBA+LWT+
               ABW+WEI+TNB+HNB+SBP+LSD+RFI|SEX, data = phenotypes,overall="Total",
            topclass="Rtable1-grid Rtable1-shade Rtable1-times")
#Few of the variables at ago
table1::table1(~MPAR+ TAGE +BWT+BF1+BF2+BF3+BWT+ADGW+ADGP|SEX, data = phenotypes,overall="Total",
               topclass="Rtable1-grid Rtable1-shade Rtable1-times")









