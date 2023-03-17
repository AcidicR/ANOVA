##############################################################################
###############  Analysis of Completely randomized design (CRD     ############
##############################################################################
#Remove or clear old objects stored in the environment
rm(list = ls(all = TRUE))

#Print your current working directory
getwd()

# set your working directory by pasting the path to your working directory
setwd("C:/Users/user/Documents/")

#Importing data into R
#Data_CRD <- read.csv("CRD.csv", header = TRUE)
Data_CRD <- read.csv("CRD.CSV", header = TRUE)

#print the variable names in the dataset
names(Data_CRD)

#print the first six rows of the dataset
head(Data_CRD)

#print the data structure of imported data
str(Data_CRD)

#print summary of data imported
#summary(Data_CRD)

#Define variable
Data_CRD <- as.data.frame(Data_CRD)
str(Data_CRD)
Data_CRD$variety <- as.factor(Data_CRD$variety)

#check the structure of data whether the variable are successfully reclassify.
str(Data_CRD, giv.attr=F)

#load packages
library(agricolae)
library(ggplot2) #generating high quality plot/figure/graphics
#library(ggthemes) #extra themes, geoms, and scales for ggplot2
library(tidyverse) #data manipulation, wrangling
library(multcompView) # mean comparisons, displaying compact letters
library(multcomp)
library(emmeans) # extracting adjusted means


#Descriptive statistics of the data
my_mean <-  Data_CRD %>%  group_by(variety) %>% 
            summarize(n= n(),
                    mean  = mean(yield,na.rm=TRUE),
                    min  = min(yield,na.rm=TRUE),
                    max   = max(yield,na.rm=TRUE),
                    std   = sd(yield,na.rm=TRUE),
                    var   = var(yield,na.rm=TRUE),
                    s.e   = std/sqrt(n),
                    CV    = (std/mean)*100,
                    n_missing  =sum(is.na(yield))/n*100)


write.csv(my_mean ,"decriptive.csv") 

##########################################
#Testing assumptions of analysis of variance
#histogram to test normality
?hist
hist(Data_CRD$yield,col= rainbow(12)) 


##histogram using ggplot
mean=mean(Data_CRD$yield,na.rm = T)
sd=sd(Data_CRD$yield,na.rm = T)
n=length(Data_CRD$yield)
min=min(Data_CRD$yield,na.rm = T)
max=max(Data_CRD$yield,na.rm = T)
bin=sqrt(n)                                                                    
bw=(max-min)/bin

p1 <- ggplot(Data_CRD,aes(x=yield)) +
  geom_histogram(color="black",fill="blue",bins = bin,binwidth = bw) +
  xlab("Yield") + ylab("Frequency") +
  stat_function(fun = function(x) dnorm(x,mean=mean, sd=sd)*n*bw,
                color="darkred",size=1)
  
p1

##Perform a Shapiro-Wilk test for normality of residuals 
shapiro.test(Data_CRD$yield)

# Bartlett Test of Homogeneity of Variances
bartlett.test(yield ~ variety, data= Data_CRD)

# Figner-Killeen Test of Homogeneity of Variances

fligner.test(yield ~ variety, data= Data_CRD) 

#Perform Levene's Test for homogeneity of variances
library(car)
leveneTest(yield  ~ variety, data = Data_CRD)  

################Alternatively using residuals
#
mod <- lm(yield ~ variety, data = Data_CRD)
mod
resid <- residuals(mod)

hist(resid)

shapiro.test(resid)


##################### model for CRD########################           
## Analysis of variance 
mod <- lm(yield ~ variety, data = Data_CRD)

#print analysis of variance table
#anova(mod) same as below

mod %>% anova()

#save output to word document
capture.output(anova(mod),file = "crd.doc")

# print coefficient of variation
mod %>% cv.model()

#
#print LSD value for treatment means
#to use this function you need to load agricolae package
(LSD.test(mod, "variety"))


#Extract adjusted means
#load the emmeans
Adjusted_mean <- mod %>%  emmeans(pairwise ~ "variety", adjust="tukey") %>% 
                              pluck("emmeans") %>% multcomp::cld(details=TRUE, Letters=letters) # add letter display
Adjusted_mean$emmeans

##########################################