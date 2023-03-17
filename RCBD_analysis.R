##############################################################################
##### Anaysis of  randomized Complete Block design     ############
##############################################################################
#Remove or clear old objects stored in the environment
rm(list = ls(all = TRUE))

#Print your current working directory
getwd()

# set your working directory by pasting the path to your working directory
setwd("C:/Users/user/Documents/")

#Importing data into R
Dat_RCBD <- read.csv("soybean_rcbd.csv", header = TRUE)

#print the variable names in the dataset
names(Dat_RCBD)

#print the first six rows of the dataset
head(Dat_RCBD)

#print the data structure of imported data
str(Dat_RCBD)

#print summary of data imported
summary(Dat_RCBD)

#Re-classify variable 
Dat_RCBD <-as.data.frame(Dat_RCBD)
Dat_RCBD$variety <- as.factor(Dat_RCBD$variety)
Dat_RCBD$block <- as.factor(Dat_RCBD$block)
Dat_RCBD$yield <- as.numeric(Dat_RCBD$yield)
#check the structure of data whether the variable are successfully reclassify.
str(Dat_RCBD, giv.attr=F)

#load packages required for analysis
library(agricolae)
library(ggplot2) #generating high quality plot/figure/graphics
library(lme4)
library(lmerTest)
library(tidyverse) #data manipulation, wrangling
library(multcompView) # mean comparisons, displaying compact letters
library(multcomp)
library(emmeans) # extracting adjusted means


#Descriptive statistics of the data
Dat_RCBD %>%  group_by(variety) %>% 
  summarize(n= n(),
            mean  = mean(yield,na.rm=TRUE),
            min  = min(yield,na.rm=TRUE),
            max   = max(yield,na.rm=TRUE),
            std   = sd(yield,na.rm=TRUE),
            var   = var(yield,na.rm=TRUE),
            s.e   = std/sqrt(n),
            CV    =  (std/mean)*100)

##########################################
#Testing assumptions of analysis of variance
#histogram to test normality
hist(Dat_RCBD$yield) 

##Perform a Shapiro-Wilk test for normality of residuals 
shapiro.test(Dat_RCBD$yield)

# Bartlett Test of Homogeneity of Variances
bartlett.test(yield ~ variety, data= Dat_RCBD)

# Figner-Killeen Test of Homogeneity of Variances

fligner.test(yield ~ variety, data= Dat_RCBD) 

#Perform Levene's Test for homogeneity of variances
library(car)
leveneTest(yield  ~ variety, data = Dat_RCBD)
#########################################################

### Modelling/analysis of variance

mod <-   lm(yield ~ block + variety, data = Dat_RCBD)
anova <- anova(mod)
CV    <- cv.model(mod)
Lsd <-   LSD.test(mod, "variety",console=FALSE,group=TRUE)
HSD <-   HSD.test(mod,"variety",console=FALSE)

#load the emmeans
Adjusted_mean <- mod %>%  emmeans(pairwise ~ "variety", adjust="tukey") %>% 
          pluck("emmeans") %>% multcomp::cld(details=TRUE, Letters=letters) # add letter display
Adjusted_mean$emmeans

