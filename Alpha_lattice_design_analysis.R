##############################################################################
###############       Analysis of  lattice design             ############
##############################################################################
#Remove or clear old objects stored in the environment
rm(list = ls(all = TRUE))

#Print your current working directory
getwd()

# set your working directory by pasting the path to your working directory
setwd("C:/Users/user/Documents/")

#Importing data into R
my_alpha <- read.csv("alphalattice.csv", header = TRUE)

#print the variable names in the dataset
names(my_alpha)

#print the first six rows of the dataset
head(my_alpha)

#print the data structure of imported data
str(my_alpha)

#print summary of data imported
#summary(my_alpha)

#Re-classify variable 
my_alpha <- as.data.frame(my_alpha)
my_alpha$rep  <- as.factor(my_alpha$rep)
my_alpha$block  <- as.factor(my_alpha$block)
my_alpha$gen <- as.factor(my_alpha$gen)

#check the structure of data whether the variable are successfully reclassify.
str(my_alpha)

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
my_alpha %>%  group_by(gen) %>% 
      summarize(n= n(),
                mean  = mean(yield,na.rm=TRUE),
                min  = min(yield,na.rm=TRUE),
                max   = max(yield,na.rm=TRUE),
                std   = sd(yield,na.rm=TRUE),
                var   = var(yield,na.rm=TRUE),
                s.e   = std/sqrt(n),
                CV    =  (std/mean)*100,
                n_missing=sum(is.na(yield))/n*100)

# Test for assumptions of anova before analysis



# linear model of lattice design
m1 <- lm(yield ~ gen + rep/block, data = my_alpha)
m1
anova(m1)
anova <- anova(m1)
CV    <- cv.model(m1)
Lsd <-   LSD.test(m1, "gen",console=FALSE,group=TRUE)
HSD <-   HSD.test(m1,"gen",console=FALSE)

#load the emmeans
Adjusted_mean <- m1 %>%  emmeans(pairwise ~ "gen", adjust="tukey") %>% 
  pluck("emmeans") %>% multcomp::cld(details=TRUE, Letters=letters) # add letter display
Adjusted_mean$emmeans

