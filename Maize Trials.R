M15<-read.csv("M22-15.csv")
c<-M15[complete.cases(M15)]
my_data_imputed<-M15
my_data_imputed<-my_data_imputed[1:60,1:22]
library(mice)
library(tidyverse)
library(plyr)
library(lme4)
library(stats)
library(ExpDes)
library(doebioresearch)
library(agricolae)
library(Rmisc)
library(mice)
library(openxlsx)
library(car)
library(ggplot2)
View(my_data_imputed)

missing_vars <- colnames(my_data_imputed)[apply(is.na(my_data_imputed), 2, any)]
imputing<-mice(my_data_imputed,m=5,maxit = 50)
complete_impute<-complete(imputing)
my_data_imputed[missing_vars]<-complete_impute[missing_vars]
view(my_data_imputed[missing_vars])
#####################################
head(complete_impute)
model<-lm(PA~Plot+Entry+Genotypes+(Genotypes:Plot),data=complete_impute)
Anova(model)
summary(model)


##############################bar plot
LSD1<-LSD.test(y=model,trt="Genotypes")
group1<-LSD1$groups
let<-group1$groups
plot1<-group_by(as.data.frame(complete_impute),Genotypes)%>% ###has to be a dataframe to work
  summarise(w=mean(EA),sd=sd(EA))%>% 
  arrange(desc(w))
#
EA_plot<-ggplot(plot1,aes(Genotypes,w))+
  geom_bar(stat = "identity",aes(fill=Genotypes),width=.5)+
  geom_errorbar(aes(ymin=w-sd,ymax=w+sd),width=.2)+
  geom_text(aes(label=let,y=w+sd),vjust=-.5)+
  xlab("Genotypes")+
  ylab("Ear aspect")+
  scale_fill_manual(values=col1)+
  theme_classic()+theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   color = "black", size = 10),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(color = "black", size = 15))+
  geom_hline(yintercept =5.333333, linetype = "dashed")

###do not use this 
# Assume your ANOVA output is stored in a variable called 'ano'
write.xlsx(ano,file = "D:/DOC/ANOVA.PRACTICE/ANOVA.PRACTICE/aov-M22-15.xlsx")

# load existing file
wb <- loadWorkbook("aov-M22-15.xlsx")

# create new sheet
addWorksheet(wb, "correlation p")

# write output to new sheet
writeData(wb, sheet = "correlation p")

# save changes to file
saveWorkbook(wb, "D:/DOC/ANOVA.PRACTICE/ANOVA.PRACTICE/aov-M22-15.xlsx", overwrite = TRUE)

library(Hmisc)
numeric_data1 <- complete_impute[, c(4:19, 21:22)]
correlation1<-rcorr(as.matrix(numeric_data1),type = "pearson")
p_value<-round(correlation1$P,2)
r_value<-round(correlation1$r,2)

sig_corr <- p_value< 0.05
sig_corr[is.na(sig_corr)] <- FALSE
sig_corr_idx <- which(sig_corr)
corr_table<- r_value
symbols <-ifelse(sig_corr < 0.001, "***", ifelse(sig_corr < 0.01, "**", 
                                                 ifelse(sig_corr < 0.05, "*", "")))
corr_table[sig_corr] <- paste0(corr_table[sig_corr], symbols[(sig_corr)])

print(corr_table)

# Print correlation matrix with significant symbols

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(correlation1$r, correlation1$P)
library(corrplot)
corrplot(correlation1$r, type="upper", 
         p.mat = correlation1$P, sig.level = 0.05, 
         insig = "label_sig",pch.cex = 2,tl.col = "black",tl.srt =45,
         title = "relationship inter-intra traits")
  
#########################################
?corrplot
