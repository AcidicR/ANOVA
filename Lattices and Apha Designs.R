rm(mode)
Trial1<-read.csv("NOW.csv")
library(metan)
library(tidyverse)
library(plyr)
library(reshape2)
library(agricolae)
install.packages("plotly")
library(plotly)
library(ggplot2)
library(doebioresearch)
view(data_alpha)
metan::data_alpha
attach(data_alpha)
head(data_alpha)
str(data_alpha)
wiams<-PBIB.test(BLOCK,GEN,REP,YIELD,6,method = "VC",test = "lsd",
          alpha = .05,console = TRUE,group = TRUE)
################################
col1<-colorRampPalette(c("#023020","#355E3B","#008000","#2E8B57","#009E60","#023020"))(24)
l<-wiams$groups
bars<-group_by(data_alpha,GEN) %>% 
  summarise(w=mean(YIELD),sd=sd(YIELD)) %>% 
  arrange(desc(w))
p1<-ggplot(bars,aes(GEN,w,fill=GEN))+
  geom_bar(stat ="identity",width=.5)+
  geom_errorbar(aes(ymin=w-sd,ymax=w+sd),width=.2)+
  scale_fill_manual(values =col1 )+
  geom_text(aes(label=l$groups,y=w+sd),vjust=-0.5)+
  theme_classic()+
  theme(legend.position = "none")
####################################################
rm(b)
view
class(model)
library(lme4)
model<-lm(FW~Genotypes+Block+(Genotypes:Block),data=Trial1)
anova(model)
shapiro.test(residuals(model))
test<-LSD.test(y=model,trt = "Genotypes")
cld<-test$groups
bar1<-cld$groups
plot<-group_by(Trial1,Genotypes) %>% 
  summarise(w=mean(FW),sd=sd(FW))

p2<-ggplot(plot,aes(Genotypes,w))+
  geom_bar(stat ="identity",width=.5,aes(fill=Genotypes))+
  geom_errorbar(aes(ymin=w-sd,ymax=w+sd),width=.2)+
  geom_text(aes(label=bar1,y=w+sd),vjust=-.5)+
  xlab("Genotypes")+ylab("Field weight(Kg)")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_fill_manual(values = col)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                 color = "black", size = 10),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(color = "black", size = 15),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept =4.133333, linetype = "dashed")+
  ggtitle("field wieght used as yield estimate for Genotypes")
######
ggsave("Trial barPlot1",plot = p2,width = 6,path = "C:\Users\Administrator\Desktop\ANOVA.PRACTICE", height = 4, dpi = 300)
###########################performing correlation test
install.packages("Hmisc")
library(Hmisc)
rm(M)
class(Trial1)
numeric_data<-Trial1[,3:16]###exclude independent variables
cor.matrix<-cor(numeric_data,method="pearson")##produces only corr,no Pvalues
round(cor.matrix,2)##2.points
cor(x, method = "pearson", use = "complete.obs")###use this when u have missing values
numeric_data
rcor<-rcorr(as.matrix(numeric_data),type="pearson")
pvalue<-rcor$P
round(pvalue,2)
#####4coulumn correlation table
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(rcor$r, rcor$P)
                              
install.packages("corrplot")###vizualize correlation
library(corrplot)
col <- colorRampPalette(c( "#023020","#DAF7A6",
                                             "#00A36C",
                                             "#196f3d"))(20)
  
  
corrplot(cor.matrix,type ="upper",order ="hclust",
         tl.col = "black",col=col)

corrplot(rcor$r, type="upper", order="hclust", 
         p.mat = rcor$P, sig.level = 0.05, 
         insig = "label_sig",pch.cex = 2,tl.col = "black",tl.srt =45,col=)
######
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(numeric_data, histogram=TRUE, pch=19)
rm(complete_impute)
############################################
complete_impute$Rep<-as.factor(complete_impute$Rep) 
complete_impute$Block<-as.factor(complete_impute$Block)      
complete_impute$Genotypes<-as.factor(complete_impute$Genotypes)      
str(complete_impute)
library(mice)
Nitrogen_data<-readxl::read_xlsx("D:/DOC/ANOVA.PRACTICE/ANOVA.PRACTICE/Evaluation of maize Top-crosses under Low nitrogen condition.xlsx")
Nitrogen_data<-Nitrogen_data[,c(2:3,6:23)]
impute_model <- mice(Nitrogen_data, m = 5, maxit = 50)
complete_impute<-complete(impute_model)
###############################################
ALdata<-read.csv(file='https://gist.githubusercontent.com/ikmalmal/1819fc03dd0fb9219c3f42d62693be35/raw/e3f9120f2629d28d68fcb9a600eae29a46a1d288/ALdata.csv')

lattice1<-PBIB.test(complete_impute$Block,complete_impute$Genotypes,complete_impute$Rep,
                    y=complete_impute$Ear_Hieght ,k=10,method = "REML",
                               test="lsd",console = TRUE,group = TRUE,alpha = .05) 
lattice1$ANOVA
library(lme4)
library(car)
rm(lin)
lin<-lm(Ear_Hieght~Genotypes+(Block:Rep), data=complete_impute)####this model id fine
anova(lin)
#######test for normality
qqnorm(residuals(lin))#roughly straight line
plot(density(residuals(lin))) ##bell curve

?lmer
library(emmeans)
library(multcomp)

marginal_means<-emmeans(lin,"Genotypes")
comparisons <- glht(marginal_means, linfct = mcp(Genotypes = "Tukey"))
summary(comparisons)

comparisons <- glht(lin, linfct = mcp(Genotypes = "Tukey"))
summary(comparisons)
rm(PSmeans)
class(lin)
print(Tukey)
LSD.test(y=PSmeans,trt = "Genotypes")
?LSD.test
