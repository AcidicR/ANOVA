#costum function
library(gridExtra)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat) 
  p.mat
}
M<-cor(numeric_data1)
p.mat<-cor.mtest(M)
title <- "significant correlation(<.05)of traits"
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         diag=FALSE,tl.col= "black",
         type="upper", order="hclust",
         title=title, 
         addCoef.col = "black",addCoefasPercent = TRUE, # Add coefficient of correlation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         mar=c(0,0,1,0)
)

?corrplotins
install.packages()
library(tidyverse)
#datacamp practice
x <- c(2, 43, 27, 96, 18)
min(x)
max(x)
which.max(x)
which.min(x)
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time_1hr<-round(time/60,3)
speed<-distance/time_1hr
table<-data.frame(name=name,speed=speed,time_1hr=time_1hr)
subset(table,name=="Amy")
mean(murder_rate)
which.max(murders$population)
south<-murders$region=="South"
alright<-murder_rate<16
index<-which(south&alright)
murders$state[index]
c("West","North","East")%in%murders$region
?
rm(s)
sum(s)
my_states <- murders %>% 
  mutate(rate=total/population*100000, 
         rank=(rank(-rate)))%>% filter(region %in% 
             c("Northeast","West")&rate<1) %>% 
  select (state,region,rate,rank)
