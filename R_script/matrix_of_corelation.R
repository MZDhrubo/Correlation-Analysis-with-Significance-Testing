#-------
# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables 
library(ggcorr)
install.packages('ggcorr')
install.packages("Hmisc")

library('Hmisc')
#load data
data_muscle <- readxl::read_excel('data/muscle_data.xlsx')

data_muscle$Location <- as.factor(data_muscle$Location)
res <- cor(data_muscle)

round(res, 2)
cor(data_muscle, use = "complete.obs")
res2 <- rcorr(as.matrix(data_muscle))      

res2<-rcorr(as.matrix(mtcars[,1:7]))
flattenCorrMatrix(res2$r, res2$P)
symnum(res, abbr.colnames = FALSE)

install.packages("corrplot")


library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)






