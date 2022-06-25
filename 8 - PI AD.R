library(readxl)
datafinal <- read_excel("C:/Users/brody/OneDrive/Bureau/DYVINE/SEMESTRE 2/DATA MINING/datafinal.xlsx")
View(datafinal)
str(datafinal)
colnames(datafinal)

# modification de la data
data2=datafinal[,-c(1,13,14)]
str(data2)

data2$company=as.factor(data2$company)
data2$level=as.factor(data2$level)
data2$title=as.factor(data2$title)
  data2$location=as.factor(data2$location)
  data2$yearsofexperience=as.numeric(data2$yearsofexperience)
  
  data2$yearsatcompany=as.numeric(data2$yearsofexperience)
  data2$yearsatcompany=as.numeric(data2$yearsatcompany)
  data2$`yes or no`=as.factor(data2$`yes or no`)
  str(data2)
  
  # Analyse multivariée (ACP sur données quantitatives)
  library(tidyr)
 datanum=data2[,c(4,6:10)]
 datanum=datanum %>% drop_na()
dim(datanum)
  library(FactoMineR)
  
  PCA(datanum)

  # hierarchique
  
  
  # Clustering = segmentation
  
 kc= kmeans(datanum, 2)
 classekm=kc$cluster
 
 clValid(classekm)
 
 library(factoextra)
 fviz_cluster(list(data=datanum,cluster=classekm))

 
 # Arbre de décision
library(rpart.plot)
Tree=rpart( `yes or no` ~.,data=data2)

rpart.plot(Tree)

REG Logic
library(dplyr)
data2=mutate(data2,`yes or no`=as.factor(`yes or no`))

glimpse(data2)
str(data2)
library(ggplot2)
ggplot(cancerprostate, aes(x=Y, y=age))+geom_boxplot(aes(fill=Y))
                  