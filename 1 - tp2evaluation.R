temperature=read.csv("C:/Users/brody/OneDrive/Bureau/DYVINE/SEMESTRE 2/DATA MINING/temperature.csv" , sep =";" , row.names=1 ) 
View(temperature)
temperature
summary(temperature)
str(temperature)

# QUESTION 3
library(FactoMineR)
pca.temperature=PCA(temperature , ind.sup = 24:35 ,quanti.sup=13:16,quali.sup = 17)
# ls variables sont fortement relies a la dim 1

#QUESTION 4
plot.PCA(pca.temperature,choix='ind',habillage=17)
library(plyr)
library(factoextra)
#les temperatures des pays du sud sont très eleves contrairement a ceux du nord

fviz_pca_ind(pca.temperature,habillage=17)

# QUESTION 5,6
dimdesc(pca.temperature)
pca.temperature$eig
# on garde les deux composantes principales pour les valeurs propres qui sont superieur a 1
# dim1 represente la temperature annuelle
#dim 2 represente la longitude et amplitude elevées 

# QUESTION 7,8
pca.temperature$ind
pca.temperature$ind.sup

# QUESTION 9
which.max(round(pca.temperature$ind$contrib[,1],2))#l’individu ayant le plus contribué à la formation du premier axe est
which.min(round(pca.temperature$ind$contrib[,1],2))
#l’individu ayant le plus contribué à la formation du premier axe est 

# QUESTION 10
which.max(round(pca.temperature$ind$contrib[,2],2))
which.min(round(pca.temperature$ind$contrib[,2],2))

# QUESTION 11,12
round(pca.temperature$ind$cos2[,1]+pca.temperature$ind$cos2[,2],3)

# QUESTION 13
pca.temperature$var

# QUESTION14:
#dim1 : October=0.99 ; Dim2:June=0.55


# QUESTION 15 ET 16
pca.temperature$quanti.sup
pca.temperature$quali.sup

# QUESTION 17
cor(temperature[1 :23,1 :16])

# QUESTION 18

# QUESTION 19