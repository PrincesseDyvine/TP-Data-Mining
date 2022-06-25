---
title: "corrigé tp1"
output:
  pdf_document: default
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(FactoMineR)
data(decathlon)
View(decathlon)
```
#Q2

```{r}
str(decathlon) # str sert a afficher des données de manieres compactes
View(decathlon)
summary(decathlon)
```
#Q3
```{r}
library(plyr)
attach(decathlon)

ddply(decathlon, c("Competition"), summarise,    
      N = sum(Points), MOY_100m =mean('100m'))
```
#Q4 Normalisation
```{r}
decathlon2=scale(decathlon[,1:12]) # données normalisées= centrées réduites
View(decathlon2)
colnames(decathlon2)
X=cbind.data.frame(decathlon2, decathlon$Competition)# tout le jeu
# de données centré réduit
colnames(X)=colnames(decathlon) 
#cbind.data.frame sert grace a des arguments vectorielles, matricielles,ou de donnée de combiner respectivement par colones ou lignes
# $ sert a extraire des elements d'une liste en fonction de leur noms
```
Q5
```{r}
library(reshape2)
library(ggplot2)
dfmelt<-melt(X, measure.vars = 1:12)
View(dfmelt)
gr<-ggplot(dfmelt, aes(y=value, x=Competition,  fill=Competition))+
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~variable)
gr
```
#Q6
```{r}
library(corrgram)
cor(X[,-13])
corrgram(X[,-13], order=TRUE, lower.panel=panel.conf)
```

```{r}
ggplot(X,aes(x=`100m`, Y=`400m`))+geom_point()+
  geom_text(label=rownames(X))
```
# section 3 Q1
```{r}
Y=as.matrix(decathlon[,1:10])
g=colMeans(Y)
g
Y=sweep(Y,MARGIN=2,STATS=g,FUN="-")
View(Y)
round(colMeans(Y),3) # mynn nulle , on a bien centré le jeu de donnees 
Y=sweep(Y, MARGIN = 2,STATS = et, FUN = "/")
View(Y)
Y=as.data.frame(Y)
attach(Y)
apply(Y,2,function(x) sqrt(sum(x^2)/n)) #recalculer l ecart type
```
```{r}
View (decathlon)
sd(decathlon$`100m`)
n=nrow(Y) #nmbre individus
p=ncol(Y) #nmbre d variable
et=apply(Y,2, function(x) sqrt(sum(x^2)/n)) #calcul ecart type

```
```{r}
Y=sweep(Y, MARGIN = 2,STATS = et, FUN = "/")
View(Y)
Y=as.data.frame(Y)
attach(Y)
apply(Y,2,function(x) sqrt(sum(x^2)/n)) #recalculer l ecart type
```


```{r}
# construire la matrive v=Y'*D*Y
D=diag(rep(1/n,n))
n*n
View(D)
Y=as.matrix(Y)
V=t(Y)%*%D%*%Y # transposée
View(V)
  
```

```{r}
M=diag(rep(1,p))
View(M)
```

```{r}
Vs=eigen(M%*%V)
Vs
```
```{r}
lambda=Vs$values # valeurs propres
lambda
Vp=Vs$vectors # vecteurs propres
Vp
c=Y%*%Vp # matrices des composantes principales
View(c)
coord_ind= c # les coordonnees des individus sont les valeurs de la matrice c
```

```{r}
library(FactoMineR)
colnames(decathlon)
res.pa=PCA(decathlon,quanti.sup = 11:12,quali.sup = 13)
```
Q2+Q3
```{r}
plot.PCA(res.pa, choix="ind") # nuage de points =representation graphique des individus
plot.PCA(res.pa,choix="var") # cercle de correlation= representation

# etudes des individus nuages de points
res.pa$ind$coord # les coordonnes des indi sur les axes de(C programme)
# sur le nuage de points
res.pa$ind$cos2 # coordonner des indiviidus
res.pa$ind$coord2 # cos2
res.pa$ind$contrib # la contribution de chaque individu a la construction de l'axe , sur le nuage de points



res.pa2=PCA(decathlon,quanti.sup = 11:12,quali.sup = 13, axes = c(3,4))
```
# etude des inercies nuages de points

```{r}
(res.pa$eig)# les valeurs propres
# inertie totale =la sommes des valeurs propres =somme
# variance d'une dimensions sa valeur propre /inertie totale
sum(res.pa$eig[,1])# inertie totale=somme des valeurs propres
# variance de la deuxieme dimension
(res.pa$eig[2,1]/sum(res.pa$eig[,1]))*100
# plan factoriel: muni de deux dimensions
# premier plan factoriel : plan muni de dim1 et dim2
# critere de kaiser :retenir les composants  ayant des valeurs propres>1 (lambda > 1): on retient  les 4 premeiere composantes 
# critère  de coude: considerer  les composnats juste avant le baissemnent ,le decrochement du coude , la chute
# 
```
# critere de coude
```{r}
barplot(res.pa$eig[,1], main = "eblouis des val propres", names.arg = paste("eig", 1:nrow(res.pa$eig)))


# l'eblouis de val propre ne represente aucun decrochement(chute,cassure ) on va consider les critres Kaiser
```
le graphique n'affiche aucun decrochement (ne pa considere le decrochement de la primiere composante)

```{r}
#etude des variables
res.pa$var$coord# coordonnees des variables sur les axes du cercle de corelation
res.pa$var$contrib # la contribution de chak variable a la construction de l'axe , cercle"
res.pa$var$cos2 # qualite de representation permet de synthetiser 
res.pa$var$cor # la corelation entre la dimension est la variable est expliquée par le cis de l'angle entre eux  d

plot.PCA(res.pa, choix ="var")
#la correlation entre la dimension est la variable expliquée par le cos de l'angle entre eux
# une variable bien represente si elle est proche du bord de cercle
# une variable mal representé est prochee du centre du crcle
res.pa=PCA(decathlon,quanti.sup = 11:12, quali.sup = 13)
res.pa=PCA(decathlon,quanti.sup = 11:12, quali.sup = 13, axes =c(2,3)) # 2eme plan factoriel
res.pa=PCA(decathlon,quanti.sup = 11:12, quali.sup = 13, axes =c(3,4))  # 3eme plan factoriel
```

on arrive a construire de nouvel variabmles synthetique
- dim1 qui evalue la performance en vitesse (course) des athlets 
-dim2 qui evaluela performance de force des athlet
on peut faire une analogie de nuage diindividus et cercl de corelation (caracterisation ,la typologie et profilage des individus par rapport aux variables et dimensions )

Q13 et Q14
```{r}
plot.PCA(res.pa, choix="ind" , habillage=13 , ellipse=ellipse.coord)
plotellipses(res.pa) # les ellipses de confiance sont tracees autour de la barycentre des individus de chaque categorie. ici les ellipses chevauchent (superposés), donc il n y pas une typologie entrre les caracteristiques(performance) des individus, ressemblance des performances des athlete...