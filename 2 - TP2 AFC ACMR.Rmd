---
title: "Untitled"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
tab<-matrix(c(908,869,901,619,1307,1008,1035,612,73,107,80,177,642,408,140,209,360,336,311,298,435,494,504,281), ncol=6)
rownames(tab)<-c("Prim","Second","Tech","Sup")
colnames(tab)<-c("Radio","Tele","QuotNat", "QuotReg", "PrMag","PrTV")
sum(tab)
View(tab)
```
# profil ligne: etude selon le niveau d'etude
```{r}
addmargins(tab,1)
prop.table(addmargins(tab,1),1) # 2eme argument margins
prof_lig=addmargins(100*prop.table(addmargins(tab,1),1),2)
prof_lig
```
```{r}
#profil colonne: selon type media
addmargins(tab,2)
prop.table(addmargins(tab,1),2)
prof_col=addmargins(100*prop.table(addmargins(tab,2),2),1)
prof_col
```
```{r}
#test ki2
test=chisq.test(tab)
# significatif

```
#application significativ 
# exploration des relation : AFC

```{r}
library(FactoMineR)
res.ca<-CA(tab, graph = T)
res.ca$row
res.ca$col
```
```{r}
plot.CA(res.ca, invisible="col") # representation modalites niveau d'etude
plot.CA(res.ca , invisible = "row") # representation des modalites media
#les modalites du niveau d'etude sont ordonnés
# plus j'augemnte sur la dimension 1 plus j'augemnte en niveau
#donc dim1=niveau d'etude
# sur la dim1 on passe d'un nivau regional a national
# la dim1 oppose des media auto-visuels(tele, radio..) aux media presse(mag)
plot.CA(res.ca)
#les enfants et les adolescents s'interessent plutot a la tele qu'a la lecture
```
```{r}
# etude de l'inertie
res.ca$eig
res.ca$eig
barplot(res.ca$eig[,2], names=paste("Dim",1:nrow(res.ca$eig)))
# nombre de dimension de l'acp= nombre de dimensions de la variable active
# l'information de la dimension s devient plus importante
#nbre dim= q-p+1 (6-4)+1
# selo, coude on retient les deux premiere dimension a savoir dim1 et dim2
# il y'a une chute entre dim2 et dim3

```
```{r}
# representation via factoextra
library(factoextra)
fviz_ca(res.ca)
fviz_ca_col(res.ca) # graph media
fviz_ca_row(res.ca) # graph niveau d'etude
```
#ACM : base canines(purement qualitative)
```{r}
canines <- read.table("canines.txt",header = T, row.names = 1)
View(canines)
for(i in 1: ncol(canines))
  {
  canines[,i]=as.factor(canines[,i])
  }
```

```{r}
library(FactoMineR)
# analyse des correspondances multiple ACM
canines.acm <- MCA(canines, quali.sup = 7)

```
#etude de l'inertie
```{r}
canines.acm$eig
plot(canines.acm$eig[,1], type="b", main ="Screen plot")
# le nombre de modalités actives est 16( sans moderation des modalités des variables supplemantaires)
# le nombre de variables actives =6
# le nbre de dimensions est 16-6=10 =nbre modal actives -nbre var actives
# on retenir deuc dimension
# selon coude la chute se trouve entre la 2eme et la 3eme dimensions
```

