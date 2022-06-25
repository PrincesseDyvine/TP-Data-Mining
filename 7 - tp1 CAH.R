

V1=c(1.8,0.4,2.24,0.51,-0.59)
V2=c(-0.08,-1.08,-0.62,-0.09,0.03)
x=(cbind.data.frame(V1,V2))
x

d=dist(x,method="euclidian")#matrice de similarité ou matrice de distance
d
v=hclust(d,method="ward.D")
v

plot(v,hang = -1)
v$dist.method #metrique de calcule de distance pour matrice similarité
v$method #c'est la methode d'agregation
v$height #les distances a chaque niveau d'agregation
v$order
v$merge

#JEU DE DONNES DECATHLON
library(FactoMineR)

data("decathlon")

View(decathlon)
X=decathlon[,1:10]#variables actives
d1=dist(scale(X),method="euclidian")

hc1=hclust(d1,method="ward.D")
hc1

plot(hc1, hang=-1)
hc1$merge
hc1$height
hc1$order
hc1$labels
hc1$method
hc1$call
hc1$dist.method

sort(hc1$height,decreasing=TRUE)
plot(sort(hc1$height,decreasing=TRUE),type="b")
plot(sort(hc1$height,decreasing=TRUE),type="h")
#k=2,k=4,k=6 3 chutes

#VALIDATION DU NOMBRE DE CLUSTER: CHOISIR LE NBR OPTIMALE
library(clValid)
intern=clValid(X,2:6,clMethods = c("hierarchical"),validation="internal")
summary(intern)

#donc on suggére k=2

classes<-cutree(hc1,k=2)
classes
table(classes)
plot(hc1,hang=-1)
rect.hclust(hc1,k=2,border="red")
#rajout de la classe d'affectation de CAH de chaque individu en tant que variable
decathlon.classes<-cbind.data.frame(decathlon,classes)
View(decathlon.classes)
colnames(decathlon.classes)[14]<-"Classe.cah"
res.pca<-PCA(decathlon,quanti.sup = 11:12,ncp = Inf,graph=F,quali.sup = 13)

library(factoextra)
fviz_cluster(list(data=X,cluster=classes))
str(decathlon.classes)

#DISCRIPTION DES CLASSES
#la fonction catdes permet de trier les var quanti de la plus caractérisante a la moins caractérisante en positi
#(v.test> 0, mean in category>overall mean)
decathlon.classes$Classe.cah=as.factor(decathlon.classes$Classe.cah)
res.cat1=catdes(decathlon.classes,num.var=14)
View(decathlon.classes)
res.cat1

#Algoritme k-means
# inter-classe(maximiser)
#intra-classe(minimiser)

data("iris")
View(iris)
summary(iris)
# nmbre de cluster k=3 fixé a priori
#k-nmbre de noyeau initiaux= centre choisis aleatoirement

# on retire species car c'est une variable quali
kc=kmeans(iris[,-5],centers = 3, iter.max = 1000) #on augmente le nombre d'iteration pour stabiliser
kc$size # effectif de chaque classe
kc$centers # centre de classes par rapport a chaque variable
kc$cluster # affectation de chak individus par classe
kc$withinss # variables intra-classes
table(iris$species)
table(kc$cluster)

conf=table(iris$Species,kc$cluster)# matrice de confusion
error=(conf[2,1]+conf[3,3])/sum(conf)
accuray=1-error

plot(iris[,-5], col=kc$cluster)
# il y'a une erreur dans la classification qui correpond aux points verts et roses

kc1=kmeans(iris[,-5], centers = 2, iter.max = 10000)
plot(iris[,-5], col=kc1$cluster)

library(clValid)
intern4=clValid(iris[,-5],2:6,clMethods = c("kmeans"), validation= "internal")
summary(intern4)

# dans le cas de jeu de donnes iris k=2 est le nmbre de cluster optimal

library(rbokeh)
library(plotly)
figure(width = NULL, height = NULL ) %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris, color = Species)

# application kmeans

classes.km <- kmeans(X, centers = 2, iter.max = 1000)
classes.km$cluster
classes.km$size
classes.km$centers
View(decathlon.class)


decathlon.class=cbind.data.frame(decathlon.class, as.factor(classes.km$cluster))
colnames(decathlon.class)
colnames(decathlon.class)[15] <-"classe.kmeans"

res.cat3 = catdes(decathlon.class, num.var = 15)
summary(res.cat3)
# choix de la methode et du nmbre de clusters
intern5=clValid(X[,-5],2:6,clMethods = c("hierarchical","kmeans"), validation= "internal")


summary(intern5)
# dans le cas de jeu de données decathlon, CAH est plus performant que kmeans avec koptimal=2