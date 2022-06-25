library(rpart)
library(rpart.plot)

data("ptitanic")
View(ptitanic)

str(ptitanic)
summary(ptitanic)
dim(ptitanic)

# arbre de decision
Tree <- rpart(survived~. , data = ptitanic)

# dans la formule ~. indique l'explication de survived(target en fonction de toutes les autre)
# classe survived --> 1 (reussite)
# classe died --> 0 (echec)
rpart.plot(Tree)
# l'arbre de de decision se base sur des modeles

# optimiser l'arbre de decision(minimisation de l'erreur)


Tree <- rpart(survived~. , data = ptitanic)
Tree$cptable # tableau de complexite
# on optimise notre arbre en minimisant
# les parametre optimaux sont:
# xerrorminimal = 0.528
#  parametre CP (complexite): minimal= 0.01000000
# correspondant a nsplit=9

plot(Tree) 
plotcp(Tree)# graphque cp avec error
# ligne cp correspondant a xerror min

TOptimal <- rpart(survived~. , data = ptitanic, cp=0.010000, control = rpart.control(minsplit = 9))
Toptimal = rpart(survived~., data=ptitanic, cp = optcp , control= rpart.control(minsplit = optnsplit))

Tree$cptable
which.min(Tree$cptable[,4]) # parcourir 4eme colonne er chercher la val min= num ligne  de xerror min
optcp= Tree$cptable[which.min(Tree$cptable[,4]),1] # la cp minimal
optnsplit=Tree$cptable[which.min(Tree$cptable[,4]),2]



Toptimal <- rpart(survived~. , data= ptitanic, cp=optcp, control = rpart.control(minsplit = optnsplit)) # arbre optimal
rpart.plot(Toptimal)

# PREVISION
x <- sample(c(1:nrow(ptitanic)), 0.8*nrow(ptitanic))
x
# la fonction sample permet de choisir aleatoiremnt un echantillon du jeu de donnes (soit 80% de notre data)

# spliter dataset en train and test
training=ptitanic[x,]# afficher uniquement l'chantillon x (80% de la data)
View(training)

test1=ptitanic[-x,-2] # les 20% restant, et l'echantillon test sans la colonne survived 

predtest.prob = predict(Toptimal, test ,type="prob")
predtest.prob


predtest.class=predict(Toptimal,test,type="class")
predtest.class

predtest.class!=test1$survived
erreur<-sum((predtest.class!=test1$survived)/nrow(test1))
erreur

error= (MC[2,1]+MC[1,2])
predtest.class!= test1survived # designer les differences entre les predictions de mon modele et les donnes reeles
# la difference=erreur
