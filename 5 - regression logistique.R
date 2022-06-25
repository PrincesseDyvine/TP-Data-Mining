cancerprostate <- read.csv("/Users/brody/OneDrive/Bureau/DYVINE/SEMESTRE 2/DATA MINING/cancerprostate.txt", sep=";")
View(cancerprostate)
str(cancerprostate)


library(dplyr)
glimpse(cancerprostate)
cancerprostate= mutate(cancerprostate, rayonx=as.factor(rayonx))

cancerprostate= mutate(cancerprostate, taille=as.factor(taille))
cancerprostate= mutate(cancerprostate, grade=as.factor(grade))
cancerprostate= mutate(cancerprostate, Y=as.factor(Y))
glimpse(cancerprostate)
str(cancerprostate)
for(i in 3:6){
  cancerprostate[,i] <- as.factor(cancerprostate[,i])
}
str(cancerprostate)
summary(cancerprostate)







#exploration des donnees: liens entre variables

library(ggplot2)
ggplot(cancerprostate, aes(x=Y, y=age))+geom_boxplot(aes(fill=Y))
#l'age n'influence pas l'atteinte du cancer au reseau lymphatique
#ca peut dependre d'autres facteurs autre que l'age
ggplot(cancerprostate, aes(Y, log.acid))+geom_boxplot(aes(fill=Y))
#le lien entre la variable acid et lymph est fort
#plus le log acide augmente plus le risque d'atteinte du cancer aux lymphatique augmente



#construction d'un modele
#expliquer Y sur la base de log.acid
modele_log.acid <- glm(Y~log.acid, data=cancerprostate, family=binomial) #binomial p(Y=1) succes, p(Y=0) echec
summary(modele_log.acid)



#les valeurs estimees de p(x)
modele_log.acid <- glm(Y~log.acid, data=cancerprostate,
                       family=binomial) 
#objectif: changer en introduisant differentes valeurs de log acid on aura pour chacune la probabilite p(x) estimee correspondante
#p(x)= exp(o.404+ 2.24*logacid)//(1+exp(0.404+2.24*logacid))
beta0=coef(modele_log.acid)[1]
beta1=coef(modele_log.acid)[2]
x=seq(min=-2, max=2,by=0.1)
p=exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))
p
summary(cancerprostate)
plot(x,p,type="l",xlab="log.acid",ylab="p(x)",
     main="valeurs estimees de p(x) our le model_log.acid")
abline(h=0.5, lty=2)

#foward proportion : selection de variables
# modele trivial reduit a la constante
str_constant <-"~1"
#medele complet incluant toutes les variable explicatives potentiels
str_all <- "~age+acide+rayonx+taille+log.acid"
#
library(MASS) 
modele <- glm(Y ~ 1, data = cancerprostate, family = binomial) # modle reduit a la constnte: aucune variable explicative
# 
summary(modele)
modele.forward <- stepAIC(modele, scope = list(lower = str_constant, uper = str_all), trace = TRUE, data= cancerprostate, direction = "forward")

# on part de toutes les variables disponibles et on enleve au fur et a mesure les variables non explicatives
model_complet<- glm(Y ~., data = cancerprostate, family = binomial) # modele complet 

modele.backward <- stepAIC(model_complet, ~. , trace= TRUE, data= cancerprostate, direction = "backward")
# modele selectionné y~ acide + rayon + taille + log.acid

# MODELE FINALE
modele_selectionne <- glm(Y ~ acide + rayonx + taille + log.acid, data = cancerprostate, family = binomial)
summary(modele_selectionne)

# prevision
# ECHANTILLON TEST ET APPRENTISSAGE
# on va splitter notre data en echantillon d'apprenstissage(training) de 80% et un echantillon test de 20%
# pour un objectif explicative(analyse, extraction...), on fait la modelisation sur toute la base.
# pour un objectif predictif : on split notre data(training, test)

x <- sample(c(1:nrow(cancerprostate)), 0.8*nrow(cancerprostate))
x
training= cancerprostate[x,]
training
test=cancerprostate[-x,]
test
test2=test[,-6]

model_train <- glm(Y ~ ., data=training, family = binomial)

summary(model_train)
model_select_train = stepAIC(model_train, ~. , trace= TRUE, data= training, direction = "backward")
# resultat : Y~ rayonx+ taille + log.acid
model-sel-train=glm(Y ~ taille + log.acid + rayonx + acide, data=training, family = binomial)


pred_test <- predict(model_select_train, newdata = test2, type ="response")
pred_test
set.seed(123)

# PREDICTION
View(cancerprostate)
#quand on fournit l'echantillon test:
n_donnees <- data.frame(matrix(c(61,0.60,1,0,1,-0.51,49,0.86,0,0,1,-0.15,67,0.72,1,0,1,-0.33,51,0.95,1,1,1,-0.05),ncol=6,byrow = T))

names(n_donnees)<- names(cancerprostate[-6])
n_donnees
str(n_donnees)

for (i in 3:5) n_donnees[,i]<- factor(n_donnees[,i])
str(n_donnees)

prevision <- predict(model_select_train,newdata = n_donnees, type = "response")
prevision
previson >0.5

# pour les deux premiers individus , les probas predites sont inf a
#0.5 ,on predira donc Y=0 , donc le cancer n'a pas atteint le reseau lymphatique
#tandis que pour les deux derniers on predira que le cancer a atteint le reseau lymphatique