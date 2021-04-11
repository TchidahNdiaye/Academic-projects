# CHARGEMENT DES LIBRAIRIES
library(gplots)
library(caTools)
library(prettyR)
library(binom)
library(Epi)
library(corrplot)
library(psy)
library(survival)


install.packages("scorecard")

# importation du fichier csv
smp <- read.csv2("smp1.csv")

# description de la base importée
str(smp)

# Affichage des premieres lignes de la base
head(smp)

# Essai d'importation d'un fichier de donner sans passer par le choose directory
# en utilisant directement le lien d'emplacement du fichier sur notre disque.
# j'ai pu constater que sur mon ordinateur, il faut doubler les antislash comme
# sur SAS. Et que la fonction read.csv2 est utilisé dans le cas où le séparateur 
# des collonnes de votre csv est un point virgule et on utilise read.csv lorsque 
# les variables sont séparées par une virgule.
bank <- read.csv("D:\\Documents\\Data Science\\03 Dataset\\bank-sample.csv")
str(bank)
head(bank)

# Affichage de l'effectif de chaque modélité
table(smp$prof)

barplot(table(smp$prof))

pie(table(smp$prof))

hist(smp$age)

hist(smp$age, col="grey", main="Histogramme des âges", xlab="âge")

# le caractère contigu des batons de l'histogramme montre qu'il y a une continuité
# dans les variables quantitatives utilisées.
boxplot(smp$age, xlab="âge")

boxplot(smp$age ~ smp$rs, ylab = 'age', xlab = 'Recherche de sensation',
        main="Distribution du niveau de sensation en fonction de l'âge")

plot(smp$age, smp$n.enfant, main="nombre d'enfants par âge des détenus", xlab="âge", ylab="nombre d'enfants")

# Il y a certains points qui sont supperposer, pour lever cela, on a:
plot(jitter(smp$age), jitter(smp$n.enfant), main="nombre d'enfants par âge des détenus", xlab="âge", ylab="nombre d'enfants")

# importation du deuxième dataset comportant des données temporelles
repdat <- read.csv2("outils_hdrs.csv")
str(repdat)
head(repdat)

# chargement des librairies
library(gplots)
library(caTools)

plotmeans(repdat$HDRS~repdat$VISIT, gap=0, barcol='black',
          xlab='nombre de visite', ylab='Score de dépression',
          main="Etat symptomatique moyen des patients par rapport aux visites")

# Nous voyons qu'au cours du temps, l'état symptomatique des patients s'améliore progréssivement.
interaction.plot(repdat$VISIT, repdat$NUMERO, repdat$HDRS, lty=1, legend=FALSE,
                 ylab='score de dépression', xlab='nombre de visites',
                 main="Etat symptomatique de chaque patient par rapport aux visites")

# Exercice de la partie théorique
taille <- c(1.71, 1.70, 1.65, 1.54, 1.70, 1.63, 1.81, 400, 1.81, 1.64)

summary(taille)

# Affichage des statistiques descriptives de notre base
summary(smp)

# Chargement de la librairie pour describe
library(prettyR)
# prettyR est le package R qui permet de générer les statisques descriptives
# sans passer par summary qui prend beaucoup de place.

describe(smp)

# Description de la base avec organisation de la disposition des indicateurs
describe(smp, num.desc=c("mean","sd","median","min","max","valid.n"))

mean(smp$age, na.rm = TRUE)   #Calculer seulement la moyenne d'une variable

sd(smp$age, na.rm=TRUE)    # Calculer uniquement l'écart-type d'une variable

table(smp$prof, deparse.level=2, useNA="always")
# la fonction table renvoie le tableau d'effectif d'une variable

# Importation du data set du lab
smplab <- read.csv2("smp2.csv")

# View(smplab) # ouvrir le fichier sans cliquer dessus   | mais cette commande n'est pas prise en compte dans jupyter
str(smplab)              # Affichage de la nature de chaque variable

names(smplab) # obtenir le nom des variables de la dataframe

summary(smplab)          # Affichage des statistiques 

# Statistique d'une seule variable
summary(smplab$age)

smplab$prof

# Affichage des 20 premières observations
smplab$age[1:20]

# Minimum de la série
min(smplab$age)

min(smplab$age,na.rm=TRUE)  # Affichage du minimum sans valeur manquante

smplab$abus[1:20]

unique(smplab$abus)

unique(smplab$prof)      # affichage des modalités

head(smplab$abus,n=20)

table(smplab$abus)

length(smplab$abus)     # dimension de la variable

nrow(smplab)            # nombre de ligne de notre dataset

table(smplab$abus, useNA='always') # pour prendre les données manquantes en compte.

summary(smplab$abus)

head(smplab$abus)

head(factor(smplab$abus))

# nous allons procéder à la création de la variable catégorielle abus
abus <- factor(smplab$abus, levels=c(0,1), labels=c("Non","Oui"))
table(abus)

table(abus,useNA="always")

names(smplab)

head(smplab$n.enfant)

summary(smplab$n.enfant)

table(smplab$n.enfant)

table(smplab$n.enfant > 4)

smplab$n.enfant.cat <- factor(smplab$n.enfant)          # création d'une variable sous forme de facteur
table(smplab$n.enfant)

levels(smplab$n.enfant.cat)                             # modalité du facteur

nlevels(smplab$n.enfant.cat) #permet de donner le nombre total de niveau d'une variable

# nous souhaitons à présent agréger l'effectif de tout ce qui vient après 5
levels(smplab$n.enfant.cat)[6:13] <- "5+"
table(smplab$n.enfant.cat)

sum(table(smplab$prof))

smp <- read.csv2("smp2.csv")
str(smp)

# 5. la borne supérieure de l'intervalle interquantile est 75.
summary(smp$dur.interv)

# Transformation de la variable sous forme de facteur
smp$n.fratrie2 <- factor(smp$n.fratrie)          
table(smp$n.fratrie2)

levels(smp$n.fratrie2)

# 6. Recoder n.fratrie en variable binaire : <5 et 5+
n.fratrie3 <- factor(smp$n.fratrie >=5, labels=c("<5","5+"))
table(n.fratrie3)

# 7. le nombre de lignes du tableau pour lesquelles ecole = 1, 2 ou 3 : 731
table(smp$ecole)

# 8. le nombre d'individus sans emploi = 222
table(smp$prof)

head(smp)

mean(smp$age[1:10])

str(smp)

summary(smp$dur.interv[1:300])

# Importation et description de la base
hopital <- read.csv2("satisfaction_hopital.csv")
str(hopital)
summary(hopital)

# Question 1 : Pourcentage des trois variables catégorielles (sexe, profession et recommander)
table(hopital$sexe)/length(hopital$sexe)*100   # sexe
                                               # les hommes sont de 50,2% contre 49,8% de femmes

table(hopital$profession, useNA='always')/length(hopital$profession)*100  # pourcentage des professions

table(hopital$recommander, useNA='always')/length(hopital$recommander)*100   # pourcentage des recommandations 

# Question 2 : Stats Synthétique (mon choix s'est porté sur les variables quantitatives réelles)
describe(hopital, num.desc=c("mean","median","sd","min","max","valid.n"))

summary(hopital$age)

prop.table(table(hopital$sexe))

sat1 <- hopital[c(3,5,6,7,8,9)]
describe(sat1, num.desc=c("mean","sd","median","min","max","valid.n"))

describe(hopital[c(3,8,9)], num.desc=c("mean","sd","median","min","max","valid.n"))

# Question 3 : histogramme du score de relation
hist(hopital$score.relation, col="grey", main="Histogramme du Score de relation", xlab="Score de relation")

# Question 4 : Distribution du score de relation chez les hommes et chez les femmes
boxplot(hopital$score.relation ~ hopital$sexe, xlab = 'Sexe', ylab = 'Score de relation',
        main="Distribution du score de relation chez les hommes et chez les femmes")

# importation des données du cours
smpc <- read.csv2("smp1.csv")
str(smpc)

describe(smpc$age)

moy <- mean(smpc$age, na.rm = TRUE)
et <- sd(smpc$age, na.rm = TRUE)
moy
et
int0 <- moy - 1.96 * et/sqrt(nrow(smpc) - 2)
int1 <- moy + 1.96 * et/sqrt(nrow(smpc) - 2)
print("L'intervalle de confiance est") 
round(int0,2)
round(int1,2)

binom.confint(3,10,method="all")              # intervalle de confiance pour un petit dataset

binom.confint(300,1000,method="all")             # intervalle de confiance pour un grand dataset

# exemple de calcul de la corrélation entre deux variables
cor(smpc$age, smpc$n.enfant, use="complete.obs")

# création de la variable binaire ed.b
smpc$ed.b <- ifelse(smpc$ed>2, 1, 0)
str(smpc)     # vérification de la bonne prise en compte de la création de la nouvelle variable

# Croisement des deux tables
table(smpc$ed.b, smpc$ed, deparse.level=2, useNA="always")

install.packages("Epi")   # INSTALLATION DE LA LIBRAIRIE

library(Epi)             # CHARGEMENT DE LA LIBRAIRIE

# exemple de calcul du risque rélatif et de l'odds 
twoby2(1-smpc$ed.b, 1-smpc$dep.cons)

# importation des données
smp <- read.csv2("smp2.csv")
str(smp)

edf <- factor(smp$ed)

# Question 1.
which(edf == 3)

# Question 2.
barplot(table(edf))

# Question 3.
help(subset)

# Création du sous ensemble
so <- subset(smp, prof=="sans emploi" | prof == "prof.intermédiaire" | prof == "cadre", c(age, n.enfant, prof))
head(so)
str(so)

# conversion de prof en varibale catégorielle
so$prof <- factor(so$prof)
# resumer du sous ensemble
summary(so)

# Verification des effectifs dans l'ensemble d'origine
table(smp$prof)

print("Calcul du nombre moyen d'enfant par profession")
aggregate(n.enfant ~ prof, so, mean)

boxplot(n.enfant ~ prof, so, main="Distribution du nombre d'enfant par profession")

help(lm)        # Lecture de la documentation sur cette fonction

# réalisation de l'ANOVA
m01 <- lm(n.enfant ~ prof, so)
m01

print("Tableau d'Analyse de Variance")
drop1(m01, test="F")

summary(m01)
# Summary n'est pas très adapté pour une ANOVA d'où il est préférable de se limiter à la fonction drop1()

# Essai de l'ANOVA sur le premier model
anova(m01)

# Génération d'une régression linéaire
m02 <- lm(n.enfant ~ age, so)
m02

summary(m02)

# les coefficient du modèle
coef(m02)

# interval de confiance du modèle
confint(m02)

help(confint)

# Analyse de la variance de notre modèle de régression linéaire
anova(m02)

# Prédiction sur de nouvelle variable
predict(m02, data.frame(age=c(20,30,40)), interval="confidence")

# Création d'une variable binaire à l'aide du nombre d'enfant
smp$n.enfant.bin <- ifelse(smp$n.enfant > 2, 1, 0)

# croisement des deux tables
table(smp$n.enfant.bin, smp$n.enfant, deparse.level=2, useNA="always")

# documentation sur la GLM
help(glm)

# modélisation 
m03 <- glm(smp$n.enfant.bin ~ age, smp, family=binomial("logit"))
summary(m03)

# QUESTION 4
print("4. Nombre moyen d'enfant par dep.cons")
aggregate(n.enfant ~ dep.cons, smp, mean)

help(aggregate)

# création de la base age strictement inférieur à 35 ans
q5 <- subset(smp, age < 35, c(age, duree, n.enfant, prof))
head(q5)
str(q5)

# QUESTION 5
print("5. Borne sup. de l'interval interquantile")  # la borne supérieur est 5.00
summary(q5$duree)

mean(smp$dur.interv[smp$suicide.past == 1], na.rm=TRUE)

# QUESTION 6
mean(smp[smp$suicide.past == 1, 'dur.interv'], na.rm=TRUE)

# Question 7
summary(smp$age)

# QUESTION 7 : L'effectif est 209
age4c <- cut(smp$age, c(19.0, 28.0, 37.0, 48.0, 83.0))
table(age4c)

# importation des données
smpc <- read.csv2("smp1.csv")
str(smpc)

# calcul du coefficient de correlation
cor(smpc$age, smpc$n.enfant, use="complete.obs")

#Tableau croiser des deux variables
table(smpc$ed.b, smpc$dep.cons, deparse.level=2, useNA="always")

# Stockage du tableau croisé 
tab <- table(smpc$ed.b, smpc$dep.cons, deparse.level=2)

# génération des pourcentage par ligne
prop.table(tab,1)

# génération des pourcentage par colonne
prop.table(tab,2)

# test du Chi-2 au sens de Pearson
chisq.test(smpc$ed.b, smpc$dep.cons, correct=FALSE)

# Pour des données peu voluminéux, nous procédons au test de fisher
fisher.test(smpc$ed.b, smpc$dep.cons)

hist(smpc$age, col="grey")

# Test de normalité à l'aide du QQ Plot
qqnorm(smpc$age); qqline(smpc$age)

# vérification de l'égalité des écart types
by(smpc$age, smpc$ed.b, sd, na.rm=TRUE)

# vérification de l'égalité des moyennes
by(smpc$age, smpc$ed.b, mean, na.rm=TRUE)

# vérification de l'égalité des variances
by(smpc$age, smpc$ed.b, var, na.rm=TRUE)

# Procédons au test de Studen maintenant
t.test(smpc$age ~ smpc$ed.b, var.equal=TRUE)

# Test de Mann-Whitney (Wilcoxon)
wilcox.test(smpc$age ~ smpc$ed.b)

# test sur les variables : age et recherche de sensation
cor.test(smpc$age, smpc$rs)

# Corrélation des rang de Spearman
cor.test(smpc$age, smpc$rs, method="spearman")

# Comparaison d'une moyenne à une référence
t.test(smpc$age, mu=24)

# importation des données
smp <- read.csv2("smp2.csv")
str(smp)

# Question 1.
smp$place <- factor(smp$place)
profession <- table(smp$prof, smp$place, deparse.level=2)      # nous allons stocker l'élément dans un vecteur
round(prop.table(profession, margin=1), 2)                     # pourcentage des effectifs en ligne

round(prop.table(profession, margin=2), 2)              # pourcentage des effectifs en colonne

# Question 2.
by(smp[c("duree", "place")], smp$place, mean)

dim(smp) # taille du dataframe

ncol(smp)  # nombre de colonne du dataframe

nrow(smp)   # nombre de ligne du dataframe

names(smp)

colnames(smp)

X1 <- data.frame(Id=1:4,SEXE=c("H","F","F","H"), Poids=c(75,68,48,72))
X2 <- data.frame(Id=1:4,SEXE=c("H","F","F","H"), Taille=c(182,165,160,178))
X1
X2

# FUSIONNER LES DEUX TABLES
cbind(X1,X2)

# la fonction utile pour les fusions de dataframe est :
merge(X1,X2)

# Question 2
with(smp, tapply(duree, prof, mean, na.rm=TRUE))

with(smp, tapply(duree, prof, sd, na.rm=TRUE))

# Question 3.
t.test(smp$age ~ smp$place, var.equal=TRUE)

t.test(age ~ place, data=smp)

# Question 4
by(smp$dur.interv, smp$dep.cons, median, na.rm=TRUE)

# Question 5
round(cor(smp$dur.interv, smp$age, use="complete.obs"), 3)

# Question 6
smp$suicide.past <- factor(smp$suicide.past)
wilcox.test(smp$dur.interv ~ smp$suicide.past)

# Chargement des données
smpl <- read.csv2("smp2.csv")                                        # Il s'agit toujours de smp2.csv
str(smpl)

# représentation graphique 
plot(smpl$age, smpl$dur.interv)

# représentation graphique - pivoter le graphique pour mettre en évidence les observations superposées
plot(jitter(smpl$age), jitter(smpl$dur.interv))

# droite de la régression
abline(lm(dur.interv ~ age, data=smpl))

help(abline)

# modèle de régression linéaire simple
mod1 <- lm(dur.interv ~ age, data=smpl)
summary(mod1)

# test de corrélation
cor.test(smpl$dur.interv, smpl$age)

# Lien entre la régression linéaire et le t test
mod2 <- lm(smpl$dur.interv ~ smpl$dep.cons, data=smpl)
summary(mod2)

t.test(smpl$dur.interv ~ smpl$dep.cons, var.equal=TRUE)

# modélisation
mod3 <- lm(dur.interv ~ age + dep.cons + subst.cons + scz.cons, data=smpl)
summary(mod3)

# 4e modèle en rajoutant la profession - ajout d'une variable catégorielle à plus de deux modalités
mod4 <- lm(dur.interv ~ age + dep.cons + subst.cons + scz.cons + prof, data=smpl)
summary(mod4)

# fixer la modalité de référence pour la variable qualitative à plusieurs modalités
smpl$prof <- relevel(smpl$prof, ref="ouvrier")
mod5 <- lm(dur.interv ~ age + dep.cons + subst.cons + scz.cons + prof, data=smpl)
summary(mod5)

drop1(mod5,.~.,test="F")

# Régression avec intérraction entre deux variables
mod6 <- lm(dur.interv ~ age + dep.cons*subst.cons + scz.cons, data=smpl)
summary(mod6)

# Procédons désormais à l'analyse de variance
mod7 <- lm(dur.interv~prof, data=smpl)
summary(mod7)

# tableau d'ANOVA
drop1(mod7,.~.,test="F")

# vérification des conditions de validité de la régression linéaire par la normalité du résidu
hist(resid(mod3), col="blue")

# modèle logistique avec une seule variable explicative
model1 <- glm(suicide.hr~abus, data=smpl, family="binomial")
summary(model1)

# calcul de l'exponentiel afin d'une meilleure interprétation
exp(coef(model1))

# calcul de l'odds-ratio
twoby2(1-smpl$suicide.hr, 1-smpl$abus)

model2 <- glm(suicide.hr~abus+discip+duree, data=smpl, family="binomial")
summary(model2)

exp(coefficients(model2))

exp(coef(model2))

drop1(model1,.~.,test="Chisq")

drop1(model2,.~.,test="Chisq")

drop1(model2, test="Chisq")

drop1(mod6,test="F")

exp(1.95)

predict(model2)

help(predict.glm)

# chargement de la data set
smp <- read.csv2("smp2.csv")
dim(smp)

# Question 4
q4 <- subset(smp, n.enfant>=4 & (prof=="sans emploi"|prof=="ouvrier"|prof=="cadre"|prof=="employé"), c(age, n.enfant, prof))
head(q4)
dim(q4)

by(q4$age, q4$prof, var, na.rm=TRUE)

table(q4$prof)

summary(q4$n.enfant)

281.2262/158
# La réponse à la question 4 est que le rapport est suppérieur à 4

# CONVERSION DES VARIABLES QUALITATIVES DE LA BASE
varcat <- c("discip","ecole","separation","juge.enfant","place","abus","dep.cons","ago.cons","ptsd.cons","alc.cons",
            "subst.cons","scz.cons","char","rs","ed","dr","suicide.s","suicide.hr","suicide.past")
smp[varcat] <- lapply(smp[varcat], factor)
str(smp)

describe(smp)

# QUESTION 6
model <- glm(separation~age, data=smp, family="binomial")
summary(model)

twoby2(smpl$separation, smpl$age)

twoby2(1-smpl$suicide.hr, 1-smpl$abus)

summary(smpl$n.fratrie)

# chargement des données
alc <- read.csv2("alcool.csv")
str(alc)

# librairie pour la courbe de Kaplan-Meier
library(survival)

plot(survfit(Surv(alc$t, alc$SEVRE) ~1), main="Courbe de maintien dans l'abstinence")

# Comparaison de deux groupes
plot(survfit(Surv(t, SEVRE) ~ SEXE, data=alc), col=c("black", "red"), main="Courbe de maintien dans l'abstinence")

# Médiane de survie
survfit(Surv(alc$t, alc$SEVRE) ~1)

# comparaison de deux moyenne en fonction du  / TEST DE LOG-RANK
survdiff(Surv(t, SEVRE) ~SEXE, data=alc)

# Modèle de Cox pour tester l'association de la survie à une variable quantitative
coxph(Surv(t, SEVRE) ~AGE, data=alc)

mod <- coxph(Surv(t, SEVRE) ~AGE+SEXE+EDVNEG, data=alc)
mod

# calcul des exponentielles des coefficients
exp(coef(mod))

par(mfrow=c(2,2))
plot(cox.zph(mod))

# retour à la base initiale smp
smpl <- read.csv2("smp2.csv")
str(smpl)

# calcul de la matrice de corrélation
var <- c("age","n.enfant","scz.cons","dep.cons","grav.cons","rs","ed","dr")
matrice <- round(cor(smpl[,var], use="complete.obs"), digits=3)
matrice

corrplot(matrice,, method="circle")

library(psy)

install.packages("psy")

# APPLICATION DE L'ACP SUR LES VARIABLES
var <- c("age","n.enfant","scz.cons","dep.cons","grav.cons","rs","ed","dr")
mdspca(smpl[,var])

help(mdspca)

help(sphpca)

# Représentation sphérique de la matrice de corrélation grâce à l'ACP
sphpca(smpl[,var])

sphpca(smpl[,var], v=55)

help(fpca)

# ACP autour d'une variable à expliquer
expliquer <- "grav.cons"
explicatives <- c("age","n.enfant","scz.cons","dep.cons","rs","ed","dr")
fpca(data=smpl, y=expliquer, x=explicatives, partial="No")

# CAH sur les variables
var <- c("age","n.enfant","scz.cons","dep.cons","grav.cons","rs","ed","dr")
cha <- hclust(dist(t(scale(smpl[,var]))), method="ward")
plot(cha)

# CAH sur les variables avec un heatmap
obj <- cor(smpl[,var], use="pairwise.complete.obs")
heatmap(obj)

# CAH sur les observations                                t() permet de classer les variables ; scale permet de CR
cah <- hclust(dist(scale(smpl[,var])), method="ward")
plot(cah)

# importation de la data set
alcool <- read.csv2("alcool.csv")
dim(alcool)
str(alcool)

# constitution du sous ensemble des moins et + de 50 ans
alcool$AGEgroup <- ifelse(alcool$AGE > 50, 1, 0)
table(alcool$AGEgroup)
table(alcool$AGEgroup, alcool$AGE)

# QUESTION 1.
survdiff(Surv(t, SEVRE) ~AGEgroup, data=alcool)

help(survdiff)

# toujours QUESTION 1 pour avoir quatre décimal à la p-value
coxph(Surv(t, SEVRE) ~AGEgroup, data=alcool)

# QUESTION 2
coxph(Surv(t, SEVRE) ~AGE+SEXE+AGE*SEXE, data=alcool)

# importation des données
satis <- read.csv2("satisfaction_hopital.csv")
dim(satis)
str(satis)

# Inspection des effectifs de la variable recommander
table(satis$recommander)

# Question 1 : Transformation de la variable recommander en binaire
satis$recommander.b <- ifelse(satis$recommander <=1, 0, 1)
table(satis$recommander.b, satis$recommander)

# Question 2 : Estimation de la force d'association entre recommander.b et sexe à l'aide de l'odds-ratio
twoby2(1-satis$recommander.b, 1-satis$sexe)
# L'intervalle de confiance est directement donnée dans les résultats de l'odds-ratio et est : [0.7169 ; 1.6383]

# Question 3 : Calcul de la corrélation de Pearson
cor.test(satis$score.relation, satis$age)

# Question 4 : Evaluation de la différence du score de relation entre les sexe
t.test(satis$score.relation, satis$age, var.equal=TRUE)
# Les résultats du test t de student montrent une différence significative entre la moyenne du score de relation chez les 
# hommes et chez les femmes. Pour inclure le test de validité de ce test, nous nous sommes servi de : var.equal=TRUE.

help(t.test)

# CODE à EXPORTER
# importation des données
satis <- read.csv2("satisfaction_hopital.csv")
dim(satis)
str(satis)

# Inspection des effectifs de la variable recommander
table(satis$recommander)

# Question 1 : Transformation de la variable recommander en binaire
satis$recommander.b <- ifelse(satis$recommander <=1, 0, 1)
table(satis$recommander.b, satis$recommander)

# Question 2 : Estimation de la force d'association entre recommander.b et sexe à l'aide de l'odds-ratio
twoby2(1-satis$recommander.b, 1-satis$sexe)
# L'intervalle de confiance est directement donnée dans les résultats de l'odds-ratio et est : [0.7169 ; 1.6383]

# Question 3 : Calcul de la corrélation de Pearson
cor.test(satis$score.relation, satis$age)

# Question 4 : Evaluation de la différence du score de relation entre les sexe
t.test(satis$score.relation, satis$age, var.equal=TRUE)
# Les résultats du test t de student montrent une différence significative entre la moyenne du score de relation chez les 
# hommes et chez les femmes. Pour inclure le test de validité de ce test, nous nous sommes servi de : var.equal=TRUE.

# QUESTION 1 : ESTIMATION DU MODELE DE REGRESSION LINEAIRE
mod1 <- lm(score.relation ~ age+sexe+score.information+amelioration.sante+amelioration.moral+profession+service, data=satis)
summary(mod1)

# QUESTION 2 : ESTIMATION DU MODELE DE REGRESSION LOGISTIQUE
mod2 <- glm(recommander.b ~ age+sexe+score.information+amelioration.sante+amelioration.moral+profession+service, 
            data=satis, family=binomial("logit"))
summary(mod2)

# CODE à EXPORTER
# importation des données
satis <- read.csv2("satisfaction_hopital.csv")
dim(satis)
str(satis)

# Inspection des effectifs de la variable recommander
table(satis$recommander)

# Question 1 : Transformation de la variable recommander en binaire
satis$recommander.b <- ifelse(satis$recommander <=1, 0, 1)
table(satis$recommander.b, satis$recommander)

# QUESTION 1 : ESTIMATION DU MODELE DE REGRESSION LINEAIRE
mod1 <- lm(score.relation ~ age+sexe+score.information+amelioration.sante+amelioration.moral+profession+service, data=satis)
summary(mod1)

# QUESTION 2 : ESTIMATION DU MODELE DE REGRESSION LOGISTIQUE
mod2 <- glm(recommander.b ~ age+sexe+score.information+amelioration.sante+amelioration.moral+profession+service, 
            data=satis, family=binomial("logit"))
summary(mod2)
