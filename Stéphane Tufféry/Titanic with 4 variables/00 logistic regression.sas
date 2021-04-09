*********** 01 Description de la base ***********;
proc contents data=work.titanic;
run;

proc means data=work.titanic nmiss n mean std;
	title "Inspection des données manquantes";
run;

proc freq data=work.titanic;
	tables Age Class Sex Survived;
	title "1. Effectif de chaque modalité";
run;

proc freq data=work.titanic;
	tables ( Age Class Sex) * Survived;
	title "2. Probabilité de survie en fonction de chaque caractéristique";
run;

proc freq data=work.titanic;
	tables Age * Class * Survived;
	title "2.1. Tableau de contingence de survie en fonction de l'âge et la classe";
run;

*********** 02 Modélisation d'une régression logistique ***********;
proc logistic data = work.titanic;
	model Survived (ref="0") = Class Age Sex
	/ selection=stepwise RSQUARE;
	title "3. Premier modèle de régression pour la sélection de variable";
run;

proc logistic data = work.titanic;
	class Class / param=ref;
	model Survived (ref="0") = Class Age Sex
	/ selection=stepwise RSQUARE;
	title "4. Deuxième modèle de sélection de variable";
run;

* Création d'une variable indicatrice pour palier à l'interprétabilité 
des coefficients de la variable class;

data titanic;
	set work.titanic;
	class0 = (class = 0);
	class1 = (class = 1);
	class2 = (class = 2);
	class3 = (class = 3);
run;

proc logistic data=work.titanic;
	model Survived (ref="0") = class0 class1 class2 class3 Age Sex
	/ selection=stepwise rsquare;
	title "5. Troisième modèle : avec des variables indicatrices pour class";
run;

proc logistic data=work.titanic;
	model Survived (ref="0") = class0 | class1 | class2 | class3 | Age | Sex
	/ selection=stepwise rsquare;
	title "6. Modèle avec les interactions";
run;

proc logistic data=work.titanic;
	model Survived (ref="0") = class0 | class1 | class2 | class3 | Age | Sex
	/ hierarchy=none selection=stepwise rsquare;
	title "7. Modèle permettant à une variable d'apparaître dans une
			interaction sans apparaître elle-même directement";
run;

proc logistic data=work.titanic;
	model Survived (ref="0") = class0 | class1 | class2 | class3 | Age | Sex @2
	/ hierarchy=none selection=stepwise rsquare;
	title "8. Modèle permettant à une variable d'apparaître en spécifiant de ne 
	 pas tester les interactions d'ordre au plus de 2 (@2 à la fin)";
run;

ods excel file="/home/u44791576/thesis/ML Coursera/modèle final titanic.xlsx"
		options(embedded_titles="yes");
ods graphics on;
proc logistic data=work.titanic plots=all;
	model Survived (ref="0") = class0 class3 Sex class2*Age class3*Sex Age*Sex
	/ rsquare;
	title "Modèle le plus performant";
run;
ods excel close;

*********** 03 Modèle logistique avec un échantillon test et d'apprentissage ***********;
data titanic;
	set work.titanic;
	if ranuni(0) < 0.66 then cible = Survived;
run;

proc freq data=work.titanic;
	table cible;
run;

proc logistic data=work.titanic;
	model cible (ref="0") = class0 | class1 | class2 | class3 | Age | Sex @2
	/ hierarchy=none selection=stepwise rsquare;
	output out=modele predicted=proba;
	title "9. Modèle sur l'échantillon d'entraînement";
run;

/* %AUC(modele,Survived,proba); */
/* proc npar1way wilcoxon data=&data correct=no; */
/* 	where cible = .; */
/* 	Class &target; */
/* 	Var &score; */
/* 	title "10. Modèle sur l'échantillon test"; */
/* run; */