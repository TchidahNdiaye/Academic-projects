# -*-coding: Latin -1-*
'''
    PROGRAMME DE RESOLUTION D'UNE EQUATION DE SECOND DEGRE
    Auteur : Tchidah Ndiaye
    Date : Avril 2020
    But du programme :
    Ce programme python sert à calculer les résultats d'une équation du second degré.
    Entrée : Valeur des coefficients a, b et c de l'équation du sécond degré du type ax2 + bx + c.
    Sortie : Valeur du discriminant : Delta, valeurs des solutions imaginaires (nombre complexe)
             lorsque delta est négatif, de la solution unique lorsque delta est nul et des solutions
             pour delta positifs.
'''
import os                                                              # Importation des librairies

a = float(input("BIENVENUE DANS LE PROGRAMME DE RESOLUTION D'UNE EQUATION DE SECOND DEGRE \nEntrez la valeur du coefficient a : "))
b = float(input("Entrez la valeur du coefficient b : "))
c = float(input("Entrez la valeur du coefficient c : "))

delta = (b ** 2) - (4 * a * c)                                         # Calcul du discriminant delta
racine = delta ** 0.5

if delta < 0:
    print("L'équation ne possède pas solution réelle mais des solutions imaginaires z1 et z2.")
    z1 = (-b - complex(racine))/(2 * a)
    z2 = (-b + complex(racine))/(2 * a)
    print("z1 = ",z1,"\nz2 = ",z2)                                     # Affichage de la solution imaginaire
elif delta == 0:
    print("L'équation possède une racine (une solution unique)")
    x0 = (-b)/(2 * a)
    print("x = ", x0)                                                  # Affichage des résultats pour delta nul
else:
    print("L'équation possède deux racines distinctes x1 et x2")
    x1 = (-b - racine)/(2 * a)
    x2 = (-b + racine)/(2 * a)
    print("x1 = ",x1,"\nx2 = ", x2)                                     # Affichage des résultats pour delta positif
print("La valeur du discriminant est : ",delta,"\nEt sa racine carrée est : ",racine)
os.system("pause")
