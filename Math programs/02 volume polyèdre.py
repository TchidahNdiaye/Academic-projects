# 3.3.10. Exercice UpyLaB 3.8
from math import sqrt
'''
Écrire un programme qui lit :
la première lettre en majuscule du nom du polyèdre ("T", "C", "O", "D" ou "I"),
la longueur de l’arête du polyèdre,
et qui imprime le volume du polyèdre correspondant.
Si la lettre lue ne fait pas partie des cinq initiales, le programme imprime le message "Polyèdre non connu".
'''
polyedre = input()
a = float(input())

if polyedre == "T":
    print((sqrt(2) / 12) * (a ** 3))
elif polyedre == "C":
    print(a ** 3)
elif polyedre == "O":
    print((sqrt(2) / 3) * (a ** 3))
elif polyedre == "D":
    print(((15 + (7 * sqrt(5))) / 4) * (a ** 3))
elif polyedre == "I":
    print(((5 * (3 + sqrt(5))) / 12) * (a ** 3))
else:
    print("Polyèdre non connu")
    
    
    #ijo
