#requirements
import numpy as np
import math
import pandas as pd

#définition de la probabilité binomiale
def binom(k,i,lbd):
    if i >= k and k >= 0:
        x = math.comb(i,k)*pow(lbd,k)*pow(1-lbd,i-k)
    else :
        x = 0
    return x
    

# Calcul des probas
def somme(i,j,lbd,mu):
    s = 0
    for k in range(i+1):
        s =  s + binom(k,i,lbd)*binom(k-j+i,i,mu)
    return(s)

def somme2(i,N,lbd,mu):
    s = 0
    for k in range(i+1):
        for l in range(N):
            s =  s + binom(k,i,lbd)*binom(k-l+i,i,mu)
    return(1-s)


# Matrice de transition de (X_n) pour N=2
def MT(N, lbd, mu):
    Matrice=np.zeros((N+1,N+1))
    Matrice[0,0] = 1
    for k in range(1,N+1):
        for j in range(N):
            Matrice[k,j] = somme(k,j,lbd,mu)
        Matrice[k,N] = somme2(k,N,lbd,mu)
    return Matrice


# Temps moyen de retour en 0 pour N = 2
def T_moy(M):
    V0 = 0
    V1 = 1 / (1 - M[1,2]) + (M[1,1] * (1 - M[1,2] + M[2,1])) / (( 1 - M[1,2]) * ( 1 - M[1,2] - M[2,2] + M[1,2] * M[2,2] - M[1,1] * M[2,1]))
    V2 = (1 - M[1,2] + M[2,1]) / ( 1 - M[1,2] - M[2,2] + M[1,2] * M[2,2] - M[1,1] * M[2,1])
    Vi = [V0, V1, V2]
    print(f'\n V0 = {Vi[0]}',f'\n V1 = {Vi[1]}', f'\n V2 = {Vi[2]}')
    return Vi


# Noms des lignes et colonnes du dataframe
col = ["Test 1", "Test 2", "Test 3", "Test 4", "Test 5"]
ind = ["Lambda", "Mu", "Temps moy : Etat 1", "Temps moy : Etat 2"]

# Données du dataframe
Lambda = [0.5, 0.3, 0.2, 0.7, 0.9]
Mu = [0.5, 0.6, 0.2, 0.2, 0.05]
Temps_moy_Etat_1 = [T_moy(MT(2, 0.5, 0.5))[1], T_moy(MT(2, 0.3, 0.6))[1], T_moy(MT(2, 0.2, 0.2))[1], T_moy(MT(2, 0.7, 0.2))[1], T_moy(MT(2, 0.9, 0.05))[1]]
Temps_moy_Etat_2 = [T_moy(MT(2, 0.5, 0.5))[2], T_moy(MT(2, 0.3, 0.6))[2], T_moy(MT(2, 0.2, 0.2))[2], T_moy(MT(2, 0.7, 0.2))[2], T_moy(MT(2, 0.9, 0.05))[2]]
data = np.array([Lambda, Mu, Temps_moy_Etat_1, Temps_moy_Etat_2])

# Dataframe
Df = pd.DataFrame(data = data, columns = col, index = ind)
