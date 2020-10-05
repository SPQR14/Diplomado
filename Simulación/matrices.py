import pandas as pd 

df = pd.read_csv("chisp.csv", sep = ',', encoding='utf8')

v1 = list(df['V1'])
v2 = list(df['V2'])
v3 = list(df['V3'])
v4 = list(df['V4'])
v5 = list(df['V5'])

def matriz_de_transicion(transiciones):
    n =  max(transiciones) + 1
    print(n)
    M =[[0]*n for _ in range(n)]
    for (i,j) in zip(transiciones, transiciones[1:]):
        M[i][j] += 1
    for r in M:
        s = sum(r)
        if s > 0:
            r[:] = [f/s for f in r]
    return M

def imprime_matriz(m):
    for i in range(len(m)):
        for j in range (len(m[i])):
            print("%.4f" % m[i][j], end = ' ')
        print()

m1 = matriz_de_transicion(v1)
#imprime_matriz(m1)

m2 = matriz_de_transicion(v2)
imprime_matriz(m2)
