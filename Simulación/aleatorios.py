import pandas as pd 
import random

cols = ['Cuadrados centrales', 'Congruencial', 'Congruencial mixto', 'Python random']
df = pd.DataFrame(columns = cols)
a = 2 ** (16)
a += 3
r0 = 7
m = 2**31
c = 21
cuadrados_centrales = 2081

def extraer_centrales(r):
    if len(r) == 8:
        return (int(r[2:6]))
    elif len(r) == 7:
        r = '0' + r
    elif len(r) == 6:
        r = '0'*2 + r
    elif len(r) == 5:
        r = '0'*3 + r
    elif len(r) == 4:
        r = '0'*4 + r
    elif len(r) == 3:
        r = '0'*5 + r
    else:
        return 0
    return (int(r[2:6]))

for i in range(100):
    cuadrados_centrales = extraer_centrales(str(cuadrados_centrales**2))
    congruencial = a * r0 % m
    r0 = congruencial
    congruencial_mixto = congruencial + c
    python = random.random()
    x = f'{cuadrados_centrales/10000} {congruencial/m} {congruencial_mixto/m} {python}'
    d = dict(zip(cols, x.split()))
    df = df.append(d, ignore_index = True)
    pass

df.to_csv("Practica_2_3_4.csv", encoding='utf8')
