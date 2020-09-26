from random import randint
import pandas as pd

def generar_sorteos():
    l = []
    for i in range(98280):
        urna = list(range(1,29))
        l.append([])
        for j in range(5):
            a = randint(0, len(urna)-1)
            l[i].append(urna[a])
            urna.pop(a)
            l[i].sort()
        print(l[i])
    return l

cols = 'V1 V2 V3 V4 V5'
df = pd.DataFrame(generar_sorteos(), columns = cols.split())
df.to_csv("chisp.csv", index = False, header = True)
