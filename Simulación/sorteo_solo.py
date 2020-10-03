from random import randint
l = []
urna = list(range(1,29))
for x in range(5):
    a = randint(0, len(urna)-1)
    l.append(urna[a])
    urna.pop(a)

l.sort() 

print(l)