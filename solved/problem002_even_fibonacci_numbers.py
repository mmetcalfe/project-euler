__author__ = 'mitchell'


n = int(4e6)

f1 = 0
f2 = 1
s = 0

while f1 < n:
    if f1 % 2 == 1:
        s += f1

    f2 += f1
    f1, f2 = f2, f1

print(s)

