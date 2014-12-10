# 
# Problem 9: Special Pythagorean triplet
# (Published on Friday, 25th January 2002, 06:00 pm; Solved by 145671)
# 
#     A Pythagorean triplet is a set of three natural numbers, a < b <
# 	c, for which,
#   a^{2} + b^{2} = c^{2}
#
#     For example, 3^{2} + 4^{2} = 9 + 16 = 25 = 5^{2}.
# 
#     There exists exactly one Pythagorean triplet for which a + b + c
# 	= 1000.
# Find the product abc.



def pythag_triplet(n):
    for b in range(1, s + 1):
        #print('b', b, end=' ')
        for a in range(1, b + 1):
            c = s - a - b
            if c <= 0:
                continue
            if a**2 + b**2 == c**2:
                return a, b, c

            #print([a, c], end=' ')
        #print()

s = 1000
A, B, C = pythag_triplet(1000)

print(A, B, C)
print(A**2 + B**2, C**2)

print(A*B*C)