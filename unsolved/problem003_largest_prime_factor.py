# 
# Problem 3: Largest prime factor
# (Published on Friday, 2nd November 2001, 06:00 pm; Solved by 194923)
# 
#     The prime factors of 13195 are 5, 7, 13 and 29.
# 
#     What is the largest prime factor of the number 600851475143 ?
# 
# Note: This problem has been changed recently, please check that you
# 	are using the right number.

import math

def is_prime(n):
    if n < 2:
        return False

    for k in range(2, min(n, int(math.sqrt(n)) + 2)):
        if n % k == 0:
            return False
    return True

# Find the set of prime factors of a given integer.
def prime_factors(n):
    factors = []
    m = n
    k = 0
    while k < m:
        k += 1
        if not is_prime(k):
            continue

        if m % k == 0:
            while m % k == 0:
                factors.append(k)
                m /= k
                #print(k)

    return factors

answer_factors = prime_factors(600851475143)

# Print answer:
print('Answer:', max(answer_factors))

# Check answer:
prod = 1
for v in answer_factors:
    prod *= v
print('Check: prod(', answer_factors, ') -> ', prod, sep='')