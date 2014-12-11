# 
# Problem 10: Summation of primes
# (Published on Friday, 8th February 2002, 06:00 pm; Solved by 132998)
# 
#     The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
# 
#     Find the sum of all the primes below two million.
# 
# <p class="info">Note: This problem has been changed recently, please
# 	check that you are using the right parameters.</p>
import math

n = 2000000

def sieve_of_eratosthenes():
    nums = []
    primes = []

    for k in range(1, (n//2) + 1):
        v = 2*k + 1
        nums.append(v)

    primes.append(2)

    while True:
        v = nums[0]

        if v > int(math.sqrt(n) + 1):
            for k in nums:
                primes.append(k)
            return primes

        primes.append(v)

        nums = [ k for k in nums if k % v != 0 ]


s = sieve_of_eratosthenes()

print(s)
print(sum(s))