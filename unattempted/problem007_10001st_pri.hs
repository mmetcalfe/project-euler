# 
# Problem 7: 10001st prime
# (Published on Friday, 28th December 2001, 06:00 pm; Solved by 165770)
# 
#     By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
# 	we can see that the 6th prime is 13.
# 
#     What is the 10 001st prime number?


import solved.problem003_largest_prime_factor as problem3

def nth_prime(n):
    p = 1
    m = 0
    k = 0
    while m < n:
        k += 1
        if problem3.is_prime(k):
            p = k
            m += 1
    return p


if __name__ == "__main__":
    print(nth_prime(10001))
    pass