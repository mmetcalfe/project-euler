# 
# Problem 4: Largest palindrome product
# (Published on Friday, 16th November 2001, 06:00 pm; Solved by 178192)
# 
#     A palindromic number reads the same both ways. The largest
# 	palindrome made from the product of two 2-digit numbers is 9009 =
# 	91 × 99.
# 
#     Find the largest palindrome made from the product of two 3-digit
# 	numbers.

def is_palindrome(p):
    s = str(p)

    for i in range(0, len(s) // 2):
        #print(s[i], s[-i - 1])
        if(s[i] != s[-i - 1]):
            return False

    return True

max_palindrome = 0
for p in range(1, 1000):
    #print(p, end=': ')
    for q in range(1, p + 1):
        if(is_palindrome(p * q)):
            max_palindrome = max(p*q, max_palindrome)
            #print(p * q)
        #print(q, end='')
    #print()

print('Answer:', max_palindrome)