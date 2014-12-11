# 
# Problem 6: Sum square difference
# (Published on Friday, 14th December 2001, 06:00 pm; Solved by 194702)
# 
#     The sum of the squares of the first ten natural numbers is,
# 1^{2} + 2^{2} + ... + 10^{2} = 385
#     The square of the sum of the first ten natural numbers is,
# (1 + 2 + ... + 10)^{2} = 55^{2} = 3025
#     Hence the difference between the sum of the squares of the first
# 	ten natural numbers and the square of the sum is 3025 − 385 = 2640.
# 
#     Find the difference between the sum of the squares of the first
# 	one hundred natural numbers and the square of the sum.

n = 100
nums = range(n + 1)

sum_squares = sum([k ** 2 for k in nums])
squared_sum = sum([k for k in nums]) ** 2


print('Sum of squares:', sum_squares)
print('Square of sum:', squared_sum)
print('Difference:', squared_sum - sum_squares)