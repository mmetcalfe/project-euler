#
# Problem 1: Multiples of 3 and 5
# (Published on Friday, 5th October 2001, 06:00 pm; Solved by 324709)
#
#     If we list all the natural numbers below 10 that are multiples
# 	of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
#
#     Find the sum of all the multiples of 3 or 5 below 1000.

print(sum([k for k in range(1000) if k % 3 is 0 or k % 5 is 0]))

