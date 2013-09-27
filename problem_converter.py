__author__ = 'mitchell'

from bs4 import BeautifulSoup as Soup
import codecs


def soup_for_problem(number="1"):
    f = codecs.open("problem_pages/problem%s.html" % number, 'r', 'utf-8')
    return Soup(f)


soup = soup_for_problem(1)

content = soup.select("#content")[0]

title = content.select("h2")[0]
print(title.string)


problemContent = content.select(".problem_content")[0]
print(problemContent)

# for tag in problemContent:
#     if tag == '\n':
#         continue
#     print(tag)
#     print(tag.string)