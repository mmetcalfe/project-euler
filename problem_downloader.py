__author__ = 'mitchell'

import urllib.request
from bs4 import BeautifulSoup as Soup


f = urllib.request.urlopen("http://projecteuler.net/problem=1")
soup = Soup(f)

content = soup.select("#content")[0]

title = content.select("h2")[0]
print(title.string)

problemNumber = content.select("h3")[0]
print(problemNumber.string)

problemContent = content.select(".problem_content")[0]
print(problemContent)

