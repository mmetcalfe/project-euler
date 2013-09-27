__author__ = 'mitchell'

import urllib.request
from bs4 import BeautifulSoup as Soup

import codecs
import os

# f = urllib.request.urlopen("http://projecteuler.net/problem=186")
# soup = Soup(f)

# print(soup)

# content = soup.select("#content")[0]
#
# title = content.select("h2")[0]
# # print(title.string)
#
# problemNumber = content.select("h3")[0]
# # print(problemNumber.string)
#
# problemContent = content.select(".problem_content")[0]
# # print(problemContent)
#
# for tag in problemContent:
#     if tag == '\n':
#         continue
#     print(tag)
#     print(tag.string)


def make_dirs_for_image(img):
    dirs = img.attrs['src'].split("/")
    dirs.pop()
    path = ""
    for d in dirs:
        path += d + "/"
    os.makedirs(path, exist_ok=True)


def save_problem_page(problem_number):
    f = urllib.request.urlopen("http://projecteuler.net/problem={}".format(problem_number))
    soup = Soup(f)

    with codecs.open("problem_pages/problem{}.html".format(problem_number), 'w', 'utf-8') as text_file:
        text_file.write(str(soup))

    # Download images:
    images = soup.select("img")
    for img in images:
        make_dirs_for_image(img)

        url = 'http://projecteuler.net/' + img.attrs['src']
        urllib.request.urlretrieve(url, img.attrs['src'])


for n in range(151, 438):
    print(n)
    save_problem_page(n)


