import textwrap

__author__ = 'mitchell'

import codecs

from bs4 import BeautifulSoup as Soup


def soup_for_problem(number=1):
    f = codecs.open('problem_pages/problem%s.html' % number, 'r', 'utf-8')
    return Soup(f)


def make_filename(number=1, title='Multiples of 3 and 5'):
    t = title.lower().replace(' ', '_')

    return "problem{0:03}_{1}.py".format(number, t)


def write_to_file(text_file, line='', indent=0, cont=False, end='\n'):
    comment = line
    if not cont:
        comment = '# {0}{1}'.format(' ' * indent * 4, comment)

    text_file.write('{0}{1}'.format(comment, end))
    print(comment, end=end)


def write_p_tag(element, indent, text_file):
    write_to_file(text_file)
    write_to_file(text_file, end='')
    cont = True
    end = ''
    for tag in element:
        write_element_to_file(text_file, tag, indent, cont, end)
    write_to_file(text_file, cont=True)


def process_tag_img(element, text_file):
    img_src_ = element.attrs['src']
    img_alt_ = element.attrs['alt']
    symbols = {
        'images/symbol_le.gif',
        'images/symbol_le.gif',
        'images/blackdot.gif',
        'images/bracket_left.gif',
        'images/bracket_right.gif',
        'images/icon_lock.png',
        'images/icon_rss.png',
        'images/pe_banner.png',
        'images/pe_banner_light.png',
        'images/spacer.gif',
        'images/symbol_asymp.gif',
        'images/symbol_cong.gif',
        'images/symbol_ge.gif',
        'images/symbol_gt.gif',
        'images/symbol_implies.gif',
        'images/symbol_le.gif',
        'images/symbol_lfloor.gif',
        'images/symbol_lt.gif',
        'images/symbol_maps.gif',
        'images/symbol_minus.gif',
        'images/symbol_ne.gif',
        'images/symbol_plusmn.gif',
        'images/symbol_radic.gif',
        'images/symbol_rfloor.gif',
        'images/symbol_sum.gif',
        'images/symbol_times.gif',
    }
    if img_src_ in symbols:
        write_to_file(text_file, img_alt_, cont=True, end='')
    else:
        write_to_file(text_file)
        write_to_file(text_file, element)


def write_element_to_file(text_file, element, indent=0, cont=False, end='\n'):
    if element == '\n':
        return

    if element.string != None:
        write_to_file(text_file, element.string, indent, cont, end)
        return

    tagname = element.name
    if tagname == "div":
        pass
    if tagname == "p":
        write_p_tag(element, indent, text_file)
        return
    elif tagname == "img":
        process_tag_img(element, text_file)
        return
    else:
        pass

    for tag in element:
        write_element_to_file(text_file, tag, indent, cont, end)


def convert_div_to_string(element):
    str = ""

    for tag in element:
        str += convert_element_to_string(tag)

    return str


def convert_img_to_string(element):
    img_src_ = element.attrs['src']
    img_alt_ = element.attrs['alt']
    symbols = {
        'images/symbol_le.gif',
        'images/symbol_le.gif',
        'images/blackdot.gif',
        'images/bracket_left.gif',
        'images/bracket_right.gif',
        'images/icon_lock.png',
        'images/icon_rss.png',
        'images/pe_banner.png',
        'images/pe_banner_light.png',
        'images/spacer.gif',
        'images/symbol_asymp.gif',
        'images/symbol_cong.gif',
        'images/symbol_ge.gif',
        'images/symbol_gt.gif',
        'images/symbol_implies.gif',
        'images/symbol_le.gif',
        'images/symbol_lfloor.gif',
        'images/symbol_lt.gif',
        'images/symbol_maps.gif',
        'images/symbol_minus.gif',
        'images/symbol_ne.gif',
        'images/symbol_plusmn.gif',
        'images/symbol_radic.gif',
        'images/symbol_rfloor.gif',
        'images/symbol_sum.gif',
        'images/symbol_times.gif',
    }
    if img_src_ in symbols:
        return img_alt_
    else:
        return '\n{}'.format(element)


def convert_h2_to_string(element):
    return '\n{}\n'.format(element.string)


def convert_sub_to_string(element):
    return '_{{0}}'.format(convert_div_to_string(element))

def convert_sup_to_string(element):
    return '^{{0}}'.format(convert_div_to_string(element))


def convert_p_to_string(element):
    return '\n\t{}\n'.format(convert_div_to_string(element))


def convert_tr_to_string(element):
    return '{}\n'.format(convert_div_to_string(element))


def convert_td_to_string(element):
    return '\t{0:>7}'.format(convert_div_to_string(element))


def convert_element_to_string(element):
    if element == '\n':
        return ""

    tagname = element.name
    if tagname == None:
        return element
    if tagname == 'div':
        return convert_div_to_string(element)
    elif tagname == 'img':
        return convert_img_to_string(element)
    elif tagname == 'h2':
        return convert_h2_to_string(element)
    elif tagname == 'h3':
        return convert_h2_to_string(element)
    elif tagname == 'span':
        return convert_div_to_string(element)
    elif tagname == 'p':
        return convert_p_to_string(element)
    elif tagname == 'table':
        return convert_p_to_string(element)
    elif tagname == 'tr':
        return convert_tr_to_string(element)
    elif tagname == 'td':
        return convert_td_to_string(element)
    elif tagname == 'sub':
        return convert_sub_to_string(element)
    elif tagname == 'sup':
        return convert_sup_to_string(element)
    elif tagname == 'br':
        return '\n'
    else:
        raise Exception('Unrecognised tagname: \'{}\''.format(tagname))


def convert_problem_page_to_python_file(problem_number=1):
    problem_soup = soup_for_problem(problem_number)
    content_div = problem_soup.select('#content')[0]

    problem_title = content_div.select('h2')[0].string
    filename = make_filename(problem_number, problem_title)

    problem_content = content_div.select('.problem_content')[0]
    print(filename + ': ')
    with codecs.open('unsolved/{}'.format(filename), 'w', 'utf-8') as text_file:
        #write_element_to_file(text_file, content_div)
        problem_str = convert_element_to_string(content_div)

        lines = problem_str.splitlines(False)
        for line in lines:
            wrapped_text = textwrap.wrap(
                text=line,
                width=70 - 2,
                tabsize=4,
                subsequent_indent='\t')
            if len(wrapped_text) == 0:
                write_to_file(text_file)
                continue
            for wrapped_line in wrapped_text:
                write_to_file(text_file, wrapped_line)


convert_problem_page_to_python_file(186)
