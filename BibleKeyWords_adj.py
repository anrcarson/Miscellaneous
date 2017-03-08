#!/usr/bin/python
# -*- coding: utf-8 -*-

#get Bible keywords from book and each chapter

##### imports
import urllib
from bs4 import BeautifulSoup
import re
from watson_developer_cloud import AlchemyLanguageV1
import time
import json
import codecs
import Tkinter
import tkFileDialog

def getWebsite(link):
    """
    Open a website, read the file, and return the HTML
    :param link: website URL
    :return: website HTML
    """
    #system pause - 2 seconds
    time.sleep(2)

    #read website
    f = urllib.urlopen(link)
    website = f.read()

    return website


def getText(website):
    """
    Take a website's HTML and parse the text.
    Then find specific text within the website and return the text.

    :param website: HTML for a website
    :return: string of text
    """

    text = BeautifulSoup(website,'html.parser')

    list = []
    for i in text.find_all('span', class_=re.compile('^text ')):
        list.append(i.get_text())
    full_text = ''.join(list)

    #cleanup text
    full_text = re.sub('\d','',full_text)
    full_text = re.sub('\[\D\]','',full_text)

    return full_text


def getKeywords(full_text):
    """
    Pass a text string to the Alchemy API to analyze it for keywords and relevance.

    :param full_text: a text string
    :return: a JSON string of keywords and relevance
    """

    alchemy_language = AlchemyLanguageV1(api_key='your key here')
    text_json = alchemy_language.keywords(text = full_text)

    keywords_json = text_json["keywords"]   # keep only keywords, relevance

    return keywords_json


def printKeywords(keywords_json):
    """
    Print list of 10 keywords and relevance from JSON string

    :param keywords_json: a JSON string of keywords and relevance
    """
    print "Keyword".ljust(20) + "Relevance"
    for i in keywords_json:
            print i["text"].ljust(20) + i["relevance"]


def getKeywordsList(keywords_json, keyword_list, relevance_list, book_list, book,counter,counter_list):
    for i in keywords_json:
        keyword_list.append(i["text"])
        relevance_list.append(i["relevance"])
        book_list.append(book)
        counter_list.append(counter)

def getLink(book, chapter):
    book_adj = re.sub(" ","%20",book)
    link = "https://www.biblegateway.com/passage/?search=" + book + "+" + str(chapter) + "&version=NRSV"

    return link


def getBookChapters():

    book_chapters = [

        # Old Testament
          ["Genesis",50]
        , ["Exodus",40]
        , ["Leviticus", 27]
        , ["Numbers", 36]
        , ["Deuteronomy", 34]
        , ["Joshua", 24]
        , ["Judges", 21]
        , ["Ruth", 4]
        , ["1 Samuel", 31]
        , ["2 Samuel", 24]
        , ["1 Kings", 22]
        , ["2 Kings", 25]
        , ["1 Chronicles", 29]
        , ["2 Chronicles", 36]
        , ["Ezra", 10]
        , ["Nehemiah", 13]
        , ["Tobit", 14]
        , ["Judith", 16]
        , ["Esther", 10]
        , ["1 Maccabees", 16]
        , ["2 Maccabees", 15]
        , ["Job", 42]
        , ["Psalm", 150]
        , ["Proverbs", 31]
        , ["Ecclesiastes", 12]
        , ["Song of Solomon", 8]
        , ["Wisdom", 19]
        , ["Sirach", 51]
        , ["Isaiah", 66]
        , ["Jeremiah", 52]
        , ["Lamentations", 5]
        , ["Baruch", 5]
        , ["Ezekiel", 48]
        , ["Daniel", 12]
        , ["Hosea", 14]
        , ["Joel", 3]
        , ["Amos", 9]
        , ["Obadiah", 1]
        , ["Jonah", 4]
        , ["Micah", 7]
        , ["Nahum", 3]
        , ["Habakuk", 3]
        , ["Zephaniah", 3]
        , ["Haggai", 2]
        , ["Zechariah", 14]
        , ["Malachi", 4]

        #New Testament
        , ["Matthew", 28]
        , ["Mark", 16]
        , ["Luke", 24]
        , ["John", 21]
        , ["Acts", 28]
        , ["Romans", 16]
        , ["1 Corinthians", 16]
        , ["2 Corinthians", 13]
        , ["Galations", 6]
        , ["Ephesians", 6]
        , ["Phillipians", 4]
        , ["Colossians", 4]
        , ["1 Thessalonians", 5]
        , ["2 Thessalonians", 3]
        , ["1 Timothy", 6]
        , ["2 Timothy", 4]
        , ["Titus", 3]
        , ["Philemon", 1]
        , ["Hebrews", 13]
        , ["James", 5]
        , ["1 Peter", 4]
        , ["2 Peter", 3]
        , ["1 John", 5]
        , ["2 John", 1]
        , ["3 John", 1]
        , ["Jude", 1]
        , ["Revelation", 22]

    ]

    return book_chapters

# Main
if __name__ == "__main__":

    book_chapters =  getBookChapters()

    for i in book_chapters:

        book_text = ''

        for j in range(i[1]):

            #get link
            link = getLink(i[0],j+1)

            #get website HTML
            website = getWebsite(link)

            #get text from the website
            book_text += "\n" + getText(website)

            print i[0] + " " + str(j+1)

        #return JSON dictionary of keywords and relevance
        keywords_json = getKeywords(book_text)

        #save book_text file
        book_filename = "C:\%s_text.txt" % (i[0])
        output = open(book_filename, 'w')
        output.write(book_text.encode('utf-8'))
        output.close()

        #save keywords_json
        keyword_filename = "C:\%s_keywords.txt" % (i[0])
        output = open(keyword_filename, 'w')
        output.write(str(keywords_json))
        output.close()

    keyword_list = []
    relevance_list = []
    book_list = []
    counter = 0
    counter_list = []

    #get all keywords into list
    for i in book_chapters:
        filename = "C:\%s_keywords.txt" % (i[0])
        output = codecs.open(filename, 'r', 'utf-8')
        keywords = output.read()

        #clean up keywords before json
        keywords = re.sub("u\'","\"",keywords)
        keywords = re.sub("\'", "\"", keywords)
        keywords = re.sub(r"\u2019", "", keywords)
        keywords = re.sub(r"\xa0", "", keywords)
        keywords = re.sub(r"xa0", "", keywords)
        keywords = re.sub(r"\u2018", "", keywords)
        keywords = re.sub(r"\\", "", keywords)

        #convert to json
        keywords_json = json.loads(keywords)

        #create lists
        getKeywordsList(keywords_json, keyword_list, relevance_list, book_list, i[0],counter,counter_list)
        counter+=1


    output_list = []
    for i in range(len(keyword_list)):
        output_list.append([counter_list[i],book_list[i],keyword_list[i],relevance_list[i]])

    # convert to string and reformat for writing out.  CSV format.
    outputString = str(output_list)
    outputString = re.sub("\[", "", outputString)
    outputString = re.sub("\],", "\n", outputString)
    outputString = re.sub("\]", "", outputString)
    outputString = re.sub(" ", "", outputString)
    outputString = re.sub("'", "", outputString)

    # write file
    root = Tkinter.Tk()
    root.withdraw()
    outputFilename = tkFileDialog.asksaveasfilename(parent=root)
    output = open(outputFilename, 'w')
    output.write(outputString)