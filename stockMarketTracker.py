#!/usr/bin/python
# -*- coding: latin-1 -*-



##### imports
import urllib
from bs4 import BeautifulSoup
import re
import Tkinter




def getWebsite(link):
    """
    Open a website, read the file, and return the HTML
    :param link: website URL
    :return: website HTML
    """
    f = urllib.urlopen(link)
    website = f.read()

    return website


def getText(website, pattern):
    """
    Take a website's HTML and parse the text.
    Then find specific text within the website and return the text.

    :param website: HTML for a website
    :return: string of text
    """

    text = BeautifulSoup(website,'html.parser')

    list = []
    for i in text.find_all('span', class_= pattern):
        list.append(i.get_text())

    #cleanup text

    return list



# Main
if __name__ == "__main__":

    #hard coded link
    link = "https://www.msn.com/en-us/money/indexdetails/fi-30.10.%21DJI.30.%24INDUV"

    #get website HTML
    website = getWebsite(link)

    #get text from the website
    labels = getText(website,re.compile('^name'))
    values = getText(website, re.compile('^value baseminus'))

    #join labels and values together in list
    data_list = []
    for i in range(len(labels)):
        data_list.append([labels[i],values[i]])

    fulltext = 'Stock Market Values for Today: \n'
    for i in range(len(data_list)):
       fulltext += data_list[i][0].ljust(20) + " " + str(data_list[i][1]) + "\n"

    #do calculations
    open = float(re.sub(",","",data_list[0][1]))
    high = float(re.sub(",","", data_list[6][1]))
    low = float(re.sub(",","", data_list[7][1]))

    percentage_high =  open / high #want close to 0 as possible
    percentage_low =  low / open #want as close to 1 as possible
    percentage_increase = (open - low)/ (high - low)  #want as close to 0 as possible


    calculation_text = "\nCalculations:\nOpen/High: %f\nLow/Open: %f\nPercent_Increase: %f" % (percentage_high, percentage_low,percentage_increase)

    if percentage_increase > .90:
        buy_text = "\n\n\nBuy?: No"
    elif percentage_increase <= .90 and percentage_increase > .80:
        buy_text = "\n\n\nBuy?: Maybe"
    else:
        buy_text = "\n\n\nBuy?: YES!!!!" #less than .80

    complete_text = fulltext + calculation_text + buy_text

    #complete_text = fulltext.join(calculation_text)

    #show text box
    root = Tkinter.Tk()
    T = Tkinter.Text(root, height=30, width=60)
    T.pack()
    T.insert(Tkinter.END, complete_text)
    Tkinter.mainloop()