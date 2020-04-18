import requests
import pandas as pd
from bs4 import BeautifulSoup as bs
import csv
import os

# check file path
# os.getcwd()
# print(os.listdir(os.getcwd()))

df = pd.read_excel(r'list_mode_export_fox.xlsx')
numDate = []

for x in range(2800, 2880):
    url = df["Original Url"][x]

    content = requests.get(url)
    # create beautiful soup object to parse HTML
    soup = bs(content.content, "html.parser")
    # date when the video was published
    temp_publishDate = soup.find("strong", attrs={"class": "watch-time-text"}).text
    publishDate = temp_publishDate[5:]
    numDate.append(publishDate)

print(numDate)




#url = df["Original Url"][0]
# print(url)
# download HTML code
#content = requests.get(url)
# create beautiful soup object to parse HTML
#soup = bs(content.content, "html.parser")
# date when the video was published
#temp_publishDate = soup.find("strong", attrs={"class": "watch-time-text"}).text
#publishDate = temp_publishDate[5:]
#print(publishDate)