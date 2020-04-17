import requests
import pandas as pd
from bs4 import BeautifulSoup as bs
import csv
import os

# check file path
# os.getcwd()
# print(os.listdir(os.getcwd()))

df = pd.read_excel(r'list_mode_export_fox.xlsx')

numView67 = []

for x in range(2000, 2100):
    url = df["Original Url"][x]

    content = requests.get(url)
    # create beautiful soup object to parse HTML
    soup = bs(content.content, "html.parser")
    # date when the video was published
    temp_publishDate = soup.find("strong", attrs={"class": "watch-time-text"}).text
    publishDate = temp_publishDate[5:]
    numView67.append(publishDate)

print(numView67)




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