import requests
import pandas as pd
from bs4 import BeautifulSoup as bs
import csv
import os

# check file path
# os.getcwd()
# print(os.listdir(os.getcwd()))

df = pd.read_excel(r'list_mode_export_msnbc_final.xlsx')
url = df["Original Url"][2759]
content = requests.get(url)
# create beautiful soup object to parse HTML
soup = bs(content.content, "html.parser")
# print(soup)
# date when the video was published
# temp_publishDate = soup.find("strong", attrs={"class": "watch-time-text"}).text
temp_publishDate = soup.find("strong", attrs={"class": "watch-time-text"})
# print(temp_publishDate)
temp_publishDate = temp_publishDate.text
publishDate = temp_publishDate[5:]
print(publishDate)

numDate = []

for x in range(2700, 2758):
    url = df["Original Url"][x]

    content = requests.get(url)
    # create beautiful soup object to parse HTML
    soup = bs(content.content, "html.parser")
    #print(soup)
    # date when the video was published
    # temp_publishDate = soup.find("strong", attrs={"class": "watch-time-text"}).text
    temp_publishDate = soup.find("strong", attrs={"class": "watch-time-text"})
    #print(temp_publishDate)
    temp_publishDate = temp_publishDate.text
    publishDate = temp_publishDate[5:]
    numDate.append(publishDate)

print(numDate)
