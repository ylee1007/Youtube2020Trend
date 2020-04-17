import requests
import pandas as pd
from bs4 import BeautifulSoup
import csv
import os

## check file path
#os.getcwd()
#print(os.listdir(os.getcwd()))

## read the data
#df = pd.read_excel (r'list_mode_export_fox.xlsx')
df = pd.read_excel (r'list_mode_export_msnbc_final.xlsx')
numView50 = []

#print (df)

## loop through the file ####
#for index, row in df.iterrows():
#	url = row["Original Url"]
#	soup = BeautifulSoup(requests.get(url).text, 'lxml')
#	view = soup.select_one('meta[itemprop="interactionCount"][content]')['content']
#	numView.append(view)

## or this code work as well
#for link in df["Original Url"]:
#	soup = BeautifulSoup(requests.get(link).text, 'lxml')
#	view = soup.select_one('meta[itemprop="interactionCount"][content]')['content']
#	print(view)
#	numView.append(view)

# loop 50 rows
for x in range(0,50):
	url = df["Original Url"][x]
	soup = BeautifulSoup(requests.get(url).text, 'lxml')
	view = soup.select_one('meta[itemprop="interactionCount"][content]')['content']
	print(view)
	numView50.append(view)
print(numView50)

#for x in range(100,150):
#	url = df["Original Url"][x]
#	soup = BeautifulSoup(requests.get(url).text, 'lxml')
#	view = soup.select_one('meta[itemprop="interactionCount"][content]')['content']
#	print(view)
#	numView150.append(view)
#print(numView150)

#for x in range(150,200):
#	url = df["Original Url"][x]
#	soup = BeautifulSoup(requests.get(url).text, 'lxml')
#	view = soup.select_one('meta[itemprop="interactionCount"][content]')['content']
#	print(view)
#	numView200.append(view)
#print(numView200)
	
## check for individual
#url = 'https://www.youtube.com/watch?v=-GKAG3orkbI'

#soup = BeautifulSoup(requests.get(url).text, 'lxml')

#print(soup.select_one('meta[itemprop="interactionCount"][content]')['content'])

