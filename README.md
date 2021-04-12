# Youtube2020Trend
Stat479 data science project

## Executive Summary
This report analyzes and examines the overall 2020 topic trends from Fox and MSNBC’s Youtube channel, and the relationship between which channel it is and the words in a video’s title. The method of analysis divides into two big sections, unsupervised learning and supervised learning. Unsupervised learning includes monthly trend detection, bigram analysis, and hierarchical clustering. The model is used to predict the channel from the words from the title using penalized logistic regression, a method of supervised learning. The data analysis draws attention to the fact that “Trump” was the main topic for both Fox and MSNBC channels in 2020. Further investigation reveals that each channel has a different nuance towards the word “Trump”. The report finds the relationship between title and channel is significant, in particular, with the word “Trump”. The model is built to predict either Fox and MSNBC channels from the video title that has “Trump”, and its prediction accuracy is about 75%. The major areas of weakness require more investigation for longer periods.

Run the following code on RStudio's console
```
library(shiny)
runGitHub("Youtube2020Trend", "younanna", subdir = "Shinyapp")
```
