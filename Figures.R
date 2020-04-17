#Load Libraries
library(tuber)
library(magrittr)
library(tidyverse)
library(purrr)

client_id <-"299305249029-0uha158f1vhohapgoht41ip6t42d12n1.apps.googleusercontent.com"
client_secret <- "v0R80iHC_pgRfn0jD6VnuMC_"

yt_oauth(app_id = client_id,
         app_secret = client_secret,
         token = '')

g<-get_all_channel_video_stats(channel_id = "UChqUTb7kYRX8-EiaN3XFrSQ" )

#MSNBC
#msnbc <- get_all_channel_video_stats(channel_id = "msnbcleanforward")

#PBS
#pbs <- get_all_channel_video_stats(channel_id = "UC6ZFN9Tx6xh-skXCuRHCDpQ")

#abc
#abc <- get_all_channel_video_stats(channel_id="UCBi2mrWuNuyYy4gbM6fU18Q")

g$viewCount <- as.numeric(g$viewCount)
g$likeCount <- as.numeric(g$likeCount)
g$commentCount <- as.numeric(g$commentCount)

ggplot(g, aes(x=log(likeCount), y=log(commentCount)))+
  geom_point()+
  stat_smooth(method="lm", col="red")

ggplot(g, aes(x=log(likeCount), y=log(viewCount)))+
  geom_point()+
  stat_smooth(method="lm", col="red")

library(ggplot2)
ggplot(g, aes(x=likeCount, y=viewCount))+
  geom_point()+
  stat_smooth(method="lm", col="red")

w <-word(g[,2])
table(w)
ww <- as.data.frame(table(w))
www <-ww[which(ww[,2]>5),]


ggplot(www, aes(w, Freq))+
  geom_bar(stat="identity")+
  xlab("words")+
  ylab("frequency")


comments <-get_all_comments(video_id ="	p5n8yLSGFps" )
c <-word(comments[,6])
cc <- as.data.frame(table(c))
ccc <-cc[which(cc[,2]>2),]

ggplot(ccc, aes(c, Freq))+
  geom_bar(stat="identity")+
  xlab("words")+
  ylab("frequency")
