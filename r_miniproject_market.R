getwd()

setwd('C:/Users/jayde/OneDrive/Desktop/rprog')

#install and load package arules
#install.packages("arules")
library(arules)
#install and load arulesViz
#install.packages("arulesViz")
library(arulesViz)
#install and load tidyverse
#install.packages("tidyverse")
library(tidyverse)
#install and load readxml
#install.packages("readxml")
library(readxl)
#install and load knitr
#install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
#install.packages("lubridate")
library(lubridate)
#install and load plyr
#install.packages("plyr")
library(plyr)
library(dplyr)

tr <- read.transactions('Market_Basket_Optimisation - Market_Basket_Optimisation.csv', sep=',')
tr

summary(tr)

if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")

association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
detach(package:tm, unload=TRUE)
inspect(association.rules[1:10])

subRules<-association.rules[quality(association.rules)$confidence>0.8]

top10subRules <- head(subRules, n = 10, by = "confidence")

plot(top10subRules, method = "graph",  engine = "htmlwidget")

filePath <- "C:/Users/jayde/OneDrive/Desktop/rprog/marketbasket.txt"

# Install
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud2") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud2")
library("RColorBrewer")

text <- readLines(filePath)

docs <- Corpus(VectorSource(text))                 

inspect(docs)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=129, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
wordcloud2(data=d, size=0.7, color='random-dark')
