

#This file contains the analysis code for the empirical dataregarding the link between collaboration pattern
#and diversity of materials studied. The code contains:
#For the experiemnts between PM and DOM the list of DOI are extracted between 1990-2015 (comparable to the DOM-PM network paper)
#then the authors collaboration network is extracted, the distribution of the authors duration in the field is calculated. A survival curve
#is fitted to the data to obtain the aproximated duration of staying in the field, e.g Weibull distribution.
#For the ENPs toxicity data, the same is done for these data that describes the experimental data regarding the toxcity of ENPs experiments (toxicology only)
#the main point of the code here is to extract relevant information regarding the collaboration pattern in terms of network structure and duration of activity



# ENPs toxicity -----------------------------------------------------------

#text mining of 2569 experimental papers of nanoparticles toxicity.

#read in the files (with abstracts and references):
tox.ref.file <- file("../data/toxicity/wos_output.ciw", open = "r")
tox.data <- data.frame(doi = c(), year = c(), abstract = c())
while(length(line <- readLines(tox.ref.file, n = 1, warn = FALSE)) > 0){
  if(grepl(pattern = "^AB\\s", x = line)){
    #new instance of a publication, the abstract part
    ab <- strsplit(line, split = "AB\\s")[[1]][2]
  }
  if(grepl(pattern = "^PY\\s", x = line)){
    #new instance of a publication, the publication year
    y <- strsplit(line, split = "PY\\s")[[1]][2]
  }
  if(grepl(pattern = "^DI\\s", x = line)){
    #new instance of a publication, the DOI number
    di <- strsplit(line, split = "DI\\s")[[1]][2]
  }
  if(grepl(pattern = "^ER", x = line)){
    #if the instance description is over, insert the information collected into the data frame
    tox.data <- rbind(tox.data, data.frame(doi = di, year = y, abstract = ab))
  }
}
close(tox.ref.file)
nrow(tox.data)
#sort the data according to the year of publication (so it'll be easier later to extract the terms used according to year):
tox.data <- tox.data[order(as.numeric(tox.data$year)), ]
#save as an sql relation
#BAG OF WORDS ANALYSIS: USING THE qdap LIBRARY

####eliminate review papers

#####extract ebstracts and concatenate into a single text object
#object of all abstracts, eliminta
library(qdap)#basic text mining tools
freq_terms()#bag of words analysis

#TEXT ANALYSIS USING THE tm LIBRARY
library(tm)
library(SnowballC)#to enable the
library(wordcloud)
library(openNLP)
#conver the abstracts vector into a source object:
source.abstract <- VectorSource(tox.data$abstract)#if more than 1 column is to be converted into a corpus use  DataframeSource() instead.
#convert the absract vector into a Vcorpus:
corpus.abstract <- VCorpus(source.abstract)
corpus.abstract[[1]][1]#the text content of the first entry

#Clean and preprocessing of the data:
#clean corpus function
clean.corpus <- function(corpus){
  #corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  #corpus <- tm_map(corpus, removeNumbers)
  #corpus <- tm_map(corpus, stemDocument, language = "english", lazy = TRUE)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "signific", "likely", "We", "the", "nm", "cell", "increas", "ros", "sugges"))
  #corpus <- tm_map(corpus, content_transformer(bracketX()))#remove words within brackets
  return(corpus)
}

#basic cleaning of the data:
corpus.abstract <- clean.corpus(corpus.abstract)
#In order to perform bag of words on the data, we can here 
#term document matrix:
abstract.dtm <- DocumentTermMatrix(corpus.abstract)#each row is an abstract and each column is a word, the data is originally sorted accroding 
#to the year of publication (in an increasing order) therefore the rows are sorted according to the year of publication, and it is therefore easy
#to obtain a time series for the frequency of each word, if we aggregate the rows according to years.

abstract.matrix <- as.matrix(abstract.dtm)#observe different parts of the term matrix
dim(abstract.matrix)
sum(abstract.matrix[, 3934])
t <- inspect(abstract.dtm)
apply(abstract.matrix[, sum(colnames(abstract.matrix) == tolower("CuZnFe2O4"))]), 2, sum)

#frequency of the words

abstract.freq <- sort(colSums(abstract.matrix), decreasing = T)
freq <- data.frame(words = names(abstract.freq), freq = abstract.freq)
#take only the most frequent words:
#10 percent quantile:
head(abstract.freq)
barplot(height = freq$freq[freq$freq < 25 & freq$freq > 20], names = freq$words[freq$freq < 25 & freq$freq > 20], 
        las = 2, cex.names = 0.5)

#find which words are associated with the terms: nanomateri

np.assoc <- findAssocs(abstract.tdm, c('nanoparticles','nanomaterial'), 0.1)#it will help create the list of potential nanoparticles in the database
organ.assoc <- findAssocs(abstract.tdm, c('toxic','toxicity', 'effect', 'test', "organism", "cell", "cells", "organ"), 0.1)#
exp.assoc <- findAssocs(abstract.tdm, c('exposure'), 0.1)#
#using the tm_map() function that applies the function to the corpus content!
### tm_map(corpus.abstract, removePunctuation)#the removePunctuation is part of the tm library fucntions
### tm_map(corpus.abstract, content_transformer(a function that is not in the tm library))
for(i in seq(0,1, by = 0.05)){
  organ.assoc <- findAssocs(abstract.tdm, c('toxic','toxicity', 'effect', 'test', "organism", "cell", "cells", "organ"), 0.5)#
}

#word cloud

wordcloud(words = freq$words, freq = freq$freq, min.freq = 1,
          max.words=2000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



#each abstract should be stored as an elemnt in a large vector of all abtracts in order to convert to corpus (VCorpus object)
#extract abstract and store in one single text vector