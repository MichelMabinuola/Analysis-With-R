# An analysis on the speeches made by the president of Nigeria 
# Speach made on our independence day and during the current EndSars protest
# protest was organized due to the current police brutality

# libraries already installed 
# loading necessary libraries

library("tm")   #for textmining
library("SnowballC") # for text stemming
library("wordcloud") # analysis of the word (word cloud)
library("RColorBrewer") # color palettes

# setting the working directory
setwd("//Users//Kingmichael//desktop//DA_hW//text_mining")
#get the work dir and save in a variable
ctext <- getwd()
length(dir(ctext)) # total length should be equal to 2
dir(ctext) # name of files 

# Load the data as a corpus, a collection of text document!!!!
docs <- Corpus(DirSource(ctext)) # DirSource since its two files am reading

inspect(docs) #inspect the documents

# DATA CLEANING 
# Transformation
# taking out the unnessary things
for (i in seq(docs))
{
  docs[[i]] = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", docs[[i]])
  docs[[i]] = gsub("@\\w+", "", docs[[i]])
  docs[[i]] = gsub("http\\w+", "", docs[[i]])
  docs[[i]] = gsub("[ \t]{2,}", "", docs[[i]])
  docs[[i]] = gsub("^\\s+|\\s+quot;", "", docs[[i]])
  docs[[i]] = gsub("[^\x20-\x7E]", "", docs[[i]])
}

# removing stopwords
myStopwords <- c(stopwords('english'), 'must', 'also','will','per', 'can')

# A shorter pipeline to transfer to the termDocument
# matrix term-document pipeline ####
kb.tf <- list(weighting = weightTf, stopwords  = myStopwords,
              removePunctuation = TRUE,
              tolower = TRUE,
              minWordLength = 4,
              removeNumbers = TRUE, stripWhitespace = TRUE,
              stemDocument= TRUE)

#  The term-documents matrix is a table containing the frequency of each word in the speech.
dtm = TermDocumentMatrix(docs, control = kb.tf)

m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)
head(d, 30)

#  Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=40, random.order=FALSE, use.r.layout=FALSE, rot.per=0.25,
          colors=brewer.pal(3, "Dark2"))



