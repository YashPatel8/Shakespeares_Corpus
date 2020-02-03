#In this tutorial, we will be using works by Shakespeare. 
#In order to have a better view of Corpus and word count matrix, 
#we divide the original text file into three and you can download 
#them from 1.txt, 2.txt and 3.txt.

#Alternatively you can just run the following commands the 
#download and store the files in a folder called data:

library(RCurl)
dir.create("data")
setwd("data")
url <- "https://raw.githubusercontent.com/angerhang/statsTutorial/master/src/textMining/data/1.txt"
write(getURL(url), file = "1.txt")
url <- "https://raw.githubusercontent.com/angerhang/statsTutorial/master/src/textMining/data/2.txt"
write(getURL(url), file = "2.txt")
url <- "https://raw.githubusercontent.com/angerhang/statsTutorial/master/src/textMining/data/3.txt"
write(getURL(url), file = "3.txt")
setwd("..")


#Corpus the main structure that tm uses for storing 
#and manipulating text documents. There are two 
#types VCorpus (Volatile Corpus) and PCorpus 
#(Permanent Corpus), the main difference between 
#these two implementations is that the former holds 
#the documents as R objects in memory whereas the latter 
#deals with documents that are stored outside of R environment. 
#When we are constructing our Corpus object, 
#we need to provide a source and there are three 
#types of such sources, DirSource, VectorSource and 
#DataFrameSource. We will use DirSource the import the 
#three text files that we just downloaded and using 
#DirSource is the only way to import files from the user system

library(tm)
### Loading required package: NLP

shakespeare <- VCorpus(DirSource("data", encoding = "UTF-8"))
shakespeare

#As we can see our Shakespeare corpus contains three documents. 
#Just a side remark, if one wants to create an easy 
#Corpus object, it is easy to do via VectorSrouce.

twoPhrases <- c("Phrase one ", "Phrase two")
simpleCorpus <- VCorpus(VectorSource(twoPhrases))
simpleCorpus

#If you want to export the your Corpus 
#for to used by other tools. Simply use:
writeCorpus(shakespeare)
#which will output the documents to 
#multiple files in the Corpus to the hard disk.


#Corpus mainipulation
#There are many methods we can use to play with Corpus. 
#Each Corpus has its own meta data and each document in the 
#Corpus also has one.

meta(shakespeare[[1]])

shakespeare[[1]]
summary(shakespeare)

#There are also other useful methods available such as 
#tmUpdate() which checks the new files that do not exist 
#yet and add those in and inspect() which gives a more 
#detailed overview than summary().


#Cleaning data
#We now want to transform the data we have before 
#we actually conduct out analysis. Some classical method 
#for cleaning are removing unnecessary white space, 
#stemming and stop words removal and conversion to 
#lower or upper case. For the ones don't know, stop words 
#removal is to eliminate some common words that have 
#little value in the analysis e.g. the and be. 
#Stem mining is to reduce derived words to their root 
#form such as making says, said into say because they 
#essentially have the same meanings. We do the cleaning 
#with the help of tm_map which applies a function across 
#all the documents.

library(SnowballC)
# Remove whitespace
shakespeare <- tm_map(shakespeare, stripWhitespace, lazy=TRUE)
# Stemming 
shakespeare <- tm_map(shakespeare, stemDocument, lazy=TRUE)
# Remove punctuation
shakespeare <- tm_map(shakespeare, removePunctuation, lazy=TRUE)
# Remove stopwords
tm_map(shakespeare, content_transformer(removeWords), stopwords("the"), lazy=TRUE)

# Case conversion
tm_map(shakespeare, content_transformer(tolower), lazy = TRUE)



#Term document matrix
#at this point our data is ready for analysis

dtm <- DocumentTermMatrix(shakespeare)

#Essentially the frequency of a word is a representation 
#of its importance. We have see the terms that show 
#up more than 25 times

highFreqTerms <- findFreqTerms(dtm, 25, Inf)
summary(highFreqTerms)

highFreqTerms[1:10]

#We can also find the words that have a 
#certain correlations with one word in the 
#term document matrix. The correlation value is one 
#if two words always appear together and it becomes 
#zero if they never show up at the same time. 
#We first want to see how many words have a correlation 
#higher than 0.95 with the word love.

loves_assocs <- findAssocs(dtm, "love", 0.95)

#visualization on all the words that have frequency higher than 2500.


freq <- sort(colSums(as.matrix(dtm)),decreasing=TRUE)
library(wordcloud)

set.seed(555)
wordcloud(names(freq), freq, min.freq=2500, max.words = 100, colors=brewer.pal(8, "Dark2"))


barplot(freq[1:50], xlab = "term", ylab = "frequency",  col=heat.colors(50))











