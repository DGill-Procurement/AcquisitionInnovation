###### 
# 2021-08-18
# Robert Carlson - Data and Analytic Solutions (DAS)
# IRS OCPO ART - PRP Procurement Research Partnership Contract
###### 

## Analysis with Natural Language Processing (NLP) of Procurement Documents
## The example tested with randomly selected Statement of Work (SOW) and Performance Work Statement (PWS)
## Procurement docuements were previously pre-processed from PDF and DOCX into text TXT files 

## The purpose of this code is to automatically extract the top keywords from 
## a procurement document using the "Rapid Automatic Keyword Extraction" RAKE algorithm

## This code uses the "udpipe" library with the downloaded English language module
## Sample extracted keyword results are included below

#install.packages("udpipe")
library(udpipe)
library(tidyverse)
library(janitor)

#### Load the "English" UDPIPE model
udmodel <- udpipe_download_model(language = "english") ###note this downloads the model - might not work behind a firewall
udmodel <- udpipe_load_model(file = udmodel$file_model)

#### Read sample SOW file
#filename<-"EXAMPLE.TXT"
filename<-"/home/username/Downloads/EXAMPLE.TXT"
rawSOW<-read_file(filename)

#Annotate the SOW
testSOW <- udpipe_annotate(udmodel, x = rawSOW)
testSOW <- as.data.frame(testSOW, detailed = TRUE)
testSOW <- testSOW %>% filter(sentence_id<=36) ###hard code sentence limit to roughly the first page, only process the first 36 sentenices of the sample SOW

###  Check the parts of speech available
tabyl(testSOW$xpos)

###  Check the other parts of speech available
tabyl(testSOW$upos)

##Review the top lemmas (lowercase tokens)
tabyl(testSOW$lemma) %>% filter(n>10)

## Select the top keywords using the "Rapid Automatic Keyword Extraction" RAKE algorithm
## Telling the algorithms that all the Nouns, Adjectives, and Verbs are relevant
keywords <- 
  keywords_rake(x = testSOW, term = "lemma", group = "doc_id", 
                relevant = testSOW$xpos %in% c("NN", "JJ", "NNP", "NNPS", "NNS", "VB", "VBG", "VBN", "VBZ"))
head(keywords)
#                    keyword ngram freq     rake
# 1       Operations support     2    2 4.564103
# 2  business transformation     2    2 3.616162
# 3 workforce transformation     2    2 3.388889
# 4       project management     2    2 3.107143
# 5               be provide     2    3 3.042857
# 6         perform analysis     2    2 2.666667


# a different calculation using only true nouns and adjectives
head(keywords_rake(x = testSOW, term = "lemma", group = "doc_id", relevant = testSOW$xpos %in% c("NN", "JJ")))
# 1  business transformation     2    2 2.3131313
# 2 workforce transformation     2    2 2.2222222
# 3            business case     2    2 2.0909091
# 4       project management     2    2 2.0000000
# 5                 business     1    2 1.0909091
# 6                  support     1    6 0.7272727

# a second different calculation
head(keywords_rake(x = testSOW, term = "lemma", group = "doc_id", relevant = testSOW$upos %in% c("NOUN", "ADJ")))
# 1  business transformation     2    2 2.878788
# 2 workforce transformation     2    2 2.583333
# 3          support request     2    2 2.272727
# 4       project management     2    2 2.166667
# 5        different project     2    3 2.000000
# 6              action plan     2    2 1.833333

######## BONUS #############
######## Keyword Collocation
stats<-keywords_collocation(x = testSOW, term = "lemma", group = "doc_id")
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
library(lattice)
barchart(key ~ pmi, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
         main = "Keywords identified by PMI Collocation", 
         xlab = "PMI (Pointwise Mutual Information)")
