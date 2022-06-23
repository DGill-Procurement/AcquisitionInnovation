## IRS OCPO ART
## Procurement Research Partnership (PRP) contract
## Data and Analytic Solutions, Inc. (DAS) (https://dasconsultants.com/)
## Contract Officer Representative: David Gill
## Author and Data Scientist: Ping Yu 
## Project Manager: Robert Carlson
## Contract Manager: Dawn Li
## Revised: 2022-06-20

## This script converts all PDF, DOC, DOCX, and RTF files in a folder path into Text format.
## Conversion of binary filetypes to text is an important preprocessing step before
## applying Natural Language Processing (NLP) and text search techniques to procurement documents
## such as Statements of Work (SOW), Performance Work Statements (PWS), Solicitations, Forms, and Contracts.


## Required libraries
library(pdftools) # for pdf_text()
library(textreadr) # for read_rtf(), read_docx(), read_doc()

## Optional libraries with 
#library(wordcloud)
#library(DataCombine)
#require(readxl)
#require(tidyverse)
#library(stringr)
#library(miceadds)

##TEST CASE - Output Page One of PDF Only
#lf = list.files(path="C:/folder",  all.files=FALSE,pattern = "pdf$",full.names = TRUE)
#for (i in 1:length(lf)){
#  temp.file_ <- pdf_text(lf[i])
#  input.file<- sub('\\.pdf$', '', temp.file_[1]) 
#  cat(file=paste(lf[i], ".txt"),input.file)
#}

#read files in directory and output text

lf = list.files(path="C:/folder",  all.files=FALSE,pattern = "pdf$",full.names = TRUE)
for (i in 1:length(lf)){
  input.file<- pdf_text(lf[i])
  cat(file=paste(lf[i], ".txt"),input.file)
}

lf = list.files(path="C:/folder",  all.files=FALSE,pattern = "*.rtf",full.names = TRUE)
for (i in 1:length(lf)){
  input.file<- read_rtf(lf[i])
  cat(file=paste( lf[i],   ".txt"),input.file)
}

lf = list.files(path="C:/folder",  all.files=FALSE,pattern = "*.docx",full.names = TRUE)
for (i in 1:length(lf)){
  input.file<- read_docx(lf[i])
  cat(file=paste( lf[i],   ".txt"),input.file)
}

lf = list.files(path="C:/folder",  all.files=FALSE,pattern = "*.doc$",full.names = TRUE)
for (i in 1:length(lf)){
  input.file<- read_doc(lf[i])
  cat(file=paste( lf[i],   ".txt"),input.file)
}
