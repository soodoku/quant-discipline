"

Parse AER Data

"

# Set directory
ifelse(grepl("aguess", getwd()), setwd("~/Dropbox/"), setwd(basedir))
setwd("quant-discipline/")

# Load libs
library(dplyr)
library(readr)
library(stringr)

# All the vols
vols <- dir("data/aer/sgml/", full.names=T)

absts <- NULL

for(j in vols) {
  
  # read_in_volume
  vol   <- read_file(j)

  # n_articles
  n.art <- str_count(vol, fixed("<artinfo>"))

  # Loop over all articles
  for(i in 1:n.art) {
    
    # first and last line of each article
    firstline <- str_locate_all(vol, fixed("<artinfo>"))[[1]]
    lastline  <- as.data.frame(str_locate_all(vol, fixed("</artinfo>"))[[1]])
  
    # malformed/truncated sgml where closing article is missing
    if (nrow(firstline) > nrow(lastline)) lastline[nrow(firstline), ] <- c(NA, nchar(vol))

    # Subset article data 
    article   <- substr(vol, firstline[i,1], lastline[i,2])

    # start/end of abstract
    firstab <- str_locate_all(article, fixed("<ab>"))[[1]]
    lastab  <- str_locate_all(article, fixed("</ab>"))[[1]]

    abst <- NA
    if (nrow(firstab)==1) abst    <- substr(article, firstab[1,2]+1, lastab[1,1]-1)
    
    doi1 <- str_locate_all(article, fixed("<doi>"))[[1]]
    doi2 <- str_locate_all(article, fixed("</doi>"))[[1]]

    if (length(doi1)==0) {
      doi <- NA
    } else {
      doi   <- substr(article, doi1[1,2]+1, doi2[1,1]-1)
    }

    absts <- rbind(absts, c(doi = doi,  volno = gsub("data/aer/sgml/|.sgml|AER_", "", j), abstract = abst))    
  }
}

# Coerce to df
absts <- data.frame(absts, row.names=1:nrow(absts))

# Filter abstracts !=n/a
absts <- absts %>% filter(!is.na(abstract))
#write.csv(absts, "data/aer_all.csv")

absts1 <- absts

## Now again for second set:

vols <- dir("data/aer/sgml_2/", full.names=T)[1:16]

absts <- NULL

for(j in vols) {
  
  # read_in_volume
  vol   <- read_file(j)
  
  # n_articles
  n.art <- str_count(vol, fixed("<artclinfo>"))
  
  # Loop over all articles
  for(i in 1:n.art) {
    
    # first and last line of each article
    firstline <- str_locate_all(vol, fixed("<artclinfo>"))[[1]]
    lastline  <- as.data.frame(str_locate_all(vol, fixed("</artclinfo>"))[[1]])
    
    # malformed/truncated sgml where closing article is missing
    if (nrow(firstline) > nrow(lastline)) lastline[nrow(firstline), ] <- c(NA, nchar(vol))
    
    # Subset article data 
    article   <- substr(vol, firstline[i,1], lastline[i,2])
    
    # start/end of abstract
    firstab <- str_locate_all(article, fixed("<ab>"))[[1]]
    lastab  <- str_locate_all(article, fixed("</ab>"))[[1]]
    
    abst <- NA
    if (nrow(firstab)==1) abst    <- substr(article, firstab[1,2]+1, lastab[1,1]-1)
    
    doi1 <- str_locate_all(article, fixed("<doi>"))[[1]]
    doi2 <- str_locate_all(article, fixed("</doi>"))[[1]]
    
    if (length(doi1)==0) {
      doi <- NA
    } else {
      doi   <- substr(article, doi1[1,2]+1, doi2[1,1]-1)
    }
    
    absts <- rbind(absts, c(doi = doi,  volno = gsub("data/aer/sgml_2/|.sgml|AER_|aer", "", j), abstract = abst))    
  }
}

# Coerce to df
absts <- data.frame(absts, row.names=1:nrow(absts))

# Filter abstracts !=n/a
absts <- absts %>% filter(!is.na(abstract))
absts <- rbind(absts, absts1)
absts$volno <- gsub("/", "", absts$volno)

write.csv(absts, "data/aer_all.csv")
