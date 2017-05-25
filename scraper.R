library(boilerpipeR) # this grabs main text, throws out extraneous garbage from web pages
library(RCurl)

#load("unique_urls.RData")
urls100 <- readLines("unique_urls_100k.txt") # , stringsAsFactors = FALSE

head(urls100)
tail(urls100)

urls100 <- gsub("\"", "", urls100)

setwd("text_try3")
for(i in 1:34175) { #length(urls100)  11655 23069 33000 34176
  
  page <- ""
  url <- urls100[i]
  try(page <- getURLContent(paste0("http://", url), .opts=curlOptions(followlocation=TRUE, timeout=2, maxredirs=20)), silent = TRUE)
  try(maintext <- ArticleExtractor(page, asText = TRUE), silent = TRUE)
  
  writeLines(maintext, paste0("url_", i, ".txt")) # saving to individual text files
  
}
