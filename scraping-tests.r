setwd("~/git/scraping")

# install current version of R package
library(devtools)
document("SCRAP")
install("SCRAP")
library(SCRAP)

# scraping homepage
headlines <- scrapeZeitHeadlines()

# scraping each article
articles <- list()
for (i in 1:nrow(headlines)){
	message(headlines$title[i])
	articles[[i]] <- scrapeZeitArticles(headlines$url[i])
	Sys.sleep(1)
}