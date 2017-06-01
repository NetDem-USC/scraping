### BAUSTELLEN:
# parse_html: boilerpipeR bringt R zum Absturz
# download_urls: noch nicht vektorisiert
# Filebenennung über digest() adhoc
# Parser ausbauen: Links in Docs identifizieren, Video-Content, Metadaten des Artikels (headline, autor, text...) etc.


## function to parse and classify urls
classify_urls <- function(urls) {
  # load packagesdev
  require(urltools)
  require(digest)
  # parse url
  urls_df <- url_parse(urls)
  # store full url
  urls_df$url_full <- urls
  # store unique_id
  urls_df$uid <- sapply(urls_df$url_full, digest)
  # generate indicators
  urls_df$isMainpage <- is.na(urls_df$path)
  urls_df$isSurvey <- str_detect(urls_df$domain, "survey") | str_detect(urls_df$path, "survey")
  urls_df$isSurveyPlatform <- str_detect(urls_df$domain, "samplicio.us|yougov|mturk.com|myview.com|zoompanel.com|mintvine.com|sample-cube.com|springboardamerica.com|quickrewards.net|sassieshop.com|lb.ocucom.com|oneopinion.com")
  urls_df$isRewardingPlatform <- str_detect(urls_df$domain, "swagbucks.com|mypoints.com|neobux.com|clixsense.com|inboxdollars.com|prizegrab.com|crowdtap.com|instagc.com|comparteunclic.com|ncponline.com")
  urls_df$isSearchEngine <- urls_df$domain %in% c("google.com", "bing.com", "search.yahoo.com", "search.aol.com", "duckduckgo.com")
  urls_df$isSearch <- str_detect(urls_df$domain, "search") | str_detect(urls_df$path, "search")
  urls_df$isMail <- str_detect(urls_df$domain, "mail|outlook")
  urls_df$isGoogle <- str_detect(urls_df$domain, "google")e
  urls_df$isFacebook <- str_detect(urls_df$domain, "facebook")
  urls_df$isTwitter <- str_detect(urls_df$domain, "twitter")
  urls_df$isGooglePlus <- str_detect(urls_df$domain, "plus.google")
  urls_df$isYouTube <- str_detect(urls_df$domain, "youtube")
  urls_df$isInstagram <- str_detect(urls_df$domain, "instagram")
  urls_df$isTumblr <- str_detect(urls_df$domain, "tumblr")
  urls_df$isShopping <- str_detect(urls_df$domain, "amazon.com|ebay.com|amazon.com|paypal.com|walmart.com|pinterest.com|etsy.com")
  urls_df$isGaming <- str_detect(urls_df$domain, "player.pureplay.com|pogo.com|worldwinner.com|gsn.com|football.fantasysports.yahoo.com")
  urls_df$isPorn <- str_detect(urls_df$domain, "xhamster.com|youporn.com|pornhub.com")
  urls_df$isWikipedia <-  str_detect(urls_df$domain, "wikipedia")
  urls_df$isResidual <- str_detect(urls_df$domain, "ab.entertainmentcrave.com|netflix.com|ss.ktrmr.com|pch.com|sp004.pcrint.net|person.ancestry.com|imdb.com|prod70.datacollectionsite.com|spectrum.pch.com|realtor.com|login.live.com|zillow.com")
  news <- c("huffingtonpost.com", "washingtonpost.com", "nytimes.com", "foxnews.com", "dailykos.com", "drudgereport.com", "breitbart.com")
  urls_df$isNews <- urls_df$domain %in% news
  # return output
  return(urls_df)
}


## function to download urls as html
download_urls <- function(url,filename) {
  # packages
  require(curl)
  # setup new handle
  h <- new_handle()
  # get dat
  # download
  curl_download(url, filename, handle = h)
}

# use this for faster download?
#browseURL("https://github.com/jeroenooms/curl/blob/master/examples/sitemap.R")


## function to parse downloaded htmls
parse_html <- function(url, dest_folder) {
  # require packages
  require(xml2)
  require(rvest)
  require(boilerpipeR)
  # create filename
  url_name <- str_replace(url, "\\.[[:alnum:]]+$", "")
  filename <- paste0(basename(url_name), ".txt")
  # parse file
  url_parsed <- read_html(url)
  url_text <- html_text(url_parsed) #what is this?
  try(main_text <- ArticleExtractor(url_text, asText = FALSE), silent = TRUE)
  # export file
  writeLines(main_text, paste0(dest_folder, "/", filename))
}



## test functions ---------------------------

# prepare urls
urls <- c("http://www.spiegel.de/politik/ausland/scott-pruitt-und-donald-trump-wer-ist-der-neue-chef-der-epa-a-1135234.html",
          "http://www.sueddeutsche.de/politik/sicherheitskonferenz-trump-der-unsichtbare-elefant-1.3385139",
          "http://www.focus.de/wissen/weltraum/gefahr-aus-dem-all-droht-ein-schwarzes-loch-die-erde-zu-verschlingen_id_6658237.html",
          "http://www.bild.de/politik/inland/muenchner-sicherheitskonferenz/russland-aussenminister-lawrow-auf-muenchener-sicherheitskonferenz-50493950.bild.html",
          "http://www.zeit.de/politik/deutschland/2017-02/afd-bjoern-hoecke-parteiausschluss-dresden-entschuldigung",
          "https://www.welt.de/wirtschaft/article162190193/Merkel-kontert-Kritik-an-Exportstaerke-mit-kleinem-Scherz.html",
          "http://www.t-online.de/lifestyle/gesundheit/id_80399498/umweltministerium-verbietet-fleisch-und-fisch-fuer-seine-gaeste.html")

library(readr)
library(stringr)
library(magrittr)
library(urltools)
library(dplyr)
urls_long <- read_csv("sample_urls.csv")
urls_long_domain <- url_parse(urls_long$urls)$domain
table(urls_long_domain) %>% sort(decreasing = T) %>% extract(1:50)


# classify urls
urls_df <- classify_urls(urls_long$urls)

urls_df_filtered <- filter(urls_df,
                           isMainpage == F,
                           isSurvey == F,
                           isSurveyPlatform == F,
                           isRewardingPlatform == F,
                           isSearchEngine == F,
                           isSearch == F,
                           isMail == F,
                           isGoogle == F,
                           isFacebook == F,
                           isTwitter == F,
                           isGooglePlus == F,
                           isInstagram == F,
                           isYouTube == F,
                           isTumblr == F,
                           isShopping == F,
                           isGaming == F,
                           isPorn == F,
                           isWikipedia == F,
                           isResidual == F,
                           isNews == T
                           )

urls_df_filtered <- filter(urls_df, isNews == T, isMainpage == F)

urls_df_filtered <- filter(urls_df, str_detect(domain, "tumblr")) %>% View
nrow(urls_df_filtered)
table(urls_df_filtered$domain) %>% sort(decreasing = T) %>% extract(1:50)


# download urls
download(urls_df_filtered[1:5,], "url_full", "uid", filname_type = "digest", dest_folder = "../../data/urls")

h <- new_handle()
for(i in 1:10) {
  filename <- paste0("../../data/urls", "/", urls_df_filtered$uid[i], ".html")
  curl_download(urls_df_filtered$url_full[i], filename, handle = h)
}


urls_df %>% filter(!is.na(path)) %>% # don't scrape content from top-level page
            filter()
sapply(urls_df_filtered$url_full[1:50], download_urls, url_uid = "uid", dest_folder = "../../data/urls")



# parse url files, store text
url_files <- list.files("../../data/urls")
sapply(url_files, parse_html, dest_folder = "../../data/urls_parsed")



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
