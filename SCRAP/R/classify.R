#' @rdname classify_urls
#' @export 

#' @title
#' Take vector of URLs and classify into URL types

#' @description
#' This function takes a vector of URLs and will use a dictionary
#' of domains to classify into different types: survey platforms,
#' mail, google, social media, etc. and most importantly, news sites.

#' @details
#' Eventually, we will have an argument that will contain a dictionary of
#' URL domains and their corresponding type. For now, everything is
#' just hard-coded in the function.
#' It will return a data frame with one row per URL and Boolean variables 
#' indicating whether the URL corresponds to each type.

#' @author
#' Anthony Ramos, Simon Munzert, Pablo Barbera

#' @param urls Vector with URLs.

#' @import urltools
#' @importFrom digest digest
#' @import stringr
#' 
#' 
#' @title classify_urls
#' @description takes a vector of URLs, parses them, and classifies them by category
#' @params vector of urls
classify_urls_new <- function(urls) {
  urls_df <- parse_urls(urls$url,urls)
  urls_df$category <- fill('NA',len(urls_df$domain))
  ClassifyDF <- read.csv('Insert location of CSV with that data here')
  for(i in len(urls_df$domain))
    Cat <- ClassifyDF[ClassifyDF$domain == urls_df$domain[i]]$Category
    if(length(Cat) > 1){
      urls_df[i,'category'] <- Cat[1]
    }
}

classify_urls <- function(urls) {

  # parse urls
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
  urls_df$isGoogle <- str_detect(urls_df$domain, "google")
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
  urls_df$isNews <- str_detect(urls_df$domain, "cnn.com|huffingtonpost.com|washingtonpost.com|nytimes.com|foxnews.com|dailykos.com|drudgereport.com|breitbart.com|spiegel.de|sueddeutsche.de|focus.de|bild.de|zeit.de|welt.de|t-online.de")
  # return output
  return(urls_df)
}
