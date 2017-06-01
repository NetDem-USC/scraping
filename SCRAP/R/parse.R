#Parse a vector of URLs, and put them into a dataframe
parse_urls <- function(urls,url_df)
{
  url_df <- url_parse(urls)
  url_df$full_url <- urls
  return(url_df)
}
#Download the html of a URL with a given filename
download_url <- function(url,filename) {
  # packages
  require(curl)
  h <- new_handle()
  curl_download(url, filename, handle = h)
}

# use this for faster download?
#browseURL("https://github.com/jeroenooms/curl/blob/master/examples/sitemap.R")


## function to parse downloaded htmls
parse_html <- function(url, filename) {
  # require packages
  require(xml2)
  require(rvest)
  require(boilerpipeR)
  # parse file
  url_parsed <- read_html(url)
  url_text <- html_text(url_parsed) #what is this?
  try(main_text <- ArticleExtractor(url_text, asText = FALSE), silent = TRUE)
  # export file
  writeLines(main_text,filename)
}
