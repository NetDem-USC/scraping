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

#' @import boilerpipeR
#' @import readr

## function to parse downloaded htmls
parse_html <- function(filename, output) {
  # parse file
  url_text <- read_file(filename)
  main_text <- ArticleExtractor(url_text)
  # export file
  writeLines(main_text,output)
}
