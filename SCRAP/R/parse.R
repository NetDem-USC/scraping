#' @rdname Parse.R
#' @export

#' @description Parse a vector of URLs, and put them into a dataframe.
#' @param vector of urls, and dataframe to put results in.
#'
parse_urls <- function(urls,url_df)
{
  url_df <- url_parse(urls)
  url_df$full_url <- urls
  return(url_df)
}

#' @export
#' @description downloads the HTML of URLs into a given file name
#' @param string of url, string of file name
download_url <- function(url) {
  # packages
  require(curl)
  h <- new_handle()
  curl_download(url, paste0("~/Desktop/",gsub('/',"-",url),".txt"), handle = h)
}
#' @export 
#' @description removes a vector of URLS in parallel, by default uses 2 cores for a dual-core processor
#' @param a vector of URLs
download_in_parallel <- function(urls) {
  require(parallel)
  c <- makeCluster(2)
  clusterExport(c,"download_url")
  parLapply(c,urls,function(x) download_url(x))
  
}

# use this for faster download?
#browseURL("https://github.com/jeroenooms/curl/blob/master/examples/sitemap.R")

#' @import boilerpipeR
#' @import readr

#' @export
#' @description takes in a file name to download, and downloads it to output
#' @param string of name of file to download, string of output file
## function to parse downloaded htmls
parse_html <- function(filename, output) {
  # parse file
  url_text <- read_file(filename)
  main_text <- ArticleExtractor(url_text)
  # export file
  writeLines(main_text,output)
}
