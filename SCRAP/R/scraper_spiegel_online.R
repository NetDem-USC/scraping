#!/usr/local/bin/Rscript
setwd("/Users/munzerts/Dropbox/Uni/Projects/2017-CSS-Attention/code/scraping_pages")

scraper_spiegel_headlines <- function(folder) {
  # load packages
  require(httr)
  require(stringr)
  require(magrittr)
  # get headlines
  url <- "http://www.spiegel.de/schlagzeilen/index.html"
  url_out <- GET(url) %>% content(as = "text")
  # write raw html
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  datetime <- format(as.POSIXct(Sys.time(), tz = Sys.timezone()), usetz = TRUE)  %>% as.character() %>% str_replace_all("[ :]", "-")
  write(url_out, file = paste0(folder, "/spiegel-", datetime, ".html"))
}

scraper_spiegel_articles <- function(folderInput, folderOutput) {
  # load packages
  require(httr)
  require(rvest)
  require(stringr)
  require(magrittr)
  require(R.utils)
  # import htmls
  htmls <- list.files(folderInput, pattern = "spiegel.+html$", full.names = TRUE)
  htmls <- htmls[length(htmls)] # pick only last html
  htmls_parsed <- lapply(htmls, read_html)
  urls_parsed <- lapply(htmls_parsed, function(x) { html_nodes(x, css = ".schlagzeilen-content a") %>% html_attr("href") })
  # download article htmls
  dir.create(folderOutput, showWarnings = FALSE, recursive = TRUE)
  urls_articles <- urls_parsed %>% unlist 
  urls_articles <- str_subset(urls_articles, "^/") %>% paste0("http://www.spiegel.de", .) # exclude full URLs (e.g., http://bento.de)
  sapply(urls_articles, function(x){
      destfile <- paste0(folderOutput, "/", basename(x))
      if(!file.exists(destfile)) {
        evalWithTimeout({try(download.file(x, destfile = destfile, method = "libcurl"))},
                        timeout = 10,
                        onTimeout = "silent")
      }
  })
}
# do to's:
  # make try call more robust
  # add check on file size
  # add report (how many URLs downloaded, which could not be downloaded)
  # implement concurrent downloads (see https://github.com/jeroen/curl/blob/master/R/multi.R)

  
parser_spiegel_articles <- function(folderInput) {
  # load packages
  require(rvest)
  require(purrr)
  require(magrittr)
  require(lubridate)
  require(stringr)
  htmls <- list.files(folderInput, pattern = ".+html$", full.names = TRUE)
  html_list <- lapply(htmls, read_html)
  page_parser <- function(css, attr = "", multi = FALSE) {
    if (multi == TRUE){
    sapply(html_list, function(x) { html_nodes(x, css = css) %>% html_text()}) %>% lapply(function(x) {str_replace_all(x, "\\r|\\n|\\t", "") %>% paste(sep=" ", collapse=" ")}) %>% unlist()
    }else if (str_length(attr) == 0) {
    sapply(html_list, function(x) { html_nodes(x, css = css) %>% html_text()}) %>% map(1) %>% lapply(function(x) ifelse(is_null(x), "", x)) %>%   unlist()
    }else{
    sapply(html_list, function(x) { html_nodes(x, css = css) %>% html_attr(attr)}) %>% map(1) %>% lapply(function(x) ifelse(is_null(x), "", x)) %>%   unlist()
    }
  }
  headline <- page_parser(".headline")
  headline_intro <- page_parser(".headline-intro")
  datetime <- page_parser(".timeformat", attr = "datetime") %>% ymd_hms(tz = "Europe/Berlin")
  domain <- page_parser(".current-channel-name")
  text_intro <- page_parser(".article-intro")
  text <- page_parser("#js-article-column li , p", multi = TRUE)
  articles_dat <- data.frame(outlet = "http://www.spiegel.de",
                             file = htmls,
                             headline, 
                             headline_intro,
                             datetime,
                             domain,
                             text_intro,
                             text,
                             stringsAsFactors = FALSE
                             )
  
  articles_dat
}
# to do's
  # add parameters: author, comments, raw text
  # clean dump of parsed data into database

scraper_spiegel_headlines(folder = "data/spiegelonline/indices")
scraper_spiegel_articles(folderInput <- "data/spiegelonline/indices", folderOutput <- "data/spiegelonline/articles")
#articles_df <- parser_spiegel_articles("data/spiegelonline/articles")
