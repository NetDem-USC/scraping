#' @rdname scrapeZeitHeadlines
#' @export

#' @title
#' Scrape homepage of Zeit Online

#' @author Anthony Ramos, Simon Munzert, Pablo Barbera

#' @title Scrape headlines from Zeit Online
#' @description This function takes the headlines off of ZeitOnline and returns a dataFrame with two
#' columns: titles and URLs.
#' 
#' @param path Path where file with homepage in html format will be stored

scrapeZeitHeadlines <- function(path)
{

  html <- download_url("http://www.zeit.de", path=path)
  ZeitHome <-read_html(html)

  nodes <- paste0(".teaser-fullwidth__title, .teaser-small__combined-link, ",
      ".teaser-fullwidth__media-link, .teaser-large__combined-link")
  title_nodes <- html_nodes(ZeitHome, nodes)
  titles <- xml_attr(title_nodes,"title")
  links <- xml_attr(title_nodes,"href")
  df <- data.frame(title=titles, url=links, 
    time=as.character(Sys.time()), stringsAsFactors=F)
  df <- df[!is.na(df$url),] # removing empty URLs
  df <- df[!duplicated(df$url),] # removing duplicated URLs
  return(df)
}

#' @rdname scrapeZeitArticle
#' @export
#' @title Scrape individual articles from the Zeit Online website

#' @description
#' This function takes a URL from an article from Zeit Online, and returns
#' a data frame with the URL, date, title, comments, summary, and text.

#' @param url string containing URL of article from ZeitOnline
#' @param path Path where file will be stored

#' @details
#' Return values will eventually contain main text

scrapeZeitArticle <- function(url, path)
{

  html <- download_url(url,  path=path)
  article <- read_html(html)

  date <- html_text(html_nodes(article, ".meta, .metadata__date, .entry-meta"))
  date <- gsub("^ *", "", gsub("\n|\t", "", date))

  title <- html_text(html_nodes(article,".article-heading__title, .headline__title, .entry-title"))

  comments <- html_text(html_nodes(article, "#js-article .js-scroll"))
  comments <- gsub("^ *| *$", "", gsub("\n", "", comments))
  comments <- gsub(" {2,}", " ", comments)

  summary <- html_text(html_nodes(article,".summary, .header-article__subtitle"))
  summary <- gsub("^ *| *$", "", gsub("\n", "", summary))
  summary <- gsub(" {2,}", " ", summary)

  text <- html_text(html_nodes(article,".paragraph, .entry-content p"))
  text <- paste(text, collapse="\n\n")

  df <- data.frame(
    url, date, title, comments=ifelse(length(comments)==0, NA, comments), 
    summary=ifelse(length(summary)==0, NA, summary), text, 
    stringsAsFactors=F)

  return(df)

}

#' @rdname scrapeZeitRSS
#' @export
#' @title Scrape RSS feeds of Zeit
#' @description scrapes RSS feeds of Zeit home page
#' @param path Path where file with homepage in html format will be stored


scrapeZeitRSS <- function(folder) {
  # get RSS
  rss_feeds <- c("http://newsfeed.zeit.de/index",
                 "http://newsfeed.zeit.de/politik/index",
                 "http://newsfeed.zeit.de/wirtschaft/index",
                 "http://newsfeed.zeit.de/gesellschaft/index",
                 "http://newsfeed.zeit.de/kultur/index",
                 "http://newsfeed.zeit.de/wissen/index",
                 "http://newsfeed.zeit.de/digital/index",
                 "http://newsfeed.zeit.de/sport/index",
                 "http://newsfeed.zeit.de/karriere/index",
                 "http://newsfeed.zeit.de/entdecken/index",
                 "http://newsfeed.zeit.de/mobilitaet/index",
                 "http://newsfeed.zeit.de/all")
  rss_out_list <- lapply(rss_feeds, read_xml)
  # write raw RSS
  rss_feeds <- rss_feeds %>% str_replace("/index$", "")
  filepaths <-paste0(folder, "/RSS-", datetime, "-", basename(rss_feeds), ".rss")
  Map(function(x, filepath) write_xml(x, file = filepath, w, options = "format"), rss_out_list, filepaths)
}




#' @rdname scrapeZeitRSSarticles
#' @export
#' @title Scrape articles from RSS feeds of Zeit
#' @description scrapes articles from RSS feeds of Zeit home page
#' @param folderInput Folder where files with RSS feeds are located
#' @param folderOutput Folder where articles in html format will be stored
#' @param donefile File with URLs that have already been downloaded

scrapeZeitRSSarticles <- function(folderInput, folderOutput, donefile) {
  # import xmls
  xmls <- list.files(folderInput, pattern = "RSS.+rss$", full.names = TRUE)
  xmls <- xmls[(length(xmls)-11):length(xmls)] # pick only newest files
  xmls_parsed <- lapply(xmls, read_xml)
  urls_parsed <- lapply(xmls_parsed, function(x) { xml_nodes(x, "link") %>% xml_text}) %>% unlist %>% unique()
  # download article htmls
  urls_articles <- urls_parsed %>% unlist 
  # excluding URLs already downloaded
  if (file.exists(donefile)) done <- scan(donefile, what="character") 
  if (!file.exists(donefile)) done <- c()
  out <- file(donefile, "a")
  urls_articles <- urls_articles[urls_articles %in% done == FALSE]

  sapply(urls_articles, function(x){
      destfile <- paste0(folderOutput, "/", basename(x))
      if(!file.exists(destfile)) {
        res <- evalWithTimeout({try(download.file(x, destfile = destfile, method = "libcurl"))},
                        timeout = 10,
                        onTimeout = "silent")
        if (!is.null(res)){writeLines(x, con=out)}
      }
  })
  close(out)
}









