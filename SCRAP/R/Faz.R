#' @rdname scrapeFazHeadlines
#' @export

#' @title
#' Scrape homepage of Frankfurter Allgemeine

#' @author Simon Munzert, Pablo Barbera, Joshua Timm

#' @title Scrape headlines from Frankfurter Allgemeine
#' @description This function takes the headlines off of Frankfurter Allgemeine and returns a dataFrame with two
#' columns: titles and URLs.
#' 
#' @param path Path where file with homepage in html format will be stored

scrapeFazHeadlines <- function(path)
{

  html <- download_url("http://www.faz.net/aktuell/", path=path)
  home <-read_html(html)

  title_nodes <- html_nodes(home, ".tsr-Base_ContentLink")
  titles <- xml_attr(title_nodes, "title")
  links <- xml_attr(title_nodes, "href")

  df <- data.frame(title=titles, url=paste0("http://www.faz.net/", links), 
    time=as.character(Sys.time()), stringsAsFactors=F)
  df <- df[!is.na(df$url),] # removing empty URLs
  df <- df[!duplicated(df$url),] # removing duplicated URLs
  return(df)
}


#' @rdname scrapeFazArticle
#' @export
#' @title Scrape individual articles from the Frankfurter Allgemeine website

#' @description
#' This function takes a URL from an article from Frankfurter Allgemeine, and returns
#' a data frame with the URL, date, title, comments, summary, and text.

#' @param url string containing URL of article from Frankfurter Allgemeine
#' @param path Path where file will be stored

#' @details
#' Return values will eventually contain main text


scrapeFazArticle <- function(url, path)
{

  html <- download_url(url,  path=path)
  article <- read_html(html)

  date <- html_text(html_nodes(article, ".atc-MetaTime"))
  

  title <- html_text(html_nodes(article,".atc-HeadlineText"))

  comments <- NA

  summary <- html_text(html_nodes(article,".atc-IntroText"))
  summary <- gsub("\n|\t", "", summary)
  

  text <- html_text(html_nodes(article,".atc-TextParagraph"))
  text <- paste(text, collapse="\n\n")

  df <- data.frame(
    url, date, title, comments=ifelse(length(comments)==0, NA, comments), 
    summary=ifelse(length(summary)==0, NA, summary), text, 
    stringsAsFactors=F)

  return(df)

}







