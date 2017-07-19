#' @rdname scrapeZeitHeadlines
#' @export

#' @title
#' Scrape homepage of Zeit Online

#' @author Anthony Ramos, Simon Munzert, Pablo Barbera

#' @title Scrape headlines from Zeit Online
#' @description This function takes the headlines off of ZeitOnline and returns a dataFrame with two
#' columns: titles and URLs.
#' 

scrapeZeitHeadlines <- function()
{
  ZeitHome <- read_html("http://www.zeit.de")
  nodes <- paste0(".teaser-fullwidth__title, .teaser-small__combined-link, ",
      ".teaser-fullwidth__media-link, .teaser-large__combined-link")
  title_nodes <- html_nodes(ZeitHome, nodes)
  titles <- xml_attr(title_nodes,"title")
  links <- xml_attr(title_nodes,"href")
  df <- data.frame(title=titles, url=links, 
    time=as.character(Sys.time()), stringsAsFactors=F)
  df <- df[!is.na(df$url),]
  return(df)
}

#' @rdname scrapeZeitArticles
#' @export
#' @title Scrape individual articles from the Zeit Online website

#' @description
#' This function takes a URL from an article from Zeit Online, and returns
#' a data frame with the URL, date, title, comments, summary, and text.

#' @param url string containing URL of article from ZeitOnline

#' @details
#' Return values will eventually contain main text

scrapeZeitArticles <- function(url)
{

  article <- read_html(url)

  date <- html_text(html_nodes(article, ".meta, .metadata__date"))
  date <- gsub("^ *", "", gsub("\n", "", date))

  title <- html_text(html_nodes(article,".article-heading__title, .headline__title"))

  comments <- html_text(html_nodes(article, "#js-article .js-scroll"))
  comments <- gsub("^ *| *$", "", gsub("\n", "", comments))
  comments <- gsub(" {2,}", " ", comments)

  summary <- html_text(html_nodes(article,".summary, .header-article__subtitle"))
  summary <- gsub("^ *| *$", "", gsub("\n", "", summary))
  summary <- gsub(" {2,}", " ", summary)

  text <- html_text(html_nodes(article,".paragraph"))
  text <- paste(text, collapse="\n\n")

  df <- data.frame(
    url, date, title, comments, summary, text, 
    stringsAsFactors=F)

  return(df)

}

