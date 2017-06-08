#' @rdname ZeitOnline
#' @author Anthony Ramos, Simon Munzert, Pablo Barbera
#' @export

#' @title
#' Packages for scraping articles from Zeit Online

#' @description This function takes the headlines off of ZeitOnline and returns a dataFrame with two
#' columns, titles and links.

#' @param none
#' @export
scrape.ZeitHeadlines <- function()
{
  require(rvest)
  ZeitHome <- read_html("http://www.zeit.de")
  titles_nodes <- html_nodes(ZeitHome,".teaser-small__combined-link, .teaser-fullwidth__media-link, .teaser-large__combined-link")
  titles <- xml_attr(title_nodes,"title")
  links <- xml_attr(title_nodes,"title")
  Titles.Links <- data.frame(titles=titles,links=links)
  return(Titles.Links)
}


#' @param string containing URL of article from ZeitOnline
#' @description
#' This function takes the title, summary, and main text from an article from Zeit Online, and returns
#' a vector with the date,title, and summary.
#'
#' @details
#' Return values will eventually contain main text
#' @export
scrapeZeitArticles <- function(url)
{

  article <- read_html(url)
  date <- xml_attr(html_nodes(article, ".metadata__date"),"datetime")
  title <- html_text(html_nodes(article,".article-heading__title"))
  summary <- html_text(html_nodes(article,".summary"))
  main_text <- html_text(html_nodes(article,".paragraph"))
  return(c(date,title,summary))

}

