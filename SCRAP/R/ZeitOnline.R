#' @export

scrape.ZeitHeadlines <- function()
{
  require(rvest)
  titles_nodes <- html_text(doc,".teaser-small__combined-link, .teaser-fullwidth__media-link, .teaser-large__combined-link")
  titles <- xml_attr(title_nodes,"title")
  links <- xml_attr(title_nodes,"title")
  Titles.Links <- data.frame(titles=titles,links=links)
  return(Titles.Links)
}


