#' @rdname Spiegel
#' @title Package for Scraping articles off of spiegel online


#' @name scrape.SpiegelOnline
#' @description scrapes headlines off of spiegel online home page
scrape.SpiegelOnline <- function() {

  require(rvest)
  doc <-read_html("http://www.spiegel.de/")
  #get main article
  doc %>% html_nodes(".hp-first-article a")[1] %>% xml_attr("href")
  doc %>% html_nodes(".hp-first-article a")[1] %>% xml_attr("title")

  doc %>% html_nodes(".column-wide h2 a") %>% xml_attr("href")
  doc %>% html_nodes(".column-wide h2 a") %>% xml_attr("title")
}

#' @rdname Spiegel
#'@name scrape.SpiegelArticle
#'@description Scrapes article date, content, and summary from spiegel article
#'@param string containing url of function
scrape.SpiegelArticle <- function(url) {
  article <- read_html(url)
  date <- html_nodes(".article-function-date")
  content <- html_text(html_nodes(".spArticleContent p"))
  summary <- content[1]

}
