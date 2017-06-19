#' @rdname Frankfurt.R
#' 
#' 
#' @title This package is for scraping the Zeit online front page
#' 
#' @description Scrapes front page headlines from Frankfurt Allgemaine

getFrankfurtHeadlines <- function()
{
  Allegmaine <- read_html("http://www.faz.net/aktuell/")
  title_nodes <- html_nodes(Allegmaine,".TeaserHeadLink")
  links <- xml_attr(title_nodes,"href")
  titles <- xml_attr(title_nodes,"title")
  headlines <- data.frame(titles=titles,links = links)
  return(headlines)

}

#' @rdname Frankfurt.R
#' @description scrapes the date, summary, title, and main text from an article on Frankfurt Allgemaine
#' @param url string
#' @return dataframe with date, headline, summary and maintext as columns

getFrankfurtArticle <- function(url)
{
  article <- read_html(url)
  date <- xml_attr(html_nodes(article,".Datum"),"content")
  summary <- html_text(html_nodes(".Copy"))[1]
  headline <- html_text(html_nodes(article,"h2"))[1]
  main_text <- html_text(html_nodes(article,". p")) #this is inside a class="" div tag, but i'm not sure what the selector is
  article_df <- data.frame(date=date,headline=headline,summary=summary,main_text=main_text)
  return(article_df)
}



