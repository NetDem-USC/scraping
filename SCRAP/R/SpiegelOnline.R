#' @rdname scrapeSpiegelOnline
#' @export

#' @title Scrape homepage of Spiegel Online
#' @description scrapes headlines off of Spiegel Online home page

scrapeSpiegelOnline <- function() {

  doc <-read_html("http://www.spiegel.de/")
  #get main articles
  title=html_nodes(doc,".article-title a")
  titles = xml_attr(title,"title")
  title_links = xml_attr(title,"href")
  df <- data.frame(title=titles, url=title_links, 
                   time=as.character(Sys.time()), stringsAsFactors=F)
  df <- df[!is.na(df$url),]
  df <- df[!duplicated(df$url),]
  
  return(df)
}

#' @rdname scrapeSpiegelArticle
#' @export
#'@title scrapes an article off of spiegel.com
#'@description Scrapes article date, content, and summary from spiegel article
#'@param url string of article url

scrapeSpiegelArticle <- function(url) {
  
  article <- read_html(url)
  article_intro <- html_text(html_nodes(article,".headline-intro"))[1]
  article_title <- html_text(html_nodes(article,".headline"))[1]
  article_title <- paste(article_intro,article_title,sep=": ")
  
  date <- html_text(html_nodes(article,".article-function-date"))[1]
  date <- gsub("\t", "", gsub("\r", "", date))
  
  content <- html_text(html_nodes(article,"p"))
  summary <- content[1]
  
  content <- paste(content, collapse="\n\n")
  content <- gsub("^ *| *$", "", gsub("\n|\t|\r", "", content))
  
  summary <- gsub("^ *| *$", "", gsub("\n", "", summary))
  summary <- gsub(" {2,}", " ", summary)
  
  comments = grep("insgesamt (.*) Beiträge",html_text(html_nodes("span"),value=TRUE))
  
  
  article_df <- data.frame(date=date,summary=summary,
                           headline=article_title,text=content,
                           comments=ifelse(length(comments)==0, NA, comments),
                           stringsAsFactors=F)
                           
  return(article_df)

}
