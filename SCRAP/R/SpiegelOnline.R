#' @rdname scrapeSpiegelOnline
#' @export
#' @title Scrape homepage of Spiegel Online
#' @description scrapes headlines off of Spiegel Online home page
#' @param path Path where file with homepage in html format will be stored

scrapeSpiegelOnline <- function(path) {

  html <- download_url("http://www.spiegel.de/",
    path=path)
  doc <-read_html(html)

  #get main articles
  title=html_nodes(doc,".article-title a")
  titles = xml_attr(title,"title")
  title_links = xml_attr(title,"href")

  title_links <- ifelse( grepl("https?://", title_links),
    title_links, paste0("http://www.spiegel.de", title_links) )

  df <- data.frame(title=titles, url=title_links, 
                   time=as.character(Sys.time()), stringsAsFactors=F)
  df <- df[!is.na(df$url),]
  df <- df[!duplicated(df$url),]
  
  return(df)
}

#' @rdname scrapeSpiegelArticle
#' @export
#' @title scrapes an article off of spiegel.com
#' @description Scrapes article date, content, and summary from spiegel article
#' @param url string of article url
#' @param path Path where file will be stored 

scrapeSpiegelArticle <- function(url, path) {
  
  html <- download_url(url,  path=path)

  article <- read_html(html)
  article_intro <- html_text(html_nodes(article,".headline-intro"))[1]
  article_title <- html_text(html_nodes(article,".headline"))[1]
  article_title <- paste(article_intro,article_title,sep=": ")
  
  date <- html_text(html_nodes(article,".article-function-date"))[1]
  date <- gsub("\t|\n|\r", "", gsub("\r", "", date))
  
  content <- html_text(html_nodes(article,"p"))
  summary <- content[1]
  
  content <- paste(content, collapse="\n\n")
  content <- gsub("^ *| *$", "", gsub("\n|\t|\r", "", content))
  
  summary <- gsub("^ *| *$", "", gsub("\n", "", summary))
  summary <- gsub(" {2,}", " ", summary)
  
  comments <- grep("insgesamt (.*) Beiträge",html_text(html_nodes(article, "span")),value=TRUE)
  comments <- gsub("^ *", "", gsub("\r|\n|\t", "", comments))
  
  
  article_df <- data.frame(date=date,summary=summary,
                           headline=article_title,text=content,
                           comments=ifelse(length(comments)==0, NA, comments),
                           stringsAsFactors=F)
                           
  return(article_df)

}

#' @rdname scrapeSpiegelRSS
#' @export
#' @title Scrape RSS feeds of Spiegel Online
#' @description scrapes RSS feeds of of Spiegel Online home page
#' @param path Path where file with homepage in html format will be stored

scrapeSpiegelRSS <- function(folder) {
  # get RSS
  rss_feeds <- c("http://www.spiegel.de/schlagzeilen/tops/index.rss",
                 "http://www.spiegel.de/schlagzeilen/index.rss",
                 "http://www.spiegel.de/politik/index.rss",
                 "http://www.spiegel.de/wirtschaft/index.rss",
                 "http://www.spiegel.de/panorama/index.rss",
                 "http://www.spiegel.de/sport/index.rss",
                 "http://www.spiegel.de/kultur/index.rss",
                 "http://www.spiegel.de/netzwelt/index.rss",
                 "http://www.spiegel.de/wissenschaft/index.rss",
                 "http://www.spiegel.de/gesundheit/index.rss",
                 "http://www.spiegel.de/karriere/index.rss",
                 "http://www.spiegel.de/reise/index.rss",
                 "http://www.spiegel.de/auto/index.rss",
                 "http://www.spiegel.de/einestages/index.rss")
  rss_out_list <- lapply(rss_feeds, read_xml)
  # write raw RSS
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  datetime <- format(as.POSIXct(Sys.time(), tz = Sys.timezone()), usetz = TRUE)  %>% as.character() %>% str_replace_all("[ :]", "-")
  rss_feeds <- rss_feeds %>% str_replace("/index.rss$", "")
  filepaths <-paste0(folder, "/spiegel-", datetime, "-", basename(rss_feeds), ".rss")
  Map(function(x, filepath) write_xml(x, file = filepath, w, options = "format"), rss_out_list, filepaths)
}







