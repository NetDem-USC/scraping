
#' @rdname scrapeJungefreiheitHeadlines
#' @export

#' @title
#' Scrape homepage of Jungefreiheit

#' @author Simon Munzert, Pablo Barbera, Joshua Timm

#' @title Scrape headlines from Jungefreiheit
#' @description This function takes the headlines off of Jungefreiheit and returns a dataFrame with two
#' columns: titles and URLs.
#' 
#' @param path Path where file with homepage in html format will be stored

scrapeJungefreiheitHeadlines <- function(path)
{
  html <- download.file("http://www.Jungefreiheit.de", destfile =path)
  JungefreiheitHome <-read_html(path)

  nodes <- paste0("h2, .entry-title")
  title_nodes <- html_nodes(JungefreiheitHome, nodes)
  titles <- html_text(title_nodes)
  links <- gsub(".*href=\\\"\\s*|\\\".*", "", title_nodes)
  df <- data.frame(title=titles, url=links, 
                   time=as.character(Sys.time()), stringsAsFactors=F)
  df <- df[!is.na(df$url),] # removing empty URLs
  df <- df[!duplicated(df$url),] # removing duplicated URLs
  
  return(df)
}


#' @rdname scrapeJungefreiheitArticle
#' @export

#' @title scrapes an article off of Jungefreiheit.de
#' @description Scrapes article date, content, and summary from Jungefreiheit article
#' @param url string of article url
#' @param path Path where file will be stored 

scrapeJungefreiheitArticle <- function(url, path)
{
  html <- download.file(url,  destfile = path)
  article <- read_html(path)
  
  date <- (html_nodes(article, "time.entry-time"))
  date <- xml_attr(date, "datetime")
  
  
  title <- html_text(html_nodes(article, "h1.entry-title"))
  summary <- html_text(html_nodes(article,"p"))[4]
  
  
  comments <- html_text(html_nodes(article, ".sh-comment-count"))[1]
  
  
  text <- html_text(html_nodes(article,".entry-content"))
  text <- paste(text, collapse="\n\n")
  text <- gsub("\n","",text) # some of the return carriages remain, so get rid of them.
  
  
  df <- data.frame(
    url, date, title, comments=ifelse(length(comments)==0, NA, comments), 
    summary=ifelse(length(summary)==0, NA, summary), text, 
    stringsAsFactors=F)
}


#' @rdname scrapeJungefreiheitRSS
#' @export
#' @title Scrape RSS feeds of Jungefreiheit
#' @description scrapes RSS feeds of Jungefreiheit home page
#' @param path Path where file with homepage in html format will be stored


scrapeJungefreiheitRSS <- function(folder) {
  # get RSS
  rss_feeds <- c("https://jungefreiheit.de/kategorie/politik/index.rss",
                 "https://jungefreiheit.de/kategorie/wirtschaft/index.rss",
                 "https://jungefreiheit.de/kategorie/kultur/index.rss",
                 "https://jungefreiheit.de/kategorie/wissen/index.rss",
                 "https://jungefreiheit.de/kategorie/debatte/index.rss",
                 "https://jungefreiheit.de/service/archiv/index.rss")
  rss_out_list <- lapply(rss_feeds, read_xml)
  # write raw RSS
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  datetime <- format(as.POSIXct(Sys.time(), tz = Sys.timezone()), usetz = TRUE)  %>% as.character() %>% str_replace_all("[ :]", "-")
  rss_feeds <- rss_feeds %>% str_replace(fixed("index.rss"), "")
  filepaths <-paste0(folder, "/Jungefreiheit-", datetime, "-", basename(rss_feeds), ".rss")
  Map(function(x, filepath) write_xml(x, file = filepath, w, options = "format"), rss_out_list, filepaths)
}

#' @rdname scrapeJungefreiheitRSSarticles
#' @export
#' @title Scrape articles from RSS feeds of Jungefreiheit
#' @description scrapes articles from RSS feeds of Jungefreiheit home page
#' @param folderInput Folder where files with RSS feeds are located
#' @param folderOutput Folder where articles in html format will be stored
#' @param donefile File with URLs that have already been downloaded


scrapeJungefreiheitRSSarticles <- function(folderInput, folderOutput) {
  # import xmls
  xmls <- list.files(folderInput, pattern = "Jungefreiheit.+rss$", full.names = TRUE)
  xmls <- xmls[(length(xmls)-11):length(xmls)] # pick only newest files
  xmls_parsed <- lapply(xmls, read_xml)
  urls_parsed <- lapply(xmls_parsed, function(x) { xml_nodes(x, "link") %>% xml_text}) %>% unlist %>% unique()
  # download article htmls
  dir.create(folderOutput, showWarnings = FALSE, recursive = TRUE)
  urls_articles <- urls_parsed %>% unlist 
  sapply(urls_articles, function(x){
    destfile <- paste0(folderOutput, "/", basename(x), ".html")
    if(!file.exists(destfile)) {
      evalWithTimeout({try(download.file(x, destfile = destfile, method = "libcurl"))},
                      timeout = 10,
                      onTimeout = "silent")
    }
  })
}
