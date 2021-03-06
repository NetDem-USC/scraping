
#' @rdname scrapegmxHeadlines
#' @export

#' @title
#' Scrape homepage of GMX

#' @author Simon Munzert, Pablo Barbera, Joshua Timm

#' @title Scrape headlines from GMX
#' @description This function takes the headlines off of GMX and returns a dataFrame with two
#' columns: titles and URLs.
#' 
#' @param path Path where file with homepage in html format will be stored

scrapegmxHeadlines <- function(path)
{
  html <- download.file("http://www.gmx.de", destfile =path)
  gmxHome <-read_html(path)
  
  nodes <- paste0(".news, .news-fullsize-item")
  title_nodes <- html_nodes(gmxHome, nodes)
  
  titles <- html_text(title_nodes)
  titles <- gsub(pattern = "\\n",replacement = "",x = titles)
  titles <- sub(pattern = " +", replacement = "", titles)
  titles <- sub("$[A-Za-z] +", "", titles)
  titles <- sub("\\s[^A-Za-z]+$", "", titles)
  titles <- gsub("   +.+", "", titles)
  titles <- gsub('\\\"', "", titles)
  
  links <- gsub(".*href=\\\"\\s*|\\\".*", "", title_nodes)
  
  df <- data.frame(title=titles, url=links, 
                   time=as.character(Sys.time()), stringsAsFactors=F)
  df <- df[!is.na(df$url),] # removing empty URLs
  df <- df[!duplicated(df$url),] # removing duplicated URLs
  
  return(df)
}


#' @rdname scrapegmxArticle
#' @export

#' @title scrapes an article off of gmx.de
#' @description Scrapes article date, content, and summary from GMX article
#' @param url string of article url
#' @param path Path where file will be stored 

scrapegmxArticle <- function(url, path)
{
  html <- download.file(url,  destfile = path)
  article <- read_html(path)
  
  date <- (html_nodes(article, "time"))
  date <- xml_attr(date, "datetime")[1]
  
  
  title <- html_text(html_nodes(article, "h1.article-title"))
  summary <- html_text(html_nodes(article,"p.intro"))
  
  
  comments <- html_text(html_nodes(article, ".article-info-comments-number"))
  
  
  text <- html_text(html_nodes(article,".article-body"))
  text <- paste(text, collapse="\n\n")
  text <- gsub("\n","",text) # some of the return carriages remain, so get rid of them.
  text <- gsub("  +", "", text) # sometimes there are long strings of whitespace, so remove any whitespace longer than 2 spaces.
  
  
  df <- data.frame(
    url, date, title, comments=ifelse(length(comments)==0, NA, comments), 
    summary=ifelse(length(summary)==0, NA, summary), text, 
    stringsAsFactors=F)
}

#' @rdname scrapegmxRSS
#' @export
#' @title Scrape RSS feeds of GMX
#' @description scrapes RSS feeds of GMX home page
#' @param path Path where file with homepage in html format will be stored


scrapegmxRSS <- function(folder) {
  # get RSS
  rss_feeds <- c("https://www.gmx.net/feeds/rss/magazine/index.rss",
                 "https://www.gmx.net/magazine/nachrichten/index.rss",
                 "https://www.gmx.net/magazine/lifestyle/index.rss",
                 "https://www.gmx.net/magazine/unterhaltung/index.rss",
                 "https://www.gmx.net/magazine/auto/index.rss",
                 "https://www.gmx.net/magazine/finanzen/index.rss",
                 "https://www.gmx.net/magazine/sport/index.rss",
                 "https://www.gmx.net/magazine/gesundheit/index.rss",
                 "https://www.gmx.net/magazine/tv/index.rss",
                 "https://www.gmx.net/magazine/digitale-welt/index.rss",
                 "https://www.gmx.net/magazine/wissen/index.rss",
                 "https://www.gmx.net/magazine/reise/index.rss")
  rss_out_list <- lapply(rss_feeds, read_xml)
  # write raw RSS
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  datetime <- format(as.POSIXct(Sys.time(), tz = Sys.timezone()), usetz = TRUE)  %>% as.character() %>% str_replace_all("[ :]", "-")
  rss_feeds <- rss_feeds %>% str_replace(fixed("index.rss"), "")
  filepaths <-paste0(folder, "/gmx-", datetime, "-", basename(rss_feeds), ".rss")
  Map(function(x, filepath) write_xml(x, file = filepath, w, options = "format"), rss_out_list, filepaths)
}

#' @rdname scrapegmxRSSarticles
#' @export
#' @title Scrape articles from RSS feeds of GMX
#' @description scrapes articles from RSS feeds of GMX home page
#' @param folderInput Folder where files with RSS feeds are located
#' @param folderOutput Folder where articles in html format will be stored
#' @param donefile File with URLs that have already been downloaded
#' @param donefile File with URLs that have already been downloaded


scrapegmxRSSarticles <- function(folderInput, folderOutput, donefile) {
  # import xmls
  xmls <- list.files(folderInput, pattern = "gmx.+rss$", full.names = TRUE)
  xmls <- xmls[(length(xmls)-11):length(xmls)] # pick only newest files
  xmls_parsed <- lapply(xmls, read_xml)
  urls_parsed <- lapply(xmls_parsed, function(x) { xml_nodes(x, "link") %>% xml_text}) %>% unlist %>% unique()
  # download article htmls
  dir.create(folderOutput, showWarnings = FALSE, recursive = TRUE)
  urls_articles <- urls_parsed %>% unlist 

  # excluding URLs already downloaded
  if (file.exists(donefile)) done <- scan(donefile, what="character") 
  if (!file.exists(donefile)) done <- c()
  out <- file(donefile, "a")
  urls_articles <- urls_articles[urls_articles %in% done == FALSE]

  sapply(urls_articles, function(x){
    destfile <- paste0(folderOutput, "/", basename(x), ".html")
    if(!file.exists(destfile)) {
        res <- evalWithTimeout({try(download.file(x, destfile = destfile, method = "libcurl"))},
                        timeout = 10,
                        onTimeout = "silent")
        if (!is.null(res)){writeLines(x, con=out)}
    }
  })
  close(out)
}





