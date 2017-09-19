scrapeFreitagHeadlines <- function(path)
{
  html <- download.file("http://www.Freitag.de", destfile =path)
  FreitagHome <-read_html(path)
  nodes <- paste0("h3")
  title_nodes <- html_nodes(FreitagHome, nodes)
  titles <- html_text(title_nodes)
  titles <- gsub(pattern = "\\n",replacement = "",x = titles)
  titles <- sub(pattern = " +", replacement = "", titles)
  titles <- sub("$[A-Za-z] +", "", titles)
  titles <- sub("\\s[^A-Za-z]+$", "", titles)
  title_nodes <- as.character(title_nodes)
  links <- gsub(".*href=\\\"\\s*|\\\".*", "", title_nodes)
  df <- data.frame(title=titles, url=links, 
                   time=as.character(Sys.time()), stringsAsFactors=F)
  df <- df[!is.na(df$url),] # removing empty URLs
  df <- df[!duplicated(df$url),] # removing duplicated URLs
  df <- subset(df, subset = (df$url != "<h3 class="))
  View(df)
  return(df)
}


scrapeFreitagArticle <- function(url, path)
{
  
  html <- download.file(url,  destfile = path)
  article <- read_html(path)
  
  date <- html_text(html_nodes(article, "span.issue"))
  date <- gsub("/", ",", date)
  
  
  title <- html_text(html_nodes(article, "h1.title"))
  title <- gsub("\n +|\\s[^A-Za-z]+$","", title)
  
  
  comments <- 0
  # comments <- gsub("^ *| *$", "", gsub("\n", "", comments))
  # comments <- gsub(" {2,}", " ", comments)
  
  summary <- html_text(html_nodes(article,"div.abstract.column"))
  summary <- gsub("\n +|\\s[^A-Za-z]+$","", summary)
  
  
  text <- html_text(html_nodes(article,".column, .s-article-text, .x-article-text, .js-dynamic-advertorial, .js-external-links"))
  text <- text[9:length(text)]
  text <- paste(text, collapse="\n\n")
  
  df <- data.frame(
    url, date, title, comments=ifelse(length(comments)==0, NA, comments), 
    summary=ifelse(length(summary)==0, NA, summary), text, 
    stringsAsFactors=F)
}

scrapeFreitagRSS <- function(folder) {
  # get RSS
  rss_feeds <- c("https://www.freitag.de/@@RSS",
                 "https://www.freitag.de/politik/@@RSS")
  rss_out_list <- lapply(rss_feeds, read_xml)
  # write raw RSS
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  datetime <- format(as.POSIXct(Sys.time(), tz = Sys.timezone()), usetz = TRUE)  %>% as.character() %>% str_replace_all("[ :]", "-")
  rss_feeds <- rss_feeds %>% str_replace(fixed("@@RSS"), "")
  filepaths <-paste0(folder, "/freitag-", datetime, "-", basename(rss_feeds), ".rss")
  Map(function(x, filepath) write_xml(x, file = filepath, w, options = "format"), rss_out_list, filepaths)
}


scrapeFreitagRSSarticles <- function(folderInput, folderOutput) {
  # import xmls
  xmls <- list.files(folderInput, pattern = "freitag.+rss$", full.names = TRUE)
  xmls <- xmls[(length(xmls)-1):length(xmls)] # pick only newest files
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
