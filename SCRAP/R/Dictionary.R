#' @title getAlexa
#' @description Uses both the aws.alexa api and web scraping to get news categories for websites
#' @param path path to be scraped

#' @import aws.alexa
#' @import rvest

getAlexa <- function(path="Top/News") {
  Top.News.SubCategories <- browse_categories(path=path)
  Top.News.SubCategories <- Top.News.SubCategories[Top.News.SubCategories$total_listing_count != 0,]
  URLFrame = data.frame()
  for(path in Top.News.SubCategories$path){
    i = 0
    info <- data.frame()
    CatListing <- category_listing(path=path, start=i)
    # scrape Alexa, and keep running until no more domains
    while (!is.null(CatListing)){
      i <- i + 20
      info <- rbind(info, CatListing)
      CatListing <- category_listing(path=path, start=i)
    }
    info$domain <- NA
    info$path <- NA
    # split URL into components
    for(i in 1:nrow(info)){
        urldata <- url_parse(info$DataUrl.text[i])
        info$domain[i] <- urldata$domain
        info$path[i] <- path
    }
    URLFrame <- rbind(URLFrame, info)
    }
  
  return(URLFrame)   
  #write.csv(URLFrame,file=paste0(drop_directory,'AlexaDictionary.csv'))  
}

#' @title getDMOZ
#' @description Scrapes the DMOZ website for list of Domains
#' @param path path to be scraped  
  
 
getDMOZ <- function(path="News/By_Subject/"){

  URLFrame = data.frame()

    html <- read_html(paste0('http://dmoztools.net/', path))
    links <- html_nodes(html,'#cat-list-content-2 a')
    subjects <- xml_attr(links,'href')
    for(path in subjects) {
      page <- read_html(paste0('http://dmoztools.net',path))
      site_section <- html_nodes(page,'.title-and-desc a')
      site_linkes <- xml_attr(site_section,'href')
      for(link in site_linkes) {
        info <- url_parse(url)
        info$category <- path
        URLFrame <- rbind(URLFrame, info)
     }
  }
    
  #write.csv(URLFrame,file=paste0(drop_directory,'DMOZDictionary.csv'))  
  return(URLFrame)

}



