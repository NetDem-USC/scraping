library(aws.alexa)
library(rvest)

#' @title GetTop_News
#' @description Uses both the aws.alexa api and web scraping to get news categories for websites
#' @param directory of dropbox folder to store the CSV file

GetTop_News <- function(drop_directory) {
  Top.News.SubCategories <- browse_categories(path='Top/News')
  Top.News.SubCategories <- Top.News.SubCategories[Top.News.SubCategories$total_listing_count != 0,]
  URLFrame = data.frame()
  for(path in Top.News.SubCategories$path){
    CatListing <- category_listing(path=path)
    for(url in CatListing$DataUrl.text){
        info <- url_parse(url)
        info$category <- path
        URLFrame <- rbind(URLFrame, info)
      }
    }
  
  
    html <- read_html('http://dmoztools.net/News/By_Subject/')
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
    
    
    
    
    
  }
  
  
  write.csv(URLFrame,file=paste0(drop_directory,'AlexaDictionary.csv'))


