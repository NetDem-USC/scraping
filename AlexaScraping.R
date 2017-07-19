library(aws.alexa)


GetTop_News <- function(drop_direcotry) {
  Top.News.SubCategories <- browse_categories(path='Top/News')
  Top.News.SubCategories <- Top.News.SubCategories[Top.News.SubCategories$total_listing_count > 0,]
  URLFrame = data.frame()
  for(path in Top.News.SubCategories$path){
    CatListing <- category_listing(path=path)
    for(url in CatLisitng$DataURL.text){
      info <- url_parse(url)
      info$category <- path
      URLFrame <- rbind(URLFrame, info)
    }
  }
  
  write.csv(URLFrame,file=paste0(drop_directory,'AlexaDictionary.csv'))
  
}
