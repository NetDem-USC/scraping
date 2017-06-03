setwd("~/git/scraping")

# install current version of R package
library(devtools)
install("SCRAP")
library(SCRAP)

# test downloading a few URLs
urls <- c("http://www.spiegel.de/politik/ausland/scott-pruitt-und-donald-trump-wer-ist-der-neue-chef-der-epa-a-1135234.html",
          "http://www.sueddeutsche.de/politik/sicherheitskonferenz-trump-der-unsichtbare-elefant-1.3385139",
          "http://www.focus.de/wissen/weltraum/gefahr-aus-dem-all-droht-ein-schwarzes-loch-die-erde-zu-verschlingen_id_6658237.html",
          "http://www.bild.de/politik/inland/muenchner-sicherheitskonferenz/russland-aussenminister-lawrow-auf-muenchener-sicherheitskonferenz-50493950.bild.html",
          "http://www.zeit.de/politik/deutschland/2017-02/afd-bjoern-hoecke-parteiausschluss-dresden-entschuldigung",
          "https://www.welt.de/wirtschaft/article162190193/Merkel-kontert-Kritik-an-Exportstaerke-mit-kleinem-Scherz.html",
          "http://www.t-online.de/lifestyle/gesundheit/id_80399498/umweltministerium-verbietet-fleisch-und-fisch-fuer-seine-gaeste.html")

# 1) take lists of URLs and classify as news or not
urls_df <- classify_urls(urls)
urls_df <- urls_df[urls_df$isNews==TRUE,]

# 2) download URLs
urls_df$filename <- paste0("test/urls/", gsub("/", "_", urls_df$url_full))

for (i in 1:nrow(urls_df)){
	download_url(urls_df$url_full[i], urls_df$filename[i])
}

# 3) parse URLs
urls_df$output <- paste0("test/urls_parsed/", gsub("/", "_", urls_df$url_full), ".txt")
for (i in 1:nrow(urls_df)){
	parse_html(urls_df$filename[i], urls_df$output[i])
}

#POSSIBLE ERROR
#cnn_extract contains nothing
cnn_extract <-ArticleExtractor("http://www.cnn.com/2017/06/02/us/who-commits-hate-crimes/index.html")





