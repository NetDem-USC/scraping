% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FrankFurt.R
\name{getFrankfurtArticle}
\alias{getFrankfurtArticle}
\title{Scrape information from a Frankfurt Allegmaine Article}
\usage{
getFrankfurtArticle(url)
}
\arguments{
\item{url}{string of article url}
}
\value{
dataframe with date, headline, summary and maintext as columns
}
\description{
scrapes the date, summary, title, and main text from an article on Frankfurt Allgemaine
}
