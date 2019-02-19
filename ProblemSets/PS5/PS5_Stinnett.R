library("rvest")
library("quantmod")
library("forecast")
# code for scaping Rehnquist data
url <- "https://en.wikipedia.org/wiki/List_of_United_States_Supreme_Court_cases_by_the_Rehnquist_Court"
RehnquistCourt<- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table()
RehnquistCourt<- RehnquistCourt[[1]]

head(RehnquistCourt)
# code for scaping stock data on AMD and Nvidia
getSymbols('AMD',src='yahoo')
getSymbols('NVDA',src='yahoo')
autoplot(AMD[,4])
autoplot(NVDA[,4])
cor(AMD[,4],NVDA[,4])
