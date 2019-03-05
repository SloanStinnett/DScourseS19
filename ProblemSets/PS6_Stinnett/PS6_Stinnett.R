
library("ggplot2")
library("reshape2")
library("rvest")
#data collection from wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_United_States_Supreme_Court_cases_by_the_Rehnquist_Court"
RehnquistCourt<- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table()
RehnquistCourt<- RehnquistCourt[[1]]

head(RehnquistCourt)
#data manipulation
case.names.split<-colsplit(RehnquistCourt$`Case name`,"v.",names=c("prosecution","defense"))

RehnquistCourt<-cbind(case.names.split,RehnquistCourt)

#table for prosecutors
mytable<-as.data.frame(table(RehnquistCourt$prosecution))

mytable<-mytable[order(mytable[,2],decreasing = TRUE),]

"table for prosecutors"<-ggplot(mytable[1:25,],aes(x=mytable[1:25,]$Var1,y=mytable[1:25,]$Freq))+ 
  geom_col(color="red")+
  labs(x="prosecutor",y="# of cases")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Prosecutors that appeared before the Rehnquist court the most")
`table for prosecutors`
ggsave(filename = "tableforprosecutors.jpg",plot = `table for prosecutors`)

# table for defendants
mytable2<-as.data.frame(table(RehnquistCourt$defense))

mytable2<-mytable2[order(mytable2[,2],decreasing = TRUE),]

"table for defendants"<-ggplot(mytable2[1:25,],aes(x=mytable2[1:25,]$Var1,y=mytable2[1:25,]$Freq))+
  geom_col(color="blue")+
  labs(x="defendant",y="# of cases")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("defendants that appeared before the Rehnquist court the most")
`table for defendants`
ggsave(filename = "tablefordefendants.jpg",plot = `table for defendants`)
