library(jsonlite)
library(dplyr)
library(readr)

s104105y<- read_csv("http://ipgod.nchc.org.tw/dataset/b6f36b72-0c4a-4b60-9254-1904e180ddb1/resource/98d5094d-7481-44b5-876a-715a496f922c/download/a17000000j-020066-mah.csv")
s107y <- read_csv("C:/Users/yin/Desktop/homework/107yy.csv")
#資料表結合
s104105y$大職業別 <- gsub("部門","",s104105y$大職業別)
tablejoin <- merge(s104105y,s107y,by = "大職業別")
#資料清洗
tablejoin$`大學-薪資.x` <- gsub("—", "",tablejoin$`大學-薪資.x`)
tablejoin$`大學-薪資.y` <- gsub("—", "",tablejoin$`大學-薪資.y`)
tablejoin$`大學-薪資.y` <- gsub("…", "",tablejoin$`大學-薪資.y`)

tablejoin$`大學-薪資.x` <- as.numeric(tablejoin$`大學-薪資.x`)
tablejoin$`大學-薪資.y` <- as.numeric(tablejoin$`大學-薪資.y`)
#新增資料成長率
tablejoin$薪資成長率 <- tablejoin$`大學-薪資.y`/tablejoin$`大學-薪資.x`
tablejoin <- head(tablejoin[order(tablejoin$薪資成長率,decreasing =T),],10)

#篩選與排序
tablejoin <- tablejoin[which(tablejoin$薪資成長率 >1.05),]
tableJoin$薪資成長率 <- sort(tablejoin$薪資成長率,decreasing = T) 

#職業分析
tableList <- tableGrowing[grepl("-",c(tableGrowing$大職業別)),1]
tableList <- strsplit(tableList,"-")%>%unlist()
tableoccupation <- tableList[c(T,F)] 

table(tableoccupation)
-------------------------------------------
  library(readr)
library(dplyr)
s104105y<- read_csv("http://ipgod.nchc.org.tw/dataset/b6f36b72-0c4a-4b60-9254-1904e180ddb1/resource/98d5094d-7481-44b5-876a-715a496f922c/download/a17000000j-020066-mah.csv")
s107y <- read_csv("C:/Users/yin/Desktop/homework/107yy.csv")
#資料清洗
s104105y$大職業別 <- gsub("部門","",s104105y$大職業別)
s104105y$`大學-女/男` <- gsub("—", "",s104105y$`大學-女/男`)
s104105y$`大學-女/男` <- gsub("…", "",s104105y$`大學-女/男`)
s104105y$`大學-女/男` <- as.numeric(s104105y$`大學-女/男`)
s107y$`大學-女/男` <- gsub("—", "",s107y$`大學-女/男`)
s107y$`大學-女/男` <- gsub("…", "",s107y$`大學-女/男`)
s107y$`大學-女/男` <- as.numeric(s107y$`大學-女/男`)

#104年男女薪資差異最大前十項
m104 <-head(s104105y[order(s104105y$`大學-女/男`,decreasing = T),],10)
f104 <- head(s104105y[order(s104105y$`大學-女/男`,decreasing = F),],10)
#107年男女薪資差異最大前十項
m107 <-head(s107y[order(s107y$`大學-女/男`,decreasing = T),],10)
f107 <-head(s107y[order(s107y$`大學-女/男`,decreasing = F),],10)
-------------------------------------------------------------------------
  library(readr)
library(dplyr)
s107y <- read_csv("C:/Users/yin/Desktop/homework/107yy.csv")
  #資料清洗
s107y$`大學-薪資` <- gsub("—", "",s107y$`大學-薪資`)
s107y$`大學-薪資` <- gsub("…", "",s107y$`大學-薪資`)
s107y$`大學-薪資` <- as.numeric(s107y$`大學-薪資`)
s107y$`研究所-薪資` <- gsub("—", "",s107y$`研究所-薪資`) 
s107y$`研究所-薪資` <- gsub("…", "",s107y$`研究所-薪資`)
s107y$`研究所-薪資` <- as.numeric(s107y$`研究所-薪資`)
#取出欄位
s107ysalary <- select(s107y,大職業別,`大學-薪資`,`研究所-薪資`)
#新增薪資差異
s107ysalary$'研究所薪資 / 大學薪資' <- s107ysalary$`研究所-薪資`[]/s107ysalary$`大學-薪資`[]

s107ysalary <- s107ysalary[order(s107ysalary$`研究所薪資 / 大學薪資`,decreasing = T),]
------------------------------------------
  library(readr)
library(dplyr)
s107y <- read_csv("C:/Users/yin/Desktop/homework/107yy.csv")
#資料清洗
s107y$`大學-薪資` <- gsub("—", "",s107y$`大學-薪資`)
s107y$`大學-薪資` <- gsub("…", "",s107y$`大學-薪資`)
s107y$`大學-薪資` <- as.numeric(s107y$`大學-薪資`)
s107y$`研究所-薪資` <- gsub("—", "",s107y$`研究所-薪資`) 
s107y$`研究所-薪資` <- gsub("…", "",s107y$`研究所-薪資`)
s107y$`研究所-薪資` <- as.numeric(s107y$`研究所-薪資`)
#取出欄位
s107yinterest <- select(s107y,大職業別,`大學-薪資`,`研究所-薪資`)
#選出有興趣的職業
s107yinterest <- 
  filter(s107yinterest,大職業別 %in% c("藝術_娛樂及休閒服務業","製造業","專業_科學及技術服務業","金融及保險業","醫療保健業"))
s107yinterest$薪資差異 <- s107yinterest$`研究所-薪資`- s107yinterest$`大學-薪資`
