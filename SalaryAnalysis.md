108-2 大數據分析方法 作業一
================
yin

搞不清楚各行各業的薪資差異嗎? 念研究所到底對第一份工作的薪資影響有多大? CP值高嗎? 透過分析**初任人員平均經常性薪資**-
（107年）<https://data.gov.tw/dataset/6647>
（104-105年）<http://ipgod.nchc.org.tw/dataset/a17000000j-020066>
，可初步了解台灣近幾年各行各業、各學歷的起薪。

## 比較104年度和107年度大學畢業者的薪資資料

### 資料匯入與處理

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
s104105y<- read_csv("http://ipgod.nchc.org.tw/dataset/b6f36b72-0c4a-4b60-9254-1904e180ddb1/resource/98d5094d-7481-44b5-876a-715a496f922c/download/a17000000j-020066-mah.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
s107y <- read_csv("C:/Users/yin/Desktop/homework/107yy.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_character(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所-薪資` = col_character(),
    ##   `研究所-女/男` = col_character()
    ## )

``` r
#資料表結合
s104105y$大職業別 <- gsub("部門","",s104105y$大職業別)
tablejoin <- merge(s104105y,s107y,by = "大職業別")
#資料清洗
tablejoin$`大學-薪資.x` <- gsub("—", "",tablejoin$`大學-薪資.x`)
tablejoin$`大學-薪資.y` <- gsub("—", "",tablejoin$`大學-薪資.y`)
tablejoin$`大學-薪資.y` <- gsub("…", "",tablejoin$`大學-薪資.y`)
tablejoin$`大學-薪資.x` <- as.numeric(tablejoin$`大學-薪資.x`)
tablejoin$`大學-薪資.y` <- as.numeric(tablejoin$`大學-薪資.y`)
```

### 107年度薪資較104年度薪資高的職業有哪些?

``` r
#新增資料成長率
tablejoin$薪資成長率 <- tablejoin$`大學-薪資.y`[]/tablejoin$`大學-薪資.x`[]
tablejoin <- head(tablejoin[order(tablejoin$薪資成長率,decreasing =T),],10)

#篩選與排序
tablejoin <- tablejoin[which(tablejoin$薪資成長率 >1),]
View(tablejoin)
```

薪資成長最高的是服務業或是產業中與客戶接觸較多的服務性質的職位。 \#\#\# 提高超過5%的的職業有哪些?

``` r
#篩選與排序
tablejoin <- tablejoin[which(tablejoin$薪資成長率 >1.05),]
View(tablejoin)
```

### 主要的職業種別是哪些種類呢?

``` r
#職業分析
tableList <- tablejoin[grepl("-",c(tablejoin$大職業別)),1]
tableList <- strsplit(tableList,"-")%>%unlist()
tableoccupation <- tableList[c(T,F)] 
table(tableoccupation)
```

    ## tableoccupation
    ##         工業及服務業             不動產業 用水供應及污染整治業 
    ##                    1                    1                    1 
    ##         住宿及餐飲業               服務業         金融及保險業 
    ##                    2                    1                    2 
    ##         運輸及倉儲業 
    ##                    1

## 男女同工不同酬現況分析

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為104到107年度的大學畢業薪資。

### 104和107年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?

根據資料顯示，男女薪資與行業別並無明顯關係，但男性從事行業中的操作、專業人員薪資普遍較女性高

``` r
#重新匯入
s104105y<- read_csv("http://ipgod.nchc.org.tw/dataset/b6f36b72-0c4a-4b60-9254-1904e180ddb1/resource/98d5094d-7481-44b5-876a-715a496f922c/download/a17000000j-020066-mah.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
s107y <- read_csv("C:/Users/yin/Desktop/homework/107yy.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_character(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所-薪資` = col_character(),
    ##   `研究所-女/男` = col_character()
    ## )

``` r
#資料清洗
s104105y$大職業別 <- gsub("部門","",s104105y$大職業別)
s104105y$`大學-女/男` <- gsub("—", "",s104105y$`大學-女/男`)
s104105y$`大學-女/男` <- gsub("…", "",s104105y$`大學-女/男`)
s104105y$`大學-女/男` <- as.numeric(s104105y$`大學-女/男`)
s107y$`大學-女/男` <- gsub("—", "",s107y$`大學-女/男`)
s107y$`大學-女/男` <- gsub("…", "",s107y$`大學-女/男`)
s107y$`大學-女/男` <- as.numeric(s107y$`大學-女/男`)
#104年度男生薪資比女生多的行業
m104 <-head(s104105y[order(s104105y$`大學-女/男`,decreasing = F),],10)
View(m104)
#107年度男生薪資比女生多的行業
m107 <-head(s107y[order(s107y$`大學-女/男`,decreasing = F),],10)
View(m107)
```

### 哪些行業女生薪資比男生薪資多?

``` r
#104年女生比男生薪資高的行業
f104 <- head(s104105y[order(s104105y$`大學-女/男`,decreasing = T),],10)
View(f104)
#107年女生比男生薪資高的行業
f107 <-head(s107y[order(s107y$`大學-女/男`,decreasing = T),],10)
View(f107)
```

根據資料顯示，男女薪資與行業別並無明顯關係，但女性從事行業中的銷售、助理人員薪資普遍較男性高

## 研究所薪資差異

以107年度的資料來看，哪個職業別念研究所最划算呢 (研究所學歷薪資與大學學歷薪資增加比例最多)?

``` r
s107y <- read_csv("C:/Users/yin/Desktop/homework/107yy.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_character(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所-薪資` = col_character(),
    ##   `研究所-女/男` = col_character()
    ## )

``` r
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
```

就資料分析結果，研究所與大學畢業薪資相差最高的是其他服務業、專業科學與技術服務業，的資新差異比例最高，而土石礦、運輸業和藝術、娛樂休閒的薪資比例最低，大概顯示了較深度、專業的行業念研究所帶來的薪資差距會比較大，而需要較粗重的產業和需要創作的行業，研究所畢業與大學畢業的薪資差距較少
\#\# 我有興趣的職業別薪資狀況分析

### 有興趣的職業別篩選，呈現薪資

``` r
#取出欄位
s107yinterest <- select(s107y,大職業別,`大學-薪資`,`研究所-薪資`)
#選出有興趣的職業之薪資
s107yinterest <- 
  filter(s107yinterest,大職業別 %in% c("藝術_娛樂及休閒服務業","製造業","專業_科學及技術服務業","金融及保險業","醫療保健業"))
```

薪資差距比想像中要少，但因為資料是剛畢業的起薪，念研究所的原因可能還有之後的升遷問題 \#\#\# 這些職業別研究所薪資與大學薪資差多少呢？

``` r
s107yinterest$薪資差異 <- s107yinterest$`研究所-薪資`- s107yinterest$`大學-薪資`
```

我本身是沒有唸研究所的打算，但就分析結果來說，念研究所的差異並不是非常大
