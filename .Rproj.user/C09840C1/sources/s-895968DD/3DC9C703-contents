install.packages("readr")
install.packages("knitr")
library(readr)
library(jsonlite)
library(dplyr)
data107_education<-read_csv("/Users/harry88/Downloads/hw1/107data.csv")
data104_education<-read_csv("/Users/harry88/Downloads/hw1/104data.csv")
#轉換型態
data104_education$`經常性薪資-薪資`<-as.character(data104_education$`經常性薪資-薪資`)
class(data104_education$`經常性薪資-薪資`)
#把104與107年度職業名稱統一
data107_education$大職業別<-gsub("_","、",data107_education$大職業別)
data107_education$大職業別<-gsub("營建工程","營造業",data107_education$大職業別)
data104_education$大職業別<-gsub("醫療保健服務業","醫療保健業",data104_education$大職業別)
data104_education$大職業別<-gsub("部門","",data104_education$大職業別)
data104_education$大職業別<-gsub("教育服務業","教育業",data104_education$大職業別)
data104_education$大職業別<-gsub("資訊及通訊傳播業","出版、影音製作、傳播及資通訊服務業",data104_education$大職業別)
#把兩資料join
newdata<-full_join(data107_education,data104_education,by="大職業別")
#更改column name
colnames(newdata)<-gsub("x","107",colnames(newdata))
colnames(newdata)<-gsub("y","104",colnames(newdata))
newdata$年度.107<-NULL
newdata$年度.104<-NULL
colnames(newdata)[12]<-"研究所-薪資.107"
colnames(newdata)[13]<-"研究所-女/男.107"
colnames(newdata)[24]<-"研究所-薪資.104"
colnames(newdata)[25]<-"研究所-女/男.104"

#Q1
#新增欄位計算107年度大學畢業薪資 / 104年度大學畢業薪資
newdata$`大學畢業薪資提高比例`<-
  as.numeric(newdata$`大學-薪資.107`)/as.numeric(newdata$`大學-薪資.104`)
#由大到小排序
newdata<-arrange(newdata,desc(`大學畢業薪資提高比例`))
#呈現前十名的資料
head(newdata,10)
#呈現前十名的大職業別
head(newdata$大職業別,10)
#兩年度薪資比例 >1.05的欄位
More_than_5_percent<-filter(newdata,`大學畢業薪資提高比例`>1.05)
More_than_5_percent$大職業別
#主要的職業種別
More_than_5_percent$產業別<-
  strsplit(More_than_5_percent$大職業別,"-")%>%
  sapply("[", 1)
#出現次數
table(More_than_5_percent$產業別)

#Q2
newdata$`大學-女/男.104`<-as.numeric(newdata$`大學-女/男.104`)
newdata$`大學-女/男.107`<-as.numeric(newdata$`大學-女/男.107`)
#將104年度大學畢業男女薪資比例由小到大排序
newdata[complete.cases(newdata),]%>%
  filter(`大學-女/男.104`<100)%>%
  arrange(`大學-女/男.104`)%>%
  select(大職業別,`大學-女/男.104`)%>%
  head(10)%>%
  View()
#將107年度大學畢業男女薪資比例由小到大排序
newdata[complete.cases(newdata),]%>%
  filter(`大學-女/男.107`<100)%>%
  arrange(`大學-女/男.107`)%>%
  select(大職業別,`大學-女/男.107`)%>%
  head(10)%>%
  View()
#將104年度大學畢業男女薪資比例由大到小排序
newdata[complete.cases(newdata),]%>%
  filter(`大學-女/男.104`>100)%>%
  arrange(desc(`大學-女/男.104`))%>%
  select(大職業別,`大學-女/男.104`)%>%
  head(10)%>%
  View()
#將107年度大學畢業男女薪資比例由大到小排序
newdata[complete.cases(newdata),]%>%
  filter(`大學-女/男.107`>100)%>%
  arrange(desc(`大學-女/男.107`))%>%
  select(大職業別,`大學-女/男.107`)%>%
  head(10)%>%
  View()

#Q3
#取出107年度大學薪資欄位與研究所薪資欄位
grad<-select(newdata,大職業別,`研究所-薪資.107`,`大學-薪資.107`)
#新增欄位薪資差異比例 計算 107年度研究所畢業薪資/107年度大學畢業薪資
grad$薪資差異比例<-as.numeric(grad$`研究所-薪資.107`)/as.numeric(grad$`大學-薪資.107`)
#用薪資差異比例欄位由大到小排序，並呈現出前十筆
arrange(grad,desc(薪資差異比例))%>%
  head(10)%>%
  View()

#Q4
#取出出版、影音製作、傳播及資通訊服務業相關欄位
like<-grad[grepl("出版、影音製作、傳播及資通訊服務業",grad$大職業別),]
like$`研究所-薪資.107`<-as.numeric(like$`研究所-薪資.107`)
like$`大學-薪資.107`<-as.numeric(like$`大學-薪資.107`)
#新增研究所薪資與大學薪資差異欄位
like$薪資差異<-like$`研究所-薪資.107`-like$`大學-薪資.107`
