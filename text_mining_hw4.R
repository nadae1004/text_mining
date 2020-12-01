library(tidyverse);  
apt.tran=read.csv("apt.csv",header = TRUE, fileEncoding = "CP949", encoding = "UTF-8");
head(apt.tran)

# 아파트 매매 실거래 자료 전처리 작업
apt.tran=apt.tran[,-c(1)]
rownames(apt.tran)=NULL;
apt.tran$price=as.numeric(gsub("," , "" , apt.tran$price))*10;
apt.tran$buildyear=as.numeric(apt.tran$buildyear);
apt.tran$year=as.numeric(apt.tran$year);
apt.tran$mon=as.numeric(apt.tran$mon);
apt.tran$area=as.numeric(apt.tran$area);
apt.tran$loccode=as.numeric(apt.tran$loccode);
apt.tran$floor=as.numeric(apt.tran$floor);
apt.tran$yymmdd=paste0(apt.tran$year, str_pad(apt.tran$mon, 2, pad=0),
                       str_pad(sapply(str_split(apt.tran$date, "~"), "[[",1),2,pad=0)) %>%
  as.Date(format="%Y%m%d");
head(apt.tran); 

#1-1
# 월별 분석을 위해 거래일을 해당 월의 1일로 변환
library(lubridate);
(apt.agg=apt.tran %>% mutate(aggdate=floor_date(yymmdd, unit="month")) %>%
   group_by(aggdate) %>% summarise(price=mean(price, na.rm=T), n=n()) %>% ungroup());
(apt.agg[apt.agg$price==max(apt.agg$price),]);

#1-2
# 매매가격 시계열 시각화
library(scales); x11(); windows(width=7.0, height=5.5);
ggplot(apt.agg, aes(x=aggdate, y=price/1000)) +
  geom_line(color="darkorange", size=1.5) +
  geom_smooth(method="loess", color="firebrick1", size=1.2) + 
  scale_x_date(date_labels="%Y-%m", date_breaks="3 month", expand=c(0,0)) +
  scale_y_continuous(labels=comma) + labs(x="", y="매매가(백만원)",
                                          title="아파트 매매 실거래", subtitle="서울지역 아파트 매매가 추이",
                                          caption="출처: 국토교통부")+ theme_gray() ;
# 거래량 시계열 시각화
library(scales); x11(); windows(); windows(width=7.0, height=5.5);
ggplot(apt.agg, aes(x=aggdate, y=n)) +
  geom_line(color="darkorange", size=1.5) + geom_smooth(method="loe
ss", color="firebrick1", size=1.2) +
  scale_x_date(date_labels="%Y-%m", date_breaks="3 month", expand=c(0,0)) +
  scale_y_continuous(labels=comma) + labs(x="", y="거래량", title="아파트 매매 실거래",
                                          subtitle="서울지역 아파트 거래량 추이", caption="출처: 국토교통부")+ theme_gray() ;

#1-3
# 구별 아파트 매매가 평균/ 거래량 계산
library(lubridate);
(apt.agg2=apt.tran %>% mutate(aggdate=floor_date(yymmdd, unit="month")) %>%
   group_by(loccode, aggdate) %>% summarise(price=mean(price, na.rm=T), n=n()) %>%
   ungroup());
(apt.agg2[apt.agg2$price==max(apt.agg2$price),]); #11170:용산구

#1-4
# 구별 데이터 매매가격 시계열 시각화
library(scales); x11(); windows(width=7.0, height=5.5);
ggplot(apt.agg, aes(x=aggdate, y=price/1000)) + geom_point(color="darkturquoise") +
  geom_smooth(method="loess", color="firebrick1", size=1.2) +
  scale_x_date(date_labels="%Y-%m", date_breaks="years", expand=c(0,0)) +
  scale_y_continuous(labels=comma) + facet_wrap(~loccode, scale="free_y", ncol=5) +
  labs(x="", y="매매가(백만원)", title="아파트 매매 실거래",
       subtitle="서울지역 아파트 매매가 추이", caption="출처: 국토교통부")+ theme_bw() ;

#1-5
(apt.agg2[apt.agg2$n==max(apt.agg2$n),]); #11290:성북구

#2-1
library(tidyverse);
load("vote.Rdata");

nrow(filter(tweet.vote, query=="Trumph")); 
nrow(filter(tweet.vote, query=="Biden")); 
min(tweet.vote$created_at); max(tweet.vote$created_at);  # 발생 시점 검색

# 키워드 별 가장 오래된 트윗의 발생 시점 확인 
stime.Trumph=min(filter(tweet.vote, query=="Trumph")$created_at); 
stime.Biden=min(filter(tweet.vote, query=="Biden")$created_at); 
stime.vote=max(stime.Trumph, stime.Biden); stime.vote; 
tweet.vote2=filter(tweet.vote, created_at>=stime.vote);

library(lubridate);
tweet.vote3=tweet.vote2 %>% group_by(query) %>%
  mutate(time=floor_date(x=created_at, unit="minute")) %>%
  count(query, time) %>% slice(2:(n()-1)) %>% ungroup();
tweet.vote3;

# 시각화
x11(); windows(width=7.0, height=5.5); Sys.setlocale("LC_TIME", "English");
ggplot(tweet.vote3, aes(x=time, y=n, fill=query, color=query))+
  geom_area(position="identity", alpha=0.3)+geom_line(size=1.5)+
  scale_fill_manual(labels=c("apple iphone", "Samsung galaxy"),
                    values=c("orangered", "deepskyblue2"))+
  scale_color_manual(labels=c("apple iphone", "Samsung galaxy"),
                     values=c("orangered", "deepskyblue2"))+
  scale_x_datetime(date_labels="%b %d %H:%M", date_breaks="12 hours")+
  labs(x=NULL, y="Number of Tweets", title="Twitter Statuses over Time",
       subtitle="Tweets on topics of apple iphone and Samsung galaxy",
       caption="Source: Twitter") + theme_minimal() +
  theme(plot.title=element_text(face="bold"), axis.text=element_text(face="bold"),
        axis.text.x=element_text(size=8, angle=15, vjust=0.5),
        panel.grid.minor=element_blank(), legend.position="bottom", legend.title=element_blank());
Sys.setlocale();

#2-2
(tweet.vote3[tweet.vote3$ n==max(tweet.vote3$ n),]);

#3-1
g1 = searchTwitter(enc2utf8('삼성'), n=1500, lang='ko') 
galaxy_tweet = sapply(g1, function(x) x$getText());
save(galaxy_tweet, file='galaxy.Rdata');

#3-2
(myword=unlist(str_extract_all(galaxy_tweet, boundary("word")))); 
(sum(str_count(myword, "보상")));
