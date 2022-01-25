#Paketlerin Yüklenmesi
install.packages("twitteR")
install.packages("ROAuth")
install.packages("ROAuth")
install.packages("hms")
install.packages("lubridate")
install.packages("tidytext")
install.packages("tm")
install.packages("wordcloud")
install.packages("igraph")
install.packages("glue")
install.packages("networkD3")
install.packages("rtweet")
install.packages("plyr")
install.packages("stringr")
install.packages("networkD3")
install.packages("ggplot2")
install.packages("ggeasy")
install.packages("plotly")
install.packages("dplyr")
install.packages("hms")
install.packages("magrittr")
install.packages("tidyverse")
install.packages("janeaustenr")
install.packages("widyr")

#Paketlerin aktif edilmesi
library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud)
library(igraph)
library(glue)
library(networkD3)
library(rtweet)
library(plyr)
library(stringr)
library(ggplot2)
library(ggeasy)
library(plotly)
library(dplyr)  
library(hms)
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)

options(httr_oauth_cache=T)

api_key <- "S140N9Qq5vY1MGCZaYdEu0r01" 
api_key_secret <- "hcHP3zZwozqS5hPp7PKidU32S3ssS6NGJmTH5A0wjYCGKV0Gz7"
access_token <- "4753092995-eWCKRFDNWLVXSZEoPz7T7zOHABTNUuJARouKOKw"
access_token_secret <- "C7x4kNGo8tUvUefyMZXVzePQwiJRVh3cgjGmryYDy2H0s"


setup_twitter_oauth(api_key,api_key_secret,access_token,access_token_secret)  
# "#kitap" etiketiyle atýlmýþ 10000 twitin çekilmesi
tweets <- searchTwitteR("#kitap", n=10000,  locale = "tr_TR")

n.tweet <- length(tweets)

# tweetleri veri çerçevesine dönüþtürme
tweets.df <- twListToDF(tweets)

tweets.txt <- sapply(tweets, function(t)t$getText())
# Giriþ hatalarýný önlemek için grafiksel Parametreleri yoksayma
tweets.txt <- str_replace_all(tweets.txt,"[^[:graph:]]", " ")

#ön iþleme metni:
clean.text = function(x)
{
  # küçük harfe dönüþtürme
  x = tolower(x)
  # rt ifadelerinin kaldýrýlmasý
  x = gsub("rt", "", x)
  # @ iþaretlerinin kaldýrýlmasý
  x = gsub("@\\w+", "", x)
  # noktalama iþaretlerinin kaldýrýlmasý
  x = gsub("[[:punct:]]", "", x)
  # sayýlarýn kaldýrýlmasý
  x = gsub("[[:digit:]]", "", x)
  # link uzantýlarýnýn kaldýrýlmasý
  x = gsub("http\\w+", "", x)
  # sekmelerin kaldýrýlmasý
  x = gsub("[ |\t]{2,}", "", x)
  # baþtaki boþluklarýn kaldýrýlmasý
  x = gsub("^ ", "", x)
  # baþtaki boþluklarýn kaldýrýlmasý
  x = gsub(" $", "", x)
  # diðer gereksiz öðelerin kaldýrýlmasý
  x = gsub('https://','',x)
  x = gsub('http://','',x)
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:punct:]]', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = str_replace_all(x,"[^[:graph:]]", " ")
  return(x)
}

cleanText <- clean.text(tweets.txt)
# boþ sonuçlarý kaldýr (varsa)
idx <- which(cleanText == " ")
cleanText <- cleanText[cleanText != " "]

tweets.df %<>% 
  mutate(
    created = created %>% 
      # Sýfýrlarý kaldýrýn.
      str_remove_all(pattern = '\\+0000') %>%
      #Ayrýþtýrma tarihi..
      parse_date_time(orders = '%y-%m-%d %H%M%S')
  )

tweets.df %<>% 
  mutate(Created_At_Round = created%>% round(units = 'hours') %>% as.POSIXct())
#En önce ve en son atýlan twitlerin gösterilmesi
tweets.df %>% pull(created) %>% min()
tweets.df %>% pull(created) %>% max()
#Atýlan twitlerin saatlere göre sayýsýnýnýn grafik halinde gösterilmesi
plt <- tweets.df %>% 
  dplyr::count(Created_At_Round) %>% 
  ggplot(mapping = aes(x = Created_At_Round, y = n)) +
  theme_light() +
  geom_line() +
  xlab(label = 'Tarih') +
  ylab(label = NULL) +
  ggtitle(label = 'Saat Baþýna Tweet Sayýsý')

plt %>% ggplotly()
        
#pozitif ve negatif kelimelerin içe aktarýlmasý
positive <- read_csv("positive-words.csv")
negative <- read_csv("negative-words.csv")

# pozitif ve negatif kelimelere ekleme yapýlmasý
pos.words = c(positive,'güzel','iyi','mükemmel','beðendim','olmuþ','özgün','doðru','etkin','hatasýz','etkiliyeci','büyüleyici','sürükleyici','tatlý','sevimli','heycanlý')

neg.words = c(negative,'Bearish','berbat', 'yanlýþ' ,'az', 'beðenmedim' ,'sat' ,'alçak', 'destek', 'güvensiz' ,'olumsuz' ,
              'alýnmaz' ,"düþtü",'zarar','beðenmedim','kötü','iyi deðil','delist','olmaz','sabýrsýz','yapmaz','düþüþ','pahalý','uzun','sýkýcý'
              ,'dondu')
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # Cümle vektörlerini girdi olarak veriyoruz.
  # plyr bir listeyi veya vektörü bizim için "l" olarak iþleyecek
  # basit bir puan dizisini geri istiyoruz, bu yüzden "l" + "a" + "ply" = laply kullanýyoruz:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # cümleleri R'nin regex güdümlü global ikamesi, gsub() iþleviyle temizleyelim
    sentence = gsub('https://','',sentence)
    sentence = gsub('http://','',sentence)
    sentence = gsub('[^[:graph:]]', ' ',sentence)
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = str_replace_all(sentence,"[^[:graph:]]", " ")
    #ve küçük harfe dönüþtürülmesi
    sentence = tolower(sentence)
    
    # kelimelerin str_splint ile bölünmesi
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # kelimelerimizi pozitif ve negatif terimler sözlükleriyle karþýlaþtýrlmasý
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    
    # sadece DOÐRU/YANLIÞ çevirilmesi
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # DOÐRU/YANLIÞ 1 vevya 0 olarak deðerlendirilecektir:
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
analysis <- score.sentiment(cleanText, pos.words, neg.words)
# duyarlýlýk puaný frekans tablosu
table(analysis$score)

analysis %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "lightblue")+ 
  ylab("Frekans") + 
  xlab("Duygu Skoru") +
  ggtitle("Tweetlerin Duyarlýlýk Puanlarýnýn Daðýlýmý") +
  ggeasy::easy_center_title()

neutral <- length(which(analysis$score == 0))
positive <- length(which(analysis$score > 0))
negative <- length(which(analysis$score < 0))
toplam=positive+neutral+negative
Sentiment <- c("Pozitif","Nötr","Negatif")

Count <- c((positive/toplam)*100,(neutral/toplam)*100,(negative/toplam)*100)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)
ggplot(output, aes(x=Sentiment,y=Count,))+

  geom_bar(stat = "identity", aes(fill = Sentiment ))+
  ggtitle("Duygu Analizine Göre Pozitif, Negatif ve Nötr Kelimelerin Yüzde Grafiði")
head((positive/toplam)*100,"Pozitif")
head((neutral/toplam)*100 ,"Nötr")
head((negative/toplam)*100 ,"Negatif")



