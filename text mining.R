#Tüm paketlerin yüklenmesi
install.packages("twitteR") #Twitter veri cekme için kullanýlýr.
install.packages("ROAuth") #Twitter'da ki uygulamaya giris yapmak ve iletisim kurmak icin kullanilirr.
install.packages("openssl") #imzalar ve sertifakalar icin arac seti
install.packages("httpuv") #HTTP ve WebSocket Sunucu Kitapligi
install.packages("tm") #veri madenciliÄŸi iÃ§in kullanÄ±lÄ±r.
install.packages("readxl") #ecxel dosyalarini okur
install.packages("tidytext")
install.packages("wordcloud") #kelime bulutu oluþturmada kullanýlýr. 
install.packages("ggplot2") #olusturacagimiz grafiklerini görüntülemek icin kullaniriz.
install.packages("stringr") #String verilere yani metinsel verilere manipülasyon icin kullanilir.
install.packages("writexl") #verileri excel formatýna aktarmak icin kullanýlýr. 

library(twitteR)
library(ROAuth)
library(openssl)
library(httpuv)
library(stringi)
library(stringr)
library(tm)
library(readxl)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(writexl)


options(httr_oauth_cache=T)


api_key <- "S140N9Qq5vY1MGCZaYdEu0r01" 
api_key_secret <- "hcHP3zZwozqS5hPp7PKidU32S3ssS6NGJmTH5A0wjYCGKV0Gz7"
access_token <- "4753092995-eWCKRFDNWLVXSZEoPz7T7zOHABTNUuJARouKOKw"
access_token_secret <- "C7x4kNGo8tUvUefyMZXVzePQwiJRVh3cgjGmryYDy2H0s"


setup_twitter_oauth(api_key,api_key_secret,access_token,access_token_secret)  

tweets<- searchTwitteR('#kitap', n=10000,  locale = "tr_TR")

tweets.df <- twListToDF(tweets)
tweet_clean <- tweets.df

tweet_clean$text <- stri_enc_toutf8(tweet_clean$text)

#RT ifadelerinin kaldirilmesi

tweet_clean$text <- ifelse(str_sub(tweet_clean$text,1,2) == "RT",
                           substring(tweet_clean$text,3),
                           tweet_clean$text)

#URL linklerinin kaldirilmasi 

tweet_clean$text <- str_replace_all(tweet_clean$text, "http[^[:space:]]*", "")

#hastag "#" ve "@" isaretlerinin temizlenmesi 
tweet_clean$text <- str_replace_all(tweet_clean$text, "#\\S+", "")
tweet_clean$text <- str_replace_all(tweet_clean$text, "@\\S+", "")

#noktalama isaretlerinin kaldirilmesi 
tweet_clean$text <- str_replace_all(tweet_clean$text, "[[:punct:][:blank:]]+", " ")

#tum harflerin kucuk harfe donusturulmesi
tweet_clean$text  <- str_to_lower(tweet_clean$text, "tr")


#Rakamlarin temizlenmesi 
tweet_clean$text <- removeNumbers(tweet_clean$text)

#ASCII formatina uymayan karaketerlerin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "[<].*[>]", "")
tweet_clean$text <- gsub("\uFFFD", "", tweet_clean$text, fixed =  TRUE)
tweet_clean$text <- gsub("\n", "", tweet_clean$text, fixed =  TRUE)

#Alfabetik olmayan karakterlerin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "[^[:alnum:]]"," " )

#etkisiz kelimelerin stopwords analizinden cikarilmasi
Turkish_Stopwords <- read.csv("Turkish-Stopwords.csv")
head(Turkish_Stopwords)
 

#tidytext analizi
#install.packages("tidytext")
library(tidytext)
library(dplyr)
library(ggplot2)

tidy_tweets <- tweet_clean %>% select(text) %>% 
  mutate(linenumber = row_number()) %>% unnest_tokens(word, text)
tidy_tweets <- tidy_tweets %>% anti_join(Turkish_Stopwords, by=c("word"="STOPWORD"))
head(tidy_tweets)


tidy_tweets %>%
  count(word, sort = TRUE) %>%
  filter(n >250) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + theme_minimal() +
  ggtitle("Tweetlerde en cok kullanilan kelimeler")

#Kelime bulutu haline getirme
tidy_tweets %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


#Excel formatinda export alma 
#install.packages("writexl")
library("writexl")

write_xlsx(tweet_clean, "D:/Veriler/temizveri_13 ocak.xlsx")

#21 günlük birleþtirilmiþ olan veri setinin içeriye aktarýlmasý "temizveri.csv" dosyasý
temizveri =read.table(file.choose(), header = T, sep = ";")

#21 gÜnlük verilerden etkisiz kelimelerin çýkartýlmasý
temiz_twit <- temizveri %>% select(text) %>% 
  mutate(linenumber = row_number()) %>% unnest_tokens(word, text)
temiz_twit <- temiz_twit %>% anti_join(Turkish_Stopwords, by=c("word"="STOPWORD"))
head(temiz_twit)

#Twitlerde toplamda 250'den daha fazla kullanýlan kelimelerin listelenmesi
temiz_twit %>%
  count(word, sort = TRUE) %>%
  filter(n > 250) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + theme_minimal() +
  ggtitle("Tweetlerde en çok kullanýlan kelimeler")

#Kelime bulutu
temiz_twit %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

