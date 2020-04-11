library(readr)
library(ggplot2)
library(dplyr)
library(mlbench)
library(modelr)
library(lubridate)
library(tm)
library(wordcloud)
library(tidytext)
library(tidyverse)

kickstarters<-read_csv("C:/Users/thyfe/OneDrive/Desktop/MBA Documents/DS 5110 - Introduction to Data Management/Homework #5/ks-projects-201801.csv")



######
######
######  WORD CLOUD CODE
project_stop <- tibble(word=c("art","1","2","3","4","5","2012","2013","2014",
                              "2015","2016","2017","100","comic","comics",
                              "issue","issues","volume","vol","2011","arts",
                              "dance","dancer","dancers","dancing","la",
                              "2.0","fi","movie","music","video","food",
                              "de","1st","7","ep","pre","photography",
                              "project","theatre","theater"))

kickstarters_word_cloud <- kickstarters %>% 
  as_tibble() %>% 
  filter(year(deadline)!=2018 & state=="successful")%>%
  select (name,main_category)

wordsUnigrams <-unnest_tokens(kickstarters_word_cloud,word,name) %>%
  anti_join(stop_words) %>%
  anti_join(project_stop)

wordsBigrams<-unnest_tokens(kickstarters_word_cloud,word,name,token="ngrams",n=2) %>%
  separate(word, c("part1","part2"),sep = " ")%>%
  filter(!part1 %in% stop_words$word,!part1 %in% project_stop$word)%>%
  filter(!part2 %in% stop_words$word,!part2 %in% project_stop$word)%>%
  as_tibble()

topwordsUnigrams <- wordsUnigrams %>% 
  mutate(main_category=as.factor(main_category))%>%
  group_by(main_category) %>%
  count(word) %>%
  top_n(50)

maincats<-levels(topwordsUnigrams$main_category)


topwordsUnigramsSplit<-split(topwordsUnigrams,topwordsUnigrams$main_category, drop=TRUE)

ArtUni<-topwordsUnigramsSplit[[1]]
ComicsUni<-topwordsUnigramsSplit[[2]]
CraftsUni<-topwordsUnigramsSplit[[3]]
DanceUni<-topwordsUnigramsSplit[[4]]
DesignUni<-topwordsUnigramsSplit[[5]]
FashionUni<-topwordsUnigramsSplit[[6]]
FilmnVideoUni<-topwordsUnigramsSplit[[7]]
FoodUni<-topwordsUnigramsSplit[[8]]
GamesUni<-topwordsUnigramsSplit[[9]]
JournalismUni<-topwordsUnigramsSplit[[10]]
MusicUni<-topwordsUnigramsSplit[[11]]
PhotographyUni<-topwordsUnigramsSplit[[12]]
PublishingUni<-topwordsUnigramsSplit[[13]]
TechnologyUni<-topwordsUnigramsSplit[[14]]
TheaterUni<-topwordsUnigramsSplit[[15]]


topwordsBigrams<-wordsBigrams %>%
  mutate(main_category=as.factor(main_category))%>%
  group_by(main_category)%>%
  mutate(bigram=paste(part1, part2))%>%
  count(bigram)%>%
  top_n(50)

topwordsBigramsSplit<-split(topwordsBigrams,topwordsBigrams$main_category, drop=TRUE)

ArtBi<-topwordsBigramsSplit[[1]]
ComicsBi<-topwordsBigramsSplit[[2]]
CraftsBi<-topwordsBigramsSplit[[3]]
DanceBi<-topwordsBigramsSplit[[4]]
DesignBi<-topwordsBigramsSplit[[5]]
FashionBi<-topwordsBigramsSplit[[6]]
FilmnVideoBi<-topwordsBigramsSplit[[7]]
FoodBi<-topwordsBigramsSplit[[8]]
GamesBi<-topwordsBigramsSplit[[9]]
JournalismBi<-topwordsBigramsSplit[[10]]
MusicBi<-topwordsBigramsSplit[[11]]
PhotographyBi<-topwordsBigramsSplit[[12]]
PublishingBi<-topwordsBigramsSplit[[13]]
TechnologyBi<-topwordsBigramsSplit[[14]]
TheaterBi<-topwordsBigramsSplit[[15]]



#pdf("Kickstarter Wordclouds.pdf",width=17,height = 11)
wordcloud(ArtUni$word,ArtUni$n,scale = c(6,0.5))
wordcloud(ArtBi$bigram,ArtBi$n,scale = c(6,0.5))

wordcloud(ComicsUni$word,ComicsUni$n,scale = c(6,0.5))
wordcloud(ComicsBi$bigram,ComicsBi$n,scale = c(6,0.5))

wordcloud(CraftsUni$word,CraftsUni$n,scale = c(6,0.5))
wordcloud(CraftsBi$bigram,CraftsBi$n,scale = c(6,0.5))

wordcloud(DanceUni$word,DanceUni$n,scale = c(6,0.5))
wordcloud(DanceBi$bigram,DanceBi$n,scale = c(6,0.5))

wordcloud(DesignUni$word,DesignUni$n,scale = c(6,0.5))
wordcloud(DesignBi$bigram,DesignBi$n,scale = c(6,0.5))

wordcloud(FashionUni$word,FashionUni$n,scale = c(6,0.5))
wordcloud(FashionBi$bigram,FashionBi$n,scale = c(6,0.5))

wordcloud(FilmnVideoUni$word,FilmnVideoUni$n,scale = c(6,0.5))
wordcloud(FilmnVideoBi$bigram,FilmnVideoBi$n,scale = c(6,0.5))

wordcloud(FoodUni$word,FoodUni$n,scale = c(6,0.5))
wordcloud(FoodBi$bigram,FoodBi$n,scale = c(6,0.5))

wordcloud(GamesUni$word,GamesUni$n,scale = c(6,0.5))
wordcloud(GamesBi$bigram,GamesBi$n,scale = c(6,0.5))

wordcloud(JournalismUni$word,JournalismUni$n,scale = c(6,0.5))
wordcloud(JournalismBi$bigram,JournalismBi$n,scale = c(6,0.5))

wordcloud(MusicUni$word,MusicUni$n,scale = c(6,0.5))
wordcloud(MusicBi$bigram,MusicBi$n,scale = c(6,0.5))

wordcloud(PhotographyUni$word,PhotographyUni$n,scale = c(6,0.5))
wordcloud(PhotographyBi$bigram,PhotographyBi$n,scale = c(6,0.5))

wordcloud(PublishingUni$word,PublishingUni$n,scale = c(6,0.5))
wordcloud(PublishingBi$bigram,PublishingBi$n,scale = c(6,0.5))

wordcloud(TechnologyUni$word,TechnologyUni$n,scale = c(6,0.5))
wordcloud(TechnologyBi$bigram,TechnologyBi$n,scale = c(6,0.5))

wordcloud(TheaterUni$word,TheaterUni$n,scale = c(6,0.5))
wordcloud(TheaterBi$bigram,TheaterBi$n,scale = c(6,0.5))

#dev.off()

######
######
###### Exploratory Graphs



MainCatGrouping<-kickstarters%>%
  filter(year(deadline)!=2018 & state %in% c("successful","failed"))%>%
  group_by(main_category,state)%>%
  count()

YearGrouping<-kickstarters%>%
  filter(year(deadline)!=2018 & state %in% c("successful","failed"))%>%
  mutate(year=year(deadline), year=as.factor(year))%>%
  group_by(year,state)%>%
  count()

#Bar Chart with all the groups per year
ggplot(MainCatGrouping)+geom_col(aes(x=main_category,y=(n/sum(n)),fill=state), position = "fill")+coord_flip()+ylab("Proportion")+xlab("Main Category")

ggplot(YearGrouping)+geom_col(aes(x=year,y=(n/sum(n)),fill=state), position = "fill")+coord_flip()+ylab("Proportion")+xlab("Year")

#Starters getting extra funding for their projects, column is called overage
#filter Out all the low dollar kickstarters, "fishers"
kickstarters_extra<-kickstarters%>%
  mutate(pctfunding=usd_pledged_real/usd_goal_real, overage = usd_pledged_real-usd_goal_real)%>%
  filter(usd_goal_real>100)%>%
  filter(pctfunding>0)%>%
  filter(backers>0)


##############################################
#this looks good!
ggplot(kickstarters_extra, aes(x=log2(backers), y=log2(pctfunding)))+geom_point()

test2<-lm(log2(pctfunding)~log2(backers),kickstarters_extra)
summary(test2)

kickstarters_extra%>%
  add_residuals(test2,"residuals")%>%
  ggplot(aes(x=log2(backers),y=residuals))+
  geom_point()

kickstarters_extra%>%
  add_residuals(test2,"residuals")%>%
  ggplot(aes(x=deadline,y=residuals))+
  geom_point()


test3<-lm(log2(pctfunding)~log2(backers)+deadline,kickstarters_extra)
summary(test3)



####
####
####
#Models

kickstarters_fishers<-kickstarters%>%
  mutate(pctfunding=usd_pledged_real/usd_goal_real, overage = usd_pledged_real-usd_goal_real)%>%
  filter(usd_goal_real<100)%>%
  filter(pctfunding>0)%>%
  filter(backers>0)

ggplot(kickstarters_fishers, aes(x=main_category,y=pctfunding))+
  geom_boxplot()+
  coord_flip()

test4<-lm(pctfunding~main_category,kickstarters_fishers)
summary(test4)

#Test time of year vs success
#test overage vs catagory
#glm functions for success vs failure

kickstarters_SnF<-kickstarters%>%
  filter(state %in% c("successful", "failed"))%>%
  mutate(state=as.factor(state))

#^^^ failed = 0, successful = 1 ^^^

SNFfit<-glm(formula=state~main_category+backers, family=binomial(link = "logit"),data=kickstarters_SnF)
summary(SNFfit)


successfailureCategory<-kickstarters_SnF%>%
  add_predictions(SNFfit,type = "response")%>%
  mutate(pred_state = ifelse(pred>0.5,"successful","failed"),
         correct = state==pred_state)

mean(successfailureCategory$correct)

SNFfit2<-glm(formula=state~deadline, family=binomial(link = "logit"),data=kickstarters_SnF)
summary(SNFfit2)


 successfailureDates<-kickstarters_SnF%>%
  add_predictions(SNFfit2,type = "response")%>%
  mutate(pred_state = ifelse(pred>0.5,"successful","failed"),
         correct = state==pred_state)

mean(successfailureDates$correct)

test<-lm(pctfunding~main_category, kickstarters_extra)
summary(test)

test2<-lm(pctfunding~backers,kickstarters_extra)
summary(test2)

OverageProjects2%>%
  add_residuals(test,"residuals")%>%
  ggplot(aes(x=backers,y=residuals))+
  geom_point()



test3<-lm(overage~main_category+backers, kickstarters_extrafunding)
rmse(test3, kickstarters_extrafunding)
  



test_od <- lm(usd_pledged_real~ main_category,kickstarters)
summary(test_od)
