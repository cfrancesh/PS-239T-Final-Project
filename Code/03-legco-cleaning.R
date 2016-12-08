## course: PS239T-Final Project
## title: 03-legco-database-cleaning
## author: Chuyue Huang
## date: 12-02-2016
## description: this R code cleans and analyzes the legco database 
## I obtained using the python script, and produce the plot on the number of dicussions 
## the last part performs basic text analysis and produces a 

require(stringr)
require(base)
require(ggplot2)
require(tidyr)
require(dplyr)
require(lubridate)

#change your directory 
setwd("/Users/Chuyue/Google Drive/Berkeley/Fall 2016/Academics/PS 239T/PS-239T-Final-Project")

## functions -------
firstup <- function(x) {
  #a function to make string from all caps to only first letter cap 
  words<-strsplit(tolower(x), split=" ")[[1]]
  nwords<-length(words) #number of words 
  for (i in 1:nwords){
    substr(words[i], 1, 1)<-toupper(substr(words[i], 1, 1))
  }
  out<-paste(words, collapse=" ")
  return(out)
}


# first import the csv files created from the python script, 
# they are on 5 different subtopics 
acct=read.csv("Data/LegCo/legco_acct.csv", stringsAsFactors = F)
chex=read.csv("Data/LegCo/legco_chex.csv", stringsAsFactors = F)
condev=read.csv("Data/LegCo/legco_condev.csv", stringsAsFactors = F)
elema=read.csv("Data/LegCo/legco_elema.csv", stringsAsFactors = F)
prfm=read.csv("Data/LegCo/legco_prfm.csv", stringsAsFactors = F)

# next combine all of them into one big dataset
leg=
  rbind(acct, chex, condev, elema, prfm)%>%
  dplyr::select(-key)%>%
  unique()

# I created an id for each case, but the key variable from the API seems to have failed already
# and they were some duplicated cases 
leg$id<-seq(1:nrow(leg))
leg$issu<-factor(leg$issu, levels=unique(leg$issu), labels=c("SA", "CE", "CD", "EM", "PF"))

# output a copy of the above dataset
write.csv(leg,"Data/LegCo/legco_all_clean.csv")

#check the structure 
str(leg)
names(leg)

#start cleaning
leg1<-
  leg%>%
  mutate(date=as.Date(date, "%d.%m.%y"), #reformat the date
         year=year(date))%>% #create the year variable 
  group_by(year, issu)%>%
  mutate(nyr=n())

# discussion on press freedom, electoral matters and constitutional development
leg2<- #leg2 df only has the 3 key issues 
  leg1%>%
  filter(issu%in%c("CD", "PF", "EM"))

# plot of the number of discussions on these years since 1998 
plot1_keyissu<-
  ggplot(leg2)+
  geom_line(aes(x=year, y=nyr, color=issu))+
  labs(x="Year", 
       y="Number of Events", 
       title="Discussion on Key Constitutional Issues in HK LegCo")+
  theme_bw()+
  scale_color_discrete(name ="Issues",
                       breaks=c("CD",
                                "EM",
                                "PF"),
                       labels=c("Const.Dev",
                                "Electoral Matters",
                                "Press Freedom"))+
  scale_x_continuous(breaks=c(seq(1998, 2016, by=2)))


# save the plot in the Results folder 
ggsave("Results/keyissu.jpeg", plot1_keyissu, width = 8, height = 6)


## next part deals with extracting the motion topics from the event summaries 

## first I create regex patterns 
pattern.link="\\[.+\\]$" #get all the a href stuff 
pattern.brk="<[^<]+>" # get all xml syntax 
pattern.xml="\\n&bull;&nbsp;|\\n" #get all the xml stuff 

# remove all the extra characters from the strings 
topic<-
  leg3%>%
  dplyr::select(issu, summ, splitsumm, date, year, id)%>%
  as.data.frame()%>%
  mutate(summ=gsub(pattern.link, "", summ),
         summ=gsub(pattern.brk, "", summ),
         summ=gsub(pattern.xml, "", summ)) # cleaned 

match.motions<-
  topic%>%
  extractMatches(pattern.quote, summ)%>% #extractMatches() extracts the topics from the stirngs 
  rename(mtn=match1)%>% # rename the new variable 
  dplyr::select(id, issu, summ, mtn) 

## text analysis of the motion titles 
# create a corpus for the motion topic 
doc.motion <- Corpus(VectorSource(match.motions$mtn))
# process the text 
dtm.motion <- DocumentTermMatrix(doc.motion,
                                 control = list(tolower = TRUE,
                                                removePunctuation = TRUE,
                                                removeNumbers = TRUE,
                                                stopwords = TRUE)) 

freq.motion <- colSums(as.matrix(dtm.motion)) # generate the frequency of these words 

# least frequent terms
freq.motion[head(ord.motion)]

# most frequent
freq.motion[tail(ord.motion)]

set.seed(123)

#I create a wordcloud here based on the frequency of the motion titles 
jpeg("Plots_Figs/wc-motion.jpeg", width=3, height=3, units="in", res=300)
wordcloud(names(freq.motion), 
          freq.motion,
          random.color = T,
          scale=c(2,0.5),
          max.words = 100,
          min.freq = 5, 
          rot.per = 0.35,
          use.r.layout=F, 
          colors=brewer.pal(8, "Dark2"))
dev.off()
warnings()
## the wordcloud jpeg is saved in the "Results" folder 
