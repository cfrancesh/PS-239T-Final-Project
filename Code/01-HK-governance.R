## course: PS239T-Final Project
## title: 01-HK-governance
## author: Chuyue Huang
## date: 12-02-2016
## description: this R code cleans and analyzes the WGI data and produces three plots

## change to your working directory
setwd("/Users/Chuyue/Google Drive/Berkeley/Fall 2016/Academics/PS 239T/PS-239T-Final-Project")

## load libraries 
require(base)
require(dplyr)
require(xlsx)
require(ggplot2)
require(tidyr)
require(stringr)

# function I wrote to capitalize words, "firstup()"
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


# import all the sheets in the WGI dataset, which are stored in the "/Data/WGI" folder 
# they are six separate csv files with different indicators of governance 

# voice of accountability "va"
va<-read.csv("Data/WGI/WGI_raw_VA.csv", stringsAsFactors = F)%>%mutate(varx="va")
# control of corruption "cc"
cc<-read.csv("Data/WGI/WGI_raw_CC.csv", stringsAsFactors = F)%>%mutate(varx="cc")
# regulatory quality "rq"
rq<-read.csv("Data/WGI/WGI_raw_RQ.csv", stringsAsFactors = F)%>%mutate(varx="rq")
# rule of law "rl"
rl<-read.csv("Data/WGI/WGI_raw_RL.csv", stringsAsFactors = F)%>%mutate(varx="rl")
# government effectivness "ge"
ge<-read.csv("Data/WGI/WGI_raw_GE.csv", stringsAsFactors = F)%>%mutate(varx="ge")
# political stability "ps"
ps<-read.csv("Data/WGI/WGI_raw_PS.csv", stringsAsFactors = F)%>%mutate(varx="ps")

# combine all the spreadsheets into one big dataframe
wgi<-rbind(va[2:215,], cc[2:215,], rq[2:215,], rl[2:215,], ge[2:215,], ps[2:215,])

# the original csv files have confidence intervals and SEs of the indicators, 
# but I will only keep the estimate, so I use grepl() here to keep only the columns I want 
var_name<-names(wgi)
keep_var=var_name[grepl("^X$|^X\\.1$|^X.{4}$|var", var_name)] #keep only the estimate columns 

# then I further clean the sheet by gathering the variables, 
# and then spread it by the name of the indicator 
wgi1<-
  wgi[,keep_var]%>% #keep the variables I want 
  rename(ctry=X,
         ccode=X.1)%>% #rename the variables 
  gather(year, score, -ctry, -ccode, -varx)%>%
  mutate(score=as.numeric(gsub("#N/A|#REF", NA, score)),
         ctry=gsub("\xef", "O", ctry), #chane the weird characters in the country names
         ctry=gsub("\x83","e", ctry),
         ctry=gsub("\xcc","a", ctry),
         first=tolower(word(ctry, 1)),
         first=gsub(",$","", first))%>%
  spread(varx, score)%>%
  mutate(year=as.numeric(gsub("X","",year))) #the dataframe is cleaned 

# need to fix the country string
country<-unique(wgi$X) #then I changed the gsub above to fix the random characters 

# import the csv that matches countries with their regions
# it is also in the "Data/WGI" folder 
counreg<-
  read.xlsx2("Data/WGI/counreg.xls",1, stringsAsFactors=F)%>%
  dplyr::select(Country, Region)%>%
  mutate(Country=gsub("&", "and", Country))# modify the country names to match those in the 
                                           # WGI datasets 

# trim leading and trailing whitespace in the character strings 
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
counreg$Country<-trim(counreg$Country)
counreg$Region<-trim(counreg$Region)
counreg$first<-tolower(word(counreg$Country, 1))
counreg$first<-gsub(",$","", counreg$first)

# merge the counreg dataframe with the wgi one 
# so that every country is matched with a region
wgi2<-
  wgi1%>%
  left_join(counreg, by=c("first"="first"))%>%
  rename(rgn=Region)%>% #rename the variables 
  mutate(ctry=sapply(ctry, firstup))

wgi2<-wgi2[,-c(4,11)]


##################
## plots #########

# subset to just Hong Kong 
hk<-
  wgi2%>%
  filter(ccode=="HKG")%>%
  gather(gi, score, -ctry, -ccode, -rgn, -year)

#plot of the six indicators for HK 
hk_plot_all<-
  ggplot(hk)+
  geom_line(aes(x=year, y=score, color=gi))+
  facet_wrap(~gi)+
  labs(x="Year", 
       y="Governance Index", 
       title="Effectiveness of Governnance, Hong Kong (1996-2015)")+
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015))+
  scale_colour_discrete(name  ="Index",
                        breaks=c("cc",
                                 "ge",
                                 "ps",
                                 "rl",
                                 "rq",
                                 "va"),
                        labels=c("Corruption",
                                 "Government",
                                 "Stability",
                                 "Rule of Law",
                                 "Regulatory",
                                 "Accountability"))+
  theme_bw()+
  theme(
    axis.text.x=element_text(size=5)
  )
# save the plot to the results folder 
ggsave("Results/hk_plot.jpeg", hk_plot_all) ##save the plot 


# plot the accountability score for all Asian countries 

## the year of 2015
## subset to va scores for Asian countries in the year of 2015 
ea_va_15<-
  wgi2%>%
  filter(rgn=="ASIA (EX. NEAR EAST)",
         year==2015)%>% #only keep one country variable 
  unique()

## assign ranks based on the va score 
ea_va_15$rank <-rank(-ea_va_15$va, ties.method = "random")

## plot of their scores in order, and highlight HK in red 
ea_va_15_plot <-
  ggplot(ea_va_15, aes(rank, va))+
  geom_bar(stat="identity", #barplot
           position="dodge",
           fill=ifelse(ea_va_15$rank==17, "red", "blue"))+ # HK in red 
  geom_text(aes(label=ctry),
            angle=45, 
            y=ifelse(ea_va_15$rank <= 9, -0.05, 0.05), # add country names as text
            size=2, 
            hjust=ifelse(ea_va_15$rank <= 9, 1, 0)
  )+
  labs(y="Voice of Accounability Score (2015)", #titles 
       x="Rank",
       title="VA Score East Asian (2015)")+
  scale_y_continuous(breaks=c(-2, -1.5, -1, -0.5,0, 0.5, 1))+
  theme_bw()+
  theme(
    axis.text.x=element_text(size=5),
    axis.text.y=element_text(size=5)    
  )
# save the plot in the "Results" folder 
ggsave("Results/ea_va_15.jpeg", ea_va_15_plot)

## Year of 1998
## similar to the procedure above 

## subset 
ea_va_98<-
  wgi2%>%
  filter(rgn=="ASIA (EX. NEAR EAST)", # only Asian countries 
         year==1998)%>%
  unique()

## assign ranks 
ea_va_98$rank <-rank(-ea_va_98$va)


## plot their scores in order, HK in red 
ea_va_98_plot <-
  ggplot(ea_va_98, aes(rank, va))+
  geom_bar(stat="identity",
           position="dodge",
           fill=ifelse(ea_va_98$rank==20, "red", "blue"))+
  geom_text(aes(label=ctry),
            angle=45, 
            y=ifelse(ea_va_98$rank <= 8, -0.05, 0.05), 
            size=2, 
            hjust=ifelse(ea_va_98$rank <= 8, 1, 0)
  )+
  labs(y="Voice of Accounability Score (1998)",
       x="Rank",
       title="VA Score East Asian (1998)")+
  scale_y_continuous(breaks=c(-2, -1.5, -1, -0.5,0, 0.5, 1))+
  theme_bw()+
  theme(
    axis.text.x=element_text(size=5),
    axis.text.y=element_text(size=5)    
  )

## save the plot in the "Results" folder
ggsave("Results/ea_va_98.jpeg", ea_va_98_plot)


## this concludes part I of the project 
