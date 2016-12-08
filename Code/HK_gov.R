## title: Hong Kong Governance
## author: Chuyue Huang
## date: 12-02-2016
## course: PS239T-Final Project


#I load all my packages here 
source("/Users/Chuyue/Google Drive/Berkeley/Rye Pie/R Packages.R") 
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


# find the members, by first finding the title word

#first split the summary
leg3<-leg2%>%mutate(splitsumm=strsplit(summ, " ")) #split the summary string into words 

##filter out cases that don't have a member actor 
pattern.title<-"Hon"
pattern.last<-"[[:upper:]]{2,6}"

actor<-
  leg3%>%
  filter(grepl(pattern.last, summ),
         grepl(pattern.title, summ))

actor$postitle<-rep(NA, nrow(actor))
actor$poslast<-rep(NA, nrow(actor))

for (i in 1:nrow(actor)){
  actor$postitle[i]<-grep(pattern.title, actor$splitsumm[[i]])[1]
  actor$poslast[i]<-grep(pattern.last, actor$splitsumm[[i]])[1]
}

#here I extract the member name from the event summary
#the order in which HK people write their names: title, English first name (if any), last name, 
#Chinese first name 
#since I only got the position for the last name, if the difference between the position of the 
#first name and the last name is one (meaning that the title is immediately followed by a lastname),
#that neans the member does not have an English, so I would add one to the position of the last name
#to get their Chinese name 

actor1<-
  actor%>%
  mutate(dif=poslast-postitle)%>%
  filter(poslast<=6)%>%
  mutate(last=sapply(word(summ, poslast, poslast), firstup), #last name 
         first=ifelse(dif==1,  #first name 
                      word(summ, poslast+1), 
                      word(summ, poslast-1)))%>%
  mutate(first=gsub("\'s$", "", first),
         full=paste(first, last, sep=" "))

### member info ##########

##### the legco webpage, not very efficient--------
#function to obtain current members' (fifth term of the legislature) information 

#get a list of the member bio page by extracting the url suffix from the main bio page 
bio.main <- "http://www.legco.gov.hk/general/english/members/yr16-20/biographies.htm"
main.doc<- htmlParse(bio.main)
name.links <- xpathSApply(main.doc, "//a/@href")
#total number of members
nmember<-length(name.links)
base.bio.url<-"http://www.legco.gov.hk/general/english/members/yr16-20/"

#create a dataframe with the members' info
name<-rep(NA, nmember)
hedu<-rep(NA, nmember)
occu<-rep(NA, nmember)
pty<-rep(NA, nmember)
cons<-rep(NA, nmember)

getleg5<-function(web, suffix){
  for(i in 1:nmember){
    full.web<-paste(web, suffix[i], sep="")
    URL=getURL(full.web)
    doc=htmlParse(URL, asText=TRUE)
    text <- xpathSApply(doc, 
                        "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    pos.name<-26 #position of the name is 26
    pos.cons<-31 #position of the consituency is 31
    pos.hedu<-which(text=="Occupation :")-1 #highest education is the line above occupation
    pos.occu<-which(text=="Occupation :")+2 #occupation is two lines below occupation 
    pos.pty<-which(text=="Political affiliation :")+2 #party 
    
    #function to remove the extra characters 
    rm<-function(x){
      out<-gsub("\r\n$", "", x)
      return(out)
    }
    name[i]<-ifelse(length(pos.name)!=0, rm(text[pos.name]), NA) 
    hedu[i]<-ifelse(length(pos.hedu)!=0, rm(text[pos.hedu]), NA)  
    occu[i]<-ifelse(length(pos.occu)!=0, rm(text[pos.occu]), NA)  
    pty[i]<-ifelse(length(pos.pty)!=0, rm(text[pos.pty]), NA)  
    cons[i]<-ifelse(length(pos.cons)!=0, rm(text[pos.cons]), NA)  
  }
    legco_5<-data.frame(name,
                        hedu,
                        occu,
                        pty,
                        cons)
    return(legco_5)
                    
}
  
legco_5_df<-getleg5(base.bio.url, name.links)

#### using the membership info on wikipedia--------
## scrap the wikipedia membership page
## write a function that scrapes the table from the 

wiki_mem<-function(term, current=T){# 'term' is the session of the council 
                                    # 'current' adjusts for the different XML paths 
  base.url.1<-"https://en.wikipedia.org/wiki/" # based on the pattern in the url
  base.url.2<-"_Legislative_Council_of_Hong_Kong"
  url<-paste(base.url.1, term, base.url.2, sep = "") 
  table<-
    read_html(url)%>%
    html_nodes(xpath=ifelse(current==T,
                            '//*[@id="mw-content-text"]/table[5]',
                            '//*[@id="mw-content-text"]/table[6]'))%>% #the XML node 
    html_table()
  table<-
    table[[1]][,-3]%>% #first element of the list   
    mutate(term=substr(term, 1 , 1)) #
  return(table)
}

lc_3<-wiki_mem("3rd", F) #2004-2008 session 
lc_4<-wiki_mem("4th", F) #2008-2012 session 
lc_5<-wiki_mem("5th", F) #2012-2016 session 
lc_6<-wiki_mem("6th", T) #2016-present session

#combine member table
lc_3_6<-rbind(lc_3,
              lc_4, 
              lc_5,
              lc_6)
names(lc_3_6)<-c("gc",
                 "const",
                 "em",
                 "ep",
                 "fod",
                 'byr',
                 "term")
#clean it, fix the name string
lc<-
  lc_3_6%>%
  mutate(em=substr(em, 1, (nchar(em)+1)/2),
         ep=gsub("a$", "", ep))%>%
  separate(em, into=c("last", "first"), sep=", ")%>%
  separate(ep, into=c("ep", "ep2"), sep="/|\\(")%>%
  mutate(ep2=gsub("\\)", "", ep2), #get the alternative party
         ep=gsub("Frontier","Democratic", ep)) #frontier becomes the democratic party 

#### get the party affiiliation info---------
# obtain a list of parties and their affiliations 
party_aff<-
  read_html("https://en.wikipedia.org/wiki/Legislative_Council_of_Hong_Kong")%>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[5]')%>%
  html_table(fill=T)

party_aff1<-party_aff[[1]]
info_party<-party_aff1[-1,3]

party<-
  data.frame(c(info_party[1:8],
               info_party[10:19],
               info_party[21:25],
               info_party[27:29]),
             c(rep("PB", 8),
               rep("PD", 10),
               rep("LO", 5),
               rep("NO", 3)))%>%
names(party)<-c("party", "aff")
party1<-
  party%>%
  mutate(party=gsub("PP-LSD", "LSD", party))



# merge the aff frame to the member frame 
mem.party<-
  lc%>%
  left_join(party1, by=c("ep"="party"))%>%
  mutate(full=paste(first, last, sep=" "))
  dplyr::select(full, term, ep, aff)

#merge the two dataframes to get the members and their affiliations 
active.member<-
  actor1%>%
  left_join(mem.party, by=c("full"="full"))#merge their parties 

# get their number of mentions 
active3<-
  active.member%>%
  dplyr::select(year, id, full, aff, issu)%>%
  group_by(year, full, issu, aff)%>%
  summarise(times=n())%>%
  ungroup()%>%
  group_by(issu)%>%
  top_n(n=5, wt=times)

top.10<-
  active3%>%
  arrange(desc(times))%>%
  head(20)
top.10.name<-unique(top.10$full)


active.member.plot<-
  ggplot(active3, aes(x=mname, y=times, group=issu))+
  geom_bar(stat="identity")+
  facet_grid(issu~., drop=T)+
  coord_flip()+
  labs(x="Legislator's Name", 
       y="Number of Mentions", 
       title="Top 20 Most Active Members By Year")


