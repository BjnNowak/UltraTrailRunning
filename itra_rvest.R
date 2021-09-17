
library(tidyverse)

# Articles on the topic:
# In a 100-Mile Race That Demands Both Physical and Mental Fortitude, Women Set the Pace
# https://www.nytimes.com/2021/07/11/sports/western-states-women.html
# Women Reduce the Performance Difference to Men with Increasing Age in Ultra-Marathon Running 
# https://www.mdpi.com/1660-4601/16/13/2377/htm

# Race selection: https://itra.run/Races/FindRaceResults

# Only 100 miles
# (select criteria: 155km-175km)

# 2021
RaceYearId_100m_2021 <- c(
  "68140","72496","69855","67856","70469","66887","67851","68241",
  "70241","69945","67218","65713","66425","66003","71873","69219",
  "68455","68575","69019","67244","68776","68778","67515","70168",
  "67118","67402","71066","64191","69065","68416","69919","68874",
  "68354","70181","71777","71778","69315","64849","69395","63925",
  "67033","71209","66105","66840","67410","68605","71293","65183",
  "64771","66949","67422","70708","67736","68042","70440","70255",
  "69334","71184","70681","71704","69319","69523","65403","68615",
  "69758","70118","66689","71992","68098","71140","68348","64671",
  "71220","64661","68501","64219","65413","69001","64103","68010"
)

# 2020
RaceYearId_100m_2020 <- c(
 '67526','68562','64575','64037','68007','58877','67289','67634',
 '56323','64481','68472','68239','59437','62737','69754','67282',
 '59805','68633','67406','62531','62021','67723','62193','67914',
 '56017','51397','56693','58579','49589','52607','54013','66483',
 '62347','60243','66580','61189','59557','66321','65591','59995',
 '50041','53559','67160','67713','62121','49901','53999','59481',
 '58623','59271','57055','54829','61035','57587','54859','65665',
 '55265','62503','51393','53939','66836','51777','50051','56831',
 '50845','66413','60049','50803','62705','55429','50523','48383',
 '49345','53633','61149','52827','63235','57473','62593','51649',
 '55551','59053','49059','51945','57191','61175','49849','53283',
 '57131','60007','50097','60495','60491','44291'
)

# 2019
RaceYearId_100m_2019 <- c(
  '49431','45417','65861','48861','58259','43585','55417','43795',
  '51033','42239','48423','47165','50187','30267','42867','46539',
  '47381','50241','46657','59415','58289','41009','46671','50463',
  '54821','54119','43385','52353','47279','46241','56591','54493',
  '40621','56729','48019','50683','52399','46617','40741','42123',
  '39531','41175','45287','47701','47571','54421','52435','50011',
  '52499','42707','58551','46687','45167','45319','46701','46901',
  '38949','39019','45885','36169','47035','40097','42289','38525',
  '52425','47641','58553','40445','58291','51495','46739','42807',
  '52973','49537','67711','57283','49517','53619','42631','37871',
  '39131','58005','43459','39881','56571','45609','43567','41269',
  '41443','48837','38185','52109','48391','40737','41117','49421',
  '40025','36117','37967','43051','45911','47489','51331','35613',
  '66412','39821','48055','48751','50485','35983','36267','37005',
  '45153','38947','39995','39425','39985','45339','43675','44333',
  '49189','49089','38057','40727','45639','41865','48737','42619',
  '42693','41413','47257','38229','49781','43657','51343','42329',
  '35719','44903','36431','41919','41449','56513','45199','41131',
  '39073','42493','46363','44745','34461','39397','40649','44563',
  '40115','48603','41499','41585','45873','35185','39819','50505',
  '42635','39055','37711','41399','47625','35803','39229','33939',
  '35721','44131','45763','35373','46567','45523','47925','36539',
  '46917','42679','38835','46679','44003','40681','35385','35579',
  '40171','34071','45837','62597','34063','45839','41595','41505',
  '35113','38011','37637','43893','56951','41573','41369','37065',
  '31215','40653'
  
)

# 2012
RaceYearId_100m_2012 <- c(
  "3534","3491","28463","3445","3261","4782","3444","2692","3226",
  "3218","3242","3247","2871","3446","3241","3689","3436","3244",
  "2705","3597","3249","3442","3234","3957","3003","4619","2688",
  "4129","3254","3257","3259","2872","2917","2665","2914","2694",
  "3240","2903","2906","2696","2905","3245","3804","2616","2916",
  "4763","3379","2664","2555",'3243','2915','3255','3260','3492',
  '2854','3013','3423','3047','3256','3720','3431','3488','3258',
  '2562','4509','5456','2910','4164','3433','3305','3453','3601',
  '2320','3439','5640','3202','3253'
)

RaceYearId <- RaceYearId_100m_2020
RaceYearId
# Function to extract race features
Feat <- function(features){
   # Get race location
   loc<-features%>%
     rvest::html_nodes(xpath = "/html/body/div[1]/div[1]/div/div/div/div[2]/div/div[2]/div[1]")%>%
     rvest::html_text()%>%
     gsub(pattern = "20.* ", replacement = "")%>%
     gsub(pattern = "00.* ", replacement = "")%>%
     gsub(pattern = "\r\n", replacement = "")%>%
     gsub(pattern = " ", replacement = "")%>%
     str_split_fixed(",",2)%>%
     as_tibble()%>%
     rename(City=1)%>%
     rename(Country=2)
     
   # Create tibble
   tib <- tibble(
    Race = features%>%rvest::html_nodes(xpath = "/html/body/div[1]/div[2]/div[2]/div[1]/div[2]/div/h3")%>%rvest::html_text(),
    Date = features%>%rvest::html_nodes(xpath = "/html/body/div[1]/div[2]/div[3]/div/div[2]/div[1]/span")%>%rvest::html_text(),
    StartTime = features%>%rvest::html_nodes(xpath = "/html/body/div[1]/div[2]/div[3]/div/div[2]/div[2]/span")%>%rvest::html_text(),
    Participation = features%>%rvest::html_nodes(xpath = "/html/body/div[1]/div[2]/div[3]/div/div[2]/div[3]/span")%>%rvest::html_text(),
    Distance = features%>%rvest::html_nodes(xpath = "/html/body/div[1]/div[2]/div[3]/div/div[3]/div[1]/span")%>%rvest::html_text(),
    ElevationGain = features%>%rvest::html_nodes(xpath = "/html/body/div[1]/div[2]/div[3]/div/div[3]/div[2]/span")%>%rvest::html_text(),
    ElevationLoss = features%>%rvest::html_nodes(xpath = "/html/body/div[1]/div[2]/div[3]/div/div[3]/div[3]/span")%>%rvest::html_text(),
    # Wrong time limit: removed
    #TimeLimit = features%>%rvest::html_nodes(xpath = "/html/body/div[1]/div[2]/div[3]/div/div[4]/div[1]/span")%>%rvest::html_text(),
    AidStations = features%>%rvest::html_nodes(xpath = "/html/body/div[1]/div[2]/div[3]/div/div[4]/div[2]/span")%>%rvest::html_text(),
    Participants = features%>%rvest::html_nodes(xpath = "/html/body/div[1]/div[2]/div[3]/div/div[4]/div[3]/span")%>%rvest::html_text()
  )%>%
  add_column(loc,.before = 1)
  # Return tibble
  return(tib)
}

# Loop over all races
race_features <- tibble()
for (i in 1:length(RaceYearId)){
  features<-rvest::read_html(
    paste0("https://itra.run/Races/RaceDetails?raceYearId=",RaceYearId[i])
  )
  race_features <- bind_rows(race_features,Feat(features))
}

race_features

# Further cleaning country names
race_features$Country<-noquote(race_features$Country)
race_features$Country<-gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", race_features$Country)  
race_features$Country<-gsub(",", ", ", race_features$Country)  

# Adding France as Country for UTMB (only for year 2021)
# Also set country names as character
race_features<-race_features%>%
  mutate(Country=case_when(
    City=="Chamonix"~"France",
    TRUE~as.character(Country)
  ))

race_features

#library(knitr)
#tibble(variable = names(race_features)) %>% 
#  mutate(class = map(race_features, typeof)) %>% 
#  kable()

# Race results

Rank <- function(features,ranking){

  feat<-tibble(
    Race = features%>%rvest::html_nodes(xpath = "/html/body/div[1]/div[2]/div[2]/div[1]/div[2]/div/h3")%>%rvest::html_text(),
    Date = features%>%rvest::html_nodes(xpath = "/html/body/div[1]/div[2]/div[3]/div/div[2]/div[1]/span")%>%rvest::html_text()
  )

  rk <- ranking %>%
    rvest::html_element(".table") %>% 
    rvest::html_table() %>% 
    rename(Rank=1) %>%
    mutate(Rank=as.numeric(Rank)) %>%
    add_column(feat,.before = 1) %>%
    select(-Score)
  return(rk)
}



# Loop over all races
race_ranking <- tibble()
for (i in 1:length(RaceYearId)){
  features<-rvest::read_html(
    paste0("https://itra.run/Races/RaceDetails?raceYearId=",RaceYearId[i])
  )
  ranking<-rvest::read_html(
    paste0("https://itra.run/Races/RaceResults?raceYearId=",RaceYearId[i])
  )
  race_ranking <- bind_rows(race_ranking,Rank(features,ranking))
}
race_ranking
# Change initial for gender
race_ranking<-race_ranking%>%
  mutate(Gender=case_when(
    Gender=="F"~"W",
    Gender=="H"~"M"
  ))


readr::write_csv(x=race_features,"race_features_2019.csv")
readr::write_csv(x=race_ranking,"race_ranking_2019.csv")


feat_2021<-readr::read_csv("race_features_2021.csv")
feat_2020<-readr::read_csv("race_features_2020.csv")
feat_2019<-readr::read_csv("race_features_2019.csv")
feat_2012<-readr::read_csv("race_features_2012.csv")

race_features<-bind_rows(feat_2021,feat_2012)%>%
  bind_rows(feat_2020)%>%
  bind_rows(feat_2019)
readr::write_csv(x=race_features,"features.csv")


rank_2021<-readr::read_csv("race_ranking_2021.csv")
rank_2020<-readr::read_csv("race_ranking_2020.csv")
rank_2019<-readr::read_csv("race_ranking_2019.csv")
rank_2012<-readr::read_csv("race_ranking_2012.csv")

race_ranking<-bind_rows(rank_2021,rank_2012)%>%
  bind_rows(rank_2020)%>%
  bind_rows(rank_2019)

readr::write_csv(x=race_ranking,"ranking.csv")
