rm(list = ls())


## packages

library(dplyr)
library(pmdplyr)
library(tidyr)
library(sjlabelled)


## attempt 1

setwd("~/Documents/GitHub/Economic_Voting")
dataset2 <- read_sav("ZA3521_v2-0-1.sav")


dataset1 <- read_data(
  "~/Documents/GitHub/Economic_Voting/ZA3648.dta",
  atomic.to.fac = FALSE,
  drop.labels = FALSE,
  enc = NULL,
  verbose = FALSE
)




## keeping variables ZA3521

dataset2$nation2 <- factor(dataset2$nation2, levels = c(1:17), labels = c("France", "Belgium", "Netherlands", "Germany", "Italy", "Luxembourg", "Denmark", "Ireland", "UK", "Greece", "Spain", "Portugal", "Norway", "Finland", "Sweden", "Austria", "Switzerland"))
dataset2$country_year <- paste(dataset2$nation2, dataset2$year, sep="_")

dataset2 <- dataset2 %>% select(country_year, id, split, eb, better, econpast, finapast, satisdmo, closepty, feelclo, voteint, inclvote, lastvote, particip, lrs, party, married, educ, sex, age, sizehh, occup, soclass, income, regionat)
View(dataset2)

## ZA3648 - 

dataset1$country <- factor(dataset1$V3, levels = c(1:22), labels = c("Albania", "Armenia", "Belarus", "Bulgaria", "Croatia", "CzechRepublic", "Slovakia", "Estonia", "Georgia", "Hungary", "Latvia", "Lithuania", "Macedonia", "Moldova", "Poland", "Romania", "Russia", "Slovenia", "Ukraine", "Kazakhstan", "Yugoslavia", "GDR"))
dataset1$country_year <- paste(dataset1$country, dataset1$V4,sep="_")


#start by creating new variable for each V79:V100

dataset1<-dataset1 %>% mutate(vote_Albania=V79)
dataset1<-dataset1 %>% mutate(vote_Armenia=V80)
dataset1<-dataset1 %>% mutate(vote_Belarus=V81)
dataset1<-dataset1 %>% mutate(vote_Bulgaria=V82)
dataset1<-dataset1 %>% mutate(vote_Croatia=V83)
dataset1<-dataset1 %>% mutate(vote_Czech=V84)
dataset1<-dataset1 %>% mutate(vote_Slovakia=V85)
dataset1<-dataset1 %>% mutate(vote_Estonia=V86)
dataset1<-dataset1 %>% mutate(vote_Hungary=V87)
dataset1<-dataset1 %>% mutate(vote_Latvia=V88)
dataset1<-dataset1 %>% mutate(vote_Lithuania=V89)
dataset1<-dataset1 %>% mutate(vote_Macedonia=V90)
dataset1<-dataset1 %>% mutate(vote_Poland=V91)
dataset1<-dataset1 %>% mutate(vote_Romania=V92)
dataset1<-dataset1 %>% mutate(vote_Russia=V93)
dataset1<-dataset1 %>% mutate(vote_Slovenia=V94)
dataset1<-dataset1 %>% mutate(vote_Ukraine=V95)
dataset1<-dataset1 %>% mutate(vote_Georgia=V96)
dataset1<-dataset1 %>% mutate(vote_Kazachstan=V97)
dataset1<-dataset1 %>% mutate(vote_Yugoslavia=V98)
dataset1<-dataset1 %>% mutate(vote_GDR=V99)
dataset1<-dataset1 %>% mutate(vote_Moldova=V100)



## Replace NAs to 0
dataset1$vote_Albania<-replace(dataset1$vote_Albania, is.na(dataset1$vote_Albania), 0)
dataset1$vote_Armenia<-replace(dataset1$vote_Armenia, is.na(dataset1$vote_Armenia), 0)
dataset1$vote_Belarus<-replace(dataset1$vote_Belarus, is.na(dataset1$vote_Belarus), 0)
dataset1$vote_Bulgaria<-replace(dataset1$vote_Bulgaria, is.na(dataset1$vote_Bulgaria), 0)
dataset1$vote_Croatia<-replace(dataset1$vote_Croatia, is.na(dataset1$vote_Croatia), 0)
dataset1$vote_Czech<-replace(dataset1$vote_Czech, is.na(dataset1$vote_Czech), 0)
dataset1$vote_Slovakia<-replace(dataset1$vote_Slovakia, is.na(dataset1$vote_Slovakia), 0)
dataset1$vote_Estonia<-replace(dataset1$vote_Estonia, is.na(dataset1$vote_Estonia), 0)
dataset1$vote_Hungary<-replace(dataset1$vote_Hungary, is.na(dataset1$vote_Hungary), 0)
dataset1$vote_Latvia<-replace(dataset1$vote_Latvia, is.na(dataset1$vote_Latvia), 0)
dataset1$vote_Lithuania<-replace(dataset1$vote_Lithuania, is.na(dataset1$vote_Lithuania), 0)
dataset1$vote_Macedonia<-replace(dataset1$vote_Macedonia, is.na(dataset1$vote_Macedonia), 0)
dataset1$vote_Poland<-replace(dataset1$vote_Poland, is.na(dataset1$vote_Poland), 0)
dataset1$vote_Romania<-replace(dataset1$vote_Romania, is.na(dataset1$vote_Romania), 0)
dataset1$vote_Russia<-replace(dataset1$vote_Russia, is.na(dataset1$vote_Russia), 0)
dataset1$vote_Slovenia<-replace(dataset1$vote_Slovenia, is.na(dataset1$vote_Slovenia), 0)
dataset1$vote_Ukraine<-replace(dataset1$vote_Ukraine, is.na(dataset1$vote_Ukraine), 0)
dataset1$vote_Georgia<-replace(dataset1$vote_Georgia, is.na(dataset1$vote_Georgia), 0)
dataset1$vote_Kazachstan<-replace(dataset1$vote_Kazachstan, is.na(dataset1$vote_Kazachstan), 0)
dataset1$vote_Yugoslavia<-replace(dataset1$vote_Yugoslavia, is.na(dataset1$vote_Yugoslavia), 0)
dataset1$vote_GDR<-replace(dataset1$vote_GDR, is.na(dataset1$vote_GDR), 0)
dataset1$vote_Moldova<-replace(dataset1$vote_Moldova, is.na(dataset1$vote_Moldova), 0)



# Add all relevant columns to get the sum; if NA then you get 0 otherwise the value of the party
#dataset1$vote_int <- (dataset1$vote_Albania+dataset1$vote_Armenia+dataset1$vote_Belarus+dataset1$V82+dataset1$V83+dataset1$V84+dataset1$V85+dataset1$V86+dataset1$V87+dataset1$V88+dataset1$V89+dataset1$V90+dataset1$V91+dataset1$V92+dataset1$V93+dataset1$V94+dataset1$V95+dataset1$V96+dataset1$V97+dataset1$V98+dataset1$V99+dataset1$V100)


dataset1$vote_int = (dataset1$vote_Albania+dataset1$vote_Armenia+dataset1$vote_Belarus+dataset1$vote_Bulgaria+dataset1$vote_Croatia+dataset1$vote_Czech+dataset1$vote_Slovakia+dataset1$vote_Estonia+dataset1$vote_Hungary+dataset1$vote_Latvia+dataset1$vote_Lithuania+dataset1$vote_Macedonia+dataset1$vote_Poland+dataset1$vote_Romania+dataset1$vote_Russia+dataset1$vote_Slovenia+dataset1$vote_Ukraine+dataset1$vote_Georgia+dataset1$vote_Kazachstan+dataset1$vote_Yugoslavia+dataset1$vote_GDR+dataset1$vote_Moldova) 
table(dataset1$vote_int)
# Test by country
table(dataset1$country,dataset1$vote_int)
View(dataset1)


#Replace 0s to NAs and no vote/spoil. no answer

dataset1$vote_int[dataset1$vote_int==0] <- NA
dataset1$vote_int[dataset1$vote_Albania>=20] <- NA
dataset1$vote_int[dataset1$vote_Belarus>=25] <- NA
dataset1$vote_int[dataset1$vote_Bulgaria>=21] <- NA
dataset1$vote_int[dataset1$vote_Croatia >=11] <- NA
dataset1$vote_int[dataset1$vote_Czech>=28] <- NA
dataset1$vote_int[dataset1$vote_Slovakia>=25] <- NA
dataset1$vote_int[dataset1$vote_Estonia>=38] <- NA
dataset1$vote_int[dataset1$vote_Hungary>=17] <- NA
dataset1$vote_int[dataset1$vote_Latvia>=25] <- NA
dataset1$vote_int[dataset1$vote_Lithuania>=25] <- NA
dataset1$vote_int[dataset1$vote_Macedonia>=25] <- NA
dataset1$vote_int[dataset1$vote_Poland>=37] <- NA
dataset1$vote_int[dataset1$vote_Romania>=30] <- NA
dataset1$vote_int[dataset1$vote_Russia>=41] <- NA
dataset1$vote_int[dataset1$vote_Slovenia==19] <- NA
dataset1$vote_int[dataset1$vote_Slovenia>=21] <- NA
dataset1$vote_int[dataset1$vote_Ukraine>=22] <- NA
dataset1$vote_int[dataset1$vote_Georgia>=25] <- NA
dataset1$vote_int[dataset1$vote_Kazachstan>=20] <- NA
dataset1$vote_int[dataset1$vote_Yugoslavia>=17] <- NA
dataset1$vote_int[dataset1$vote_GDR>=17] <- NA
dataset1$vote_int[dataset1$vote_Moldova>=15] <- NA

table(dataset1$vote_int)
table(dataset1$country,dataset1$vote_int)


# Save the dataset
save(dataset1, file="EAEurobarometer.RData")


# mutating vote incl variables

dataset1<-dataset1 %>% mutate(voteinc_Albania=V101)
dataset1<-dataset1 %>% mutate(voteinc_Belarus=V102)
dataset1<-dataset1 %>% mutate(voteinc_Bulgaria=V103)
dataset1<-dataset1 %>% mutate(voteinc_Czech=V104)
dataset1<-dataset1 %>% mutate(voteinc_Slovakia=V105)
dataset1<-dataset1 %>% mutate(voteinc_Estonia=V106)
dataset1<-dataset1 %>% mutate(voteinc_Hungary=V107)
dataset1<-dataset1 %>% mutate(voteinc_Latvia=V108)
dataset1<-dataset1 %>% mutate(voteinc_Lithuania=V109)
dataset1<-dataset1 %>% mutate(voteinc_Macedonia=V110)
dataset1<-dataset1 %>% mutate(voteinc_Poland=V111)
dataset1<-dataset1 %>% mutate(voteinc_Romania=V112)
dataset1<-dataset1 %>% mutate(voteinc_Russia=V113)
dataset1<-dataset1 %>% mutate(voteinc_Slovenia=V114)
dataset1<-dataset1 %>% mutate(voteinc_Ukraine=V115)
dataset1<-dataset1 %>% mutate(voteinc_Georgia=V116)
dataset1<-dataset1 %>% mutate(voteinc_Moldova=V117)


# Replace NAs to 0s

dataset1$voteinc_Albania<-replace(dataset1$voteinc_Albania, is.na(dataset1$voteinc_Albania), 0)
dataset1$voteinc_Belarus<-replace(dataset1$voteinc_Belarus, is.na(dataset1$voteinc_Belarus), 0)
dataset1$voteinc_Bulgaria<-replace(dataset1$voteinc_Bulgaria, is.na(dataset1$voteinc_Bulgaria), 0)
dataset1$voteinc_Czech<-replace(dataset1$voteinc_Czech, is.na(dataset1$voteinc_Czech), 0)
dataset1$voteinc_Slovakia<-replace(dataset1$voteinc_Slovakia, is.na(dataset1$voteinc_Slovakia), 0)
dataset1$voteinc_Estonia<-replace(dataset1$voteinc_Estonia, is.na(dataset1$voteinc_Estonia), 0)
dataset1$voteinc_Hungary<-replace(dataset1$voteinc_Hungary, is.na(dataset1$voteinc_Hungary), 0)
dataset1$voteinc_Latvia<-replace(dataset1$voteinc_Latvia, is.na(dataset1$voteinc_Latvia), 0)
dataset1$voteinc_Lithuania<-replace(dataset1$voteinc_Lithuania, is.na(dataset1$voteinc_Lithuania), 0)
dataset1$voteinc_Macedonia<-replace(dataset1$voteinc_Macedonia, is.na(dataset1$voteinc_Macedonia), 0)
dataset1$voteinc_Poland<-replace(dataset1$voteinc_Poland, is.na(dataset1$voteinc_Poland), 0)
dataset1$voteinc_Romania<-replace(dataset1$voteinc_Romania, is.na(dataset1$voteinc_Romania), 0)
dataset1$voteinc_Russia<-replace(dataset1$voteinc_Russia, is.na(dataset1$voteinc_Russia), 0)
dataset1$voteinc_Slovenia<-replace(dataset1$voteinc_Slovenia, is.na(dataset1$voteinc_Slovenia), 0)
dataset1$voteinc_Ukraine<-replace(dataset1$voteinc_Ukraine, is.na(dataset1$voteinc_Ukraine), 0)
dataset1$voteinc_Georgia<-replace(dataset1$voteinc_Georgia, is.na(dataset1$voteinc_Georgia), 0)
dataset1$voteinc_Moldova<-replace(dataset1$voteinc_Moldova, is.na(dataset1$voteinc_Moldova), 0)

table(dataset1$country,dataset1$voteinc_Albania)
table(dataset1$country,dataset1$voteinc_Belarus)
table(dataset1$country,dataset1$voteinc_Bulgaria)
table(dataset1$country,dataset1$voteinc_Czech)
table(dataset1$country,dataset1$voteinc_Slovakia)
table(dataset1$country,dataset1$voteinc_Estonia)
table(dataset1$country,dataset1$voteinc_Hungary)
table(dataset1$country,dataset1$voteinc_Latvia)
table(dataset1$country,dataset1$voteinc_Lithuania)
table(dataset1$country,dataset1$voteinc_Macedonia)
table(dataset1$country,dataset1$voteinc_Poland)
table(dataset1$country,dataset1$voteinc_Romania)
table(dataset1$country,dataset1$voteinc_Russia)
table(dataset1$country,dataset1$voteinc_Slovenia)
table(dataset1$country,dataset1$voteinc_Ukraine)
table(dataset1$country,dataset1$voteinc_Georgia)
table(dataset1$country,dataset1$voteinc_Moldova)


table(dataset1$V101,dataset1$voteinc_Albania)
table(dataset1$V102,dataset1$voteinc_Belarus)
table(dataset1$V103,dataset1$voteinc_Bulgaria)
table(dataset1$V104,dataset1$voteinc_Czech)
table(dataset1$V105,dataset1$voteinc_Slovakia)
table(dataset1$V106,dataset1$voteinc_Estonia)
table(dataset1$V107,dataset1$voteinc_Hungary)
table(dataset1$V108,dataset1$voteinc_Latvia)
table(dataset1$V109,dataset1$voteinc_Lithuania)
table(dataset1$V110,dataset1$voteinc_Macedonia)
table(dataset1$V111,dataset1$voteinc_Poland)
table(dataset1$V112,dataset1$voteinc_Romania)
table(dataset1$V113,dataset1$voteinc_Russia)
table(dataset1$V114,dataset1$voteinc_Slovenia)
table(dataset1$V115,dataset1$voteinc_Ukraine)
table(dataset1$V116,dataset1$voteinc_Georgia)
table(dataset1$V117,dataset1$voteinc_Moldova)
# thingy

dataset1$voteinc = (dataset1$voteinc_Albania+dataset1$voteinc_Belarus+dataset1$voteinc_Bulgaria+dataset1$voteinc_Czech+dataset1$voteinc_Slovakia+dataset1$voteinc_Hungary+
                      dataset1$voteinc_Latvia+dataset1$voteinc_Lithuania+dataset1$voteinc_Macedonia+
                      dataset1$voteinc_Poland+dataset1$voteinc_Romania+dataset1$voteinc_Russia+
                      dataset1$voteinc_Slovenia+dataset1$voteinc_Ukraine+dataset1$voteinc_Georgia+
                      dataset1$voteinc_Moldova)
table(dataset1$voteinc)

                     
                      



table(dataset1$voteinc)
summary(dataset1$voteinc)
table(dataset1$country,dataset1$voteinc)
#Replace 0s to NAs and no vote/spoil. no answer

dataset1$voteinc[dataset1$voteinc==0] <- NA
dataset1$voteinc[dataset1$voteinc_Albania>=24] <- NA
dataset1$voteinc[dataset1$voteinc_Belarus>=28] <- NA
dataset1$voteinc[dataset1$voteinc_Bulgaria>=21] <- NA
dataset1$voteinc[dataset1$voteinc_Czech>=31] <- NA
dataset1$voteinc[dataset1$voteinc_Slovakia>=28] <- NA
dataset1$voteinc[dataset1$voteinc_Estonia>=38] <- NA
dataset1$voteinc[dataset1$voteinc_Hungary==17] <- NA
dataset1$voteinc[dataset1$voteinc_Hungary>=19] <- NA
dataset1$voteinc[dataset1$voteinc_Latvia==27] <- NA
dataset1$voteinc[dataset1$voteinc_Latvia>=29] <- NA
dataset1$voteinc[dataset1$voteinc_Lithuania==26] <- NA
dataset1$voteinc[dataset1$voteinc_Lithuania>=28] <- NA
dataset1$voteinc[dataset1$voteinc_Macedonia>=27] <- NA
dataset1$voteinc[dataset1$voteinc_Poland>=39] <- NA
dataset1$voteinc[dataset1$voteinc_Romania>=30] <- NA
dataset1$voteinc[dataset1$voteinc_Russia==39] <- NA
dataset1$voteinc[dataset1$voteinc_Russia>=41] <- NA
dataset1$voteinc[dataset1$voteinc_Slovenia==22] <- NA 
dataset1$voteinc[dataset1$voteinc_Slovenia>=24] <- NA 
dataset1$voteinc[dataset1$voteinc_Ukraine>=25] <- NA
dataset1$voteinc[dataset1$voteinc_Georgia>=28] <- NA
dataset1$voteinc[dataset1$voteinc_Moldova>=17] <- NA

table(dataset1$voteinc)
table(dataset1$country,dataset1$voteinc)

