# Define dates of interventions:
interventions<-as.Date(c("2020-03-10","2020-03-20","2020-03-30","2020-04-30")) #need to be arranged by order, update when neccessary

#####url for google sheets, if they change it we are f##$

url="https://docs.google.com/spreadsheets/d/1N1qLMoWyi3WFGhIpPFzKsFmVE0IwNP3elb_c18t2DwY/edit#gid=0"

library(gsheet)

#####import Slo data

flag=FALSE
while(flag==FALSE){
  dd<-try(read.csv(text=gsheet2text(url,format='csv'),stringsAsFactors=FALSE,skip=1),silent=TRUE)
  if (class(dd)!="try-error") flag=TRUE
}

# dd <- dd[as.Date(substr(dd$datum, 1,7), format = "%d. %m.")<=as.Date("2020-04-13"),] # limit data to a given date
dd <- dd[1:(nrow(dd)-which.max((diff(cumsum(is.na(rev(dd$datum)))))==0)),]

url="https://docs.google.com/spreadsheets/d/1N1qLMoWyi3WFGhIpPFzKsFmVE0IwNP3elb_c18t2DwY/edit#gid=918589010"

flag=FALSE
while(flag==FALSE){
  dd2<-try(read.csv(text=gsheet2text(url,format='csv'),stringsAsFactors=FALSE,skip=1),silent=TRUE)
  if (class(dd)!="try-error") flag=TRUE
}

dd <- dplyr::left_join(dd, 
                       dd2[,c('datum', 'I.i', 'I.o', 'D.u', 'H.D')], 
                       by='datum')

# Limit yourself to 3. 6. 2020:
dd <- dd[as.Date(substr(dd$datum, 1,7), format = "%d. %m.")<=as.Date("2020-06-03"),] # limit data to a given date

####
# Add additional data if you want:
if(!exists('additional_data')) additional_data <- 'no'
# additional_data=c('no', 'all_zeros', 'autumn_wave','immediate_wave')

# Check:
# dd %>% select(datum, hospitalizirani..trenutno., novi, iz.bol..oskrbe..vsi., potrjeni.danes, umrli..vsi., intenzivna.enota..trenutno.,
#               I.i, I.o, D.u, H.D) %>% View

####

save(dd,interventions,file="data.Rdata")
# load("data.Rdata")

####
# Hospital data:
library(dplyr)

url="https://docs.google.com/spreadsheets/d/1N1qLMoWyi3WFGhIpPFzKsFmVE0IwNP3elb_c18t2DwY/edit#gid=918589010"


flag=FALSE
while(flag==FALSE){
  dd_hosp<-try(read.csv(text=gsheet2text(url,format='csv'),stringsAsFactors=FALSE, skip = 1),silent=TRUE)
  if (class(dd_hosp)!="try-error") flag=TRUE
}

# dd_hosp <- dd_hosp[as.Date(substr(dd_hosp$datum, 1,7), format = "%d. %m.")<=as.Date("2020-04-13"),] # limit data to a given date
dd_hosp <- dd_hosp[1:(which.max(dd_hosp[,1]=='')-1),]

# Make sure the dates are the same:
dd_hosp <- dd %>% 
  select(datum) %>% 
  left_join(dd_hosp, by='datum')

# Take columns we need:
dd_hosp <- dd_hosp[,c('datum'
                      ,'H.c.2', 'H.i.1', 'H.o.2', 'H.D.t.1', 'I.i.1', 'I.o.1', 'I.c.2' 
                      ,'H.c.3', 'H.i.2', 'H.o.3', 'H.D.t.2', 'I.i.2', 'I.o.2', 'I.c.3' 
                      ,'H.c.4', 'H.i.3', 'H.o.4', 'H.D.t.3', 'I.i.3', 'I.o.3', 'I.c.4'
                      ,'H.c.5', 'H.i.4', 'H.o.5', 'H.D.t.4', 'I.i.4', 'I.o.4', 'I.c.5'
)]
dd_hosp[is.na(dd_hosp)] <- '0'
dd_hosp[dd_hosp==''] <- '0'
dd_hosp <- dd_hosp[-1,]
# dd_hosp_tmp <- dd_hosp[1:12,]
# dd_hosp_tmp[,] <- '0'
# dd_hosp <- rbind(dd_hosp_tmp,dd_hosp)
dd_hosp <- dd_hosp[-1:-4,]

dd_hosp[,2:ncol(dd_hosp)] <- sapply(dd_hosp[,2:ncol(dd_hosp)], as.numeric)

save(dd_hosp, file='hosp_data.RData')

# Notes for data:

# # Ljubljana: 
# dd_hosp$H.c.2 # hospitalizirani
# dd_hosp$H.i.1 # hospitaliziraniin
# dd_hosp$H.o.2 # hospitaliziraniout
# dd_hosp$H.D.t.1 # deaths
# 
# dd_hosp$I.i.1 # icuin
# dd_hosp$I.o.1 # icuout
# dd_hosp$I.c.2 # icu
# 
# # Maribor:
# dd_hosp$H.c.3 # hospitalizirani
# dd_hosp$H.i.2 # hospitaliziraniin
# dd_hosp$H.o.3 # hospitaliziraniout
# dd_hosp$H.D.t.2 # deaths
# 
# dd_hosp$I.i.2 # icuin
# dd_hosp$I.o.2 # icuout
# dd_hosp$I.c.3 # icu
# 
# # Golnik:
# dd_hosp$H.c.4 # hospitalizirani
# dd_hosp$H.i.3 # hospitaliziraniin
# dd_hosp$H.o.4 # hospitaliziraniout
# dd_hosp$H.D.t.3 # deaths
# 
# dd_hosp$I.i.3 # icuin
# dd_hosp$I.o.3 # icuout
# dd_hosp$I.c.4 # icu
# 
# # Celje:
# dd_hosp$H.c.5 # hospitalizirani
# dd_hosp$H.i.4 # hospitaliziraniin
# dd_hosp$H.o.5 # hospitaliziraniout
# dd_hosp$H.D.t.4 # deaths
# 
# dd_hosp$I.i.4 # icuin
# dd_hosp$I.o.4 # icuout
# dd_hosp$I.c.5 # icu

####

rm(list = ls())
