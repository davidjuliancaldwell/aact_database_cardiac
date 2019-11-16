#########################################
# set working directory for your machine
setwd("C:/Users/david/SharedCode/aact")

#########################################
# load libraries
library(tidyr)
library(RPostgreSQL)
library(plyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggsci)
library(gridExtra)

#########################################
# create search parameters
#strings = c('black','asian','african american','hispanic')
stringBlack = c('black','african american')
stringHisp = c('hispanic','latino','latina')
stringAsian = c('non-hispanic asian','asian american','asian-american','asian')
startDate = as.Date("2009-01-01")
termsSearchMesh = c('hypertension','blood pressure','prehypertension')
termsSearchCondTitle = c('blood pressure','diastolic','systolic','hypertension')
countriesList = c("United States")
`%nin%` = Negate(`%in%`)

#########################################
# boolean options for saving
saveData = TRUE
savePlot = TRUE

#########################################
# connect to database
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user="djcald", password="DD968radford")

# begin loading, filtering, selecting tables
study_tbl = tbl(src=con,'studies')
filter_dates <- study_tbl %>% select(official_title,start_date,nct_id,phase,last_known_status,study_type,enrollment,overall_status) %>% filter(start_date >= startDate)  %>% collect()

location_tbl = tbl(src=con,'countries')
locations <- location_tbl %>% select(nct_id,name) %>% filter(name %in% countriesList) %>% collect()

sponsor_tbl = tbl(src=con,'sponsors')
#sponsor <- sponsor_tbl %>% select(nct_id,agency_class) %>% collect()
#sponsor <- sponsor_tbl %>% select(nct_id,agency_class,lead_or_collaborator) %>% filter(lead_or_collaborator == 'lead')%>% collect()
sponsor <- sponsor_tbl %>% select(nct_id,agency_class,lead_or_collaborator)%>% collect()


sponsor = sponsor %>% group_by(nct_id) %>% mutate(funding = case_when(any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'NIH',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'U.S. Fed',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'NIH',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'U.S. Fed',
                                                                      TRUE ~ 'other'))

sponsor = distinct(sponsor,nct_id,.keep_all=TRUE) %>% select(nct_id,funding)


calculatedValues_tbl = tbl(src=con,'calculated_values')
calculatedValues <- calculatedValues_tbl %>% select(nct_id,were_results_reported) %>% collect()

#baselineMeasurements_tbl = tbl(src=con,'baseline_measurements')
#baselineMeasurements <- baselineMeasurements_tbl %>% select(nct_id,category) %>% collect()

interventions_tbl = tbl(src=con,'interventions')
interventions = interventions_tbl %>% select(nct_id,intervention_type) %>% collect()
interventions <- interventions %>% group_by(nct_id) %>% summarize(intervention_comb = paste(intervention_type,collapse=", "))
interventionTrial = interventions %>% mutate(interventionType = case_when(str_detect(tolower(intervention_comb), pattern = paste('procedure')) ~ 'procedure',
                                                                            str_detect(tolower(intervention_comb), pattern = paste('behavioral')) ~ 'behavioral',
                                                                            str_detect(tolower(intervention_comb), pattern = paste('device')) ~ 'device',
                                                                            str_detect(tolower(intervention_comb), pattern = paste('biological')) ~ 'biological',
                                                                            str_detect(tolower(intervention_comb), pattern = paste('drug')) ~ 'drug',
                                                                            TRUE ~ 'other'))

#interventions_nest <- interventions %>% group_by(nct_id) %>% nest() 
#inverventions_nest <- interventions_nest %>% mutate(Intervention = case_when(str_detect(tolower(data), pattern = paste('procedure')) ~ 'interrv',
#                                                               TRUE ~ 'drug'))

facilities_tbl = tbl(src=con,'facilities')
facilities <- facilities_tbl %>% select(nct_id,status,name) %>%collect()
facilities_tabulated <- facilities %>% group_by(nct_id) %>% tally()
facilities_tabulated <- rename(facilities_tabulated,facilitiesCount = n)
facilities_tabulated <- facilities_tabulated %>% mutate(multisite = ifelse(facilitiesCount>1,TRUE,FALSE))

study_ref_tbl = tbl(src=con,'study_references')
# study_ref <- study_ref_tbl %>% select(nct_id,pmid,reference_type,citation)%>% filter(reference_type=="results_reference") %>% collect()
study_ref <- study_ref_tbl %>% select(nct_id,pmid,reference_type,citation) %>% collect()
study_ref_tabulated <- study_ref %>% group_by(nct_id) %>% tally()
study_ref_tabulated <- rename(study_ref_tabulated,pubCount = n)

study_tbl_browse_conditions = tbl(src=con, 'browse_conditions')

# search within mesh terms
condsMesh <- study_tbl_browse_conditions %>% select(nct_id,mesh_term) %>% filter(tolower(mesh_term) %in% termsSearchMesh) %>% collect()

study_tbl_conditions = tbl(src=con, 'conditions')
condsCond <- study_tbl_conditions %>% select(nct_id,downcase_name) %>%filter(downcase_name %in% termsSearchCondTitle) %>% collect()

# search within title
condsTitle <- filter_dates %>% filter(str_detect(tolower(official_title),pattern = paste(termsSearchCondTitle,collapse="|"))) 

study_tbl_description = tbl(src=con, 'detailed_descriptions')

# select which trials match all of our conditions
selectMesh = condsMesh$nct_id 
selectCond= condsCond$nct_id
selectTitle = condsCond$nct_id

filtered_table = study_tbl_description %>% select(nct_id,description) %>% filter((nct_id %in% selectMesh) | (nct_id %in% selectCond) | (nct_id %in% selectTitle)) %>% collect()
#filtered_table = study_tbl_description %>% filter((nct_id %in% condsMesh$nct_id) | (nct_id %in% condsCond$nct_id)) 
#filtered_table = study_tbl_description %>% filter(nct_id %in% condsMesh$nct_id) %>% collect()

#########################################
# joined Tables

#joinedTable %>% filtered_table %>% inner_join(facilities_tabulated,by="nct_id",copy=TRUE)
joinedTable <- join_all(list(filtered_table,facilities_tabulated,sponsor,filter_dates,locations,interventionTrial,calculatedValues),by='nct_id',type="inner")
joinedTable <- left_join(joinedTable,study_ref_tabulated,by='nct_id')

#joinedTable_tabulated <- joinedTable %>% group_by(nct_id) %>% tally()
#joinedTable <- joinedTable %>% mutate(diverseGroup = as.numeric(str_detect(tolower(description), pattern = paste(stringBlack, collapse = "|"))))
joinedTable <- joinedTable %>% mutate(pubCountBool = case_when(!is.na(pubCount) ~ 'TRUE',
                                                               TRUE ~ 'FALSE'))
joinedTable <- joinedTable %>% mutate(diverseGroup = case_when(str_detect(tolower(description), pattern = paste(c(stringBlack,stringHisp,stringAsian), collapse = "|")) ~ 'diverse',
                                                               TRUE ~ 'not diverse'))

joinedTable <- joinedTable %>% mutate(diverse = case_when(str_detect(tolower(description), pattern = paste(stringBlack, collapse = "|")) ~ 'black',
                                 str_detect(tolower(description), pattern = paste(stringHisp, collapse = "|"))~  'hispanic',
                                 str_detect(tolower(description), pattern = paste(stringAsian, collapse = "|"))~  'asian',
                                 TRUE ~ 'not diverse'))
joinedTable <- joinedTable %>% mutate(yearStart=year(joinedTable$start_date))
joinedTableCount <- joinedTable %>% group_by(yearStart,diverse) %>% tally()
joinedTableCount <- rename(joinedTableCount,yearlyCount = n)

joinedTableCountGroup <- joinedTable %>% group_by(yearStart,diverseGroup) %>% count()
joinedTableCountGroup <- rename(joinedTableCountGroup,yearlyCount = n)

endedTrials = c("terminated","withdrawn")

joinedTableDiverseDiscontinued <- joinedTable %>% filter((diverseGroup == "diverse") & str_detect(tolower(overall_status),pattern = paste(endedTrials,collapse="|"))) %>% collect()

# calculate statistics
joinedTableTotals <- joinedTable %>% group_by(diverse) %>% tally()

joinedTableSummarizeInterv <- joinedTable %>% group_by(diverse,interventionType) %>% tally()
joinedTableSummarizeType <- joinedTable %>% group_by(diverse,study_type) %>% tally()
joinedTableSummarizePhase <- joinedTable %>% group_by(diverse,phase) %>% tally()
joinedTableSummarizeAgency <- joinedTable %>% group_by(diverse,funding) %>% tally()
joinedTableSummarizeReported <- joinedTable %>% group_by(diverse,were_results_reported) %>% tally()
joinedTableSummarizeSite<- joinedTable %>% group_by(diverse,multisite) %>% tally()
joinedTableSummarizeStatus<- joinedTable %>% group_by(diverse,last_known_status) %>% tally()
joinedTableSummarizeOverallStatus <- joinedTable %>% group_by(diverse,overall_status) %>% tally()
joinedTableSummarizePubCount <- joinedTable %>% group_by(diverse,pubCountBool) %>% tally()

#########################################
# save data
if (saveData){
write.csv(joinedTable,'htnTableTotal_11_16_2019.csv')
write.csv(joinedTableDiverseDiscontinued,'htnTableDiscDiverse_11_16_2019.csv')
write.csv(joinedTableSummarizeInterv,'htnTableInterv_11_16_2019.csv')
write.csv(joinedTableSummarizeType,'htnTableType_11_16_2019.csv')
write.csv(joinedTableSummarizePhase,'htnTablePhase_11_16_2019.csv')
write.csv(joinedTableSummarizeAgency,'htnTableAgency_11_16_2019.csv')
write.csv(joinedTableSummarizeReported,'htnTableReported_11_16_2019.csv')
write.csv(joinedTableSummarizeSite,'htnTableSite_11_16_2019.csv')
write.csv(joinedTableSummarizeStatus,'htnTableStatus_11_16_2019.csv')
write.csv(joinedTableSummarizeOverallStatus,'htnTableOverallStatus_11_16_2019.csv')
write.csv(joinedTableSummarizePubCount,'htnTablePubCount_11_16_2019.csv')


}

# make plots
pInd<-ggplot(joinedTableCount, aes(x=yearStart,y=yearlyCount, group=diverse, color=diverse)) +
  geom_line()+
  geom_point() +
  labs(x = "Year Started",y="Number of Trials",title = "Number of Blood Pressure Trials started per Year") +
  scale_y_continuous(breaks=seq(0,250,10)) +
  scale_x_continuous(breaks=seq(2009,2019,1),limits=c(2009,2019)) + 
  scale_color_jama() +
  labs(color = 'Type of Trial')
  
print(pInd)
if (savePlot){
ggsave("trialsByYearConditions_11_16_2019.png", units="in", width=5, height=4, dpi=600)
}

pComb<-ggplot(joinedTableCountGroup, aes(x=yearStart,y=yearlyCount, group=diverseGroup, color=diverseGroup)) +
  geom_line()+
  geom_point() +
  labs(x = "year",y="count",title = "Number of Blood Pressure Trials Started Per Year") +
  scale_y_continuous(breaks=seq(0,250,10)) +
  scale_x_continuous(breaks=seq(2009,2019,1),limits=c(2009,2019)) +
  scale_color_jama()
print(pComb)
if (savePlot){
ggsave("trialsByYearConditionsComb_11_16_2019.png", units="in", width=5, height=4, dpi=600)
}

# calculate ratio of diverse to non diverse 
joinedTableRatio <- data.frame(year = unique(joinedTableCountGroup$yearStart))
joinedTableRatio$ratio = joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'diverse',]$yearlyCount/joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'not diverse',]$yearlyCount
joinedTableRatio$groupRatio = 'Ratio'

pRatio<-ggplot(joinedTableRatio, aes(x=year,y=ratio)) +
  geom_line()+
  geom_point() +
  labs(x = "Year Started",y="Ratio of Diverse to Non-Diverse Trials",title = "Ratio of Diverse to Non-Diverse Trials") +
  scale_x_continuous(breaks=seq(2009,2019,1),limits=c(2009,2019)) +
  scale_color_jama()
print(pRatio)
if (savePlot){
  ggsave("trialsByYearRatio_11_16_2019.png", units="in", width=5, height=4, dpi=600)
}

grid.arrange(pInd,pRatio,ncol=2)
pComb <- arrangeGrob(pInd,pRatio,ncol=2)
pComb <- plot_grid(pInd,pRatio,ncol=2,rel_widths = c(5/9,4/9))

#print(pComb)
if (savePlot){
  ggsave(file="trialsByYearConditionsGrid_11_16_2019.png",pComb, units="in", width=10, height=4, dpi=600)
}

# ### scratch below
# 
# 
# filteredNum = filtered_table %>% filter(str_detect(tolower(description), pattern = paste(strings, collapse = "|")))
# filteredNumNotDiverse = filtered_table %>% filter(nct_id %nin% filteredNum$nct_id)
# 
# joinedTableFirst <- filter_dates %>% inner_join(filteredNum, by = "nct_id",copy = TRUE)
# joinedTable <- joinedTableFirst %>% inner_join(locations,by="nct_id",copy=TRUE) %>% collect()
# yearly_counts <- year(pull(joinedTable,start_date)) 
# yearly_counts = as.data.frame(yearly_counts)
# colnames(yearly_counts) <- c("year")
# yearly_counts_summed <- yearly_counts %>% count(year)
# yearly_counts_summed$diverse = 'true'
# 
# 
# joinedTableNotDiverseFirst <- filter_dates %>% inner_join(filteredNumNotDiverse, by = "nct_id",copy = TRUE)
# joinedTableNotDiverse <- joinedTableNotDiverseFirst %>% inner_join(locations,by="nct_id",copy=TRUE) %>% collect()
# yearly_counts_NotDiverse <- year(pull(joinedTableNotDiverse,start_date))
# yearly_counts_NotDiverse = as.data.frame(yearly_counts_NotDiverse)
# colnames(yearly_counts_NotDiverse) <- c("year")
# yearly_counts_summed_NotDiverse <- yearly_counts_NotDiverse %>% count(year)
# yearly_counts_summed_NotDiverse$diverse = 'false'
# 
# yearly_counts_total <- rbind(yearly_counts_summed,yearly_counts_summed_NotDiverse)
# 
# 
# p<-ggplot(yearly_counts_total, aes(x=year,y=n, group=diverse, color=diverse)) +
#   geom_line()+
#   geom_point() +
#   labs(x = "year",y="count",title = "Number of Blood Pressure Trials Started Per Year") +
#   scale_y_continuous(breaks=seq(0,250,10)) +
#   scale_x_continuous(breaks=seq(2009,2019,1),limits=c(2009,2019)) 
# print(p)
# #ggsave("trialsByYearConditionsTitle.png", units="in", width=5, height=4, dpi=600)
