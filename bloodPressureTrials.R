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
library(cowplot)


#########################################
# create search parameters
#strings = c('black','asian','african american','hispanic')
stringBlack = c('black','african american')
stringHisp = c('hispanic','latino','latina')
stringAsian = c('non-hispanic asian','asian american','asian-american','asian')
startDate = as.Date("2009-01-01")
startDateEnd = as.Date("2018-12-31")
termsSearchMesh = c('hypertension','blood pressure','prehypertension')
termsSearchCondTitle = c('blood pressure','diastolic','systolic','hypertension')
countriesList = c("United States")
`%nin%` = Negate(`%in%`)

#########################################
# boolean options for saving
saveData = FALSE
savePlot = FALSE

#########################################
# connect to database
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user="djcald", password="DD968radford")

# begin loading, filtering, selecting tables
study_tbl = tbl(src=con,'studies')
filter_dates <- study_tbl %>% select(official_title,start_date,nct_id,phase,last_known_status,study_type,enrollment,overall_status) %>% filter(start_date >= startDate && start_date <= startDateEnd && study_type == 'Interventional')  %>% collect()
filter_dates <- filter_dates%>% mutate(phase = replace(phase, phase == "N/A", "Not Applicable"))


location_tbl = tbl(src=con,'countries')

# check if country is the only one in a list 
locations = location_tbl %>% select(nct_id,name) %>% collect()
locations <- locations %>% group_by(nct_id) %>% summarize(countriesPaste = paste(name,collapse=", ")) %>% filter (countriesPaste == countriesList) %>% collect()

locationsTotal = location_tbl %>% select(nct_id,name) %>% collect()
locationsTotal = locationsTotal %>% group_by(nct_id) %>% summarize(countriesPaste = paste(name,collapse=", ")) %>% collect()

sponsor_tbl = tbl(src=con,'sponsors')
#sponsor <- sponsor_tbl %>% select(nct_id,agency_class) %>% collect()
#sponsor <- sponsor_tbl %>% select(nct_id,agency_class,lead_or_collaborator) %>% filter(lead_or_collaborator == 'lead')%>% collect()
sponsor <- sponsor_tbl %>% select(nct_id,agency_class,lead_or_collaborator)%>% collect()


sponsor = sponsor %>% group_by(nct_id) %>% mutate(funding = case_when(any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'NIH',
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


# baseline counts
baseline_counts = tbl(src=con,'baseline_counts')
baselineCounts <- baseline_counts %>% select(nct_id,ctgov_group_code,count) %>% collect()

# baseline measurements
baseline_measurements = tbl(src=con,'baseline_measurements')
baselineMeasurements <- baseline_measurements %>% select(nct_id,classification,category,title,ctgov_group_code) %>% collect()

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

# this one was the original inner join
#joinedTable <- join_all(list(filtered_table,facilities_tabulated,sponsor,filter_dates,locations,interventionTrial,calculatedValues),by='nct_id',type="full")

# this is a join that includes all categories, but only ones that match the description 
joinedTable <- join_all(list(facilities_tabulated,sponsor,locations,interventionTrial,calculatedValues),by='nct_id',type="full")
joinedTable <- left_join(filter_dates,joinedTable,by='nct_id')
joinedTable <- inner_join(filtered_table,joinedTable,by='nct_id')


# this adds pub counts, and NAs for those that dont have pubs
joinedTable <- left_join(joinedTable,study_ref_tabulated,by='nct_id')

#joinedTable_tabulated <- joinedTable %>% group_by(nct_id) %>% tally()
#joinedTable <- joinedTable %>% mutate(diverseGroup = as.numeric(str_detect(tolower(description), pattern = paste(stringBlack, collapse = "|"))))
joinedTable <- joinedTable %>% mutate(pubCountBool = case_when(!is.na(pubCount) ~ 'TRUE',
                                                               TRUE ~ 'FALSE'))
joinedTable <- joinedTable %>% mutate(diverseGroup = case_when(str_detect(tolower(description), pattern = paste(c(stringBlack,stringHisp,stringAsian), collapse = "|")) ~ 'Diverse',
                                                               TRUE ~ 'General'))

joinedTable <- joinedTable %>% mutate(diverse = case_when(str_detect(tolower(description), pattern = paste(stringBlack, collapse = "|")) ~ 'Black',
                                 str_detect(tolower(description), pattern = paste(stringHisp, collapse = "|"))~  'Hispanic',
                                 str_detect(tolower(description), pattern = paste(stringAsian, collapse = "|"))~  'Asian',
                                 TRUE ~ 'General'))
joinedTable <- joinedTable %>% mutate(yearStart=year(joinedTable$start_date))

# count number of missing columns
joinedTable<- joinedTable %>% mutate(numMissing = rowSums(is.na(.)))

# double check that no trials are double counted
doubleCounts <- joinedTable %>% group_by(nct_id) %>% summarise(count=n())
unique(doubleCounts$count)


# add in drug vs. non-drug 

joinedTable <- joinedTable %>% mutate(interventionDrugNonDrug = case_when(str_detect(tolower(intervention_comb), pattern = paste('drug')) ~ 'Drug Intervention',
                                                                          TRUE ~ 'Non-Drug Intervention'))

# add in industry vs. non industry

joinedTable <- joinedTable %>% mutate(industryNonIndustry = case_when(str_detect(tolower(funding), pattern = paste('industry')) ~ 'Industry Sponsor',
                                                                      TRUE ~ 'Non-Industry Sponsor'))

# rename race-specific 
joinedTable <- joinedTable %>% mutate(raceSpecific = case_when(str_detect(diverseGroup, pattern = paste('Diverse')) ~ 'Race-Specific',
                                                               TRUE ~ 'Non-Race Specific'))


joinedTableCount <- joinedTable %>% group_by(yearStart,diverse) %>% tally()
joinedTableCount <- rename(joinedTableCount,yearlyCount = n)

joinedTableCountGroup <- joinedTable %>% group_by(yearStart,diverseGroup) %>% count()
joinedTableCountGroup <- rename(joinedTableCountGroup,yearlyCount = n)

endedTrials = c("terminated","withdrawn")

joinedTableDiverseDiscontinued <- joinedTable %>% filter((diverseGroup == "Diverse") & str_detect(tolower(overall_status),pattern = paste(endedTrials,collapse="|"))) %>% collect()

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

# to avoid excel field size bug, exclude 
joinedTableNoDescrip <- joinedTable
joinedTableNoDescrip$description = NULL

# calculate ratio of diverse to non diverse 
joinedTableRatio <- data.frame(year = unique(joinedTableCountGroup$yearStart))
joinedTableRatio$ratio = joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'Diverse',]$yearlyCount/joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'General',]$yearlyCount
joinedTableRatio$ratioTotal = joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'Diverse',]$yearlyCount/(joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'General',]$yearlyCount + joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'Diverse',]$yearlyCount)
joinedTableRatio$groupRatio = 'Ratio'

#########################################
# save data
if (saveData){
saveRDS(joinedTable, file = "htnRdata_12_9_2019.rds")
write.csv(joinedTable,'htnTableTotal_12_9_2019.csv')
write.csv(joinedTableNoDescrip,'htnTableTotalNoDescrip_12_9_2019.csv')
write.csv(joinedTableDiverseDiscontinued,'htnTableDiscDiverse_12_9_2019.csv')
write.csv(joinedTableSummarizeInterv,'htnTableInterv_12_9_2019.csv')
write.csv(joinedTableSummarizeType,'htnTableType_12_9_2019.csv')
write.csv(joinedTableSummarizePhase,'htnTablePhase_12_9_2019.csv')
write.csv(joinedTableSummarizeAgency,'htnTableAgency_12_9_2019.csv')
write.csv(joinedTableSummarizeReported,'htnTableReported_12_9_2019.csv')
write.csv(joinedTableSummarizeSite,'htnTableSite_12_9_2019.csv')
write.csv(joinedTableSummarizeStatus,'htnTableStatus_12_9_2019.csv')
write.csv(joinedTableSummarizeOverallStatus,'htnTableOverallStatus_12_9_2019.csv')
write.csv(joinedTableSummarizePubCount,'htnTablePubCount_12_9_2019.csv')

}

# make plots
pInd<-ggplot(joinedTableCount, aes(x=yearStart,y=yearlyCount, group=diverse, color=diverse)) +
  geom_line()+
  geom_point() +
  labs(title='Number of Trials by Race Group Enrolled',x = "Year Registered",y="Number of Trials") +
  # scale_y_continuous(breaks=seq(0,250,10)) +
  ylim(0,max(joinedTableCount$yearlyCount)+10) +
  scale_x_continuous(breaks=seq(2009,2018,1),limits=c(2009,2018)) + 
  scale_color_jama() +
  labs(color = 'Race-Specific Enrollment')
  
print(pInd)
if (savePlot){
ggsave("trialsByYearConditions_12_9_2019.png", units="in", width=5, height=4, dpi=600)
}

pComb<-ggplot(joinedTableCountGroup, aes(x=yearStart,y=yearlyCount, group=diverseGroup, color=diverseGroup)) +
  geom_line()+
  geom_point() +
  labs(x = "year",y="count",color = 'Race-Specific Enrollment') +
  #scale_y_continuous(breaks=seq(0,250,10)) +
  ylim(0,max(joinedTableCount$yearlyCount)+10) +
  scale_x_continuous(breaks=seq(2009,2018,1),limits=c(2009,2018)) +
  scale_color_jama()
print(pComb)
if (savePlot){
ggsave("trialsByYearConditionsComb_12_9_2019.png", units="in", width=5, height=4, dpi=600)
}

pRatio<-ggplot(joinedTableRatio, aes(x=year,y=ratio)) +
  geom_line(color='steelblue')+
  geom_point(color='steelblue') +
  labs(title='Ratio of Diverse Trials over Time',x = "Year Registered",y="Ratio of Diverse to General Trials") +
  scale_x_continuous(breaks=seq(2009,2018,1),limits=c(2009,2018)) +
  ylim(0,max(joinedTableRatio$ratio)+0.015) +
  scale_color_jama()
print(pRatio)
if (savePlot){
  ggsave("trialsByYearRatio_12_9_2019.png", units="in", width=5, height=4, dpi=600)
}

pRatioTotal<-ggplot(joinedTableRatio, aes(x=year,y=ratioTotal)) +
  geom_line(color='steelblue')+
  geom_point(color='steelblue') +
  labs(x = "Year Registered",y="Ratio of Diverse to All Trials") +
  scale_x_continuous(breaks=seq(2009,2018,1),limits=c(2009,2018)) +
  ylim(0,max(joinedTableRatio$ratio)+0.015) +
  scale_color_jama()
print(pRatioTotal)
if (savePlot){
  ggsave("trialsByYearRatioTotal_12_9_2019.png", units="in", width=5, height=4, dpi=600)
}

grid.arrange(pInd,pRatio,ncol=2)
pComb <- arrangeGrob(pInd,pRatio,ncol=2)
#pComb <- plot_grid(pInd,pRatio,ncol=2,rel_widths = c(5/9,4/9))

#print(pComb)
if (savePlot){
  ggsave(file="trialsByYearConditionsGrid_12_9_2019.png",pComb, units="in", width=10, height=4, dpi=600)
}

grid.arrange(pInd,pRatioTotal,ncol=2)
pCombTotal <- arrangeGrob(pInd,pRatioTotal,ncol=2)
#pComb <- plot_grid(pInd,pRatio,ncol=2,rel_widths = c(5/9,4/9))

#print(pComb)
if (savePlot){
  ggsave(file="trialsByYearConditionsGridTotal_12_9_2019.png",pCombTotal, units="in", width=10, height=4, dpi=600)
}


pHist<-ggplot(joinedTable, aes(x=numMissing)) +
  geom_histogram(binwidth=1,color="black", fill="white") +
  labs(x = "Number of Missing Data Columns",y="Count") +
  xlim(0,8)
print(pHist)
if (savePlot){
  ggsave("trialsByYearNumMissing_12_9_2019.png", units="in", width=5, height=4, dpi=600)
}

# find max y count values of drug and industry categories 
joinedTableDrug <- joinedTable %>% group_by(interventionDrugNonDrug,raceSpecific) %>% tally()
joinedTableIndustry <- joinedTable %>% group_by(industryNonIndustry,raceSpecific) %>% tally()
maxDrugIndustry = max(c(max(joinedTableDrug$n),max(joinedTableIndustry$n)))

# facet wrap drug 
pFacetDrug<-ggplot(joinedTable, aes(x=raceSpecific)) +
  geom_bar(fill='steelblue') +
  labs(x = "",y="Count") +
  ylim(0,maxDrugIndustry+50)
pFacetDrug <- pFacetDrug + facet_wrap(~interventionDrugNonDrug)

print(pFacetDrug)
if (savePlot){
  ggsave("trialsByRaceSpecific_2_1_2020.png", units="in", width=5, height=4, dpi=600)
}

# facet wrap funder
pFacetFund<-ggplot(joinedTable, aes(x=raceSpecific)) +
  geom_bar(fill='steelblue') +
  labs(x = "",y="Count") +
  ylim(0,maxDrugIndustry+50)
pFacetFund <- pFacetFund + facet_wrap(~industryNonIndustry)

print(pFacetFund)
if (savePlot){
  ggsave("trialsByIndustrySpecific_2_1_2020.png", units="in", width=5, height=4, dpi=600)
}

# now those two together 
grid.arrange(pFacetDrug,pFacetFund,ncol=1)
pCombIndDrug <- arrangeGrob(pFacetDrug,pFacetFund,ncol=1)

print(pCombIndDrug)
if (savePlot){
  ggsave(file="trialsDrugIndustryGrid_2_1_2019.png",pCombIndDrug, units="in", width=6, height=8, dpi=600)
}

# now those two together horizontal  
pFacetFundNoText <- pFacetFund
pFacetFundNoText <- pFacetFundNoText + theme(axis.title.y = element_blank(),
                                             axis.text.y = element_blank(),
                                             axis.ticks.y = element_blank())

grid.arrange(pFacetDrug,pFacetFundNoText,ncol=2)
pCombIndDrugHorz <- arrangeGrob(pFacetDrug,pFacetFundNoText,ncol=2)


print(pCombIndDrugHorz)
if (savePlot){
  ggsave(file="trialsDrugIndustryGridHorz_2_1_2019.png",pCombIndDrugHorz, units="in", width=10, height=4, dpi=600)
}
