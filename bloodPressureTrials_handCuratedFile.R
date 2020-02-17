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
startDate = as.Date("2009-01-01")
startDateEnd = as.Date("2018-12-31")
countriesList = c("United States")
`%nin%` = Negate(`%in%`)

#########################################
# boolean options for saving
savePlot = FALSE
loadExcel = TRUE
saveData = TRUE

handCurated <- read.csv(file="C:/Users/david/SharedCode/aact/NCT_Race_All.csv", header=TRUE, sep=",",na.strings=c(""))


names(handCurated)[1] <- "nct_id"
names(handCurated)[2] <- "diverse"

# connect to database
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user="djcald", password="DD968radford")

# begin loading, filtering, selecting tables
study_tbl = tbl(src=con,'studies')
filter_dates <- study_tbl %>% select(official_title,start_date,nct_id,phase,last_known_status,study_type,enrollment,overall_status) %>% filter(start_date >= startDate && start_date <= startDateEnd && study_type == 'Interventional')  %>% collect()
filter_dates <- filter_dates%>% filter(nct_id %in% handCurated$nct_id) %>% mutate(phase = replace(phase, phase == "N/A", "Not Applicable"))

location_tbl = tbl(src=con,'countries')

# check if country is the only one in a list 
locations = location_tbl %>% select(nct_id,name)  %>% collect()
locations <- locations %>% filter(nct_id %in% handCurated$nct_id) %>%  group_by(nct_id) %>% summarize(countriesPaste = paste(name,collapse=", ")) %>% filter (countriesPaste == countriesList) %>% collect()

#locationsTotal = location_tbl %>% select(nct_id,name) %>%  collect()
#locationsTotal = locationsTotal %>% filter(nct_id %in% handCurated$nct_id) %>% group_by(nct_id) %>% summarize(countriesPaste = paste(name,collapse=", ")) %>% collect()

sponsor_tbl = tbl(src=con,'sponsors')
#sponsor <- sponsor_tbl %>% select(nct_id,agency_class) %>% collect()
#sponsor <- sponsor_tbl %>% select(nct_id,agency_class,lead_or_collaborator) %>% filter(lead_or_collaborator == 'lead')%>% collect()
sponsor <- sponsor_tbl %>%  select(nct_id,agency_class,lead_or_collaborator)%>% collect()


sponsor = sponsor %>% filter(nct_id %in% handCurated$nct_id) %>% group_by(nct_id) %>% mutate(funding = case_when(any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'NIH',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'U.S. Fed',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'NIH',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'U.S. Fed',
                                                                      TRUE ~ 'Other'))

sponsor = distinct(sponsor,nct_id,.keep_all=TRUE) %>% select(nct_id,funding)

sponsorCombined <- sponsor_tbl %>%  select(nct_id,agency_class,lead_or_collaborator)%>% collect()
sponsorCombined = sponsorCombined %>% filter(nct_id %in% handCurated$nct_id) %>% group_by(nct_id) %>% mutate(fundingComb = case_when(any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'Public',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'Public',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'Public',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'Public',
                                                                                                                 TRUE ~ 'Other'))

sponsorCombined = distinct(sponsorCombined,nct_id,.keep_all=TRUE) %>% select(nct_id,fundingComb)


calculatedValues_tbl = tbl(src=con,'calculated_values')
calculatedValues <- calculatedValues_tbl  %>% select(nct_id,were_results_reported) %>% collect()
calculatedValues <- calculatedValues %>% filter(nct_id %in% handCurated$nct_id)

#baselineMeasurements_tbl = tbl(src=con,'baseline_measurements')
#baselineMeasurements <- baselineMeasurements_tbl %>% select(nct_id,category) %>% collect()

interventions_tbl = tbl(src=con,'interventions')
interventions = interventions_tbl %>%  select(nct_id,intervention_type) %>% collect()
interventions <- interventions %>% filter(nct_id %in% handCurated$nct_id) %>% group_by(nct_id) %>% summarize(intervention_comb = paste(intervention_type,collapse=", "))

interventionTrial = interventions %>% mutate(interventionType = case_when(str_detect(tolower(intervention_comb), pattern = paste('procedure')) ~ 'Procedure',
                                                                          str_detect(tolower(intervention_comb), pattern = paste('behavioral')) ~ 'Behavioral',
                                                                          str_detect(tolower(intervention_comb), pattern = paste('device')) ~ 'Device',
                                                                          str_detect(tolower(intervention_comb), pattern = paste('biological')) ~ 'Biological',
                                                                          str_detect(tolower(intervention_comb), pattern = paste('drug')) ~ 'Drug',
                                                                          TRUE ~ 'other'))

interventionTrialCombined = interventions %>% mutate(interventionTypeCombined = case_when(str_detect(tolower(intervention_comb), pattern = paste('procedure')) ~ 'Procedure',
                                                                          str_detect(tolower(intervention_comb), pattern = paste('behavioral')) ~ 'Behavioral',
                                                                          str_detect(tolower(intervention_comb), pattern = paste('device')) ~ 'Device',
                                                                          str_detect(tolower(intervention_comb), pattern = paste('biological')) ~ 'Pharmaceutical',
                                                                          str_detect(tolower(intervention_comb), pattern = paste('drug')) ~ 'Pharmaceutical',
                                                                          TRUE ~ 'other'))

#interventions_nest <- interventions %>% group_by(nct_id) %>% nest() 
#inverventions_nest <- interventions_nest %>% mutate(Intervention = case_when(str_detect(tolower(data), pattern = paste('procedure')) ~ 'interrv',
#                                                               TRUE ~ 'drug'))

facilities_tbl = tbl(src=con,'facilities')
facilities <- facilities_tbl  %>% select(nct_id,status,name) %>%collect()
facilities_tabulated <- facilities %>% filter(nct_id %in% handCurated$nct_id) %>% group_by(nct_id) %>% tally()
facilities_tabulated <- rename(facilities_tabulated,facilitiesCount = n)
facilities_tabulated <- facilities_tabulated %>% mutate(multisite = ifelse(facilitiesCount>1,TRUE,FALSE))

study_ref_tbl = tbl(src=con,'study_references')
# study_ref <- study_ref_tbl %>% select(nct_id,pmid,reference_type,citation)%>% filter(reference_type=="results_reference") %>% collect()
study_ref <- study_ref_tbl %>% select(nct_id,pmid,reference_type,citation) %>% collect()
study_ref_tabulated <- study_ref %>% filter(nct_id %in% handCurated$nct_id) %>% filter(reference_type == 'results_reference') %>% group_by(nct_id) %>% tally()
study_ref_tabulated <- rename(study_ref_tabulated,pubCount = n)

#study_tbl_description = tbl(src=con, 'detailed_descriptions')
#filtered_table = study_tbl_description %>% select(nct_id,description) %>% collect()
#filtered_table <- filtered_table %>% filter(nct_id %in% handCurated$nct_id)

# this is a join that includes all categories, but only ones that match the description 
joinedTable <- join_all(list(filter_dates,facilities_tabulated,sponsor,sponsorCombined,locations,interventionTrial,interventionTrialCombined,calculatedValues),by='nct_id',type="full")
joinedTable <- joinedTable %>% filter(nct_id %in% locations$nct_id)
#joinedTable <- inner_join(filtered_table,joinedTable,by='nct_id')

# get rid of any NA start dates
joinedTable <- joinedTable[complete.cases(joinedTable$start_date),]


# this adds pub counts, and NAs for those that dont have pubs
joinedTable <- left_join(joinedTable,study_ref_tabulated,by='nct_id')

#joinedTable_tabulated <- joinedTable %>% group_by(nct_id) %>% tally()
#joinedTable <- joinedTable %>% mutate(diverseGroup = as.numeric(str_detect(tolower(description), pattern = paste(stringBlack, collapse = "|"))))
joinedTable <- joinedTable %>% mutate(pubCountBool = case_when(!is.na(pubCount) ~ 'TRUE',
                                                               TRUE ~ 'FALSE'))

handCuratedShrunk <- handCurated %>% filter(nct_id %in% joinedTable$nct_id) %>% collect()
joinedTable <- inner_join(joinedTable,handCuratedShrunk,by='nct_id')


joinedTable <- joinedTable %>% mutate(diverseGroup = case_when(str_detect(tolower(diverse), pattern = paste('non-race specific')) ~ 'Non-race specific',
                                                               TRUE ~ 'Race-specific'))

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

# group by year and diversity group 
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

# calculate ratio of diverse to non diverse 
joinedTableRatio <- data.frame(year = unique(joinedTableCountGroup$yearStart))
joinedTableRatio$ratio = joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'Race-specific',]$yearlyCount/joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'Non-race specific',]$yearlyCount
joinedTableRatio$ratioTotal = joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'Race-specific',]$yearlyCount/(joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'Non-race specific',]$yearlyCount + joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'Race-specific',]$yearlyCount)
joinedTableRatio$groupRatio = 'Ratio'
joinedTableRatio$label = (1:length(joinedTableRatio$year))

#########################################
# statistical testing

yearlyCount = joinedTableCountGroup$yearlyCount
lengthYC= length(yearlyCount)
chiSquareData = matrix(c(yearlyCount[1],yearlyCount[2],yearlyCount[lengthYC-1],yearlyCount[lengthYC]),nrow=2,byrow = TRUE)
colnames(chiSquareData) <- c("Diverse","General")
rownames(chiSquareData) <- c("2009","2018")
chiSquareData <- as.table(chiSquareData)
chisq.test(chiSquareData)

lmRatio <- lm(ratio~label,data=joinedTableRatio)
summary(lmRatio)

########################
if (saveData){
  saveRDS(joinedTable, file = "htnRdata_2_14_2020.rds")
  write.csv(joinedTable,'htnTableTotal_2_14_2020.csv')
  write.csv(joinedTableDiverseDiscontinued,'htnTableDiscDiverse_2_14_2020.csv')
  write.csv(joinedTableSummarizeInterv,'htnTableInterv_2_14_2020.csv')
  write.csv(joinedTableSummarizeType,'htnTableType_2_14_2020.csv')
  write.csv(joinedTableSummarizePhase,'htnTablePhase_2_14_2020.csv')
  write.csv(joinedTableSummarizeAgency,'htnTableAgency_2_14_2020.csv')
  write.csv(joinedTableSummarizeReported,'htnTableReported_2_14_2020.csv')
  write.csv(joinedTableSummarizeSite,'htnTableSite_2_14_2020.csv')
  write.csv(joinedTableSummarizeStatus,'htnTableStatus_2_14_2020.csv')
  write.csv(joinedTableSummarizeOverallStatus,'htnTableOverallStatus_2_14_2020.csv')
  write.csv(joinedTableSummarizePubCount,'htnTablePubCount_2_14_2020.csv')
}

#########################################

# make plots
pInd<-ggplot(joinedTableCount, aes(x=yearStart,y=yearlyCount, group=diverse, color=diverse)) +
  geom_line()+
  geom_point() +
  labs(title="Number of Hypertension Clinical Trials \nRegistered by Race-Group Enrolled, by Year",x = "Year Registered",y="Number of Trials") +
  # scale_y_continuous(breaks=seq(0,250,10)) +
  ylim(0,max(joinedTableCount$yearlyCount)+10) +
  scale_x_continuous(breaks=seq(2009,2018,1),limits=c(2009,2018)) + 
  scale_color_jama() +
  labs(color = 'Race-Specific Enrollment ')

print(pInd)
if (savePlot){
  ggsave("trialsByYearConditions_2_14_2020.png", units="in", width=5, height=4, dpi=600)
}

pComb<-ggplot(joinedTableCountGroup, aes(x=yearStart,y=yearlyCount, group=diverseGroup, color=diverseGroup)) +
  geom_line()+
  geom_point() +
  labs(title="Number of Hypertension Clinical Trials \nRegistered by Race-Specific Status, by Year",x = "year",y="count",color = 'Race-Specific Enrollment ') +
  #scale_y_continuous(breaks=seq(0,250,10)) +
  ylim(0,max(joinedTableCount$yearlyCount)+10) +
  scale_x_continuous(breaks=seq(2009,2018,1),limits=c(2009,2018)) +
  scale_color_jama()
print(pComb)
if (savePlot){
  ggsave("trialsByYearConditionsComb_2_14_2020.png", units="in", width=5, height=4, dpi=600)
}

pRatio<-ggplot(joinedTableRatio, aes(x=year,y=ratio)) +
  geom_line(color='steelblue')+
  geom_point(color='steelblue') +
  labs(title='Ratio of Race-Specific to Non-Race Specific \nHypertension Clinical Trials, by Year',x = "Year Registered",y="Ratio of Diverse to General Trials") +
  scale_x_continuous(breaks=seq(2009,2018,1),limits=c(2009,2018)) +
  ylim(0,max(joinedTableRatio$ratio)+0.015) 
print(pRatio)
if (savePlot){
  ggsave("trialsByYearRatio_2_14_2020.png", units="in", width=5, height=4, dpi=600)
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
  ggsave("trialsByYearRatioTotal_2_14_2020.png", units="in", width=5, height=4, dpi=600)
}

#grid.arrange(pComb,pRatio,ncol=2)
#pCombGrid <- arrangeGrob(pComb,pRatio,ncol=2)
#pComb <- plot_grid(pInd,pRatio,ncol=2,rel_widths = c(5/9,4/9))

#print(pCombGrid)
#if (savePlot){
#  ggsave(file="trialsByYearGroupGrid_2_14_2020.png",pComb, units="in", width=10, height=4, dpi=600)
#}

prow <- plot_grid(pComb + theme(legend.position = "none"),
                  NULL,
                  pRatio + theme(legend.position = "none"),
                  align='vh',
                  hjust = -1,
                  nrow=1,
                  rel_widths = c(1,0.1,1))

legend <- get_legend(pComb + theme(legend.box.margin=margin(0,0,0,12)))
pTotal <- prow + draw_grob(legend,0.9/4.5,0,.3/3.3,0.8)
print(pTotal)

if (savePlot){
  save_plot('trialsByYearGroupGrid_2_14_2020.png', pTotal, ncol = 2, nrow = 1, base_height = 4, base_width=6,dpi=600)
}

prowInd <- plot_grid(pInd + theme(legend.position = "none"),
                  NULL,
                  pRatio + theme(legend.position = "none"),
                  align='vh',
                  hjust = -1,
                  nrow=1,
                  rel_widths = c(1,0.1,1))

legend <- get_legend(pInd + theme(legend.box.margin=margin(0,0,0,12)))
pTotalInd <- prowInd + draw_grob(legend,1.4/4.5,0,.4/3.3,0.75)
print(pTotalInd)

if (savePlot){
  save_plot('trialsByYearGroupGridInd_2_14_2020.png', pTotalInd, ncol = 2, nrow = 1, base_height = 4, base_width=6,dpi=600)
}

grid.arrange(pComb,pRatioTotal,ncol=2)
pCombTotal <- arrangeGrob(pInd,pRatioTotal,ncol=2)
#pComb <- plot_grid(pInd,pRatio,ncol=2,rel_widths = c(5/9,4/9))

#print(pComb)
if (savePlot){
  ggsave(file="trialsByYearGroupGridTotal_2_14_2020.png",pCombTotal, units="in", width=10, height=4, dpi=600)
}


pHist<-ggplot(joinedTable, aes(x=numMissing)) +
  geom_histogram(binwidth=1,color="black", fill="white") +
  labs(x = "Number of Missing Data Columns",y="Count") +
  xlim(0,8)
print(pHist)
if (savePlot){
  ggsave("trialsByYearNumMissing_2_14_2020.png", units="in", width=5, height=4, dpi=600)
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

####### make facet wrap plots
# group by year and diversity group 
interventionInterest <- c("Behavioral","Pharmaceutical")
fundingInterest <- c("Public","Industry")

#joinedTableCountSelectInterv <- joinedTable %>% filter(interventionType %in% interventionInterest) %>% group_by(yearStart,diverse,interventionType) %>% tally()
#joinedTableCountSelectInterv <- rename(joinedTableCountSelectInterv,yearlyCount = n)

joinedTableCountGroupSelectInterv <- joinedTable %>% filter(interventionTypeCombined %in% interventionInterest) %>% group_by(yearStart,diverseGroup,interventionTypeCombined) %>% count()
joinedTableCountGroupSelectInterv <- rename(joinedTableCountGroupSelectInterv,yearlyCount = n)

#joinedTableCountSelectFund <- joinedTable %>% filter(fundingComb %in% fundingInterest) %>% group_by(yearStart,diverse,fundingComb) %>% tally()
#joinedTableCountSelectFund <- rename(joinedTableCountSelectFund,yearlyCount = n)

joinedTableCountGroupSelectFund <- joinedTable %>% filter(fundingComb %in% fundingInterest) %>% group_by(yearStart,diverseGroup,fundingComb) %>% count()
joinedTableCountGroupSelectFund <- rename(joinedTableCountGroupSelectFund,yearlyCount = n)

ymax = max(joinedTableCountGroupSelectFund$yearlyCount,joinedTableCountGroupSelectInterv$yearlyCount)

pGroupSelectInt<-ggplot(joinedTableCountGroupSelectInterv, aes(x=yearStart,y=yearlyCount, group=diverseGroup, color=diverseGroup)) +
  geom_line()+
  geom_point() +
  facet_wrap(~ interventionTypeCombined) +
  labs(title='Panel A. Intervention Type',x="",y="") +
  # scale_y_continuous(breaks=seq(0,250,10)) +
  ylim(0,ymax+10) +
  scale_x_continuous(breaks=seq(2009,2018,1),limits=c(2009,2018)) + 
  scale_color_jama() +
  labs(color = 'Race-Specific Enrollment ')

print(pGroupSelectInt)
#if (savePlot){
#  ggsave("trialsByYearIntervGroup_2_14_2020.png", units="in", width=5, height=4, dpi=600)
#}

pGroupSelectFund<-ggplot(joinedTableCountGroupSelectFund, aes(x=yearStart,y=yearlyCount, group=diverseGroup, color=diverseGroup)) +
  geom_line()+
  geom_point() +
  facet_wrap(~ fundingComb) +
  labs(title='Panel B. Funding Type',x = "Year Registered",y="Number of Trials") +
  # scale_y_continuous(breaks=seq(0,250,10)) +
  ylim(0,ymax+10) +
  scale_x_continuous(breaks=seq(2009,2018,1),limits=c(2009,2018)) + 
  scale_color_jama() +
  labs(color = 'Race-Specific Enrollment ')

print(pGroupSelectFund)
#if (savePlot){
#  ggsave("trialsByYearFundGroup_2_14_2020.png", units="in", width=5, height=4, dpi=600)
#}

#grid.arrange(pGroupSelectInt,pGroupSelectFund,ncol=1)
#pGroupFundInt <- arrangeGrob(pFacetDrug,pFacetFundNoText,ncol=1)

prowFundInt <- plot_grid(pGroupSelectInt + theme(legend.position = "none"),
                     pGroupSelectFund + theme(legend.position = "none"),
                     NULL,
                     align='vh',
                     hjust = -1,
                     nrow=3,
                     rel_widths = c(1,1,1),
                     rel_heights = c(3,3,1))

legend <- get_legend(pGroupSelectInt + theme(legend.box.margin=margin(0,0,0,12)))
pTotalFundInt <- prowFundInt + draw_grob(legend,1.7/4.5,0,1/3.3,0.12)
print(pTotalFundInt)


if (savePlot){
  ggsave(file="trialsDrugIndustryGridHorz_2_14_2019.png",pTotalFundInt, units="in", width=8, height=8, dpi=600)
}


####### make facet wrap plots 3 way
# group by year and diversity group 
interventionInterest <- c("Behavioral","Pharmaceutical")
fundingInterest <- c("Public","Industry","Other")

#joinedTableCountSelectInterv <- joinedTable %>% filter(interventionType %in% interventionInterest) %>% group_by(yearStart,diverse,interventionType) %>% tally()
#joinedTableCountSelectInterv <- rename(joinedTableCountSelectInterv,yearlyCount = n)

joinedTableCountGroupSelectInterv <- joinedTable %>% filter(interventionTypeCombined %in% interventionInterest) %>% group_by(yearStart,diverseGroup,interventionTypeCombined) %>% count()
joinedTableCountGroupSelectInterv <- rename(joinedTableCountGroupSelectInterv,yearlyCount = n)

#joinedTableCountSelectFund <- joinedTable %>% filter(fundingComb %in% fundingInterest) %>% group_by(yearStart,diverse,fundingComb) %>% tally()
#joinedTableCountSelectFund <- rename(joinedTableCountSelectFund,yearlyCount = n)

joinedTableCountGroupSelectFund <- joinedTable %>% filter(fundingComb %in% fundingInterest) %>% group_by(yearStart,diverseGroup,fundingComb) %>% count()
joinedTableCountGroupSelectFund <- rename(joinedTableCountGroupSelectFund,yearlyCount = n)

pGroupSelectInt<-ggplot(joinedTableCountGroupSelectInterv, aes(x=yearStart,y=yearlyCount, group=diverseGroup, color=diverseGroup)) +
  geom_line()+
  geom_point() +
  facet_wrap(~ interventionTypeCombined) +
  labs(title='Panel A. Intervention Type',x="",y="") +
  # scale_y_continuous(breaks=seq(0,250,10)) +
  ylim(0,ymax+10) +
  scale_x_continuous(breaks=seq(2009,2018,1),limits=c(2009,2018)) + 
  scale_color_jama() +
  labs(color = 'Race-Specific Enrollment ')

print(pGroupSelectInt)
#if (savePlot){
#  ggsave("trialsByYearIntervGroup_2_14_2020.png", units="in", width=5, height=4, dpi=600)
#}

pGroupSelectFund<-ggplot(joinedTableCountGroupSelectFund, aes(x=yearStart,y=yearlyCount, group=diverseGroup, color=diverseGroup)) +
  geom_line()+
  geom_point() +
  facet_wrap(~ fundingComb) +
  labs(title='Panel B. Funding Type',x = "Year Registered",y="Number of Trials") +
  # scale_y_continuous(breaks=seq(0,250,10)) +
  ylim(0,ymax+10) +
  scale_x_continuous(breaks=seq(2009,2018,1),limits=c(2009,2018)) + 
  scale_color_jama() +
  labs(color = 'Race-Specific Enrollment ')

print(pGroupSelectFund)
#if (savePlot){
#  ggsave("trialsByYearFundGroup_2_14_2020.png", units="in", width=5, height=4, dpi=600)
#}

#grid.arrange(pGroupSelectInt,pGroupSelectFund,ncol=1)
#pGroupFundInt <- arrangeGrob(pFacetDrug,pFacetFundNoText,ncol=1)

#pRowTop2 <- plot_grid(pGroupSelectInt + theme(legend.position = "none"),
#                      NULL,
#                      ncol=2,
#                      rel_widths = c(4,1),
#                      rel_heights = c(1,1))

prowFundInt <- plot_grid(pGroupSelectInt + theme(legend.position = "right"),
                         pGroupSelectFund + theme(legend.position = "none"),
                         nrow=2)

#legend <- get_legend(pGroupSelectInt + theme(legend.box.margin=margin(0,0,0,12)))
#pTotalFundInt <- prowFundInt + draw_grob(legend,1.7/4.5,0,1/3.3,0.12)
print(prowFundInt)


if (savePlot){
  ggsave(file="trialsDrugIndustryGridHorz3way_2_14_2019.png",prowFundInt, units="in", width=10, height=8, dpi=600)
}


lay <- rbind(c(1,NA),c(2))
grid.arrange(pGroupSelectInt,pGroupSelectFund + theme(legend.position = "none"),
             ncol=2,
             widths = c(8,1),
             layout_matrix = lay
)


lay <- rbind(c(1,NA),c(2))
pTest <- arrangeGrob(pGroupSelectInt,pGroupSelectFund + theme(legend.position = "none"),
                     ncol=2,
                     widths = c(8,1),
                     layout_matrix = lay
)


if (savePlot){
  ggsave(file="trialsDrugIndustryGridHorz3wayGRIDARRANGE_2_14_2019.png",pTest, units="in", width=10, height=8, dpi=600)
}
