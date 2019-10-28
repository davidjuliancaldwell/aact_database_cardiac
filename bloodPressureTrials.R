
library(RPostgreSQL)
library(plyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
#strings = c('black','asian','african american','hispanic')
stringBlack = c('black','african american')
stringHisp = c('hispanic','latino','latina')
startDate = as.Date("2009-01-01")
termsSearchMesh = c('hypertension','blood pressure','prehypertension')
termsSearchCondTitle = c('blood pressure','diastolic','systolic','hypertension')
countriesList = c("United States")
`%nin%` = Negate(`%in%`)

saveData = FALSE
savePlot = FALSE


drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user="djcald", password="DD968radford")

study_tbl = tbl(src=con,'studies')
filter_dates <- study_tbl %>% select(official_title,start_date,nct_id,phase,last_known_status,study_type) %>% filter(start_date >= startDate)  %>% collect()

location_tbl = tbl(src=con,'countries')
locations <- location_tbl %>% select(nct_id,name) %>% filter(name %in% countriesList) %>% collect()

sponsor_tbl = tbl(src=con,'sponsors')
#sponsor <- sponsor_tbl %>% select(nct_id,agency_class) %>% collect()
sponsor <- sponsor_tbl %>% select(nct_id,agency_class,lead_or_collaborator) %>% filter(lead_or_collaborator == 'lead')%>% collect()

calculatedValues_tbl = tbl(src=con,'Calculated_Values')
calculatedValues <- calculatedValues_tbl %>% select(nct_id,were_results_reported) %>% collect()

baselineMeasurements_tbl = tbl(src=con,'Baseline_Measurements')
baselineMeasurements <- baselineMeasurements_tbl %>% select(nct_id,category) %>% collect()

interventions_tbl = tbl(src=con,'Interventions')
interventions = interventions_tbl %>% select(nct_id,intervention_type) %>% collect()


facilities_tbl = tbl(src=con,'facilities')
facilities <- facilities_tbl %>% select(nct_id,status,name) %>%collect()
facilities_tabulated <- facilities %>% group_by(nct_id) %>% tally()

study_tbl_browse_conditions = tbl(src=con, 'browse_conditions')
condsMesh <- study_tbl_browse_conditions %>% select(nct_id,mesh_term) %>% filter(tolower(mesh_term) %in% termsSearchMesh) %>% collect()

study_tbl_conditions = tbl(src=con, 'conditions')
condsCond <- study_tbl_conditions %>% select(nct_id,downcase_name) %>%filter(downcase_name %in% termsSearchCondTitle) %>% collect()

condsTitle <- filter_dates %>% filter(str_detect(tolower(official_title),pattern = paste(termsSearchCondTitle,collapse="|"))) 

study_tbl_description = tbl(src=con, 'detailed_descriptions')
filtered_table = study_tbl_description %>% select(nct_id,description) %>% filter((nct_id %in% condsMesh$nct_id) | (nct_id %in% condsCond$nct_id) | (nct_id %in% condsTitle$nct_id)) %>% collect()
#filtered_table = study_tbl_description %>% filter((nct_id %in% condsMesh$nct_id) | (nct_id %in% condsCond$nct_id)) 
#filtered_table = study_tbl_description %>% filter(nct_id %in% condsMesh$nct_id) %>% collect()
#joinedTable %>% filtered_table %>% inner_join(facilities_tabulated,by="nct_id",copy=TRUE)
joinedTable <- join_all(list(filtered_table,facilities_tabulated,sponsor,filter_dates,locations),by='nct_id',type="inner")
#joinedTable_tabulated <- joinedTable %>% group_by(nct_id) %>% tally()
#joinedTable <- joinedTable %>% mutate(diverseGroup = as.numeric(str_detect(tolower(description), pattern = paste(stringBlack, collapse = "|"))))

joinedTable <- joinedTable %>% mutate(diverseGroup = case_when(str_detect(tolower(description), pattern = paste(c(stringBlack,stringHisp), collapse = "|")) ~ 'diverse',
                                                               TRUE ~ 'not diverse'))

joinedTable <- joinedTable %>% mutate(diverse = case_when(str_detect(tolower(description), pattern = paste(stringBlack, collapse = "|")) ~ 'black',
                                 str_detect(tolower(description), pattern = paste(stringHisp, collapse = "|"))~  'hispanic',
                                 TRUE ~ 'not diverse'))
joinedTable <- joinedTable %>% mutate(yearStart=year(joinedTable$start_date))
joinedTableCount <- joinedTable %>% group_by(yearStart,diverseGroup) %>% count()
joinedTableSummarize <- joinedTable %>% group_by(diverseGroup,agency_class,phase) %>% count()

if (saveData){
write.csv(joinedTable,'htnTable_10_24_2019.csv')
write.csv(joinedTableSummarize,'htnTableSummarize_10_24_2019.csv')
}

p<-ggplot(joinedTableCount, aes(x=yearStart,y=nn, group=diverseGroup, color=diverseGroup)) +
  geom_line()+
  geom_point() +
  labs(x = "year",y="count",title = "Number of Blood Pressure Trials Started Per Year") +
  scale_y_continuous(breaks=seq(0,250,10)) +
  scale_x_continuous(breaks=seq(2009,2019,1),limits=c(2009,2019)) 
print(p)
if (savePlot){
ggsave("trialsByYearConditionsHispBlack.png", units="in", width=5, height=4, dpi=600)
}

p<-ggplot(joinedTableCount, aes(x=yearStart,y=nn, group=diverse, color=diverse)) +
  geom_line()+
  geom_point() +
  labs(x = "year",y="count",title = "Number of Blood Pressure Trials Started Per Year") +
  scale_y_continuous(breaks=seq(0,250,10)) +
  scale_x_continuous(breaks=seq(2009,2019,1),limits=c(2009,2019)) 
print(p)
if (savePlot){
ggsave("trialsByYearConditionsComb.png", units="in", width=5, height=4, dpi=600)
}


### scratch below


filteredNum = filtered_table %>% filter(str_detect(tolower(description), pattern = paste(strings, collapse = "|")))
filteredNumNotDiverse = filtered_table %>% filter(nct_id %nin% filteredNum$nct_id)

joinedTableFirst <- filter_dates %>% inner_join(filteredNum, by = "nct_id",copy = TRUE)
joinedTable <- joinedTableFirst %>% inner_join(locations,by="nct_id",copy=TRUE) %>% collect()
yearly_counts <- year(pull(joinedTable,start_date)) 
yearly_counts = as.data.frame(yearly_counts)
colnames(yearly_counts) <- c("year")
yearly_counts_summed <- yearly_counts %>% count(year)
yearly_counts_summed$diverse = 'true'


joinedTableNotDiverseFirst <- filter_dates %>% inner_join(filteredNumNotDiverse, by = "nct_id",copy = TRUE)
joinedTableNotDiverse <- joinedTableNotDiverseFirst %>% inner_join(locations,by="nct_id",copy=TRUE) %>% collect()
yearly_counts_NotDiverse <- year(pull(joinedTableNotDiverse,start_date))
yearly_counts_NotDiverse = as.data.frame(yearly_counts_NotDiverse)
colnames(yearly_counts_NotDiverse) <- c("year")
yearly_counts_summed_NotDiverse <- yearly_counts_NotDiverse %>% count(year)
yearly_counts_summed_NotDiverse$diverse = 'false'

yearly_counts_total <- rbind(yearly_counts_summed,yearly_counts_summed_NotDiverse)


p<-ggplot(yearly_counts_total, aes(x=year,y=n, group=diverse, color=diverse)) +
  geom_line()+
  geom_point() +
  labs(x = "year",y="count",title = "Number of Blood Pressure Trials Started Per Year") +
  scale_y_continuous(breaks=seq(0,250,10)) +
  scale_x_continuous(breaks=seq(2009,2019,1),limits=c(2009,2019)) 
print(p)
#ggsave("trialsByYearConditionsTitle.png", units="in", width=5, height=4, dpi=600)