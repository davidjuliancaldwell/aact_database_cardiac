library(RPostgreSQL)
library(dplyr)
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user="djcald", password="DD968radford")

aact_sample <- dbGetQuery(con, "select * from ctgov.all_conditions where nct_id='NCT00000146'")
aact_sample <- dbGetQuery(con,"select count(*) from studies")
aact_sample <- dbGetQuery(con, "select * from ctgov.Browse_Conditions where mesh_term= 'Hypertension'")
write.csv(aact_sample, file='aact_sample.csv')
print(aact_sample)

study_tbl = tbl(src=con, 'studies')
study_tbl = tbl(src=con, 'detailed_descriptions')

head(study_tbl,3)
study_tbl %>% select(study_type) %>%
  group_by(study_type) %>% summarize(count = n()) %>%
  collect()
x  = study_tbl %>% filter(official_title %like% '%TP53%') %>% collect()
dim(x)
x$official_title[1:3]

aact_sample <- dbGetQuery(con, "select * from ctgov.brief_summaries where nct_id='NCT03726710'")

grepl('black',aact_sample$description)


aact_sample <- dbGetQuery(con, "select * from ctgov.brief_summaries inner join ctgov where nct_id='NCT03726710'")
grepl('black',aact_sample$description)
######
study_tbl_brief_summaries = tbl(src=con,'brief_summaries')
x = study_tbl_brief_summaries %>% filter(nct_id == "NCT03726710") %>% select(description) %>% collect()


grepl('black',x)



###

library(RPostgreSQL)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
strings = c('black','asian','african american','hispanic')
startDate = as.Date("2009-01-01")
termsSearch = c('Hypertension','Blood pressure','Prehypertension')
`%nin%` = Negate(`%in%`)


drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user="djcald", password="DD968radford")

study_tbl = tbl(src=con,'studies')
filter_dates <- study_tbl %>% filter(start_date >= startDate) 

study_tbl_conditions = tbl(src=con, 'browse_conditions')
conds <- study_tbl_conditions %>% filter(mesh_term %in% termsSearch) %>% collect()

study_tbl_description = tbl(src=con, 'detailed_descriptions')
filtered_table = study_tbl_description %>% filter(nct_id %in% conds$nct_id) %>% collect()

filtered_table_description = filtered_table %>% select(description) %>% collect()
filteredNum = filtered_table %>% filter(str_detect(tolower(description), pattern = paste(strings, collapse = "|"))) %>% collect()
filteredNumNotDiverse = filtered_table %>% filter(nct_id %nin% filteredNum$nct_id)

joinedTable <- filter_dates %>% inner_join(filteredNum, by = "nct_id",copy = TRUE) %>% collect()
yearly_counts <- year(pull(joinedTable,start_date)) 
yearly_counts = as.data.frame(yearly_counts)
colnames(yearly_counts) <- c("year")
yearly_counts_summed <- yearly_counts %>% count(year)
yearly_counts_summed$diverse = 'true'


joinedTableNotDiverse <- filter_dates %>% inner_join(filteredNumNotDiverse, by = "nct_id",copy = TRUE) %>% collect()
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
ggsave("trialsByYear.png", units="in", width=5, height=4, dpi=600)



x = study_tbl_description %>% filter(nct_id == "NCT03726710") %>% select(description) %>% collect()

study_tbl_studies = tbl(src=con, 'studies')


