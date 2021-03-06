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
savePlot = FALSE
loadExcel = FALSE
loadRdataFile = TRUE

if (loadExcel){
joinedTable <- read.csv(file="C:/Users/david/SharedCode/aact/htnTableTotalNoDescrip_12_9_2019.csv", header=TRUE, sep=",",na.strings=c(""))
}

if (loadRdataFile){
  joinedTable <- readRDS(file="C:/Users/david/SharedCode/aact/htnRdata_12_9_2019.rds")
  
}
# double check that no trials are double counted
doubleCounts <- joinedTable %>% group_by(nct_id) %>% summarise(count=n())
unique(doubleCounts$count)

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
joinedTableRatio$ratio = joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'Diverse',]$yearlyCount/joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'General',]$yearlyCount
joinedTableRatio$ratioTotal = joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'Diverse',]$yearlyCount/(joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'General',]$yearlyCount + joinedTableCountGroup[joinedTableCountGroup$diverseGroup == 'Diverse',]$yearlyCount)
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


#########################################

# make plots
pInd<-ggplot(joinedTableCount, aes(x=yearStart,y=yearlyCount, group=diverse, color=diverse)) +
  geom_line()+
  geom_point() +
  labs(title='Number of Trials by Race Group Enrolled',x = "Year Registered",y="Number of Trials") +
  # scale_y_continuous(breaks=seq(0,250,10)) +
  ylim(0,max(joinedTableCount$yearlyCount)+10) +
  scale_x_continuous(breaks=seq(2009,2018,1),limits=c(2009,2018)) + 
  scale_color_jama() +
  labs(color = 'Race-Specific Enrollment ')

print(pInd)
if (savePlot){
  ggsave("trialsByYearConditions_12_9_2019.png", units="in", width=5, height=4, dpi=600)
}

pComb<-ggplot(joinedTableCountGroup, aes(x=yearStart,y=yearlyCount, group=diverseGroup, color=diverseGroup)) +
  geom_line()+
  geom_point() +
  labs(x = "year",y="count",color = 'Race-Specific Enrollment ') +
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
  ylim(0,max(joinedTableRatio$ratio)+0.015) 
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

prow <- plot_grid(pInd + theme(legend.position = "none"),
                  NULL,
                  pRatio + theme(legend.position = "none"),
                  align='vh',
                  hjust = -1,
                  nrow=1,
                  rel_widths = c(1,0.4,1))

legend <- get_legend(pInd + theme(legend.box.margin=margin(0,0,0,12)))
pTotal <- prow + draw_grob(legend,2/4.5,0,.3/3.3,1)
print(pTotal)


save_plot('trialsByYearConditionsGrid_12_9_2019.png', pTotal, ncol = 2, nrow = 1, base_height = 4, base_width=6,dpi=600)

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
