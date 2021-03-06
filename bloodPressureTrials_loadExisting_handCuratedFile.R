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
countriesList = c("United States")
`%nin%` = Negate(`%in%`)

#########################################
# boolean options for saving
savePlot = FALSE

handCurated <- read.csv(file="C:/Users/david/SharedCode/aact/htn_race_final.csv", header=TRUE, sep=",",na.strings=c(""))
joinedTable <- readRDS(file="C:/Users/david/SharedCode/aact/htnRdata_9_16_2020.rds")

handCurated <- handCurated %>% select(nct_id,diverse)

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
  saveRDS(joinedTable, file = "htnRdata_9_16_2020.rds")
  write.csv(joinedTable,'htnTableTotal_9_16_2020.csv')
  write.csv(joinedTableNoDescrip,'htnTableTotalNoDescrip_9_16_2020.csv')
  write.csv(joinedTableDiverseDiscontinued,'htnTableDiscDiverse_9_16_2020.csv')
  write.csv(joinedTableSummarizeInterv,'htnTableInterv_9_16_2020.csv')
  write.csv(joinedTableSummarizeType,'htnTableType_9_16_2020.csv')
  write.csv(joinedTableSummarizePhase,'htnTablePhase_9_16_2020.csv')
  write.csv(joinedTableSummarizeAgency,'htnTableAgency_9_16_2020.csv')
  write.csv(joinedTableSummarizeReported,'htnTableReported_9_16_2020.csv')
  write.csv(joinedTableSummarizeSite,'htnTableSite_9_16_2020.csv')
  write.csv(joinedTableSummarizeStatus,'htnTableStatus_9_16_2020.csv')
  write.csv(joinedTableSummarizeOverallStatus,'htnTableOverallStatus_9_16_2020.csv')
  write.csv(joinedTableSummarizePubCount,'htnTablePubCount_9_16_2020.csv')
  
}


#########################################

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
  labs(title="Number of Hypertension Clinical Trials \nRegistered by Race-Specific Status, by Year",x = "Year Registered",y="Number of Trials",color = 'Race-Specific Enrollment ') +
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
  save_plot('trialsByYearGroupGridInd_2_14_2020_300.png', pTotalInd, ncol = 2, nrow = 1, base_height = 4, base_width=6,dpi=300)
  save_plot('trialsByYearGroupGridInd_2_14_2020_300.tiff', pTotalInd, ncol = 2, nrow = 1, base_height = 4, base_width=6,dpi=300)
  save_plot('trialsByYearGroupGridInd_2_14_2020_300.jpeg', pTotalInd, ncol = 2, nrow = 1, base_height = 4, base_width=6,dpi=300)
  
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
  ggsave("trialsByIndustrySpecific_2_14_2020.png", units="in", width=5, height=4, dpi=600)
}

# now those two together 
grid.arrange(pFacetDrug,pFacetFund,ncol=1)
pCombIndDrug <- arrangeGrob(pFacetDrug,pFacetFund,ncol=1)

print(pCombIndDrug)
if (savePlot){
  ggsave(file="trialsDrugIndustryGrid_9_16_2020.png",pCombIndDrug, units="in", width=6, height=8, dpi=600)
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
  ggsave(file="trialsDrugIndustryGridHorz_9_16_2020.png",pCombIndDrugHorz, units="in", width=10, height=4, dpi=600)
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
  ggsave(file="trialsDrugIndustryGridHorz_9_16_2020.png",pTotalFundInt, units="in", width=8, height=8, dpi=600)
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
  ggsave(file="trialsDrugIndustryGridHorz3way_9_16_2020.png",prowFundInt, units="in", width=10, height=8, dpi=600)
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
  ggsave(file="trialsDrugIndustryGridHorz3wayGRIDARRANGE_9_16_2020_300.png",pTest, units="in", width=10, height=8, dpi=300)
  ggsave(file="trialsDrugIndustryGridHorz3wayGRIDARRANGE_9_16_2020_300.tiff",pTest, units="in", width=10, height=8, dpi=300)
  ggsave(file="trialsDrugIndustryGridHorz3wayGRIDARRANGE_9_16_2020_300.jpeg",pTest, units="in", width=10, height=8, dpi=300)
}

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
  ggsave(file="trialsDrugIndustryGridHorz3way_9_16_2020.png",prowFundInt, units="in", width=10, height=8, dpi=600)
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
  ggsave(file="AAonly_trialsDrugIndustryGridHorz3wayGRIDARRANGE_9_16_2020_300.png",pTest, units="in", width=10, height=8, dpi=300)
  ggsave(file="AAonly_trialsDrugIndustryGridHorz3wayGRIDARRANGE_9_16_2020_300.tiff",pTest, units="in", width=10, height=8, dpi=300)
  ggsave(file="AAonly_trialsDrugIndustryGridHorz3wayGRIDARRANGE_9_16_2020_300.jpeg",pTest, units="in", width=10, height=8, dpi=300)
}
