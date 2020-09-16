### R code for "Trends in Hypertension Clinical Trials Focused on Interventions Specific for Black Americans: An analysis of ClinicalTrials.gov"

This is the R code to parse ClinicalTrials.gov data compiled by AACT. Running the file **"bloodPressureTrials_handCuratedFile.R"** will reproduce the figures and generate the compiled data present in the manuscript **"Trends in Hypertension Clinical Trials Focused on Interventions Specific for Black Americans: An analysis of ClinicalTrials.gov".**

The source directory/working directory should be set to wherever this R file is saved, and the csv file **"nct_race_final.csv"** should be within the same directory. After running the **"bloodPressureTrials_handCuratedFile.R"** script, a datafile titled **"htnRdata_9_16_2020.rds"** will be created. This can be used in the **"bloodPressureTrials_loadExisting_handCuratedFile.R"**.  

Before running the **"bloodPressureTrials_handCuratedFile.R"** script, an individual must make an account with the
AACT website via the following link.  
*https://aact.ctti-clinicaltrials.org/users/sign_up*
To run the **"bloodPressureTrials_loadExisting_handCuratedFile.R"**, a database account is not needed. Therefore, if one wishes to use the existing compiled data and CSV, the above script can be used.

Within the R script **"bloodPressureTrials_handCuratedFile.R"** are several variables that should be set before running the script.   
**savePlot** - this variable is boolean (TRUE/FALSE), and determines whether the plots generated will be saved in the working directory.    
**saveData** - this variable is boolean (TRUE/FALSE), and determines whether the data generated will be saved in the working directory.  
**userAACT** - this variable needs to be set as a string to whatever user name the user has setup with the above link. e.g. "user_name".  
**passwordAACT** - this variable needs to be set as a string to whatever password the user has setup with the above link. e.g. "user_name".


---
R Packages required for running this analysis.
tidyr  
RPostgreSQL  
dplyr  
plyr  
stringr  
lubridate  
ggplot2  
ggsci  
gridExtra  
cowplot  
here  

Install with install.packges('packageName')

---

BSD-3 License  
David Caldwell, on behalf of the coauthors
