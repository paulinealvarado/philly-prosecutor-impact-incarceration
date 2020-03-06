#   SCRIPT 1 - DATA MANAGEMENT
    #   Pauline I. Alvarado
    #   Spring 2019 Thesis (MS Criminology & Master of Public Adminsitration)
    #   University of Pennsylvania
    #   Title: The Impact of the 2018 Prosecutorial-Driven Initiative on Incarceration Sentence Lengths in Philadelphia
    #   Thesis advisor: Dr. John MacDonald
    #   Partner Agency: Philadelphia District Attorney's Office
    #   Partner Agency Advisors: Dr. Oren Gur & Michael Hollander, Esq


#   ----------


#   DATA SOURCE
    #   Administrative Office of Pennsylvania Courts Philadelphia Records (2015-2019) were provided by the Philadelphia 
    #   District Attorney's Office. Mike Hollander from the Research Unit of the Philadelphia District Attorney's Office (DAO) 
    #   added columns to the dataset that included the lead charge information in order to facilitate grouping case dockets by Primary OTN.
  

#   UPDATES
    #   This is an updated code varying from the original that was submitted to the Phildelphia District Attorney's Research Unit / DATA Lab 
    #   and the University of Pennsylvania's Department of Criminology. Revisions were made in Fall 2019 / Winter 2020 to incorporate feedback from advisors.


# ---------


#   LOAD PACKAGES & DATA

    #   Packages
        library(tidyverse)
        library(lubridate)
        library(stringi)


    #   Load and view data # 173,520 case dockets, 33 variables
        sent_2015_2019 <- read.csv("aopc-sentences-2015-2019-with-lead.csv", na.strings = c("", "NA"), colClasses = c("character"))

        glimpse(sent_2015_2019) 
        
        
# ----------
        

#   CLEAN UP DATES AND FILTER CONVICTIONS SENTENCED TO INCARCERATION/CONFINEMENT
        
    #   Function: converts a sting (e.g. "5 days, 3 hours") to a dbl of months
        convertTimePeriodToMonths <- function(timePeriod) {
            hours <- stri_match_first_regex(timePeriod, "(\\d+\\.?\\d{0,2}) hour{1}.", opts_regex=stri_opts_regex(case_insensitive=TRUE))
            days <- stri_match_first_regex(timePeriod, "(\\d+\\.?\\d{0,2}) day{1}.", opts_regex=stri_opts_regex(case_insensitive=TRUE))
            weeks <- stri_match_first_regex(timePeriod, "(\\d+\\.?\\d{0,2}) week{1}.", opts_regex=stri_opts_regex(case_insensitive=TRUE))
            months <- stri_match_first_regex(timePeriod, "(\\d+\\.?\\d{0,2}) month{1}.", opts_regex=stri_opts_regex(case_insensitive=TRUE))
            years <- stri_match_first_regex(timePeriod, "(\\d+\\.?\\d{0,2}) year{1}.", opts_regex=stri_opts_regex(case_insensitive=TRUE))
          
            hours <- ifelse(is.na(hours[2]), 0, as.numeric(hours[2]))
            days <- ifelse(is.na(days[2]), 0, as.numeric(days[2]))
            weeks <- ifelse(is.na(weeks[2]), 0, as.numeric(weeks[2]))
            months <- ifelse(is.na(months[2]), 0, as.numeric(months[2]))
            years <- ifelse(is.na(years[2]), 0, as.numeric(years[2]))
          
            return(hours / 24 / 30 + days / 30 + weeks * 7 /30 + months + years * 12)
        }
        
        convertTimePeriodToMonthsVec <- function(x) {
            sapply(x, function(x) {convertTimePeriodToMonths(x)})
        }
        
    #   Apply function and filter convictions that lead to incarceration/confinement
        inc_2015_2019 <- sent_2015_2019 %>% filter(!offenseDisposition %in% c("Nolle Prossed", 
                                                                              "Not Guilty", 
                                                                              "Withdrawn", 
                                                                              "Proceed to Court (Conviction Reversed)", 
                                                                              "Held for Court", 
                                                                              "Proceed to Court (Mistrial)", 
                                                                              "Disposed at Lower Court"),
                                                   sentenceType == "Confinement") %>%
                                              mutate(dispositionDate = as.Date(dispositionDate),
                                                     minSentenceMonths = convertTimePeriodToMonthsVec(minPeriod),
                                                     maxSentenceMonths = convertTimePeriodToMonthsVec(maxPeriod))

                
#   ----------
  
              
#   REMOVE MISSING VALUES
    
    #   Keep only variables that are useful within the scope of the research
        inc_2015_2019 <- inc_2015_2019[ , names(inc_2015_2019) %in% c("docketNumber",
                                                                      "primaryOtn",
                                                                      "dispositionDate",
                                                                      "offenseDisposition",
                                                                      "lead_charge_offenseCode",
                                                                      "lead_charge_offenseCodeWithSubsection",
                                                                      "lead_charge_description",
                                                                      "lead_charge_grade",
                                                                      "minSentenceMonths",
                                                                      "maxSentenceMonths")]
        
    #   Remove 0's
        inc_2015_2019 <- inc_2015_2019 %>% filter(minSentenceMonths > 0, maxSentenceMonths > 0)
        
    #   Remove NA's
        na <-sapply(inc_2015_2019, function(y) sum(length(which(is.na(y)))))
        (na <- data.frame(na)) # view
        
        inc_2015_2019 <- na.omit(inc_2015_2019) # remove NA's
        
        rm(na) #remove from dataframe from workspace 
        

#   ----------
        

#   SUBSET DATASET
        
    #   Subset Case Dockets from 4/1/2017 to 1/31/2019 to follow an interrupted time series design (11 months before
    #   and after the Krasner Memo)
        inc_2017_2019 <- inc_2015_2019 %>% filter(dispositionDate >= ymd("2017-04-01"),
                                                  dispositionDate <= ymd("2019-01-31"))
        
        range(inc_2017_2019$dispositionDate) # check date range
        
    
    #   Subset case dockets from 2017-2019 data by court of origin (Municipal Court vs Court of Common Pleas)
        cp_2017_2019 <- subset(inc_2017_2019, grepl("CP", inc_2017_2019$docketNumber)) # common pleas
        mc_2017_2019 <- subset(inc_2017_2019, grepl("MC", inc_2017_2019$docketNumber)) # municipal court
        
    
    #   Which court to use?
        #   Total incarceration cases (overall) 2017-2019: 13,067
            nrow(inc_2017_2019)
            
        #   Percentage of total incarceration case dockets (2017-2019) disposed in CP: 87.37%
            (nrow(cp_2017_2019) / nrow(inc_2017_2019)) * 100
            
        #   Total of case dockets in CP: 11,417
            nrow(cp_2017_2019)
            
        # Drop MC from global environment
            rm(mc_2017_2019)

        
#   ----------
            

#   AGGREGATE / TRANSFORM DATA
            
    #   Group 2017-2019 CP case dockets by Primary OTN and keep the lead charge (most serious charge per individual, per incident)
        cp_2017_2019_lead <- cp_2017_2019 %>% group_by(primaryOtn) %>% distinct(primaryOtn, .keep_all = TRUE)    
    
    #   Total of unique cases: 6,543
        nrow(cp_2017_2019_lead)
        
        
#   ----------
        

#   CREATE DUMMY INDEPENDENT VARIABLES
    
    #   Dummary variable 1: "post"
        #   Pre-Memo = 0 (4/1/2017 - 3/1/2018)
        #   Post-Memo = 1 (3/1/2018 - 1/31/2019)
        #   Why: Interrupted time series design to measure the efect of the policy change. 
        #   Note: Post-memo period starts the month after the memo release to account for implementation rollout.
            cp_2017_2019_lead$post <- as.numeric(ifelse(cp_2017_2019_lead$dispositionDate >= "2018-03-01", 1, 0))
            cp_2017_2019_lead$postLabel <- ifelse(cp_2017_2019_lead$post == 1, "Post-Memo", "Pre-Memo") 

        
    #   Dummy variable 2: "negotiated"
        #   Negotiated Guilty Plea = 1
        #   Other Findings of Guilt = 0
        #   Why: Create treatment group (1) and control group (0) to determine causal relationship of plea deals.
            cp_2017_2019_lead$negotiated <- as.numeric(ifelse(cp_2017_2019_lead$offenseDisposition == "Guilty Plea - Negotiated", 1, 0))
        
    
    #   Create column to contain cleaner labels for creation of graph and tables.
        cp_2017_2019_lead$postLabel <- ifelse(cp_2017_2019_lead$post == 1, "Post-Memo", "Pre-Memo") 
        cp_2017_2019_lead$negotiatedLabel <- ifelse(cp_2017_2019_lead$negotiated == 1, "Negotiated Guilty Plea", "Other Findings of Guilt")
        

    #   View random rows to check if variables are coded correctly.
        View(cp_2017_2019_lead[sample(nrow(cp_2017_2019_lead), 3), ])
        
        
        
#   ----------
        

#   SAVE DATASETS
        
    #   Save dataset with all case dockets disposed to incarceration (2015-2017)
        save(inc_2015_2019, file = "inc-2015-2019.RData")
        
    #   Save dataset with all case dockets disposed to incarceration in CP (2017-2019)
        save(cp_2017_2019, file = "cp-2017-2019.RData")
        
    #   Save MAIN WORKING DATASET, all unique cases disposed ot incarceration in CP (2017-2019) with 2 new dummy variables
        save(cp_2017_2019_lead, file = "cp-2017-2019-lead.RData")
        
        
        
        
        
        
        
        
        
        
        
        
        
  
        

