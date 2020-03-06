#   SCRIPT 3 - ANALYSIS
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
    #   added columns to the dataset that included the  lead charge information in order to facilitate grouping case dockets by Primary OTN.


#   UPDATES
    #   This is an updated code varying from the original that was submitted to the Phildelphia District Attorney's Research Unit / DATA Lab 
    #   and the University of Pennsylvania's Department of Criminology. Revisions were made in Fall 2019 / Winter 2020 to incorporate feedback from advisors.


#   ---------


#   LOAD PACKAGES & DATA

    #   Packages
        library(tidyverse)
        library(MASS)
        library(stargazer)


    #   Load main working data set
        load("cp-2017-2019-lead.RData")


#   ----------
            
            
#   ROBUST LINEAR REGRESSION MODEL (See Table 1)
            
    #   Minimum Sentencing Range Values
        rlm_min <- rlm(minSentenceMonths ~ post + negotiated + post*negotiated, data = cp_2017_2019_lead)
        
        summary(rlm_min)
            
    #   Maximum Sentencing Range Values
        rlm_max <- rlm(maxSentenceMonths ~ post + negotiated + post*negotiated, data = cp_2017_2019_lead)
        
        summary(rlm_max)
        
    #   Export results to LaTeX Table
        stargazer(rlm_max, rlm_min, title="Results", align=TRUE)
        

#   ----------


#   SCATTER PLOTS TO SHOW THE VARIANCE OF DATA (See Figure 3)
        
    #   Create base plot
        p4 <- ggplot() + geom_point(data = cp_2017_2019_lead, aes(x=dispositionDate, y=minSentenceMonths), color = "blue", size = 0.1) +
                         geom_point(data = cp_2017_2019_lead, aes(x=dispositionDate, y=maxSentenceMonths), color = "black", size = 0.1) +
                         facet_wrap(~negotiatedLabel) +
        
        
        
        
        p4
        
   #    Format plots
        sent_length_plot <- p4 + labs(title = "Sentencing Lengths Per Day",
                                     subtitle = "Philadelphia Court of Common Pleas (2015-2019)",
                                     x = "Disposition Date", 
                                     y = "Months") +
          scale_x_date(labels = date_format("%m-%Y")) +


          theme_classic() + 
          
          
          theme(panel.border = element_blank(), 
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                
                axis.line = element_line(color = "#D0D0D0"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_blank(),
                
                plot.title = element_text(color = "#404040", size = 12, face = "bold"),
                plot.subtitle = element_text(color = "#404040", size = 9, face = "italic"),
                axis.title.x = element_text(color = "#404040", size = 9, face = "bold"),
                axis.title.y = element_text(color = "#404040", size = 9, face = "bold"),
                legend.title = element_blank())
        
          sent_length_plot
      
              
#   ----------
          
          
#   EXAMPLE DAY TO DEMONSTRATE VARIANCE OF SENTENCING LENGTHS AND OFFENSES CHARGED (See Appendix C, Table 2)
    #   Randomly find a date: 11/29/2017
        cp_2017_2019_lead[sample(nrow(cp_2017_2019_lead), 1), ]
        
    #   Subset all data from 11/29/2017
        Nov29_2017 <- cp_2017_2019_lead %>% filter(dispositionDate == "2017-11-29") %>%
                                            group_by(lead_charge_offenseCode, lead_charge_description, lead_charge_grade, minSentenceMonths, maxSentenceMonths)
          
        Nov29_2017 <- Nov29_2017[ , names(Nov29_2017) %in% c("dispositionDate",
                                                              "offenseDisposition",
                                                              "lead_charge_offenseCode",
                                                              "lead_charge_description",
                                                              "lead_charge_grade",
                                                              "minSentenceMonths",
                                                              "maxSentenceMonths")]
        
    #   Export to .csv file for easier formatting
        write.csv(Nov29_2017,"Nov29_2017.csv", row.names = FALSE)
        

#   ----------
        