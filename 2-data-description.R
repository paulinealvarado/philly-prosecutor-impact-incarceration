#   SCRIPT 2 - DATA DESCRIPTION
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


# ---------

#   LOAD PACKAGES & DATA

    #   Packages
        library(tidyverse)
        library(lubridate)
        library(ggplot2)
        library(scales)


    #   Load main working data set
        load("cp-2017-2019-lead.RData")
      

#   ----------

        
#   COUNTS & DESCRIPTIVE STATISTICS
        
    #   Minimum Sentencing Range Values    
        desc_stats_min <- cp_2017_2019_lead %>% group_by(negotiatedLabel, postLabel) %>%
                                                summarise(Median = median(minSentenceMonths),
                                                          Min = min(minSentenceMonths),
                                                          Max = max(minSentenceMonths),
                                                          Count = n())
    #   Maximum Sentencing Range Values    
        desc_stats_max <- cp_2017_2019_lead %>% group_by(negotiatedLabel, postLabel) %>%
                                                summarise(Median = median(maxSentenceMonths),
                                                          Min = min(maxSentenceMonths),
                                                          Max = max(maxSentenceMonths),
                                                          Count = n())
        

#   ----------
        
        
#   CHECK OVERALL TREND IN NUMBER OF CASE DOCKETS DISPOSED (Used in Appendix B, Figure 4)
    
    #   Load data set with case dockets sentenced to incarceration (2015-2019)
        load("inc-2015-2019.RData")
        inc_2015_2019$negotiatedLabel <- ifelse(inc_2015_2019$offenseDisposition == "Guilty Plea - Negotiated", "Negotiated Guilty Plea", "Other Findings of Guilt")
        
        
    #   Aggregate number of case dockets per day
        trend_disp_count <- inc_2015_2019 %>% count(dispositionDate, negotiatedLabel)
        
    #   Create base scatter plot with trend line
        p1 <- ggplot(trend_disp_count, aes(x = dispositionDate, y = n)) + 
                                           geom_point(size = 0.4) + 
                                           geom_smooth(method = "lm") + 
                                           facet_wrap(~negotiatedLabel)
        
    #   Format scatter plot
        trend_disp_plot <- p1 + labs(title = "Number of Case Dockets Sentenced to Incarceration by Year",
                                    subtitle = "Philadelphia Court of Common Pleas (2015-2019)",
                                    x = "Year", 
                                    y = "Number of Unique Cases") +
                                    scale_x_date(labels = date_format("%Y"), breaks=date_breaks("1 year")) +
                                    scale_y_continuous(limits = c(0, 80)) +
          
                                geom_vline(xintercept = as.numeric(as.Date("2018-02-15")), linetype = "dotted", color = "blue", size = 0.5) +
          
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
                                      legend.title = element_blank()
                                )
        
    #   View
        trend_disp_plot
        
        
#   ----------
        

#   FREQUENCY DISTRIBUTIONS
    #   Add factor levels
        cp_2017_2019_lead$postLabel = factor(cp_2017_2019_lead$postLabel, levels = c("Pre-Memo", "Post-Memo"))
        
    #   Minimum Sentencing Range Value (See Figure 1)
        #   Base histogram
            p2 <- ggplot(cp_2017_2019_lead, aes(x = minSentenceMonths)) + 
                                               geom_histogram(color = "black", fill = "white") + 
                                               facet_wrap(postLabel~negotiatedLabel) + 
                                                geom_vline(xintercept = median(cp_2017_2019_lead$minSentenceMonths), linetype = "dotted", color = "blue", size = 0.5)
        #   Format histogram
            hist_min <- p2 + labs(title = "Frequency Distribution of Minimum Sentencing Range Values by Time Period and Disposition Type",
                                  subtitle = "Unique cases sentenced to incarceration from the Philadelphia Court of Common Pleas (4/1/2017-1/31/2019)",
                                  x = "Time Period", 
                                  y = "Sentence Length (in Months)") + 
          
          
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
                                    axis.title.y = element_text(color = "#404040", size = 9, face = "bold")
                              )
        
        #   View Graph
            hist_min
          
            
    #   Maximum Sentencing Range Value (See Figure 2)
        #   Base histogram
              p3 <- ggplot(cp_2017_2019_lead, aes(x = maxSentenceMonths)) + 
                                                  geom_histogram(color = "black", fill = "white") + 
                                                  facet_wrap(postLabel~negotiatedLabel) + 
                                                  geom_vline(xintercept = median(cp_2017_2019_lead$maxSentenceMonths), linetype = "dotted", color = "blue", size = 0.5)
             
        #   Format histogram
            hist_max <- p3 + labs(title = "Frequency Distribution of Maximum Sentencing Range Values by Time Period and Disposition Type",
                                  subtitle = "Unique cases sentenced to incarceration from the Philadelphia Court of Common Pleas (4/1/2017-1/31/2019)",
                                  x = "Time Period", 
                                  y = "Sentence Length (in Months)") + 
              
                
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
                                    axis.title.y = element_text(color = "#404040", size = 9, face = "bold")
                              )
                            
              #   View graph
                  hist_max
                  
         
#   ----------
                