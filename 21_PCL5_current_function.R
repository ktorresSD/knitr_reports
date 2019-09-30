#########################################################################################
# Last Date modified: 1/10/2019
# Author: Katy Torres
# Description: Subset of question 20, PCL Current
##########################################################################################
pclcurrent<- function(dat0, exportdate)
{
  
#Load plyr library
 library(plyr)


#Only retain relevant variables
 datpclcurr <- subset(dat0, 
               select= c(assessment_id,vista_lastname,visit_number,
                    pcl5_m_1_memories,
                    pcl5_m_2_dream,
                    pcl5_m_3_acting,
                    pcl5_m_4_upset,
                    pcl5_m_5_physical,
                    pcl5_m_6_avoid,
                    pcl5_m_7_external,
                    pcl5_m_8_trouble,
                    pcl5_m_9_negbelief,
                    pcl5_m_10_blame,
                    pcl5_m_11_fear,
                    pcl5_m_12_interest,
                    pcl5_m_13_distant,
                    pcl5_m_14_posfeel,
                    pcl5_m_15_irritable,
                    pcl5_m_16_risk,
                    pcl5_m_17_superalert,
                    pcl5_m_18_jumpy,
                    pcl5_m_19_concentrate,
                    pcl5_m_20_sleep,
                    PCL5m_sum
               ))
 
 #Check each item is within range 1-5
 datpclcurr$within_range <- sapply(1:nrow(datpclcurr), function(x) { 
   tr <- datpclcurr[x, c(4:23)]
   all(tr >= 0 & tr <= 4)
 })
 
 
#Scoring function defined
pcl_5_current <- function(x)
{

    #attach(x)
    for (v in 1:length(x)) assign(names(x)[v], x[[v]])
    
	#PCL summary score is just the summation of all items 1-20
	#Note: This function is not designed to handle NA values (subject must have complete data)

                
  #sum of items 1-5            
	 pcl_b <- pcl5_m_1_memories +
        pcl5_m_2_dream +
        pcl5_m_3_acting +
        pcl5_m_4_upset +
        pcl5_m_5_physical 
	
  #sum of items 6 and 7
    pcl_c <- pcl5_m_6_avoid +
        pcl5_m_7_external 
    
  #sum of items 8-14    
    pcl_d <-  pcl5_m_8_trouble +
        pcl5_m_9_negbelief +
        pcl5_m_10_blame +
        pcl5_m_11_fear +
        pcl5_m_12_interest +
        pcl5_m_13_distant +
        pcl5_m_14_posfeel 
        
  #sum of items 15-20
    pcl_e <- pcl5_m_15_irritable +
        pcl5_m_16_risk +
        pcl5_m_17_superalert +
        pcl5_m_18_jumpy +
        pcl5_m_19_concentrate +
        pcl5_m_20_sleep
    
   #total symptom severity score (range - 0-80). Obtained by summing the scores for each of the 20 items
    pcl_total <- pcl_b + pcl_c + pcl_d + pcl_e
    
   #sum of all non-na entries for questions 1-20  
    pcl_incomplete <- sum(pcl5_m_1_memories,
                          pcl5_m_2_dream,
                          pcl5_m_3_acting,
                          pcl5_m_4_upset,
                          pcl5_m_5_physical,
                          pcl5_m_6_avoid,
                          pcl5_m_7_external,
                          pcl5_m_8_trouble,
                          pcl5_m_9_negbelief,
                          pcl5_m_10_blame,
                          pcl5_m_11_fear,
                          pcl5_m_12_interest,
                          pcl5_m_13_distant,
                          pcl5_m_14_posfeel,
                          pcl5_m_15_irritable,
                          pcl5_m_16_risk,
                          pcl5_m_17_superalert,
                          pcl5_m_18_jumpy,
                          pcl5_m_19_concentrate,
                          pcl5_m_20_sleep,na.rm=T)
                    
  #A PCL-5 cutpoint score of 33 appears to be a reasonable value to propose until further psychometric work is available
   pcl_33 <- as.numeric(pcl_total >= 33)
          
    if(pcl_incomplete >= 33)
    {
     pcl_33 <- 1
    }
   
    
    ##PCL B 
    #Assign TRUE to each PCL B item score that is >= 2
    pcl_5_b_gt2 <- c(pcl5_m_1_memories,pcl5_m_2_dream,pcl5_m_3_acting, pcl5_m_4_upset, pcl5_m_5_physical) >= 2
    
    #Assign TRUE if at least one PCL B is >= 2
    pcl_5_b_dsm5 <- sum(pcl_5_b_gt2) >= 1
    
    
    ##PCL C
    #Assign TRUE to each PCL C item score that is >= 2
    pcl_5_c_gt2 <- c(pcl5_m_6_avoid, pcl5_m_7_external ) >= 2
    
    #Assign TRUE if at least one PCL C is >= 2
    pcl_5_c_dsm5 <- sum(pcl_5_c_gt2) >= 1
    
    
    ##PCL D 
    #Assign TRUE to each PCL D item score that is >= 2
    pcl_5_d_gt2 <- c(pcl5_m_8_trouble ,
        pcl5_m_9_negbelief ,
        pcl5_m_10_blame ,
        pcl5_m_11_fear ,
        pcl5_m_12_interest ,
        pcl5_m_13_distant ,
        pcl5_m_14_posfeel  ) >= 2
    
    #Assign TRUE if at least two PCL Ds are >= 2
    pcl_5_d_dsm5 <- sum(pcl_5_d_gt2) >= 2
    
    
    ##PCL E 
    #Assign TRUE to each PCL E item score that is >= 2
    pcl_5_e_gt2 <- c(pcl5_m_15_irritable ,
        pcl5_m_16_risk ,
        pcl5_m_17_superalert ,
        pcl5_m_18_jumpy ,
        pcl5_m_19_concentrate ,
        pcl5_m_20_sleep  ) >= 2
      
    #Assign TRUE if at least two PCL Es are >= 2
    pcl_5_e_dsm5 <- sum(pcl_5_e_gt2) >= 2
    
    
    #DSM-5 symptom cluster severity scores can be obtained by summing the
    #scores for the items within a given cluster
    #Assign TRUE if all PCL sub-symptoms are TRUE
    pcl_5_dsm <- as.numeric(
                    sum(pcl_5_b_dsm5, pcl_5_c_dsm5, pcl_5_d_dsm5, pcl_5_e_dsm5) == 4
                        )
    
    #flag checks if data has been entered for all 20 questions
    data_complete_pcl_curr <- as.numeric( 
      sum(
        is.na(
          c(pcl5_m_1_memories,
            pcl5_m_2_dream,
            pcl5_m_3_acting,
            pcl5_m_4_upset,
            pcl5_m_5_physical,
            pcl5_m_6_avoid,
            pcl5_m_7_external,
            pcl5_m_8_trouble,
            pcl5_m_9_negbelief,
            pcl5_m_10_blame,
            pcl5_m_11_fear,
            pcl5_m_12_interest,
            pcl5_m_13_distant,
            pcl5_m_14_posfeel,
            pcl5_m_15_irritable,
            pcl5_m_16_risk,
            pcl5_m_17_superalert,
            pcl5_m_18_jumpy,
            pcl5_m_19_concentrate,
            pcl5_m_20_sleep)
        )
      ) == 0
    )
    
    data_not_attempted_pcl_curr <- as.numeric( 
      sum(
        is.na(
          c(pcl5_m_1_memories,
            pcl5_m_2_dream,
            pcl5_m_3_acting,
            pcl5_m_4_upset,
            pcl5_m_5_physical,
            pcl5_m_6_avoid,
            pcl5_m_7_external,
            pcl5_m_8_trouble,
            pcl5_m_9_negbelief,
            pcl5_m_10_blame,
            pcl5_m_11_fear,
            pcl5_m_12_interest,
            pcl5_m_13_distant,
            pcl5_m_14_posfeel,
            pcl5_m_15_irritable,
            pcl5_m_16_risk,
            pcl5_m_17_superalert,
            pcl5_m_18_jumpy,
            pcl5_m_19_concentrate,
            pcl5_m_20_sleep)
        )
      ) == 20
    )
    
    completeness_pcl_curr<- "1"
    if(!(is.na(data_not_attempted_pcl_curr))){
      if(data_not_attempted_pcl_curr==1)
      {
        completeness_pcl_curr <- "not attempted"}else{}
    }else{completeness_pcl_curr<-NA}
    
    if(!(is.na(data_complete_pcl_curr))){
      if(data_complete_pcl_curr==1){
        completeness_pcl_curr <- "complete"} else{}
    }else{completeness_pcl_curr<-NA}
    
    
    if(data_not_attempted_pcl_curr==0 & data_complete_pcl_curr==0){
      completeness_pcl_curr <- "partially completed"}else{}
    
    
    ###Infer DSM if data is incomplete
    ##PCL B
    #Assign TRUE to each PCL B item score that is >= 2
    pcl_5_b_gt2_infer <- na.omit(c(pcl5_m_1_memories,pcl5_m_2_dream,pcl5_m_3_acting, pcl5_m_4_upset, pcl5_m_5_physical)) >= 2

    #Assign TRUE if at least one PCL B is >= 2
    pcl_5_b_dsm5_infer <- sum(pcl_5_b_gt2_infer) >= 1

    ##PCL C
    #Assign TRUE to each PCL C item score that is >= 2
    pcl_5_c_gt2_infer <- na.omit(c(pcl5_m_6_avoid, pcl5_m_7_external )) >= 2

    #Assign TRUE if at least one PCL C is >= 1
    pcl_5_c_dsm5_infer <- sum(pcl_5_c_gt2_infer) >= 1

    ##PCL D
    #Assign TRUE to each PCL D item score that is >= 2
    pcl_5_d_gt2_infer <- na.omit(c(pcl5_m_8_trouble ,
        pcl5_m_9_negbelief ,
        pcl5_m_10_blame ,
        pcl5_m_11_fear ,
        pcl5_m_12_interest ,
        pcl5_m_13_distant ,
        pcl5_m_14_posfeel  )) >= 2

    #Assign TRUE if at least two PCL Ds are >= 2
    pcl_5_d_dsm5_infer <- sum(pcl_5_d_gt2_infer) >= 2

    ##PCL E
    #Assign TRUE to each PCL E item score that is >= 2
    pcl_5_e_gt2_infer <- na.omit(c(pcl5_m_15_irritable ,
        pcl5_m_16_risk ,
        pcl5_m_17_superalert ,
        pcl5_m_18_jumpy ,
        pcl5_m_19_concentrate ,
        pcl5_m_20_sleep  )) >= 2



    #Assign TRUE if at least two PCL Es are >= 2
    pcl_5_e_dsm5_infer <- sum(pcl_5_e_gt2_infer) >= 2

    #Assign TRUE if all PCL sub-symptoms are TRUE
    pcl_5_dsm_infer0 <- as.numeric(
                    sum(pcl_5_b_dsm5_infer, pcl_5_c_dsm5_infer, pcl_5_d_dsm5_infer, pcl_5_e_dsm5_infer) == 4
                        )
    
    pcl_5_dsm_infer <- pcl_5_dsm
    
    if(pcl_5_dsm_infer0 == TRUE)
    {
     pcl_5_dsm_infer = 1
    }

                
    scores <- data.frame(pcl_b,pcl_c,pcl_d,pcl_e,pcl_total, pcl_incomplete, pcl_33,pcl_5_dsm,  pcl_5_dsm_infer, data_complete_pcl_curr, data_not_attempted_pcl_curr, completeness_pcl_curr)
    
	return(scores)
}


#Calculate summary scores in data 
 pcl_5_scorescurr <- adply(datpclcurr, 1, pcl_5_current)
 
 #to anonymize data
 pcl_5_scorescurr1<- within(pcl_5_scorescurr,
                         {
                           assessment_id <- NULL
                           vista_lastname <- NULL
                         })

 
 #________________________________________________________________________________________ 
 #Report
 #----------------------------------------------------------------------------------------
 library(psych)
 
 table(pcl_5_scorescurr$completeness_pcl_curr)
 
 #completeness table
 table(pcl_5_scorescurr$completeness_pcl_curr, pcl_5_scorescurr$visit_number)
 
 #subset by visit to get report information
 v1 <- pcl_5_scorescurr[ which(pcl_5_scorescurr$visit_number==1), ]
 v2 <- pcl_5_scorescurr[ which(pcl_5_scorescurr$visit_number==2), ]
 v3 <- pcl_5_scorescurr[ which(pcl_5_scorescurr$visit_number==3), ]
 
 
 table(v1$completeness_pcl_curr)
 table(v2$completeness_pcl_curr)
 table(v3$completeness_pcl_curr)
 
 #summary statistics for total PCL
 describe(v1$pcl_total)
 describe(v2$pcl_total)
 describe(v3$pcl_total)
 describe(pcl_5_scorescurr$pcl_total)
 
 #mode
 Mode <- function(x) {
   ux <- unique(x)
   ux[which.max(tabulate(match(x, ux)))]
 }
 
 Mode(v1$pcl_total)
 Mode(v2$pcl_total)
 Mode(v3$pcl_total)
 Mode(pcl_5_scorescurr$pcl_total)
 

 
 #histograms
 par(mfrow=c(2,2))
 hist(pcl_5_scorescurr$pcl_total, breaks=10, xlab = "PCL score", ylim=c(0,45), col = c("lightyellow"), main = "PCL total Score (all visits)")
 hist(v1$pcl_total, breaks=10, xlab = "PCL score", ylim=c(0,45), col = c("lightyellow"), main = "PCL total Score (visit 1 only)")
 hist(v2$pcl_total, breaks=10, xlab = "PCL score", ylim=c(0,45), col = c("lightyellow"), main = "PCL total Score (visit 2 only)")
 hist(v3$pcl_total, breaks=10, xlab = "PCL score", ylim=c(0,45), col = c("lightyellow"), main = "PCL total Score (visit 3 only)")
 

 
 #histogram of all scores
 par(mfrow=c(1,1))
 hist(pcl_5_scorescurr$pcl_total, xlab = "PCL score", xlim=c(0,85), ylim=c(0,50), col = c("lightyellow"), main = "PCL Total Score \n (all visits)")
 

 #histogram of cases/controls based on PCL DSM-IV and PCL total score
 p0 <- hist(subset(pcl_5_scorescurr$pcl_total, pcl_5_scorescurr$pcl_5_dsm == 1), breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80), plot=FALSE)
 p1 <- hist(subset(pcl_5_scorescurr$pcl_total, pcl_5_scorescurr$pcl_5_dsm == 0), breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80), plot=FALSE)
 
 transparency_level=0.4
 plot(p0, col=rgb(1,0,0,transparency_level),freq=TRUE,xlim=c(0,85),ylim=c(0,30), ylab="Frequency", xlab="PCL_total_Score",main="Total PCL Score for Cases and Controls \n  based on PCL DSM-IV",cex.axis=1.2,cex.lab=1.2) 
 plot(p1, col=rgb(0,0,1,transparency_level),freq=TRUE,xlim=c(0,85),ylim=c(0,30), add=T)  # second
 legend('topright',legend=c("Case","Control", "Cut-off"),col=c(rgb(1,0,0,transparency_level),rgb(0,0,1,transparency_level), "black"),pch=c(19,19,95))
 abline(v=33, col = "black", lty="dashed")
 
 
 
 
 
 #________________________________________________________________________________________ 
 #Export
 #----------------------------------------------------------------------------------------
 filename <- paste("~/Biobank/21_PCL_5_monthly/pcl5_current_scored_data_export.csv", sep="")
 write.csv( pcl_5_scorescurr, filename,quote=T,row.names=F,na="NA")
 
 
 filename <- paste("~/Biobank/21_PCL_5_monthly/pcl5_current_scored_data_export_DEIDENTIFIED.csv", sep="")
 write.csv( pcl_5_scorescurr1, filename,quote=T,row.names=F,na="NA")
 
print("21_PCL_current_done")

#return completness column
myvars <- c("assessment_id", "completeness_pcl_curr")
newdata <- pcl_5_scorescurr[myvars]
return(newdata)
}





