# Build Conservation Evidence Report
#Cover Crop Review Summary file

#Filter Cover Crop Review articles and identify Paper_id #'s that we can then use to filter summary dataframe


CC_review_summary <- read.csv("CC_report_summary.csv", header=TRUE, row.names = "X")
colnames (CC_review_summary)

#Descriptors for querying dataset and producing report

#"Paper_id" <- unique identification number
#"Year_result" 
#"Group_finelevel"       
#"Response_var" 
#"Trt_id1"
#"Trt1_interaction"
#"Trt1_interaction2"     
#"Trt_id1description"
#"Trt_id2"
#"Trt2_interaction"
#"Trt2_interaction2"     
#"Trt_id2description"

#"introduction" <- introduction statement for report
#"citation_short" <- abridged citation "last name (year)" for report
#"results_short" <- condensed results statement for report
#"methods"  <- methods statement for report             
#"Reviewers_results_long" <- comprehensive results statement for report

#"citation" <- full citation for report