library(xts)
library(dygraphs)
library(plyr)
library(dplyr)

load("total_postcode.rda")
load("Spatial_Zipcodes.rda")

SF_rest= read.csv("Restaurant_Scores.csv",sep=',')

#rename columns
SF_rest= SF_rest%>%select(rest_id=business_id, 
                    name=business_name, 
                    address=business_address,
                    postcode= business_postal_code, 
                    lat=business_latitude, 
                    long=business_longitude, 
                    ins_id= inspection_id, 
                    fulldate=inspection_date, 
                    score= inspection_score,
                    descr= violation_description,
                    risk_cat= risk_category)

#remove rows with missing score values & position(lat & long) values 
SF_rest=SF_rest %>% filter(is.na(lat)!=TRUE)
SF_rest=SF_rest %>% filter(is.na(score)!=TRUE)


#change date to date object and extract day, month, year into each seperate column

SF_rest$fulldate=as.Date(SF_rest$fulldate, format = "%m/%d/%Y")


SF_rest$mmyydate=format(SF_rest$fulldate, "%Y-%m")
SF_rest$day= format(SF_rest$fulldate, "%d")
SF_rest$month= format(SF_rest$fulldate, "%m")
SF_rest$year= format(SF_rest$fulldate, "%Y")



#rename risk factor
SF_rest$risk_cat= gsub("^$","No Risk", SF_rest$risk_cat)


#clean post code factor
SF_rest= SF_rest %>% filter (!(postcode %in% c("00000","Ca","CA","","941"))) %>% droplevels()

SF_rest$postcode=gsub(".*2019.*", "94110", SF_rest$postcode)

#add a none feature for the day date
unique_days=unique(sort(SF_rest[,"day"], decreasing=FALSE))
unique_days=append(unique_days,"None")

#add NA for violation description of No risks category level
levels(SF_rest$descr)[levels(SF_rest$descr)==""] = "NA"


save(SF_rest, file = "Clean_SF_rest.rda")
