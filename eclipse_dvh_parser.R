#Read Before executing. 
#Place all DVH files with .txt extension into a subfolder called DVH. 
#Make a subfolder called dvhdata to store the extracted DVH data.  
#Make a subfolder called pointdose to store the point doses extracted. 
#The function will concatanate the patient ID, date of plan approval and plan name as the file names. 


#Save all library calls in top to prevent execution in a loop
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(arrow)
setwd("/cloud/project/doses/")

#Read all text files in folder
fileNames <- Sys.glob("DVH/*/*.txt")


for (fileName in fileNames) {
  
  fl <-fileName
  a <- readLines(fl)
  
  randn <- sample(200:10000,1)
  
  #Use this for troubleshooting the code below. Allows you to test the code on a single file. 
  #fl <- "/cloud/project/doses/DVH/16_007830/BR2.txt"
  #a <- readLines(fl)
  
  #Read Patient ID and Store It
  
  id <- str_extract(a,"\\bPatient ID(.*?)$")
  id <- id[!is.na(id)]
  id <- str_remove_all(id,"\\bPatient ID|\\:")
  id <- trimws(id)
  
  #Extract the date of plan approval and Store It
  
  date <- str_extract(a,"\\bPlan Status(.*?)$")
  date <- date[!is.na(date)]
  date <- str_remove_all(date,"Plan|Status|Treatment|Approved")
  date <- str_remove_all(date,"\\bby(.*?)$")
  #date <- mdy_hms(date)
  
  #Read the Total Dose and the units in the DVH file
  
  total_dose <- str_extract(a,"Total dose(.*?)$")
  total_dose <- total_dose[!is.na(total_dose)]
  units <- str_extract(total_dose,"cGy|Gy")
  total_dose <- str_remove(total_dose,"Total(.*?)\\:")
  total_dose <- as.numeric(as.character(trimws(total_dose)))
  
  #Extract the Plan Name. 
  
  plan_name <- str_extract(a,"Plan:(.*?)$")
  plan_name <- plan_name[!is.na(plan_name)]
  plan_name <- plan_name[1]
  plan_name <- str_remove(plan_name,"Plan:")
  plan_name <- trimws(plan_name)
  
  #Now we split the text files into parts. 
  #First we read the data into a data frame
  
  b <- read_table(fl,col_names=F,guess_max = 1)
  
  
  #We obtain the row numbers at which the word Structure: appears. These are the rows from which the DVH description of the structure starts. 
  #First we obtain the index numbers of the row -1
  rowscheck <- which(grepl("\\bStructure\\b",b$X1))-1
  #Then we obtain index numbers
  rowscheck2 <- which(grepl("\\bStructure\\b",b$X1))
  ##Append the two vectors created above
  rowscheck <- append(rowscheck,rowscheck2)
  ##Append 1 to the vector
  rowscheck <- append(1,rowscheck)
  ##Append the maximum number of rows to the vector
  rowscheck <- append(rowscheck,nrow(b))
  ##Sort the vector in ascending order
  rowscheck <- sort(rowscheck,decreasing=F)
  
  #Create a 2 coloumn matrix with the row numbers at which we will split the dataframe
  rowm <- matrix(rowscheck,ncol=2,byrow=T)
  
  #Now split the dataframe into a list of dataframes based on the row numbers. Note the value 1 in apply means the function is applied to the row and not the coloumn. 
  bl <- apply(rowm,1,function(x) b[c(x[1]:x[2]),])
  
  #The first dataframe in the list should have general information on the Patient. The rest of the dataframes will have DVH information on the structure. 
  #Hence we remove the first dataframe. This reduces the size of object in memory. 
  bl <- bl[-1]
  
  #Create a list of structure set names.
  str_list <- str_extract(a,"^Structure:(.*?)$")
  str_list <- str_list[!is.na(str_list)]
  str_list <- str_remove_all(str_list,"\\bStructure\\:")
  str_list <- trimws(str_list)
  str_list <- as.vector(str_list)
  
  #Append the structure names to the list of dataframes created above. 
  #Make a new coloumn named structure.
  bl <- mapply(cbind,bl,"structure"=str_list,SIMPLIFY=F)
  
  ## Collect mean, median, minimum and maximum dose of the structures with the same methodology. 
  ## We start with Mean dose (dmean)
  
  dmean <-str_extract(a,"^Mean(.*?)$")
  dmean <- dmean[!is.na(dmean)]
  dmean <- str_remove_all(dmean,"Mean(.*?)\\:")
  dmean <- as.numeric(as.character(trimws(dmean)))
  
  ##Then we extract the Median Dose (D50)
  d50 <- str_extract(a,"^Median(.*?)$")
  d50 <- d50[!is.na(d50)]
  d50 <- str_remove_all(d50,"Median(.*?)\\:")
  d50 <- as.numeric(as.character(trimws(d50)))
  
  ##Then we extract the Minimum Dose (dmin)
  
  dmin <- str_extract(a,"^Min(.*?)$")
  dmin <- dmin[!is.na(dmin)]
  dmin <- str_remove_all(dmin,"Min(.*?)\\:")
  dmin <- as.numeric(as.character(trimws(dmin)))
  
  ##Then we extract the Maximum Dose (dmax)
  
  dmax <- str_extract(a,"^Max(.*?)$")
  dmax <- dmax[!is.na(dmax)]
  dmax <- str_remove_all(dmax,"Max(.*?)\\:")
  dmax <- as.numeric(as.character(trimws(dmax)))
  
  ##We also extract the Volume (vol)
  
  vol <- str_extract(a,"^Volume(.*?)cm(.*?)$")
  vol <- vol[!is.na(vol)]
  vol <- str_remove_all(vol,"Volume(.*?)\\:")
  vol <- as.numeric(as.character(trimws(vol)))
  
  ##Join them in a data frame
  
  pointdoses <- data.frame(str_list,vol,dmean,d50,dmin,dmax)
  
  ##Add information on MR Number, prescribed dose and dose units in the same dataframe. 
  pointdoses$id <- id[1]
  pointdoses$prescribed_dose <- total_dose[1]
  pointdoses$dose_unit <- units[1]
  pointdoses$date <- date[1]
  pointdoses$plan_name <- plan_name[1]
  
  
  ##Check if the dose is relative dose or absolute dose
  
  dose_type <- str_extract(a,"\\bMin(.*?)$")
  dose_type <- dose_type[!is.na(dose_type)]
  dose_type <- str_extract(dose_type,"\\%|cGy|Gy")
  pointdoses <- cbind(pointdoses,dose_type)
  
  #Then we convert the doses to absolute dose to ensure consistency as well as to ensure that dose volume data is reported in units of dose. 
  
  pointdoses$dmean <- as.numeric(ifelse(pointdoses$dose_type=="%",(pointdoses$prescribed_dose*pointdoses$dmean/100),pointdoses$dmean))
  pointdoses$d50 <- as.numeric(ifelse(pointdoses$dose_type=="%",(pointdoses$prescribed_dose*pointdoses$d50/100),pointdoses$d50))
  pointdoses$dmin <- as.numeric(ifelse(pointdoses$dose_type=="%",(pointdoses$prescribed_dose*pointdoses$dmin/100),pointdoses$dmin))
  pointdoses$dmax <- as.numeric(ifelse(pointdoses$dose_type=="%",(pointdoses$prescribed_dose*pointdoses$dmax/100),pointdoses$dmax))
  
  #Then we remove the dose indicator coloumn
  
  pointdoses <- subset(pointdoses,select=-c(dose_type,dose_unit))
  
  #Then we save the file in a seperate location. We save these doses seperate from the dose volume histogram data. 
  
  id2 <- str_replace_all(id,pattern = "/","")
  
  name <- paste("pointdose/",paste(id2,plan_name,randn,sep="_"),".csv",sep = "")
  
  write.csv(x = pointdoses,file =name,row.names = F)
  
  
  #Now we strip unnecessary rows from the dataframe which do not have the dose volume data. 
  #We have already stored the information about patient id etc previously. 
  #We will however add the patient ID into the dataframe. 
  
  bl <- mapply(cbind,bl,"id"=id,SIMPLIFY=F)
  
  #We will also add information of the date and plan names into the dataframe
  
  bl <- mapply(cbind,bl,"date"=date,SIMPLIFY=F)
  bl <- mapply(cbind,bl,"plan"=plan_name,SIMPLIFY=F)
  
  ##Some dose volume histograms have  absolute dose while others have relative dose. We need to add information to the effect. 
  ##First we add information on dose type
  
  bl <- mapply(cbind,bl,"dose_type"=dose_type,SIMPLIFY=F)
  
  ##Check if the volumes are relative or absolute that is in cc
  
  vol_type <- str_extract(a,"\\bStructure Volume(.*?)\\]")
  vol_type <- vol_type[!is.na(vol_type)]
  vol_type <- str_extract(vol_type,"\\%|cm")
  
  ##Then we add information on Volume type  
  
  bl <- mapply(cbind,bl,"vol_type"=vol_type,SIMPLIFY=F)
  
  #Remove all rows in the dataframe that do not start with a number in the first position in th first row. 
  #All eclipse DVHs files have the feature that the rows in which dose information is avialble is starting with a number. 
  bl <- lapply(bl,function(x) x[!grepl("^[[:alpha:]]",x$X1),])
  
  
  
  dvhdata <- bind_rows(bl)
  
  
  ##In eclipse the order of dose coloumns changes based on what was selected. If relative dose was exported first row is percentage dose, while if the absolute dose was exported, then the first row is absolute dose.
  ##Similiarly, for volume depending on the type of export selected, the notation changes. 
  ##Below we change coloumn names based on rules depending on the type of export option selected. 
  ##Furthermore in case the DVH is of plan sum then only two coloumns will be exported. First row with the dose and 2nd with the volume. 
  
  plansum <- str_extract(a,"Comment(.*?)$")
  plansum <- plansum[!is.na(plansum)]
  plansum <- ifelse(grepl("one plan",plansum),"single","sumplan")
  
  names(dvhdata)[names(dvhdata)=="X1"] <- ifelse(plansum=="sumplan","absolute_dose",ifelse(dose_type[1]=="%","relative_dose","absolute_dose"))
  names(dvhdata)[names(dvhdata)=="X2"] <- ifelse(plansum=="sumplan", "volume", ifelse(dose_type[1]=="%","absolute_dose","relative_dose"))
  names(dvhdata)[names(dvhdata)=="X3"] <- ifelse(plansum=="sumplan", "X3","volume")
  
  ##Rename the volume coloumn created above to relative or absolute based on the previously extracted data.
  names(dvhdata)[names(dvhdata)=="volume"] <- ifelse(vol_type[1]=="%","relative_volume","absolute_volume")
  
  ##Remove all coloumns where every value is a NA 
  
  dvhdata <- dvhdata[,which(unlist(lapply(dvhdata,function(x) !all(is.na(x)))))]
  
  ##Finally save the DVH object into a dvhdata file
  
  name <- paste("dvhdata/",paste(id2,plan_name,date,randn,sep="_"),".parquet",sep = "")
  write_parquet(dvhdata,sink=name)
  rm(list=ls()) 
  gc()
}
