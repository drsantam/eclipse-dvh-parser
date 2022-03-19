# eclipse-dvh-parser
This is a hobby R code for parsing Eclipse Dose Volume Histogram Files into a tabular format. The following are needed
1. DVH files are exported from Eclipse and saved with a .txt extension (To use the code with other extensions please change the extension in line 17
2. Three folders need to created : 
- DVH : To contain all DVH files that need to parsed
- dvhdata : Will contain the extracted dvh data
- pointdose : Will contain the information on the extracted point dose
3. Please change the working directory to your desired directory
Required libraries:
1. dplyr
2. stringr
3. readr
4. lubridate
5. arrow

All files in the DVH folder will be sequentially read and the two sets of files will be saved for each patient :
1. Point dose file : Containing information on the patient name, unique ID, plan name, structure name, mean , median minimum, maximum dose and volume for each structure. 
2. DHV file : This will contain the tabular DVH data 

As large DVH files take up lot of memory, in order to make the code efficient the files are saved in a storage efficient Parquet format (https://arrow.apache.org/docs/r/reference/read_parquet.html)

Each file is name with the patient ID, plan name and a random number to allow scenarios where same patient ID and same plan name are there. Point dose files are usually small and stored in .csv format. 

In addition, to the extracted dose, the tabular data will also capture information on weather absolute or relative dose and absolute or relative volumes were exported. 

Plan sum DVH exports are supported. 
