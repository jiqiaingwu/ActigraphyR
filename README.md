# Pacakge: ActigraphyUtah
## Instruction
This package mainly focus on dealing with accelerometry data. In general, when researcher download the raw data from actiware, the data contains plenty of information with multiple sections. Hence, this package will help us clean the raw data and select useful information to output.

## What we need?
### 1. Actiware Raw data 
It is a csv file downloaded from Actiware software. 

### 2. Initalize sheet data
In initialize sheet data, it will point out the range of reasonable period to import. For example, the participant may wear the actiware for one month period, but only consecutive 7 days is valid. Therefore, in intialize sheet, we need to figure out the number of valid days.

## Example
library(tidyverse)

library(ActigraphyUtah)

table_raw<- readxl::read_excel("743801_v1_template.xlsx",sheet=2 ,col_names = FALSE)

initalize<- readxl::read_excel("743801_v1_template.xlsx",sheet=1,col_names = FALSE)

output <-extractdata (table_raw,initalize)



