# Pacakge: ActigraphyUtah
## Instruction
This package mainly focus on dealing with accelerometry data, which could help researchers clean the raw data and select useful information to output.

## What we need?
### 1. Actiware Raw data 
It is a csv file downloaded from Actiware software. 

### 2. Initalize sheet data
In initialize sheet data, it points out the range of reasonable period to import. For example, the participant may wear the actiware equipment for one month, but only consecutive 7 days are valid. Therefore, in intialize sheet, it helps researchers figure out the number of valid days.

## Example

### Data
[Example Data](https://drive.google.com/file/d/15P8eoJ-b8ZGktifDT8pplzs7KwcCOlcr/view?usp=sharing)

### Code
library(tidyverse)

library(ActigraphyUtah)

table_raw<- readxl::read_excel("example.xlsx",sheet=2,col_names = FALSE)

initalize<- readxl::read_excel("example.xlsx",sheet=1,col_names = FALSE)

output <-extractdata (table_raw,initalize)



