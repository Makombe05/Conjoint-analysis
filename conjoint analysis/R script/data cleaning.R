library(readxl)
library(writexl)
library(dplyr)
library(googlesheets4)
###df <- read_excel("~/Downloads/Student Job Preference Survey (Risposte).xlsx")

###import from workspace
# Define the URL of your spreadsheet
sheet_url <- "https://docs.google.com/spreadsheets/d/1D39wUaVMohKUC4iqwyvNANYhcMe9A0-75B3Riab4KWM/edit#gid=1652907087"

# Read the sheet into a data frame
# You can specify the sheet name "Risposte del modulo 1" to ensure you get the right data
df <- read_sheet(sheet_url, sheet = "Risposte del modulo 1")

# View the first few rows of your data
head(df)

View(df)
df<-df%>% select (-all_of(c(1,19)))
head(df)
write_xlsx(as.data.frame(df),
           "/Users/mac/Desktop/conjoint project/dataset/data.xlsx")

#### import the dataset again
data <- read_excel("/Users/mac/Desktop/conjoint project/dataset/data.xlsx", 
                   skip = 1,           # Skips the first row (A1), starts at A2
                   col_names = FALSE)  # Keeps your preference for no column names

colnames(data) <- c ("country","age","situation","course","salary","location","bonus"
                     ,"comp size","job 1","job 2","job 3", "job 4","job 5", "job 6",
                     "job 7","job 8","job 9")
View(data)


####change the front
#data <- data %>% slice(-3)
#data <- data %>% 
# @ mutate(`job 3` = as.integer(`job 3` ))
#View(data)

### extract for other use 
write_xlsx(as.data.frame(data),
           "/Users/mac/Desktop/conjoint project/dataset/dataset.xlsx")
#####select column of jobs rank
data <- data %>% select(9:17)
View(data)
data

###Check for ranking
data <- data %>%
  rowwise() %>%
  mutate(is_valid = all(sort(c_across(`job 1`:`job 9`)) == 1:9)) %>%
  ungroup()
View(data)
# To see only the rows that have errors (repeats or missing numbers):
errors <- data %>% filter(is_valid == FALSE)
View(errors) 

# Keep only rows where the 1-9 ranking is perfect
data_c <- data %>%
  rowwise() %>%
  filter(all(sort(c_across(`job 1`:`job 9`)) == 1:9)) %>%
  ungroup()
View(data_c)

###creat the table with only rankings column
data_c<-data_c%>% select (-all_of(10))
data_c
#####transposing(change) row to column 
# Flip the data
data_flipped <- as.data.frame(t(data_c))

# Fix the column names if necessary
colnames(data_flipped) <- paste0("Person_", 1:ncol(data_flipped))

View(data_flipped)

### extract the text file 
write.table(data_flipped, 
            file = "/Users/mac/Desktop/conjoint project/dataset/jobs.txt", 
            sep = "\t",          # This uses 'Tab' as a separator
            row.names = FALSE,   # This prevents R from adding row numbers 1, 2, 3...
            quote = FALSE)       # This prevents quotes around your text
