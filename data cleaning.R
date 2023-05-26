if (!require("readxl")){
  install.packages("readxl")
}
if (!require("igraph")){
  install.packages("igraph")
}
library('readxl')
library('dplyr')
library('tidyverse')
library('tidyr')
library('igraph')
library('Matrix')

### read raw data
survey_data <- read_excel('Assessment Data v1.xlsx')
survey_df <- as.data.frame(survey_data)
colnames(survey_df)

clean_columns = c('Respondent', 'SETID', 'Physician_Type', 'Country', 'Slot_1', 'Slot_2', 'Slot_3',
                  'Slot_4', 'Slot_5', 'Slot_6', 'Slot_7', 'Slot_8', 'Most_Appealing_Message',
                  'Least_Appealing_Message')
survey_df = survey_df %>% select(all_of(clean_columns))

### remove invalid rows
### to check whether most_appealing_message and least_appealing_message are in slots; most_appealing_message != least_appealing_message
slot_columns = c('Slot_1', 'Slot_2', 'Slot_3', 'Slot_4', 'Slot_5', 'Slot_6', 'Slot_7', 'Slot_8')
invalid_rows = c()
for(i in 1:nrow(survey_df)){
  if(!(survey_df[i, 'Most_Appealing_Message']%in%survey_df[i, slot_columns]) | 
     !(survey_df[i, 'Least_Appealing_Message']%in%survey_df[i, slot_columns]) |
     survey_df[i, 'Most_Appealing_Message']==survey_df[i, 'Least_Appealing_Message']){
    invalid_rows = c(invalid_rows, i)
  }
}

clean_df = survey_df[-invalid_rows, ]
nrow(clean_df)
write.csv(clean_df, 'clean_df.csv', row.names=FALSE)

### check consistency
### the only info is 'most' and 'least'
### if the physicians are totally reasonable people, then the info could be intuitively explained as partial order relationship
### so we need to check the validity of this partial-order relationship
### if inconsistency exists, it could be due to 'unreasonable' thoughts of physicians
### but could be also due to the incomplete info collected
### since only the message labels are shown
# adj_mat = matrix(0, 16, 16)
# 
# consist = clean_df[clean_df$Respondent==23, ]
# 
# for(i in 1:nrow(consist)){
#   for(slot in slot_columns){
#     tail = consist[i, 'Most_Appealing_Message']
#     arrow = consist[i, slot]
#     if(tail!=arrow){
#       adj_mat[tail, arrow] = adj_mat[tail, arrow] + 1 
#     }
#     
#     tail = consist[i, slot]
#     arrow = consist[i, 'Least_Appealing_Message']
#     if(tail!=arrow){
#       adj_mat[tail, arrow] = adj_mat[tail, arrow] + 1 
#     }
#   }
# }
# 
# g = graph_from_adjacency_matrix(adj_mat, mode='directed', weighted=TRUE)  
# plot.igraph(g)
# 
# igraph_is_eulerian(g)
# is_dag(g)