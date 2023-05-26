if (!require("gRbase")){
  install.packages("gRbase")
}
library('ggplot2')
library('dplyr')
library('tidyverse')
library('tidyr')
library('gRbase')

### read data
clean_df <- read.csv('clean_df.csv')

### visualization
map = setNames(c("USA", "Canada", "Mexico"), c(1, 2, 3))
map_physician = setNames(c("Physician 1", "Physician 2"), c(1, 2))
map_message = setNames(c('Efficacy in presence of NNRTI resistance mutations',
                         'Resistance rate comparison',
                         'Unique resistance profile',
                         'Efficacy in high baseline VL',
                         'Efficacy comparison (vs darunavir)',
                         'Efficacy comparison (vs efavirenz)',
                         'Lipid-lowering benefits',
                         'Managing patients with HIV and hyperlipidemia',
                         'Lipid effects comparison',
                         'Discontinuation rate comparison',
                         'CNS tolerability comparison',
                         'GI tolerability comparison',
                         'Use with common concomitant medications',
                         'Concomitant acid-reducing agents use',
                         'Flexible dosing',
                         'Flexible prescribing') , 1:16)

clean_df$Country = map[unlist(clean_df$Country)]
clean_df$Physician_Type = map_physician[unlist(clean_df$Physician_Type)]
clean_df$Most_Appealing_Message = map_message[unlist(clean_df$Most_Appealing_Message)]
clean_df$Least_Appealing_Message = map_message[unlist(clean_df$Least_Appealing_Message)]

indices = expand.grid(c("Physician 1", "Physician 2"), c('USA', 'Canada', 'Mexico'), c('Efficacy in presence of NNRTI resistance mutations',
                                                                                       'Resistance rate comparison',
                                                                                       'Unique resistance profile',
                                                                                       'Efficacy in high baseline VL',
                                                                                       'Efficacy comparison (vs darunavir)',
                                                                                       'Efficacy comparison (vs efavirenz)',
                                                                                       'Lipid-lowering benefits',
                                                                                       'Managing patients with HIV and hyperlipidemia',
                                                                                       'Lipid effects comparison',
                                                                                       'Discontinuation rate comparison',
                                                                                       'CNS tolerability comparison',
                                                                                       'GI tolerability comparison',
                                                                                       'Use with common concomitant medications',
                                                                                       'Concomitant acid-reducing agents use',
                                                                                       'Flexible dosing',
                                                                                       'Flexible prescribing'))
colnames(indices) <- c('Physician_Type', 'Country', 'Most_Appealing_Message')

most_appealing = clean_df %>% 
  select(c('Physician_Type', 'Country', 'Most_Appealing_Message')) %>% 
  group_by(Physician_Type, Country, Most_Appealing_Message) %>% 
  summarise(total_count=n()) %>%
  as.data.frame() %>%
  merge(y=indices, by=c('Physician_Type', 'Country', 'Most_Appealing_Message'), all.y=TRUE)
most_appealing[is.na(most_appealing)] = 0
most_appealing = mutate(most_appealing, Freq=total_count/sum(total_count))
colnames(most_appealing) <- c('Physician_Type', 'Country', 'Message_Type', 'total_count', 'Freq')


colnames(indices) <- c('Physician_Type', 'Country', 'Least_Appealing_Message')
least_appealing = clean_df %>%
  select(c('Physician_Type', 'Country', 'Least_Appealing_Message')) %>% 
  group_by(Physician_Type, Country, Least_Appealing_Message) %>% 
  summarise(total_count=n()) %>%
  as.data.frame() %>%
  merge(y=indices, by=c('Physician_Type', 'Country', 'Least_Appealing_Message'), all.y=TRUE)
least_appealing[is.na(least_appealing)] = 0
least_appealing = mutate(least_appealing, Freq=total_count/sum(total_count))
colnames(least_appealing) <- c('Physician_Type', 'Country', 'Message_Type', 'total_count', 'Freq')

appealing_score = data.frame(most_appealing) %>% select(-Freq)
appealing_score$total_score = most_appealing$Freq - least_appealing$Freq

appealing_score$most_appealing_score = most_appealing$Freq
appealing_score$least_appealing_score = least_appealing$Freq

physician_score = appealing_score %>% 
  group_by(Physician_Type, Message_Type) %>% 
  summarise(total_score=sum(total_score),
            most_appealing_score=sum(most_appealing_score),
            least_appealing_score=sum(least_appealing_score)) %>%
  mutate(most_appealing_score=most_appealing_score/sum(most_appealing_score),
         least_appealing_score=least_appealing_score/sum(least_appealing_score)) %>%
  mutate(total_score=most_appealing_score-least_appealing_score) %>%
  as.data.frame()

country_score = appealing_score %>% 
  group_by(Country, Message_Type) %>% 
  summarise(total_score=sum(total_score),
            most_appealing_score=sum(most_appealing_score),
            least_appealing_score=sum(least_appealing_score)) %>%
  mutate(most_appealing_score=most_appealing_score/sum(most_appealing_score),
         least_appealing_score=least_appealing_score/sum(least_appealing_score)) %>%
  mutate(total_score=most_appealing_score-least_appealing_score) %>%
  as.data.frame()

phy_cou_score = appealing_score %>% select(-c(Physician_Type, Country))
phy_cou_score$phy_cou_type = paste(appealing_score$Physician_Type, appealing_score$Country, sep=' & ') 
phy_cou_score = phy_cou_score %>%
  group_by(phy_cou_type, Message_Type) %>%
  summarise(total_score=sum(total_score),
            most_appealing_score=sum(most_appealing_score),
            least_appealing_score=sum(least_appealing_score)) %>%
  mutate(most_appealing_score=most_appealing_score/sum(most_appealing_score),
         least_appealing_score=least_appealing_score/sum(least_appealing_score)) %>%
  mutate(total_score=most_appealing_score-least_appealing_score) %>%
  as.data.frame()

all_score = appealing_score %>% 
  group_by(Message_Type) %>% 
  summarise(total_score=sum(total_score),
            most_appealing_score=sum(most_appealing_score),
            least_appealing_score=sum(least_appealing_score)) %>%
  mutate(most_appealing_score=most_appealing_score/sum(most_appealing_score),
         least_appealing_score=least_appealing_score/sum(least_appealing_score)) %>%
  mutate(total_score=most_appealing_score-least_appealing_score) %>%
  as.data.frame()

write.csv(physician_score, 'physician_score.csv', row.names=FALSE)
write.csv(country_score, 'country_score.csv', row.names=FALSE)
write.csv(phy_cou_score, 'phy_cou_score.csv', row.names=FALSE)
write.csv(all_score, 'all_score.csv', row.names=FALSE)
