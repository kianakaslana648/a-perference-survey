if (!require("BayesMallows")){
  install.packages("BayesMallows")
}
library('ggplot2')
library('dplyr')
library('tidyverse')
library('tidyr')
library('BayesMallows')

### Pairwise Preferences

clean_df = read.csv('clean_df.csv')

message_preferences = clean_df %>% select('Respondent', 'Physician_Type', 'Country', 'Most_Appealing_Message', 'Least_Appealing_Message')
colnames(message_preferences) = c('assessor', 'Physician_Type', 'Country', 'top_item', 'bottom_item')
physician_1_preferences = message_preferences %>% 
  filter(Physician_Type==1) %>%
  select('assessor', 'bottom_item', 'top_item')
physician_2_preferences = message_preferences %>% 
  filter(Physician_Type==2) %>%
  select('assessor', 'bottom_item', 'top_item')
USA_preferences = message_preferences %>% 
  filter(Country==1) %>%
  select('assessor', 'bottom_item', 'top_item')
Canada_preferences = message_preferences %>% 
  filter(Country==2) %>%
  select('assessor', 'bottom_item', 'top_item')
Mexico_preferences = message_preferences %>% 
  filter(Country==3) %>%
  select('assessor', 'bottom_item', 'top_item')
All_preferences = message_preferences %>%
  select('assessor', 'bottom_item', 'top_item')

### BMM

#### physician_1
bmm_physician_1 <- compute_mallows(preferences = physician_1_preferences, nmc = 102000, save_aug = TRUE, error_model='bernoulli')
bmm_physician_1$burnin <- 2000
plot_bmm_physician_1 <- plot_top_k(bmm_physician_1)
plot_bmm_physician_1

#### physician_2
bmm_physician_2 <- compute_mallows(preferences = physician_2_preferences, nmc = 102000, save_aug = TRUE, error_model='bernoulli')
bmm_physician_2$burnin <- 2000
plot_bmm_physician_2 <- plot_top_k(bmm_physician_2)
plot_bmm_physician_2

#### USA
bmm_USA <- compute_mallows(preferences = USA_preferences, nmc = 102000, save_aug = TRUE, error_model='bernoulli')
bmm_USA$burnin <- 2000
plot_bmm_USA <- plot_top_k(bmm_USA)
plot_bmm_USA

#### Canada
bmm_Canada <- compute_mallows(preferences = Canada_preferences, nmc = 102000, save_aug = TRUE, error_model='bernoulli')
bmm_Canada$burnin <- 2000
plot_bmm_Canada <- plot_top_k(bmm_Canada)
plot_bmm_Canada

#### Mexico
bmm_Mexico <- compute_mallows(preferences = Mexico_preferences, nmc = 102000, save_aug = TRUE, error_model='bernoulli')
bmm_Mexico$burnin <- 2000
plot_bmm_Mexico <- plot_top_k(bmm_Mexico)
plot_bmm_Mexico

#### All
bmm_All <- compute_mallows(preferences = All_preferences, nmc = 102000, save_aug = TRUE, error_model='bernoulli')
bmm_All$burnin <- 2000
plot_bmm_All <- plot_top_k(bmm_All)
plot_bmm_All