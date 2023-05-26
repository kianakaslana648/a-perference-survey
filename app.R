if (!require("shiny")){
  install.packages("shiny")
}
if (!require("gRbase")){
  install.packages("gRbase")
}

library('shiny')
library('ggplot2')
library('readxl')
library('dplyr')
library('tidyverse')
library('tidyr')
library('igraph')
library('Matrix')
library('gRbase')

### read csv
physician_score = read.csv('physician_score.csv')
country_score = read.csv('country_score.csv')
phy_cou_score = read.csv('phy_cou_score.csv')
all_score = read.csv('all_score.csv')

### shiny app
# Define UI for application that draws two scatter plots

divided_cols = c('Physician Type', 'Country', 'Physician Type & Country', 'All')
score_cols = c('most_appealing_score', 'least_appealing_score', 'total_score')
bmm_cols = c('Physician 1', 'Physician 2', 'USA', 'Canada', 'Mexico', 'All')
map_groups = setNames(c("bmm_physician_1.png", "bmm_physician_2.png", "bmm_USA.png",
                        'bmm_Canada.png', 'bmm_Mexico.png', 'bmm_All.png'), 
                      c('Physician 1', 'Physician 2', 'USA', 'Canada', 'Mexico', 'All'))

ui <- fluidPage(
  
  titlePanel("A Preference Survey"),
  
  p("Minglei Cai"),
  p('05/26/2023'),
  tags$hr(),
  
  
  h3('Data Description'),
  tags$ul(
    tags$li('The data is composed of survey respondents who are physicians from different countries. Each 
            respondent was given 12 sets of messages to assess. Each set consisted of 8 messages out of 
            possible 16 messages. Therefore, only 8 messages at a time can be assessed against each 
            other. The respondent is then asked to select the message that is most appealing to them and 
            the message that is least appealing to them from each set.
            ')
  ),
  tags$hr(),
  
  h3('Data Preprocessing'),
  tags$ul(
    tags$li('Remove the invalid rows with most appealing or least appealing message not in the set slots.'),
    tags$li('Check for each row whether most appealing message label equal to least appealing message label.')
    
  ),
  tags$hr(),
  
  h3('EDA'),
  tags$ul(
    tags$li('Compute proportions of most and least appealing messages across each classification group as the corresponding score'),
    tags$li('Use the difference of most_appealing_score and least_appealing_score as the total_score'),
  ),
  verticalLayout(
    
    # Sidebar with a select inputs to choose variables to plot
    sidebarLayout(
      sidebarPanel(
        selectInput("Divided_Type", "Classification Factor", divided_cols, selected = "Physician Type"),
        selectInput("Score_Type", "Score Type", score_cols, selected = "most_appealing_score")
      ),
      
      # Show a plot with configurable axes
      mainPanel(
        plotOutput("barPlot")
      )
    ),
    tags$hr()
  ),
  tags$ul(
    tags$li('Intuitively there are differences across different groups and preferences for certain messages.')
  ),
  tags$hr(),
  
  h3('Simple Graph Model'),
  tags$ul(
    tags$li('The only information given by user are the most appealing and least appealing messages, which could be
            considered as giving a definition of relationship for most appealing messages over others, and similarly for least appealing messages.'),
    tags$li('If the physicians are totally reasonable people, then we could achieve a partial-order relationship on the 16 message labels.'),
    tags$li('But after checking for the responses for some physicians, we could find partially-ordered set assumption doesn\'t exist in this setting.'),
    tags$li('It could be due to inconsistent responses from the physicians, and could also be due to the incomplete information of messages.'),
  ),
  tags$hr(),
  tags$ul(
    tags$li('A simple statistical directed graph model to describe the behavior, with messages being the points and weighted edges.'),
    tags$li('For a person at each edge, he/she would have a probability to decide a preference for either of the two items.'),
    tags$li('Once it\'s not equal to 1/2 with statistical significance, we tend to think there are preferences for a item.'),
    tags$li('We could also use it to confirm the preferences of a group or a population.'),
  ),
  tags$hr(),
  
  h3('Bayes Mallows Model'),
  tags$ul(
    tags$li('By fitting BMM models with different populations, we could see the top rankings for message types
            and thus different preferences in different populations'),
    tags$li('By applying Bernoulli error models, we could fix the inconsistency in raw data'),
  ),
  verticalLayout(
    
    # Sidebar with a select inputs to choose variables to plot
    sidebarLayout(
      sidebarPanel(
        selectInput("group", 'Respondents', bmm_cols, selected = "All"),
      ),
      
      # Show a plot with configurable axes
      mainPanel(
        imageOutput("bmmPlot"),
      ),
    ),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$hr()),
  
  h3('Conclusions'),
  tags$ul(
    tags$li("Most physicians in this survey prefer messages about 'Efficacy in high baseline VL' and 
            'Efficacy comparison (vs darunavir)'."),
    tags$li('According to result plot of BMM, there are preference differences between these two kinds of physicians,
            and there are clear preference differences between USA and Canada/Mexico'),
    tags$li('For simplication, by doing binomial test on the proportions of most appealing and least appealing messages,
            we could also see the statistically significant difference.'),),
  tags$hr(),
  
  h3('Anything Else a Analyst Should Provide'),
  tags$ul(
    tags$li("Make concrete strategies for business benefits in the real setting."),
    tags$li('Give suggestions on improvement of questionnaire design.'),
    tags$li('Up-to-date relevant theories and technologies.'),),
  tags$br(),)




# Define server logic required to draw both plots
server <- function(input, output) {
  physician_data = reactive({
    physician_score
  })
  country_data = reactive({
    country_score
  })
  phy_cou_data = reactive({
    phy_cou_score
  })
  all_data = reactive({
    all_score
  })
  
  output$barPlot <- renderPlot({
    if(input$Divided_Type=='Physician Type'){
      final_plot <- ggplot(data=physician_data(), mapping = aes(x = .data[['Message_Type']],
                                                                y = .data[[input$Score_Type]],
                                                                fill = .data[['Physician_Type']])) +
        geom_col(position = "dodge2", width = 0.7) +
        coord_flip() +
        theme_light()
    }else if(input$Divided_Type=='Country'){
      final_plot <- ggplot(data=country_data(), mapping = aes(x = .data[['Message_Type']],
                                                              y = .data[[input$Score_Type]],
                                                              fill = .data[['Country']])) +
        geom_col(position = "dodge2", width = 0.7) +
        coord_flip() +
        theme_light()
    }else if(input$Divided_Type=='Physician Type & Country'){
      final_plot <- ggplot(data=phy_cou_data(), mapping = aes(x = .data[['Message_Type']],
                                                              y = .data[[input$Score_Type]],
                                                              fill = .data[['phy_cou_type']])) +
        geom_col(position = "dodge2", width = 0.7) +
        coord_flip() +
        theme_light()
    }else if(input$Divided_Type=='All'){
      final_plot <- ggplot(data=all_data(), mapping = aes(x = .data[['Message_Type']],
                                                          y = .data[[input$Score_Type]])) +
        geom_col(position = "dodge2", width = 0.7) +
        coord_flip() +
        theme_light()
    }
    final_plot
  })
  
  img_filename = reactive({
    map_groups[input$group]
  })
  
  output$bmmPlot <- renderImage({
    filename <- normalizePath(file.path('./pics',
                                        img_filename()))
    
    # Return a list containing the filename
    list(src = filename, width=500, alt = "Alternate text")
  }, deleteFile = FALSE)
  
}

# Run the application
shinyApp(ui = ui, server = server)
