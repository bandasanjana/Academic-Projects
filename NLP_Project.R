library(shiny)
library(shinydashboard)


ui <- 
  dashboardPage(
    skin = "yellow",
    dashboardHeader(title = "Sanjana Banda_Natural Language Processing", titleWidth = 400,
                    dropdownMenu(
                      type = "notifications", 
                      icon = icon("question-circle"),
                      badgeStatus = NULL,
                      headerText = "See also:",
                      
                      notificationItem("Natural Language Processing", icon = icon("file"),
                                       href = 'https://www.tidytextmining.com/'),
                      notificationItem("shinydashboard", icon = icon("file"),
                                       href = "https://rstudio.github.io/shinydashboard/")
                    )
                    
    ),
    dashboardSidebar(
      sidebarMenu(id = "sidebarmenu",
                  
               
                  menuItem("Train Data", tabName = "train_data",
                           menuSubItem('Train Table', tabName = 'train_table'),
                           menuSubItem('Normalized Review', tabName = 'train_normalized_review'),
                           menuSubItem('Tags', tabName = 'train_tags')),
                  menuItem("Test Data", tabName = "test_data",
                           menuSubItem('Test Table', tabName = 'test_table'),
                           menuSubItem('Normalized Review', tabName = 'test_normalized_review'),
                           menuSubItem('Tags', tabName = 'test_tags')),
                  menuItem("Sentiment Analysis", tabName = "sentiment_analysis",
                           menuSubItem('Average Sentiment Rating', tabName = 'train_average_sentiment_rating'),
                           menuSubItem('Average Star Rating', tabName = 'train_average_star_rating'),
                           menuSubItem('Summary', tabName = 'train_sentiment_analysis_summary')),
                  menuItem("Tags", tabName = "tags",
                           menuSubItem('Service', tabName = 'train_service'),
                           menuSubItem('Price', tabName = 'train_price'),
                           menuSubItem('Handling', tabName = 'handling'),
                           menuSubItem('Interior', tabName = 'Interior')),
                  menuItem("Model", tabName = "train_model" , 
                           menuSubItem('N/A', tabName = 'n/a'),
                           menuSubItem('N/A', tabName = 'n/a')),
                  menuItem("TF-IDF", tabName = "train_tfidf", 
                           menuSubItem('Service Tag', tabName = 'train_tfidf_service'),
                           menuSubItem('Price Tag', tabName = 'train_tfidf_price'),
                           menuSubItem('Handling Tag', tabName = 'train_tfidf_handling'),
                           menuSubItem('Interior Tag', tabName = 'train_tfidf_Interior')),
                  
                  menuItem("Summary", tabName = "Summary")
      )
    ),
    
    
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "train_table",
                fluidRow(
                  tableOutput("TrainTable")
                )
        ),
        tabItem(tabName = "train_normalized_review",
                fluidRow(
                  tableOutput("TrainNormalizedTable")
                )
        ),
        tabItem(tabName = "train_tags",
                fluidRow(
                  tableOutput("TrainTagTable")
                )
        )
    
        )
      )
    )
  )




server <- function(input, output, session) {


#Output for Emails Table   
output$TrainTable <- renderTable({

  TrainData()
})







#######Functions#############
getTrainData <- function(){
library(rvest)
library(purrr)
library(tidyverse)
library(tokenizers)
library(rlist)

link <-"https://www.cars.com/research/toyota-camry-%d/consumer-reviews/?pg=%dnr=100"
yearList <- list(2012, 2013, 2014,2015, 2016)

map_df(yearList, function(j) {
  review <- data.frame()
  i = 0
  while (i >= 0) {
    cat(".")
    i = i + 1
    pg <- read_html(sprintf(link, j, i))
    
    data <- data.frame(Review=html_text(html_nodes(pg, ".mmy-reviews__blurb")),
                       Rating=as.numeric(html_attr(html_nodes(pg, ".cr-star-rating"), name = "rating")),
                       stringsAsFactors=FALSE)
    
    review <- rbind(data, review)
    if(nrow(data) == 0){
      i = -1
    }
  }
  
  count1 <- nrow(review)
  Year <- rep(j,count1)
  review <-cbind(Year,review)
}) -> train
    return(train)
}








output$TrainNormalizedTable <- renderTable({
  
  library(tm)
  
  review <- getTrainData()[,2]
  review_cleaned <- list()
  
  for( i in 1:length(review)){
    
    
    data<-removePunctuation(review[i])
    reviews.clean <- gsub("\n|[\t]", "", data)
    review.lower<- tolower(reviews.clean)
    train[i,4] <- review.lower
    
  }
  
})



output$TrainTagTable <- renderTable({
  x <- list("service","price","handling","interior")
  train_review <- getTrainData()[,2]
  
  m <- list()
  for(k in 1:length(train_review)){
    
    words <- tokenize_words(train_review[k])
    words_l <- list()
    words_l <- words[[1]]
    
    string1 <- ""
    string2 <- ""
    for(i in 1:length(words_l)){
      require(stringdist)
      h <- words_l[i]
      index <- amatch(h,x)
      if ((is.na(index) || index == '') == FALSE && length(grep(x[index], string1)) == 0){
        string1 <- paste(string1, x[index], sep = ",")
        string2 <- substring(string1,2)
        
      }
    }
    train[k,5] <- string2
  }
  colnames(train)[4] <-"Cleaned Review"
  colnames(train)[5] <-"Tags"
  
  
  
  
})


}



shinyApp(ui = ui, server = server)

