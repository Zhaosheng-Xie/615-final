library(tidyverse)
library(dplyr)
library(magrittr)
library(readr)
library(scales)
library(base)
library(shiny)
library(tm)
library(tmap)
library(tmaptools)
library(wordcloud2)
library(raster)
library(PROJ)
library(NLP)
library(sp)
library(sf)


## Importing Date
# Here is the api link: https://developer.adzuna.com/activedocs#!/adzuna/search  
# When you have appid and app_key, you can set some parameters and click "try it out". Data is in Response body.

adzuna_sample = read.csv("adzuna_sample.csv") # This data is a sample containing 50 observations
load("DS4.RData") # This data is tidy data containing 483 observations
word_freq.tit = read.csv("word_freq.tit.csv") # This data is word frequency of title
word_freq.des = read.csv("word_freq.des.csv") # This data is word frequency of description
# load("category_plot.RData")  # This data is for tmap




ui <- fluidPage(
    title = "Samples of Data Tables",
    sidebarLayout(
        tabsetPanel(
             conditionalPanel('input.dataset === "adzuna_sample"'),
             conditionalPanel('input.dataset === "DS4"')
        ),
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel("Data scientist job sample",
                
                # Create a new Row in the UI for selecting Inputs
                fluidRow(
                    column(4,
                           selectInput("category_label",
                                       "category_label:",
                                       c("All",
                                         unique(as.character(adzuna_sample$category_label))))
                    ),
                    column(4,
                           selectInput("title",
                                       "title:",
                                       c("All",
                                         unique(adzuna_sample$title)))
                    ),
                    column(4,
                           selectInput("company_name",
                                       "company_name:",
                                       c("All",
                                         unique(adzuna_sample$company_name)))
                    ),
                    column(4,
                           selectInput("location",
                                       "location:",
                                       c("All",
                                         unique(adzuna_sample$location)))
                )
                ),
                # Create a new row for the table.
                DT::dataTableOutput("table1")),
                
                tabPanel("Tidy data related to the work of data scientists",
                         
                         # Create a new Row in the UI for selectInputs
                         fluidRow(
                             column(5,
                                    selectInput("category_label",
                                                "category_label:",
                                                c("All",
                                                  unique(as.character(DS4$category_label))))
                             ),
                             column(5,
                                    selectInput("title",
                                                "title:",
                                                c("All",
                                                  unique(DS4$title)))
                             ),
                             column(5,
                                    selectInput("company_name",
                                                "company_name:",
                                                c("All",
                                                  unique(DS4$company_name)))
                             ),
                             column(5,
                                    selectInput("city",
                                                "city:",
                                                c("All",
                                                  unique(DS4$city)))
                             ),
                             column(5,
                                    selectInput("date",
                                                "date:",
                                                c("All",
                                                  unique(DS4$date)))
                             )
                         ),
                         # Create a new row for the table.
                         DT::dataTableOutput("table2"))
               
                        ,
                tabPanel("Wordcloud of title",
                         wordcloud2Output("plot1")),
                tabPanel("Wordcloud of description",
                         wordcloud2Output("plot2")),
                tabPanel("Category label map",
                         tmapOutput("plot3"))
                      )
                )
                )
)
    

server <- function(input, output) {
    
    # Filter data based on selections
    output$table1 <- DT::renderDataTable(DT::datatable({
        data1 <- adzuna_sample
        if (input$category_label != "All") {
            data1 <- data1[data1$category_label == input$category_label,]
        }
        if (input$title != "All") {
            data1 <- data1[data1$title == input$title,]
        }
        if (input$company_name != "All") {
            data1 <- data1[data1$company_name == input$company_name,]
        }
        if (input$location != "All") {
            data1 <- data1[data1$location == input$location,]
        }
        data1
    }))
    
    output$table2 <- DT::renderDataTable(DT::datatable({
        data2 <- DS4
        if (input$category_label != "All") {
            data2 <- data2[data2$category_label == input$category_label,]
        }
        if (input$title != "All") {
            data2 <- data2[data2$title == input$title,]
        }
        if (input$company_name != "All") {
            data2 <- data2[data2$company_name == input$company_name,]
        }
        if (input$city != "All") {
            data2 <- data2[data2$city == input$city,]
        }
        if (input$date != "All") {
            data2 <- data2[data2$date == input$date,]
        }
        data2
    }))    
   
    
    
    # Wordcloud for title
    output$plot1 <- renderWordcloud2(expr = wordcloud2(data=word_freq.tit,shape = "diamond",size = 1))
   
    # Wordcloud for dewcription
    output$plot2 = renderWordcloud2(wordcloud2(data=word_freq.des,shape = "diamond"))
    
    
    # Category label map
    output$plot3 = renderTmap({
        tmap_mode('view')
        epsg_wgs84 <- 4326 # GPS CRS (WGS 84)
        category <- 
            DS4 %>% 
            st_as_sf(coords = c("longitude", "latitude")) %>%
            st_set_crs(epsg_wgs84)
        category_plot<- category %>% dplyr::select(id, title, company_name, category_label,date, geometry)
        
        category_map <-
        tm_shape(category_plot) +
            tm_dots(col = 'category_label', size = .02, alpha=.5,title='Category label') 
        category_map
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
