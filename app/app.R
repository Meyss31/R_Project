#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
top_killers = killers %>% group_by(killedBy) %>% summarise(nb =n()) %>% arrange(desc(nb)) %>% head(10)
library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "GOT Visualisation",
    tabPanel(
        "Killers",
        sidebarLayout(
            sidebarPanel(
                radioButtons(
                    inputId = "killer",
                    label = "Select killer",
                    choices = top_killers$killedBy
                )
            ),
            mainPanel(
                dataTableOutput(
                    outputId = 'killed'
                )
            )
        )
    ),
    tabPanel(
        "Statistics",
        sidebarLayout(
            sidebarPanel(
                radioButtons(
                    inputId = "season",
                    label = "Select season",
                    choices = c("1","2","3","4","5","6","7","8")
                )
            ),
            mainPanel(
                plotOutput(
                    outputId = 'seasons'
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$killed <- renderDataTable(
        {
            killers  %>% filter(killedBy == input$killer) %>% select(name)
        }
    )
    output$seasons <- renderPlot(
        {
            dat = episodes %>% select(episodeId, seasonNum, episodeNum) %>% 
                left_join(scenes %>% select(episodeId, nbdeath)) %>%
                filter(seasonNum == input$season)  %>%
                group_by(episodeNum) %>% summarise(ttDeath= sum(nbdeath))
            ggplot(
                data = dat,
                aes(
                    x = episodeNum
                )
            ) + geom_bar()
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
