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
        "Static visualisation",
        sidebarLayout(
            sidebarPanel(
                h3(
                    "Total number of death on the serie, distributed by season"
                ),
                p(
                    "this histogram shows the total number of evry"
                )
            ),
            mainPanel(
                plotOutput(
                    outputId = "seas_epi_death"
                )
            )
        ),
        sidebarLayout(
            sidebarPanel(
                h3(
                    "Total number of death per hous"
                ),
                p(
                    "This histogram shows the total number of evry house"
                )
            ),
            mainPanel(
                plotOutput(
                    outputId =  "houses"
                )
            )
        )
    ),
    tabPanel(
        "Dynamic visualisation",
        sidebarLayout(
            sidebarPanel(
                radioButtons(
                    inputId = "ses",
                    label = "Select season",
                    choices = c("1","2","3","4","5","6","7","8")
                )
            ),
            mainPanel(
                plotOutput(
                    outputId = "seso"
                )
            )
        ),
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
        ),
        sidebarLayout(
            sidebarPanel(
                radioButtons(
                    inputId = "box_season",
                    label = "Select season",
                    choices = c("1","2","3","4","5","6","7","8")
                ),
                sliderInput(
                    inputId = "main_char",
                    label = "Important Char",
                    min = 20,
                    max = 60,
                    value = 45
                )
            ),
            mainPanel(
                plotOutput(
                    outputId = "box"
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
                    x = factor(episodeNum),
                    y = ttDeath
                )
            ) + geom_bar(stat="identity") +
                labs(
                    x = "Episodes",
                    y = "Total number of death",
                    title = "Variation of number of death for each season"
                )
        }
    )
    output$houses <- renderPlot(
        {
            char_house = characters %>% 
                select(house, killedBy) %>% 
                na.omit() %>% group_by(house) %>% 
                summarise(nb=n()) 
            char_house_ordered = char_house %>% 
                arrange(desc(nb)) %>% 
                mutate(house=factor(house,levels=house))
            ggplot(data = char_house_ordered)+ 
                geom_bar(aes(x=house, y=nb), stat='identity')+ 
                scale_x_discrete("house")+ 
                scale_y_continuous("deaths")
        }
    )
    output$seas_epi_death <- renderPlot(
        {
            death_episode = episodes %>% 
                left_join(scenes) %>% 
                group_by(seasonNum, episodeNum) %>% 
                summarize (nbe=sum(nbdeath))
            death_saison = death_episode %>% 
                group_by(seasonNum) %>% 
                summarize (nbs=sum(nbe)) 
            seasons = death_saison %>% 
                arrange(nbs) %>% 
                mutate(snb=factor(seasonNum,levels = seasonNum))
            df = death_episode %>% left_join(seasons) %>% filter(!is.na(snb))
            ggplot(data = df)+ 
                geom_bar(aes(y=snb,x=nbe,fill=factor(episodeNum,level=10:1)), stat="identity")+ 
                scale_fill_brewer("episode",palette = "Spectral")+theme_bw()+ 
                geom_text(data=seasons,aes(y=snb,x=nbs, label=paste(round(nbs),'deaths')),hjust = "left")+ 
                scale_x_continuous("deaths", breaks = seq(0,100,by=25),limits = c(0,100),expand = c(0,1))+ 
                scale_y_discrete("season")
        }
    )
    output$seso <- renderPlot(
        {
            death_episode=scenes%>%
                left_join(episodes) %>%filter(seasonNum==input$ses)%>% 
                group_by(episodeTitle, episodeNum) %>% 
                summarize(nbe=sum(nbdeath), duration_epi=sum(duration)) %>% filter(nbe != 0)
            ggplot(death_episode,aes(x=duration_epi/60,y=nbe,col=factor(episodeNum)))+
                geom_point(aes(size=2*nbe))+
                geom_text(data=death_episode,aes(label=episodeTitle),vjust=-0.6)+
                scale_x_continuous("Durée de l'épisode")+
                scale_y_continuous("Nombre des morts")+
                scale_color_brewer("Episode",palette ="Spectral")+
                guides(colour = "legend", size = "legend")+
                theme_bw()
        }
    )
    output$box <- renderPlot(
        {
            death_char =  scenes%>% 
                left_join(appearances) %>%
                left_join(characters) %>%filter(!is.na(killedBy)) %>% 
                left_join(episodes) %>%filter(seasonNum==input$box_season) %>% 
                group_by(seasonNum, episodeNum, name)%>% 
                summarize(dur_Car=sum(duration)) %>% 
                arrange(desc(dur_Car))
            char = death_char %>% group_by(seasonNum, name)%>%
                summarize(dur_seas=sum(dur_Car))%>%
                filter(dur_seas>60*input$main_char) %>% 
                arrange(dur_seas)%>%
                mutate(nameC=factor(name,levels = name))
            db = death_char %>% left_join(char) %>%filter(!is.na(nameC))
            ggplot(data=db)+
                geom_boxplot(aes(x=nameC,y=dur_Car, fill=factor(seasonNum)))+
                scale_x_discrete("Characters Mortes")
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
