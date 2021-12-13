#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
top_killers_data = killers_data %>% group_by(killedBy) %>% summarise(nb =n()) %>% arrange(desc(nb)) %>% head(10)
library(shiny)
library(ggplot2)
library(fmsb)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "GOT Visualisation",
    tabPanel(
        "Landing page",
        sidebarLayout(
            sidebarPanel(
                radioButtons(
                    inputId = "killer",
                    label = "Select killer",
                    choices = top_killers_data$killedBy
                )
            ),
            mainPanel(
                # img(src = "../images/hot1.jpg", height = 72, width = 72)
            )
        )
    ),
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
                    outputId = "sex"
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
        ),
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
    output$sex <- renderPlot(
        {
            #Créer un dataframe des plus grands meurterieux de la série
            top_killers = characters %>% 
                group_by(killedBy) %>% filter(!is.na(killedBy)) %>% 
                summarise(nb =n()) %>% 
                arrange(desc(nb)) %>% 
                head(5)
            #Créer un dataframe des houses le plus touchés par la mort
            top_houses_killed=characters %>% filter(!is.na(killedBy)) %>%filter(!is.na(house))%>%
                group_by(house) %>% 
                summarize(nbk=n()) %>% arrange(desc(nbk)) %>% head(5)
            #Créer un dataframe des meurterieux et des houses le plus touchés par la mort
            killers=top_killers %>% 
                left_join(characters) %>% filter(!is.na(killedBy)) %>% filter(!is.na(house))%>%
                left_join(top_houses_killed) %>% 
                group_by (killedBy, house)  %>%
                summarize(nbk=n())
            killers=pivot_wider(killers,names_from=house,values_from=nbk)
            killers[is.na(killers)]=0
            killers=killers%>% select(Stark,Lannister,Dothraki,`Gard Royal`,'Night Gard')
            killers=killers %>% filter(killedBy=='Jon Snow')
            dt=subset(killers, select = -c(killedBy))
            colnames(dt) <- c('Stark','Lannister','Dothraki','`Gard Royal`','Night Gard')
            rownames(dt) <- c('Jon Snow')
            dt = rbind(rep(7,5) , rep(0,5) , dt)
            colors_border=c( rgb(0.2,0.5,0.5,0.5), rgb(0.8,0.2,0.5,0.5) , rgb(0.7,0.5,0.1,0.5), rgb(1,0.7,0.8,0.5), rgb(0.5,0,0.5,0.5) )
            colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.5) , rgb(0.7,0.5,0.1,0.5), rgb(1,0.7,0.8,0.5), rgb(0.5,0,0.5,0.5) )
            radarchart( dt  , axistype=1 , 
                        pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                        cglcol="blue", cglty=1, axislabcol="blue", caxislabels=seq(0,8,2), cglwd=0.8,
                        vlcex=0.8)
            legend(x=1.6, y=1, legend = 'Jon Snow', bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=5)
        }
    )
    output$seasons <- renderPlot(
        {
            #Créer un dataframe des morts dans la série par saison
            deaths_in_serie = scenes %>% 
                left_join(episodes) %>% 
                group_by(seasonNum) %>% 
                summarize(nbd_in_serie=sum(nbdeath))
            #Construire le lineplot montrant l'évolution de la mort dans la série
            ggplot(data=deaths_in_serie)+
                geom_line(aes(x=seasonNum, y=nbd_in_serie), stat='identity')+
                scale_x_continuous("Season", breaks =seq(1,8, by=1), limits =c(1,8), expand=c(0,1))+
                scale_y_continuous("Deaths")
        }
    )
    output$houses <- renderPlot(
        {
            #Créer une dataframe des morts par house
            
            char_house = characters %>% 
                select(house, killedBy) %>% 
                na.omit() %>% group_by(house) %>% 
                summarise(nbd_by_house=n())%>%
                arrange(desc(nbd_by_house)) %>% 
                mutate(house=factor(house,levels=house)) #ordonner le dataframe par nombre de morts cummulé par house 
            #Construire le barplot mobtrant la mort par house
            ggplot(data = char_house)+ 
                geom_bar(aes(x=house, y=nbd_by_house), stat='identity')+ 
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
