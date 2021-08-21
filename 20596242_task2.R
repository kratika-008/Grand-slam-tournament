  #--------------------------Packages------------------------------------------------------------------------
  
  library(shiny)
  library(ggplot2)
  library(plotly)
  library(shinydashboard)
  library(dplyr)
  library(tidyverse)
  
  #final
  # ------------------------------Load Data and conversion--------------------------------------------------
  
  grand_slam_data <- read.csv(file="C:/Users/dell/OneDrive - LA TROBE UNIVERSITY/Documents/grand_slam_data.csv")
  
  #grand_slam_data<-read.csv(file = "grand_slam_data.csv")
  #View(grand_slam_data)
  
  winner<-grand_slam_data$winner
  tournament<-grand_slam_data$tournament
  year<-as.numeric(grand_slam_data$year)
  
  # defaultYearValue <- 2017
  # yearValue <- as.character(1877:2018) %>% setNames(nm = .)
  
  
  #------------------------------------UI --------------------------------------------------------------------
  
  ui <- dashboardPage(
     
     dashboardHeader(title="Tennis Grand Slam Winners Analysis",titleWidth = 400),
      
     dashboardSidebar(
  
      sidebarMenu (
      h4(checkboxInput("checkbox", strong("Compare Performances of all tournaments "),TRUE ),
         radioButtons("selecttou", "Individual Tournament",
                     choices = list("Australian Open" = 1,
                                    "Wimbledon" = 2,
                                    "U.S. Open" = 3,
                                    "French Open" = 4),selected = 1),
         sliderInput("years", "Year", min= 1877,max=2018,value = 1953),)
      )),
    
dashboardBody( 
  tabBox(title = "Tab Box",id = "tabset1",width=1000,
 
tabPanel("Task 3 (a.1)" ,plotlyOutput("plott1")),
tabPanel("Task 3 (a.2)", plotlyOutput("plot1")),
tabPanel("Task 3 (b)" ,plotlyOutput("plot2"))
)
)
)
 
# -----------------------------------------SERVER---------------------------------------------------------
  
  server <- function(input, output) {
    
    
    
    # -----------------------------------------TASK 2 (A)---------------------------------------------------
    
    No_wins<-grand_slam_win$No_wins
    
    output$plott1<-renderPlotly({
      
      if(input$checkbox == TRUE)
      {
        
        grand_slam_win = grand_slam_data %>%
          group_by(winner) %>% 
          summarise(No_wins=n()) %>% 
          arrange(desc(No_wins))
        
        grand_slam_win$winner <- factor(grand_slam_win$winner,levels = grand_slam_win$winner[order(grand_slam_win$No_wins)])
        
        grand_slam_winner_group= grand_slam_win %>% 
          filter(No_wins>=7)
        
        grand_slam_win_tour= grand_slam_data%>%
          filter(winner %in% grand_slam_winner_group$winner)%>%
          group_by(tournament,winner)%>%
          summarise(No_of_wins=n())%>%
         
          arrange(desc(No_of_wins))
        
        grand_slam_win_tour$No_of_wins<-factor(grand_slam_win_tour$No_of_wins)
        
      
        
      a<-  ggplot(data=grand_slam_win_tour, aes(x=winner,y=No_of_wins,fill=tournament)) +
          geom_bar(stat='identity', color = "black",position="dodge") +
          # scale_y_discrete()+
          #facet_grid(. ~tournament)+
          #scale_fill_gradientn(colours = the_colours)+
          #coord_flip()+
          xlab("Winners")+ylab("Number Of Wins")+ 
          theme(plot.title = element_text(size = 13))+
          scale_fill_brewer(palette = "Greens")+
          theme_classic(base_size = 16)+
          theme(plot.background = element_rect(fill = "#ECF0F5"),
                panel.background = element_rect(fill = "#F5F7FA"))+
          #scale_x_discrete(guide = guide_axis(n.dodge=3))+
          theme(plot.title = element_text(size = 10,face = "bold"))+
        theme(axis.text.x = element_text(angle = 90, size = 7,face="bold"))+
          #theme(strip.text.x = element_text(size = 8))+
          ggtitle("Comparing Performances of winners of all Grand Slam tournaments")
        
     ggplotly(a)
         
      }
    })
  #  ---------------------------------task 2(a.2)---------------------------
    
    
    #filter data of all 4 tournaments separately
    
    Ausdata<-grand_slam_win_tour
    FilteredAus = filter(Ausdata, tournament == "Australian Open")
    
    No_of_wins<-FilteredAus$No_of_wins
    winners <- FilteredAus$winner[order(No_of_wins, decreasing = TRUE)]
 
    Wimbdata<-grand_slam_win_tour
    Filteredwimb<-filter(Wimbdata,tournament =="Wimbledon")
    No_of_winsW<-Filteredwimb$No_of_wins
    winnersW <- Filteredwimb$winner [order (Filteredwimb$No_of_wins, decreasing = TRUE)]
    
    USdata<-grand_slam_win_tour
    FilteredUS<-filter(USdata,tournament =="U.S. Open")
    No_of_winsUS<-FilteredUS$No_of_wins
    winnersUS <- FilteredUS$winner [order (FilteredUS$No_of_wins, decreasing = TRUE)]
    
    Fdata<-grand_slam_win_tour
    FilteredF<-filter(Fdata,tournament =="French Open")
    No_of_winsF<-FilteredF$No_of_wins
    winnersF <- FilteredF$winner [order (FilteredF$No_of_wins, decreasing = TRUE)]
   
    output$plot1 <- renderPlotly({
      ggplotly({
      if (input$selecttou == 1)
      {
      ggplot(FilteredAus, aes(x=No_of_wins, y=winners,group=tournament)) +
          geom_bar(stat='identity', color = "black",fill="#48B5BF",position="dodge")+
       #  geom_line( lwd = 1.5,color="gold1")+
      theme_classic(base_size = 16)+   theme(plot.title = element_text(size = 13,face = "bold"))+
          ggtitle("Winners Performances for tournament Australian Open")+ ylab("Winners")+xlab("Number Of Wins")+
          theme(plot.background = element_rect(fill = "#ECF0F5"))
     }
      
     else if(input$selecttou == 2)
     {
       
     ggplot(Filteredwimb, aes(x=No_of_winsW, y=winnersW,group=tournament)) +
         geom_bar(stat='identity', color = "black",fill="#48B5BF",position="dodge" )+
         #geom_line( lwd = 1.5,color="gold1" )
       theme_classic(base_size = 16)+   
       theme(plot.title = element_text(size = 13,face = "bold"))+
         ggtitle("Winners Performances for tournament Wimbledon")+ ylab("Winners")+xlab("Number Of Wins")+
         theme(plot.background = element_rect(fill = "#ECF0F5"))
       
     }
      
      else if(input$selecttou == 3)
      {
       ggplot(FilteredUS, aes(x=No_of_winsUS, y=winnersUS,group=tournament)) +
          geom_bar(stat='identity', color = "black",fill="#48B5BF",position="dodge")+  theme(plot.title = element_text(size = 13,face = "bold"))+
          #geom_line( lwd = 1.5,color="gold1" )+
          theme_classic(base_size = 16)+ 
         theme(plot.title = element_text(size = 13,face = "bold"))+
          ggtitle("Winners Performances for tournament U.S Open")+ ylab("Winners")+xlab("Number Of Wins")+
          theme(plot.background = element_rect(fill = "#ECF0F5")) 
     
      }
      
      else if(input$selecttou == 4)
      {
       ggplot(FilteredF, aes(x=No_of_winsF, y=winnersF,group=tournament)) +
          geom_bar(stat='identity', color = "black",fill="#48B5BF",position="dodge" )+
         #geom_line( lwd = 1.5,color="gold1" )+
          theme_classic(base_size = 16)+   
         theme(plot.title = element_text(size = 13,face = "bold"))+
          ggtitle("Winners Performances for tournament French Open")+ ylab("Winners")+xlab("Number Of Wins")+
          theme(plot.background = element_rect(fill = "#ECF0F5")) 
        
      }
    })
    })
    
    # -----------------------------------TASK 2 (B)--------------------------------------------------------------------------
    
   
    # --------------- Top 5 winners yearly using reactive()  ------------------------------------
   
    
    slamreactivedataset = grand_slam_data %>%
      group_by(year,winner,tournament) %>% 
      summarise(Number_of_wins=n()) %>% 
      arrange(desc(Number_of_wins))
    
   # yearr<-slamreactivedataset$year
    filtered_grandslam <- reactive({
      slamreactivedataset%>%
        arrange(winner) %>%
        filter(year == input$years)
    })
   
  output$plot2<- renderPlotly({
    
   
  
    s<-ggplot(filtered_grandslam(), aes(interaction(x=year,Number_of_wins),y=winner,fill=tournament))+
      geom_bar(stat = "identity",position = position_dodge2(preserve = "single"))+
      xlab("Year")+ ylab("Winners")+scale_fill_brewer(palette = "Oranges")+
      #facet_grid(. ~tournament)+
      #coord_flip()+
      theme_classic(base_size = 16)+theme(plot.background = element_rect(fill="#ECF0F5"))+
      theme(strip.text.x = element_text(size = 8))+ 
      theme(plot.title = element_text(size = 13,face = "bold"))+ theme(legend.background = element_rect(fill="#ECF0F5"))+
       ggtitle("Yearly performances of winners for all Grand Slam Tournaments")
   ggplotly(s)
   
   
  })
  
  output$plot <- renderText(
    
    paste("Yearly performances of winners for all Grand Slam Tournaments") 
  
    
  )
  
  
  
#--------------------------------------Run Code----------------------------------------------------
  
  }
  
  shinyApp(ui, server)  
  
  #https://rstudio.github.io/shinydashboard/get_started.html
  #https://unc-libraries-data.github.io/R-Open-Labs/Extras/shiny/shiny.html
  #https://guidotti.shinyapps.io/h83h5/
