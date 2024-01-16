library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(magrittr)

linebreaks <- function(n){HTML(strrep(br(), n))}
team <- read_excel("../data/teamTest.xlsx")

shinyApp(
  ui <- fluidPage(
    
    titlePanel("Frisbee Stats"),
    hr(),
    # mainPanel(
    #   DT::DTOutput("teamDf"),
    #  ),
    # mainPanel(
    #   DT::DTOutput("team")
    # ),
    fluidRow(
      column(2, offset=1,
             selectInput("team", label = "Team", unique(team$team))),
      column(6, offset = 1, uiOutput("secondSelection")),
      
      column(2, offset=1,
             selectInput("opponent", label = "Opponent", c("Team 1", "Team 2", "Team 3", "Team 4", "Team 5"))),
      column(2,offset=1,
             dateInput("date", "Date", format = "mm-dd-yy"))
    ),
    linebreaks(1),
    
    fluidRow(column(7, offset = 2,
                    actionButton("offense", "Offense"),
                    actionButton("defense", "Defense"),
                    actionButton("oppScore", "Opponent Score"),
                    actionButton("submit", "Submit Game"))),
    linebreaks(1),
    DT::dataTableOutput("data"),
    linebreaks(1),
    
    textOutput('myText'),
    linebreaks(1),
    mainPanel(
      width = 8,
      DT::dataTableOutput('teamTable')
    ),
    mainPanel(
      width = 8,
      tableOutput('score')
    )
  ),
  
  server <- function(input, output) {
    
    myValue <- reactiveValues(play = '')
    
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }
    
    
    
    
    teamFiltered <- eventReactive(input$team, {
      team %>% filter(team == input$team) #%>%dplyr::select(player)
    })
    teamFilteredPlayer <- eventReactive(input$players, {
      team %>% filter(player %in%input$players) #%>%dplyr::select(player)
    })
    output$teamDf <- DT::renderDataTable(
      teamFilteredPlayer(), server = FALSE, escape = FALSE, selection = 'none',rownames=FALSE, options=list(dom="t",pageLength=11)
      #> am== input$gearbox) #> commented because output is not shown when uncommented
    )
    
    # df <- reactiveValues(
       
      # data = bind_cols(teamFiltered(), Catch = shinyInput(actionButton, 5, 'button_', label = "Catch", onclick = 'Shiny.onInputChange(\"catch_button\",  this.id)' ))
                            
                          #  data.frame(
    #   
    #   Name = c('playerOne', 'playerTwo', 'playerThree', 'playerFour', 'playerFive'),
    #   Catch = shinyInput(actionButton, 5, 'button_', label = "Catch", onclick = 'Shiny.onInputChange(\"catch_button\",  this.id)' ),
    #   Drop = shinyInput(actionButton, 5, 'button_', label = "Drop", onclick = 'Shiny.onInputChange(\"drop_button\",  this.id)' ),
    #   ThrowAway = shinyInput(actionButton, 5, 'button_', label = "TA", onclick = 'Shiny.onInputChange(\"ta_button\",  this.id)' ),
    #   Score = shinyInput(actionButton, 5, 'button_', label = "Score", onclick = 'Shiny.onInputChange(\"score_button\",  this.id)' ),
    #   Assist = shinyInput(actionButton, 5, 'button_', label = "Assist", onclick = 'Shiny.onInputChange(\"assist_button\",  this.id)' ),
    #   Defense = shinyInput(actionButton, 5, 'button_', label = "Defense", onclick = 'Shiny.onInputChange(\"d_button\",  this.id)' ),
    #   PickUp = shinyInput(actionButton, 5, 'button_', label = "Pick Up", onclick = 'Shiny.onInputChange(\"pickUp_button\",  this.id)' )
    # 
    #   #row.names = 1:5
    
   #  )
    
     df <- reactiveValues(data = data.frame(
     
    #   Name = as.list(teamFiltered())
       Catch = shinyInput(actionButton, 11, 'button_', label = "Catch", onclick = 'Shiny.onInputChange(\"catch_button\",  this.id)' ),
      Drop = shinyInput(actionButton, 11, 'button_', label = "Drop", onclick = 'Shiny.onInputChange(\"drop_button\",  this.id)' ),
      ThrowAway = shinyInput(actionButton, 11, 'button_', label = "TA", onclick = 'Shiny.onInputChange(\"ta_button\",  this.id)' ),
      Score = shinyInput(actionButton, 11, 'button_', label = "Score", onclick = 'Shiny.onInputChange(\"score_button\",  this.id)' ),
      Assist = shinyInput(actionButton, 11, 'button_', label = "Assist", onclick = 'Shiny.onInputChange(\"assist_button\",  this.id)' ),
      Defense = shinyInput(actionButton, 11, 'button_', label = "Defense", onclick = 'Shiny.onInputChange(\"d_button\",  this.id)' ),
      PickUp = shinyInput(actionButton, 11, 'button_', label = "Pick Up", onclick = 'Shiny.onInputChange(\"pickUp_button\",  this.id)' )

    #   #row.names = 1:5
    #
    ))
    
    
    # output$data <- DT::renderDataTable(
    #   df$data, server = FALSE, escape = FALSE, selection = 'none', options = list(lengthChange = FALSE,
    #                                                                               dom = 't'
    #                                                                               ), rownames=FALSE
    # )
     output$data <- DT::renderDataTable(
       bind_cols(teamFilteredPlayer()%>%dplyr::select(player), df$data[1:nrow(teamFilteredPlayer()),]), server = FALSE, escape = FALSE, selection = 'none', options = list(lengthChange = FALSE,
                                                                                   dom = 't', pageLength=11
       ), rownames=FALSE
     )
     
     output$secondSelection <- renderUI({
       selectInput("players", "Players", choices = teamFiltered()%>%dplyr::select(player), multiple = T, selected = c(team$player))
     })
    script <- data.frame(play="")
    
    #Catch
    observeEvent(input$catch_button, {
      selectedRow <- as.numeric(strsplit(input$catch_button, "_")[[1]][2])
      tmp <- teamTable()
      tmp[selectedRow, "Catch"] <-  tmp[selectedRow, "Catch"]+as.integer(1)
      teamTable(tmp)
      myValue$play <<- paste('Catch ',teamFilteredPlayer()[selectedRow,2])
      script <- bind_rows(script, play=as.data.frame(myValue$play))
    })
    #Drop
    observeEvent(input$drop_button, {
      selectedRow <- as.numeric(strsplit(input$drop_button, "_")[[1]][2])
      tmp <- teamTable()
      tmp[selectedRow, "Drop"] <-  tmp[selectedRow, "Drop"]+as.integer(1)
      teamTable(tmp)
      myValue$play <<- paste('Drop ',teamFilteredPlayer()[selectedRow,2])
      script <- bind_rows(script, play=as.data.frame(myValue$play))
    })
    #TA
    observeEvent(input$ta_button, {
      selectedRow <- as.numeric(strsplit(input$ta_button, "_")[[1]][2])
      tmp <- teamTable()
      tmp[selectedRow, "TA"] <-  tmp[selectedRow, "TA"]+as.integer(1)
      teamTable(tmp)
      myValue$play <<- paste('Throw Away ',teamFilteredPlayer()[selectedRow,2])
      script <- bind_rows(script, play=as.data.frame(myValue$play))
    })
    #Score
    observeEvent(input$score_button, {
      selectedRow <- as.numeric(strsplit(input$score_button, "_")[[1]][2])
      tmp <- teamTable()
      tmp[selectedRow, "Score"] <-  tmp[selectedRow, "Score"]+as.integer(1)
      teamTable(tmp)
      tmp2 <- score()
      tmp2[1, "Score"] <-  tmp2[1, "Score"]+as.integer(1)
      score(tmp2)
      myValue$play <<- paste('Score ',teamFilteredPlayer()[selectedRow,2])
      script <- bind_rows(script, play=as.data.frame(myValue$play))
    })
    #Assist
    observeEvent(input$assist_button, {
      selectedRow <- as.numeric(strsplit(input$assist_button, "_")[[1]][2])
      tmp <- teamTable()
      tmp[selectedRow, "Assist"] <-  tmp[selectedRow, "Assist"]+as.integer(1)
      teamTable(tmp)
      myValue$play <<- paste('Assist ',teamFilteredPlayer()[selectedRow,2])
      script <- bind_rows(script, play=as.data.frame(myValue$play))
    })
    #Defense
    observeEvent(input$d_button, {
      selectedRow <- as.numeric(strsplit(input$d_button, "_")[[1]][2])
      tmp <- teamTable()
      tmp[selectedRow, "Defense"] <-  tmp[selectedRow, "Defense"]+as.integer(1)
      teamTable(tmp)
      myValue$play <<- paste('Defense ',teamFilteredPlayer()[selectedRow,2])
      script <- bind_rows(script, play=as.data.frame(myValue$play))
    })
    #Pick Up
    observeEvent(input$pickUp_button, {
      selectedRow <- as.numeric(strsplit(input$pickUp_button, "_")[[1]][2])
      tmp <- teamTable()
      tmp[selectedRow, "PickUp"] <-  tmp[selectedRow, "PickUp"]+as.integer(1)
      teamTable(tmp)
      myValue$play <<- paste('Pick Up ',teamFilteredPlayer()[selectedRow,2])
      script <- bind_rows(script, play=as.data.frame(myValue$play))
    })
    
    
    
    output$myText <- renderText({
      
      myValue$play
      
    })
    teamTable <- reactiveVal(
      data.frame(
                 Catch = as.integer(rep(0, 11)),
                 Drop = as.integer(rep(0, 11)),
                 TA= as.integer(rep(0, 11)),
                 Score = as.integer(rep(0, 11)),
                 Assist=as.integer(rep(0, 11)),
                 Defense=as.integer(rep(0,11)),
                 PickUp=as.integer(rep(0,11)))
    )
    
    # output$teamTable = renderTable(teamTable(),
    #                                bordered = TRUE,
    #                                spacing = c('l'),
    #                                width = "100%",
    #                                striped = TRUE,
    #                                align = 'c',
    #                                rownames = FALSE)
    
    output$teamTable <- DT::renderDataTable(
      bind_cols(teamFilteredPlayer()%>%dplyr::select(player),teamTable()[1:nrow(teamFilteredPlayer()), ]), server = FALSE, escape = FALSE, selection = 'none', options = list(lengthChange = FALSE,
                                                                                      #searching=FALSE,
                                                                                      dom = 't',
                                                                                      pageLength= 11,
                                                                                      
                                                                                      #iDisplayLength=11,                    # initial number of records
                                                                                      #aLengthMenu=c(5,10),                  # records/page options
                                                                                      #bLengthChange=0,                       # show/hide records per page dropdown
                                                                                      #bFilter=0,                                    # global search box on/off
                                                                                      #bInfo=0,                                      # information on/off (how many records filtered, etc)
                                                                                      #bAutoWidth=0,                            # automatic column width calculation, disable if passing column width via aoColumnDefs
                                                                                      aoColumnDefs = list(list(sWidth="300px", aTargets=c(list(0),list(1))))    # custom column size            )
    ), rownames=FALSE)
    
    #Scores
    score <- reactiveVal(
      data.frame(
        Score = as.integer(rep(0, 1)),
        Opponent = as.integer(rep(0, 1)))
    )
    
    output$score = renderTable(score(),
                               bordered = TRUE,
                               spacing = c('l'),
                               width = "100%",
                               striped = TRUE,
                               align = 'c',
                               rownames = FALSE)
    #Scores
    observeEvent(input$oppScore, { 
      tmp <- score()
      tmp[1, "Opponent"] <-  tmp[1, "Opponent"]+as.integer(1)
      score(tmp)
      
    })
    
  }
  

)