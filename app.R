library(shiny)
library(dplyr)
library(stringr)
library(Lahman)
library(readr)


Batting2 <- read_csv('Batting2.csv')
                       
ui <- fluidPage(
  h2('Player Comparison Tool'),
  sidebarPanel(
    selectInput(inputId = "player",
                label = "Player Name",
                choices = sort(Batting2$name)),
    numericInput(inputId = 'numdisplay', label = "Number of Results to Display", value = 10, min = 2, max = 100),
    sliderInput(inputId = 'daterange',
                label = "Players Making their Debut Between:",
                value = c(1950,2014), min = 1950, max = 2014, sep = ''),
    sliderInput(inputId = 'agerange',
                label = 'Player Ages to include',
                value = c(17, 59), min = 17, max = 59),
    numericInput(inputId = 'minpa', label = "Minimum Plate Appearances", value = 1000, min = 1, max = 10000),
    checkboxGroupInput("variable", "Variables to Compare on:", 
                       choices = c('BA','OBP','ISO','K%' = "percentK",'BB%' = "percentBB", ##SLG excluded because ISO is a linear combination of BA and SLG##
                                   'Single%' = "percentSingle" , 'Double%' = "percentDouble" ,
                                   'Triple%' = "percentTriple" ,'HR%' = "percentHR"))
  ),
  
  mainPanel(tableOutput("table"),
            textOutput('warning')
  ))


server <- function(input, output) {
 batting <- reactive({
   Batting2 %>%
     mutate(season_age = yearID - birthYear,
            IBB = ifelse(is.na(IBB),0, IBB),
            SF = ifelse(is.na(SF),0,SF)) %>%
     filter(season_age %in% seq(input$agerange[1], input$agerange[2]) & 
              debutyear >= input$daterange[1] &
              debutyear <= input$daterange[2]) %>%
     group_by(name, playerID) %>%
     summarise(G = sum(G, na.rm=T),
               AB = sum(AB, na.rm=T),
               X2B = sum(X2B, na.rm=T),
               X3B = sum(X3B, na.rm=T),
               HR = sum(HR, na.rm=T),
               SO = sum(SO, na.rm=T),
               BB = sum(BB, na.rm=T),
               IBB = sum(IBB, na.rm=T),
               HBP = sum(HBP, na.rm=T),
               SH = sum(SH, na.rm=T),
               SF = sum(SF, na.rm=T),
               H = sum(H, na.rm=T)) %>%
     mutate(pa = AB + BB + IBB + HBP + SH + SF,
            H1B = H - X2B - X3B - HR,
            percentSingle = H1B/pa,
            percentDouble = X2B/pa,
            percentTriple = X3B/pa,
            percentHR = HR/pa,
            percentBB = BB/pa,
            percentK = SO/pa,
            OBP = (H1B + X2B + X3B + HR + BB + IBB + HBP)/pa,
            SLG = (1*H1B + 2*X2B + 3*X3B + 4*HR)/AB,
            BA = (H1B + X2B + X3B + HR)/AB,
            ISO = SLG - BA) %>%
     filter(AB>0) %>%
     ungroup() %>%
     left_join(select(Batting2, playerID, Max_Age, Debut_Age) %>% distinct(), by = 'playerID')
 })

 player <- reactive({
   as.matrix(filter(batting(), name == input$player) %>%
               select(match(input$variable, names(batting()))))
 })

 output$warning <-  renderText({if (length(player())==0) 'No Player Data'})

players <- reactive({filter(batting(), pa >= input$minpa)})
 playercovs <- reactive({
    as.matrix(select(players(), match(input$variable, names(batting()))))
                    })
 mahal <- reactive({mahalanobis(playercovs(), player(), cov(playercovs()))})
 
 output$table <- renderTable({
   if (length(player())>0){
     head(players()[order(mahal()),] %>%
            select(playerID) %>%
            left_join(players(), by = 'playerID', copy = TRUE) %>%
            filter(pa >= input$minpa) %>%
            select(name, playerID, Debut_Age, Max_Age, G, pa, match(input$variable, names(players()))),
          input$numdisplay)}}
   ,digits = 3)
}

shinyApp(ui = ui, server = server)
