shinyUI(fluidPage(
  
  titlePanel("MegaBummer"),
  fluidRow(
    
    column(3,
           h3("input"),
           actionButton("action", label = "Action"),
           textInput("text", label = h3("Text input"), 
                     value = "Enter text..."), 
           br(), 
           submitButton("Submit"))
    

  ),
  
  sidebarLayout(
    sidebarPanel( "rn"),
    
    mainPanel("the tweets")
  )
))