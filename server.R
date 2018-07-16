library(shiny)
library(readr)
library(ggplot2)
library(tidyr)
library(tibble)
# Define server logic required to draw a histogram
getdirs <- function(){
  print (input$dir)
  dirs <-   list.dirs(input$dir)
  res = c("..",".", dirs)
  return (res)
}

curdir <- getwd()

getFiles <- function(){
  files <- list.files(getwd())
  files <- files [ file.exists(files)]
  files <- c(" ", files)
  return(files)
}

getDirs <- function(){
  files <- list.files(getwd())
  files <-  files[ dir.exists(files)]
  files <- c(".", "..", files)
  return(files)
}

getdf <- function(fpath){
  #fpath <- input$fileSelect
  if (! file.exists(fpath)){
    msg <- paste("File ", fpath, " does not exist")
    print (msg)
    return (NA)
  }
  df <- readr::read_csv(fpath)
  return (df)
}

df <- NA
server <- function(input, output, session) {
  
  observeEvent(input$dirSelect, {
    newdir <- input$dirSelect
    if (dir.exists(newdir)){
      setwd(newdir)
    }
    updateSelectInput(session, "fileSelect", "Files:", c(getFiles()))      
    updateSelectInput(session, "dirSelect", "Dirs:", getDirs(),
                      selected=NULL) 
  })
  
  
  
  observeEvent(input$fileSelect, {
    print ("here")

    #print(glimpse(df))
  })
  
  output$dirSelector <- renderUI({
    selectInput("dirSelect", "Dirs:", getDirs()) 
  })
  
  output$fileSelector <- renderUI({
    selectInput("fileSelect", "Files:", getFiles())
  })  
  
  output$statsTable <- DT::renderDataTable({
    df <- getdf(input$fileSelect)
    
    if (is.na(df)){
      sTable <- data.frame()
      msg <- "df is NA"
      print(msg)
    } else {
      sTable <- df
      msg <- paste("Df has nrows=",nrow(df))
      print(msg)
    }
    sTable
  } #options = list(sDom  = '<"top">t<"bottom">')
  )
}
