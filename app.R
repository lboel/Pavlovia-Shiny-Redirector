library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(DT)
library(purrr)
library(dplyr)
library(waiter)
library(readxl)
library(rhandsontable)

jscode <- "Shiny.addCustomMessageHandler('redirect', function(redirectURL) {window.location = redirectURL;});"
jsResetCode <- "Shiny.addCustomMessageHandler('reset', function(resetVar) {history.go(0);});" 


ui <- fluidPage(
  tags$head(
    tags$style(HTML(".jexcel {
                        width: auto;
                        height: auto;
                        white-space: normal;
                      }"))
  ),
  use_waiter(),
  tags$head(tags$script(jsResetCode)),
  tags$head(tags$script(jscode)),
  useShinyjs(),
  hidden(
    div(
      id = "main_content",
      dashboardPage(
        dashboardHeader(title = "Redirection"),
        dashboardSidebar(disable = T),
        dashboardBody(
          fluidPage(
          fluidRow(box(width = "100%",collapsible = T,
            title = "Hinweise",htmlOutput("usage"))),
          fluidRow(box(width = "100%",collapsible = T,
            title = "Config", column(12,
            rHandsontableOutput("table",width = "100%"),
          actionButton(inputId = "Redirect", label = "To Experiment"),
          downloadButton('downloadData', 'Download data')),
          fileInput("fileUpload", "Choose New XLSX File",
                    multiple = FALSE,
                    accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                               ".xlsx")),
          )),
          fluidRow(box(width = "100%",title="Overview",collapsible = T,
                       column(12, DT::dataTableOutput("dataTable")))),
          fluidRow(box(width = "100%",title="Current Participants",collapsible = T,
                       column(12,DT::dataTableOutput("runningDT")))),
          fluidRow(width = "100%",box(width = "100%",
            title = "Debug",
          h3("URL components"),
          verbatimTextOutput("urlText"),
          h3("Parsed query string"),
          verbatimTextOutput("queryText")))
        ))
      )
    )
  )
)

conditionFileDownloader <- function() {
  overview <- readxl::read_xlsx(path = "shinyRedirector.xlsx", sheet = "overview")
  participants <- readxl::read_xlsx(path = "shinyRedirector.xlsx", sheet = "participants", col_types = c("text"))
  config <- readxl::read_xlsx(path = "shinyRedirector.xlsx", sheet = "config")
  config <- config[1,]
  return(list(overview = overview, participants = participants, config = config))
}


server <- function(input, output, session) {
  waiter_show( # show the waiter
    html = spin_fading_circles() # use a spinner
  )

  conditionFile <- reactiveVal(conditionFileDownloader())
  
  queryParamters <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    query
  })
  
  updateGFileSheet <- function(newXLSX) {
    writexl::write_xlsx(newXLSX, "shinyRedirector.xlsx")
  }
  
  observeEvent(input$fileUpload, {
    tryCatch(
      {
        config <- readxl::read_xlsx(path = input$fileUpload$datapath, sheet = "config")
        if(nrow(config)==1)
        {
          file.copy(input$fileUpload$datapath,
                    "shinyRedirector.xlsx" , overwrite = TRUE)
          delay(1000,session$sendCustomMessage("reset","")   )       
        }
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  })
  
  observe({
    query <- queryParamters()
    overview <- conditionFile()$overview
    participants <- conditionFile()$participants
    config <- conditionFile()$config
    now_time <- Sys.time()
    
    oldParticipants <- participants  %>% filter( as.numeric(difftime(now_time,as.POSIXct(timestamp,format="%Y-%m-%d %H:%M:%OS"))) > 60 ) %>% mutate(status="INCOMPLETE")
  
    participants <- rows_update(participants, oldParticipants, by="tic" )
    
    conditionsCompleted <- participants %>%
      filter(status == "COMPLETE") %>%
      group_by(condition) %>%
      count() %>%
      ungroup()
    conditionsIncomplete <- participants %>%
      filter(status == "INCOMPLETE") %>%
      group_by(condition) %>%
      count() %>%
      ungroup()

    overview$completedParticipants <- 0
    overview$incompleteParticipants <- 0
    overview$activeParticipants <- 0
    overview$remainingParticipants <- 0
    
    if(nrow(conditionsCompleted) > 0)
    {
      overview <- overview %>% rowwise() %>% mutate(completedParticipants = ifelse(is.element(betweenCondition,conditionsCompleted$condition), conditionsCompleted %>% filter(betweenCondition ==condition) %>% .$n, 0))
    }
    
    if(nrow(conditionsIncomplete) > 0)
    {
      overview <- overview %>% rowwise() %>% mutate(incompleteParticipants = ifelse(is.element(betweenCondition,conditionsIncomplete$condition), conditionsIncomplete %>% filter(betweenCondition ==condition) %>% .$n, 0))
    }
    
    conditionsRunnindOrCompleted <- participants %>%
      filter(status == "RUNNING") %>%
      group_by(condition) %>%
      count() %>%
      ungroup()
    
    overview <- overview %>% rowwise() %>% mutate(activeParticipants = ifelse(is.element(betweenCondition,conditionsRunnindOrCompleted$condition), conditionsRunnindOrCompleted %>% filter(betweenCondition ==condition) %>% .$n, 0)) %>% ungroup()
    
    overview <- overview %>% mutate(remainingParticipants = expectedParticipants - activeParticipants - completedParticipants)
  
    updateGFileSheet(newXLSX = list(overview = overview, participants = participants, config = config))
    conditionFile(conditionFileDownloader())
    
    if (is.element("tic", names(query))) {
      print("Das ist ein Tic")
      tic <- query$tic
      returnURL <- config$surveyURL
      expURL <- config$pavloviaURL
      pilotToken <- config$pilotToken
      
      overview <- conditionFile()$overview
      config <- conditionFile()$config

      if (!is.element(tic, participants$tic)) {
        now <- as.character(now_time)
        
        print("TIC ist NEU")
        overviewNew <- overview
        print(overviewNew)
        possibleConditions <- overviewNew %>%
          filter(remainingParticipants > 0) %>%
          rowwise() %>%
          mutate(lists = list(rep(betweenCondition, remainingParticipants))) %>%
          .$lists %>%
          unlist()
        
        if(length(possibleConditions) < 1)
        {
          session$sendCustomMessage("redirect", paste0(expURL,"&",pilotToken))
          
        }
        
        newCondition <- sample(possibleConditions, 1)
        participants <- dplyr::bind_rows(participants, tibble(tic = tic, condition =    newCondition, status = "RUNNING", timestamp=now))
        updateGFileSheet(newXLSX = list(overview = overviewNew, participants = participants, config = config))
        conditionFile(conditionFileDownloader())
        session$sendCustomMessage("redirect", paste0(expURL, "condition=",    newCondition, "&tic=", tic,"&",pilotToken))
      }
      else {
        print("TIC ist ALT")
        rowIndexParticipantTable <- which(participants$tic == tic)
        conditionOftic <- participants$condition[rowIndexParticipantTable]
        statusOfTic <-    participants$status[rowIndexParticipantTable]
        
        if(statusOfTic=="RUNNING") 
        {
        if (is.element("status", names(query))) {
          print("TIC HAT EIN STATUS")
          newStatus <- query$status
          if (newStatus == "COMPLETE") {
            print("STATUS GEÄNDERT UND REDIRECT")
            participants$status[rowIndexParticipantTable] <- "COMPLETE"
            updateGFileSheet(newXLSX = list(overview = overview, participants = participants, config = config))
            session$sendCustomMessage("redirect", paste0(returnURL,"returntic=", tic,"&status=COMPLETE"))
          }
          if (newStatus == "INCOMPLETE") {
            print("STATUS GEÄNDERT UND REDIRECT")
            participants$status[rowIndexParticipantTable] <- "INCOMPLETE"
            updateGFileSheet(newXLSX = list(overview = overview, participants = participants, config = config))
            session$sendCustomMessage("redirect", paste0(returnURL,"returntic=", tic,"&status=INCOMPLETE"))
          }
          if (      newStatus != "INCOMPLETE" &       newStatus != "COMPLETE") {
            
            print("NEUER STATUS IST NICHT INCOMPLETE ODER COMPLETE")
            session$sendCustomMessage("redirect", paste0(expURL, "condition=", conditionOftic, "&tic=", tic, "&", pilotToken))
          }
        }
        }
        
        else {
          
          if(statusOfTic == "INCOMPLETE")
          {
          print("TIC WAR ALT UND STATUS INCOMPLETE UND KEIN STATUS VARIABLE ALSO WIEDER ZUM EXPERIMENT")
            session$sendCustomMessage("redirect", paste0(returnURL,"returntic=", tic,"&status=INCOMPLETE"))
          }
          
          if(statusOfTic == "COMPLETE")
          {
            print("TIC WAR ALT UND STATUS INCOMPLETE UND KEIN STATUS VARIABLE ALSO WIEDER ZUM EXPERIMENT")
            session$sendCustomMessage("redirect", paste0(returnURL,"returntic=", tic,"&status=COMPLETE"))
          }
          }
      }
    }
    else {
      conditionFile(conditionFileDownloader())
      show("main_content")
      waiter::waiter_hide()
    }
  })

  output$table <- renderRHandsontable(rhandsontable(data = conditionFile()$config))

  observeEvent(input$table,{
    table_data <- hot_to_r(input$table)
    if(!is.null(table_data)){
      updateGFileSheet(newXLSX = list(overview = conditionFile()$overview, participants =  conditionFile()$participants, config =  table_data))
      conditionFile(conditionFileDownloader())
    }}
  )
  
  output$downloadData <- downloadHandler(
    filename = "shinyRedirector.xlsx",
    content = function(con) {
         serverfile <- "shinyRedirector.xlsx"
         file.copy(serverfile, con)
    }
  )
    
  output$usage <- renderText({ '
<ul>
<li><strong>General Information:</strong>
<ul>
<li>TIC is the universal identifier for the participant</li>
<li>If the particpant visits the shiny-app for the first time (tic as uid), the participant will be redirected to a remaining condition,randomly. </br>
If the participant with the same UID is visiting again, they will be redirected to their previous assigned condition.&nbsp;</li>
<li> If the status is set in URL, the participant will be redirected to the Qualtrics link specified.&nbsp;</li>
<li>Assigning to the condition:
<ul>
<li>If the participant with a specfied UID is visiting a random condition is chosen and added to the redirection link to Pavlovia</li>
<li>After a participant is redirected, the remaining conditions are updated</li>
<li>remainingParticipants = expectedParticipants - activeParticipants -completedParticipants</li>
</ul>
</li>
<li>RUNNING-Participants will be treated as incomplete after 60 minutes</li>
</ul>
</li>
<li><strong>Adding new participants</strong>
<ul>
<li>New particpants are added by using this link (and directly redirected to Pavlovia)
<ul>
<li><strong>"SHINY-URL"+/?tic=*TIC*</strong></li>
</ul>
</li>
<li><strong>Edit status of participant</strong>
<ul>
<li>The status of a running participant can changed like this:
<ul>
<li>Completed:
<ul>
<li>"SHINY-URL"+/?tic=*TIC*&amp;status=COMPLETE</li>
</ul>
</li>
<li>Incomplete:
<ul>
<li>"SHINY-URL"+/?tic=*TIC*&amp;status=INCOMPLETE</li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
<li><strong>Updating the Condition File</strong>
<ul>
<li>You can Download and Upload the Database XLSX</li>
<li>furthmore you can Update the config sheet directly in the app</li>
</ul>
</li>
</ul>'})

  output$urlText <- renderText({
    paste(
      sep = "",
      "protocol: ", session$clientData$url_protocol, "\n",
      "hostname: ", session$clientData$url_hostname, "\n",
      "pathname: ", session$clientData$url_pathname, "\n",
      "port: ", session$clientData$url_port, "\n",
      "search: ", session$clientData$url_search, "\n"
    )
  })

  output$dataTable <- DT::renderDataTable({
    sheets <- conditionFile()
    sheets$overview
  } , options = list(scrollX = TRUE)
)

  output$runningDT <- DT::renderDataTable({
    sheets <- conditionFile()
    sheets$participants
  }, options = list(scrollX = TRUE))

  
  output$queryText <- renderText({
    query <- parseQueryString(session$clientData$url_search)
    paste(names(query), query, sep = "=", collapse = ", ")
  })

  observeEvent(input$Redirect, {
    session$sendCustomMessage("redirect", paste0(conditionFile()$config$pavloviaURL,conditionFile()$config$pilotToken))
  })
}

shinyApp(ui, server)
