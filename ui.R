dashboardPage(skin = "blue",
              dashboardHeader(title = "HPSW-Prof", titleWidth = 250
                              ),
              
              dashboardSidebar(width = 250, 
                               sidebarMenu(id = "sidebarmenu",
                                           menuItem(icon = icon("hand-pointer-o"), "Select", tabName = "select",
                                           selectInput("scriptName", "Script Name", choices = script_names),
                                           #conditionalPanel("input.sidebarmenu === 'profiler'", 
                                                            dateRangeInput("dateId", "Date range"),
                                                            selectInput("scriptId", "Script Id", choices = NULL),
                                                            actionButton("updateButton", "Update", width="60%"),
                                                            tags$style(type='text/css', "#updateButton {
                                                                       margin:auto; margin-bottom: 0.5cm; display:block; }")
                                           ),
                                           menuItem(icon = icon("bar-chart"), "Profiler", tabName = "profiler"),
                                           menuItem(icon = icon("spinner"), "Execution overview", tabName = "executionOverview"),
                                           menuItem(icon = icon("search"), "Domain info", tabName = "domainInfo"),
                                           
                                           # menuItem("Download",
                                           #          radioButtons(inputId = "downloadFormatt", label = "Choose a type: ",
                                           #                      choices = list("Duration"=1,"Cpu Usage"=2,"Written Bytes"=3,
                                           #                                     "Read Bytes"=4,"Memory used"=5,"All Graphis"=6),selected = 6),
                                           #          downloadButton(outputId = "downloadButton", label = "Download")
                                           #          
                                           # ),
                                           menuItem(icon = icon("code"), "Source code",
                                                    href = "https://github.com/mmondelli/swift-prof"
                                           )
                                )#fim menu
              ),
              dashboardBody(useShinyjs(),
                            tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                            ),
                tabItems(
                  tabItem(tabName = "profiler",
                          fluidRow(
                            valueBoxOutput("totalTime"),
                            valueBoxOutput("totalApps"),
                            valueBoxOutput("finalState")
                          ),
                          fluidRow(
                            #box(
                            #  title = "Legend", status = "primary", solidHeader = TRUE,
                            #  collapsible = TRUE, formattableOutput("tableLegend"), width = 3
                            #),
                            
                            tabBox(
                              title = "Summary",
                              id = "tabsetSummary", width = 12,
                              tabPanel("Table", formattableOutput("tableQuery")),
                              tabPanel("Summary", verbatimTextOutput("summaryPlot"))
                            ),#fim tabbox
                            box(
                              title = "Level of Parallelism", solidHeader = TRUE, collapsible = TRUE,
                              plotOutput("plotGantt"), width = 12
                            )
                          ), #fim fluidRow
                         
                          fluidRow(
                            box(
                              title = "Duration", solidHeader = TRUE, collapsible = TRUE,
                              plotOutput("plotDuration")
                            ),
                            
                            box(
                              title = "CPU Usage", solidHeader = TRUE, collapsible = TRUE,
                              plotOutput("plotCpu")
                            ),
                            
                            box(
                              title = "Written Bytes", solidHeader = TRUE, collapsible = TRUE,
                              plotOutput("plotBytesWritten")
                            ),
                            
                            box(
                              title = "Read Bytes", solidHeader = TRUE, collapsible = TRUE,
                              plotOutput("plotBytesRead")
                            ),
                            
                            box(
                              title = "Memory used", solidHeader = TRUE, collapsible = TRUE,
                              plotOutput("plotMemory")
                            ),
                            
                            box(
                              title = "System and User CPU usage", solidHeader = TRUE, collapsible = TRUE,
                              plotOutput("plotKernelUser")
                            ),
                            
                            box(
                              title = "Read and Written data", solidHeader = TRUE, collapsible = TRUE,
                              plotOutput("plotReadWritten")
                            )
                          )#fim fluidRow
                  ),
                  # tabItem(tabName = "executionOverview",
                  #           box(
                  #             title = "Workflow execution and its arguments",  
                  #             collapsible = TRUE, width = NULL, status = "primary",
                  #             div(style = 'overflow-x: scroll', DT::dataTableOutput('tableWorkflowExecution'))
                  #           ),
                  #           box(
                  #             title = "Executed activities and its arguments (summary)", 
                  #             collapsible = TRUE, width = NULL, status = "primary",
                  #             div(style = 'overflow-x: scroll', DT::dataTableOutput('tableAppsExecution'))
                  #           )
                  #         ),
                  tabItem(tabName = "domainInfo")
                )#fim tabItems
              )#fim body
)