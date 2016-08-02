dashboardPage(skin = "blue",
      dashboardHeader(title = "HPSW-Prof", titleWidth = 300),

      dashboardSidebar(width = 300,
        sidebarMenu(id = "sidebarmenu",
          selectInput("scriptName", "Script Name", choices = script_names),
          menuItem("Profiler", tabName = "profiler"),
          conditionalPanel("input.sidebarmenu === 'profiler'",
                           dateRangeInput("dateId", "Date range"),
                           selectInput("scriptId", "Script Id", choices = NULL),
                           actionButton("updateButton", "Update",
                                        tags$style(type='text/css', "#updateButton { width: 100px;
                                        margin:auto; display:block; }"))
          ),

          menuItem("Predictions", tabName = "predictions"),

          menuItem("Download",
                   radioButtons(inputId = "downloadFormatt", label = "Choose a format: ",
                                choices = list("png","pdf"), inline = TRUE),
                   downloadButton(outputId = "downloadButton", label = "Download")
          ),
          menuItem("Source code", icon = icon("file-code-o"),
                    href = "https://github.com/mmondelli/swift-prof"
          )
        )#fim menu
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "profiler",
            fluidRow(
              valueBoxOutput("totalTime"),
              valueBoxOutput("totalApps"),
              valueBoxOutput("finalState")
            ),
            fluidRow(
              box(
                title = "Legend", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, formattableOutput("tableLegend"), width = 3
              ),

              tabBox(
                title = "Summary",
                id = "tabsetSummary", height = "250px", width = 9,
                tabPanel("Table", tableOutput("tableQuery")),
                tabPanel("Summary", verbatimTextOutput("summaryPlot"))
              )#fim tabbox
            ), #fim fluidRow

          fluidRow(
            box(
              title = "Duration", background = "black", solidHeader = TRUE, collapsible = TRUE,
              plotOutput("plotDuration")
            ),

            box(
              title = "Cpu Usage", background = "black", solidHeader = TRUE, collapsible = TRUE,
              plotOutput("plotCpu")
            ),

            box(
              title = "Written Bytes", background = "black", solidHeader = TRUE, collapsible = TRUE,
              plotOutput("plotBytesWritten")
            ),

            box(
              title = "Read Bytes", background = "black", solidHeader = TRUE, collapsible = TRUE,
              plotOutput("plotBytesRead")
            ),

            box(
              title = "Memory used", background = "black", solidHeader = TRUE, collapsible = TRUE,
              plotOutput("plotMemory")
            )
          )#fim fluidRow
         ),#fim tabItem
         tabItem(tabName = "predictions",
          fluidRow(
            box(
              title = "Regression ", solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("plotRegression", height = 500)
            ),
            box(title = "Data Summary", width = 6, status = "warning",
                tableOutput("textDataSumamry")
            ),
            box(title = "Regression Summary", width = 6, status = "warning",
                formattableOutput("tableDescription")
            )
          )
         )#fim tabItem
        )#fim tabItems
      )#fim body
    )

