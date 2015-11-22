###### TODO
# Rise an event from the tree

## app.R ##
library(shiny)
library(htmlwidgets)
library(widgetHierarchy)
# library(fmsb) library(ggplot2) library(DiagrammeR) library(plotly)

source("chooser.R")
###################################
### SAMPLE
#sites54 <- read.csv2("dataset/Overall_Ranking_54Sites.csv")
sites54 <- cbind( SITES = paste0("'Site ", 1:54,"'"),
                 read.csv2("dataset/Overall_Table_v3.csv"))
sites54full <- cbind(sites54,
                 read.csv2("dataset/CopyOfMetric_refinment.csv"))
## MODIFY for the paper
sites54full[ sites54full$SITES == "'Site 6'", c("Completeness")] <- 50
sites54full[ sites54full$SITES == "'Site 6'", c("Timeliness")] <- 51
sites54full[ sites54full$SITES == "'Site 6'", c("Believability")] <- 30
sites54full[ sites54full$SITES == "'Site 54'", c("Completeness")] <- 26
sites54full[ sites54full$SITES == "'Site 54'", c("Believability")] <- 49
sites54full[ sites54full$SITES == "'Site 54'", c("Timeliness")] <- 50
#read.csv2("dataset/Overall_Ranking_54Sites.csv")
#
# Data must be given as the data frame, where the first cases show maximum.
#maxmin <- data.frame(  Total=c(5, 1),  Believability=c(15, 3),  Completeness=c(5, 1), Timeliness=c(5, 1), Environmental=c(3, 0) )
# data for radarchart function version 1 series, minimum value must be omitted from above.
RNGkind("Mersenne-Twister")
set.seed(123)
dat <- data.frame(
    #   Believability = rnorm(54, 25, 11),
    #   Completeness = runif(54, 1, 54),
    #   Timeliness = runif(54, 1, 54),
    #Environmental = rnorm(54, 10, 2),
    Latitude = rnorm(54, 52, 22),
    Longitude = rnorm(54, 15, 30) #, Criteria =  paste0(LETTERS[3],letters[3], sample(1:6,54,replace=T))
)
data54Sites <- cbind(sites54full, dat)
#thename <- colnames(data54Sites)
thename <- c("Believability", "Completeness","Timeliness") # "Total", "Environmental"
outSelection <- data54Sites
op <- par(mar = c(1, 2, 2, 1),mfrow = c(2, 2))
###################################
rooti <- 'Quality assessment' # Reporting quality assessment and ranking of OEM sites
theTree <- data.frame(
    name = c( rooti, 
              'Believability', 'Completeness', 'Timeliness', 
              paste0(LETTERS[3],letters[2], 1:3),
              paste0(LETTERS[3],letters[3], 1:8),
              paste0(LETTERS[3],letters[20], 1),
              paste0('Site ', 1:54)
              #                      paste0('Site ', 1:10),
              #                      paste0('Site ', 1:10)
    ),
    parent = c( NA, rooti, rooti, rooti,
              rep('Believability', each = 3),
              rep('Completeness', each = 8),
              rep('Timeliness', each = 1),
              rep( paste0(LETTERS[3],letters[3], 4), each = 54)
              #                      rep( paste0(LETTERS[3],letters[2], 2), each = 10),
              #                      rep( paste0(LETTERS[3],letters[2], 3), each = 10)
    ),
    score = runif(70,1, 100)# 4 + 12 + 54
)

# cbList <- list(5)
# for (k in 1:5) { cbList[k] <- list(name = paste0(LETTERS[3],letters[2], k)) }
# ccList <- list(2)
# for (k in 1:2) { ccList[k] <- list(name = paste0(LETTERS[3],letters[3], k)) }
# ctList <- list(3)
# for (k in 1:3) { ctList[k] <- list(name = paste0(LETTERS[3],letters[20], k) ) }
# alterList <- list(55)
# for (k in 1:54) { alterList[k] <- list(name = paste0("Site ", k) ) }
# ahpTree <- list(name = "Reporting quality assessment and ranking of OEM sites", 
#             children = list(
#                 list(name = "Believability", children = cbList),
#                 list(name = "Completeness", children = ccList),
#                 list(name = "Timeliness", children = ctList)
#             ))
##########################################
##########################################
##          Tierce Function             ##
##########################################
specify_digits <- function(x, k) {
    format(round(x, k), nsmall = k)
}
makeSaatyListMenu <- function(id, alabel) {
#     selectInput(inputId = id, label = alabel,
#                 choices = c('Equally preferred' = 1,
#                             'Equally to moderately preferred' = 2,
#                             'Moderately preferred' = 3,
#                             'Moderately to strongly preferred' = 4,
#                             'Strongly preferred' = 5,
#                             'Strongly to very strongly preferred' = 6,
#                             'Very strongly preferred' = 7,
#                             'very strongly to Extremely preferred' = 8,
#                             'Extremely preferred' = 9),
#                 multiple = FALSE, selected = 1)
    sliderInput(
        id, alabel, min = 1,max = 9, value = 5, step = 2, ticks = FALSE, post = "x more important"
    )
}
makeCriteriaListMenu <- function(id, alabel, crit1, crit2) {
    selectInput(inputId = id, label = alabel, choices = c(crit1, crit2), multiple = FALSE, selected = crit1)
}

myEigenValue <- function(matx) {
    sqOne <- matx %*% matx
    sqTwo <- sqOne %*% sqOne
    sqMatx <- sqTwo %*% sqTwo
    #
    dimRow <- nrow(matx)
    vMat <- vector(mode = "numeric", length = dimRow)
    for (i in 1:dimRow ) {
        vMat[i] = sum( sqMatx[i,] )
    }
    vMat
}

createCriteriaData <- function(out, colCr1, colCr2) {
    gCr <- complete.cases( out[, c(colCr1,colCr2)] )
    setCr <- out[gCr,]
    setCr
}
###################################
# A dashboard has three parts: a header, a sidebar, and a body.
ui <- shinydashboard::dashboardPage(
    skin = "purple",
    title = "MRQA",

    ## The header content
    shinydashboard::dashboardHeader(
        title = "Maintenance Reporting Quality Assessment (MRQA) dashboard", #Quality Function Deployment
        titleWidth = 600,
        disable = FALSE,
        
        ## Dropdown menus are generated by the dropdownMenu() function. There are three types of menus – messages, notifications, and tasks
        # Dropdown menu for tasks, with progress bar
        shinydashboard::dropdownMenu(
            type = "tasks", badgeStatus = "danger",
            shinydashboard::taskItem(value = 50, color = "aqua",
                                     "Refactor code"),
            shinydashboard::taskItem(value = 20, color = "green",
                                     "Design new layout"),
            shinydashboard::taskItem(value = 60, color = "yellow",
                                     "Another task"),
            shinydashboard::taskItem(value = 90, color = "red",
                                     "Write documentation")
        )
    ),
    
    ## The sidebar content
    shinydashboard::dashboardSidebar(
        #  sidebarMenuOutput("menu")
        shinydashboard::sidebarMenu(
            shinydashboard::menuItem(
                "Site performance", tabName = "dashboard", icon = icon("dashboard")
            ),
#             shinydashboard::menuItem(
#                 "Requirement elicitation", tabName = "reli", icon = icon("th")
#             ),
#             shinydashboard::menuItem(
#                 "Requirement analysis", tabName = "rana", icon = icon("th")
#             ),
#             shinydashboard::menuItem(
#                 "Requirement specifications", tabName = "rspe", icon = icon("th")
#             ),
            shinydashboard::menuItem(
                "Dimension preferences", tabName = "rval", icon = icon("th")
            )#,
#            shinydashboard::menuItemOutput("menuitem")
        )
    ),
    
    ## The body content
    shinydashboard::dashboardBody(
        includeCSS("www/custom.css"),
        includeCSS("www/tree.css"), # tags$head( tags$link(rel = 'stylesheet', type = 'text/css', href = 'tree.css') ),
        #includeScript("gomap.js"),
        # http://docs.mathjax.org/en/latest/output.html
        tags$head(
            tags$script("MathJax.Hub.Config({
            'HTML-CSS': { linebreaks: { automatic: false } },
             SVG: { linebreaks: { automatic: false }, scale: 0.25 }
        });", type = "text/x-mathjax-config")
        ),
        
        # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, b
        shinydashboard::tabItems(
            shinydashboard::tabItem( tabName = "dashboard",
                fluidRow(
                    shinydashboard::box(
                        title = "Map of Company Sites", background = "black", solidHeader = TRUE, # width = 6, height = '500px',
                        collapsible = TRUE, collapsed = FALSE,
                        leaflet::leafletOutput("TheMap")
                    ),
                    uiOutput("siteGraph")
                ),
                fluidRow(
                    column(width = 5, 
                           shinydashboard::box( width = 12,
                                               dateRangeInput('dateRange',
                                                              label = 'Date range input: yyyy-mm-dd', width = '100%',
                                                              start = Sys.Date() - 2, end = Sys.Date() + 2
                                               )
                                               #verbatimTextOutput("dateRangeText")
                           ),
                           uiOutput("criteriaChart")
                           ),
                    column(width = 7,
                           shinydashboard::box( width = 12,  height = '540px',
                                                title = "Disintegrated Quality View", solidHeader = TRUE, #  height = '500px', status = "primary",
                                                collapsible = TRUE,
                                                column(4, chooserInput("mychooser", "Available frobs", "Selected frobs", data54Sites[,c("SITES")], c(), size = 10, multiple = TRUE),
                                                       #verbatimTextOutput("selection")
                                                       selectInput(inputId = "listButton", label = "Criteria Refinement",
                                                                   choices = c('None',
                                                                               'Refinement Believability',
                                                                               'Refinement Completness',
                                                                               'Refinement Timeliness'),
                                                                   multiple = FALSE, selected = 'None'),
                                                       checkboxGroupInput('show_vars', 'Variables to show:', thename, selected = thename, inline = FALSE)
                                                ),
                                                column(8,  
                                                       plotOutput("radar4", width = "100%", height = '475px')
                                                       #rCharts::showOutput("highRadar", "highcharts")  #height = "1200px"
                                                       #                                fluidRow(
                                                       #                                    column(6, ),
                                                       #                                    column(6,  actionButton("goButton", "Refinement Beliavility") )
                                                       #                                )
                                                )
                           )
                    )
                )
            ),
            shinydashboard::tabItem( tabName = "reli",
                                    tabsetPanel(
                                        tabPanel( "Plot",
                                                  # http://rich-iannone.github.io/DiagrammeR/graphviz.html
                                                  DiagrammeR::grViz( diagram = "digraph AHP_TREE {
                                                        
                                                        # graph [rankdir = LR] # With DOT
                                                        
                                                        # several 'node' statements
                                                        node [shape = egg, style=filled, fontname = Helvetica, penwidth=3.0, fillcolor=yellow, color=red]
                                                        a [label='@@1']
                                                        
                                                        node [shape=square, color=pink]
                                                        b [label='@@2-1']
                                                        c [label='@@2-2']
                                                        d [label='@@2-3']
                                                        
                                                        node [shape=circle, color=blue]
                                                        
                                                        # several 'edge' statements
                                                        edge [color = grey, arrowhead = none, arrowtail = none]
                                                        a -> {b c d}
                                                        b -> {'@@3-1', '@@3-2', '@@3-3'}
                                                        c -> {'@@3-4', '@@3-5', '@@3-6','@@3-7','@@3-8','@@3-9'}
                                                        d -> {'@@3-10', '@@3-11', '@@3-12','@@3-13','@@3-14','@@3-15'}
                                                        }
                                                        [1]: 'Goal'
                                                        [2]: paste0('Criteria:\\n', 1:3)
                                                        [3]: 1:15
                                                        ", engine = "dot"
                                                  )
                                        ),
                                        tabPanel("Summary", verbatimTextOutput("summary")),
                                        tabPanel("Tree",
                                                 tags$div(class = "tree",
                                                          tags$ul(tags$li(
                                                              tags$a(href = "#", "Parent"),
                                                              tags$ul(tags$li(
                                                                  tags$a(href = "#", "Child"),
                                                                  tags$ul(tags$li(tags$a(href = "#", "Grand Child")))
                                                              ),
                                                              tags$li(
                                                                  tags$a(href = "#", "Child"),
                                                                  tags$ul(tags$li(tags$a(href = "#", "Grand Child")),
                                                                          tags$li(tags$a(href = "#", "Grand Child")),
                                                                          tags$li(tags$a(href = "#", "Grand Child")))
                                                              ))
                                                          ))),
                                                 shinydashboard::box(
                                                     radioButtons(
                                                         "label_button", "label", c("1","3","5","7","9") ,selected = NULL, inline = TRUE
                                                     ),
                                                     radioButtons(
                                                         "dist", "Distribution type:",c("Normal" = "norm", "Uniform" = "unif", "Log-normal" = "lnorm", "Exponential" = "exp")
                                                     ),
                                                     sliderInput(
                                                         "sliderInput", "label",min = 1,max = 9, value = 1, step = 2, ticks = FALSE
                                                     ),
                                                     sliderInput(
                                                         "dblclick_delay", "Delay", min = 100, max = 1000, value = 400,step = 100
                                                     )
                                                 )
                                        ) # Tab panel
                                    )),
            # The basic building block of most dashboards is a box. Boxes in turn can contain any content.
            shinydashboard::tabItem( tabName = "rval",
                                     fluidRow(
                                         #                                         shinydashboard::box(
                                         #                                                 title = "Scatter", background = "maroon", solidHeader = TRUE,
                                         #                                                 verbatimTextOutput("boxtext4"),
                                         #                                                 plotOutput("plothist4", height = 250)
                                         #                                         ),
                                         #                                         shinydashboard::box(
                                         #                                                 title = "Inputs", background = "black",
                                         #                                                 "Box content here", br(), "More box content",
                                         #                                                 sliderInput(
                                         #                                                         "boxslider4", "Number of points:", min = 10, max = 200, value = 50, step = 10
                                         #                                                 ),
                                         #                                                 
                                         #                                                 # With the conditionalPanel, the condition is a JavaScript
                                         #                                                 # expression. In these expressions, input values like
                                         #                                                 # input$n are accessed with dots, as in input.n
                                         #                                                 conditionalPanel("input.boxslider4 >= 50", textInput("boxtext4", "Text input:"))
                                         shinydashboard::box( width = 12,
                                                              #column(1,
                                                                     #radioButtons(
                                                                     #"label_button", "label", c('Equally preferred',
                                                                     #                          'Equally to moderately preferred',
                                                                     #                         'Moderately preferred',
                                                                     #                         'Moderately to strongly preferred',
                                                                     #                            'Strongly preferred',
                                                                     #                         'Strongly to very strongly preferred',
                                                                     #                              'Very strongly preferred',
                                                                     #                    'very strongly to Extremely preferred',
                                                                     #           'Extremely preferred'), selected = NULL, inline = FALSE
                                                                     #),
                                                                     #sliderInput(
                                                                     # inputId = "sliderInput", label = "label", min = 1,max = 9, value = 1, step = 1, ticks = FALSE
                                                                     #),
                                                              #),
                                                              #column(11, 
                                                                     #tabPanel( "AHP tree", htmlOutput("gtree"))
                                                            widgetHierarchyOutput("hier", height = '600px'),
                                                            selectInput( inputId = "treeLevelChoice", 
                                                                         label = "Choose level to compute pairwise comparison",
                                                                         choices = c('Level 0 - root' = 0, 'Level 1' = 1, 'Level 2' = 2),
                                                                         multiple = FALSE, selected = 0)
                                                              #)
                                         ),
                                         uiOutput("listMenuBoxMatrix")
                                       
                                     )
                                     # fluidPage( theme = "mytheme.css", ... )
        ),
        shinydashboard::tabItem( tabName = "sofspe",
            fluidRow(
                shinydashboard::tabBox(
                # Title can include an icon
                title = tagList(shiny::icon("gear"), "tabBox status"),
                tabPanel(
                    "Tab1",
                    "Currently selected tab from first box:",
                    verbatimTextOutput("tabset1Selected")
                ),
                tabPanel("Tab2", "Tab content 2")
            )),
            fluidRow(
                shinydashboard::infoBoxOutput("approvalBoxI"),
                shinydashboard::infoBoxOutput("approvalBox"),
                shinydashboard::valueBoxOutput("approvalBoxV")
                
            ),
            fluidRow(# Clicking this will increment the progress amount
                shinydashboard::box(width = 4, actionButton("count", "Increment progress"))
            )
        )
    ) # Tab items
    ) # End of body
) # End of dashboard

##########################################
##                Server                ##
##########################################
server <- function(input, output) {
    
    # dynamically generate individual items
    output$menuitem <- shinydashboard::renderMenu({
        shinydashboard::menuItem(
            "Software specifications", tabName = "sofspe", icon = icon("th"), badgeLabel = "new", badgeColor = "green"
        )
    })
    # The currently selected tab from the first box
    output$tabset1Selected <- renderText({
        input$tabset1
    })
    ## http://rstudio.github.io/shinydashboard/structure.html#infobox
    output$approvalBoxI <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
            "Approval",  paste0(25 + input$count, "%"), icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
    })
    output$approvalBox <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
            "Approval", "0%", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow", fill =  TRUE
        )
    })
    output$approvalBoxV <- shinydashboard::renderInfoBox({
        shinydashboard::valueBox(
            paste0(25 + input$count, "%"), "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    ## OUTPUT
    output$boxtext4 <-
        renderPrint({
            paste0("print This here: ", input$boxtext4)
        })
    output$plothist4 <- renderPlot({
        x <- rnorm(input$boxslider4)
        y <- rnorm(input$boxslider4)
        plot(x, y)
    })
    
    output$siteGraph <- renderUI({
        #input$MAPID_OBJCATEGORY_EVENTNAME
        # Valid values for OBJCATEGORY above are marker, popup, shape, geojson, and topojson. (Tiles and controls don’t raise mouse events.) Valid values for EVENTNAME are click, mouseover, and mouseout.
        if (is.null(input$TheMap_marker_click))
            return()
        events <- input$TheMap_marker_click
        ## events are set to either NULL if the event has never happened, or a list() that includes:
        #lat - The latitude of the object, if available; otherwise, the mouse cursor
        #lng - The longitude of the object, if available; otherwise, the mouse cursor
        #id - The layerId, if any
        
        if (events$lat != 0 && events$lng != 0) {
            lt <- sapply(data54Sites$Latitude, function(x) { identical( TRUE, all.equal(x, events$lat, tolerance = 0.0005))} )
            lg <- sapply(data54Sites$Longitude, function(x){ identical( TRUE, all.equal(x, events$lng, tolerance = 0.0005))} )
            #val <- data54Sites[ lt & lg == events$lng, c("VALUES")]
            theline <- data54Sites[lt & lg,]
            if (nrow(theline) > 0) {
                n <-
                    as.numeric(rownames(theline)) * 1.185 # Space between barplot??
                thesiteX <- c(n, n)
                thesiteY = theline[, c("TOT")]
            } else {
                # No problem of variable scope / visibility
                thesiteX <- c(0, 0)
                thesiteY = 1
            }
        }
        
        output$plothist <- renderPlot({
            barplot(
                data54Sites[,c("TOT")], main = "Site Distribution", horiz = FALSE, 
                names.arg = data54Sites[,c("SITES")], cex.names = 1, axis.lty = 1, las = 2
            )
            lines(thesiteX, c(0, thesiteY), col = "red",lwd = 5)
        })
        
        output$data_table <- renderTable({
            if (!input$visibleTable)
                return()
            # Make sure columns are correct for data set (when data set changes, the
            # columns will initially be for the previous data set)
            #if ( !(input$show_vars %in% thename) )  return()
            # Keep the selected columns
            if (length(input$mychooser$right) > 0) {
                outSelection <- subset( data54Sites, data54Sites$SITES %in% input$mychooser$right)
            }
            outSelection[, c("SITES", input$show_vars), drop = FALSE]
        })
        
        ## INPUT
        shinydashboard::box(
            title = "Final Assessment & Ranking", background = "black", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = FALSE,
            plotOutput("plothist", height = '320px'),#
            checkboxInput("visibleTable", "Show data table", value =  FALSE),
            # XXX TODO UNUSED XXX
            checkboxInput("visibleEvo", "Evolution over time", value =  FALSE),
            ###
            tableOutput("data_table")
        )
    })
    
#    observeEvent(input$goButton, { })
    
    output$dateRangeText  <- renderText({
        # http://shiny.rstudio.com/gallery/date-and-date-range.html
        paste0("Period is ", paste(as.character(input$dateRange), collapse = " to ")
        )
    })
    
    output$highRadar <- rCharts::renderChart({
        if (length(input$mychooser$right) > 0) {
            outSelection <- subset(data54Sites, data54Sites$SITES %in% input$mychooser$right)
        }
        maxmin <- data.frame(
            Believability = c(54, 1), 
            Completeness = c(54, 1), 
            Timeliness = c(54, 1)#, Environmental = c(54, 1)
        )
        outSelected <- rbind(maxmin[, input$show_vars], outSelection[, input$show_vars, drop = FALSE])
         h1 <- Highcharts$new()
         h1$chart(type = "line", polar = TRUE)
#         h1$set(tooltip = list(
#             crosshairs =  TRUE,
#             shared = TRUE,
#             valueSuffix =  '%%%'
#         ))
#        h1$series(data = c(43000, 19000, 60000, 35000, 17000, 10000), dashStyle = "longdash", pointPlacement = 'on', name = 'Allocated Budget')
            h1$series(
#                data = jsonlite::toJSON(outSelected),
                data = as.numeric(outSelected[7, ]), dashStyle = "shortdot", 
                pointPlacement = 'on',
                name = as.character(outSelection[7, c("SITES")])
                )
        h1$xAxis( categories = input$show_vars, tickmarkPlacement = 'on', lineWidth = 0)
        h1$yAxis( gridLineInterpolation = 'polygon', lineWidth = 0, min = 0)
        h1$tooltip( shared = TRUE, pointFormat = '<span style="color:{series.color}">{series.name}: <b>${point.y:,.0f}</b><br/>' )
        #
        h1$legend( align = 'right', verticalAlign = 'top', y = 70, layout = 'vertical' )
        #  This is required so that both server.R and ui.R are communicating about the correct chart.
        h1$addParams(dom = 'highRadar')
        return(h1)
    })
    
    output$radar4 <- renderPlot({
        if (length(input$mychooser$right) > 0) {
            outSelection <- subset(data54Sites, data54Sites$SITES %in% input$mychooser$right)
        }
        maxmin <- data.frame(
            Believability = c(1, 54), 
            Completeness = c(1, 54), 
            Timeliness = c(1, 54)#, Environmental = c(54, 1)
        )
        outSelected <- rbind(maxmin[, input$show_vars], outSelection[, input$show_vars, drop = FALSE])
        
        fmsb::radarchart(# outSelection[, input$show_vars, drop = FALSE]
            outSelected, vlabels = input$show_vars, # pdensity=c(5, 10, 30),pdensity=1, pfcol=topo.colors(54), pcol=topo.colors(54)
            axistype = 2, seg = 5, maxmin = TRUE, centerzero = TRUE, axislabcol = "red", plwd = 2,
            caxislabels = c("worst", "", "", "", "best"), na.itp = FALSE, title = "Performance chart"
        )
        #axistype = 4, seg = 3, cglty = 3, pty = 32, cglcol = 1, plwd = 3, pcol = 1, axislabcol = 1, labels = seq(from = 1, to = 54, length = 4) )
        #             axistype=1, seg= 5, plty=1, vlabels= input$show_vars, title="(axis=1, 3 segments, with specified vlabels)")
        #             axistype=2, pcol=topo.colors(3), plty=1, vlabels= input$show_vars, pdensity=c(5, 10, 30), pangle=c(10, 45, 120), pfcol=topo.colors(3), title="(topo.colors, fill, axis=2)")
        #             axistype=1, plwd=1:5, pcol=1, centerzero=TRUE, seg= length( input$show_vars ), caxislabels=c("worst", "", "", "", "best"), title="(use lty and lwd but b/w, axis=1,\n centerzero=TRUE, with centerlabels)")
        if (length(input$mychooser$right) > 0) {
            # 1200px / coord: -1.3,-1.1 / ncol=4
            # no px/ coord: -3.38, 1.29 / ncol=2
            legend(
                -1.7,-0.7, legend = unique(input$mychooser$right), title = "SITES", col = 1:8, lty =
                    1:6 , y.intersp = 1,text.width = 0.5, ncol = 4, horiz = FALSE
            )
        } else {
            legend(
                "bottom", legend = unique(input$mychooser$left), title = "SITES", col = 1:8, lty =
                    1:6, y.intersp = 1,text.width = 0.5, ncol = 7, horiz = FALSE
            )
        }
    })
    
    output$criteriaChart <- renderUI({
        if ( input$listButton != "None") 
        {
            ## INPUT
            shinydashboard::box(width = 12,# height = '500px',
                                rCharts::showOutput("chartN", lib = "nvd3", package = "rCharts")
                                # Disintegrated Quality View - level 3
            )
        }
    })
    
    #session$sendCustomMessage(type = 'testmessage', message = 'Thank you for clicking') 
    output$chartN <- rCharts::renderChart({
        # https://github.com/novus/nvd3/blob/master/src/models/multiBarChart.js
        # http://nvd3-community.github.io/nvd3/examples/documentation.html#multiBarChart 
        require(rCharts)
        #options(RCHART_WIDTH = 800)
      
        if (length(input$mychooser$right) > 0) {
            outSelection <- subset(data54Sites, data54Sites$SITES %in% input$mychooser$right)
        }
        mydata <- NULL
        if ( input$listButton == "None") {
            return;
        } else if ( input$listButton == "Refinement Believability")  
            {
            gCB1 <- complete.cases( outSelection[, c("CB1","X.10")] )
            gCB2 <- complete.cases( outSelection[, c("CB2","X.8")] )
            gCB3 <- complete.cases( outSelection[, c("CB3","X.9")] )
            setCB1 <- outSelection[gCB1,]
            setCB2 <- outSelection[gCB2,]
            setCB3 <- outSelection[gCB3,]
            valCB1 <- as.numeric(as.character(setCB1[, c("X.10")])) / setCB1[, c("CB1")]*100
            valCB2 <- as.numeric(as.character(setCB2[, c("X.8")])) / setCB2[, c("CB2")]*100
            valCB3 <- as.numeric(as.character(setCB3[, c("X.9")])) / setCB3[, c("CB3")]*100
            
            mydata <- rbind(
                data.frame( 
                    SITES = setCB1[, c("SITES")],
                    name = c( rep( paste0(LETTERS[3],letters[2], 1), each = length(valCB1)) ),
                    score =  valCB1
                ),
                data.frame( 
                    SITES = setCB2[, c("SITES")],
                    name = c( rep( paste0(LETTERS[3],letters[2], 2), each = length(valCB2)) ),
                    score =  valCB2
                ),
                data.frame( 
                    SITES = setCB3[, c("SITES")],
                    name = c( rep( paste0(LETTERS[3],letters[2], 3), each = length(valCB3)) ),
                    score =  valCB3
                )
            )
            #mydata[ mydata$SITES == "'Site 6'" && mydata$name == "Cb2", c("score")] <- 100
            #mydata[ mydata$SITES == "'Site 54'" && mydata$name == "Cb1", c("score")] <- 50
        } else if ( input$listButton == "Refinement Completness") 
            {
            setCC1 <- createCriteriaData(outSelection, "CC1","X")
            setCC2 <- createCriteriaData(outSelection,"CC2","X.1")
            setCC3 <- createCriteriaData(outSelection,"CC3","X.2")
            setCC4 <- createCriteriaData(outSelection,"CC4","X.3")
            setCC5 <- createCriteriaData(outSelection,"CC5","X.4")
            setCC6 <- createCriteriaData(outSelection,"CC6","X.5")
            setCC7 <- createCriteriaData(outSelection,"CC7","X.6")
            setCC8 <- createCriteriaData(outSelection,"CC8","X.7")
            
            valCC1 <- as.numeric(as.character(setCC1[, c("X")])) / setCC1[, c("CC1")]*100
            valCC2 <- as.numeric(as.character(setCC2[, c("X.1")])) / setCC2[, c("CC2")]*100
            valCC3 <- as.numeric(as.character(setCC3[, c("X.2")])) / setCC3[, c("CC3")]*100
            valCC4 <- as.numeric(as.character(setCC4[, c("X.3")])) / setCC4[, c("CC4")]*100
            valCC5 <- as.numeric(as.character(setCC5[, c("X.4")])) / setCC5[, c("CC5")]*100
            valCC6 <- as.numeric(as.character(setCC6[, c("X.5")])) / setCC6[, c("CC6")]*100
            valCC7 <- as.numeric(as.character(setCC7[, c("X.6")])) / setCC7[, c("CC7")]*100
            valCC8 <- as.numeric(as.character(setCC8[, c("X.7")])) / setCC8[, c("CC8")]*100
            
            mydata <- rbind(
                data.frame( 
                    SITES = setCC1[, c("SITES")],
                    name = c( rep( paste0(LETTERS[3],letters[3], 1), each = length(valCC1)) ),
                    score =  valCC1
                ),
                data.frame( 
                    SITES = setCC2[, c("SITES")],
                    name = c( rep( paste0(LETTERS[3],letters[3], 2), each = length(valCC2)) ),
                    score =  valCC2
                ),
                data.frame( 
                    SITES = setCC3[, c("SITES")],
                    name = c( rep( paste0(LETTERS[3],letters[3], 3), each = length(valCC3)) ),
                    score =  valCC3
                ),
                data.frame( 
                    SITES = setCC4[, c("SITES")],
                    name = c( rep( paste0(LETTERS[3],letters[3], 4), each = length(valCC4)) ),
                    score =  valCC4
                ),
                data.frame( 
                    SITES = setCC5[, c("SITES")],
                    name = c( rep( paste0(LETTERS[3],letters[3], 5), each = length(valCC5)) ),
                    score =  valCC5
                ),
                data.frame( 
                    SITES = setCC6[, c("SITES")],
                    name = c( rep( paste0(LETTERS[3],letters[3], 6), each = length(valCC6)) ),
                    score =  valCC6
                ),
                data.frame( 
                    SITES = setCC7[, c("SITES")],
                    name = c( rep( paste0(LETTERS[3],letters[3], 7), each = length(valCC7)) ),
                    score =  valCC7
                ),
                data.frame( 
                    SITES = setCC8[, c("SITES")],
                    name = c( rep( paste0(LETTERS[3],letters[3], 8), each = length(valCC8)) ),
                    score =  valCC8
                )
            )
#             mydata <- rbind( mydata, data.frame( 
#                 SITES = rep("'Site 6'", each = 8), 
#                 name = paste0(LETTERS[3],letters[3], 1:8), 
#                 score =  c(14,19,33,8,2,55,22,14)
#             ))
#             mydata <- rbind( mydata, data.frame( 
#                 SITES = rep("'Site 54'", each = 5), 
#                 name = paste0(LETTERS[3],letters[3], c(3:5, 7:8)), 
#                 score =  c(52,20,18,28,29)
#             ))
#            mydata[ mydata$SITES == "'Site 54'" & mydata$name == "Cc1", c("score")] <- 25
#            mydata[ mydata$SITES == "'Site 54'" & mydata$name == "Cc2", c("score")] <- 28
#            mydata[ mydata$SITES == "'Site 54'" & mydata$name == "Cc6", c("score")] <- 45
        } else if ( input$listButton == "Refinement Timeliness") {
            return;
        }
        theplot <- nPlot( score ~ name, group = "SITES", data = mydata, type = 'multiBarChart', width = 550) # multiBarChart, multiBarHorizontalChart, 'scatterChart', 'pieChart'
        theplot$chart( showLegend = TRUE, reduceXTicks = FALSE, rotateLabels = -45, rightAlignYAxis = FALSE, 
                       tooltip = TRUE, tooltipContent = paste0("#! function(key, x, y) { return ", "key"," } !#"))
        theplot$chart(color = c('orange', 'blue', '#594c26', 'green'))
        
        #            theplot$xAxis(axisLabel = 'all Criteria values for selected sites')
        theplot$addParams(dom = 'chartN')
        return(theplot)
    })
    
    ## http://rstudio.github.io/leaflet/basemaps.html
    output$TheMap <- leaflet::renderLeaflet({
        require(leaflet)
        #pal <- colorQuantile("YlOrRd", NULL, n = 8)
        # create a vector of Longitude coordinates Long <- 10 + rnorm(n = 10, mean = 3, sd = 6)
        # create a vector of Latitude coordinates Lat <- 40 + rnorm(n = 10, mean = 3, sd = 6)
        
        leaflet() %>% 
            setView(24.827741, 60.186779, zoom = 10) %>%
            #
            #addCircles(Long, Lat, color='red' ) %>% clusterOptions=markerClusterOptions()
            addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = 10, color = c('red'), fill = TRUE, fillColor = c('blue'), fillOpacity = 0.2, data = data54Sites[,c('TOT', "Latitude", "Longitude")]) %>%
            addMarkers(lng = 24.827741, lat = 60.186779, popup = "<span style='color:black;'>Aalto</span>") %>%
#            addMarkers(lng = 6.168843, lat = 49.632338,  popup = "<span style='color:black;'>SnT</span>") %>%
            addMarkers(lng = 174.768, lat = -36.852, popup = "<span style='color:black;font-family:courier;'>The birthplace of R</span>") %>%
            
            addTiles() %>% # Add default OpenStreetMap map tiles
            #      addProviderTiles("Acetate.terrain") %>%
            addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE))
    })
    
    output$listMenuBoxMatrix <- renderUI({
        if ( identical(TRUE, all.equal(input$treeLevelChoice, 0, tolerance = 0)) ) {
        } else if ( input$treeLevelChoice == 1 ) {
            ## INPUT BOX
            shinydashboard::box(width = 12,
                                checkboxInput("visibleMTX", "Check the box to see the comparison matrix", value =  FALSE),
                                column(2,
                                       makeCriteriaListMenu("BandC", "Criteria importance","Believability over Completeness", "Completeness over Believability"),
                                       makeCriteriaListMenu("BandT", "Criteria importance","Believability over Timeliness", "Timeliness over Believability"),
                                       makeCriteriaListMenu("CandT", "Criteria importance","Timeliness over Completeness", "Completeness over Timeliness")
                                ),
                                column(4,
                                       makeSaatyListMenu("BoverC", " "),
                                       makeSaatyListMenu("BoverT", " "),
                                       makeSaatyListMenu("CoverT", " ")
                                ),
                                column(6, uiOutput("matx"))
            )
        } else if ( input$treeLevelChoice == 2 ) {
            ## INPUT BOX
            shinydashboard::box(width = 12,
                                checkboxInput("visibleMTX", "Check the box to see the comparison matrix", value =  FALSE),
                                column(2,
                                       makeSaatyListMenu("cb1overcb2", "Cb1 over Cb2"),
                                       makeSaatyListMenu("cb1overcb3", "Cb1 over Cb3"),
                                       makeSaatyListMenu("cb1overcb4", "Cb1 over Cb4"),
                                       makeSaatyListMenu("cb1overcb5", "Cb1 over Cb5")
                                ),
                                column(2,
                                       makeSaatyListMenu("cb2overcb3", "Cb2 over Cb3"),
                                       makeSaatyListMenu("cb2overcb4", "Cb2 over Cb4"),
                                       makeSaatyListMenu("cb2overcb5", "Cb2 over Cb5")
                                ),
                                column(2,
                                       makeSaatyListMenu("cb3overcb4", "Cb3 over Cb4"),
                                       makeSaatyListMenu("cb3overcb5", "Cb3 over Cb5"),
                                       makeSaatyListMenu("cb4overcb5", "Cb4 over Cb5")
                                ),
                                column(6, uiOutput("matx"))
            )
        }
    })
    
    output$matx <- renderUI({
        if (!input$visibleMTX) { 
            return()
        }
#         M <- matrix(rep(1,6),nrow=3)
#         rownames(M) <- c('a','b','c')
#         M <- print( xtable::xtable(M, align = rep("c", ncol(M)+1)), floating = FALSE, tabular.environment = "array", comment = FALSE, print.results = FALSE)
#         html <- paste0("$$", M, "$$")
#         list( withMathJax(HTML(html)) )
        intMatx <- matrix( c(1, 0, 0, 
                             0, 1, 0,
                             0, 0, 1), nrow = 3, ncol = 3, byrow = FALSE)
        matrixRepresentation <- NULL
        if ( identical(TRUE, all.equal(input$treeLevelChoice, 0, tolerance = 0)) ) {
        } else if ( input$treeLevelChoice == 1 ) {
            if ( input$BandC == "Believability over Completeness") {
                intMatx[1, 2] <- as.numeric(input$BoverC)
                intMatx[2, 1] <- (1/(as.numeric(input$BoverC)))
            } else if ( input$BandC == "Completeness over Believability") {
                intMatx[2, 1] <- as.numeric(input$BoverC)
                intMatx[1, 2] <- (1/(as.numeric(input$BoverC)))
            }
            if ( input$BandT == "Believability over Timeliness") {
                intMatx[1, 3] <- as.numeric(input$BoverT)
                intMatx[3, 1] <- (1/(as.numeric(input$BoverT)))
            } else if ( input$BandT == "Timeliness over Believability") {
                intMatx[3, 1] <- as.numeric(input$BoverT)
                intMatx[1, 3] <- (1/(as.numeric(input$BoverT)))
            }
            if ( input$CandT == "Completeness over Timeliness") {
                intMatx[2, 3] <- as.numeric(input$CoverT)
                intMatx[3, 2] <- (1/(as.numeric(input$CoverT)))
            } else if ( input$CandT == "Timeliness over Completeness") {
                intMatx[3, 2] <- as.numeric(input$CoverT)
                intMatx[2, 3] <- (1/(as.numeric(input$CoverT)))
            }
            vMat <- myEigenValue(intMatx)
            vMatSum <- sum(vMat)
            ahppmr <- pmr::ahp(dset = intMatx, sim_size = 500)
            
            
            matrixRepresentation <- paste0("$$ \\begin{matrix}
        ", "  ", " & ", "\\text{Believability}", " & ", "\\text{Completeness}"," & ", "\\text{Timeliness} \\\\
        ", "\\text{Believability}", " & ", 1, " & ", MASS::fractions(intMatx[1, 2])," & ", MASS::fractions(intMatx[1, 3]), " \\\\
        ", "\\text{Completeness}", " & ", MASS::fractions(intMatx[2, 1]), " & ", 1," & ", MASS::fractions(intMatx[2, 3]), " \\\\
        ", "\\text{Timeliness}", " & ", MASS::fractions(intMatx[3, 1]), " & ", MASS::fractions(intMatx[3, 2])," & ", 1 , " \\\\
        \\end{matrix} ")
        
        withMathJax( helpText("The pairwise comparison matrix:"), 
                     matrixRepresentation,
                     paste0(" \\Longrightarrow \\begin{bmatrix} 
        ", specify_digits((vMat[1]/vMatSum), 3)," \\\\
        ", specify_digits((vMat[2]/vMatSum), 3)," \\\\
        ", specify_digits((vMat[3]/vMatSum), 3)," \\\\
        \\hline
        ", ((vMat[1]/vMatSum) + (vMat[2]/vMatSum) + (vMat[3]/vMatSum))," \\\\
        \\end{bmatrix} 
        or  \\begin{bmatrix}
        ", specify_digits((ahppmr$weighting[1]), 3)," \\\\
        ", specify_digits((ahppmr$weighting[2]), 3)," \\\\
        ", specify_digits((ahppmr$weighting[3]), 3)," \\\\
        \\hline
        ", sum(ahppmr$weighting)," \\\\
        \\end{bmatrix} \\\\ $$"),
            paste0(" \\begin{array}{c} 
                        \\text{Saaty's inconsistency} = ", specify_digits((ahppmr$Saaty), 3)," \\\\ ",
                        "\\text{Koczkodaj's inconsistency} = ", specify_digits((ahppmr$Koczkodaj), 3),
                   " \\end{array}")
        )
        
        } else if ( input$treeLevelChoice == 2 ) {
            # http://www.onemathematicalcat.org/MathJaxDocumentation/TeXSyntax.htm
            intMatx <- matrix(c(1, as.numeric(input$cb1overcb2),  as.numeric(input$cb1overcb3),  as.numeric(input$cb1overcb4),  as.numeric(input$cb1overcb5),
                                (1/(as.numeric(input$cb1overcb2))), 1, as.numeric(input$cb2overcb3), as.numeric(input$cb2overcb4), as.numeric(input$cb2overcb5),
                                (1/(as.numeric(input$cb1overcb3))), (1/(as.numeric(input$cb2overcb3))), 1, as.numeric(input$cb3overcb4), as.numeric(input$cb3overcb5),
                                (1/(as.numeric(input$cb1overcb4))), (1/(as.numeric(input$cb2overcb4))), (1/(as.numeric(input$cb3overcb4))), 1, as.numeric(input$cb4overcb5),
                                (1/(as.numeric(input$cb1overcb5))), (1/(as.numeric(input$cb2overcb5))), (1/(as.numeric(input$cb3overcb5))), (1/(as.numeric(input$cb4overcb5))), 1), 
                              nrow = 5, ncol = 5, byrow = FALSE)
            ##
            vMat <- myEigenValue(intMatx)
            vMatSum <- sum(vMat)
            ahppmr <- pmr::ahp(dset = intMatx, sim_size = 500)
            withMathJax( helpText("The pairwise comparison matrix:"), 
                         paste0("$$ \\begin{matrix}
        ", "  ", " & ", "C_{b1}", " & ", "C_{b2}"," & ", "C_{b3}"," & ", "C_{b4}", " & ", "C_{b5} \\\\
        ", "C_{b1}", " & ", 1, " & ", input$cb1overcb2," & ", input$cb1overcb3," & ", input$cb1overcb4," & ", input$cb1overcb5," \\\\
        ", "C_{b2}", " & \\frac{1}{", input$cb1overcb2, "} & ", 1," & ", input$cb2overcb3," & ", input$cb2overcb4," & ", input$cb2overcb5," \\\\
        ", "C_{b3}", " & \\frac{1}{", input$cb1overcb3, "} & \\frac{1}{", input$cb2overcb3,"} & ", 1 ," & ", input$cb3overcb4," & ", input$cb3overcb5," \\\\
        ", "C_{b4}", " & \\frac{1}{", input$cb1overcb4, "} & \\frac{1}{", input$cb2overcb4,"} & \\frac{1}{", input$cb3overcb4,"} & ", 1 ," & ", input$cb4overcb5," \\\\
        ", "C_{b5}", " & \\frac{1}{", input$cb1overcb5, "} & \\frac{1}{", input$cb2overcb5,"} & \\frac{1}{", input$cb3overcb5,"} & \\frac{1}{", input$cb4overcb5,"} & ", 1,"\\\\
        \\end{matrix}  \\Longrightarrow \\begin{bmatrix} 
        ", (vMat[1]/vMatSum)," \\\\
        ", (vMat[2]/vMatSum)," \\\\
        ", (vMat[3]/vMatSum)," \\\\
        ", (vMat[4]/vMatSum)," \\\\
        ", (vMat[5]/vMatSum)," \\\\ 
        \\hline
        ", ((vMat[1]/vMatSum) + (vMat[2]/vMatSum) + (vMat[3]/vMatSum) + (vMat[4]/vMatSum) + (vMat[5]/vMatSum))," \\\\
        \\end{bmatrix} 
        or  \\begin{bmatrix}
        ", (ahppmr$weighting[1])," \\\\
        ", (ahppmr$weighting[2])," \\\\
        ", (ahppmr$weighting[3])," \\\\
        ", (ahppmr$weighting[4])," \\\\
        ", (ahppmr$weighting[5])," \\\\ 
        \\hline
        ", sum(ahppmr$weighting)," \\\\
        \\end{bmatrix} \\\\ $$"),
                         paste0("Saaty's inconsistency = ", (ahppmr$Saaty)," ; "),
                         paste0("Koczkodaj's inconsistency = ",(ahppmr$Koczkodaj) )
            )
        }
    })
    
    output$hier <- renderWidgetHierarchy({
        widgetHierarchy(theTree, boxHeight = 60, boxWidth = 130, tx = 700, ty = 0, angle = 0, scale = 0.75)
    })
    
    ##########################################
    ##               GoogleVis              ##
    ##########################################
#     output$gtree <- googleVis::renderGvis({
#         require(googleVis)
#         gvisOrgChart(theTree, idvar = "Node", parentvar = "Link", tipvar = "Val",
#                      options = list(nodeClass = "nodeContainer",#  selectedNodeClass width=600, height=250,
#                                   size = 'large', allowHtml = TRUE, allowCollapse = TRUE)) # gvis.editor='edit Me!'
#     })
    
}

shinyApp(ui = ui, server = server)