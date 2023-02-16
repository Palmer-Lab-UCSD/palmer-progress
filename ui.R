# define the layout/UI elements of the app

ui <- secure_app(
  dashboardPage(
    # non page-body ----
    
    # at the top of the page; always visible, even when sidebar is collapsed
    dashboardHeader(title = "Palmer Lab"),
    
    # collapsible sidebar with options to leave the current page
    dashboardSidebar(
      # CSS styling for the sidebar items
      tags$style(HTML(".sidebar-menu li a { font-size: 14pt; }")),
      sidebarMenu(
        # other tabs within this app
        menuItem("Progress Log and Tables", tabName = "1st", 
                 icon = icon("user")),
        menuItem("Interactive Phenotypes", tabName = "2nd", 
                 icon = icon("user")),
        menuItem("Available Data Tables", tabName = "3rd", icon = icon("user")),
        menuItem("Genotyping Report", tabName = "4th", icon = icon("user")),
        # external links
        menuItem("Check out the Palmer Lab here", icon = icon("file-code-o"), 
                 href = "https://palmerlab.org/"),
        menuItem("Return to ratgenes.org here", icon = icon("file-code-o"), 
                 href = "https://ratgenes.org/")
      ),
      # the sidebar is 300 pixels wide, and starts collapsed
      width = 300, collapsed = TRUE
    ),
    
    # main page body layout
    dashboardBody(
      # these pages are linked in the sidebar by their tabName
      tabItems(
        # default page ----
        tabItem(tabName = "1st",
                fluidPage(
                  tags$head(tags$style(
                    HTML("
                    #by_project_active, #by_project_complete, #by_animal, 
                    #active_caption, #complete_caption, #animal_caption 
                      {font-size: 20px}
                    #active_caption, #complete_caption, #animal_caption 
                      {color: #777;}
                         ")
                  )),
                  # page title
                  titlePanel("Checklist by project_name"),
                  # sub-panels within the page
                  tabsetPanel(
                    # set up tab-controlled sub-panels, defaulting to table one
                    id = "Progress Tables", type = "tabs",
                    # this sub-panel has the tables in it
                    tabPanel("Progress Tables",
                             fluidRow(
                               # offer projects to select from for data analysis
                               column(width = 6, uiOutput("projects_selector")),
                               column(width = 6, uiOutput("animal_selector"))
                             ),
                             "* cells manually updated in the database", br(),
                             # display data tables
                             textOutput("active_caption"),
                             dataTableOutput("by_project_active"),
                             textOutput("complete_caption"),
                             dataTableOutput("by_project_complete"),
                             textOutput("animal_caption"),
                             dataTableOutput("by_animal")
                    ),
                    # this sub-panel has bar-plots in it
                    tabPanel("Progress Barplots",
                             # download plot
                             fluidRow(
                               column(width = 2, 
                                      downloadButton("download", "Download")),
                               column(width = 4, 
                                      textInput("out_file", 
                                                "Name of file to save to")),
                               column(width = 3, 
                                      numericInput("width", 
                                                   "Width (pixels) of image", 
                                                   2000)),
                               column(width = 3,
                                      numericInput("height", 
                                                   "Height (pixels) of image", 
                                                   2000))
                             ),
                             plotOutput("progress_bars", height = 1000)
                    )
                  )
                )
        ),
        
        # second page ----
        tabItem(tabName = "2nd",
                fluidPage(
                  tags$head(tags$style(
                    HTML("#project_info {font-size: 20px}
                          #pheno_table {font-size: 24px}
                          #non_NA_data {font-size: 20px}")
                  )),
                  # page title
                  titlePanel("Phenotypes and Plots"),
                  tabsetPanel(
                    # set up tab-controlled sub-panels, defaulting to select one
                    id = "Select a project", type = "tabs",
                    # this sub-panel allows the user to select a project
                    tabPanel("Select a project",
                             uiOutput("single_project_selector"),
                             # list with information about the given project
                             withTags({
                               ul(id = 'project_info',
                                  li(b("Project Title:"), 
                                     textOutput("get_title")),
                                  li(b("Primary Investigator:"), 
                                     textOutput("get_PI")),
                                  li(b("The project name in the database:"), 
                                     textOutput("get_name")),
                                  li(b(paste("Data collected/Analyses", 
                                             "done on this project:")), 
                                     textOutput("get_desc")),
                                  li(b(paste("Can phenotype graphs be made for", 
                                             "this project:")), 
                                     textOutput("get_has_pheno_desc")),
                                  li(b("Link to Genetic Reports (if available):"),
                                     uiOutput("get_report_URL"))
                               )
                             }),
                             textOutput("round8_text")
                    ),
                    # this sub-panel displays a table about measured phenotypes
                    tabPanel("Phenotypes table",
                             actionButton("make_pheno_table", 
                                          "Generate table/Refresh"),
                             br(), br(), br(), br(),
                             dataTableOutput("pheno_table")
                    ),
                    # this sub-panel displays a customized and zoomable plots
                    tabPanel("Boxplots and Histograms",
                             fluidRow(
                               column(width = 6, uiOutput("box_hist_vars")),
                               column(width = 6,
                                      actionButton("load_boxhist", 
                                                   "Load plots/Refresh"),
                                      downloadButton(
                                        "downloadData",
                                        "Download values for all variables"
                                      ), br(), br(),
                                      sliderInput("bins", "Number of bins:",
                                                  min = 10, max = 100, 
                                                  step = 30, value = 10)
                               )
                             ),
                             fluidRow(
                               column(width = 6, 
                                      plotlyOutput("box", height = 600)),
                               column(width = 6, 
                                      plotlyOutput("hist", height = 600))
                             )
                    ),
                    # this sub-panel displays a plot showing correlations
                    tabPanel('Correlation Plot',
                             fluidRow(
                               column(width = 6, 
                                      actionButton("load_corr", 
                                                   "Load correlation plot"), 
                                      uiOutput("corr_vars")
                               ),
                               column(width = 6, uiOutput("non_NA_data"))
                             ),
                             fluidRow(
                               plotOutput("scatter", 
                                          height = 600, width = 1200),
                               plotOutput("corr", height = 1200, width = 1200)
                             )
                    )
                  )
                )
        ),
        
        # third page ----
        tabItem(tabName = "3rd",
                fluidPage(
                  tags$head(tags$style(
                    HTML("
                    #tables_in_schemas, #projects_in_tables {font-size: 20px}
                    #t_in_s_explanation, #p_in_t_explanation {font-size: 24px}
                    .col-sm-8 {width: 100%}
                         ")
                  )),
                  # Which schemas/projects have subtables
                  uiOutput("t_in_s_explanation"),
                  checkboxInput("show_empty", "Check whether tables are empty"),
                  dataTableOutput("tables_in_schemas"),
                  # Which sample_tracking tables have projects
                  uiOutput("p_in_t_explanation"),
                  dataTableOutput("projects_in_tables")
                )
        ),
        
        # fourth page ----
        tabItem(tabName = "4th",
                fluidPage(
                  tags$head(tags$style(
                    HTML("#genotype_reports li {font-size: 32px}
                          #genotype_reports h3 {font-size: 24px}")
                  )),
                  titlePanel(
                    "Click on a link to download the corresponding report"),
                  uiOutput("genotype_reports")
                )
        )
      )
    )
  )
)
