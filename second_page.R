# information about current project ----

# this stores metadata about the active project
project_details <- reactiveValues()

# update metadata whenever the selected project changes
observeEvent(input$selected_project, ignoreInit = T, {
  meta <- project_meta[project_meta$project_name == input$selected_project,]
  
  # basic metadata
  project_details$title <- meta$project_title
  project_details$name <- input$selected_project
  project_details$PI <- meta$PI_name
  project_details$desc <- paste(names(meta)[meta == 'TRUE'], collapse = ', ')
  
  # deeper details
  project_details$has_pheno <- 
    ifelse(table_exists(input$selected_project, 'descriptions'),
           'YES (has .descriptions table)', 'NO (lacks .descriptions table)')
  project_details$URL <- get_report_URL(input$selected_project)
  project_details$round8 <- 
    ifelse(
      input$selected_project == "r01_doug_adams" |
        (startsWith(input$selected_project, "p50") & 
           endsWith(input$selected_project, "2014")),
      "Genotypes used for report: round8 GBS bioinformatics pipeline", ""
    )
})

# load metadata about phenotypes
pheno_info_table <- reactive({
  safely_load_df(
    input$selected_project, "descriptions", 
    "This project does not have information about the phenotypes measured"
  )[,1:3]
})

# load measurements for phenotypes
pheno_val_table <- reactive({
  gwas <- safely_load_df(
    input$selected_project, "gwas_phenotypes", 
    "This project does not have data/values for the phenotypes measured"
  )
  
  # only use columns in .gwas_phenotypes which are traits from .descriptions
  traits_with_cols <- intersect(get_measured_traits(), colnames(gwas))
  validate(need(length(traits_with_cols) > 0, 
                paste("No traits from .descriptions are column names in",
                      ".gwas_phenotypes; please update database")))
  gwas[,c("cohort", "sex", "rfid", traits_with_cols)]
})

# load the names of all traits measured
get_measured_traits <- reactive({
  phenotypes <- pheno_info_table()
  phenotypes[phenotypes$trait_covariate == 'trait',]$column_name
})

# -- first tab items -- #

# select one project to view from the ones this user has available
output$single_project_selector <- renderUI({
  selectInput("selected_project", "Choose from the following", 
              choices = user_projects())
})

# these communicate the information back to the UI
output$get_title <- renderText({ project_details$title })
output$get_name <- renderText({ project_details$name })
output$get_PI <- renderText({ project_details$PI })
output$get_desc <- renderText({ project_details$desc })
output$get_has_pheno_desc <- renderText({ project_details$has_pheno })
output$get_report_URL <- renderUI({ project_details$URL })
output$round8_text <- renderText({ project_details$round8 })

# second tab items ----

# output table with phenotype information
output$pheno_table = DT::renderDataTable({
  DT::datatable(
    pheno_info_table(), filter = list(position = "top", clear = FALSE),
    extensions = c("Buttons","FixedColumns"),
    rownames = F, options = table_options(T, "phenotypes_table")
  )
}) 

# third tab items ----

# offer options for what variables to plot on the boxplot/histogram
output$box_hist_vars <- renderUI({
  mainPanel(
    selectInput("Ind", "Independent Variable", choices = c("cohort", "sex"),
                width = '100%'),
    selectInput("Dep", "Dependent Variable", choices = get_measured_traits(),
                width = '100%')
  )
})

# these reactively pull information about what to plot
ind <- reactive({ input$Ind })
dep <- reactive({ input$Dep })

# load the actual phenotype measurements, but only when load button is clicked
pheno_data <- eventReactive(input$load_boxhist, ignoreInit = T, {
  pheno_val_table()
}) 

# render zoomable boxplot which plots the input variables
output$box <- renderPlotly({
  df <- pheno_data()
  layout(plot_ly(x = df[[ind()]], y = df[[dep()]], 
                 type = "box", boxpoints = "all", jitter = 0.3), 
         title = paste("comparisons by", input$Ind),
         xaxis = list(title = input$Ind))
})

# render zoomable histogram which plots the input value
output$hist <- renderPlotly({
  layout(plot_ly(x = pheno_data()[[dep()]], nbinsx = input$bins, 
                 type = "histogram"), 
         bargap = 0.1, title = paste("Histogram for", input$Dep), 
         xaxis = list(title = input$Dep), yaxis = list(title = 'Counts'))
})

# download the phenotype measurements when the button is clicked
output$downloadData <- downloadHandler(
  filename = paste0(input$selected_project, ".csv"),
  content = function(file) {write.csv(pheno_data(), file, row.names = FALSE)}
)

# fourth tab items ----

# load the names of all numeric traits measured
get_numeric_traits <- reactive({
  # select phenotype names which a) are traits and b) use numeric measurements
  names(select(pheno_val_table()[,get_measured_traits()], where(is.numeric)))
})

# offer options for what variables to plot on the correlation plot
output$corr_vars <- renderUI({
  mainPanel(
    selectInput("X", "Select X-axis variable", get_numeric_traits(), 
                width = "100%"),
    selectInput("Y", "Select Y-axis variable", get_numeric_traits(), 
                selected = get_numeric_traits()[2], width = "100%"))
})

# these reactively pull information about what to plot
X <- reactive({ input$X })
Y <- reactive({ input$Y })

# load the actual phenotype measurements, but only when load button is clicked
pheno_numeric_data <- eventReactive(input$load_corr, ignoreInit = T, {
  pheno_val_table()[,get_numeric_traits()]
})

# present information about how much data there is to plot
output$non_NA_data <- renderUI({
  # pull the row nums which have non-NA values
  data <- pheno_numeric_data()
  non_NA_x <- which(!is.na(data[[X()]]))
  non_NA_y <- which(!is.na(data[[Y()]]))
  non_NA_both <- intersect(non_NA_x, non_NA_y)
  
  # build up nicely formatted list output
  tags$ul(
    tags$li(paste("There are", length(non_NA_both), 
                  "samples with data for both"), tags$code(X()), 
            "and", tags$code(Y())),
    tags$li(paste("There are", length(non_NA_x), 
                  "samples with data for"), tags$code(X())),
    tags$li(paste("There are", length(non_NA_y), 
                  "samples with data for"), tags$code(Y()))
  )
})

# scatterplot of the two selected variables against each other
output$scatter <- renderPlot({
  ggscatter(pheno_numeric_data(), x = X(), y = Y(),
            # regression line & confidence interval
            add = "reg.line", conf.int = TRUE,
            # make dots for all the available data points
            add.params = list(color = "blue", fill = "lightgray"), 
            na.rm = TRUE) +
    # add correlation coefficients on top
    stat_cor(label.x.npc = "middle", label.y.npc = "top", size = 8)
})

# barplot showing the top 10 most correlated pairs of variables, by p-value
output$corr <- renderPlot({
  # get top correlations
  corrs <- as.data.frame(corr_cross(pheno_numeric_data(), plot = FALSE, 
                                    rm.na = TRUE, top = 10))
  # set up labels with linebreaks
  corrs$label <- paste(corrs$group1, "+\n", corrs$group2)
  corrs$label <- factor(corrs$label, levels = rev(corrs$label))
  # imitate corr_cross plot, but now there are linebreaks in the labels
  ggplot(corrs, aes(x = corr, y = label)) + geom_col(fill = '#3ca9c7') + 
    theme(text = element_text(size = 25), 
          plot.background = element_rect(fill = 'white')) +
    labs(x = 'R', y = 'variables', title = "Ranked Cross-Correlations", 
         subtitle = "10 most relevant [NAs removed]")
})
