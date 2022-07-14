library(shiny)
library(shinydashboard)
library(fontawesome)
library(readxl)
library(lubridate)
library(knitr)
library(tidyverse)

fire <- read_xlsx("tree_summary_dt.xlsx") %>% mutate(date2 = ymd(date)) %>% select(-c(date, stem_biomass_ha))

ui <- dashboardPage(skin="red",
                    
                    # Adds a title
                    dashboardHeader(title="Wildfire Data Exploration and Analysis", titleWidth=1000),
                    
                    # Defines sidebar items
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("About", tabName = "about", icon = icon("fire", lib = "font-awesome")),
                        menuItem("Data Exploration", tabName = "explore", icon = icon("chart-bar", lib = "font-awesome")),
                        menuItem("Modeling", tabName = "modeling", icon = icon("chart-line", lib = "font-awesome"),
                          menuSubItem("Modeling Info", tabName = "info", icon = icon("info", lib = "font-awesome")),
                          menuSubItem("Modeling Fitting", tabName = "fit", icon = icon("tree", lib = "font-awesome")),
                          menuSubItem("Prediction", tabName = "prediction", icon = icon("mountain", lib = "font-awesome"))),
                        menuItem("Data", tabName = "data", icon = icon("fire-extinguisher", lib = "font-awesome"))
                    )),
                    
                    # Defines the body of the app
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "about",
                                fluidRow(
                                  
                                  # Adds in latex functionality
                                  withMathJax(),
                                  
                                  # Three columns for each of the Three items
                                  column(6,
                                         # Purpose of the app
                                         h1("What is the purpose of this app?"),
                                         # Box to contain description
                                         box(background="red",width=12,
                                             p("The Purpose of this app is to explore Wildfire data and model it!")
                                             ),
                                         # What the app contains
                                         h1("What does this app contain?"),
                                         # Box to contain description
                                         box(background="red",width=12,
                                             p("This app contains this tab you're currently on, the About tab, the Data Exploration tab, the Modeling tab, and the Data tab."),
                                             p("The About page (this one!) introduces this app with the app purpose, data description, and app overview (what this is!)."),
                                             p("The Data Exploration page explores the data with different numerical and graphical summaries. You can choose what variable and plot type you'd like to look at."),
                                             p("The Modeling tab has a Modeling Info page, a Model Fitting page, and a Prediction page. The Modeling Info page provides info on multiple linear regression models, regression tree models, and random forest models. The Model Fitting tab splits the data into training and testing data sets (whatever proportion you choose), builds the three models and compares them to each other. The Prediction page obtains a prediction for the response using whichever model you'd like."),
                                             p("The Data page has the dataset used for this app. You can save the data if you'd like!")
                                             
                                         )
                                  ),
                                  
                                  column(6,
                                         # Description of the data
                                         h1("What data is being used?"),
                                         # Box to contain description
                                         box(background="red",width=12,
                                             p("The data was collected from the Klamath network parks which include Crater Lake National Park, Lassen Volcanic National Park, Lava Beds National Monument, and Whiskeytown National Recreation Area. The measurements were taken before, pre-burn, post-burn, and after the fire occurred in these areas. The data was proveded by a Regional Fire Ecologist for the National Park Service, Interior Regions 8, 9, 10, and 12 and exported by the U.S. Geological Survey.")
                                             )
                                  ),
                                  
                                  img(src = "NationalParkService.jpg"),
                                  
                                  column(6,
                                         
                                         #image_read("NationalParkService.jpg")
                                   )
                                )
                        ),
                        
                        #actual app layout      
                        tabItem(tabName = "explore",
                                fluidRow(
                                  column(width=3,
                                         box(width=12,
                                             background="red",
                                             radioButtons(inputId = "variable",
                                                          label = "Choose a variable:",
                                                          choices = c("Number of Trees" = 1,
                                                                      "Basal Area" = 2,
                                                                      "Stem Carbon" = 3),
                                                          selected = 1)),
                                         box(width=12,
                                             background="red",
                                             radioButtons(inputId = "park",
                                                          label = "Filter by Park:",
                                                          choices = c("Do Not Filter" = "No",
                                                                      "Crater Lake National Park" = "CRLA",
                                                                      "Lassen Volcanic National Park" = "LAVO",
                                                                      "Lava Beds National Monument" = "LABE",
                                                                      "Whiskeytown National Recreation Area" = "WHIS"),
                                                          selected = "No")),
                                         
                                         box(width=12,
                                             background="red",
                                             selectInput(inputId = "plot",
                                                         label = "Plot Type:",
                                                         choices = list("Histogram" = "hist", "Density Plot" = "density"),#, "Scatterplot" = "scatter"),
                                                         selected = "hist")),
                                         
                                         box(width=12,
                                             background="red",
                                              checkboxGroupInput(inputId = "summary",
                                                                 label = "Choose what you want included in the summary table:",
                                                                 choices = c("Minimum" = "min",
                                                                                "Mean" = "mean",
                                                                                "Median" = "med",
                                                                                "Maximum" = "max",
                                                                                "Variance" = "var"),
                                                                 selected = c("Minimum" = "min",
                                                                                 "Mean" = "mean",
                                                                                 "Median" = "med",
                                                                                 "Maximum" = "max",
                                                                                 "Variance" = "var")))
                                         ),
                                  
                                  column(width=9,
                                         plotOutput("plotEDA"),
                                         
                                         box(width = 15,
                                              background = "red",
                                              tableOutput("tableEDA")
                                             )
                                         
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "info",
                                fluidRow(
                                  column(6,
                                         # Linear Regression
                                         h1("Multiple Linear Regression"),
                                         #box to contain description
                                         box(background="red",
                                             width=12,
                                             h4("What is it?"),
                                             br(),
                                             h4("Benefits"),
                                             br(),
                                             h4("DrawBacks")
                                             )
                                  ),

                                  column(6,
                                         # Tree
                                         h1("Regression Tree"),
                                         #box to contain description
                                         box(background="red",
                                             width=12,
                                             h4("What is it?"),
                                             p("Random forest is a an ensemble learning method that is an extension of the idea of the bagging method. Like the bagging method, the random forest algorithm uses bagging, also known as bootstrap aggregation, to resample from the data or a fitted model randomly. Then multiple decision trees are created from these samples to create an uncorrelated forest and the results are then averaged. Unlike bagging, random forest doesn't use all of it's predictors but uses a random subset of predictors for each bootstrap sample. If there is a strong predictor, it'll likely be used for every first split in bagging, so randomly subsetting predictors will reduce correlation of tree predictions in random forest models."),
                                             br(),
                                             h4("Benefits"),
                                             br(),
                                             h4("DrawBacks")
                                            )
                                  ),

                                  column(6,
                                         # Random Forest
                                         h1("Random Forest"),
                                         #box to contain description
                                         box(background="red",
                                             width=12,
                                             h4("What is it?"),
                                             br(),
                                             h4("Benefits"),
                                             br(),
                                             h4("DrawBacks")
                                             )
                                  )
                                )
                        ),

                        tabItem(tabName = "fit",
                                fluidRow(
                                  column(width=3,
                                         box(width=12,
                                             background="red",
                                             p("Now we are splitting our data into training and testing data sets."),
                                             p("The proportions of the training and testing datasets equal 1."),
                                             p("Here you can choose a proportion for your training dataset (e.g. 0.8) and the testing dataset will be 1 subtracted by the proportion chosen (e.g. 1 - 0.8 = 0.2)."),
                                             numericInput(inputId = "prop",
                                                          label = "Choose a Proportion:",
                                                          value = 0.8,
                                                          min = 0,
                                                          max = 1,
                                                          step = 0.1)
                                         ),
                                         box(width=12,
                                             )
                                  ),
                                  column(width=9,
                                         fluidRow(
                                           box(width=6,
                                               ),
                                           box(width=6,
                                               )
                                         )
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "prediction",
                                fluidRow(
                                  column(width=3,
                                         box(width=12,
                                             background="red",
                                             # CHOSE MODEL HERE WITH RADIOBUTTONS
                                             numericInput(inputId = "prop",
                                                          label = "Choose a Proportion:",
                                                          value = 0.8,
                                                          min = 0,
                                                          max = 1,
                                                          step = 0.1)
                                         ),
                                         box(width=12,
                                         )
                                  ),
                                  column(width=9,
                                         fluidRow(
                                           box(width=6,
                                           ),
                                           box(width=6,
                                           )
                                         )
                                  )
                                )
                        )
                        # 
                        # tabItem(tabName = "data",
                        #         fluidRow(
                        #           column(width=3,
                        #                  box(width=12,background="red",sliderInput("yvalue","Y=Number of Successes",min = 0,max = 30,value = 15)
                        #                  ),
                        #                  box(width=12,
                        #                      title="Hyperparameters of the prior distribution for \\(\\Theta\\)",
                        #                      background="red",
                        #                      solidHeader=TRUE,
                        #                      p("\\(\\frac{\\Gamma(\\alpha+\\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)}\\theta^{\\alpha-1}(1-\\theta)^{\\beta-1}\\)"),
                        #                      h5("(Set to 1 if blank.)"),
                        #                      numericInput("alpha",label=h5("\\(\\alpha\\) Value (> 0)"),value=1,min=0,step=0.1),
                        #                      numericInput("beta",label=h5("\\(\\beta\\) Value (> 0)"),value=1,min=0,step=0.1)
                        #                  )
                        #           ),
                        #           column(width=9,
                        #                  fluidRow(
                        #                    box(width=6,
                        #                        plotOutput("priorPlot"),
                        #                        br(),
                        #                        h4("Prior distribution for the probability of success parameter \\(\\Theta\\).")
                        #                    ),
                        #                    box(width=6,
                        #                        plotOutput("distPlot"),
                        #                        br(),
                        #                        h4("Posterior distribution for the probability of success \\(\\Theta\\).")
                        #                    )
                        #                  )
                        #           )
                                )
                        #)
                      #)
                    )
)

# Define server logic required to draw the plots
server <- shinyServer(function(input, output, session) {
  
  #getData <- reactive({
  #  fire <- read_xlsx("tree_summary_dt.xlsx") %>% mutate(date2 = ymd(date)) %>% select(-c(date))
  #})
  

  # THIS DOESN'T WORK
  # observe(
  #   if(input$variable == 3){
  #     updateSelectInput(session, inputID = "plot", choices = list("Histogram" = "hist", "Density Plot" = "density", "Scatterplot" = "scatter"))
  #   } else {
  #     updateSelectInput(session, inputId = "plot", choices = list("Histogram" = "hist", "Density Plot" = "density", "Scatterplot" = "scatter"))
  #   } 
  # )
  
  #Create plot output
  output$plotEDA <-renderPlot({
    
    if(input$park != "No"){
      fire <- fire %>% filter(park == input$park)
    }
    
    
    if(input$variable == 1){
      
      #updateSelectInput(session, inputId = "plot", choices = list("Histogram" = "hist", "Density Plot" = "density", "Scatterplot" = "scatter"))
      
      if(input$plot == "hist"){
        
        g <- ggplot(fire, aes(n_trees_ha)) + 
             geom_histogram(fill = "blue", binwidth = 25) +
             labs(x = "Number of Trees", y = "Count", title = "Histogram of Number of Trees")
        
       } else if (input$plot == "density"){
         
         g <- ggplot(fire, aes(n_trees_ha)) + 
              geom_density(kernel = "gaussian", color = "Green", fill = "Green", alpha = .5) + 
              labs(y = "Density", x = "Number of Trees", title = "Density plot: Number of Trees") 
         
      } else if (input$plot == "scatter"){

        g <- ggplot(fire, aes(x = n_trees_ha, y = stem_c_ha)) +
             geom_point() +
             labs(x = "Number of Trees", y = "Stem Carbon", title = "Scatterplot of Number of Trees by Stem Carbon")

      }


    } else if (input$variable == 2) {

      #updateSelectInput(session, inputId = "plot", choices = list("Histogram" = "hist", "Density Plot" = "density", "Scatterplot" = "scatter"))

      if(input$plot == "hist"){

        g <- ggplot(fire, aes(basal_area_ha)) +
             geom_histogram(fill = "blue", binwidth = 4) +
             labs(x = "Basal Area", y = "Count", title = "Histogram of Basal Area")

      } else if (input$plot == "density"){

        g <- ggplot(fire, aes(basal_area_ha)) +
             geom_density(kernel = "gaussian", color = "Green", fill = "Green", alpha = .5) +
             labs(y = "Density", x = "Basal Area", title = "Density plot: Basal Area")

      } else if (input$plot == "scatter"){

        g <- ggplot(fire, aes(x = basal_area_ha, y = stem_c_ha)) +
             geom_point() +
             labs(x = "Basal Area", y = "Stem Carbon", title = "Scatterplot of Basal Area by Stem Carbon")

      }


    } else if (input$variable == 3) {

      #updateSelectInput(session, inputID = "plot", choices = list("Histogram" = "hist", "Density Plot" = "density", "Scatterplot" = "scatter"))
      

      if(input$plot == "hist"){
       
        g <- ggplot(fire, aes(stem_c_ha)) +
          geom_histogram(fill = "blue", binwidth = 15000) +
          labs(x = "Stem Carbon", y = "Count", title = "Histogram of Stem Carbon")

      } else if (input$plot == "density"){

        g <- ggplot(fire, aes(stem_c_ha)) +
          geom_density(kernel = "gaussian", color = "Green", fill = "Green", alpha = .5) +
          labs(y = "Density", x = "Stem Carbon", title = "Density plot: Stem Carbon")

      }
     }
    
    g
    
  })
  
  output$tableEDA <- renderTable({
    
    if(input$park != "No"){
      fire <- fire %>% filter(park == input$park)
    }
  
    sum <- input$summary
    
    try <- c(1:length(sum)) %>% t() %>% data.frame()
    #for(i in 1:length(sum)){
      colnames(try) <- sum
    #}
    
    if(input$variable == 1){
      table <- fire %>% summarise(Minimum = min(n_trees_ha), Mean = mean(n_trees_ha), Median = median(n_trees_ha), Maximum = max(n_trees_ha), Variance = var(n_trees_ha)) 
    } else if (input$variable == 2) {
      table <- fire %>% summarise(Minimum = min(basal_area_ha), Mean = mean(basal_area_ha), Median = median(n_trees_ha), Maximum = max(basal_area_ha), Variance = var(basal_area_ha)) 
    } else if (input$variable == 3) {
      table <- fire %>% summarise(Minimum = min(stem_c_ha), Mean = mean(stem_c_ha), Median = median(n_trees_ha), Maximum = max(stem_c_ha), Variance = var(stem_c_ha)) 
    }
  
    if(!(try %in% "min")){
      table <- table %>% select(-c(Minimum))
    }

    if(!(try %in% "max")){
      table <- table %>% select(-c(Maximum))
    }
  
  table 
  
  })
  
  
})

shinyApp(ui = ui, server = server)
