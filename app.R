library(shiny)
library(shinydashboard)
library(fontawesome)
library(readxl)
library(lubridate)
library(knitr)
library(tidyverse)
library(caret)

ui <- dashboardPage(skin="red",
                    
                    # Creates title
                    dashboardHeader(title="Wildfire Data Exploration and Analysis", titleWidth=1000),
                    
                    # Creates sidebar items
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
                    
                    # Creates the body of the body 
                    dashboardBody(
                      tabItems(
                        
                        # The About Page
                        tabItem(tabName = "about",
                                fluidRow(
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
                                  # National Park Service Photo
                                  img(src = "NationalParkService.jpg"),
                                  
                                  column(6,
                                         
                                         #image_read("NationalParkService.jpg")
                                   )
                                )
                        ),
                        
                        # Explore Page      
                        tabItem(tabName = "explore",
                                fluidRow(
                                  column(width=3,
                                         box(width=12,
                                             background="red",
                                             
                                             # Choose variable for graph and plots
                                             radioButtons(inputId = "variable",
                                                          label = "Choose a variable:",
                                                          choices = c("Number of Trees" = 1,
                                                                      "Basal Area" = 2,
                                                                      "Stem Carbon" = 3),
                                                          selected = 1)),
                                         box(width=12,
                                             background="red",
                                             
                                             # Choose to filter the data by park or not
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
                                             
                                             # Choose a plot type
                                             selectInput(inputId = "plot",
                                                         label = "Plot Type:",
                                                         choices = list("Histogram" = "hist", "Density Plot" = "density"),
                                                         selected = "hist")),
                                         box(width=12,
                                             background="red",
                                             
                                             # Choose a summary Type
                                             selectInput(inputId = "summaryType",
                                                         label = "Summary Type:",
                                                         choices = list("Numeric Summary for Variable" = "sumVar", "Contingency Table for Park and Year" = "conTable"),
                                                         selected = "sumVar")),
                                         
                                             # If a numeric summary is chosen, choose what you want in it
                                             conditionalPanel(condition = "input.summaryType == 'sumVar'",
                                                       box(width=12,
                                                       background="red",
                                                       checkboxGroupInput(inputId = "summary",
                                                                         label = "Choose what you want included in the summary table:",
                                                                         choices = c("Minimum" = "Minimum",
                                                                                     "Mean" = "Mean",
                                                                                     "Median" = "Median",
                                                                                     "Maximum" = "Maximum",
                                                                                      "Variance" = "Variance"),
                                                                         selected = c("Minimum" = "Minimum",
                                                                                      "Mean" = "Mean",
                                                                                      "Median" = "Median",
                                                                                      "Maximum" = "Maximum",
                                                                                      "Variance" = "Variance")))   
                                                          )
                                         ),
                                  
                                  column(width=9,
                                         
                                         # Plot the histogram or density plot
                                         plotOutput("plotEDA"),
                                         
                                         box(width = 15,
                                              background = "red",
                                             
                                              # Print out the numeric summary or contingency table 
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
                                             
                                             # Choose a proportion for training and testing datasets
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
                                             background = "red",
                                             
                                             # Choose what variables you want in the model 
                                             checkboxGroupInput(inputId = "varSelect",
                                                                label = "Choose variables you want included in the model:",
                                                                choices = c("Park" = "park",
                                                                            "Day" = "day",
                                                                            "Month" = "month",
                                                                            "Year" = "year",
                                                                            "Number of Trees" = "n_trees_ha",
                                                                            "Basal Area" = "basal_area_ha"),
                                                                selected = c("Park" = "park",
                                                                             "Day" = "day",
                                                                             "Month" = "month",
                                                                             "Year" = "year",
                                                                             "Time Since the Burn" = "time_since_burn",
                                                                             "Number of Trees" = "n_trees_ha",
                                                                             "Basal Area" = "basal_area_ha"))
                                             ),
                                         box(width=12,
                                             background = "red",
                                             
                                             # Button to press to run models
                                             actionButton("do", "Run Models"),
                                             p("Note: Please press me once and be patient. It takes a little while for me to think!")
                                             )
                                         ),
                                  column(width=9,
                                           box(width=12,
                                               background = "red",
                                               
                                               # Print out RMSE table
                                               h4("RMSE for Each Model Using the Training Dataset"),
                                               tableOutput("tableRMSE")
                                               ),
                                         box(width=12,
                                             background = "red",
                                             
                                               # Multiple Linear Regression Summary
                                               h4("Multiple Linear Regression: Model Summary"),
                                               verbatimTextOutput("sumMLR")
                                             ),
                                         box(width=12,
                                             background = "red",
                                             
                                               # Regression Tree variable importance plot
                                               h4("Regression Tree: Importance Scores of Top 10 Variables"),
                                               plotOutput("plotRT")
                                             ),
                                         box(width=12,
                                             background = "red",
                                             
                                               # Random forest variable importance plot
                                               h4("Random Forest Model: Importance Scores of Top 10 Variables"),
                                               plotOutput("plotRF")
                                             ),
                                           box(width=12,
                                               background = "red",
                                               h4(""),
                                               textOutput("comparePrint"),
                                               tableOutput("compare")
                                               )
                                         )
                                  )
                                
                        ),
                        
                        tabItem(tabName = "prediction",
                                fluidRow(
                                  column(width=3,
                                         box(width=12,
                                             background="red",
                                             
                                             # Choose model type for predictions
                                             radioButtons(inputId = "model",
                                                          label = "Choose Model Type:",
                                                          choices = c("Multiple Linear Regression Model" = 1,
                                                                      "Regression Tree Model" = 2,
                                                                      "Random Forest Model" = 3),
                                                          selected = 1)
                                             ),
                                         
                                         box(width=12,
                                             background = "red",
                                             
                                             # Choice of park for predictions
                                             selectInput(inputId = "parkChoice", 
                                                         label = "Choose a Park:",
                                                         choices = c("None" = "NA",
                                                                     "Crater Lake National Park" = "CRLA",
                                                                     "Lassen Volcanic National Park" = "LAVO",
                                                                     "Lava Beds National Monument" = "LABE",
                                                                     "Whiskeytown National Recreation Area" = "WHIS"),
                                                         selected = "NA"),
                                             
                                             # Choice of number of trees for predictions
                                             numericInput(inputId = "treeChoice",
                                                          label = "Choose number of trees from 10 to 860:",
                                                          min = 10,
                                                          max = 860,
                                                          step = 1,
                                                          value = 50),
                                             
                                             # Choice of basal area for predictions
                                             numericInput(inputId = "basalChoice",
                                                          label = "Choose Basal Area from 0.6 to 106.6:",
                                                          min = 0.6,
                                                          max = 106.6,
                                                          step = 0.01,
                                                          value = 50),
                                             
                                             # Choose day for predictions
                                             numericInput(inputId = "dayChoice",
                                                          label = "Choose a day from 1 to 31:",
                                                          min = 1,
                                                          max = 31,
                                                          step = 1,
                                                          value = 20), 
                                             
                                             # choose month for predictions
                                             selectInput(inputId = "monthChoice",
                                                          label = "Choose a month:",
                                                          choices = c("April" = 4,
                                                                      "May" = 5,
                                                                      "June" = 6,
                                                                      "July" = 7,
                                                                      "August" = 8,
                                                                      "September" = 9,
                                                                      "October" = 10,
                                                                      "November" = 11,
                                                                      "December" = 12)),
                                             
                                             # Choose year for predictions
                                             numericInput(inputId = "yearChoice",
                                                          label = "Choose a year from 1989 to 2019:",
                                                          min = 1989,
                                                          max = 2019,
                                                          step = 1,
                                                          value = 1999)
                                         )
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
                         #       )
                        #)
                      #)
                    )
))

# Define server logic required to draw the plots
server <- shinyServer(function(input, output, session) {
  
  # Reads in data and data cleans slightly
  fire <- reactive({
    fire <- read_xlsx("tree_summary_dt.xlsx") %>% 
            mutate(date2 = ymd(date), month = month(date2), day = day(date2)) %>% 
            select(-c(date, date2, stem_biomass_ha, time_since_burn, plot_id))
  })

  
  #Create plot output for EDA
  output$plotEDA <-renderPlot({
    
    if(input$park != "No"){
      fire <- fire() %>% filter(park == input$park)
    } else {
      fire <- fire()
    }
    
    
    if(input$variable == 1){
      
      if(input$plot == "hist"){
        
        g <- ggplot(fire, aes(n_trees_ha)) + 
             geom_histogram(fill = "blue", binwidth = 25) +
             labs(x = "Number of Trees", y = "Count", title = "Histogram of Number of Trees")
        
       } else if (input$plot == "density"){
         
         g <- ggplot(fire, aes(n_trees_ha)) + 
              geom_density(kernel = "gaussian", color = "Green", fill = "Green", alpha = .5) + 
              labs(y = "Density", x = "Number of Trees", title = "Density plot: Number of Trees") 
         
      }

    } else if (input$variable == 2) {

      if(input$plot == "hist"){

        g <- ggplot(fire, aes(basal_area_ha)) +
             geom_histogram(fill = "blue", binwidth = 4) +
             labs(x = "Basal Area", y = "Count", title = "Histogram of Basal Area")

      } else if (input$plot == "density"){

        g <- ggplot(fire, aes(basal_area_ha)) +
             geom_density(kernel = "gaussian", color = "Green", fill = "Green", alpha = .5) +
             labs(y = "Density", x = "Basal Area", title = "Density plot: Basal Area")

      }


    } else if (input$variable == 3) {

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
  
  # Creates table for EDA
  output$tableEDA <- renderTable({
    
    if(input$park != "No"){
      fire <- fire() %>% filter(park == input$park)
    } else {
      fire <- fire()
    }
  
    if(input$summaryType == "sumVar"){
      sum <- input$summary %>% data.frame()
      colnames(sum) <- "summaryName"
    
      if(input$variable == 1){
        table <- fire %>% summarise(Minimum = min(n_trees_ha), Mean = mean(n_trees_ha), Median = median(n_trees_ha), Maximum = max(n_trees_ha), Variance = var(n_trees_ha)) 
      } else if (input$variable == 2) {
        table <- fire %>% summarise(Minimum = min(basal_area_ha), Mean = mean(basal_area_ha), Median = median(n_trees_ha), Maximum = max(basal_area_ha), Variance = var(basal_area_ha)) 
      } else if (input$variable == 3) {
        table <- fire %>% summarise(Minimum = min(stem_c_ha), Mean = mean(stem_c_ha), Median = median(n_trees_ha), Maximum = max(stem_c_ha), Variance = var(stem_c_ha)) 
      }
  

      for(i in 1:nrow(sum)){
        if(sum$summaryName[i] == "Minimum"){
          sum$summary[i] <- round(min(fire$n_trees_ha), 2)
        } else if(sum$summaryName[i] == "Mean"){
          sum$summary[i] <- round(mean(fire$n_trees_ha), 2)
        } else if(sum$summaryName[i] == "Median"){
          sum$summary[i] <- round(median(fire$n_trees_ha), 2)
        } else if(sum$summaryName[i] == "Maximum"){
          sum$summary[i] <- round(max(fire$n_trees_ha), 2)
        } else if(sum$summaryName[i] == "Variance"){
          sum$summary[i] <- round(var(fire$n_trees_ha), 2)
        }
      }
    
    
      newSum <- sum %>% t() %>% data.frame() 
    
      colnames(newSum) <- sum$summaryName
    
      newSum[-c(1),]
    } else {
       table("Park"= fire$park, "Year" = fire$year)
    }
  })
  
  
  # If park is included in the dataset make each park it's own variable with binary values
  fire2 <- eventReactive(input$do, {
    
    varList <- input$varSelect
         
    fire <- fire() %>% select(stem_c_ha, varList)
    
   if (varList[[1]] == "park"){

      # Created dummy variables for the character variables, park 
      dmy <- dummyVars("~ park", data = fire)

      # Creates a dataframe with the dummy variables associated with our data
      fireTrsf <- data.frame(predict(dmy, newdata = fire))

      # Binds the new data with our numeric variables with the original data and deletes the original character variables
      fire2 <- cbind(fire, fireTrsf) %>% select(-c(park))

    } else {
      fire2 <- fire
    }
    
  })
  
  # Creates randomized rows to be in the training dataset with the proportion specified
  trainData <- eventReactive(input$do, {
    
    set.seed(123)
    
    trainData <- sample(1:nrow(fire2()), size = nrow(fire2())*input$prop)
  })
  
  # Takes rows chosen and put in the training dataset
  fireTrain <- eventReactive(input$do, {
    
    fire2 <- fire2()

    fireTrain <- fire2[trainData(), ]
    
  })
  
  # Takes the remaining rows and puts it in the testing dataset
  fireTest <- eventReactive(input$do, {
    
    fire2 <- fire2()

    test <- setdiff(1:nrow(fire2), trainData())
    
    fireTest <- fire2[test, ]
  })
  
  # Fits the Multiple Linear Regression Model. 5-fold Cross-Validation
  fitMLR <- eventReactive(input$do, {

    fitMLR <- train(stem_c_ha ~ ., data = fireTrain(),
                    method = "lm",
                    preProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv", number = 5))

  })
  
  # Fits the Regression Tree Model. 5-fold Cross-Validation
  fitRT <- eventReactive(input$do, {
    
    fitRT <- train(stem_c_ha ~ ., data = fireTrain(),
                   method = "rpart",
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "cv", number = 5))    
  })
  
  # Fits the Random Forest Model. 5-fold Cross-Validation
  fitRF <- eventReactive(input$do, {
    
    fitRF <- train(stem_c_ha ~ ., data = fireTrain(),
                   method = "rf",
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = expand.grid(mtry = c(1:round(ncol(fireTrain())/3))))    
  })
  
  # Creates the RMSE table for the 3 models
  output$tableRMSE <- renderTable({

    fitMLR <- fitMLR()
    fitRT <- fitRT()
    fitRF <- fitRF()

    rmseMLR <- fitMLR$results %>% select(RMSE)

    rmseRT <- fitRT$results %>% filter(cp == fitRT$bestTune$cp) %>% select(RMSE)

    rmseRF <- fitRF$results %>% filter(mtry == fitRF$bestTune$mtry) %>% select(RMSE)

    rmseTable <- cbind(rmseMLR, rmseRT, rmseRF)

    colnames(rmseTable) <- list("Multiple Linear Regression", "Regression Tree", "Random Forest")

    rmseTable
  })
  
  # Prints out the summary for the Multiple Linear Regression Model
  output$sumMLR <- renderPrint(
    summary(fitMLR())
  )
  
  # Creates the variable importance plot for the regression tree model
  output$plotRT <- renderPlot({
    rtImp <- varImp(fitRT())
    plot(rtImp, top = 10)
  })
  
  # Creates the variable importance plot for the random forest model
  output$plotRF <- renderPlot({
    rfImp <- varImp(fitRF())
    plot(rfImp, top = 10)
  })
  
  winner <- reactive({
    
    fitMLR <- fitMLR()
    fitRT <- fitRT()
    fitRF <- fitRF()
    
    fireTest <- fireTest()
    
    # Predicts Shares using the models and test data
    predFitMLR <- predict(fitMLR, newdata = fireTest)
    predFitRT <- predict(fitRT, newdata = fireTest)
    predFitRF <- predict(fitRF, newdata = fireTest)
    
    # Compares the predicted shares found above to the shares in the test dataset
    postMLR <- postResample(predFitMLR, obs = fireTest$stem_c_ha)
    postRT <- postResample(predFitRT, obs = fireTest$stem_c_ha)
    postRF <- postResample(predFitRF, obs = fireTest$stem_c_ha)
    
    
    # Creates the names of the different models 
    modelNames <- c("Multiple Linear Regression Model", "Regression Tree Model", "Random Forest Model")
    
    # row binds the post comparisons and makes it a dataframe
    comp <- rbind(postMLR, postRT, postRF) %>% data.frame()
    
    # Changes the row names to the names of the different models created above
    rownames(comp) <- modelNames
    
    # Chooses the winner of the models by finding minimum RMSE
    winner <- comp %>% filter(RMSE == min(RMSE))
    
  })
  
  output$comparePrint <- renderText({
    # Prints out the minimum
    print(paste0("The winning model is the ", rownames(winner())))
  })
  
  output$compare <- renderTable({
    
    winner()

  })
  
  
})

shinyApp(ui = ui, server = server)
