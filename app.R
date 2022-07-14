library(shiny)
library(shinydashboard)
library(fontawesome)

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
                                  
                                  column(6,
                                         # What the app contains
                                         h1("What does this app contain?"),
                                         # Box to contain description
                                         box(background="red",width=12,
                                             p("This app contains")
                                             )
                                   )
                                  
                                  #imageOutput("NationalParkService.gif")
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
                                                          selected = 1),
                                             
                                             selectInput(inputId = "plot",
                                                         label = "Plot Type:",
                                                         choices = list("Histogram", "Density Plot", "Scatterplot"),
                                                         selected = "Histogram"))
                                         ),
                                         box(width=12,
                                             title="Hyperparameters of the prior distribution for \\(\\Theta\\)",
                                             background="red",
                                             solidHeader=TRUE,
                                             p("\\(\\frac{\\Gamma(\\alpha+\\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)}\\theta^{\\alpha-1}(1-\\theta)^{\\beta-1}\\)"),
                                             h5("(Set to 1 if blank.)"),
                                             numericInput("alpha",label=h5("\\(\\alpha\\) Value (> 0)"),value=1,min=0,step=0.1),
                                             numericInput("beta",label=h5("\\(\\beta\\) Value (> 0)"),value=1,min=0,step=0.1)
                                         )
                                  ),
                                  column(width=9,
                                         fluidRow(
                                           box(width=6,
                                               plotOutput("priorPlot"),
                                               br(),
                                               h4("Prior distribution for the probability of success parameter \\(\\Theta\\).")
                                           ),
                                           box(width=6,
                                               plotOutput("distPlot"),
                                               br(),
                                               h4("Posterior distribution for the probability of success \\(\\Theta\\).")
                                           )
                                         )
                                  )
                                ),
                      
                        
                        tabItem(tabName = "info",
                                fluidRow(
                                  column(6,
                                         # Linear Regression 
                                         h1("Multiple/generalized Linear Regression"),
                                         #box to contain description
                                         box(background="red",width=12,
                                             h4("This application shows the relationship between the prior distribution and the posterior distribution for a simple Bayesian model."),
                                             h4("The prior distribution is assumed to be a Beta distribution and the likelihood is a Binomial distribution with 30 trials (of which you can change the number of successes).  This yields a Beta distribution as the posterior. Note: As the prior distribution is in the same family as the posterior, we say the prior is conjugate for the likelihood."),
                                             h4("This application corresponds to an example in ",span("Mathematical Statistics and Data Analysis",style = "font-style:italic"), "section 3.5, example E, by John Rice."),
                                             h4("The goal of the example is to update our belief about the parameter \\(\\Theta\\) = the probability of obtaining a head when a particular coin is flipped.  The experiment is to flip the coin 30 times and observe the number of heads. The likelihood is then a binomial distribution. The prior is assumed to be a Beta distribution.")
                                         )
                                  ),
                                  
                                  column(6,
                                         # Tree
                                         h1("Regression/classification Tree"),
                                         #box to contain description
                                         box(background="red",width=12,
                                             h4("This application shows the relationship between the prior distribution and the posterior distribution for a simple Bayesian model."),
                                             h4("The prior distribution is assumed to be a Beta distribution and the likelihood is a Binomial distribution with 30 trials (of which you can change the number of successes).  This yields a Beta distribution as the posterior. Note: As the prior distribution is in the same family as the posterior, we say the prior is conjugate for the likelihood."),
                                             h4("This application corresponds to an example in ",span("Mathematical Statistics and Data Analysis",style = "font-style:italic"), "section 3.5, example E, by John Rice."),
                                             h4("The goal of the example is to update our belief about the parameter \\(\\Theta\\) = the probability of obtaining a head when a particular coin is flipped.  The experiment is to flip the coin 30 times and observe the number of heads. The likelihood is then a binomial distribution. The prior is assumed to be a Beta distribution.")
                                         )
                                  ),
                                  
                                  column(6,
                                         # Random Forest
                                         h1("Random Forest"),
                                         #box to contain description
                                         box(background="red",width=12,
                                             h4("This application shows the relationship between the prior distribution and the posterior distribution for a simple Bayesian model."),
                                             h4("The prior distribution is assumed to be a Beta distribution and the likelihood is a Binomial distribution with 30 trials (of which you can change the number of successes).  This yields a Beta distribution as the posterior. Note: As the prior distribution is in the same family as the posterior, we say the prior is conjugate for the likelihood."),
                                             h4("This application corresponds to an example in ",span("Mathematical Statistics and Data Analysis",style = "font-style:italic"), "section 3.5, example E, by John Rice."),
                                             h4("The goal of the example is to update our belief about the parameter \\(\\Theta\\) = the probability of obtaining a head when a particular coin is flipped.  The experiment is to flip the coin 30 times and observe the number of heads. The likelihood is then a binomial distribution. The prior is assumed to be a Beta distribution.")
                                         )
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "fit",
                                fluidRow(
                                  column(width=3,
                                         box(width=12,background="red",sliderInput("yvalue","Y=Number of Successes",min = 0,max = 30,value = 15)
                                         ),
                                         box(width=12,
                                             title="Hyperparameters of the prior distribution for \\(\\Theta\\)",
                                             background="red",
                                             solidHeader=TRUE,
                                             p("\\(\\frac{\\Gamma(\\alpha+\\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)}\\theta^{\\alpha-1}(1-\\theta)^{\\beta-1}\\)"),
                                             h5("(Set to 1 if blank.)"),
                                             numericInput("alpha",label=h5("\\(\\alpha\\) Value (> 0)"),value=1,min=0,step=0.1),
                                             numericInput("beta",label=h5("\\(\\beta\\) Value (> 0)"),value=1,min=0,step=0.1)
                                         )
                                  ),
                                  column(width=9,
                                         fluidRow(
                                           box(width=6,
                                               plotOutput("priorPlot"),
                                               br(),
                                               h4("Prior distribution for the probability of success parameter \\(\\Theta\\).")
                                           ),
                                           box(width=6,
                                               plotOutput("distPlot"),
                                               br(),
                                               h4("Posterior distribution for the probability of success \\(\\Theta\\).")
                                           )
                                         )
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "data",
                                fluidRow(
                                  column(width=3,
                                         box(width=12,background="red",sliderInput("yvalue","Y=Number of Successes",min = 0,max = 30,value = 15)
                                         ),
                                         box(width=12,
                                             title="Hyperparameters of the prior distribution for \\(\\Theta\\)",
                                             background="red",
                                             solidHeader=TRUE,
                                             p("\\(\\frac{\\Gamma(\\alpha+\\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)}\\theta^{\\alpha-1}(1-\\theta)^{\\beta-1}\\)"),
                                             h5("(Set to 1 if blank.)"),
                                             numericInput("alpha",label=h5("\\(\\alpha\\) Value (> 0)"),value=1,min=0,step=0.1),
                                             numericInput("beta",label=h5("\\(\\beta\\) Value (> 0)"),value=1,min=0,step=0.1)
                                         )
                                  ),
                                  column(width=9,
                                         fluidRow(
                                           box(width=6,
                                               plotOutput("priorPlot"),
                                               br(),
                                               h4("Prior distribution for the probability of success parameter \\(\\Theta\\).")
                                           ),
                                           box(width=6,
                                               plotOutput("distPlot"),
                                               br(),
                                               h4("Posterior distribution for the probability of success \\(\\Theta\\).")
                                           )
                                         )
                                  )
                                )
                        )
                      )
                    )
)

# Define server logic required to draw the plots
server <- shinyServer(function(input, output, session) {
  
  # THIS DOESN'T WORK
  observe(
    if(input$variable == 3){
      updateSelectInput(session, inputID = "plot", choices = list("Histogram", "Density Plot"))
    } else {
      updateSelectInput(session, inputId = "plot", choices = list("Histogram", "Density Plot", "Scatterplot"))
    }
  )
  
  #Create prior plot output
  output$priorPlot<-renderPlot({
    
    #Plotting sequence
    x <- seq(from=0,to=1,by=0.01)
    
    #get alpha and beta values from input
    alphaval<-input$alpha
    betaval<-input$beta
    
    #set defaults if not supplied
    if (is.na(alphaval)){alphaval<-1}
    if (is.na(betaval)){betaval<-1}
    
    #draw the prior distribution plot
    plot(x=x,y=dbeta(x=x,shape1=alphaval,shape2=betaval),main="Prior Density for Theta",xlab="theta's", ylab="f(theta)",type="l")
    
  })
  
  #create posterior plot  
  output$distPlot <- renderPlot({
    
    #Plotting sequence
    x    <- seq(from=0,to=1,by=0.01)
    
    #number of success from input slider
    numsuccess <- input$yvalue
    
    #get alpha and beta values from input
    alphaval<-input$alpha
    betaval<-input$beta
    
    #sample size
    n<-30
    
    #set defaults if not supplied
    if (is.na(alphaval)){alphaval<-1}
    if (is.na(betaval)){betaval<-1}
    
    # draw the posterior
    plot(x=x,y=dbeta(x=x,shape1=numsuccess+alphaval,shape2=n-numsuccess+betaval),main=paste("Posterior Density for Theta|Y=",numsuccess,sep=""),xlab="theta's", ylab="f(theta|y)",type="l")
  })
  
})

shinyApp(ui = ui, server = server)
