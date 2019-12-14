library(rmarkdown)
library(plotly)
library(DT)
library(shiny)


ui <- navbarPage("Horse Colic",
           tabPanel("About",
                    h2("The Data"),
                    p("Colic is a leading cause of death in horses.",
                      em("(I have horses and can tell you that as horse owners we're always \
                           worried about colic.)"),
                      "The data set is from the UCI machine learning library.",
                      "You can find the data and more information about it",
                      a(href="https://archive.ics.uci.edu/ml/datasets/Horse+Colic", "here"),
                      ". The data is mostly medical data about the horse taken by a vet.",
                      "There are two possible variables we might want to use as our target varaible.",
                      "One is the outcome of the patient (whether they lived or died).",
                      "The other is whether the lesion causing the colic needed surgery to be \
                      resolved. This was determined either during surgery or during necropsy.
                      For preprocessing, the irrelevant columns (such as whether pathology \
                      slides are available for the patient and patient ID number) were removed \
                      as well as any columns with more than 20% missing values. Subsequently, \
                      all rows with missing values were also dropped. The columns were then \
                      cast as numerical, factors, or ordinal and then split into a training \
                      andd testing set using a 80%/20% split."
                    ), #end of paragraph
                    br(),
                    hr(),
                    br(),
                    h2("The App"),
                    p("On the ", strong("Raw Data"), " page you will be able to visually \
                    peruse the full, training, and testing data sets in a table and download \
                    whichever set you prefer as a csv file. You can also sort and search the \
                    table."),
                    p("The ", strong("Data Exploration"), " tab allows you to select variables \
                    to see a graphical and numerical summary for. The graph and numerical \
                    summary is different depending on how many variables you've selected \
                    and whether the variables are categorical or numerical."),
                    p("Under ", strong("Unsupervised Learning"), " section you will be able to \
                    select the numerical variables for which you want to compute the principal \
                    components of and see a biplot. Principal components analysis is a dimension \
                    reduction technique that creates linear combinations of your variables to \
                    account for the directions of greatest variability in the data subject to \
                    all of the linear combination vectors being orthogonal to each other. Or \
                    more exactly,", 
                    withMathJax("$$ \\max_{\\phi 's}\\frac{1}{n}\\sum_{i=1}^{n}z_{i1}^2 \
                                \\textrm{ subject to } \\sum_{j=1}^p \\phi_{j1}^2 = 1$$")),
                    p("The ", strong("Supervised Learning"), " page is where you can fit \
                    your very own predictive model and use it to predict a new data point. \
                    Two types of models are available, a logistic regression and a random \
                    forest. For the logistic regression you can choose the threshold for the \
                    probability response. For the random forest you can select how many trees \
                    you want in the forest and how many predictor variables you want the model \
                    to randomly select at each branch of the trees. You will also be able to \
                    choose your target variable and your predictor variables.")
           ),
           tabPanel("Raw Data",
                    # need to add a way to subset the data here???
                    # Sidebar layout with input and output definitions ----
                    sidebarLayout(
                      # Sidebar panel for inputs ----
                      sidebarPanel(
                        # Input: Choose dataset ----
                        selectInput("dataset", "Choose a dataset:",
                                    choices = c("train", "test", "full")),
                        # Button
                        downloadButton("downloadData", "Download")
                      ),
                      
                      # Main panel for displaying outputs ----
                      mainPanel(
                        DTOutput("table")
                      )
                      
                    )
           ), # end tab
           tabPanel("Data Exploration",
                    sidebarLayout(
                      sidebarPanel(
                        h3("Select a variable:"),
                        uiOutput("var1Selector"),
                        conditionalPanel(condition = "input.var1 != '--none--'",
                                         uiOutput("var2Selector")),
                        conditionalPanel(condition = "output.bothQuant",
                                         uiOutput("var3Selector"))
                        ),
                      mainPanel(
                        plotlyOutput("plot"),
                        tableOutput("summaryTable")
                        # downloadButton("downloadPlot")
                      )
                    )
           ), # end tab
           tabPanel("Unsupervised Learning",
                    sidebarLayout(
                      sidebarPanel(
                        h2("Principal Componenets Analysis"),
                        p("For more information about PCA, see the About page."),
                        uiOutput("select_pca_vars"),
                        tags$hr(),
                        p("Select options for the PCA computation (we are using \
                          the prcomp function here)"),
                        radioButtons(inputId = 'center_vars',  
                                     label = 'Center',
                                     choices = c('Shift variables to be \
                                                 centered at 0.'='Yes',
                                                 'Do not shift variables.'='No'), 
                                     selected = 'Yes'),
                        
                        radioButtons('scale_vars', 'Scale',
                                     choices = c('Scale variables to have a \
                                                 variance of 1.'='Yes',
                                                 'Do not scale variables.'='No'), 
                                     selected = 'Yes')                        
                      ),
                      mainPanel(
                        plotOutput("biplot")
                      )
                      # would like to do another side panel here
                      # that lets you choose the pcs to use in biplot
                      # and a categorical variable
                      # would like to add plots from lecture and cluster
                      # like plot from example 
                      # https://github.com/benmarwick/Interactive_PCA_Explorer
                      # maybe an option to select which plot to view
                    )
                    
           ), # end  tab,
           tabPanel("Supervised Learning",
             sidebarLayout(
               sidebarPanel(
                 selectInput("model_type", "Choose a model:", 
                             choices = c("Logistic Regression", "Random Forest"),
                             selected = "Logistic Regression"),
                 conditionalPanel(condition = "input.model_type == 'Logistic Regression'",
                   sliderInput("thresh", "threshold", 0.5, min = 0, max = 1, step = 0.01)
                 ),
                 conditionalPanel(condition = "input.model_type == 'Random Forest'",
                   sliderInput("ntree", "number of trees", 500, 
                                min = 1, max = 5000, step = 1),
                   uiOutput("mtry_selector")
                   # add some kind of server side validation of these parameters 
                   # with text feedback here

                 ),
                 selectInput("target", "Choose a target variable:",
                             choices = c("outcome", "lesion_surgical"),
                             selected = "outcome"),
                 uiOutput("select_predictors"),
                 actionButton("train", "Train Model")
               ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                   tabPanel("Model Summary",
                            verbatimTextOutput("model_summary")
                            ),
                   tabPanel("Make a Prediction",
                            sidebarLayout(
                              sidebarPanel(
                                p("Use the model to predict a new data point. \
                                  Default values are medians.)"),
                                uiOutput("new_point")
                                # actionButton("predict", "Predict New Point")
                              ),
                              mainPanel(
                                textOutput("prediction")
                              )
                            ) 
                            ), # end of tab
                   tabPanel("Model Performance"),
                   tabPanel("Visualization")
                 )
               ) #visualization, model summary, performance & preds
               # tab or radio button for  each?
             )
           ) # end tab
        )

