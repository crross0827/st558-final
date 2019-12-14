library(shiny)
library(tidyverse)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(randomForest)


process_data <- function(colics) {
  
  # specify column names
  
  names <- c("surgery", "age", "id", "temp", "pulse", "respiration_rate", 
             "extremitites", "peripheral_pulse", "mucous_membrane", 
             "capillary_refill", "pain", "peristalsis", "distension", "tube_gas", 
             "reflux_volume", "reflux_pH", "feces", "abdomen", "cell_volume", 
             "protein", "fluid_appearance", "fluid_protein", "outcome", 
             "lesion_surgical", "lesion_site", "lesion_type", "lesion_subtype", 
             "cp_data")
  colnames(colics) <- names
  
  # coerce variables to factors or ordered factors and assign labels to levels
  
  colics$surgery <- factor(colics$surgery, levels=c("1", "2"), labels=c("yes", "no"), 
                         ordered=FALSE)
  colics$age <- factor(colics$age, levels=c("1", "9"), labels=c("adult", "juvenile"), 
                     ordered=FALSE)
  colics$extremitites <- factor(colics$extremitites, levels=c("2", "1", "3", "4"),
                           labels=c("warm", "normal", "cool", "cold"), ordered=TRUE)
  colics$peripheral_pulse <- factor(colics$peripheral_pulse, levels=c("2", "1", "3", "4"),
                              labels=c("increased", "normal", "reduced", "absent"),
                              ordered=TRUE)
  colics$mucous_membrane <- factor(colics$mucous_membrane, 
                              levels=c("1", "2", "3", "4", "5", "6"),
                              labels=c("normal pink", "bright pink", "pale pink",
                                       "pale cyanotic", "bright red", 
                                       "dark cyanotic"),
                              ordered=FALSE)
  colics$capillary_refill <- factor(colics$capillary_refill, levels=c("1", "2"), 
                            labels=c("< 3s", "> 3s"), ordered=FALSE)
  # note from source: should NOT be treated as a ordered or discrete variable!
  colics$pain <- factor(colics$pain, levels=c("1", "2", "3", "4", "5"), 
                      labels=c("alert, none", "depressed", 
                               "intermittent mild", "intermittent severe",
                               "continuous severe"), ordered=FALSE)
  colics$peristalsis <- factor(colics$peristalsis, levels=c("1", "2", "3", "4"),
                             labels=c("hypermotile", "normal", "hypomotile", 
                                      "absent"), ordered=TRUE)
  
  colics$distension <- factor(colics$distension, levels=c("1", "2", "3", "4"),
                            labels=c("none", "slight", "moderate", "severe"), 
                            ordered=TRUE)
  colics$tube_gas <- factor(colics$tube_gas, levels=c("1", "2", "3"),
                          labels=c("none", "slight", "significant"), 
                          ordered=TRUE)
  colics$reflux_volume <- factor(colics$reflux_volume, levels=c("1", "3", "2"),
                               labels=c("none", "< 1 liter", "> 1 liter"), 
                               ordered=FALSE)
  colics$feces <- factor(colics$feces, levels=c("2", "1", "3", "4"), 
                       labels=c("increased", "normal", "decreased", "absent"),
                       ordered=TRUE)
  colics$abdomen <- factor(colics$abdomen, levels=c("1", "2", "3", "4", "5"),
                         labels=c("normal", "other", "feces in LI", "distended SI",
                                  "distended LI"), ordered=FALSE)
  colics$fluid_appearance <- factor(colics$fluid_appearance, levels=c("1", "2", "3"),
                                  labels=c("clear", "cloudy", "serosanguinous"),
                                  ordered=FALSE)
  colics$outcome <- factor(colics$outcome, levels=c("1", "2", "3"),
                                         labels=c("lived", "died/euthenized", 
                                                  "died/euthenized"), ordered=FALSE)
  colics$lesion_surgical <- factor(colics$lesion_surgical, levels=c("1", "2"),
                                 labels=c("yes", "no"), ordered=FALSE)
  
  # drop columns that aren't useful
  colics <- select(colics, -c(id, cp_data, lesion_site, lesion_type, lesion_subtype))  
}


# data from https://archive.ics.uci.edu/ml/datasets/Horse+Colic
train <- read_delim("../horse-colic.data", " ", col_names=FALSE, na=c("?"))
test <- read_delim("../horse-colic.test", " ", col_names=FALSE, na=c("?"))

colics <- rbind(train, test)
colics <- process_data(colics)

for (x in names(colics)) {
  if (sum(is.na(colics[x])==TRUE) > 0.2*nrow(colics)) {
    colics <- select(colics, -x)
  }
}

colics <- na.omit(colics)

## 80% of the sample size
smp_size <- floor(0.80 * nrow(colics))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(colics)), size = smp_size)

train <- colics[train_ind, ]
test <- colics[-train_ind, ]

nums <- unlist(lapply(colics, is.numeric))
num_cols <- names(colics)[nums]

server <- function(input, output) {

  output$var1Selector <- renderUI({
    selectInput("var1", "Choose Variable:", c('--none--', names(colics))) 
  })
  
  output$var2Selector <- renderUI({
    data_names <- names(colics)
    data_names <- data_names[data_names != input$var1]
    selectInput("var2", "Choose Another Variable:", c('--none--', data_names)) 
  })
  
  output$bothQuant <- reactive({
  if (exists("input$var1") & exists("input$var2")){
    if ((input$var1 %in% num_cols) & (input$var2 %in% num_cols)) {
      return(TRUE)
    }
    else {return(FALSE)}
  }
  else {return(FALSE)}
  })
  
  outputOptions(output, "bothQuant", suspendWhenHidden = FALSE)
  
  output$var3Selector <- renderUI({
    nums <- unlist(lapply(colics, is.numeric))
    fac_cols <- names(colics)[!nums]
    selectInput("var3", "Color Points By:", c('--none--', fac_cols)) 
  })
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "train" = train,
           "test" = test,
           "full" = colics)
  })
  
  # Table of selected dataset ----
  output$table <- renderDT({
    datatable(datasetInput())
  })
  
  output$summaryTable <- renderTable({ 

    if (input$var1 == '--none--'){ # var1 is not selected
      summary_tbl <- data.frame()
    }
    else if (input$var1 %in% num_cols) { # var1 is quantitative
      if (input$var2 == '--none--') { # var 2 is not selected
        summary_tbl <- as.data.frame(as.list(summary(colics[[input$var1]])))
      }
      else if (input$var2 %in% num_cols){ # var 1 and var2 are quantitative
        if (input$var3 == '--none--'){ # var3 is not selected
          summary_tbl <- data.frame(list(
            covariance = round(cov(colics[[input$var1]], colics[[input$var2]]), 3),
            correlation = round(cor(colics[[input$var1]], colics[[input$var2]]),3)))
        }
        else { # var1 and var2 are quantitative, var3 is selected
          summary_tbl <- colics %>% 
            group_by_(input$var3) %>% 
            summarise(covariance = cov(get(input$var1), get(input$var2)), 
                      correlation = cor(get(input$var1), get(input$var2)))
        }
      }
      else { # var 1 is quantitative var2 is categorical
        summary_tbl <- colics %>% 
          group_by_(input$var2) %>% 
          summarise(minimum = min(get(input$var1)), 
                    Q1 = quantile(get(input$var1), 0.25),
                    median = median(get(input$var1)),
                    mean = mean(get(input$var1)),
                    Q3 = quantile(get(input$var1), 0.75),
                    max = max(get(input$var1)))
      }
    }
    else { # var1 is categorical
      if (input$var2 == '--none--') { # var2 is not selected
        summary_tbl <- as.data.frame(table(colics[input$var1]))
        names(summary_tbl) <- c(input$var1, "Frequency")
      }
      else if (input$var2 %in% num_cols) { # var1 is categorical, var2 is quantitative
        summary_tbl <- colics %>% 
          group_by_(input$var1) %>% 
          summarise(minimum = min(get(input$var2)), 
                    Q1 = quantile(get(input$var2), 0.25),
                    median = median(get(input$var2)),
                    mean = mean(get(input$var2)),
                    Q3 = quantile(get(input$var2), 0.75),
                    max = max(get(input$var2)))
      }
      else { # var 1 and  var2 are categorical
        summary_tbl <- data.frame(table(colics[[input$var1]], colics[[input$var2]]))
        names(summary_tbl) <- c(input$var1, input$var2, "Frequency")
      }
    }
    return(summary_tbl)
  })
  
  output$plot <- renderPlotly({ 

    if (input$var1 == '--none--'){ # var1 is not selected
      g <- ggplot(colics) + 
        geom_blank() + 
        labs(title = "Select a Variable")
    }
    else if (input$var1 %in% num_cols) { # var1 is quantitative
      if (input$var2 == '--none--') { # var 2 is not selected
        g <-ggplot(colics, aes_string(x = input$var1)) + 
          geom_histogram() + 
          labs(x = input$var1)
      }
      else if (input$var2 %in% num_cols){ # var2 is quantitative
        if (input$var3 == '--none--'){ # var3 is not selected
          g <- ggplot(colics, aes_string(x = input$var1, y = input$var2)) +
            geom_point(alpha = 0.6) + 
            labs(x = input$var1, y = input$var2)
        }
        else { # var1 and var2 are quantitative, var3 is selected
          g <- ggplot(colics, aes_string(x = input$var1, y = input$var2)) +
            geom_point(aes_string(fill=input$var3), size = 4, stroke = 0, 
                       alpha = 0.6) + 
            labs(x = input$var1, y = input$var2, fill = input$var3) + 
            theme(legend.title = element_text(face = "bold", size = 10))
        }
      }
      else { # var2 is categorical
        g <- ggplot(colics, aes_string(x = input$var2, y = input$var1)) + 
          geom_boxplot() + 
          labs(x = input$var2, y = input$var1)
      }
    }
    else { # var1 is categorical
      if (input$var2 == '--none--') { # var2 is not selected
        g <- ggplot(colics, aes_string(input$var1)) + 
          geom_bar() + 
          labs(x = input$var1)
      }
      else if (input$var2 %in% num_cols) { # var2 is quantitative
        g <- ggplot(colics, aes_string(x = input$var1, y = input$var2)) + 
          geom_boxplot() + 
          labs(x = input$var1, y = input$var2)
      }
      else { # var2 is categorical
        g <- ggplot(colics, aes_string(input$var1)) + 
          geom_bar(aes_string(fill = input$var2)) + 
          labs(x = input$var1, fill = input$var2) + 
          theme(legend.title = element_text(face = "bold", size = 10))
      }
    }
    return(ggplotly(g))
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  # Check boxes to choose columns
  output$select_pca_vars <- renderUI({
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("pca_vars", "Choose variables to include:", 
                       choices  = num_cols,
                       selected = num_cols)
  })
  
  output$mtry_selector <- renderUI({
    sliderInput("mtry", "number of predictors sampled at each split",
                round(length(input$predictors)/3, 0), min = 1, 
                max = length(input$predictors), step = 1)
  })
  
  output$select_predictors <- renderUI({
    
    predictors <- colics %>% select(-c(input$target))
    predictors <- names(predictors)
    
    # # Create the checkboxes and select them all by default
    # checkboxGroupInput("predictors", "Choose predictors to include:", 
    #                    choices  = predictors,
    #                    selected = predictors)
    selectizeInput("predictors", "Choose predictor variables to include in the model:",
                   choices = setNames(predictors, predictors),
                   multiple = TRUE)
  })
  
  output$new_point <- renderUI({
    
    dynamic_list <- lapply(input$predictors, function(p) {
      if (p %in% num_cols){
        numericInput(paste0("new_", p), p, median(colics[[p]]), 
                     min=min(colics[[p]]), max=max(colics[[p]]))
      }
      else {
        selectInput(paste0("new_", p), p, choices=levels(colics[[p]]))
      }
    })
    
    do.call(tagList, dynamic_list)
  })
  
  
  observeEvent(input$train, {
    # show a progress bar
    withProgress(message = "Model is Training", style = 'old', value = 0.5, {  
      
      # isolate variables so they're not affected if user changes input
      model_type_static <<- isolate(input$model_type)
      if (model_type_static == "Random Forest"){
        mtry <- isolate(input$mtry)
        ntree <- isolate(input$ntree)
      }
      target <- isolate(input$target)
      target_static <<- isolate(input$target)
      predictors <- isolate(input$predictors)
      predictors_static <<- isolate(input$predictors)
      
      # build formula for models programmatically
      formula <- paste(target, '~')
      for (p in predictors){
        formula <- paste(formula, p, '+')
      }
      # get rid of trailing +
      formula <- substr(formula, 1, str_length(formula)-2)
      # cast as formula instead of character (needed for rf but not glm)
      formula <- as.formula(formula)
      
      # train the model based on type and make it a global variable
      if (model_type_static == "Logistic Regression"){
        model <<- glm(formula, data = train, family = "binomial")
      }
      else {
        model <<- randomForest(formula, data = train, mtry = mtry, 
                               ntree = ntree, importance = TRUE)
      }
    })
    
    output$model_summary <- renderPrint({
      if (exists("model")){
        if (model_type_static == "Random Forest"){
          return(print(model$importance))
        }
        else {
          return(print(summary(model)))
        }
      }
      else {
        return(NULL)
      }
    })

    
  })
  
  output$prediction <- renderText({
    
    if (!exists("model")) {
      message <- "Train a model before making a prediction."
    }
    else {
      message <- paste0("Predicted ", target_static, " for the new point is ", pred())
    }
    return(message)
  })
  
  pred <- reactive({
    
    dummy <- input$train
    
    if (exists("model") & exists("predictors_static")) {
      if (length(predictors_static) > 0) {
        temp <- train
        
        for (p in predictors_static){
          if (p %in% num_cols){
            temp[1, p] <- as.numeric(input[[paste0("new_", p)]])
          }
          else {
            temp[1, p] <- input[[paste0("new_", p)]]
          }
        }
        
        new_point <- temp[1, predictors_static]
        
        if (model_type_static == "Random Forest") {
          return(toString(predict(model, newdata = new_point)))
        }
        else {
          prob <- predict(model, newdata = new_point, type = "response")
          if (prob >= input$thresh) {
            return(levels(train[[target_static]])[2])
          }
          else {
            return(levels(train[[target_static]])[1])
          }
        }      
      } else {return(NULL)}
        
      } else {return(NULL)}
    
  })
  
  pca <- reactive({
    # Keep the selected columns
    # columns <- input$pca_vars
    # data_sub <- data[, columns, drop = FALSE]
    data_sub <- colics %>% select(input$pca_vars)

    pca_output <- prcomp(data_sub, 
                         center = (input$center_vars == 'Yes'), 
                         scale = (input$scale_vars == 'Yes'))
    # data.frame of PCs
    pcs_df <- cbind(colics, pca_output$x)
    
    return(list(data_sub = data_sub,
                pca_output = pca_output, 
                pcs_df = pcs_df))
  })
  
  output$biplot <- renderPlot({
    # would be good to figure out how not to cut off labels
    # set xlim and ylim to be twice that of the ranges of the pcs?
    # would also be good to vectorize which pcs are used for axes
    # with the option choices=c(1,3)
    return(biplot(pca()$pca_output))
  })
}
