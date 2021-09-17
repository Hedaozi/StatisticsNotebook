library(shiny)
library(ggplot2)
library(miniUI)

ui <- miniPage(
  miniTitleBar("The Central Limit Theorem (i.i.d.)"),
  
  miniTabstripPanel(
    miniTabPanel(
      "Parameters",
      miniContentPanel(
        h3("Size"),
        sliderInput("population", "Population Size",
                    10000, 50000, 30000),
        sliderInput("sample", "Sample Size",
                    100, 10000, 3000),
        sliderInput("sampling", "Sampling Times",
                    100, 10000, 3000),
        h3("Distribution"),
        selectInput("distribution", "Distribution of Population",
                    c("Normal", "Uniform", "Exponential",
                      "F", "Gamma", "Geometric", "t",
                      "Hypergeometric", "Lognormal", "Logistic",
                      "Negative Binomial", "Poisson", "Chi Square")),
        uiOutput("distributionArguments"),
        actionButton("go", "GO!")
      )
    ),
    miniTabPanel(
      "Plot",
      miniContentPanel(
        plotOutput("populationPlot"),
        plotOutput("samplingPlot"),
        sliderInput("bins", "Number of Bins", 1, 50, 30),
        checkboxGroupInput(
          "plotSetting", "Plot Elements: ",
          c("Remove Normal Curve" = "rm.nor",
            "Remove Density Curve" = "rm.den"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$go, {
        p <- input$population
        
        # Generate Population
        population_vector <- switch (
            input$distribution,
            Normal = rnorm(p, input$args1, input$args2), 
            Uniform = runif(p, input$args1, input$args2), 
            Exponential = rexp(p, input$args1),
            `F` = rf(p, input$args1, input$args2), 
            Gamma = rgamma(p, input$args1, input$args2), 
            Geometric = rgeom(p, input$args1), 
            t = rt(p, input$args1),
            Hypergeometric = rhyper(p, input$args1, input$args2, input$args3), 
            Lognormal = rlnorm(p, input$args1, input$args2), 
            Logistic = rlogis(p, input$args1, input$args2),
            `Negative Binomial` = rnbinom(p, input$args1, input$args2),
            Poisson = rpois(p, input$args1), 
            `Chi Square` = rchisq(p, input$args1)
        )
        
        population_df <- data.frame(value = population_vector)
        
        mean_population <- mean(population_vector, na.rm = T)
        sd_population <- sd(population_vector, na.rm = T)
        
        mean_list <- list()
        for (i in 1:input$sampling){
            sample_tmp <- sample(population_vector, 
                                 input$sample, replace = T)
            mean_tmp <- mean(sample_tmp)
            mean_list <- c(mean_list, list(mean_tmp))
        }
        sample_df <- data.frame(means = unlist(mean_list))
        
        mean_sampling <- mean(sample_df$means, na.rm = T)
        sd_sampling <- sd(sample_df$means, na.rm = T)
        n_sample <- input$sample
        
        captionPopulation = paste0(
            "Mean of Population: ", formatC(mean_population, 5, format = "f"),
            ", Sd of Population: ", formatC(sd_population, 5, format = "f"))
        
        captionSampling = paste0(
            "Observation of Samples' Mean: ", 
            formatC(mean_sampling, 5, format = "f"),
            ", Observation of Standard Error:  ", 
            formatC(sd_sampling, 5, format = "f"), "\n",
            "Theoretical Value of Mean of Samples' Mean: ",
            formatC(mean_population, 5, format = "f"),
            ", Theoretical Value of Standard Error: ",
            formatC(sd_population / sqrt(n_sample), 5, format = "f"), "\n",
            "Sample Size: ", n_sample,
            ", Sampling Times: ", input$sampling)
        
        output$populationPlot <- renderPlot({
            ggplot(population_df, aes(x = value)) +
                geom_histogram(bins = input$bins, fill = "darkgray", 
                               color = "white") +
                labs(x = NULL, caption = captionPopulation,
                     title = "Population Distribution") +
                theme_bw()
        })
        
        output$samplingPlot <- renderPlot({
            rm1 <- "rm.nor" %in% input$plotSetting
            rm2 <- "rm.den" %in% input$plotSetting
            
            if (!rm1 & !rm2){
                ggplot(sample_df, aes(x = means)) +
                    geom_histogram(aes(y = ..density..), bins = input$bins, 
                                   fill = "darkgray", color = "white") +
                    geom_density(size = 0.75, color = "red") +
                    stat_function(fun = dnorm, 
                                  args = list(mean = mean_population,
                                              sd = sd_population / sqrt(n_sample)),
                                  color = "red", size = 0.75, linetype = 2) +
                    labs(x = NULL, caption = captionSampling,
                         title = "Sampling Distribution of Mean") +
                    theme_bw() -> samplingPlot
            }
            
            if (rm1 & !rm2){
                ggplot(sample_df, aes(x = means)) +
                    geom_histogram(aes(y = ..density..), bins = input$bins, 
                                   fill = "darkgray", color = "white") +
                    geom_density(size = 0.75, color = "red") +
                    labs(x = NULL, caption = captionSampling,
                         title = "Sampling Distribution of Mean") +
                    theme_bw()  -> samplingPlot
            }
            
            if (!rm1 & rm2){
                ggplot(sample_df, aes(x = means)) +
                    geom_histogram(aes(y = ..density..), bins = input$bins, 
                                   fill = "darkgray", color = "white") +
                    stat_function(fun = dnorm, 
                                  args = list(mean = mean_population,
                                              sd = sd_population / sqrt(n_sample)),
                                  color = "red", size = 0.75, linetype = 2) +
                    labs(x = NULL, caption = captionSampling,
                         title = "Sampling Distribution of Mean") +
                    theme_bw() -> samplingPlot
            }
            
            if (rm1 & rm2){
                ggplot(sample_df, aes(x = means)) +
                    geom_histogram(aes(y = ..density..), bins = input$bins, 
                                   fill = "darkgray", color = "white") +
                    labs(x = NULL, caption = captionSampling,
                         title = "Sampling Distribution of Mean") +
                    theme_bw() -> samplingPlot
            }
            
            return(samplingPlot)
        })
    })
    
    output$distributionArguments <- renderUI({
        if (input$distribution == "Normal"){
            return(tagList(
                sliderInput("args1", "Mean", -100, 100, 0),
                sliderInput("args2", "Standard Deviation", 0.1, 10, 1)
            ))
        }
        if (input$distribution == "Uniform"){
            return(tagList(
                sliderInput("args1", "Min", -100, 100, 0),
                uiOutput("unif_max")
            ))
        }
        if (input$distribution == "Exponential"){
            return(tagList(
                sliderInput("args1", "Rate", 0.1, 10, 1)
            ))
        }
        if (input$distribution == "F"){
            return(tagList(
                sliderInput("args1", "Degree of Freedom 1", 1, 100, 1),
                sliderInput("args2", "Degree of Freedom 2", 1, 100, 10)
            ))
        }
        if (input$distribution == "Gamma"){
            return(tagList(
                sliderInput("args1", "Shape", 0.1, 10, 1),
                sliderInput("args2", "Rate", 0.1, 10, 1),
                p("Scale is set to 1 / rate.")
            ))
        }
        if (input$distribution == "Geometric"){
            return(tagList(
                sliderInput("args1", "Probability", 0.00, 1, 0.5)
            ))
        }
        if (input$distribution == "t"){
            return(tagList(
                sliderInput("args1", "Degree of Freedom", 1, 100, 20)  
            ))
        }
        if (input$distribution == "Hypergeometric"){
            return(tagList(
                sliderInput("args1", "m", 1, 100, 3),
                sliderInput("args2", "n", 1, 100, 5),
                uiOutput("hyper_k")
            ))
        }
        if (input$distribution == "Lognormal"){
            return(tagList(
                sliderInput("args1", "Mean of Log", -10, 10, 0),
                sliderInput("args2", "Standard Deviation of Log", 0.1, 10, 1)
            ))
        }
        if (input$distribution == "Logistic"){
            return(tagList(
                sliderInput("args1", "Location", -100, 100, 0),
                sliderInput("args2", "Scale", 0.1, 10, 1)
            ))
        }
        if (input$distribution == "Negative Binomial"){
            return(tagList(
                sliderInput("args1", "Size", 1, 100, 10),
                sliderInput("args2", "Probability", 0.00, 1, 0.5)
            ))
        }
        if (input$distribution == "Poisson"){
            return(tagList(
                sliderInput("args1", "Lambda", 0, 100, 1)
            ))
        }
        if (input$distribution == "Chi Square"){
            return(tagList(
                sliderInput("args1", "Degree of Freedom", 0, 100, 1)
            ))
        }
    })
    output$hyper_k <- renderUI({
        return(tagList(
            sliderInput("args3", "k", 1, input$args1 + input$args2, 1)
        ))
    })
    output$unif_max <- renderUI({
        return(tagList(
            sliderInput("args2", "Max", input$args1 - 100, 
                        input$args1 + 100, input$args1 + 1)
        ))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
