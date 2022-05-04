library(shiny)
library(ggplot2)
library(mvtnorm) # for norm
library(MASS) # for lda


ui <- fluidPage(
  
  title = "Multivariate normal Data Creatater",
  titlePanel("Multivariate normal Data Creatater"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tags$h4("Design by Hans"),
      tags$a(href="https://github.com/hans0803", "This is my GitHub Link!"),
      
      tags$h6(""),
      downloadButton("downloadData", "Download Data"),
      
      selectInput(inputId = "random",
                  label = "Random:",
                  choices = c(FALSE, TRUE)
      ),
      
      sliderInput(inputId = "ncount",
                  label = "p (numbers of Multivariate normal):",
                  min = 2, max = 7, value = 2, step = 1,
                  animate = animationOptions(interval = 1000, loop = FALSE)
      ),
      
      sliderInput(inputId = "mrange",
                  label = "Range of mean (uniform(-x, x)):",
                  min = 1, max = 10, value = 1, step = 1,
                  animate = animationOptions(interval = 1000, loop = FALSE)
      ),
      
      sliderInput(inputId = "tcount",
                  label = "Group:",
                  min = 3, max = 7, value = 3, step = 1,
                  animate = animationOptions(interval = 1000, loop = FALSE)
      ),
      
      sliderInput(inputId = "resolution",
                  label = "Digits:",
                  min = 0, max = 7, value = 2, step = 1,
                  animate = animationOptions(interval = 1000, loop = FALSE)
      ),
      
      selectInput(inputId = "multicol",
                  label = "Collinearity:",
                  choices = c(FALSE, TRUE)
      ),
      textInput(inputId = 'who', 
                label = 'Which groups are collinearity?', "1,2"),
      
      
      selectInput(inputId = "balance",
                  label = "Balance (for groups)",
                  choices = c(TRUE, FALSE)
      ),
      textInput(inputId = 'vector', 
                label = 'Numbers of each group', "100,100,100")
      
    ),
    
    mainPanel(
      
      
      
      tabsetPanel(selected="PCA",
                  tabPanel("PCA", 
                           tags$h3("This is PCA plot"),
                           plotOutput(outputId = "distPlot1"),
                           tags$h3("Mean of every P in every classes"),
                           tableOutput("view1")
                  ),
                  tabPanel("LDA", 
                           tags$h3("This is LDA plot"),
                           plotOutput(outputId = "distPlot2"),
                           tags$h3("Mean of every P in every classes"),
                           tableOutput("view2")
                  ),
                  tabPanel("t-SNE", 
                           tags$h3("This is t-SNE plot"),
                           plotOutput(outputId = "distPlot3"),
                           tags$h3("Mean of every P in every classes"),
                           tableOutput("view3")
                  )
      ),
      
    )
  )
)

server <- function(input, output) {
  
  output$distPlot1 <- renderPlot({
    
    if(input$balance==FALSE){
      n <- as.numeric(unlist(strsplit(input$vector,",")))
    }else{
      n <- rep(100, input$tcount)
    }
    
    if(input$random==FALSE){
      set.seed(611011106)
    }else{
      set.seed(sample(1:10000, 1))
    }
    
    d_mean <- c()
    d_df   <- data.frame()
    
    for(i in 1:input$tcount){
      
      now_mean <- runif(input$ncount, -input$mrange, input$mrange)
      d_mean   <- rbind(d_mean, now_mean)
      now_df   <- as.data.frame(rmvnorm(n=n[i], mean=now_mean))
      now_df   <- round(now_df, input$resolution)
      now_df   <- cbind(as.character(i), now_df)
      d_df     <- rbind(d_df, now_df)
    }
    
    if(input$multicol==TRUE){
      whos <- as.numeric(unlist(strsplit(input$who,",")))
      whos_mean <- d_mean[whos[1],]
      
      for(who in whos[-1]){
        d_df[which(d_df[,1]==who),] <- cbind(who, rmvnorm(n=n[who], mean=whos_mean)+rnorm(n[who]))
        d_mean[who,] <- whos_mean
      }
    }
    
    d_df[,-1] <- round(d_df[,-1], input$resolution)
    colnames(d_df)[1] <- "class"
    
    output$view1 <- renderTable({
      mean_class <- as.data.frame(1:input$tcount)
      colnames(mean_class) <- "class"
      d_mean <- cbind(mean_class, d_mean)
      d_mean
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Hans_MultiData", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(d_df, file, row.names = FALSE)
      }
    )
    
    Pca <- prcomp(d_df[, -1], center=TRUE, scale.=TRUE) 
    qplot(Pca$x[, 1], Pca$x[, 2], color=d_df$class)
  })
  
  
  output$distPlot2 <- renderPlot({
    
    if(input$balance==FALSE){
      n <- as.numeric(unlist(strsplit(input$vector,",")))
    }else{
      n <- rep(100, input$tcount)
    }
    
    if(input$random==FALSE){
      set.seed(611011106)
    }else{
      set.seed(sample(1:10000, 1))
    }
    
    d_mean <- c()
    d_df   <- data.frame()
    
    for(i in 1:input$tcount){
      
      now_mean <- runif(input$ncount, -input$mrange, input$mrange)
      d_mean   <- rbind(d_mean, now_mean)
      now_df   <- as.data.frame(rmvnorm(n=n[i], mean=now_mean))
      now_df   <- round(now_df, input$resolution)
      now_df   <- cbind(as.character(i), now_df)
      d_df     <- rbind(d_df, now_df)
    }
    
    if(input$multicol==TRUE){
      whos <- as.numeric(unlist(strsplit(input$who,",")))
      whos_mean <- d_mean[whos[1],]
      
      for(who in whos[-1]){
        d_df[which(d_df[,1]==who),] <- cbind(who, rmvnorm(n=n[who], mean=whos_mean)+rnorm(n[who]))
        d_mean[who,] <- whos_mean
      }
    }
    
    d_df[,-1] <- round(d_df[,-1], input$resolution)
    colnames(d_df)[1] <- "class"
    
    output$view2 <- renderTable({
      mean_class <- as.data.frame(1:input$tcount)
      colnames(mean_class) <- "class"
      d_mean <- cbind(mean_class, d_mean)
      d_mean
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Hans_MultiData", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(d_df, file, row.names = FALSE)
      }
    )
    
    mLda <- lda(class ~ ., data=d_df)
    Lda <- predict(mLda, d_df[, -1])
    qplot(Lda$x[, 1], Lda$x[, 2], color=d_df$class)
  })
}

shinyApp(ui = ui, server = server)



