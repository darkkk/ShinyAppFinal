
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
require(ref)
require(gridExtra)


shinyServer(function(input, output) {
  
  jobExpectationWrap <- function(p, goodS, badS, nyear)
  {
    decision = numeric(nyear)
    expval = numeric(nyear)
    mpay = numeric(nyear)
    
    dec_ = ref("decision")
    expval_ = ref("expval")
    mpay_ = ref("mpay")
    
    jobExpectations <- function(p, goodS, badS, ns, n, dec_, expval_, mpay_)
    {
      if(ns == n){
        deref(dec_)[n] = 1;
        deref(expval_)[n] = p*goodS + (1-p)*badS;
        deref(mpay_)[n] = badS;	
      }
      else{
        deref(mpay_)[ns] = ((n-ns)+1)*badS;
        jobExpectations(p, goodS,badS, ns+1,n, dec_, expval_, mpay_);
        expV = deref(expval_)[ns+1];
        deref(expval_)[ns] = (n-ns+1)*(p*goodS + (1-p)*badS);
        if(expV > deref(mpay_)[ns]){
          deref(dec_)[ns] = 0;
        }else{
          deref(dec_)[ns] = 1;
        }	
      }
    }
    jobExpectations(p,goodS,badS, 1,nyear,dec_,expval_, mpay_)
    return(list("OptimalDecision" = decision, "ExpectedIncome" = expval, "MinimumIncome" = mpay))
  }
  

  
  
  result <- reactive({
    p = input$sliderID1;
    goodS = input$numID2;
    badS = input$numID3;
    nyear = input$numID1;
    jobExpectationWrap(p,goodS,badS, nyear);  
  })
  
  breakYear <- reactive({
    zz = result()[[2]];
    zzz = result()[[3]];
    zz <- zz[-1]
    zzz <- zzz[-length(zz)]
    by <- (-1)*(zz-zzz);
    
    
  })
  
  output$incomes <- renderPlot({
    input$goButton
    isolate({
#       bpt = table(result()[[2]], result()[[3]])

        bpt = rbind(result()[[2]], result()[[3]])
#           matplot(bpt)

        par(mfrow = c(1,2))
        barplot(bpt, main="Income scenario",
          xlab="Years", col=c("darkblue","red"),
        legend = c("Expected Income", "Minimum Income"), beside=TRUE)


        barplot(breakYear(),main="Potential loss when accepting the worse job",
                xlab="Years", col=c("red"))
      
      
    })
    
    
  })  

  output$quick <- renderTable({
    input$goButton
    isolate({
      startWork = c(min(which(result()[[1]] == 1)))
      minIncome = round(sum(result()[[3]][startWork:length(result()[[3]])]),0)
      txt = c("Start work in year", "Minimal cumulative income");
      dfrQuick = data.frame("Result" = txt, "Value" = c(startWork, minIncome))
      
      
      
    })
    
    
  })
  
  output$decision <- renderPlot({
    input$goButton
    isolate({
      par(mfrow = c(1,1))
      
  
      barplot(result()[[1]], main = "How long to wait for good job offer", xlab = "Years", col = c("darkblue"),
              legend = c("It is more optimal to take bad job"))      
    })    
  })

#   output$oid1 <- renderPrint({a <- require(ref); print(a)})
# #   output$oid1 <- renderPrint(input$numID1)
# #   output$oid2 <- renderText(as.character(input$numID2))
#   output$oid2 <- renderText({
#     input$goButton
#     isolate(input$numID2)
#    
#     
#     })
  #check inputs
  
  output$varCheck <- renderTable({
      input$goButton
      isolate({
        rns = c("Variable", "Value");
        nams = c("Number of years", "Good salary", "Bad salary", "Probability");
        vals = c(input$numID1, input$numID2, input$numID3, input$sliderID1);
        
        tab = data.frame(nams, vals);
        colnames(tab) = rns;
        return(tab)        
      }
        
        )       
      })
  output$sliderID1 <- renderPrint({input$goButton; isolate(input$sliderID1)})

    




  
})
