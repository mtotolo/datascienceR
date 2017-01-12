library(ggplot2)
library(caret)
data("mtcars")
#data("swiss")
inTrain<- createDataPartition(mtcars$mpg,p=0.6,list=FALSE)
training<-mtcars[inTrain,]
testing<-mtcars[-inTrain,]
mtcars$type<<-"train"
mtcars$type[-inTrain]<<-"test"
i<-0
shinyServer(  
      function(input, output) {
            x<- reactive({
                  toPred<-input$toPred
                  predictor<-input$predictor
                  if (input$calButton==i) 
                  {
                        i<<-i+1
                        inTrain<- createDataPartition(mtcars[,toPred],
                                                      p=0.6,list=FALSE)
                        training<-mtcars[inTrain,]
                        testing<-mtcars[-inTrain,]
                        mtcars$type<<-"train"
                        mtcars$type[-inTrain]<<-"test"
                  }
                  deg<-input$deg
                  str1<-""
                  for (i in 0:deg) {
                        if (i==0) {str1<-"1"}
                        else {
                              str1<-paste(str1,"+ I(",predictor,"^",i,")")
                        }
                  }
                  #                  str1<-paste0("I(",predictor,"^",deg,")")
                  formula<-as.formula(paste(toPred,"~",str1))
                  fit1<-lm(formula,data=training)
                  str2<-""
                  str3<-""
                  for (i in 0:deg) {
                        if (i==0) {
                              str2<-fit1$coefficients[1]
                              str3<-round(fit1$coefficients[1],2)
                              
                              }
                        else {
                              str2<-paste0(str2,"+",fit1$coefficients[i+1],
                                           "*",predictor,"^",i)
                              str3<-paste0(str3," + ",round(fit1$coefficients[i+1],2),
                                           "*",predictor,"^",i) 
                        }
                  }
                  #                  str2<-paste(fit1$coefficients[1],"+",fit1$coefficients[2],
                  #                              "*wt^",deg)
                  c(str2,str3,predictor,toPred)
 #                 as.function(alist(wt=,eval(parse(text=str2))))
            })
            output$myPlot <- renderPlot({      
                  strfunc<-gsub(x()[3],"x",x()[1])
                  func<-as.function(alist(x=,eval(parse(text=strfunc))))
                  g<-ggplot(data=mtcars,aes(x=mtcars[,x()[3]],y=mtcars[,x()[4]],col=type)) +
                        xlab(x()[3])+ ylab(x()[4])
                  g+geom_point(size=4,alpha=0.6)+
                        stat_function(fun=func,col="blue",size=1)
                  })
            output$myTitle <- renderText({      
                  paste0("Polynomial fit of mtcars dataset: ",x()[4]," vs. ",x()[3])
            })
            output$inputValue <- renderText({
                  strfunc<-gsub(x()[3],"x",x()[1])
                  func<-as.function(alist(x=,eval(parse(text=strfunc))))
                  pred1<-func(mtcars[mtcars$type=="train",x()[3]])
                  trainErr<-RMSE(pred1,mtcars[mtcars$type=="train",x()[4]])
                  #trainErr<-sqrt((sum((pred1-training[,toPred])^2))/nrow(training))
                  pred2<-func(mtcars[mtcars$type=="test",x()[3]])
                  testErr<-RMSE(pred2,mtcars[mtcars$type=="test",x()[4]])
                  #testErr<-(sum((pred2-testing[,toPred])^2))/nrow(testing)
                  paste("In sample RMSE:",round(trainErr,3),"\nOut of sample RMSE:",
                        round(testErr,3))
                  })
            output$myString <- renderText({
                  a<-gsub("\\+ -","- ",x()[2])
                  a<-gsub("\\^1","",a)
                  paste0(x()[4]," = ",a)
            })
      }
)