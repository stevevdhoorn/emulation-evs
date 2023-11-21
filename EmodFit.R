
EmodFit = function(myvar  = 'pm25d', res = res_sa4, myloc = 'Perth - Inner', 
                     areaType = 'SA4_NAME16', molconv = 1){
  
  mycols = paste('X', seq(8), sep="")
  tmp = res[res[,areaType] == myloc, ]
  tmp2 = tmp[tmp$scen %in% paste('s',1:32, sep=""),]
  EmDesign<-tmp2[,mycols]
  ValDesign<-tmp[tmp$scen %in% paste('s',33:48, sep=""), mycols]
  emOut = tmp2[,myvar]*molconv
  valOut = tmp[tmp$scen %in% paste('s',33:48, sep=""),myvar]*molconv 
  
  EmModel<-km(formula=~., design=EmDesign, response=emOut, covtype="gauss", optim.method="BFGS", control=list(maxit=500))
  print(EmModel)
  plot(EmModel)
  
  # Validation
  ValPred<-predict(object=EmModel,newdata=data.frame(ValDesign),type="UK",checkNames=FALSE,light.return=TRUE)
  #ValPred
  #ValPred$mean
  #ValPred$lower95
  #ValPred$upper95
  
  valData = data.frame(pred = ValPred$mean, 
                       lower = ValPred$lower95, upper = ValPred$upper95,
                       ctmpred = valOut)
  
  mywidth = diff(range(emOut))*0.03
  # Initialize ggplot with data
  p <- ggplot(
    valData, 
    aes(x = ctmpred, y = pred, ymin = lower, ymax = upper)
  )
  p = p + geom_errorbar(width = mywidth) + geom_point(size = 0.7, col="red") +
    geom_abline(col="green",lwd=1,lty=3) + 
    xlab("CTM predictions") + ylab("Emulator predictions") +
    theme(text = element_text(size = 18)) +
    theme(plot.margin = margin(1,1,1,1, "cm"))
  return(list(p=p, EmModel=EmModel))
}
