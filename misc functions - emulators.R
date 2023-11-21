
### load relevant R libraries
library(cowplot)
library(rlang)
library(ggtext)
library(grid)

element_textbox_highlight <- function(..., hi.labels = NULL, hi.fill = NULL,
                                      hi.col = NULL, hi.box.col = NULL) {
  structure(
    c(element_textbox(...),
      list(hi.labels = hi.labels, hi.fill = hi.fill, hi.col = hi.col, hi.box.col = hi.box.col)
    ),
    class = c("element_textbox_highlight", "element_textbox", "element_text", "element")
  )
}

element_grob.element_textbox_highlight <- function(element, label = "", ...) {
  if (label %in% element$hi.labels) {
    element$fill <- element$hi.fill %||% element$fill
    element$colour <- element$hi.col %||% element$colour
    element$box.colour <- element$hi.box.col %||% element$box.colour
  }
  NextMethod()
}

myCircles = circleGrob(x=c(0.35,0.35), y=c(0.6, 0), r=0.13, 
                       gp = gpar(col = "black", lty = 1, fill = c('#4A618C', '#F89096')))
myText1 = textGrob(label = 'Source of Emissions', x=0.4, y=1, just = "left")
myText2 = textGrob(label = c(' On-road Vehicles', ' Electricity'), x=c(0.4,0.4), y=c(0.6, 0), just = "left")

assoc = function(EmModel = EmModel, mylaby = 'Delta PM2.5 (ugm-3)'){
  
  tmp = data.frame(X1=1, X2=1, X3=1, X4=1, X5=1, X6=1, X7=1, X8=1)
  df = as.data.frame(lapply(tmp, rep, 1501))
  myres = NULL
  for (i in seq(8)){
    mydat = df
    thisvar = paste('X', i, sep="")
    thisplot = paste('p', i, sep="")
    x = seq(from=0, to=1.5, by=0.001)
    mydat[[thisvar]] = x
    myPred<-predict(object=EmModel,newdata=mydat,type="UK",checkNames=FALSE,light.return=TRUE)
    outdat = data.frame(var = thisvar, x, myPred=myPred$mean)
    myres = rbind(myres, outdat)
  }
  myres$myxlab = factor(myres$var, levels = paste('X', seq(8), sep=""), 
                        labels = c("NOx_EMS", "VOC_EMS", "PM2.5_EMS", "CO_EMS", "SO2_EMS", "NH3_EMS", "GAS_EMS", "COAL_EMS"))
  
  p=ggplot(myres) + 
    geom_line(aes(x=100*x, y=myPred)) + 
    facet_wrap(~myxlab, nrow = 3) +
    xlab("Percentage of baseline (%)") + ylab(mylaby) + 
    theme_bw() +
    theme(plot.margin = margin(3, 3, 3, 3, unit = "pt"),
          strip.background = element_blank(),
          strip.text = element_textbox_highlight(
            size = 12,
            color = "white", fill = "#5D729D", box.color = "#4A618C",
            halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
            padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3),
            # this is new relative to element_textbox():
            hi.labels = c("GAS_EMS", "COAL_EMS"),
            hi.fill = "#F89096", hi.box.col = "#A6424A", hi.col = "black"
          )
    ) +
	scale_y_continuous(labels = unicode_minus)
  
  #p = p + theme(plot.margin = unit(c(3, 1, 1, 1), "cm"))
  
  grid.newpage()
  grid.draw(p)
  vp = viewport(x=.8, y=.2, width=.35, height=.1)
  pushViewport(vp)
  grid.draw(myCircles)
  #grid.draw(myText1)
  grid.draw(myText2)
  upViewport()
  
  g = grid.grab(wrap.grobs=TRUE)
  #outpath = './output/'
  #ggsave(filename = paste(outpath, outfile, '.png', sep=""), dpi = 600, type = "cairo")
  return(g)
}

EMmean_fun<-function(Xnew,m)
{
  predict.km(m,Xnew,"SK",se.compute=FALSE,checkNames=FALSE)$mean
}
DistVec<-rep("qunif",8)
DistArgsList<-list(list(min=0,max=1.5), list(min=0,max=1.5), list(min=0,max=1.5), list(min=0,max=1.5),
                   list(min=0,max=1.5), list(min=0,max=1.5), list(min=0,max=1.5), list(min=0,max=1.5))

myVC = function(EmModel = EmModel){
  
  SAout<-fast99(model=EMmean_fun,factors=c("NOx", "VOC", "PM", "CO", "SO2", "NH3", "GAS", "COAL"),n=5000,q=DistVec,q.arg=DistArgsList,m=EmModel)
  plot(SAout)
  
  MainEffect<-SAout$D1/SAout$V
  
  print(paste('Main Effect:', 
              c("NOx", "VOC", "PM", "CO", "SO2", "NH3", "GAS", "COAL"), ':', 
              round(100*MainEffect,1)))
  TotalEffect<-1-(SAout$Dt/SAout$V)
  TotalEffect
  Interaction<-TotalEffect-MainEffect
  print(paste('Interaction:', round(100*sum(Interaction),1)))
  return(MainEffect)
  
}

EmPred = function(res=res_sa4wgt, scendat=scendat, regs=regs, myvar=myvar, molconv = 1){
  
  #browser()
  results = data.frame(regs = regs)
  
  for (k in 1:nrow(scendat)){
    thisscen = scendat[k, paste('X', seq(8), sep="")]  
    regres = NULL
    #browser()
    for (reg in regs){
      tmp = res[res[,2] == reg,]
      tmp2 = tmp[tmp$scen %in% paste('s',1:32, sep=""),]
      EmDesign<-tmp2[,paste('X', seq(8), sep='')]
      #ValDesign<-tmp[tmp$scen %in% paste('s',49:64, sep=""),paste('X', seq(8), sep='')]
      myOut = tmp2[,myvar]*molconv 
      #valOut = tmp[49:64,myvar] 
      
      EmModel<-km(formula=~.,design=EmDesign,response=myOut,covtype="gauss",optim.method="BFGS",control=list(maxit=500))
      myPred<-predict(object=EmModel,newdata=thisscen,type="UK",checkNames=FALSE,light.return=TRUE)
      regres = c(regres, myPred$mean)
    }
    newdat = data.frame(regs = regs, regres)  
    names(newdat)[2] = scendat$myscen[k]
    #browser()
    results = merge(results, newdat, sort=FALSE)
  }
  results
}

localEm = function(areatype = 'sa4', area_ems = area_ems, scendat=scendat, regs=regs, myvar=myvar, molconv = 1){
  
  #browser()
  results = data.frame(regs = regs)
  
  for (k in 1:nrow(scendat)){
    thisscen = scendat[k, paste('X', seq(8), sep="")]  
    regres = NULL
    #browser()
    for (j in 1:length(regs)){
	  reg = regs[j]
      mymod = area_ems[[j]]
      myPred<-predict(object=mymod,newdata=thisscen,type="UK",checkNames=FALSE,light.return=TRUE)
      regres = c(regres, myPred$mean)
    }
    newdat = data.frame(regs = regs, regres)  
    names(newdat)[2] = scendat$myscen[k]
    #browser()
    results = merge(results, newdat, sort=FALSE)
  }
  results
}