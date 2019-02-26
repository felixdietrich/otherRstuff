### Plot ----

emptyplot <- function(x) {
  plot(x, type='n', xaxt='n', yaxt='n', xlab='', ylab='', bty='n')
}

fboxplot <- function(x, names=NULL, main=NULL, mtext=NULL) {
  # http://rgraphgallery.blogspot.ch/2013/04/rg81-plotting-scatter-plot-with-means.html
  if(class(x)!='list') { return('Error: Needs lists as argument!') } # stop?
  names(x) <- c(1:length(x)) # to ensure that variables L1 is numeric
  xx <- reshape2::melt(x)
  # with (xx, plot(L1, value, pch = "-", cex=1.5, col = rgb(red=0, green=0.5, blue=0.5, alpha=0.25), xlab='', ylab='', xaxt='n', main=main) )
  with (xx, plot(L1, value, pch = "-", cex=1.5, col = rgb(red=0, green=0, blue=1, alpha=0.25), xlab='', ylab='', xaxt='n', main=main) )
  mtext(mtext)
  if(is.null(names)) axis(1, at=unique(xx$L1), labels=unique(xx$L1))
  if(!is.null(names)) { axis(1, at=unique(xx$L1), labels=names) }
  out1 <- data.frame (with (xx,  tapply( value, factor(L1), mean )) )
  names(out1) <- c("meanY")
  out1$grp <- rownames (out1)
  # points (out1$grp, out1$meanY, type = "b", col = "red", pch = 19)
  points (out1$grp, out1$meanY, col = "red", pch = 19)
}

fboxplotdouble <- function(x, y=NULL, names=NULL, main=NULL, mtext=NULL) {
  # http://rgraphgallery.blogspot.ch/2013/04/rg81-plotting-scatter-plot-with-means.html
  if(class(x)!='list') { return('Error: Needs lists as argument!') } # stop?
  names(x) <- c(1:length(x)) # to ensure that variables L1 is numeric
  xx <- reshape2::melt(x)
  if(!is.null(y)) yy <- reshape2::melt(y)
  valuez = c(xx[,'value'],yy[,'value'])
  # with (xx, plot(L1, value, pch = "-", cex=1.5, col = rgb(red=0, green=0.5, blue=0.5, alpha=0.25), xlab='', ylab='', xaxt='n', main=main) )
  with (xx, plot(L1, value, pch = "-", cex=1.5, col = rgb(red=0, green=0, blue=1, alpha=0.25), ylim=c(min(valuez, na.rm=T), max(valuez, na.rm=T)), xlab='', ylab='', xaxt='n', main=main) )
  par(new=T)
  with (yy, plot(L1, value, pch = "-", cex=1.5, col = rgb(red=0, green=0, blue=1, alpha=0.25), ylim=c(min(valuez, na.rm=T), max(valuez, na.rm=T)), axes=F, xlab='', ylab='', main=main) )
  mtext(mtext)
  if(is.null(names)) axis(1, at=unique(xx$L1), labels=unique(xx$L1))
  if(!is.null(names)) { axis(1, at=unique(xx$L1), labels=names) }
  out1 <- data.frame (with (xx,  tapply( value, factor(L1), mean )) )
  names(out1) <- c("meanY")
  out1$grp <- rownames (out1)
  points (out1$grp, out1$meanY, col = "red", pch = 19)
  out2 <- data.frame (with (yy,  tapply( value, factor(L1), mean )) )
  names(out2) <- c("meanY")
  out2$grp <- rownames (out2)
  points (out2$grp, out2$meanY, col = "green", pch = 19)
}

plotribbon <- function(x, main='', col='black', ribbon='red') { 
  xx <- as.zoo(x)
  # xx <- as.zoo(dat[,c('Close','e1','e2')])
  plot(xx[,1], col='black', xlab='', ylab='', ylim=c(min(xx[,c(1:3)], na.rm=T),max(xx[,c(1:3)], na.rm=T)), type='n') 
       # xaxt='n', yaxt='n', type='n', bty='n')
  polygon(c(index(xx), rev(index(xx))), 
          c(as.numeric(xx[,2]), as.numeric(rev(xx[,3]))),
          col='red',border=NA)
  
  # ### CHECK HOW TO REMOVE NA
  # # https://stackoverflow.com/questions/33372389/how-to-draw-a-polygon-around-na-values-in-r
  # polygon(c(index(xx), rev(index(xx))), 
  #         c(rep(1.04,200),rep(1.04,365), rep(1.06,565)),
  #         col='red',border=NA)
  # c(rep(1.04,200),rep(NA, 50),rep(1.04,315))
  # polygon(c(index(xx), rev(index(xx))), 
  #         c(rep(1.04,200),rep(NA, 50),rep(1.04,315), rep(1.06,565)),
  #         col='red',border=NA)
  # 
  lines(xx[,1], col=col, xlab='', ylab='', main=main)
}

plotribbongg <- function(x, main=NULL, col='black', ribbon='red') { 
  # require(ggplot2)
  # https://stackoverflow.com/questions/33372389/how-to-draw-a-polygon-around-na-values-in-r
  xx <- as.data.frame(x)
  # autoplot(dat[,c('Close')])
  p <- ggplot(xx,aes(x=as.Date(rownames(xx)),y=xx[,1])) + 
    geom_ribbon(aes(ymax=xx[,3],ymin=xx[,2]), fill=ribbon) + # adjustcolor("blue",.2)
    geom_line() + xlab("") + ylab("")
  if(!is.null(main)) p <- p + ggtitle(gsub('.Open','',colnames(x)[1])) + theme(plot.title = element_text(hjust = 0.5))
  p
}

plotzoo <- function(x, main='', col='black', legend='false', cex.legend = 0.8,
                        xaxt='t', lty=NULL, lwd=NULL, plot.type = 'single', save='', mtext=NULL, ...) { 
  if(is.null(lty)) lty <- rep(1, ncol(x))
  if(is.null(lwd)) lwd <- rep(1, ncol(x))
  plot.zoo(x, col=col, main=main, xaxt=xaxt, plot.type = plot.type, lty=lty, lwd=lwd, xlab='', ylab='', ...)
  mtext(mtext)
  if(legend!='false') legend(legend,colnames(x),lty=1,col=col, cex=cex.legend) 
  if(save!='') { 
    pdf(save)
    plot.zoo(x, col=col, main=main, xaxt=xaxt, plot.type = plot.type, lty=lty, lwd=lwd, xlab='', ylab='', ...)
    mtext(mtext)
    if(legend!='false') legend(legend,colnames(x),lty=1,col=col, cex=cex.legend) 
    dev.off() 
  }
}

# legend needs the position as argument
doubleplot <- function(x,y=NULL,main='',omit=FALSE,align=TRUE,
                       col1='black',col2='red',cex.main=NULL,legend='false',xaxt='t',density=NULL,polygon=FALSE,...) { # y=NULL new
  if (is.null(y)) xx=as.zoo(x)
  if (class(index(x))[1]!=class(index(y))[1]) { # [1] falls z.B. "POSIXct" "POSIXt" 
    if (class(index(x))=='yearmon') index(y)=as.yearmon(index(y))
    if (class(index(y))=='yearmon') index(x)=as.yearmon(index(x)) }
  if (omit==TRUE) xx=as.zoo(na.omit(cbind(x,y)))
  if (omit==FALSE) xx=as.zoo(cbind(na.omit(x),na.omit(y))) # xx=as.zoo(cbind(x,y))
  if (align==FALSE) {
    if (polygon==FALSE) { 
      plot(xx[,2], col=col2, lty=2, xlab='', yaxt='n', ylab='', xaxt=xaxt); axis(side = 4) }
    if (polygon==TRUE) { 
      plot(xx[,2], col=col2, type='n', xlab='', yaxt='n', ylab='', xaxt=xaxt)
      polygon(c(index(xx), rev(index(xx))),c(rep(min(xx[,2], na.rm=T),nrow(xx)),rev(xx[,2])),col=col2,density=density,border=NA) ### nrow(asd) -> nrow(xx)
      axis(side = 4) }
    par(new = T)
    plot(xx[,1], col=col1, main=main, cex.main=cex.main, xlab='', ylab='', xaxt=xaxt)
    if(legend!='false') legend(legend,colnames(xx),lty=1,col=c(col1,col2), cex=0.8) }
  if (align==TRUE) {
    plot(xx[,1], col=col1, main=main, xaxt=xaxt, xlab='', ylab='', ylim=c(min(xx, na.rm=T),max(xx, na.rm=T)))
    if(legend!='false') legend(legend,colnames(xx),lty=1,col=c(col1,col2), cex=0.8) # c('DBCFHX','USDJPY25R1M')
    lines(xx[,2], col=col2) }
}

facet_plot <- function(x,y=NULL,option='base',main=NULL,timex='time',valuex='value') { # 
  require(ggplot2)
  require(reshape2)
  if(!is.null(y)) x <- cbind(x,y)
  if(option=='top') {
    print('option: top')
    # require(ggplot2)
    # require(reshape2)
    # localenv <- environment()
    print( ggplot( melt(data.frame(time=index(x), x), id.vars="time"), aes(time, value)) + # environment = localenv
             geom_line() + labs(x = NULL, y = NULL, title = main) +
             facet_wrap( ~ variable, scale = "free_y", ncol=1) + theme(plot.title = element_text(hjust = 0.5)) )
    # print( ggplot( asd, aes_string(timex, valuex)) + # environment = localenv
    #   geom_line() + labs(x = NULL, y = NULL, title = main) +
    #   facet_wrap( ~ variable, scale = "free_y", ncol=1) + theme(plot.title = element_text(hjust = 0.5)) )
  }
  if(option=='right') {
    print('option: right')
    # require(ggplot2)
    # require(reshape2)
    print( ggplot( melt(data.frame(time=time(x), x), id.vars="time"), aes(time, value)) + 
             geom_line() + labs(x = NULL, y = NULL, title = main) +
             facet_grid(variable ~ ., scale = "free_y") + theme(plot.title = element_text(hjust = 0.5)) )
  }
  if(option=='base') {
    print('option: base')
    require(timeSeries)
    plot(as.timeSeries(x), format = "%B %Y", xlab='', main=main)
  }
}

facetplot <- function(df,colnames=NULL) { ### OLD VERSION
  # https://learnr.wordpress.com/2009/05/18/ggplot2-three-variable-time-series-panel-chart/
  require(ggplot2)
  require(reshape2)
  if(!is.null(colnames)) colnames(df) <- colnames
  df_melt = melt(data.frame(date=index(df), coredata(df)), id.vars = 'date')
  ggplot(df_melt, aes(x = date, y = value)) + 
    geom_line() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + 
    facet_wrap(~ variable, scales = 'free_y', ncol = 1)
}

smoothplot <- function(x, main=NULL, mtext=NULL, colors=seq(1:10), lty=NULL, lwd=NULL, spar = NULL,
                           legend='FALSE', colnames='FALSE', cex.legend=1) {
  if(ncol(x)==1) {
    xx <- na.omit(x)
    plot(zoo(smooth.spline(timeSeries::as.timeSeries(xx), spar = spar)$y, order.by = index(xx)), ylab='', xlab='', main=main)
    mtext(mtext)
  }
  if(ncol(x)!=1) {
    xx<-na.omit(x)
    xxx=do.call(cbind, lapply(c(1:ncol(xx)), 
                              function(x) zoo(smooth.spline(as.timeSeries(xx[,x]))$y, order.by = index(xx))))
    if(colnames=='FALSE') colnames(xxx)=seq(1:ncol(xx))
    if(colnames!='FALSE') colnames(xxx)=colnames
    plot(xxx, plot.type='single', ylab='', xlab='', lty=lty, lwd=lwd, main=main, col=colors)
    mtext(mtext)
    if(legend!='FALSE') legend(legend, colnames(xxx), col=colors, lty=1, cex=cex.legend) 
  }
}

multiplot <- function(x,smooth=NULL,main='',col=c(1:10),cex.main=NULL,legend='false') { 
  ### ACHTUNG MACHT AUTOMATISCH X=AXEN FALSCH DURCH PAR NEW
  if(!is.null(smooth)) { 
    require(timeSeries)
    xx<-na.omit(x)
    x=do.call(cbind, lapply(c(1:ncol(xx)), function(x) zoo(smooth.spline(as.timeSeries(xx[,x]))$y, order.by = index(xx))))
  }
  for (i in c(1:ncol(x))) {
    if(i==1) plot.zoo(x[,i], xlab='', ylab='', col=col[i], main=main)
    if(i>1) plot.zoo(x[,i], xlab='', ylab='', col=col[i], xaxt='n', yaxt='n', main=NULL)
    par(new = T) } 
}

add_legend <- function(...,bty='n',marleft=0,marbottom=0) { # http://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
  # opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(marbottom, marleft, 0, 0), new=TRUE) # The default fig setting is (0, 1, 0, 1) and uses the entire device space.
  on.exit(par(opar)) # bottom, left, top, and right
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...,bty=bty) } # horiz=TRUE

fplot <- function(x,col='rainbow',legend='totalbottom',main=NULL,mtext=NULL,save=NULL,lty=NULL,lwd=NULL) { # oder multiplot?
  color <- col
  if(col=='rainbow') color <- palette(rainbow(6)) # palette1 <- distinctColorPalette(ncol(x)) 
  cei1=ceiling(ncol(x)/6); cei2=ceiling(ncol(x)/cei1); 
  cei3=c(rep(1, 6),rep(2, 6),rep(3, 6),rep(4, 6))[1:ncol(x)] # das hat irgendwie mit bar plots zu tun die ich mal gemacht habe
  if(is.null(lty)) lty=cei3
  if(!is.null(save)) pdf(file = paste(save,'.pdf',sep=''), bg='white') # width = 9, height = 7, 
  plot(as.zoo(x), plot.type='single', xlab='', ylab='', main=main, 
       lty = lty, lwd = lwd, col = color)
  mtext(mtext)
  # legend('bottom',colnames(x),lty=1,col=c(1:ncol(x)),cex=0.7,horiz=TRUE)
  if(legend=='totalbottom') add_legend("bottom", colnames(x), col=color, lty = cei3, cex=0.8, lwd=2, ncol=cei2, bty='n') # horiz=TRUE, 
  if(legend!='totalbottom') legend(legend, colnames(x), col=color, lty = cei3, cex=0.8, lwd=1)
  if(!is.null(save)) dev.off() 
}

plot_wo_weekend <- function(x,xblocks=TRUE,main=NULL,mtext=NULL) { 
  library(stringr) # also check formatC sprintf
  z0 <- zoo(coredata(x)) 
  plot(z0, type='n', xaxt = "n", xlab='', ylab='', main=main) 
  mtext(mtext)
  # testx <- x['T9:30/T16:00']
  UShours <- c(paste0(str_pad(9, 2, pad = "0"),c(30:59)),
               paste0(rep(10:15, each=60),str_pad(seq(0,59,by=1), 2, pad = "0")),
               '1600')
  if(xblocks==TRUE) xblocks(zoo(format(index(test), '%H%M') %in% UShours, index(z0)), col='lightgray')
  lines(z0)
  axis(1, time(z0), lab = format(time(x), "%d\n%Hh"), cex.axis = .7) 
}

plot_triple <- function(p1,p2,do_legend,...) { # tripleplot
  # doubleplot(cumsumna(ii[[1]]$Ret), cumsumna(ii[[2]]$Reth), ...)
  doubleplot(x=p1[,1], y=p1[,2],...)
  par(new=T)
  plot.zoo(p2, xlab='', ylab='', yaxt='n', col='blue', lty=2)
  axis(side = 4)
  if(!is.null(do_legend)) legend('topleft',do_legend,col=c(1,2,'blue'),lty=c(1,1,2))
}

### needs result of f_vol_summary as input // close-to-close and garman klass
cc_vs_gk <- function(asd) {
  barplot(asd[,4]-asd[,3], yaxt='n', axisnames = FALSE, col=rgb(0,0,1,0.25), border=NA)
  axis(side = 4)
  par(new = T)
  doubleplot(asd[,c(3:4)], legend='topleft')
}

dc_vs_rv <- function(asd) {
  barplot(asd$DC, yaxt='n', axisnames = FALSE, col=adjustcolor('red', 0.25), border=NA)
  axis(side = 4)
  par(new = T)
  # plotzoo(sqrt(asd$RV*252), legend='topleft') # asd$Close, 
  plotzoo(sqrt(asd$RV*252), legend=FALSE)
  legend('topleft',c('RV','DC'),lty=1,col=c('black','red'), cex=0.8)
}

project_iv <- function(x) {
  if(ncol(x)!=2) { print('WRONG DATA INPUT'); return(NULL) }
  barplot(asd$DC, yaxt='n', axisnames = FALSE, col=adjustcolor('red', 0.25), border=NA)
  axis(side = 4)
  par(new = T)
  # plotzoo(sqrt(asd$RV*252), legend='topleft') # asd$Close, 
  plotzoo(sqrt(asd$RV*252), legend=FALSE)
  legend('topleft',c('RV','DC'),lty=1,col=c('black','red'), cex=0.8)
}

plot_with_regression <- function(x,y=NULL,main=NULL,xlab='',ylab='') {
  if(is.null(y)) { y <- x[,2]; x <- x[,1] }
  x <- as.numeric(x)
  y <- as.numeric(y)
  plot(x,y, col=rgb(0,0,255,50,maxColorValue=255), pch=16, xlab=xlab, ylab=ylab, main=main)
  abline(lm(y~x), col="red")
  abline(ltsReg(y~x), col="green") # Least Trimmed Squares Robust (High Breakdown) Regression  })
  lines(smooth.spline(x,y), col='red')
  
  # legend('topleft',c('Linear','Least Trimmed Squares Robust'),lty=c(1,1),col=c('red','green'),cex=0.8)
}

### GGPLOT ----
# also check: http://timelyportfolio.github.io/rCharts_dimple/dimple_timeseries.html
ggmultiple <- function(data) {
  df <- data.frame(time = index(data), as.data.frame(data))
  d <- reshape2::melt(df, id='time')
  ggplot(d, aes(time,value, col=variable)) + geom_line() + xlab("") + ylab("") + theme(legend.title=element_blank()) # + theme_minimal()
}

# https://stackoverflow.com/questions/47321272/linear-regression-lines-cannot-show-on-time-series-plot-in-r
# # convert stk to data frame & specify your x-axis variable explicitly
# stk.df <- as.data.frame(stk)
# stk.df$Date <- as.Date(rownames(stk.df))
# 
# # plot
# ggplot(stk.df,
#        aes(x = Date, y = log(AAPL.Close))) +
#   geom_line() +
#   geom_smooth(method = "lm", se = FALSE) +             
#   labs(x = "Year", y = "log(AAPL's closing value)") +
#   theme_bw()

ThemeFelix <- function (base_size = 11, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) + 
    theme(legend.position = 'top', legend.title = element_blank(), 
          axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 1)) # axis.text.y = element_blank(),  for ticks
    # + labs(y = '') # LABS DOES NOT WORK INSIDE FUNCTION
    # scale_x_discrete(name = "", labels = paste('<',c(0.25,0.5,0.75,1,1.5,2,5,10),'Mio.')) # DOES NOT WORK INSIDE FUNCTION
}

# https://rpubs.com/Koundy/71792
# par() # $family [1] ""
# require(ggthemes)::theme_base

theme_base <- function (base_size = 16, base_family = "") {
  # theme_foundation() + 
  # theme(panel.border = element_rect(fill = NA), legend.background = element_rect(colour = NA), 
  #       line = element_line(colour = "black"), rect = element_rect(fill = "white", colour = "black"), 
  #       text = element_text(colour = "black")) +
  theme_bw() +
  theme(line = element_line(colour = "black", lineend = "round", linetype = "solid"),
                             rect = element_rect(fill = "white", colour = "black", linetype = "solid"),
                             text = element_text(colour = "black", face = "plain", family = base_family, size = base_size, vjust = 0.5, hjust = 0.5, lineheight = 1),
                             panel.grid = element_blank(), strip.background = element_rect(colour = NA),
                             legend.key = element_rect(colour = NA),
                             title = element_text(size = rel(1)), plot.title = element_text(size = rel(1.2), face = "bold"),
                             strip.text = element_text(), axis.ticks.length = unit(0.5, "lines"))
}

# http://r.789695.n4.nabble.com/is-there-any-option-like-cex-axis-in-ggplot2-td885426.html
# BaseThemeX90 <- function(base_size = 10) { 
# structure(list( 
#   axis.line =         theme_blank(), 
#   axis.text.x =       theme_text(size = base_size * 0.8 , lineheight = 0.9, colour = "grey50", hjust = 1, angle = 90), 
#   axis.text.y =       theme_text(size = base_size * 0.8, lineheight = 0.9, colour = "grey50", hjust = 1), 
#   axis.ticks =        theme_segment(colour = "grey50"), 
#   axis.title.x =      theme_text(size = base_size), 
#   axis.title.y =      theme_text(size = base_size, angle = 90), 
#   axis.ticks.length = unit(0.15, "cm"), 
#   axis.ticks.margin = unit(0.1, "cm"), 
#   
#   legend.background = theme_rect(colour=NA), 
#   legend.key =        theme_rect(fill = "grey95", colour = "white"), 
#   legend.key.size =   unit(1.2, "lines"), 
#   legend.text =       theme_text(size = base_size * 0.7), 
#   legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0), 
#   legend.position =   "right", 
#   
#   panel.background =  theme_rect(fill = "grey90", colour = NA), 
#   panel.border =      theme_blank(), 
#   panel.grid.major =  theme_line(colour = "white"), 
#   panel.grid.minor =  theme_line(colour = "grey95", size = 0.25), 
#   panel.margin =      unit(0.25, "lines"), 
#   
#   strip.background =  theme_rect(fill = "grey80", colour = NA), 
#   strip.label =       function(variable, value) value, 
#   strip.text.x =      theme_text(size = base_size * 0.8), 
#   strip.text.y =      theme_text(size = base_size * 0.8, angle = -90), 
#   
#   plot.background =   theme_rect(colour = NA), 
#   plot.title =        theme_text(size = base_size * 1.2), 
#   plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines") 
# ), class = "options") 
# } 
