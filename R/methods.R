## print & plot methods for ishihara object

plot.ishihara<-function(x,...){
	if(hasArg(pause)) pause<-list(...)$pause
	else pause<-0
	xx<-sapply(x,function(x) x$x)
	yy<-sapply(x,function(x) x$y)
	plot.new()
	plot.window(xlim=range(xx),ylim=range(yy),asp=1)
	for(i in 1:attr(x,"nshapes")){
		draw.circle(x[[i]]$x,x[[i]]$y,radius=x[[i]]$radius,
			col=x[[i]]$color,border=x[[i]]$color)
		if(pause>0) Sys.sleep(pause)
	}
}

print.ishihara<-function(x,...){
	if(hasArg(show.hidden)) show.hidden<-list(...)$show.hidden
	else show.hidden<-FALSE
	cat("\n   Object of class \"ishihara\" containing",
		attr(x,"nshapes"),"shapes.\n\n")
	if(show.hidden)
		cat(paste("   Hidden number, character, or figure is: ",
			attr(x,"hidden"),".\n",sep=""))
	else cat(paste("   To reveal hidden number, character, or figure",
			"\n   print with show.hidden=TRUE.\n"))
	cat("\n")
}