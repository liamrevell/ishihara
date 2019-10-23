## print & plot methods for ishihara object

plot.ishihara<-function(x,...){
	if(hasArg(mar)) mar<-list(...)$mar
	else mar<-rep(2.1,4)
	if(hasArg(numbers)) numbers<-list(...)$numbers
	else numbers<-FALSE
	if(hasArg(alpha)) alpha<-list(...)$alpha
	else alpha<-1.0
	xx<-sapply(x,function(x) x$x)
	yy<-sapply(x,function(x) x$y)
	rr<-sapply(x,function(x) x$radius)
	color<-sapply(x,function(x) x$color)
	ii<-setdiff(1:attr(x,"nshapes"),attr(x,"pattern"))
	color[ii]<-sapply(color[ii],function(x,alpha)
		make.transparent(x,alpha),alpha=alpha)
	plot.new()
	par(mar=mar)
	plot.window(xlim=range(xx),ylim=range(yy),asp=1)
	symbols(x=xx,y=yy,circles=rr,bg=color,fg=color,
		inches=FALSE,add=TRUE)
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