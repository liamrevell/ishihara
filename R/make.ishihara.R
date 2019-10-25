## function to make an Ishihara object from a PNG file
make.ishihara<-function(png,hidden=NULL){
	## PNG dimensions
	dy<-nrow(png[,,1])
	dx<-ncol(png[,,1])
	## plot PNG
	plot.new()
	par(mar=rep(1.1,4))
	plot.window(xlim=c(0,dx),ylim=c(0,1.05*dy),asp=1)
	rasterImage(png,0,0,dx,dy)
	rect(par()$usr[1], par()$usr[4] - 3 * strheight("W"), 
		par()$usr[2], par()$usr[4], border = 0, 
		col = make.transparent("blue", 0.2))
	textbox(x = par()$usr[1:2], y = par()$usr[4], 
		c("Click anywhere within circles to create object\nRIGHT CLICK to stop"), 
		justify = "c", border = 0,
		col=if(par()$bg=="black") "white" else par()$fg)
	## turn off locator bell
	options(locatorBell = FALSE)
	## start creating object
	plate<-list()
	i<-1
	pt<-locator(1)
	while(!is.null(pt)){
		col<-rgb(
			png[dy-pt$y,pt$x,1],
			png[dy-pt$y,pt$x,2],
			png[dy-pt$y,pt$x,3],
			png[dy-pt$y,pt$x,4])
		h1<-0
		while((pt$x-h1)>0&&png[dy-pt$y,pt$x-h1,4]!=0) 
			h1<-h1+1
		h1<-h1-1
		h2<-0
		while((pt$x+h2)<dx&&png[dy-pt$y,pt$x+h2,4]!=0) 
			h2<-h2+1
		h2<-h2-1
		v1<-0
		while((dy-pt$y-v1)>0&&png[dy-pt$y-v1,pt$x,4]!=0) 
			v1<-v1+1
		v1<-v1-1
		v2<-0
		while((dy-pt$y+v2)<dy&&png[dy-pt$y+v2,pt$x,4]!=0) 
			v2<-v2+1
		v2<-v2-1
		lines(rep(pt$x,2),pt$y+c(v1,-v2),
			col=if(par()$bg=="black") "white" else par()$fg)
		lines(pt$x+c(-h1,h2),rep(pt$y,2),
			col=if(par()$bg=="black") "white" else par()$fg)
		points(pt$x,pt$y,cex=1.5,bg=col,pch=21)
		c1<-findCircle(pt$x-h1,pt$y,pt$x+h2,pt$y,pt$x,pt$y-v2)
		c2<-findCircle(pt$x-h1,pt$y,pt$x,pt$y-v2,pt$x,pt$y+v2)
		c3<-findCircle(pt$x+h2,pt$y,pt$x,pt$y-v2,pt$x,pt$y+v2)
		circle<-list(x=mean(c1$x,c2$x,c3$x),
			y=mean(c1$y,c2$y,c3$y),
			r=mean(c1$r,c2$r,c3$r))
		symbols(circle$x,circle$y,circles=circle$r,fg="grey",lwd=2,
			bg="transparent",add=TRUE,inches=FALSE)
		plate[[i]]<-list(x=circle$x/max(dx,dy)*300,
			y=circle$y/max(dx,dy)*300,
			radius=circle$r/max(dx,dy)*300,
			color=col)
		i<-i+1
		pt<-locator(1)
		if(is.null(pt)){
			cat("STOP or GOBACK? > ")
			flush.console()
			response<-readLines(n=1)
			if(response=="GOBACK"){
				cat("Select next point....\n")
				flush.console()
				dev.flush()
				pt<-locator(1)
				i<-i-1
			}
		}
	}
	## turn on locator bell
	options(locatorBell = TRUE)
	## add object attributes
	attr(plate,"class")<-"ishihara"
	attr(plate,"nshapes")<-length(plate)
	attr(plate,"hidden")<-hidden
	attr(plate,"pattern")<-NULL
	plate
}

## adapted from https://www.xarg.org/2018/02/create-a-circle-out-of-three-points/

findCircle<-function(x1,y1,x2,y2,x3,y3) {
	a <- x1 * (y2 - y3) - y1 * (x2 - x3) + 
		x2 * y3 - x3 * y2
	b <- (x1 * x1 + y1 * y1) * (y3 - y2) + 
		(x2 * x2 + y2 * y2) * (y1 - y3) + 
		(x3 * x3 + y3 * y3) * (y2 - y1)
	c <- (x1 * x1 + y1 * y1) * (x2 - x3) + 
		(x2 * x2 + y2 * y2) * (x3 - x1) + 
		(x3 * x3 + y3 * y3) * (x1 - x2)
	x <- -b / (2 * a)
	y <- -c / (2 * a)
	r <- sqrt((x1-x)^2+(y2-y)^2)
	list(x=x,y=y,r=r)
}