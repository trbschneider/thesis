library(irr)
library(ggplot2)
library(tikzDevice)
library(RColorBrewer)



#tikz(file = "/Volumes/KINGSTON/thesis/chapter8/figs/BMplot.tikz", width = 300, height = 300, onefile = TRUE, bg = "transparent", fg = "black",  standAlone = FALSE )


preamble <- function(classes,filename){

	preamble<-"%created by ICC.r"
	
	region_levels<-levels(classes)
	num_levels<-length(region_levels);
	colors<-brewer.pal(n=num_levels, 'Spectral');
	colors<-substring(colors,2);
	
	xcolornames<-sprintf("brewer%i_%i",num_levels,1:num_levels)
	
	xcolordefs<-sprintf("\\definecolor{%s}{HTML}{%s}",xcolornames,colors);
	
	preamble<-c(preamble,xcolordefs)
	    
	markers<-c("*","+","o","asterisk","oplus*","triangle*","square*","diamond*","diamond*","star","otimes*");
	
	
	preamble<-c(preamble,"\\pgfplotsset{blandaltman/.style={scatter/classes=");
		preamble<-c(preamble,"{");
	 		marks<-markers[1:num_levels]
	    	
			scatter_classes<-sprintf("%s={color=%s, mark=%s, mark options={scale=1.5}}",region_levels,xcolornames,marks)
			
			preamble<-c(preamble,paste(scatter_classes,collapse=", "));	
		
		preamble<-c(preamble,"}");	
	preamble<-c(preamble,"}}");
	
    fileConn<-file(filename)
    writeLines(preamble, fileConn)
    close(fileConn)
	
	
}

bland_altman_plot <- function(dat,filename){

  	
  res<-"\\begin{tikzpicture}"
 
  dat$diff <- dat$pre - dat$post
  dat$mean <- rowMeans(cbind(dat$pre, dat$post))
  
  xmin=0.9*min(dat$mean)
  xmax=1.1*max(dat$mean)
  
  ymin=-3*sd(dat$diff)
  ymax=3*sd(dat$diff)
  
  
  ci=c(-2, 0, 2) * sd(dat$diff)
  
  res<-c(res,sprintf("\\begin{axis}[blandaltman,width=0.7*\\textwidth, height=0.7*\\textwidth, xmin=%1.6f,xmax=%1.6f,ymin=%1.6f,ymax=%1.6f]",xmin,xmax,ymin,ymax));

  res<-c(res,"\\addplot+[only marks, scatter]  [scatter src=explicit symbolic] coordinates {");
  
  coords<-sprintf("(%1.5f, %1.5f)[%s]",dat$mean, dat$diff,dat$regions)
  res<-c(res,coords)
  res<-c(res,"};");

  res<-c(res,sprintf("\\draw[--,dashed] (axis cs:%1.6f,%1.6f) -- (axis cs:%1.6f,%1.6f);",xmin,ci,xmax,ci));
  res<-c(res,sprintf("\\legend{%s};",paste(levels(dat$regions),collapse=", ")));
  
  
  res<-c(res,"\\end{axis}");
  
  res<-c(res,"\\end{tikzpicture}");
 
  dat$diff <- dat$pre - dat$post
  
  print(res)
  
 
 
  fileConn<-file(filename,open="a")
  writeLines(res, fileConn)
  close(fileConn)
  
  #meandiff=mean(dat$diff) 
  #+ c(-2, 0, 2) * sd(dat$diff), linetype=2, color='brown') +
  #geom_point() +  theme_bw() +
  #xlab('mean of scan and rescan') + ylab('scan - rescan') + scale_colour_brewer(name='', palette='Paired') + scale_shape_manual(values=1:10)
}


rescanR <-read.table("/Volumes/KINGSTON/thesis/chapter8/figs/gen/scanrescanvalsR.txt", header=TRUE, sep="\t")
rescanDENS <-read.table("/Volumes/KINGSTON/thesis/chapter8/figs/gen/scanrescanvals.txt", header=TRUE, sep="\t")

dat <- data.frame(pre=rescanR$R1, post=rescanR$R2, regions=rescanR$REG)
datNOAVG <- dat[rescanR$REG!='AVG',]
datNOAVG$regions <- factor(datNOAVG$regions)

preamble(datNOAVG$regions,"/Volumes/KINGSTON/thesis/chapter8/figs/BMplot.tikz")                      
bland_altman_plot(datNOAVG,"/Volumes/KINGSTON/thesis/chapter8/figs/BMplot.tikz")

dat <- data.frame(pre=rescanDENS$R1, post=rescanDENS$R2, regions=rescanDENS$REG)
datNOAVG <- dat[rescanDENS$REG!='AVG',]
datNOAVG$regions <- factor(datNOAVG$regions)

bland_altman_plot(datNOAVG,"/Volumes/KINGSTON/thesis/chapter8/figs/BMplot.tikz")

