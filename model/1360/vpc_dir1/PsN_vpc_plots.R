#START OF AUTO-GENERATED PREAMBLE, WILL BE OVERWRITTEN WHEN THIS FILE IS USED AS A TEMPLATE
#Created 2016-11-17 at 05:00

rplots.level <- 1
xpose.runno <- ''
toolname <- 'vpc'
pdf.filename <- paste0('PsN_',toolname,'_plots.pdf')
pdf.title <- 'vpc diagnostic plots 1360.ctl'
working.directory<-'../model_ex/1360/vpc_dir1/'
raw.results.file <- 'raw_results_1360.csv'
model.directory<-'../model_ex/1360/'
model.filename<-'1360.ctl'
subset.variable<-NULL
mod.suffix <- '.ctl'
mod.prefix <- 'run'
tab.suffix <- ''
tool.results.file <- 'vpc_results.csv'
theta.labels <- c('CL(L/h)','V2 (L)','KA','WT on CL','WT on V','Q (L/h)','V3 (L)','alag1','age on CL','bili','alt','tras','cap','age on V','keto','age on Ka')
theta.fixed <- c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
omega.labels <- c('OMEGA(1,1)','OMEGA(2,2)','OMEGA(3,3)')
omega.fixed <- c(FALSE,FALSE,FALSE)
sigma.labels <- c('SIGMA(1,1)','SIGMA(2,2)')
sigma.fixed <- c(FALSE,FALSE)
n.eta <- 3
n.eps <- 2

vpctab <- 'vpctab'
have.loq.data <- FALSE
have.censored <- FALSE
is.categorical <- FALSE
is.tte <- FALSE
dv <- 'DV'
idv <- 'TIME'
home <- getwd()
setwd(working.directory)

############################################################################
#END OF AUTO-GENERATED PREAMBLE
#WHEN THIS FILE IS USED AS A TEMPLATE THIS LINE MUST LOOK EXACTLY LIKE THIS


library(xpose4)

pdf(file=pdf.filename,width=10,height=7,title=pdf.title)

done <- FALSE
if (is.tte){
   	#data is in the model directory, go there to read input
	setwd(model.directory)
	xpdb <- xpose.data(xpose.runno)
	plots <- kaplan.plot(object=xpdb,VPC=T)
	#go back to vpc directory 
	setwd(working.directory)
	done <- TRUE
}  

if (is.categorical & (!done)){
    plots<-xpose.VPC.categorical(vpc.info=tool.results.file,vpctab=vpctab)
	done <- TRUE
}

if ((have.loq.data | have.censored) & (!done) ){
    plots<-xpose.VPC.both(vpc.info=tool.results.file,vpctab=vpctab)
	done <- TRUE
}

if (!done){
	plots<-xpose.VPC(vpc.info=tool.results.file,vpctab=vpctab)
	done <- TRUE
}

print(plots) 

dev.off()

setwd(home)

