#START OF AUTO-GENERATED PREAMBLE, WILL BE OVERWRITTEN WHEN THIS FILE IS USED AS A TEMPLATE
#Created 2016-11-17 at 08:30

rplots.level <- 1
xpose.runno <- ''
toolname <- 'bootstrap'
pdf.filename <- paste0('PsN_',toolname,'_plots.pdf')
pdf.title <- 'bootstrap diagnostic plots 1360.ctl'
working.directory<-'/home/tbergsma/project/puma/model_ex/1360/bootstrap_dir1/'
raw.results.file <- 'raw_results_1360.csv'
model.directory<-'/home/tbergsma/project/puma/model_ex/1360/'
model.filename<-'1360.ctl'
subset.variable<-NULL
mod.suffix <- '.ctl'
mod.prefix <- 'run'
tab.suffix <- ''
tool.results.file <- 'bootstrap_results.csv'
theta.labels <- c('CL(L/h)','V2 (L)','KA','WT on CL','WT on V','Q (L/h)','V3 (L)','alag1','age on CL','bili','alt','tras','cap','age on V','keto','age on Ka')
theta.fixed <- c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
omega.labels <- c('OMEGA(1,1)','OMEGA(2,2)','OMEGA(3,3)')
omega.fixed <- c(FALSE,FALSE,FALSE)
sigma.labels <- c('SIGMA(1,1)','SIGMA(2,2)')
sigma.fixed <- c(FALSE,FALSE)
n.eta <- 3
n.eps <- 2

#bootstrap-specific preamble
N.ESTIMATED.PARAMS <- 21
dofv.is.run <- 0

included.ids.file <- 'included_individuals1.csv'
skip.minimization.terminated=TRUE
skip.covariance.step.terminated=FALSE
skip.with.covstep.warnings=FALSE
skip.estimate.near.boundary=TRUE

setwd(working.directory)

############################################################################
#END OF AUTO-GENERATED PREAMBLE
#WHEN THIS FILE IS USED AS A TEMPLATE THIS LINE MUST LOOK EXACTLY LIKE THIS


library(xpose4)

if(packageVersion("xpose4")<"4.5.0"){
		warning("xpose4 version must be 4.5.0 or later for bootstrap plot")	
}							 

pdf(file=pdf.filename,width=10,height=7,title=pdf.title)

bootplots<-boot.hist(results.file=raw.results.file,incl.ids.file=included.ids.file,
            min.failed=skip.minimization.terminated,
            cov.failed=skip.covariance.step.terminated,
            cov.warnings=skip.with.covstep.warnings,
            boundary=skip.estimate.near.boundary)
print(bootplots[1]) #parameters



#############################################################################################################
### Plot dOFV distribution of bootstrap -dofv option 
### Author: AG Dosne
### Date: September 2015
#############################################################################################################


if (dofv.is.run){

require(ggplot2)
require(plyr)
require(dplyr)
theme_set(theme_bw(base_size=20))


#############################################################################################################
### Read in and format data
#############################################################################################################

### Read in BOOT results (dOFV)

boot1         <- read.csv(dofv.raw.results.file,stringsAsFactors=F)
boot1         <- boot1[-1,] # remove first row=original data
names(boot1)[names(boot1) %in% c("model","bs_data_id")] <- c("modelB","model") # to match rawres file
boot1$model   <- as.numeric(boot1$model)
boot2         <- read.csv(raw.results.file)
boot          <- left_join(boot2,boot1[,c("deltaofv","model")])  # keep stats from bootstrap (terminated runs etc) to investigate dOFV distribution
boot          <- boot[order(boot$deltaofv),]  
boot$rownames <- seq(nrow(boot))/nrow(boot)
boot$METHOD   <- "BOOT"

boot_term           <- filter(boot, METHOD=="BOOT" & minimization_successful==1) # only runs with min successful
boot_term$rownames  <- seq(nrow(boot_term))/nrow(boot_term)
boot_term$minimization_successful <- "yes"
boot$minimization_successful <- "yes+no"

# boot_bound          <- filter(boot, METHOD=="BOOT" & estimate_near_boundary==0) # only runs with NO estimate near boundary
# boot_bound$rownames <- seq(nrow(boot_bound))/nrow(boot_bound)

### Create reference chisquare (for dOFV)

ref           <- data.frame("deltaofv"=qchisq(seq(0,0.99,0.01),df=N.ESTIMATED.PARAMS),"rownames"=seq(0,0.99,0.01),"METHOD"="REF")

### Merge and format datasets

all                         <- rbind.fill(boot,boot_term,ref)
all$minimization_successful[is.na(all$minimization_successful)] <- "yes"
df_est   <- ddply(all,.(METHOD,minimization_successful), summarise, "df"=round(mean(deltaofv,na.rm=T),1))  # get df for each distribution
df_est[df_est$METHOD=="REF",]$df <- N.ESTIMATED.PARAMS # replace with true df to avoid random noise

#############################################################################################################
### Do plots
#############################################################################################################

### Plot dOFV distributions

qdOFV_all <- ggplot(all,aes(x=rownames,y=deltaofv,color=METHOD,linetype=minimization_successful)) + 
  geom_line() +
  geom_text(data=df_est, aes(x = 0.7,y=c(3,2,1)*qchisq(0.7,df=N.ESTIMATED.PARAMS)/(2*nrow(df_est)),label=paste(df," (",METHOD,"-",minimization_successful,")",sep="")),show_guide=FALSE,hjust=0,size=5) +
  annotate("text",x = 0.7,y=(nrow(df_est)+1)*qchisq(0.7,df=N.ESTIMATED.PARAMS)/(2*nrow(df_est)),label="Estimated df",hjust=0,fontface="italic",size=5) +
  labs(x="Distribution quantiles",y="dOFV",title="dOFV distribution") +
  theme(legend.position="bottom",legend.box="horizontal") +
  guides(colour = guide_legend(title.position="top"),linetype = guide_legend(title.position="top")) +
  coord_cartesian(ylim=c(0,2*qchisq(0.95,df=N.ESTIMATED.PARAMS))) 


print(qdOFV_all)

}

### END
#############################################################################################################


if (rplots.level > 1){
    print(bootplots[2:4]) #SEs ofv eigenvalues
}

dev.off()



