#Create Taubes Figure 1
#Last updated: 11-8-2021

library(tidyverse)
library(openxlsx)
library(reshape2)
library(gtable)

rm(list=ls())

#Bring in Taubes data for Figure 1
taubes_data <- readRDS("Taubesfig_data.Rda")
str(taubes_data)




################
#   Data prep  #
################

#Set max UCL to maximum UCL of all estimates
taubes_data[, "Max_UCL"] <- do.call(pmax, c(taubes_data[, c("lnUCL_1","lnUCL_2","lnUCL_3")], list(na.rm=TRUE)))

#Set min LCL to minimum LCL of all estimates
#Note: values for Association B (Ionizing radiation) are excess RRs (studies did not provide RRs) and LCL_2=0 --> lnLCL_2=-Inf.  
taubes_data[, "Min_LCL"] <- do.call(pmin, c(taubes_data[, c("lnLCL_1","lnLCL_2","lnLCL_3")], list(na.rm=TRUE)))

#Change Min_LCL for Association B (because ln(0) = -Inf) to the log(0.001) so that close to 0 but will give us a real number. We will reset to -1 below because beyond y-axis range.
taubes_data$Min_LCL <- ifelse(taubes_data$Min_LCL==-Inf, log(0.001), taubes_data$Min_LCL)

#sort by Causal eval status and Max_UCL
taubes_data <- arrange(taubes_data, Causal, desc(Max_UCL))


#Create study letter based on this order
lets1 <- LETTERS[1:26] #A-Z
lets2 <- paste0(lets1,lets1) #AA-ZZ
lets_all <- c(lets1,lets2) #combination of A-ZZ (n=52)
lets_49 <- lets_all[1:49] #subset first 49 in list
taubes_data$Assoc_ABC <- lets_49 #add as variable to taubes_data
taubes_data$order <- 1:49 #add addtl order variable bc when factor study letter for plot, the order gets screwed up
taubes_data$Assoc_ABC <- as.factor(taubes_data$Assoc_ABC)

rm(lets1, lets2, lets_all, lets_49)

##########################
#        FIGURE          #
##########################

#change data to long form
dat <- reshape2::melt(taubes_data, id.vars="Assoc_ABC",
                      measure.vars=c("lnRR_1", "lnRR_2", "lnRR_3"))
dat <- left_join(dat, taubes_data, by="Assoc_ABC")

#reorder Association letter for figure
dat$Assoc_ABC = fct_reorder(dat$Assoc_ABC, dat$order)

#For 2 associations (A & B), estimates are beyond y-axis --> set these to y-axis limits (will add arrowhead to show this in plot)
dat$Max_UCL_u <- ifelse(dat$Max_UCL > 4, 4, NA)
dat$Min_LCL_u <- ifelse(dat$Min_LCL < -1, -1, NA) 

##combine individual plots rather than using facet_wrap
library(patchwork)
library(scales)

#NOTES ABOUT WARNINGS WHEN RUNNING BELOW CODE
# Long-form df includes 49 associations x 3 meta-analyses = 147 obs 
# There are associations without 3 meta-analyses (have 1 or 2 instead of 3). 
# These have NA values, which are removed when running the below code to plot estimates
# For plot_CC, we added arrows for estimates that were beyond y-axis using Min_LCL_u and Max_UCL_u (created above). Except for A and B, Min_LCL_u and Max_UCL_u = NA and are removed (n=27 rows removed for missing values) 

dat_CC <- filter(dat, Causal=="C/C")

plot_CC <- ggplot(dat_CC,aes(x = Assoc_ABC,y = value, ymin=Min_LCL, ymax=Max_UCL, color="#F8766D"))+
  geom_point(size=1.75)+
  geom_hline(yintercept =0, linetype=2)+
  xlab('')+ ylab("ln(RR)")+ scale_y_continuous(limits=c(-1,4))+
  geom_errorbar(aes(ymin=Min_LCL, ymax=Max_UCL),width=0.5,cex=0.8)+
  labs(title='Causal/Causal', tag='A') +
  theme(panel.border = element_rect(fill=NA),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        plot.title=element_text(size=8, hjust=0.5),
        axis.text.x=element_text(face="bold", angle=45),
        axis.title=element_text(size=10,face="bold"),
        legend.position = "none",
        plot.tag = element_text(face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_segment(aes(x = Assoc_ABC, xend = Assoc_ABC,
                   y = Min_LCL, yend = Max_UCL_u),
               position = position_dodge(.5),
               arrow = arrow(length = unit(0.3, "cm")),cex=0.8) +
  geom_segment(aes(x = Assoc_ABC, xend = Assoc_ABC,
                   y = Max_UCL, yend = Min_LCL_u),
               position = position_dodge(.5),
               arrow = arrow(length = unit(0.3, "cm")),cex=0.8)
plot_CC

dat_IC <- filter(dat, Causal=="I/C")

plot_IC <- ggplot(dat_IC,aes(x = Assoc_ABC,y = value, ymin=Min_LCL, ymax=Max_UCL))+
  geom_point(size=1.75,color="#A3A500")+
  geom_hline(yintercept =0, linetype=2)+
  xlab('')+ ylab("")+scale_y_continuous(limits=c(-1,4))+
  geom_errorbar(aes(ymin=Min_LCL, ymax=Max_UCL),width=0.5,cex=0.8,color="#A3A500")+
  labs(title='Indeterminate/Causal', tag='B') +
  theme(panel.border = element_rect(fill=NA),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        plot.title=element_text(size=8, hjust=0.5),
        axis.text.x=element_text(face="bold", angle=45),
        axis.title=element_text(size=10,face="bold"),
        legend.position = "none",
        plot.tag = element_text(face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot_IC

dat_II <- filter(dat, Causal=="I/I")

plot_II <- ggplot(dat_II,aes(x = Assoc_ABC,y = value, ymin=Min_LCL, ymax=Max_UCL))+
  geom_point(size=1.75,color="#00BF7D")+
  geom_hline(yintercept =0, linetype=2)+
  xlab('Exposure-Outcome Association')+ ylab("")+scale_y_continuous(limits=c(-1,4))+
  geom_errorbar(aes(ymin=Min_LCL, ymax=Max_UCL),width=0.5,cex=0.8,color="#00BF7D")+
  labs(title='Indeterminate/Indeterminate', tag='C') +
  theme(panel.border = element_rect(fill=NA),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        plot.title=element_text(size=8, hjust=0.5),
        axis.text.x=element_text(face="bold", angle=45),
        axis.title.x = element_text(size=9,face="bold",hjust=0.25),
        legend.position = "none",
        plot.tag = element_text(face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot_II

dat_INC <- filter(dat, Causal=="I/NC")

plot_INC <- ggplot(dat_INC,aes(x = Assoc_ABC,y = value, ymin=Min_LCL, ymax=Max_UCL))+
  geom_point(size=1.75,color="#00B0F6")+
  geom_hline(yintercept =0, linetype=2)+
  xlab('')+ ylab("")+scale_y_continuous(limits=c(-1,4))+
  geom_errorbar(aes(ymin=Min_LCL, ymax=Max_UCL),width=0.5,cex=0.8,color="#00B0F6")+
  labs(title='Indeterminate/\nNon-causal', tag='D') +
  theme(panel.border = element_rect(fill=NA),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        plot.title=element_text(size=8, hjust=0.5),
        axis.text.x=element_text(face="bold", angle=45),
        axis.title=element_text(size=10,face="bold"),
        legend.position = "none",
        plot.tag = element_text(face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot_INC

dat_NCNC <- filter(dat, Causal=="NC/NC")

plot_NCNC <- ggplot(dat_NCNC,aes(x = Assoc_ABC,y = value, ymin=Min_LCL, ymax=Max_UCL))+
  geom_point(size=1.75,color="#E76BF3")+
  geom_hline(yintercept =0, linetype=2)+
  xlab('')+ ylab("")+scale_y_continuous(limits=c(-1,4))+
  geom_errorbar(aes(ymin=Min_LCL, ymax=Max_UCL),width=0.5,cex=0.8, color="#E76BF3")+
  labs(title='Non-causal/\nNon-causal', tag='E') +
  theme(panel.border = element_rect(fill=NA),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        plot.title=element_text(size=8, hjust=0.5),
        axis.text.x=element_text(face="bold", angle=45),
        axis.title=element_text(size=10,face="bold"),
        legend.position = "none",
        plot.tag = element_text(face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot_NCNC

taubes_plot <- plot_CC + plot_IC + plot_II + plot_INC + plot_NCNC +
  plot_layout(nrow=1, widths = c(0.85,0.85,2.7,0.6, 0.23)) +
  plot_annotation(
    theme = theme(
      plot.title = element_text(face="bold",size = 9, hjust=0.05),
      plot.caption = element_text(size = 9, hjust=0.04)

    )
  )

# Remove title from second subplot
taubes_plot[[2]] = taubes_plot[[2]] + theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.y = element_blank() )

# Remove title from third subplot
taubes_plot[[3]] = taubes_plot[[3]] + theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.y = element_blank() )

# Remove title from forth subplot
taubes_plot[[4]] = taubes_plot[[4]] + theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.y = element_blank() )

# Remove title from fifth subplot
taubes_plot[[5]] = taubes_plot[[5]] + theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.y = element_blank() )

taubes_plot
