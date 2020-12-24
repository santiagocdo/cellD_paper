# IMPORTANT: the submitted manuscript includes the statistical analysis using the 
# package psycho(), however this was discontinued and replaced with report().
# This script uses the functions long_table(report(mod)). The results presented 
# here and the ones published point the same direction.

rm(list = ls()) # errase previous Global Environment, import manually the file
setwd("C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/raw_data") # set workspace

# packages needed
if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest) # lmer()
if (!require(report)) {install_github("easystats/report")}; library(report) # report()
if (!require(reshape2)) {install.packages("reshape2")}; library(reshape2) # melt()
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr) # %>%
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2) # ggplot()
if (!require(plyr)) {install.packages("plyr")}; library(plyr) # revalue()
if (!require(viridis)) {install.packages("viridis")}; library(viridis) # viriids()
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr) # ggarrange()

# print csv files
write_csv <- 0

# save plots
save_png <- 0

# linear contrast
linear_contrast <- 0



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # Experiment 1 # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

exp01_lf <- read.csv("exp12_lf.csv")
exp01_wf <- read.csv("exp12_wf.csv")

# general characteristics
nSubj <- nrow(exp01_wf); nSubj
sex <- table(exp01_wf$sex); sex
meanAge <- mean(exp01_wf$age); meanAge
sdAge <- sd(exp01_wf$age); sdAge
rangeAge <- range(exp01_wf$age); rangeAge

### adding the actual experience for each condition
# adding frequency and duration to conditions
exp01_lf$fA <- as.integer(36)
exp01_lf$fB <- as.integer(36)
exp01_lf$fC <- as.integer(36)
exp01_lf$fD <- as.integer(as.character(revalue(exp01_lf$condition, c("BaselineDelay"="36", "Baseline"="36", 
                                                                     "JumboLongDelay"="36", "JumboLonger"="36", "JumboMoreDelay"="324", "JumboMore"="324", 
                                                                     "LongerDelay"="36", "Longer"="36", "MoreDelay"="108", "More"="108", 
                                                                     "NoneDelay"="0", "None"="0"))))
exp01_lf$dA <- as.integer(450)
exp01_lf$dB <- as.integer(450)
exp01_lf$dC <- as.integer(450)
exp01_lf$dD <- as.integer(as.character(revalue(exp01_lf$condition, c("BaselineDelay"="450", "Baseline"="450", 
                                                                     "JumboLongDelay"="4050", "JumboLonger"="4050", "JumboMoreDelay"="450", "JumboMore"="450", 
                                                                     "LongerDelay"="1350", "Longer"="1350", "MoreDelay"="450", "More"="450", 
                                                                     "NoneDelay"="0", "None"="0"))))   




######## STATISTICAL ANALYSIS ######## 
# rescaling
exp01_lf$scFrequency <- scale(exp01_lf$frequency)
exp01_lf$scDuration <- scale(exp01_lf$frequency)
# contrasts: linear: -3,-1,1,3, proportional: c(0,3,9,27)-mean(c(0,3,9,27))=-13,-9,-1,23
if (linear_contrast == 1) {
  exp01_lf$contFreq <- as.integer(revalue(as.character(exp01_lf$fD),c("0"="-3","36"="-1","108"="1","324"="3")))
  exp01_lf$contDur <- as.integer(revalue(as.character(exp01_lf$dD),c("0"="-3","450"="-1","1350"="1","4050"="3")))
} else {
  exp01_lf$contFreq <- as.integer(revalue(as.character(exp01_lf$fD),c("0"="-13","36"="-9","108"="-1","324"="23")))
  exp01_lf$contDur <- as.integer(revalue(as.character(exp01_lf$dD),c("0"="-13","450"="-9","1350"="-1","4050"="23")))
}
exp01_lf$delay <- factor(exp01_lf$delay, levels = c("0", "3"))
levels(exp01_lf$delay) <-  c("immediate", "delay")
exp01_lf$subject <- as.factor(exp01_lf$subject)
exp01_lf$stage_order <- ifelse(exp01_lf$cond_order < 5, 1, ifelse(exp01_lf$cond_order > 8, 3, 2)) # does not make sense in the expeirment (given the design), 
                                                                                                  # however is good to have it as all the 
                                                                                                  # other experiment analysis.

# statistical analysis will be made with this, change this for constrast, scale or raw data (sensitivity analysis)
exp01_lf$freq <- exp01_lf$contFreq
exp01_lf$dur <- exp01_lf$contDur

plotANDstat_all <- 1
### ### ### exclude data for visualization (e.g. NA values or 0 frequency)
if (plotANDstat_all == 1) {
  # plot and analysis with 0.
  lf_no0 <- exp01_lf[!is.na(exp01_lf$rating),]
  
  # graphs data
  limits_vec_fre <- c(-4,330)
  break_vec_fre <- c(0,36,108,324)
  limits_vec_dur <- c(-50,4100)
  break_vec_dur <- c(0,450,1350,4050)
  
} else {
  # plot and analysis without 0.
  lf_no0 <- exp01_lf[exp01_lf$frequency != 0 & !is.na(exp01_lf$rating),]
  
  # graphs data
  limits_vec_fre <- c(0,330)
  break_vec_fre <- c(36,108,324)
  limits_vec_dur <- c(0,4100)
  break_vec_dur <- c(450,1350,4050)

}

### ### ###
# for non convergence models # convergence failure test: https://github.com/lme4/lme4/issues/120
#relgrad <- with(mod@optinfo$derivs,solve(Hessian,gradient))
#max(abs(relgrad))

#if(!require(optimx)) {install.packages("optimx")}; library(optimx) # optimx
mFull <- lmer(rating ~ 1 + delay + freq + dur + freq:delay + dur:delay + cond_order +
                (1 | subject) + (1 | condition),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              #control = lmerControl(optimizer = "optimx", optCtrl=list(method='nlminb')),
              REML = F, verbose = F, data = lf_no0); summary(mFull); anova(mFull) # Nelder_Mead, nloptwrap, bobyqa
stepFull <- step(mFull); stepFull



m_fre_del <- lmer(rating ~ 1 + freq * delay + (1 | subject) + cond_order,
                  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
                  REML = FALSE, data = lf_no0)

m_dur_del <- lmer(rating ~ 1 + dur * delay + (1 | subject) + cond_order,
                 control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
                 REML = FALSE, data = lf_no0)

m_fre_dur <- lmer(rating ~ 1 + freq + dur + (1 | subject) + cond_order,
                control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
                REML = FALSE, data = lf_no0)

m_fre <- lmer(rating ~ 1 +  freq + (1 | subject) + cond_order,
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              REML = FALSE, data = lf_no0)

m_dur <- lmer(rating ~ 1 + dur + (1 | subject) + cond_order,
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              REML = FALSE, data = lf_no0); anova(m_dur)

m_del <- lmer(rating ~ 1 + delay + (1 | subject) + cond_order,
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              REML = FALSE, data = lf_no0)

# the full model is not significanly better against frequency+delay (stats), 
# duration+delat (stats), nor frequency+duration (stats).

## AGAINST THE FULL MODEL
anova(mFull, m_fre_del, test="LRT"); exp((BIC(mFull) - BIC(m_fre_del))/2); AIC(mFull) - AIC(m_fre_del)
anova(mFull, m_dur_del, test="LRT"); exp((BIC(mFull) - BIC(m_dur_del))/2); AIC(mFull) - AIC(m_dur_del)
anova(mFull, m_fre_dur, test="LRT"); exp((BIC(mFull) - BIC(m_fre_dur))/2); AIC(mFull) - AIC(m_fre_dur)

anova(mFull, m_fre, test="LRT"); exp((BIC(mFull) - BIC(m_fre))/2); AIC(mFull) - AIC(m_fre)
anova(mFull, m_dur, test="LRT"); exp((BIC(mFull) - BIC(m_dur))/2); AIC(mFull) - AIC(m_dur)
anova(mFull, m_del, test="LRT"); exp((BIC(mFull) - BIC(m_del))/2); AIC(mFull) - AIC(m_del)

# # AGAINST THE FREQUENCY & DELAY MODEL
# anova(m_fre_del, m_fre, test="LRT"); exp((BIC(m_fre_del) - BIC(m_fre))/2); AIC(m_fre_del) - AIC(m_fre)
# anova(m_fre_del, m_dur, test="LRT"); exp((BIC(m_fre_del) - BIC(m_dur))/2); AIC(m_fre_del) - AIC(m_dur)
# anova(m_fre_del, m_del, test="LRT"); exp((BIC(m_fre_del) - BIC(m_del))/2); AIC(m_fre_del) - AIC(m_del)
# 
# # AGAINST THE DURATION & DELAY MODEL
# anova(m_dur_del, m_fre, test="LRT"); exp((BIC(m_dur_del) - BIC(m_fre))/2); AIC(m_dur_del) - AIC(m_fre)
# anova(m_dur_del, m_dur, test="LRT"); exp((BIC(m_dur_del) - BIC(m_dur))/2); AIC(m_dur_del) - AIC(m_dur)
# anova(m_dur_del, m_del, test="LRT"); exp((BIC(m_dur_del) - BIC(m_del))/2); AIC(m_dur_del) - AIC(m_del)
# 
# # AGAINST THE FREQUENCY & DURATION MODEL
# anova(m_fre_dur, m_fre, test="LRT"); exp((BIC(m_fre_dur) - BIC(m_fre))/2); AIC(m_fre_dur) - AIC(m_fre)
# anova(m_fre_dur, m_dur, test="LRT"); exp((BIC(m_fre_dur) - BIC(m_dur))/2); AIC(m_fre_dur) - AIC(m_dur)
# anova(m_fre_dur, m_del, test="LRT"); exp((BIC(m_fre_dur) - BIC(m_del))/2); AIC(m_fre_dur) - AIC(m_del)
anova(mFull, m_dur_del, m_fre_del, m_fre_dur, m_fre, m_dur, m_del, test = "LRT")
fits <- cbind(BIC(mFull, m_dur_del, m_fre_del, m_fre_dur, m_fre, m_dur, m_del),
              AIC(mFull, m_dur_del, m_fre_del, m_fre_dur, m_fre, m_dur, m_del)[,2])
colnames(fits) <- c("df","BIC","AIC")


 
#### #### #### #### #### ####
#to_print <- summary(analyze(mFull, CI = 95)) %>% mutate(p = psycho::format_p(p))
to_print <- table_long(report(mFull))
if (write_csv == 1) {
  write.csv(to_print,"C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/stats/e1_fullMod_propContrast.csv",
            row.names = F)
  print(fits)
  print(stepFull)
}



######## VISUALIZATION ########
# plot size in pixels 1500 x 1000. PDF 15 x 10
sf <- 1
plot_title_size <- 48#24
strip_text_size <- 44#22
axis_title_size <- 40#20
axis_text_size <- 36#18
legend_title_size <- 40#20
legend_text_size <- 36#18
pos <- position_dodge(10)

pF <- ggplot(lf_no0, aes(x = frequency, y = rating, col = delay, shape = delay)) +
  geom_hline(yintercept = 0, color = "gray50", size = 1.2) +
  #geom_jitter(width = 2, height = 0.2, alpha = 0.5 ,color = "gray70", size = 1) +
  geom_point(aes(y = jitter(rating)), alpha = 0.2, size = 1.2, position = pos) +
  geom_smooth(method = "lm", size = 2, se = F) +
  stat_summary(fun.data = "mean_cl_boot", size = sf*2, position = pos, stroke = sf*2, fill = "white") +
  labs(title = "Frequency effect", subtitle = paste("n =",nSubj), col = "Recall", shape = "Recall") +
  xlab("D frequency") + ylab("Rating") +
  scale_x_continuous(limits = limits_vec_fre, breaks = break_vec_fre, minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  scale_shape_manual(labels = c("Immediate", "Delay"), values=c(19,24)) + #19, 24# 16,17
  scale_colour_manual(labels = c("Immediate", "Delay"), values=viridis(5)[c(2,4)]) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = strip_text_size),
        plot.title = element_text(size = plot_title_size, hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = plot_title_size/2, hjust=0, face = "italic", color = "black"),
        plot.margin = unit(c(0.2,0.4,0.2,0.2), "cm"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = axis_text_size, colour="black"),
        axis.title.x = element_text(size = axis_title_size, face = "bold"),
        axis.text.x = element_text(size = axis_text_size*0.9, colour="black"),
        axis.line = element_line(colour = 'black', size = 1.6),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length = unit(10, "pt"),
        legend.title = element_text(size = legend_title_size), 
        legend.text = element_text(size = legend_text_size),
        legend.position = "none",
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.key = element_rect(colour = NA, fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(1, "lines")
  )
### add theoretical segment ###
# Af<-Bf<-Cf<-c(36,36,36,36)
# Df<-xf<-c(0,36,108,324)
# yf<-((Af/(Af+Bf))-(Cf/(Cf+Df)))*10
# for (i in 1:3) {
#   pF <- pF + geom_segment(x=xf[i],y=yf[i],xend=xf[i+1],yend=yf[i+1],col="red",size = 2,linetype="dotted")
# }
pF



# plot size in pixels 1500 x 1000. PDF 15 x 10
pos <- position_dodge(120)

pD <- ggplot(lf_no0, aes(x = duration, y = rating, col = delay, shape = delay)) +
  geom_hline(yintercept = 0, color = "gray50", size = 1.2) +
  #geom_jitter(width = 2, height = 0.2, alpha = 0.5 ,color = "gray70", size = 1) +
  geom_point(aes(y = jitter(rating)), alpha = 0.2, size = 1.2, position = pos) +
  geom_smooth(method = "lm", size = 2, se = F) +
  stat_summary(fun.data = "mean_cl_boot", size = sf*2, position = pos, stroke = sf*2, fill = "white") +
  labs(title = "Duration effect", subtitle = paste("n =",nSubj), col = "Recall", shape = "Recall") +
  xlab("D duration (ms)") + ylab("Rating") +
  scale_x_continuous(limits = limits_vec_dur, breaks = break_vec_dur, minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  scale_shape_manual(labels = c("Immediate", "Delay"), values=c(19,24)) + 
  scale_colour_manual(labels = c("Immediate", "Delay"), values=viridis(5)[c(2,4)]) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = strip_text_size),
        plot.title = element_text(size = plot_title_size, hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = plot_title_size/2, hjust=0, face = "italic", color = "black"),
        plot.margin = unit(c(0.2,0.4,0.2,0.2), "cm"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = axis_title_size, face = "bold"),
        axis.text.x = element_text(size = axis_text_size*0.9, colour="black"),
        axis.line = element_line(colour = 'black', size = 1.6),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length = unit(10, "pt"),
        legend.title = element_text(size = legend_title_size), 
        legend.text = element_text(size = legend_text_size),
        legend.position = "none",
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.key = element_rect(colour = NA, fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(1, "lines")
  )
### add theoretical segment ###
# Ad<-Bd<-Cd<-c(450,450,450,450)
# Dd<-xd<-c(0,450,1350,4050)
# yd<-((Ad/(Ad+Bd))-(Cd/(Cd+Dd)))*10
# #yd<-(((Ad*Af)/((Ad*Af)+(Bd*Bf)))-((Cd*Cf)/((Cd*Cf)+(Dd*Df))))*10
# for (i in 1:3) {
#   pD <- pD + geom_segment(x=xd[i],y=yd[i],xend=xd[i+1],yend=yd[i+1],col="red",size = 2,linetype="dotted")
# }
pD



# combine plots
# 1400 x 1100 pixels
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
e12 <- annotate_figure(ggarrange(pF,pD, plotlist = NULL, ncol = 2, nrow = NULL,
                  labels = NULL, label.x = 0, label.y = 1, hjust = -0.5,
                  vjust = 1.5, font.label = list(size = 52, color = "black", face = "bold", family = NULL), 
                  align = c("none", "h", "v", "hv"), widths = c(1,0.9), heights = 1, legend = "bottom", common.legend = TRUE),
                left = text_grob("Rating", size = plot_title_size,face = "bold", rot = 90, x = 0.5, y = 0.5))
e12

if (save_png == 1) {
  ggsave("C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/figures/fig2.png", 
       plot = e12, width = 25*1.61803398875, height = 25, units = "cm",limitsize = T)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # Experiment 2 # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

exp02_lf <- read.csv("exp16_lf.csv")
exp02_wf <- read.csv("exp16_wf.csv")

# general characteristics
nSubj <- nrow(exp02_wf); nSubj
sex <- table(exp02_wf$sex); sex
meanAge <- mean(exp02_wf$age); meanAge
sdAge <- sd(exp02_wf$age); sdAge
rangeAge <- range(exp02_wf$age); rangeAge



### adding the actual frequency and duration for each condition
# adding frequency and duration to conditions
exp02_lf$fA <- as.integer(revalue(substr(exp02_lf$condition,1,1),c("N"="4","Z"="24","P"="44")))
exp02_lf$fB <- as.integer(revalue(substr(exp02_lf$condition,1,1),c("N"="44","Z"="24","P"="4")))
exp02_lf$fC <- as.integer(24)
exp02_lf$fD <- as.integer(as.character(substr(exp02_lf$condition,2,4)))
exp02_lf$dA <- as.integer(450)
exp02_lf$dB <- as.integer(450)
exp02_lf$dC <- as.integer(450)
exp02_lf$dD <- ifelse(exp02_lf$fD==0,0,450)



######## STATISTICAL ANALYSIS ######## 
# rescaling
exp02_lf$scFrequency <- scale(exp02_lf$frequency)
# contrasts: linear: -3,-1,1,3, proportional: c(0,3,9,27)-mean(c(0,3,9,27))=-13,-9,-1,23
if (linear_contrast == 1) {
  exp02_lf$contFreq <- as.integer(revalue(as.character(exp02_lf$frequency), c("0"="-3","24"="-1","72"="1","216"="3")))
} else {
  exp02_lf$contFreq <- as.integer(revalue(as.character(exp02_lf$frequency), c("0"="-13","24"="-9","72"="-1","216"="23")))
}
exp02_lf$stage <- factor(exp02_lf$stage, levels = c("Z", "N", "P"))
exp02_lf$contSta <- as.integer(as.character(revalue(exp02_lf$stage,c("N"="-1","Z"="0","P"="1"))))
exp02_lf$subject <- as.factor(exp02_lf$subject)

# statistical analysis will be made with this, change this for constrast, scale or raw data (sensitivity analysis)
exp02_lf$freq <- exp02_lf$contFreq
exp02_lf$sta <- exp02_lf$contSta

plotANDstat_all <- 1
### exclude data for visualization
if (plotANDstat_all == 1) {
  # plot and analysis with 0.
  lf_no0 <- exp02_lf[!is.na(exp02_lf$rating),]
  
  # graphs data
  limits_vec <- c(-6, 222)
  break_vec <- c(0,24,72,216)
} else {
  # plot and analysis without 0.
  lf_no0 <- exp02_lf[exp02_lf$frequency != 0 & !is.na(exp02_lf$rating),]
  
  limits_vec <- c(0, 220)
  break_vec <- c(24, 72, 216)
}
###

# frequency model
mF <- lmer(rating ~ 1 + (freq * sta) + (1 + sta | subject) + (1 | condition) + stage_order, 
           control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
           #control = lmerControl(optimizer = "optimx", optCtrl=list(method='nlminb')),
           REML = FALSE,verbose = F, data = lf_no0); summary(mF) # Nelder_Mead, nloptwrap, bobyqa, calc.derivs = FALSE

# supplementary materials cell D paper (excluding 0 frequency condition) 30/09/2020
# mF <- lmer(rating ~ 1 + (freq * sta) + (1 + sta | subject) + (1 | condition) + stage_order,
#            control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
#            #control = lmerControl(optimizer = "optimx", optCtrl=list(method='nlminb')),
#            REML = FALSE,verbose = F, data = lf_no0[lf_no0$fD != 216,]); summary(mF); anova(mF)
# capture.output(summary(mF),file="test.doc")
mF_step <- step(mF); mF_step

mF_red <- lmer(rating ~ 1 + (sta) + (1 + sta | subject) + (1 | condition) + stage_order, 
           control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
           REML = FALSE,verbose = F, data = lf_no0); summary(mF_red)
anova(mF, mF_red, test="LRT"); exp((BIC(mF) - BIC(mF_red))/2); AIC(mF) - AIC(mF_red)



# post hoc analysis
fit_P <- lmer(rating ~ 1 + (freq) + stage_order + (1 | subject) + (1 | condition), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              data = lf_no0[lf_no0$stage == "P",]); summary(fit_P)
fit_Z <- lmer(rating ~ 1 + (freq) + stage_order + (1 | subject) + (1 | condition), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              data = lf_no0[lf_no0$stage == "Z",]); summary(fit_Z)
fit_N <- lmer(rating ~ 1 + (freq) + stage_order + (1 | subject) + (1 | condition), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              data = lf_no0[lf_no0$stage == "N",]); summary(fit_N)



#### #### #### #### #### ####

to_print <- table_long(report(mF))
to_print_post_hoc <- cbind(rep(c("P","Z","N"), each = 9),
                           rbind(table_long(report(fit_P)),
                                 table_long(report(fit_Z)),
                                 table_long(report(fit_N))))

if (write_csv == 1) {
  write.csv(to_print,"C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/stats/e2_fullMod_propContrasts.csv", 
            row.names = F)
  write.csv(to_print_post_hoc,"C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/stats/e2_fullMod_posthoc.csv", 
            row.names = F)
  print(mF_step)
}



######## VISUALIZATION ######## 
# change factor order, first negative (-0.42), then zero (0), and finnaly positive (0.42)
lf_no0$stage <- factor(lf_no0$stage, levels = c("N", "Z", "P"))
levels(lf_no0$stage) <- c('Negative', 'Zero', 'Positive')


# plot size in pixels 1500 x 1000. PDF 15 x 10
sf <- 1
plot_title_size <- 48#24
strip_text_size <- 44#22
axis_title_size <- 40#20
axis_text_size <- 36#18
legend_title_size <- 40#20
legend_text_size <- 36#18
pos <- position_dodge(10)

pF <- ggplot(lf_no0, aes(x = frequency, y = rating)) +
  geom_hline(yintercept = 0, color = "gray50", size = 1.2) +
  geom_line(aes(group = subject), alpha = 0.1 , size = 0.6,  position = pos)  +
  #geom_jitter(width = 2, height = 0.2, alpha = 0.2 , size = 1.2) +
  geom_point(aes(y = jitter(rating)), alpha = 0.2, size = 1.2, position = pos) +
  geom_smooth(method = "lm", col = "black", size = 2, se = F) +
  stat_summary(fun.data = "mean_cl_boot", size = 2, position = pos) +
  labs(title = "Frequency effect by contingency", subtitle = paste("n =",nSubj)) +
  xlab("D frequency") + ylab("Rating") +
  scale_x_continuous(limits = limits_vec, breaks = break_vec, minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  facet_grid(. ~ stage) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = strip_text_size),
        plot.title = element_text(size = plot_title_size, hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = plot_title_size/2, hjust=0, face = "italic", color = "black"),
        plot.margin = unit(c(0.2,0.4,0.2,0.2), "cm"),
        axis.title.y = element_text(size = axis_title_size, face="bold"),
        axis.text.y = element_text(size = axis_text_size, colour="black"),
        axis.title.x = element_text(size = axis_title_size, face="bold"),
        axis.text.x = element_text(size = axis_text_size*0.9, colour="black"),
        axis.line = element_line(colour = 'black', size = 1.6),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length = unit(10, "pt"),
        legend.title = element_text(size = legend_title_size), 
        legend.text = element_text(size = legend_text_size),
        legend.position = "bottom",
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.key = element_rect(colour = NA, fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(1, "lines")
  )
pF

if (save_png == 1) {
  ggsave("C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/figures/fig3.png", 
       plot = pF, width = 30*1.61803398875, height = 30, units = "cm",limitsize = T)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # Experiment 3 # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

exp03_lf <- read.csv("exp18_lf.csv")
exp03_wf <- read.csv("exp18_wf.csv")

# general characteristics
nSubj <- nrow(exp03_wf); nSubj
sex <- table(exp03_wf$sex); sex
meanAge <- mean(exp03_wf$age); meanAge
sdAge <- sd(exp03_wf$age); sdAge
rangeAge <- range(exp03_wf$age); rangeAge



### adding the actual frequency and duration for each condition
# adding frequency and duration to conditions
exp03_lf$fA <- as.integer(revalue(substr(exp03_lf$condition,1,1),c("N"="4","Z"="24","P"="44")))
exp03_lf$fB <- as.integer(revalue(substr(exp03_lf$condition,1,1),c("N"="44","Z"="24","P"="4")))
exp03_lf$fC <- as.integer(24)
exp03_lf$fD <- as.integer(as.character(substr(exp03_lf$condition,3,5)))
exp03_lf$dA <- as.integer(450)
exp03_lf$dB <- as.integer(450)
exp03_lf$dC <- as.integer(450)
exp03_lf$dD <- as.integer(as.character(substr(exp03_lf$condition,7,10)))
exp03_lf$dD[exp03_lf$fD==0] <- 0

######## STATISTICAL ANALYSIS ######## 
# reorder factor order and rescaling
exp03_lf$scFrequency <- scale(exp03_lf$frequency)
exp03_lf$scDuration <- scale(exp03_lf$duration)
# contrasts: linear: -3,-1,1,3, proportional: c(0,3,9,27)-mean(c(0,3,9,27))=-13,-9,-1,23
if (linear_contrast == 1) {
  exp03_lf$contFreq <- as.integer(revalue(as.character(exp03_lf$frequency),c("0"="-3","8"="-1","24"="1","72"="3")))
  exp03_lf$contDur <- as.integer(revalue(as.character(exp03_lf$duration),c("0"="-3","150"="-1","450"="1","1350"="3")))
} else {
  exp03_lf$contFreq <- as.integer(revalue(as.character(exp03_lf$frequency),c("0"="-9","8"="-7","24"="-1","72"="17")))
  exp03_lf$contDur <- as.integer(revalue(as.character(exp03_lf$duration),c("0"="-9","150"="-7","450"="-1","1350"="17")))
}
exp03_lf$subject <- as.factor(exp03_lf$subject)
exp03_lf$stage <- factor(exp03_lf$stage, levels = c("Z", "N", "P"))
exp03_lf$contSta <- as.integer(as.character(revalue(exp03_lf$stage,c("N"="-1","Z"="0","P"="1"))))



# statistical analysis will be made with this, change this for constrast, scale or raw data (sensitivity analysis)
exp03_lf$freq <- exp03_lf$contFreq
exp03_lf$dur <- exp03_lf$contDur
exp03_lf$sta <- exp03_lf$contSta


plotANDstat_all <- 1
### exclude data for visualization
if (plotANDstat_all == 1) {
  # plot and analysis with 0.
  a <- exp03_lf[!is.na(exp03_lf$rating) & exp03_lf$frequency == 0,]
  a[, "adjustedDur"] <- "No"
  a[, "adjustedNum"] <- "No"
  
  lf_no0 <- rbind(exp03_lf[!is.na(exp03_lf$rating),],a)
  
  # graphs data
  limits_vec <- c(-2, 80)
  break_vec <- c(0,8,24,72)
  # limits_vec2 <- c(-50, 1400)
  # break_vec2 <- c(0,150,450,1350)
} else {
  # plot and analysis without 0.
  lf_no0 <- exp03_lf[exp03_lf$frequency != 0 & !is.na(exp03_lf$rating),]

  limits_vec <- c(0, 80)
  break_vec <- c(8,24,72)
  # limits_vec2 <- c(100, 1400)
  # break_vec2 <- c(150,450,1350)
}
###



# full and reduced models
mF <- lmer(rating ~ 1 + (freq * sta * adjustedDur) + (1 + sta | subject) + (1 | condition) + stage_order, 
           control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), REML = FALSE,
           data = lf_no0); summary(mF)
mF_step <- step(mF); mF_step

mRed <- lmer(rating ~ 1 + (freq * sta ) + (1 + sta | subject) + (1 | condition) + stage_order, 
           control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), REML = FALSE,
           data = lf_no0); summary(mRed)
mRed_step <- step(mRed); mRed_step

anova(mF, mRed, test="LRT"); exp((BIC(mF) - BIC(mRed))/2); AIC(mF) - AIC(mRed)



#### #### #### #### #### ####

csv_print <- table_long(report(mF))

if (write_csv == 1) {
  write.csv(csv_print,"C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/stats/e3_fullMod_propContrasts.csv", 
            row.names = F)
  print(mF_step)
}


######## VISUALIZATION ######## 
# get data ready
# change factor oder, first positive (0.42), then zero (0), and finnaly negative (-0.42)
lf_no0$stage <- factor(lf_no0$stage, levels = c("N", "Z", "P"))
levels(lf_no0$stage) <- c('Negative', 'Zero', 'Positive')



# plot size in pixels 1500 x 1000. PDF 15 x 10
if (!require(viridis)) {install.packages("viridis")}; library(viridis)
sf <- 1
plot_title_size <- 48#24
strip_text_size <- 44#22
axis_title_size <- 40#20
axis_text_size <- 36#18
legend_title_size <- 40#20
legend_text_size <- 36#18
pos <- position_dodge(5)

#### get means for excel fitting, 13/03/2020
# lf_no0 %>% group_by(stage,frequency) %>% 
#   summarize(mean(rating))
####

pF <- ggplot(lf_no0, aes(x = frequency, y = rating, col = adjustedDur, shape = adjustedDur)) +
  geom_hline(yintercept = 0, color = "gray50", size = sf*1.2) +
  #geom_jitter(width = 2, height = 0.2, alpha = 0.5 ,color = "gray70", size = 1) +
  geom_point(aes(y = jitter(rating)),alpha = 0.2, size = sf*1.2, position = pos) +
  geom_smooth(method = "lm", size = sf*2, se = F) +
  stat_summary(fun.data = "mean_cl_boot", size = sf*2, position = pos, stroke = sf*2, fill = "white") +
  labs(title = "Frequency effect by Contingency", subtitle = paste("n =",nSubj), col = "D trial duration", shape = "D trial duration") + 
  xlab("D frequency") + ylab("Rating") +
  scale_x_continuous(limits = limits_vec, breaks = break_vec, minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  scale_shape_manual(labels = c("Constant'","Adjusted"), values=c(19,24)) + 
  scale_colour_manual(labels = c("Constant'","Adjusted"), values=viridis(5)[c(2,4)]) +
  facet_grid(. ~ stage) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = sf*strip_text_size),
        plot.title = element_text(size = sf*plot_title_size, hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = sf*plot_title_size/2, hjust=0, face = "italic", color = "black"),
        plot.margin = unit(c(0.2,0.4,0.2,0.2), "cm"),
        axis.title.y = element_text(size = sf*axis_title_size, face = "bold"),
        axis.text.y = element_text(size = sf*axis_text_size, colour="black"),
        axis.title.x = element_text(size = sf*axis_title_size, face = "bold"),
        axis.text.x = element_text(size = sf*axis_text_size*0.9, colour="black"),
        axis.line = element_line(colour = 'black', size = sf*1.6),
        axis.ticks = element_line(size = sf*1.6),
        axis.ticks.length = unit(sf*10, "pt"),
        legend.title = element_text(size = sf*legend_title_size), 
        legend.text = element_text(size = sf*legend_text_size),
        legend.position = "bottom",
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.key = element_rect(colour = NA, fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(1, "lines")
)
pF

if (save_png == 1) {
  ggsave("C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/figures/fig4a.png", 
       plot = pF, width = 30*1.61803398875, height = 30, units = "cm",limitsize = T)
}
  
pos <- position_dodge(50)
pD <- ggplot(lf_no0, aes(x = duration, y = rating, col = adjustedNum, shape = adjustedNum)) +
  geom_hline(yintercept = 0, color = "gray50", size = sf*1.2) +
  #geom_jitter(width = 2, height = 0.2, alpha = 0.5 ,color = "gray70", size = 1) +
  geom_point(aes(y = jitter(rating)),alpha = 0.2, size = sf*1.2, position = pos) +
  geom_smooth(method = "lm", size = sf*2, se = F) +
  stat_summary(fun.data = "mean_cl_boot", size = sf*2, position = pos, stroke = sf*2, fill = "white") +
  labs(title = "Duration effect by Contingency", subtitle = paste("n =",nSubj), col = "D trial frequency", shape = "D trial frequency") + 
  xlab("D duration") + ylab("Rating") +
  scale_x_continuous(limits = c(-50,1400), breaks = c(0,150,450,1350), minor_breaks = NULL) +
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  scale_shape_manual(labels = c("Constant'","Adjusted"), values=c(19,24)) + 
  scale_colour_manual(labels = c("Constant'","Adjusted"), values=viridis(5)[c(2,4)]) +
  facet_grid(. ~ stage) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = sf*strip_text_size),
        plot.title = element_text(size = sf*plot_title_size, hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = sf*plot_title_size/2, hjust=0, face = "italic", color = "black"),
        plot.margin = unit(c(0.2,0.4,0.2,0.2), "cm"),
        axis.title.y = element_text(size = sf*axis_title_size, face = "bold"),
        axis.text.y = element_text(size = sf*axis_text_size, colour="black"),
        axis.title.x = element_text(size = sf*axis_title_size, face = "bold"),
        axis.text.x = element_text(size = sf*axis_text_size*0.9, colour="black"),
        axis.line = element_line(colour = 'black', size = sf*1.6),
        axis.ticks = element_line(size = sf*1.6),
        axis.ticks.length = unit(sf*10, "pt"),
        legend.title = element_text(size = sf*legend_title_size), 
        legend.text = element_text(size = sf*legend_text_size),
        legend.position = "bottom",
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.key = element_rect(colour = NA, fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(1, "lines")
  )
pD

if (save_png == 1) {
  ggsave("C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/figures/fig4b.png", 
       plot = pD, width = 30*1.61803398875, height = 30, units = "cm",limitsize = T)
}


p <- ggarrange(pF,pD,nrow=2)

if (save_png == 1) {
  ggsave("C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/figures/fig4.png", 
         plot = p, width = 30*1.61803398875, height = 60, dpi = 600, units = "cm", limitsize = T)
}





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # combine experiments # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# add exp 1
exp01_lf$stage <- as.character("Z")
exp01_lf$exp <- as.character(1)
exp01_lf$subject <- paste0("ex",exp01_lf$exp,"_p",exp01_lf$subject)

# add exp 2
exp02_lf$exp <- as.character(2)
exp02_lf$subject <- paste0("ex",exp02_lf$exp,"_p",exp02_lf$subject)

# add exp 3
exp03_lf$exp <- as.character(3)
exp03_lf$subject <- paste0("ex",exp03_lf$exp,"_p",exp03_lf$subject)

# eliminate_zero?
eliminate_zero <- 0



# pooled experiment

rel_col <- c("exp","subject","condition","cond_order","stage","rating",
             "fA","fB","fC","fD","dA","dB","dC","dD")

lf <- rbind(exp01_lf[,rel_col],exp02_lf[,rel_col],exp03_lf[,rel_col])
lf$stage <- factor(lf$stage, levels = c("P","Z","N"))
lf$exp <- as.factor(lf$exp)
lf$subject <- as.factor(lf$subject)
levels(lf$stage) <- c("Positive","Zero","Negative")
nSubj <- length(levels(lf$subject))

# print for meta analysis #
lf$paper <- "cell_d"
lf <- lf[,c("paper",colnames(lf)[-ncol(lf)])]
# write.csv(lf,"cellD.csv",row.names = F)



#### visualization parameters ####
sf <- 1 # scaling factor
plot_title_size <- 40#24
strip_text_size <- 35#22
axis_title_size <- 35#20
axis_text_size <- 25#18
legend_title_size <- 35#20
legend_text_size <- 30#18
pos <- position_dodge(0.1)

theme_scdo <- theme(strip.background = element_blank(),
                    strip.text = element_text(size = sf*strip_text_size, face = "bold"),
                    plot.title = element_text(size = sf*plot_title_size, hjust = 0,face = "bold"),
                    plot.subtitle = element_text(size = sf*plot_title_size/2, hjust = 0, face = "italic", color = "black"),
                    plot.margin = unit(c(0.2,0.4,0.2,0.2), "cm"),
                    axis.title.y = element_text(size = sf*axis_title_size, face = "bold"),
                    axis.text.y = element_text(size = sf*axis_text_size, colour="black"),
                    axis.title.x = element_text(size = sf*axis_title_size, face = "bold"),
                    axis.text.x = element_text(size = sf*axis_text_size*0.8, colour="black"),
                    axis.line = element_line(colour = 'black', size = sf*1.6),
                    axis.ticks = element_line(size = sf*1.6),
                    axis.ticks.length = unit(sf*10, "pt"),
                    legend.title = element_text(size = sf*legend_title_size,face = "bold"), 
                    legend.text = element_text(size = sf*legend_text_size),
                    legend.position = "right",
                    legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
                    legend.key = element_rect(colour = NA, fill = NA),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),  
                    panel.background = element_blank(),
                    panel.border = element_blank(),
                    panel.spacing = unit(sf*1, "lines")
)



#### Figure 5 ####
# delta p calculation
# frequency delta P
lf$deltaP <- (lf$fA/(lf$fA+lf$fB))-(lf$fC/(lf$fC+lf$fD))
# frequency * duration delta P
lf$deltaP_wDur <- ((lf$fA*lf$dA)/((lf$fA*lf$dA)+(lf$fB*lf$dB)))-((lf$fC*lf$dC)/((lf$fC*lf$dC)+(lf$fD*lf$dD)))
# duration delta P
lf$deltaP_Dur <- (lf$dA/(lf$dA+lf$dB))-(lf$dC/(lf$dC+lf$dD))
# rates
lf$lambda_s1 <- lf$fA / ( ((lf$fA*lf$dA)+(lf$fB*lf$dB)) /1000)
lf$lambda_background <- lf$fC / ( ((lf$fC*lf$dC)+(lf$fD*lf$dD)) /1000)
lf$rate <- log(lf$lambda_s1 / lf$lambda_background)

# changing factor order
lf$stage <- factor(lf$stage,levels=c("Negative","Zero","Positive"))


### model comparisons ###
mDeltap <- lmer(rating ~ deltaP + (1|exp/subject),lf); summary(mDeltap)
mDeltap_dur <- lmer(rating ~ deltaP_Dur + (1|exp/subject),lf); summary(mDeltap_dur)
mDeltap_wDur <- lmer(rating ~ deltaP_wDur + (1|exp/subject),lf); summary(mDeltap_wDur)
mLograte <- lmer(rating ~ rate + (1|exp/subject),lf); summary(mLograte)

mBic <- BIC(mDeltap,mDeltap_dur,mDeltap_wDur,mLograte)
mAic <- AIC(mDeltap,mDeltap_dur,mDeltap_wDur,mLograte)



#### Figure 5 probabilities (delta P) ####
#options(scipen=999)
pos <- position_dodge(0.01)

cor_dP_rat <- cor.test(lf$deltaP,lf$rating)
fig5a <- ggplot(lf[,],aes(x=deltaP,y=rating)) +
  geom_hline(yintercept = 0, color = "gray90", size = sf*0.5) +
  geom_vline(xintercept = 0, color = "gray90", size = sf*0.5) +
  geom_jitter(height = 0.25, width = 0.015, alpha = 0.3, size = 0.6, col = "grey70") +
  # geom_segment(aes(x=-1,y=-10,xend=1,yend=10), col = "red", linetype = "dashed",lwd=2) +
  geom_smooth(method = "lm", size = 1.5, se=F, col = "black") +
  stat_summary(fun.data = "mean_cl_boot", size = 0.5, position = pos) +
  # annotate("text", x = -0.5, y = 10, label = paste0("r(",cor_dPdur_rat$parameter,") = ",round(cor_dP_rat$estimate,2),", p < 0.001"), size = 10) +
  annotate("text", x = 0.7, y = -8, label = paste0("BIC = ",round(mBic[1,2],1)), size = 3) +
  annotate("text", x = 0.7, y = -10, label = paste0("AIC = ",round(mAic[1,2],1)), size = 3) +
  labs(title = "Frequency", x = expression(Delta*p), y = "Rating") +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1,by=0.50), minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  theme_classic() #theme_scdo #+ # line legend
fig5a

cor_dPdur_rat <- cor.test(lf$deltaP_Dur,lf$rating)
fig5b <- ggplot(lf[,],aes(x=deltaP_Dur,y=rating)) + 
  geom_hline(yintercept = 0, color = "gray90", size = sf*0.5) +
  geom_vline(xintercept = 0, color = "gray90", size = sf*0.5) +
  geom_jitter(height = 0.25, width = 0.015, alpha = 0.3, size = 0.6, col = "grey70") +
  # geom_segment(aes(x=-1,y=-10,xend=1,yend=10), col = "red", linetype = "dashed",lwd=2) +
  geom_smooth(method = "lm", size = 1.5, se=F, col = "black") +
  stat_summary(fun.data = "mean_cl_boot", size = 0.5, position = pos) +
  # annotate("text", x = -0.5, y = 10, label = paste0("r(",cor_dPdur_rat$parameter,") = ",round(cor_dPdur_rat$estimate,2),", p < 0.001"), size = 10) +
  annotate("text", x = 0.7, y = -8, label = paste0("BIC = ",round(mBic[2,2],1)), size = 3) +
  annotate("text", x = 0.7, y = -10, label = paste0("AIC = ",round(mAic[2,2],1)), size = 3) +
  labs(title = "Duration", x = expression(Delta*p["duration"]), y = "Rating") +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1,by=0.50), minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  theme_classic() + theme(axis.title.y = element_blank()) #theme_scdo #+ # line legend
fig5b

cor_dPwDur_rat <- cor.test(lf$deltaP_wDur,lf$rating)
fig5c <- ggplot(lf[,],aes(x=deltaP_wDur,y=rating)) +
  geom_hline(yintercept = 0, color = "gray90", size = sf*0.5) +
  geom_vline(xintercept = 0, color = "gray90", size = sf*0.5) +
  geom_jitter(height = 0.25, width = 0.015, alpha = 0.3, size = 0.6, col = "grey70") +
  # geom_segment(aes(x=-1,y=-10,xend=1,yend=10), col = "red", linetype = "dashed",lwd=2) +
  geom_smooth(method = "lm", size = 1.5, se=F, col = "black") +
  stat_summary(fun.data = "mean_cl_boot", size = 0.5, position = pos) +
  # annotate("text", x = -0.5, y = 10, label = paste0("r(",cor_dPwDur_rat$parameter,") = ",round(cor_dPwDur_rat$estimate,2),", p < 0.001"), size = 10) +
  annotate("text", x = 0.7, y = -8, label = paste0("BIC = ",round(mBic[3,2],1)), size = 3) +
  annotate("text", x = 0.7, y = -10, label = paste0("AIC = ",round(mAic[3,2],1)), size = 3) +
  labs(title = "Frequency * Duration", x = expression(Delta*p["freq*dur"]), y = "Rating") +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1,by=0.50), minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  theme_classic() #theme_scdo #+ # line legend
fig5c

cor_rate_rat <- cor.test(lf$rate,lf$rating)
fig5d <- ggplot(lf[,],aes(x=rate,y=rating)) + #, group = olifeHL, col = olifeHL, shape = olifeHL)) + 
  geom_hline(yintercept = 0, color = "gray90", size = sf*0.5) +
  geom_vline(xintercept = 0, color = "gray90", size = sf*0.5) +
  geom_jitter(height = 0.25, width = 0.03, alpha = 0.3, size = 0.6, col = "grey70") +
  #geom_segment(aes(x=-1,y=-10,xend=1,yend=10), col = "red", linetype = "dashed",lwd=2) +
  geom_smooth(method = "lm", size = 1.5, se=F, col = "black") +
  stat_summary(fun.data = "mean_cl_boot", size = 0.5, position = position_dodge(0.02)) +
  # annotate("text", x = -1.2, y = 10, label = paste0("r(",cor_rate_rat$parameter,") = ",round(cor_rate_rat$estimate,2),", p < 0.001"), size = 10) +
  annotate("text", x = 1.4, y = -8, label = paste0("BIC = ",round(mBic[3,2],1)), size = 3) +
  annotate("text", x = 1.4, y = -10, label = paste0("AIC = ",round(mAic[3,2],1)), size = 3) +
  labs(title = "Rates (Frequency over Duration)", x = expression(ln(lambda[S1] / lambda[TM])), y = "Rating") +
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  theme_classic() + theme(axis.title.y = element_blank())#theme_scdo
fig5d

fig5 <- annotate_figure(ggarrange(fig5a,fig5b,fig5c,fig5d, nrow=2,ncol=2, labels = c(LETTERS[1:4])),
                        top =  text_grob(expression(paste(N["participants"]," = 125; ",N["ratings"]," = 1980"))))

if (save_png == 1) {
  ggsave("C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/figures/fig5.png", 
         plot = fig5, width = 17, height = 17,dpi = 900, units = "cm",limitsize = T)
}



