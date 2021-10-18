# IMPORTANT: code modified given reviewers comments
# Paper "Benefiting from trial spacing without the cost of prolonged training: ...",
# submitted to Journal of Experimental Psychology: General.

rm(list = ls()) # erase previous Global Environment, import manually the file
setwd(file.path(dirname(rstudioapi::getActiveDocumentContext()$path))) # set workspace

# packages needed
if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest) # lmer
if (!require(report)) {install.packages("report")}; library(report) # report_table()
if (!require(bayestestR)) {install.packages("bayestestR")}; library(bayestestR) # bayesfactor_models()
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

exp01_lf <- read.csv(paste0(getwd(),"/raw_data/exp12_lf.csv"))
exp01_wf <- read.csv(paste0(getwd(),"/raw_data/exp12_wf.csv"))

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
# rescaling (not used for the current analysis)
exp01_lf$scFrequency <- scale(exp01_lf$frequency)
exp01_lf$scDuration <- scale(exp01_lf$frequency)
# contrasts: 
# Frequency contrasts: f = [0, 36, 108, 324]; contrasts: (f - mean(f))/9 = [-13, -9, -1, 23]
# Duration contrasts: d = [0, 450, 1350, 4050]; contrasts: (d - mean(d))/ 112.5 = [-13, -9, -1, 23]
# Ref: https://arxiv.org/pdf/1807.10451.pdf
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
exp01_lf$stage_order <- ifelse(exp01_lf$cond_order < 5, 1, ifelse(exp01_lf$cond_order > 8, 3, 2)) # does not make sense in the experiment (given the design), 
                                                                                                  # however is good to have it as all the 
                                                                                                  # other experiment analysis.

# statistical analysis will be made with this, change this for contrast, scale or raw data (sensitivity analysis)
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
# for non convergence models # convergence failiure test: https://github.com/lme4/lme4/issues/120
#relgrad <- with(mod@optinfo$derivs,solve(Hessian,gradient))
#max(abs(relgrad))

# Random effect structured (1 + delay + freq + dur | subject) as suggested by Reviewer # 2
e1.m0 <- lmer(rating ~ 1 + cond_order + (1 + delay + freq + dur | subject),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)

e1.m1 <- lmer(rating ~ 1 + delay * (freq + dur) + cond_order + (1 + delay + freq + dur | subject),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0) # Nelder_Mead, nloptwrap, bobyqa
e1.m1_step <- step(e1.m1); e1.m1_step


e1.m2 <- lmer(rating ~ 1 + dur * delay + cond_order + (1 + delay + freq + dur | subject),
                  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
                  REML = FALSE, data = lf_no0)

e1.m3 <- lmer(rating ~ 1 +  freq * delay + cond_order + (1 + delay + freq + dur | subject),
                  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
                  REML = FALSE, data = lf_no0)

e1.m4 <- lmer(rating ~ 1 + freq + dur + cond_order + (1 + delay + freq + dur | subject),
                  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
                  REML = FALSE, data = lf_no0)

e1.m5 <- lmer(rating ~ 1 + freq + cond_order + (1 + delay + freq + dur | subject),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              REML = FALSE, data = lf_no0)

e1.m6 <- lmer(rating ~ 1 + dur + cond_order + (1 + delay + freq + dur | subject),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              REML = FALSE, data = lf_no0)

e1.m7 <- lmer(rating ~ 1 + delay + cond_order + (1 + delay + freq + dur | subject),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              REML = FALSE, data = lf_no0)



## model comparisons

# likelihood ratio tests
anova(e1.m1, e1.m0, test="LRT")
anova(e1.m1, e1.m2, test="LRT")
anova(e1.m1, e1.m3, test="LRT")
anova(e1.m1, e1.m4, test="LRT")

# Bayes factor (BF) for all possible models
e1.BFs <- bayesfactor_models(e1.m1,e1.m2,e1.m3,e1.m4,e1.m5,e1.m6,e1.m7, denominator = e1.m0)
e1.BFs <- as.matrix(e1.BFs)
e1.BFs.lf <- melt(e1.BFs); 
e1.BFs.lf$lab <- ifelse(e1.BFs.lf$value > 1000, ">1000",
                        ifelse(e1.BFs.lf$value < 0.001, "<0.001",
                               ifelse(e1.BFs.lf$value > 0.001 & e1.BFs.lf$value < 1,
                                      round(e1.BFs.lf$value,3),
                                      round(e1.BFs.lf$value,1))))
logLab <- c(0.001,0.01,0.1,1,10,100,1000)
e1_BFplot <- ggplot(e1.BFs.lf,aes(x=Var1,y=Var2,fill=log(value),label=lab)) + 
  geom_tile() + geom_text() +
  scale_fill_gradient2(low = "blue4", mid = "white", high = "red4", midpoint = 0,
                       breaks=log(logLab),labels=logLab) +
  labs(title = "Experiment 1: Model Comparisons",subtitle = "Bayes Factors (BF): bayesfactor_models() function from bayestestR",
       x = "Denominator model", y = "Numerator model", fill = "BF") +
  theme_classic() + 
  guides(fill = guide_colourbar(barwidth = 1.2, barheight = 12, ticks = F)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
e1_BFplot
if (save_png == 1) {
  ggsave(paste0(getwd(),"/figures/figS2.png"),
         plot = e1_BFplot, width = 26, height = 20, dpi = 1800, units = "cm",limitsize = T)
}

# AIC and BIC
e1.fits <- cbind(BIC(e1.m1,e1.m2,e1.m3,e1.m4,e1.m5,e1.m6,e1.m7,e1.m0),
                 AIC(e1.m1,e1.m2,e1.m3,e1.m4,e1.m5,e1.m6,e1.m7,e1.m0)[,2])
colnames(e1.fits) <- c("df","BIC","AIC")
e1.fits
e1.fits$Models <- e1.BFs.lf$Var1[1:nrow(e1.fits)]



#### #### #### #### #### ####
e1.m1.tab <- report_table(e1.m1)

if (write_csv == 1) {
  write.csv(e1.m1.tab,paste0(getwd(),"/stats/e1_fullMod_propContrast_v2.csv"),
            na="",row.names=F)
  write.csv(e1.fits,paste0(getwd(),"/stats/e1_modComp_propContrast_v2.csv"))
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
pD



# combine plots
# 1400 x 1100 pixels
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
fig2 <- annotate_figure(ggarrange(pF,pD, plotlist = NULL, ncol = 2, nrow = NULL,
                  labels = NULL, label.x = 0, label.y = 1, hjust = -0.5,
                  vjust = 1.5, font.label = list(size = 52, color = "black", face = "bold", family = NULL), 
                  align = c("none", "h", "v", "hv"), widths = c(1,0.9), heights = 1, legend = "bottom", common.legend = TRUE),
                left = text_grob("Rating", size = plot_title_size,face = "bold", rot = 90, x = 0.5, y = 0.5))
fig2

if (save_png == 1) {
  ggsave(paste0(getwd(),"/figures/fig2.png"),
       plot = fig2, width = 25*1.61803398875, height = 25, dpi = 1800, units = "cm",limitsize = T)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # Experiment 2 # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

exp02_lf <- read.csv(paste0(getwd(),"/raw_data/exp16_lf.csv"))
exp02_wf <- read.csv(paste0(getwd(),"/raw_data/exp16_wf.csv"))

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
# contrasts: 
# Frequency contrasts: f = [0,24,72,216]; contrasts: (f - mean(f))/6 = [-13, -9, -1, 23]
# Ref: https://arxiv.org/pdf/1807.10451.pdf
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



### ### ###
e2.m0 <- lmer(rating ~ 1 + stage_order + (1 + freq + sta | subject),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)

e2.m1 <- lmer(rating ~ 1 + freq + sta + freq:sta + stage_order + (1 + freq + sta | subject),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)
e2.m1_step <- step(e2.m1); e2.m1_step

e2.m2 <- lmer(rating ~ 1 + freq + sta + stage_order + (1 + freq + sta | subject),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)

e2.m3 <- lmer(rating ~ 1 + sta + stage_order + (1 + freq + sta | subject),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)

e2.m4 <- lmer(rating ~ 1 + freq + stage_order + (1 + freq + sta | subject),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)

## model comparisons

# likelihood ratio tests
anova(e2.m1, e2.m3, test="LRT")

# Bayes factor (BF) for all possible models
e2.BFs <- bayesfactor_models(e2.m1,e2.m2,e2.m3,e2.m4, denominator = e2.m0)
e2.BFs <- as.matrix(e2.BFs)
e2.BFs.lf <- melt(e2.BFs); 
e2.BFs.lf$lab <- ifelse(e2.BFs.lf$value > 1000, ">1000",
                        ifelse(e2.BFs.lf$value < 0.001, "<0.001",
                               ifelse(e2.BFs.lf$value > 0.001 & e2.BFs.lf$value < 1,
                                      round(e2.BFs.lf$value,3),
                                      round(e2.BFs.lf$value,1))))
logLab <- c(0.001,1,1000)
e2_BFplot <- ggplot(e2.BFs.lf,aes(x=Var1,y=Var2,fill=log(value),label=lab)) + 
  geom_tile() + geom_text() +
  scale_fill_gradient2(low = "blue4", mid = "white", high = "red4", midpoint = 0,
                       breaks=log(logLab),labels=logLab) +
  labs(title = "Experiment 2: Model Comparisons",subtitle = "Bayes Factors (BF): bayesfactor_models() function from bayestestR",
       x = "Denominator model", y = "Numerator model", fill = "BF") +
  theme_classic() + 
  guides(fill = guide_colourbar(barwidth = 1.2, barheight = 12, ticks = F)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
e2_BFplot
if (save_png == 1) {
  ggsave(paste0(getwd(),"/figures/figS3.png"),
         plot = e2_BFplot, width = 26, height = 20, dpi = 1800, units = "cm",limitsize = T)
}

# AIC and BIC
e2.fits <- cbind(BIC(e2.m1,e2.m2,e2.m3,e2.m4,e2.m0),
                 AIC(e2.m1,e2.m2,e2.m3,e2.m4,e2.m0)[,2])
colnames(e2.fits) <- c("df","BIC","AIC")
e2.fits
e2.fits$Models <- e2.BFs.lf$Var1[1:nrow(e2.fits)]



# post-hoc analysis
fit_P <- lmer(rating ~ 1 + freq + stage_order + (1 + freq | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              data = lf_no0[lf_no0$stage == "P",])
fit_Z <- lmer(rating ~ 1 + freq + stage_order + (1 + freq | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              data = lf_no0[lf_no0$stage == "Z",])
fit_N <- lmer(rating ~ 1 + freq + stage_order + (1 + freq | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              data = lf_no0[lf_no0$stage == "N",])

# anova no 216 frequency
e2.sen2 <- lmer(rating ~ 1 + freq * sta + stage_order + (1 + freq + sta | subject),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0[lf_no0$frequency != 216,])
anova(e2.sen2) 



#### #### #### #### #### ####
e2.m1.tab <- report_table(e2.m1)
to_print_post_hoc <- cbind(rep(c("P","Z","N"), each = 3),
                           rbind(report_table(fit_P)[1:3,],
                                 report_table(fit_Z)[1:3,],
                                 report_table(fit_N)[1:3,]))

if (write_csv == 1) {
  write.csv(e2.m1.tab,paste0(getwd(),"/stats/e2_fullMod_propContrast_v2.csv"))
  write.csv(e2.fits,paste0(getwd(),"/stats/e2_modComp_propContrast_v2.csv"))
  write.csv(to_print_post_hoc,paste0(getwd(),"/stats/e2_postHoc_propContrast_v3.csv"))
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

# p1<-ggplot(lf_no0[lf_no0$stage_order == 1,], aes(x = frequency, y = rating)) +
#   stat_summary() + labs(title = "first stage") + facet_grid(. ~ stage)
# p2<-ggplot(lf_no0[lf_no0$stage_order == 2,], aes(x = frequency, y = rating)) +
#   stat_summary() + labs(title = "second stage") + facet_grid(. ~ stage)
# p3<-ggplot(lf_no0[lf_no0$stage_order == 3,], aes(x = frequency, y = rating)) +
#   stat_summary() + labs(title = "third stage") + facet_grid(. ~ stage)
# ggarrange(p1,p2,p3,nrow=3)
# ggplot(lf_no0[,], aes(x = cond_order , y = rating, col = stage)) +
  # stat_summary(geom="line") + labs(title = "third stage")
# condOrdeSubj <- as.vector(NA)
# counter <- 0
# for (s in unique(lf_no0$subject)) {
#   temp <- lf_no0[lf_no0$subject == s,]
#   counter <- counter + 1
#   condOrdeSubj[counter] <- paste(unique(temp$stage)[1],unique(temp$stage)[2],unique(temp$stage)[3])
# }

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
  ggsave(paste0(getwd(),"/figures/fig3_v2.png"),
         plot = pF, width = 30*1.61803398875, height = 30, dpi = 1200, units = "cm",limitsize = T)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # Experiment 3 # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

exp03_lf <- read.csv(paste0(getwd(),"/raw_data/exp18_lf.csv"))
exp03_wf <- read.csv(paste0(getwd(),"/raw_data/exp18_wf.csv"))

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
# contrasts: 
# Frequency contrasts: f = [0,8,24,72]; contrasts: (f - mean(f))/2 = [-13, -9, -1, 23]
# Duration contrasts: d = [0,150,450,1350]; contrasts: (d - mean(d))/37.5 = [-13, -9, -1, 23]

if (linear_contrast == 1) {
  exp03_lf$contFreq <- as.integer(revalue(as.character(exp03_lf$frequency),c("0"="-3","8"="-1","24"="1","72"="3")))
  exp03_lf$contDur <- as.integer(revalue(as.character(exp03_lf$duration),c("0"="-3","150"="-1","450"="1","1350"="3")))
} else {
  exp03_lf$contFreq <- as.integer(revalue(as.character(exp03_lf$frequency),c("0"="-13","8"="-9","24"="-1","72"="23")))
  exp03_lf$contDur <- as.integer(revalue(as.character(exp03_lf$duration),c("0"="-13","150"="-9","450"="-1","1350"="23")))
}
exp03_lf$subject <- as.factor(exp03_lf$subject)
exp03_lf$stage <- factor(exp03_lf$stage, levels = c("Z", "N", "P"))
exp03_lf$contSta <- as.integer(as.character(revalue(exp03_lf$stage,c("N"="-1","Z"="0","P"="1"))))



# statistical analysis will be made with this, change this for contrast, scale or raw data (sensitivity analysis)
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
  lf_no0 <- rbind(exp03_lf[!is.na(exp03_lf$rating),],a);rm(a)
  # graphs data
  limits_vec <- c(-2, 80)
  break_vec <- c(0,8,24,72)
} else {
  # plot and analysis without 0.
  lf_no0 <- exp03_lf[exp03_lf$frequency != 0 & !is.na(exp03_lf$rating),]

  limits_vec <- c(0, 80)
  break_vec <- c(8,24,72)
}



### ### ###
e3.m0 <- lmer(rating ~ 1 + stage_order + (1 + freq + sta | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)

e3.m1 <- lmer(rating ~ 1 + freq * sta * adjustedDur + stage_order + (1 + freq + sta | subject), 
           control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
           REML = F, verbose = F, data = lf_no0)
e3.m1_step <- step(e3.m1); e3.m1_step

e3.m2 <- lmer(rating ~ 1 + freq + sta + freq:sta + stage_order + (1 + freq + sta | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)

e3.m3 <- lmer(rating ~ 1 + freq + sta + stage_order + (1 + freq + sta | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)

e3.m4 <- lmer(rating ~ 1 + freq * adjustedDur + stage_order + (1 + freq + sta | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)

e3.m5 <- lmer(rating ~ 1 + sta * adjustedDur + stage_order + (1 + freq + sta | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)

e3.m6 <- lmer(rating ~ 1 + freq + stage_order + (1 + freq + sta | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)

e3.m7 <- lmer(rating ~ 1 + sta + stage_order + (1 + freq + sta | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)

e3.m8 <- lmer(rating ~ 1 + adjustedDur + stage_order + (1 + freq + sta | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = F, verbose = F, data = lf_no0)

## model comparisons

# likelihood ratio tests
anova(e3.m1, e3.m2, test="LRT")

# Bayes factor (BF) for all possible models
e3.BFs <- bayesfactor_models(e3.m1,e3.m2,e3.m3,e3.m4,e3.m5,e3.m6,e3.m7,e3.m8, denominator = e3.m0)
e3.BFs <- as.matrix(e3.BFs)
e3.BFs.lf <- melt(e3.BFs); 
e3.BFs.lf$lab <- ifelse(e3.BFs.lf$value > 1000, ">1000",
                        ifelse(e3.BFs.lf$value < 0.001, "<0.001",
                               ifelse(e3.BFs.lf$value > 0.001 & e3.BFs.lf$value < 1,
                                      round(e3.BFs.lf$value,3),
                                      round(e3.BFs.lf$value,1))))
e3_BFplot <- ggplot(e3.BFs.lf,aes(x=Var1,y=Var2,fill=log(value),label=lab)) + 
  geom_tile() + geom_text() +
  scale_fill_gradient2(low = "blue4", mid = "white", high = "red4", midpoint = 0,
                       breaks=log(logLab),labels=logLab) +
  labs(title = "Experiment 3: Model Comparisons",subtitle = "Bayes Factors (BF): bayesfactor_models() function from bayestestR",
       x = "Denominator model", y = "Numerator model", fill = "BF") +
  theme_classic() + 
  guides(fill = guide_colourbar(barwidth = 1.2, barheight = 12, ticks = F)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
e3_BFplot
if (save_png == 1) {
  ggsave(paste0(getwd(),"/figures/figS4.png"),
         plot = e3_BFplot, width = 26, height = 20, dpi = 1800, units = "cm",limitsize = T)
}

# AIC and BIC
e3.fits <- cbind(BIC(e3.m1,e3.m2,e3.m3,e3.m4,e3.m5,e3.m6,e3.m7,e3.m8,e3.m0),
                 AIC(e3.m1,e3.m2,e3.m3,e3.m4,e3.m5,e3.m6,e3.m7,e3.m8,e3.m0)[,2])
colnames(e3.fits) <- c("df","BIC","AIC")
e3.fits
e3.fits$Models <- e3.BFs.lf$Var1[1:nrow(e3.fits)]


#### #### #### #### #### ####
e3.m1.tab <- report_table(e3.m1)

if (write_csv == 1) {
  write.csv(e3.m1.tab,paste0(getwd(),"/stats/e3_fullMod_propContrast_v2.csv"))
  write.csv(e3.fits,paste0(getwd(),"/stats/e3_modComp_propContrast_v2.csv"))
}



######## VISUALIZATION ######## 
# get data ready
# change factor oder, first positive (0.42), then zero (0), and finnaly negative (-0.42)
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
  scale_shape_manual(labels = c("Constant","Adjusted"), values=c(19,24)) + 
  scale_colour_manual(labels = c("Constant","Adjusted"), values=viridis(5)[c(2,4)]) +
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
  ggsave(paste0(getwd(),"/figures/fig4a.png"),
       plot = pF, width = 30*1.61803398875, height = 30, dpi = 1200, units = "cm",limitsize = T)
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
  scale_shape_manual(labels = c("Constant","Adjusted"), values=c(19,24)) + 
  scale_colour_manual(labels = c("Constant","Adjusted"), values=viridis(5)[c(2,4)]) +
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
  ggsave(paste0(getwd(),"/figures/fig4b.png"),
         plot = pD, width = 30*1.61803398875, height = 30, dpi = 1200, units = "cm",limitsize = T)
}


p <- ggarrange(pF,pD,nrow=2)

if (save_png == 1) {
  ggsave(paste0(getwd(),"/figures/fig4.png"),
         plot = p, width = 30*1.61803398875, height = 60, dpi = 900, units = "cm",limitsize = T)
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

# After reviewer # 2 comment.
lf <- rbind(exp01_lf[,rel_col],exp02_lf[,rel_col],exp03_lf[,rel_col])
# lf <- rbind(exp01_lf[,rel_col],exp03_lf[,rel_col])

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
mDeltap <- lmer(rating ~ deltaP +stage+ (1+stage+deltaP|exp/subject),lf); summary(mDeltap)
mDeltap_dur <- lmer(rating ~ deltaP_Dur +stage+ (1+stage+deltaP_Dur|exp/subject),lf); summary(mDeltap_dur)
mDeltap_wDur <- lmer(rating ~ deltaP_wDur +stage+ (1+stage+deltaP_wDur|exp/subject),lf); summary(mDeltap_wDur)
mLograte <- lmer(rating ~ rate +stage+ (1+stage+rate|exp/subject),lf); summary(mLograte)

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
  geom_smooth(aes(group = stage, color = stage, shape = stage),
              method = "lm", size = 1.5, se=F, col = "black") +
  stat_summary(aes(group = stage, color = stage, shape = stage),
               fun.data = "mean_cl_boot", size = 0.5, position = pos) +
  # annotate("text", x = -0.5, y = 10, label = paste0("r(",cor_dPdur_rat$parameter,") = ",round(cor_dP_rat$estimate,2),", p < 0.001"), size = 10) +
  annotate("text", x = 0.7, y = -8, label = paste0("BIC = ",round(mBic[1,2],1)), size = 3) +
  annotate("text", x = 0.7, y = -10, label = paste0("AIC = ",round(mAic[1,2],1)), size = 3) +
  labs(title = "Frequency", x = expression(Delta*p), y = "Rating",
       shape = "Contingencies", color = "Contingencies") +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1,by=0.50), minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  theme_classic() #theme_scdo #+ # line legend
# fig5a

cor_dPdur_rat <- cor.test(lf$deltaP_Dur,lf$rating)
fig5b <- ggplot(lf[,],aes(x=deltaP_Dur,y=rating)) + 
  geom_hline(yintercept = 0, color = "gray90", size = sf*0.5) +
  geom_vline(xintercept = 0, color = "gray90", size = sf*0.5) +
  geom_jitter(height = 0.25, width = 0.015, alpha = 0.3, size = 0.6, col = "grey70") +
  # geom_segment(aes(x=-1,y=-10,xend=1,yend=10), col = "red", linetype = "dashed",lwd=2) +
  geom_smooth(aes(group = stage, color = stage, shape = stage),
              method = "lm", size = 1.5, se=F, col = "black") +
  stat_summary(aes(group = stage, color = stage, shape = stage),
               fun.data = "mean_cl_boot", size = 0.5, position = pos) +
  # annotate("text", x = -0.5, y = 10, label = paste0("r(",cor_dPdur_rat$parameter,") = ",round(cor_dPdur_rat$estimate,2),", p < 0.001"), size = 10) +
  annotate("text", x = 0.7, y = -8, label = paste0("BIC = ",round(mBic[2,2],1)), size = 3) +
  annotate("text", x = 0.7, y = -10, label = paste0("AIC = ",round(mAic[2,2],1)), size = 3) +
  labs(title = "Duration", x = expression(Delta*p["duration"]), y = "Rating",
       shape = "Contingencies", color = "Contingencies") +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1,by=0.50), minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  theme_classic() + theme(axis.title.y = element_blank()) #theme_scdo #+ # line legend
# fig5b

cor_dPwDur_rat <- cor.test(lf$deltaP_wDur,lf$rating)
fig5c <- ggplot(lf[,],aes(x=deltaP_wDur,y=rating)) +
  geom_hline(yintercept = 0, color = "gray90", size = sf*0.5) +
  geom_vline(xintercept = 0, color = "gray90", size = sf*0.5) +
  geom_jitter(height = 0.25, width = 0.015, alpha = 0.3, size = 0.6, col = "grey70") +
  # geom_segment(aes(x=-1,y=-10,xend=1,yend=10), col = "red", linetype = "dashed",lwd=2) +
  geom_smooth(aes(group = stage, color = stage, shape = stage),
              method = "lm", size = 1.5, se=F, col = "black") +
  stat_summary(aes(group = stage, color = stage, shape = stage),
               fun.data = "mean_cl_boot", size = 0.5, position = pos) +
  # annotate("text", x = -0.5, y = 10, label = paste0("r(",cor_dPwDur_rat$parameter,") = ",round(cor_dPwDur_rat$estimate,2),", p < 0.001"), size = 10) +
  annotate("text", x = 0.7, y = -8, label = paste0("BIC = ",round(mBic[3,2],1)), size = 3) +
  annotate("text", x = 0.7, y = -10, label = paste0("AIC = ",round(mAic[3,2],1)), size = 3) +
  labs(title = "Frequency * Duration", x = expression(Delta*p["freq*dur"]), y = "Rating",
       shape = "Contingencies", color = "Contingencies") +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1,by=0.50), minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  theme_classic() #theme_scdo #+ # line legend
# fig5c

cor_rate_rat <- cor.test(lf$rate,lf$rating)
fig5d <- ggplot(lf[,],aes(x=rate,y=rating)) + #, group = olifeHL, col = olifeHL, shape = olifeHL)) + 
  geom_hline(yintercept = 0, color = "gray90", size = sf*0.5) +
  geom_vline(xintercept = 0, color = "gray90", size = sf*0.5) +
  geom_jitter(height = 0.25, width = 0.03, alpha = 0.3, size = 0.6, col = "grey70") +
  #geom_segment(aes(x=-1,y=-10,xend=1,yend=10), col = "red", linetype = "dashed",lwd=2) +
  geom_smooth(aes(group = stage, color = stage, shape = stage),
              method = "lm", size = 1.5, se=F, col = "black") +
  stat_summary(aes(group = stage, color = stage, shape = stage),
               fun.data = "mean_cl_boot", size = 0.5, position = pos) +
  # annotate("text", x = -1.2, y = 10, label = paste0("r(",cor_rate_rat$parameter,") = ",round(cor_rate_rat$estimate,2),", p < 0.001"), size = 10) +
  annotate("text", x = 1.3, y = -8, label = paste0("BIC = ",round(mBic[4,2],1)), size = 3) +
  annotate("text", x = 1.3, y = -10, label = paste0("AIC = ",round(mAic[4,2],1)), size = 3) +
  labs(title = "Rates (Frequency over Duration)", x = expression(ln(lambda[S1] / lambda[TM])), y = "Rating",
       shape = "Contingencies", color = "Contingencies") +
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  theme_classic() + theme(axis.title.y = element_blank())#theme_scdo
# fig5d

fig5 <- annotate_figure(ggarrange(fig5a,fig5b,fig5c,fig5d, nrow=2,ncol=2, labels = c(LETTERS[1:4]), 
                                  common.legend = T, legend = "bottom"),
                        top =  text_grob(expression(paste(N["participants"]," = 125; ",N["ratings"]," = 1980"))))
fig5



if (save_png == 1) {
  ggsave(paste0(getwd(),"/figures/fig5_v4.png"), 
         plot = fig5, width = 17, height = 17,dpi = 1800, units = "cm",limitsize = T)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # Signal Detection Theory # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Signal Detection Theory
#                   hit  FA  Ms  CR
bin <- 10
SDTtab <- cbind(c(rep(44,bin),rep(24,bin),rep(4,bin)),
                c(rep(4,bin),rep(24,bin),rep(44,bin)),
                rep(24,bin*3),
                rep(seq(0,216,216/(bin-1)),3))
# SDTtab <- cbind(rep(24,bin*3),
#                 rep(seq(0,216,216/(bin-1)),3),
#                 rep(24,bin*3),
#                 rep(24,bin*3))

sigmParam <- matrix(NA,ncol=2,nrow=nrow(SDTtab))

for (s in 1:nrow(SDTtab)) {
  h <- SDTtab[s,1]/(SDTtab[s,1]+SDTtab[s,3])
  f <- SDTtab[s,2]/(SDTtab[s,2]+SDTtab[s,4])
  # http://wise.cgu.edu/wise-tutorials/tutorial-signal-detection-theory/signal-detection-d-defined-2/
  if (f == 0) f <- 1/sum(SDTtab[s,]) else if (f == 1) f <- (sum(SDTtab[s,])-1)/sum(SDTtab[s,])
  if (h == 0) h <- 1/sum(SDTtab[s,]) else if (h == 1) h <- (sum(SDTtab[s,])-1)/sum(SDTtab[s,])
  sigmParam[s,1] <- qnorm(h) - qnorm(f)
  sigmParam[s,2]  <- (qnorm(h) + qnorm(f)) / 2
}

m <- data.frame(rep(c("Positive","Zero","Negative"),each=bin),SDTtab,sigmParam)
names(m) <- c("cont","A","B","C","D","d_sdt","c_sdt")
m$cont <- factor(m$cont, levels = c("Negative","Zero","Positive"))

p_d <- ggplot(m, aes(x=D,y=d_sdt)) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_line() +
  labs(title="Signal Detection Theory and co-absent information",
       y="d' (discriminability)",x="D frequency") +
  facet_grid(. ~ cont) + theme_classic()

p_c <- ggplot(m, aes(x=D,y=c_sdt)) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_line() +
  labs(title="Signal Detection Theory and co-absent information",
       y="c (response criterion)",x="D frequency") +
  facet_grid(. ~ cont) + theme_classic()

ggarrange(p_d,p_c,nrow=2)



# SDT with dichotomous ratings
subjects <- levels(lf$subject)

lf_ <- lf[lf$exp == 3,]
lf_ <- lf_[!is.na(lf_$rating),]

lf_$ratDich <- ifelse(lf_$rating >= mean(lf_$rating), 1, 0)
lf_$contDich <- ifelse(lf_$deltaP >= 0, 1, 0)

sigmParam <- data.frame(matrix(NA,nrow=length(unique(lf_$condition)),ncol=9))
SDTtab <- data.frame(matrix(NA,nrow=length(unique(lf_$condition)),ncol=4))
names(sigmParam) <- c("condition","cont","fD","dD","dP","h","f","d_sdt","c_sdt")
sigmParam$cont <- factor(sigmParam$cont, levels=c("Negative","Zero","Positive"))

for (s in 1:length(unique(lf_$condition))) {
  z <- lf_[lf_$condition == unique(lf_$condition)[s],]
  #                                                                  hit  FA   Ms  CR
  SDTtab[s,] <- colSums(paste0(z$ratDich,z$contDich)==t(matrix(rep(c("11","10","01","00"),nrow(z)),ncol=nrow(z))))
  h <- SDTtab[s,1]/(SDTtab[s,1]+SDTtab[s,3])
  f <- SDTtab[s,2]/(SDTtab[s,2]+SDTtab[s,4])
  # http://wise.cgu.edu/wise-tutorials/tutorial-signal-detection-theory/signal-detection-d-defined-2/
  if (h == 0 | is.nan(h)) h <- 1/nrow(z) else if (h == 1) h <- (nrow(z)-1)/nrow(z)
  if (f == 0 | is.nan(f)) f <- 1/nrow(z) else if (f == 1) f <- (nrow(z)-1)/nrow(z)
  
  sigmParam[s,1] <- z$condition[1]
  sigmParam[s,2] <- z$stage[1]
  sigmParam[s,3] <- z$fD[1]
  sigmParam[s,4] <- z$dD[1]
  sigmParam[s,5] <- z$deltaP[1]
  
  sigmParam[s,6] <- h
  sigmParam[s,7] <- f
  
  sigmParam[s,8] <- qnorm(h) - qnorm(f)
  sigmParam[s,9]  <- -1*(qnorm(h) + qnorm(f)) / 2
}
sigmParam <- cbind(sigmParam,SDTtab); colnames(sigmParam)[10:13] <- c("hit","FA","Ms","CR")
sigmParam <- sigmParam[order(sigmParam$cont,sigmParam$fD),]



pF_d <- ggplot(sigmParam,aes(x=fD,y=d_sdt)) + 
  geom_hline(yintercept = 0, alpha = 0.5) +
  stat_summary(geom = "line") +
  labs(title="Discriminability",y="d'",x="D frequency") +
  facet_grid(. ~ cont) + 
  theme_classic()
pF_c <- ggplot(sigmParam,aes(x=fD,y=c_sdt)) + 
  geom_hline(yintercept = 0, alpha = 0.5) +
  stat_summary(geom = "line") +
  labs(title="Reponse Criterion",y="c",x="D frequency") +
  facet_grid(. ~ cont) + 
  theme_classic()
pD_d <- ggplot(sigmParam,aes(x=dD,y=d_sdt)) + 
  geom_hline(yintercept = 0, alpha = 0.5) +
  stat_summary(geom = "line") +
  labs(title="Discriminability",y="d'",x="D duration") +
  facet_grid(. ~ cont) + 
  theme_classic()
pD_c <- ggplot(sigmParam,aes(x=dD,y=c_sdt)) + 
  geom_hline(yintercept = 0, alpha = 0.5) +
  stat_summary(geom = "line") +
  labs(title="Reponse Criterion",y="c",x="D duration") +
  facet_grid(. ~ cont) + 
  theme_classic()

p <- annotate_figure(ggarrange(pF_d,pD_d,pF_c,pD_c,nrow=2,ncol=2, labels = c("A","B","C","D")),
                top = text_grob("SDT and co-absent information", 
                                color = "black", face = "bold", size = 14),
                )
p

write.csv(sigmParam,paste0(getwd(),"/SDT_table.csv"),na="",row.names=F)
ggsave(paste0(getwd(),"/figures/SDT_cellD_v4.png"),plot=p,
       width = 18, height = 12, dpi = 1800, units = "cm",limitsize = T)
