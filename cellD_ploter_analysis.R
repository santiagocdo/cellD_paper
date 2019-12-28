rm(list = ls()) # errase previous Global Environment, import manually the file
setwd("C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/raw_data") # set workspace

# print csv files
write_csv <- 0


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # Experiment 12 # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

exp12_lf <- read.csv("exp12_lf.csv")
exp12_wf <- read.csv("exp12_wf.csv")

# general characteristics
nSubj <- nrow(exp12_wf); nSubj
sex <- table(exp12_wf$sex); sex
meanAge <- mean(exp12_wf$age); meanAge
sdAge <- sd(exp12_wf$age); sdAge
rangeAge <- range(exp12_wf$age); rangeAge



######## STATISTICAL ANALYSIS ######## 
# reorder factor order
exp12_lf$frequency <- factor(exp12_lf$frequency, levels = c("36","0","108","324"))
exp12_lf$duration <- factor(exp12_lf$duration, levels = c("450","0", "1350", "4050"))
exp12_lf$delay <- factor(exp12_lf$delay, levels = c("0", "3"))
exp12_lf$stage_order <- ifelse(exp12_lf$cond_order < 5, 1, ifelse(exp12_lf$cond_order > 8, 3, 2))

if(!require(lmerTest)) {install.packages("lmerTest")}
library(lmerTest)

plotANDstat_all <- 1
### exclude data for visualization
if (plotANDstat_all == 1) {
  # plot and analysis with 0.
  lf_no0 <- exp12_lf[!is.na(exp12_lf$rating),]
  
  # graphs data
  limits_vec_fre <- c(-4,330)
  break_vec_fre <- c(0,36,108,324)
  limits_vec_dur <- c(-50,4100)
  break_vec_dur <- c(0,450,1350,4050)
} else {
  # plot and analysis without 0.
  lf_no0 <- exp12_lf[exp12_lf$frequency != 0 & !is.na(exp12_lf$rating),]
  
  # graphs data
  limits_vec_fre <- c(0,330)
  break_vec_fre <- c(36,108,324)
  limits_vec_dur <- c(0,4100)
  break_vec_dur <- c(450,1350,4050)
}
###



# mFull <- lmer(rating ~ 1 + frequency * duration * delay + (1 | subject) + as.factor(cond_order),
#               control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
#               REML = FALSE, data = lf_no0); anova(mFull)
mFull <- lmer(rating ~ 1 + frequency + duration + delay + frequency:delay + duration:delay + 
                (1 | subject) + as.factor(cond_order),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
              REML = FALSE, data = lf_no0); anova(mFull)

m_fre_del <- lmer(rating ~ 1 + frequency * delay + (1 | subject) + as.factor(cond_order),
                  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
                  REML = FALSE, data = lf_no0); anova(m_fre_del)

m_dur_del <- lmer(rating ~ 1 + duration * delay + (1 | subject) + as.factor(cond_order),
                 control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
                 REML = FALSE, data = lf_no0); anova(m_dur_del)

m_fre_dur <- lmer(rating ~ 1 + frequency + duration + (1 | subject) + as.factor(cond_order),
                control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
                REML = FALSE, data = lf_no0); anova(m_fre_dur)

m_fre <- lmer(rating ~ 1 + frequency + (1 | subject) + as.factor(cond_order),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              REML = FALSE, data = lf_no0); anova(m_fre)

m_dur <- lmer(rating ~ 1 + duration + (1 | subject) + as.factor(cond_order),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              REML = FALSE, data = lf_no0); anova(m_dur)

m_del <- lmer(rating ~ 1 + delay + (1 | subject) + as.factor(cond_order),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), 
              REML = FALSE, data = lf_no0); anova(m_del)

# the full model is not significanly better against frequency+delay (stats), 
# duration+delat (stats), nor frequency+duration (stats).

## AGAINST THE FULL MODEL
anova(mFull, m_fre_del, test="LRT"); exp((BIC(mFull) - BIC(m_fre_del))/2); AIC(mFull) - AIC(m_fre_del)
anova(mFull, m_dur_del, test="LRT"); exp((BIC(mFull) - BIC(m_dur_del))/2); AIC(mFull) - AIC(m_dur_del)
anova(mFull, m_fre_dur, test="LRT"); exp((BIC(mFull) - BIC(m_fre_dur))/2); AIC(mFull) - AIC(m_fre_dur)

#anova(mFull, m_fre, test="LRT"); exp((BIC(mFull) - BIC(m_fre))/2); AIC(mFull) - AIC(m_fre)
#anova(mFull, m_dur, test="LRT"); exp((BIC(mFull) - BIC(m_dur))/2); AIC(mFull) - AIC(m_dur)
#anova(mFull, m_del, test="LRT"); exp((BIC(mFull) - BIC(m_del))/2); AIC(mFull) - AIC(m_del)

# AGAINST THE FREQUENCY & DELAY MODEL
anova(m_fre_del, m_fre, test="LRT"); exp((BIC(m_fre_del) - BIC(m_fre))/2); AIC(m_fre_del) - AIC(m_fre)
anova(m_fre_del, m_dur, test="LRT"); exp((BIC(m_fre_del) - BIC(m_dur))/2); AIC(m_fre_del) - AIC(m_dur)
anova(m_fre_del, m_del, test="LRT"); exp((BIC(m_fre_del) - BIC(m_del))/2); AIC(m_fre_del) - AIC(m_del)

# AGAINST THE DURATION & DELAY MODEL
anova(m_dur_del, m_fre, test="LRT"); exp((BIC(m_dur_del) - BIC(m_fre))/2); AIC(m_dur_del) - AIC(m_fre)
anova(m_dur_del, m_dur, test="LRT"); exp((BIC(m_dur_del) - BIC(m_dur))/2); AIC(m_dur_del) - AIC(m_dur)
anova(m_dur_del, m_del, test="LRT"); exp((BIC(m_dur_del) - BIC(m_del))/2); AIC(m_dur_del) - AIC(m_del)

# AGAINST THE FREQUENCY & DURATION MODEL
anova(m_fre_dur, m_fre, test="LRT"); exp((BIC(m_fre_dur) - BIC(m_fre))/2); AIC(m_fre_dur) - AIC(m_fre)
anova(m_fre_dur, m_dur, test="LRT"); exp((BIC(m_fre_dur) - BIC(m_dur))/2); AIC(m_fre_dur) - AIC(m_dur)
anova(m_fre_dur, m_del, test="LRT"); exp((BIC(m_fre_dur) - BIC(m_del))/2); AIC(m_fre_dur) - AIC(m_del)

BIC(mFull, m_dur_del, m_fre_del, m_fre_dur, m_fre, m_dur, m_del)

AIC(mFull, m_dur_del, m_fre_del, m_fre_dur, m_fre, m_dur, m_del)

if(!require(psycho)) {install.packages("psycho")}; library(psycho) # analyze
if(!require(reshape2)) {install.packages("reshape2")}; library(reshape2) # %>%
 
to_print <- summary(analyze(anova(mFull), CI = 95)) %>% mutate(p = psycho::format_p(p))
if (write_csv == 1) {
  write.csv(to_print,":/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/stats/exp12_LMM.csv"
            , row.names = F)
}



######## VISUALIZATION ######## 
if(!require(ggplot2)) {install.packages("ggplot2")}
library(ggplot2)

# get data ready
# change factor oder, first positive (0.42), then zero (0), and finnaly negative (-0.42)
lf_no0$frequency <- as.integer(as.character(factor(lf_no0$frequency, levels = c("0", "36", "108", "324"))))
lf_no0$duration <- as.integer(as.character(factor(lf_no0$duration, levels = c("0", "450", "1350", "4050"))))
lf_no0$Recall <- lf_no0$delay
levels(lf_no0$Recall) <- c("immediate", "delay")



# plot size in pixels 1500 x 1000. PDF 15 x 10
plot_title_size <- 48#24
strip_text_size <- 44#22
axis_title_size <- 40#20
axis_text_size <- 36#18
legend_title_size <- 40#20
legend_text_size <- 36#18
pos <- position_dodge(10)

pF <- ggplot(lf_no0, aes(x = frequency, y = rating, col = Recall, shape = Recall)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "gray50", size = 1.2) +
  #geom_line(aes(group = subject, col = adjustedDur)) +
  #geom_line(aes(y=fit), size = 0.8, colour = "gray90") +
  #geom_jitter(width = 2, height = 0.2, alpha = 0.5 ,color = "gray70", size = 1) +
  geom_point(aes(y = jitter(rating)),alpha = 0.2, size = 1.2, position = pos) +
  geom_smooth(method = "lm", aes(group = Recall, col = Recall), size = 2, se = F) +
  stat_summary(fun.data = "mean_cl_boot", size = 2, position = pos) +
  labs(title = "Number effect", subtitle = paste("n =",nSubj)) + #, shape = "Condition") +
  xlab("Cell D Number") + ylab("Rating") +
  #scale_shape_manual(values = c(15, 17, 19), breaks = c("few","baseline","many")) +  
  scale_x_continuous(limits = limits_vec_fre, breaks = break_vec_fre, minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  scale_colour_manual(values=c("#440154FF","#218F8DFF")) +
  #facet_grid(. ~ stage) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = strip_text_size),
        plot.title = element_text(size = plot_title_size, hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = plot_title_size/2, hjust=0, face = "italic", color = "black"),
        plot.margin = unit(c(0.2,0.4,0.2,0.2), "cm"),
        axis.title.y = element_text(size = axis_title_size, face = "bold"),
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
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),
        panel.border = element_rect(color = "black"),
        panel.spacing = unit(1, "lines")
  )
pF



# plot size in pixels 1500 x 1000. PDF 15 x 10
plot_title_size <- 48#24
strip_text_size <- 44#22
axis_title_size <- 40#20
axis_text_size <- 36#18
legend_title_size <- 40#20
legend_text_size <- 36#18
pos <- position_dodge(120)

pD <- ggplot(lf_no0[,], aes(x = duration, y = rating, col = Recall, shape = Recall)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "gray50", size = 1.2) +
  #geom_line(aes(group = subject), alpha = 0.2, position = pos) +
  #geom_line(aes(y=fit), size = 0.8, colour = "gray90") +
  #geom_jitter(width = 20, height = 0.2, alpha = 0.5 ,color = "gray70", size = 1) +
  geom_point(aes(y = jitter(rating)),alpha = 0.2, size = 1.2, position = pos) +
  geom_smooth(method = "lm", aes(group = Recall, col = Recall), size = 2, se = F) +
  stat_summary(fun.data = "mean_cl_boot", size = 2, position = pos) +
  labs(title = "Duration effect", subtitle = paste("n =",nSubj)) +
  xlab("Cell D Duration (ms)") + ylab("Rating") +
  #scale_shape_manual(values = c(15, 17, 19), breaks = c("few","baseline","many")) +  
  scale_x_continuous(limits = limits_vec_dur, breaks = break_vec_dur, minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  scale_colour_manual(values=c("#440154FF","#218F8DFF")) +
  #facet_wrap(. ~ stage) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = strip_text_size),
        plot.title = element_text(size = plot_title_size, hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = plot_title_size/2, hjust=0, face = "italic", color = "black"),
        plot.margin = unit(c(0.2,0.4,0.2,0.4), "cm"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = axis_title_size, face = "bold"),
        axis.text.x = element_text(size = axis_text_size*0.9, colour="black"),
        axis.line = element_line(colour = 'black', size = 1.6),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length = unit(10, "pt"),
        legend.title = element_text(size = legend_title_size), 
        legend.text = element_text(size = legend_text_size),
        legend.position = "bottom",
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor =element_blank(),  
        panel.background = element_blank(),
        panel.border = element_rect(color = "black"),
        panel.spacing = unit(1, "lines")
  )
pD

# combine plots
# 1400 x 1100 pixels
ggpubr::ggarrange(pF,pD, plotlist = NULL, ncol = 2, nrow = NULL,
                  labels = NULL, label.x = 0, label.y = 1, hjust = -0.5,
                  vjust = 1.5, font.label = list(size = 52, color = "black", face = "bold", family = NULL), 
                  align = c("none", "h", "v", "hv"), widths = c(1,0.87), heights = 1, legend = "bottom", common.legend = TRUE)






# plot size in pixels 1500 x 1000. PDF 15 x 10
plot_title_size <- 48#24
strip_text_size <- 44#22
axis_title_size <- 40#20
axis_text_size <- 36#18
legend_title_size <- 40#20
legend_text_size <- 36#18
pos <- position_dodge(10)
pF <- ggplot(lf_no0[lf_no0$duration != 1350 & lf_no0$duration != 4050,], aes(x = frequency, y = rating, col = Recall, shape = Recall)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "gray50", size = 1.2) +
  geom_line(aes(group = interaction(subject,Recall)), alpha = 0.1, size = 0.8, position = pos) +
  #geom_line(aes(y=fit), size = 0.8, colour = "gray90") +
  #geom_jitter(width = 2, height = 0.2, alpha = 0.5 ,color = "gray70", size = 1) +
  geom_point(aes(y = jitter(rating)), alpha = 0.2, size = 1.2, position = pos) +
  geom_smooth(method = "lm", aes(group = Recall, col = Recall), size = 2, se = F) +
  stat_summary(fun.data = "mean_cl_boot", size = 2, position = pos) +
  labs(title = "Number effect", subtitle = paste("n =",nSubj)) + #, shape = "Condition") +
  xlab("Cell D Number") + ylab("Rating") +
  #scale_shape_manual(values = c(15, 17, 19), breaks = c("few","baseline","many")) +  
  scale_x_continuous(limits = limits_vec_fre, breaks = break_vec_fre, minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  scale_colour_manual(values=c("#440154FF","#218F8DFF")) +
  #facet_grid(. ~ stage) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = strip_text_size),
        plot.title = element_text(size = plot_title_size, hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = plot_title_size/2, hjust=0, face = "italic", color = "black"),
        plot.margin = unit(c(0.2,0.4,0.2,0.2), "cm"),
        axis.title.y = element_text(size = axis_title_size, face = "bold"),
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
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),
        panel.border = element_rect(color = "black"),
        panel.spacing = unit(1, "lines")
  )
# plot size in pixels 1500 x 1000. PDF 15 x 10
plot_title_size <- 48#24
strip_text_size <- 44#22
axis_title_size <- 40#20
axis_text_size <- 36#18
legend_title_size <- 40#20
legend_text_size <- 36#18
pos <- position_dodge(120)
pD <- ggplot(lf_no0[lf_no0$frequency != 108 & lf_no0$frequency != 324,], aes(x = duration, y = rating, col = Recall, shape = Recall)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "gray50", size = 1.2) +
  geom_line(aes(group = interaction(subject,Recall)), alpha = 0.1, size = 0.8, position = pos) +
  #geom_line(aes(y=fit), size = 0.8, colour = "gray90") +
  #geom_jitter(width = 2, height = 0.2, alpha = 0.5 ,color = "gray70", size = 1) +
  geom_point(aes(y = jitter(rating)), alpha = 0.2, size = 1.2, position = pos) +
  geom_smooth(method = "lm", aes(group = Recall, col = Recall), size = 2, se = F) +
  stat_summary(fun.data = "mean_cl_boot", size = 2, position = pos) +
  labs(title = "Duration effect", subtitle = paste("n =",nSubj)) +
  xlab("Cell D Duration (ms)") + ylab("Rating") +
  #scale_shape_manual(values = c(15, 17, 19), breaks = c("few","baseline","many")) +  
  scale_x_continuous(limits = limits_vec_dur, breaks = break_vec_dur, minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  scale_colour_manual(values=c("#440154FF","#218F8DFF")) +
  #facet_wrap(. ~ stage) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = strip_text_size),
        plot.title = element_text(size = plot_title_size, hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = plot_title_size/2, hjust=0, face = "italic", color = "black"),
        plot.margin = unit(c(0.2,0.4,0.2,0.4), "cm"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = axis_title_size, face = "bold"),
        axis.text.x = element_text(size = axis_text_size*0.9, colour="black"),
        axis.line = element_line(colour = 'black', size = 1.6),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length = unit(10, "pt"),
        legend.title = element_text(size = legend_title_size), 
        legend.text = element_text(size = legend_text_size),
        legend.position = "bottom",
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor =element_blank(),  
        panel.background = element_blank(),
        panel.border = element_rect(color = "black"),
        panel.spacing = unit(1, "lines")
  )
# combine plots
# 1400 x 1100 pixels
ggpubr::ggarrange(pF,pD, plotlist = NULL, ncol = 2, nrow = NULL,
                  labels = NULL, label.x = 0, label.y = 1, hjust = -0.5,
                  vjust = 1.5, font.label = list(size = 52, color = "black", face = "bold", family = NULL), 
                  align = c("none", "h", "v", "hv"), widths = c(1,0.87), heights = 1, legend = "bottom", common.legend = TRUE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # Experiment 16 # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

exp16_lf <- read.csv("exp16_lf.csv")
exp16_wf <- read.csv("exp16_wf.csv")

# general characteristics
nSubj <- nrow(exp16_wf); nSubj
sex <- table(exp16_wf$sex); sex
meanAge <- mean(exp16_wf$age); meanAge
sdAge <- sd(exp16_wf$age); sdAge
rangeAge <- range(exp16_wf$age); rangeAge


######## STATISTICAL ANALYSIS ######## 
# reorder factor order
exp16_lf$frequency <- factor(exp16_lf$frequency, levels = c("24","0","72","216"))
exp16_lf$stage <- factor(exp16_lf$stage, levels = c("Z", "N", "P"))
if(!require(lmerTest)) {install.packages("lmerTest")}
library(lmerTest)

plotANDstat_all <- 1
### exclude data for visualization
if (plotANDstat_all == 1) {
  # plot and analysis with 0.
  lf_no0 <- exp16_lf[!is.na(exp16_lf$rating),]
  
  # graphs data
  limits_vec <- c(-2, 220)
  break_vec <- c(0,24,72,216)
} else {
  # plot and analysis without 0.
  lf_no0 <- exp16_lf[exp16_lf$frequency != 0 & !is.na(exp16_lf$rating),]
  
  limits_vec <- c(0, 220)
  break_vec <- c(24,72,216)
}
###



mF <- lmer(rating ~ 1 + (frequency * stage) + (1 + stage | subject) + as.factor(stage_order), 
           control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5), calc.derivs = FALSE), REML = FALSE,
           data = lf_no0)
anova(mF)

# classic ANOVAs https://personality-project.org/r/r.guide/r.anova.html
# aClas <- aov(rating ~ (frequency * stage) + as.factor(stage_order) + Error(subject/(frequency * stage)), data = lf_no0)
# summary(aClas)

if(!require(psycho)) {install.packages("psycho")}
library(psycho) # analyze
if(!require(reshape2)) {install.packages("reshape2")}
library(reshape2) # %>% 
to_print <- summary(analyze(anova(mF), CI = 95)) %>% mutate(p = psycho::format_p(p))

if (write_csv == 1) {
  write.csv(to_print,"C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/stats/exp16_LMM_frequency.csv"
            , row.names = F)
}

# explore interaction frequency x stage. To see if there is an effect of inidividual contingencies
# lf_cont_res[lf_cont_res$frequency != 216, ] # only for 0, 24, and 72 frequencies
fit_P <- lmer(rating ~ 1 + frequency + as.factor(stage_order) + (1 | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5), calc.derivs = FALSE), REML = FALSE,
              data = lf_no0[lf_no0$stage == "P",])
anova(fit_P)
fit_Z <- lmer(rating ~ 1 + frequency + as.factor(stage_order) + (1 | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5), calc.derivs = FALSE), REML = FALSE,
              data = lf_no0[lf_no0$stage == "Z",])
anova(fit_Z)
fit_N <- lmer(rating ~ 1 + frequency + as.factor(stage_order) + (1 | subject), 
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5), calc.derivs = FALSE), REML = FALSE,
              data = lf_no0[lf_no0$stage == "N",])
anova(fit_N)

to_print <- cbind(c("positive","positive","zero","zero","negative","negative"),
                  rbind(summary(analyze(anova(fit_P))) %>% mutate(p = psycho::format_p(p)),
                        summary(analyze(anova(fit_Z))) %>% mutate(p = psycho::format_p(p)),
                        summary(analyze(anova(fit_N))) %>% mutate(p = psycho::format_p(p))))
colnames(to_print) <- c("contingency",colnames(to_print)[2:8])
  
if (write_csv == 1) {
  write.csv(to_print,"C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/stats/exp16_LMM_frequency_posthoc.csv"
            , row.names = F)
}



######## VISUALIZATION ######## 
if(!require(ggplot2)) {install.packages("ggplot2")}
library(ggplot2)

# change factor oder, first positive (0.42), then zero (0), and finnaly negative (-0.42)
lf_no0$stage <- factor(lf_no0$stage, levels = c("N", "Z", "P"))
levels(lf_no0$stage) <- c('Negative', 'Zero', 'Positive')
lf_no0$frequency <- as.integer(as.character(factor(lf_no0$frequency, levels = c("0", "24", "72", "216"))))



# plot size in pixels 1500 x 1000. PDF 15 x 10
plot_title_size <- 48#24
strip_text_size <- 44#22
axis_title_size <- 40#20
axis_text_size <- 36#18
legend_title_size <- 40#20
legend_text_size <- 36#18
pos <- position_dodge(5)

pF <- ggplot(lf_no0, aes(x = frequency, y = rating)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "gray50", size = 1.2) +
  geom_line(aes(group = subject), color = "gray90", size = 0.6)  +
  geom_jitter(width = 2, height = 0.2, alpha = 0.5 ,color = "gray70", size = 1.2) +
  #geom_point(aes(y = jitter(rating)),alpha = 0.2, size = 1.2, position = pos) +
  geom_smooth(method = "lm",  aes(group = stage), colour = "black", size = 2, se = F) +
  stat_summary(fun.data = "mean_cl_boot", size = 2, position = pos) +
  labs(title = "Number effect by Contingency", subtitle = paste("n =",nSubj)) + #, shape = "Condition") +
  xlab("Cell D Number") + ylab("Rating") +
  #scale_shape_manual(values = c(15, 17, 19), breaks = c("few","baseline","many")) +  
  scale_x_continuous(limits = limits_vec, breaks = break_vec, minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  #scale_colour_manual(values=c("#440154FF","#218F8DFF")) +
  facet_grid(. ~ stage) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = strip_text_size),
        plot.title = element_text(size = plot_title_size, hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = plot_title_size/2, hjust=0, face = "italic", color = "black"),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        axis.title.y = element_text(size = axis_title_size, face = "bold"),
        axis.text.y = element_text(size = axis_text_size, colour="black"),
        axis.title.x = element_text(size = axis_title_size, face = "bold"),
        axis.text.x = element_text(size = axis_text_size*0.9, colour="black"),
        axis.line = element_line(colour = 'black', size = 1.6),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length = unit(10, "pt"),
        legend.title = element_text(size = legend_title_size), 
        legend.text = element_text(size = legend_text_size),
        legend.position = "bottom",
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),
        panel.border = element_rect(color = "black"),
        panel.spacing = unit(1, "lines")
  )
pF



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # Experiment 18 # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
setwd("C:/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/raw_data") # set workspace

exp18_lf <- read.csv("exp18_lf.csv")
exp18_wf <- read.csv("exp18_wf.csv")

# general characteristics
nSubj <- nrow(exp18_wf); nSubj
sex <- table(exp18_wf$sex); sex
meanAge <- mean(exp18_wf$age); meanAge
sdAge <- sd(exp18_wf$age); sdAge
rangeAge <- range(exp18_wf$age); rangeAge


######## STATISTICAL ANALYSIS ######## 
# reorder factor order
exp18_lf$frequency <- factor(exp18_lf$frequency, levels = c("24","0","8","72"))
exp18_lf$duration <- factor(exp18_lf$duration, levels = c("450","0","150","1350"))
exp18_lf$stage <- factor(exp18_lf$stage, levels = c("Z", "N", "P"))
if(!require(lmerTest)) {install.packages("lmerTest")}
library(lmerTest)

plotANDstat_all <- 1
### exclude data for visualization
if (plotANDstat_all == 1) {
  # plot and analysis with 0.
  a <- exp18_lf[!is.na(exp18_lf$rating) & exp18_lf$frequency == 0,]
  a[, "adjustedDur"] <- "Yes"
  lf_no0 <- rbind(exp18_lf[!is.na(exp18_lf$rating),],a)
  
  b <- exp18_lf[!is.na(exp18_lf$rating) & exp18_lf$duration == 0 ,]
  b[, "adjustedNum"] <- "Yes"
  lf_no02 <- rbind(exp18_lf[!is.na(exp18_lf$rating),],b)
  
  # graphs data
  limits_vec <- c(-2, 80)
  break_vec <- c(0,8,24,72)
  limits_vec2 <- c(-50, 1400)
  break_vec2 <- c(0,150,450,1350)
} else {
  # plot and analysis without 0.
  lf_no0 <- exp18_lf[exp18_lf$frequency != 0 & !is.na(exp18_lf$rating),]
  lf_no02 <- exp18_lf[exp18_lf$duration != 0 & !is.na(exp18_lf$rating),]
  
  limits_vec <- c(0, 80)
  break_vec <- c(8,24,72)
  limits_vec2 <- c(100, 1400)
  break_vec2 <- c(150,450,1350)
}
###



mF <- lmer(rating ~ 1 + frequency * stage * adjustedDur + (1 + stage | subject) + as.factor(stage_order), 
           control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), REML = FALSE,
           data = lf_no0)
mF.nAd <- lmer(rating ~ 1 + frequency * stage + (1 + stage | subject) + as.factor(stage_order), 
               control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), REML = FALSE,
               data = lf_no0)

anova(mF, mF.nAd, test="LRT"); exp((BIC(mF) - BIC(mF.nAd))/2); AIC(mF) - AIC(mF.nAd)

lf_no0$fit <- predict(mF.nAd)
BF_BIC <- exp((BIC(mF) - BIC(mF.nAd))/2); AIC(mF) - AIC(mF.nAd)
#BF_BIC


mD <- lmer(rating ~ 1 + duration * stage * adjustedNum + (1 + stage | subject) + as.factor(stage_order), 
           control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), REML = FALSE,
           #control = lmerControl(calc.derivs = FALSE), REML = FALSE,
           data = lf_no02)
mD.nAd <- lmer(rating ~ 1 + duration * stage + (1 + stage | subject) + as.factor(stage_order), 
               control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)), REML = FALSE,
               #control = lmerControl(calc.derivs = FALSE), REML = FALSE,
               data = lf_no02)
anova(mD, mD.nAd, test="LRT"); exp((BIC(mD) - BIC(mD.nAd))/2); AIC(mD) - AIC(mD.nAd)

lf_no02$fit <- predict(mD.nAd)
BF_BIC <- exp((BIC(mD) - BIC(mD.nAd))/2); AIC(mD) - AIC(mD.nAd)
#BF_BIC



if (!require(psycho)) {install.packages("psycho")}
library(psycho) # analyze()
if (!require(dplyr)) {install.packages("dplyr")}
library(dplyr) # %>%
res_adj <- cbind("Adjusted",summary(analyze(anova(mF), CI = 95)) %>% mutate(p = psycho::format_p(p)))
res_noadj <- cbind("No adjusted",summary(analyze(anova(mF.nAd), CI = 95)) %>% mutate(p = psycho::format_p(p)))
colnames(res_adj) <- c("Model",colnames(res_adj)[2:8])
colnames(res_noadj) <- c("Model",colnames(res_noadj)[2:8])
csv_print <- rbind(res_adj,res_noadj)

if (write_csv == 1) {
  write.csv(to_print,":/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/stats/exp18_LMM_frequency.csv"
            , row.names = F)
}


res_adj2 <- cbind("Adjusted",summary(analyze(anova(mD), CI = 95)) %>% mutate(p = psycho::format_p(p)))
res_noadj2 <- cbind("No adjusted",summary(analyze(anova(mD.nAd), CI = 95)) %>% mutate(p = psycho::format_p(p)))
colnames(res_adj2) <- c("Model",colnames(res_adj2)[2:8])
colnames(res_noadj2) <- c("Model",colnames(res_noadj2)[2:8])
csv_print2 <- rbind(res_adj2,res_noadj2)

if (write_csv == 1) {
  write.csv(to_print,":/Users/scastiello/OneDrive - Nexus365/1.- Contingency Learning (OBE-TM)/D cell paper/stats/exp18_LMM_duration.csv"
            , row.names = F)
}



######## VISUALIZATION ######## 
if(!require(ggplot2)) {install.packages("ggplot2")}
library(ggplot2)

# get data ready
# change factor oder, first positive (0.42), then zero (0), and finnaly negative (-0.42)
lf_no0$stage <- factor(lf_no0$stage, levels = c("N", "Z", "P"))
levels(lf_no0$stage) <- c('Negative', 'Zero', 'Positive')
lf_no0$frequency <- as.integer(as.character(factor(lf_no0$frequency, levels = c("0", "8", "24", "72"))))

# change factor oder, first positive (0.42), then zero (0), and finnaly negative (-0.42)
lf_no02$stage <- factor(lf_no02$stage, levels = c("N", "Z", "P"))
levels(lf_no02$stage) <- c('Negative', 'Zero', 'Positive')
lf_no02$duration <- as.integer(as.character(factor(lf_no02$duration, levels = c("0", "150", "450", "1350"))))



# plot size in pixels 1500 x 1000. PDF 15 x 10
plot_title_size <- 48#24
strip_text_size <- 44#22
axis_title_size <- 40#20
axis_text_size <- 36#18
legend_title_size <- 40#20
legend_text_size <- 36#18
pos <- position_dodge(5)

pF <- ggplot(lf_no0[,], aes(x = frequency, y = rating, col = adjustedDur, shape = adjustedDur)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "gray50", size = 1.2) +
  #geom_line(aes(group = subject, col = adjustedDur)) +
  #geom_line(aes(y=fit), size = 0.8, colour = "gray90") +
  #geom_jitter(width = 2, height = 0.2, alpha = 0.5 ,color = "gray70", size = 1) +
  geom_point(aes(y = jitter(rating)),alpha = 0.2, size = 1.2, position = pos) +
  geom_smooth(method = "lm", aes(group = adjustedDur, col = adjustedDur), size = 2, se = F) +
  stat_summary(fun.data = "mean_cl_boot", size = 2, position = pos) +
  labs(title = "Number effect by Contingency", subtitle = paste("n =",nSubj)) + #, shape = "Condition") +
  xlab("Cell D Number") + ylab("Rating") +
  #scale_shape_manual(values = c(15, 17, 19), breaks = c("few","baseline","many")) +  
  scale_x_continuous(limits = limits_vec, breaks = break_vec, minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  scale_colour_manual(values=c("#440154FF","#218F8DFF")) +
  facet_grid(. ~ stage) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = strip_text_size),
        plot.title = element_text(size = plot_title_size, hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = plot_title_size/2, hjust=0, face = "italic", color = "black"),
        plot.margin = unit(c(0.2,0.2,1.2,0.2), "cm"),
        axis.title.y = element_text(size = axis_title_size, face = "bold"),
        axis.text.y = element_text(size = axis_text_size, colour="black"),
        axis.title.x = element_text(size = axis_title_size, face = "bold"),
        axis.text.x = element_text(size = axis_text_size*0.9, colour="black"),
        axis.line = element_line(colour = 'black', size = 1.6),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length = unit(10, "pt"),
        legend.title = element_text(size = legend_title_size), 
        legend.text = element_text(size = legend_text_size),
        legend.position = "bottom",
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),
        panel.border = element_rect(color = "black"),
        panel.spacing = unit(1, "lines")
)
pF



# plot size in pixels 1500 x 1000. PDF 15 x 10
plot_title_size <- 48#24
strip_text_size <- 44#22
axis_title_size <- 40#20
axis_text_size <- 36#18
legend_title_size <- 40#20
legend_text_size <- 36#18
pos <- position_dodge(90)

pD <- ggplot(lf_no02, aes(x = duration, y = rating, col = adjustedNum, shape = adjustedNum)) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "gray50", size = 1.2) +
  #geom_line(aes(group = subject), alpha = 0.2, position = pos) +
  #geom_line(aes(y=fit), size = 0.8, colour = "gray90") +
  #geom_jitter(width = 20, height = 0.2, alpha = 0.5 ,color = "gray70", size = 1) +
  geom_point(aes(y = jitter(rating)),alpha = 0.2, size = 1.2, position = pos) +
  geom_smooth(method = "lm", aes(group = adjustedNum, col = adjustedNum), size = 2, se = F) +
  stat_summary(fun.data = "mean_cl_boot", size = 2, position = pos) +
  labs(title = "Duration effect by Contingency", subtitle = paste("n =",nSubj)) + #, shape = "Condition") +
  xlab("Cell D Duration (ms)") + ylab("Rating") +
  #scale_shape_manual(values = c(15, 17, 19), breaks = c("few","baseline","many")) +  
  scale_x_continuous(limits = limits_vec2, breaks = break_vec2, minor_breaks = NULL) + 
  scale_y_continuous(limits = c(-10.5, 10.5), breaks = c(-10, 0, 10)) +
  scale_colour_manual(values=c("#440154FF","#218F8DFF")) +
  facet_wrap(. ~ stage) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = strip_text_size),
        plot.title = element_text(size = plot_title_size, hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = plot_title_size/2, hjust=0, face = "italic", color = "black"),
        plot.margin = unit(c(1.2,0.2,0.2,0.2), "cm"),
        axis.title.y = element_text(size = axis_title_size, face = "bold"),
        axis.text.y = element_text(size = axis_text_size, colour="black"),
        axis.title.x = element_text(size = axis_title_size, face = "bold"),
        axis.text.x = element_text(size = axis_text_size*0.7, colour="black"),
        axis.line = element_line(colour = 'black', size = 1.6),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length = unit(10, "pt"),
        legend.title = element_text(size = legend_title_size), 
        legend.text = element_text(size = legend_text_size),
        legend.position = "bottom",
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor =element_blank(),  
        panel.background = element_blank(),
        panel.border = element_rect(color = "black"),
        panel.spacing = unit(1, "lines")
  )
pD

# wi
ggpubr::ggarrange(pF,pD, plotlist = NULL, ncol = NULL, nrow = 2,
          labels = NULL, label.x = 0, label.y = 1, hjust = -0.5,
          vjust = 1.5, font.label = list(size = 52, color = "black", face = "bold", family = NULL), 
          align = c("none", "h", "v", "hv"), widths = 1, heights = 1, legend = NULL, common.legend = FALSE)


