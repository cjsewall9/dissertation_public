# load packages #

library(tidyverse)
library(ggplot2)
library(skimr)
library(patchwork)
library(reshape2)


###################plots######################

##### screentime, social media, and PROMIS measures #####

  # add jitter to each wave for plotting
  set.seed(1021)
  combined_dat$wj <- jitter(combined_dat$wave, amount=.09)

  # total screen time
  st <- ggplot(combined_dat, aes(wj, st_total)) +
    geom_line(aes(group= prolific_pid), color = 'lightgray', alpha = 0.3) +
    geom_point(color = "purple", alpha = 0.25) +
    geom_boxplot(lwd = .1,data = combined_dat %>%
            filter(wave==1), aes(x=wave, y = st_total), width = .3,
            color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1,data = combined_dat %>%
            filter(wave==2), aes(x=wave, y = st_total), width = .3,
            color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1,data = combined_dat %>%
            filter(wave==3), aes(x=wave, y = st_total), width = .3,
            color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1,data = combined_dat %>%
            filter(wave==4), aes(x=wave, y = st_total), width = .3,
            color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    scale_x_continuous(breaks=c(1,2,3,4), labels=c("1", "2", "3", "4"), limits=c(0.5, 4.5) ) +
    xlab("Wave") + ylab("Screentime (hours)") +
    theme_classic() 

  st


  # social media
  sm <- ggplot(combined_dat, aes(wj, sm_total)) +
    geom_line(aes(group= prolific_pid), color = 'lightgray', alpha = 0.3) +
    geom_point(color = "cyan", alpha = 0.25) +
    geom_boxplot(lwd = .1,data = combined_dat %>%
              filter(wave==1), aes(x=wave, y = sm_total), width = .3,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1,data = combined_dat %>%
              filter(wave==2), aes(x=wave, y = sm_total), width = .3,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1,data = combined_dat %>%
              filter(wave==3), aes(x=wave, y = sm_total), width = .3,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1,data = combined_dat %>%
              filter(wave==4), aes(x=wave, y = sm_total), width = .3,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    scale_x_continuous(breaks=c(1,2,3,4), labels=c("1", "2", "3", "4"), limits=c(0.5, 4.5) ) +
    scale_y_continuous(limits = c(0, 65)) +
    xlab("Wave") + ylab("Social Media (hours)") +
    theme_classic() 

  sm


  # pickups
  pups <- ggplot(combined_dat, aes(wj, st_pickups_total)) +
    geom_line(aes(group= prolific_pid), color = 'lightgray', alpha = 0.3) +
    geom_point(color = "deeppink", alpha = 0.25) +
    geom_boxplot(lwd = .1,data = combined_dat %>%
              filter(wave==1), aes(x=wave, y = st_pickups_total), width = .3,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1,data = combined_dat %>%
              filter(wave==2), aes(x=wave, y = st_pickups_total), width = .3,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1,data = combined_dat %>%
              filter(wave==3), aes(x=wave, y = st_pickups_total), width = .3,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1,data = combined_dat %>%
              filter(wave==4), aes(x=wave, y = st_pickups_total), width = .3,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    scale_x_continuous(breaks=c(1,2,3,4), labels=c("1", "2", "3", "4"), limits=c(0.5, 4.5) ) +
    xlab("Wave") + ylab("Pickups") +
    theme_classic() 

  pups

  
  # depression
  dep <- ggplot(combined_dat, aes(wj, dep_tscore)) +
    geom_line(aes(x=wj, group= prolific_pid), color = 'lightgray', alpha = 0.3) +
    geom_point(color = "dodgerblue", alpha = 0.15) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==1), aes(x=wave, y = dep_tscore), width = .2,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==2), aes(x=wave, y = dep_tscore), width = .2,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==3), aes(x=wave, y = dep_tscore), width = .2,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==4), aes(x=wave, y = dep_tscore), width = .2,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    scale_x_continuous(breaks=c(1,2,3,4), labels=c("1", "2", "3", "4"), limits=c(0.5, 4.5) ) +
    xlab("Wave") + ylab("Depression") +
    theme_classic() 

  dep

 # anxiety
  anx <- ggplot(combined_dat, aes(wj, anx_tscore)) +
    geom_line(aes(x=wj, group= prolific_pid), color = 'lightgray', alpha = 0.3) +
    geom_point(color = "red", alpha = 0.15) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==1), aes(x=wave, y = anx_tscore), width = .2,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==2), aes(x=wave, y = anx_tscore), width = .2,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==3), aes(x=wave, y = anx_tscore), width = .2,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==4), aes(x=wave, y = anx_tscore), width = .2,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    scale_x_continuous(breaks=c(1,2,3,4), labels=c("1", "2", "3", "4"), limits=c(0.5, 4.5) ) +
    xlab("Wave") + ylab("Anxiety") +
    theme_classic() 

  anx

  # sleep dist.
  sleep <- ggplot(combined_dat, aes(wj, sleep_tscore)) +
    geom_line(aes(x=wj, group= prolific_pid), color = 'lightgray', alpha = 0.3) +
    geom_point(color = "green", alpha = 0.15) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==1), aes(x=wave, y = sleep_tscore), width = .2,
              color = 'black', size=1, alpha = 0, na.rm = TRUE) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==2), aes(x=wave, y = sleep_tscore), width = .2,
              color = 'black', size=1, alpha = 0, na.rm = TRUE) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==3), aes(x=wave, y = sleep_tscore), width = .2,
              color = 'black', size=1, alpha = 0, na.rm = TRUE) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==4), aes(x=wave, y = sleep_tscore), width = .2,
              color = 'black', size=1, alpha = 0, na.rm = TRUE) +
    scale_x_continuous(breaks=c(1,2,3,4), labels=c("1", "2", "3", "4"), limits=c(0.5, 4.5) ) +
    xlab("Wave") + ylab("Sleep Disturbance") +
    theme_classic() 

  sleep

  # social isolation
  iso <- ggplot(combined_dat, aes(wj, soc_iso_tscore)) +
    geom_line(aes(x=wj, group= prolific_pid), color = 'lightgray', alpha = 0.3) +
    geom_point(color = "orange", alpha = 0.25) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==1), aes(x=wave, y = soc_iso_tscore), width = .2,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==2), aes(x=wave, y = soc_iso_tscore), width = .2,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==3), aes(x=wave, y = soc_iso_tscore), width = .2,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    geom_boxplot(lwd = .1, data = combined_dat %>%
              filter(wave==4), aes(x=wave, y = soc_iso_tscore), width = .2,
              color = 'black', size=1, alpha = 0.15, na.rm = TRUE) +
    scale_x_continuous(breaks=c(1,2,3,4), labels=c("1", "2", "3", "4"), limits=c(0.5, 4.5) ) +
    xlab("Wave") + ylab("Social Isolation") +
    theme_classic() 

  iso

  
  # plot side-by-side 
  all <- st + sm + pups + dep + anx + sleep + iso
  st_sm_pups <- st + sm + pups
  promis <- dep + anx + sleep + iso


  # save high-quality versions

  tiff("all_dotandboxplots.tiff", units="in", width=7.75, height=6, res=300) 
  all
  dev.off()

  tiff("stsmpups_dotandboxplots.tiff", units="in", width=7.75, height=6, res=300) 
  st_sm_pups
  dev.off()

  tiff("promis_dotandboxplots.tiff", units="in", width=7.75, height=6, res=300) 
  promis
  dev.off()



############# individual plots ##############
  # standardize variables

  z_dat <- combined_dat %>% 
    select(id, wave, st_total, sm_total, dep_tscore, 
         anx_tscore, sleep_tscore, soc_iso_tscore) 

  z_dat <- z_dat %>% 
    mutate_at(vars(c(-id, -wave)),scale)

  # create random subsample of those with >2 waves
  set.seed(1512)
  z_dat <- z_dat %>% 
   count(id) %>% 
   left_join(z_dat, by = 'id') %>%
   filter(n>2) 

  pids <- z_dat %>%
    group_by(id) %>%
    summarize() %>%
    sample_n(50)

  slice <- right_join(z_dat, pids, by= 'id')


  i <- ggplot(data=slice, aes(x=wave)) +
    geom_line(aes(y=st_total, color= 'screen time') ) +
    geom_line(aes(y=dep_tscore, color = 'dep') ) +
    geom_line(aes(y=anx_tscore, color = 'anx') ) +
    geom_line(aes(y=sleep_tscore, color = 'sleep dist') )


  i <- i + facet_wrap(~ id, ncol = 10) +
          scale_color_manual("",
                     breaks = c("screen time", "dep", "anx", "sleep dist"),
                     values = c("screen time" ="black", "dep"="blue", "anx"="red", "sleep dist"="green")) +
          labs(x= "Wave", y= "z-score") 


  
tiff("ind_lineplots.tiff", units="in", width=7.75, height=6, res=300) 
i
dev.off()


##### plot variables by gender (male/female) #####

dat<- combined_dat %>%
  filter(gender != "other")


# summarize variables by gender using ::skimr
sum_dat_bygender<- dat %>%
  group_by(gender) %>%
  skim( c(
    age, st_total, sm_total, 
    st_pickups_total, health_steps_avg, 
    starts_with("covid_impact"), dep_tscore, si, 
    anx_tscore, sleep_tscore, soc_iso_tscore, 
    soc_comp_total) )

#summarize by gender by wave
sum_dat_bygenderwave<- dat %>%
  group_by(gender, wave) %>%
  skim( c(
    age, st_total, sm_total, 
    st_pickups_total, health_steps_avg, 
    starts_with("covid_impact"), dep_tscore, si, 
    anx_tscore, sleep_tscore, soc_iso_tscore, 
    soc_comp_total) )


# plot st/sm and promis variables by gender
meanst_bygender <- sum_dat_bygenderwave %>%
  select(wave, gender, skim_variable, numeric.mean) %>%
  filter(skim_variable == 'st_total')

st_bygender <- ggplot(meanst_bygender, aes(wave, numeric.mean, color = gender)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  geom_line(data=dat, aes(wj, st_total, group= id), color = 'lightgray', alpha = 0.3) +
  geom_point(data=dat, aes(wj, st_total, color=gender), alpha = 0.3, show.legend = FALSE) +
  xlab("Wave") + ylab("Screentime (hours)") +
  theme_classic() 


meansm_bygender <- sum_dat_bygenderwave %>%
  select(wave, gender, skim_variable, numeric.mean) %>%
  filter(skim_variable == 'sm_total')

sm_bygender <- ggplot(meansm_bygender, aes(wave, numeric.mean, color = gender)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  geom_line(data=dat, aes(wj, sm_total, group= id), color = 'lightgray', alpha = 0.3) +
  geom_point(data=dat, aes(wj, sm_total, color=gender), alpha = 0.3, show.legend = FALSE) +
  xlab("Wave") + ylab("Social Media (hours)") +
  scale_y_continuous(limits = c(0, 65)) +
  theme_classic() 


meanpups_bygender <- sum_dat_bygenderwave %>%
  select(wave, gender, skim_variable, numeric.mean) %>%
  filter(skim_variable == 'st_pickups_total')

pups_bygender <- ggplot(meanpups_bygender, aes(wave, numeric.mean, color = gender)) +
  geom_line(size = 1.5) +
  geom_line(data=dat, aes(wj, st_pickups_total, group= id), color = 'lightgray', alpha = 0.3) +
  geom_point(data=dat, aes(wj, st_pickups_total, color=gender), alpha = 0.3) +
  xlab("Wave") + ylab("Pickups") +
  theme_classic() 


meandep_bygender <- sum_dat_bygenderwave %>%
  select(wave, gender, skim_variable, numeric.mean) %>%
  filter(skim_variable == 'dep_tscore')

dep_bygender <- ggplot(meandep_bygender, aes(wave, numeric.mean, color = gender)) +
  geom_line(size = 1.5,show.legend = FALSE) +
  geom_line(data=dat, aes(wj, dep_tscore, group= id), color = 'lightgray', alpha = 0.3) +
  geom_point(data=dat, aes(wj, dep_tscore, color=gender), alpha = 0.3,show.legend = FALSE) +
  xlab("Wave") + ylab("Depression") +
  theme_classic() 


meananx_bygender <- sum_dat_bygenderwave %>%
  select(wave, gender, skim_variable, numeric.mean) %>%
  filter(skim_variable == 'anx_tscore')

anx_bygender <- ggplot(meananx_bygender, aes(wave, numeric.mean, color = gender)) +
  geom_line(size = 1.5,show.legend = FALSE) +
  geom_line(data=dat, aes(wj, anx_tscore, group= id), color = 'lightgray', alpha = 0.3) +
  geom_point(data=dat, aes(wj, anx_tscore, color=gender), alpha = 0.3,show.legend = FALSE) +
  xlab("Wave") + ylab("Anxiety") +
  theme_classic() 

meaniso_bygender <- sum_dat_bygenderwave %>%
  select(wave, gender, skim_variable, numeric.mean) %>%
  filter(skim_variable == 'soc_iso_tscore')

iso_bygender <- ggplot(meaniso_bygender, aes(wave, numeric.mean, color = gender)) +
  geom_line(size = 1.5,show.legend = FALSE) +
  geom_line(data=dat, aes(wj, soc_iso_tscore, group= id), color = 'lightgray', alpha = 0.3) +
  geom_point(data=dat, aes(wj, soc_iso_tscore, color=gender), alpha = 0.3,show.legend = FALSE) +
  xlab("Wave") + ylab("Social Isolation") +
  theme_classic() 


meansleep_bygender <- sum_dat_bygenderwave %>%
  select(wave, gender, skim_variable, numeric.mean) %>%
  filter(skim_variable == 'sleep_tscore')

sleep_bygender <- ggplot(meansleep_bygender, aes(wave, numeric.mean, color = gender)) +
  geom_line(size = 1.5,show.legend = FALSE) +
  geom_line(data=dat, aes(wj, sleep_tscore, group= id), color = 'lightgray', alpha = 0.3) +
  geom_point(data=dat, aes(wj, sleep_tscore, color=gender), alpha = 0.3) +
  xlab("Wave") + ylab("Sleep Disturbance") +
  theme_classic() 

#combine plots
usevars_bygender<- st_bygender + sm_bygender + pups_bygender
promisvars_bygender<- dep_bygender + anx_bygender + iso_bygender + sleep_bygender

tiff("usevars_bygender.tiff", units="in", width=7, height=5, res=300) 
usevars_bygender
dev.off()

tiff("promisvars_bygender.tiff", units="in", width=7, height=5, res=300) 
promisvars_bygender
dev.off()



###### plot aggregated usage_var~promis_var scatterplots by gender #####

# st ~ dep
st_dep<- ggplot(dat, aes(st_total,dep_tscore, color=gender) ) +
  geom_point(alpha = 0.2,show.legend = FALSE) +
  stat_cor(aes(label= ..r.label.. ), method = "pearson", label.x = 100, na.rm = TRUE, size = 3, show.legend = FALSE) +
  geom_smooth(method='lm',show.legend = FALSE)
# sm ~ dep
sm_dep<- ggplot(dat, aes(sm_total,dep_tscore, color=gender) ) +
  geom_point(alpha = 0.2,show.legend = FALSE) +
  stat_cor(aes(label= ..r.label.. ), method = "pearson", label.x = 35, na.rm = TRUE, size = 3, show.legend = FALSE) +
  geom_smooth(method='lm',show.legend = FALSE) +
  scale_x_continuous(limits = c(0,65))

# st ~ anx
st_anx<- ggplot(dat, aes(st_total,anx_tscore, color=gender) ) +
  geom_point(alpha = 0.2,show.legend = FALSE) +
  stat_cor(aes(label= ..r.label.. ), method = "pearson", label.x = 100, na.rm = TRUE, size = 3, show.legend = FALSE) +
  geom_smooth(method='lm',show.legend = FALSE)
# sm ~ anx
sm_anx<- ggplot(dat, aes(sm_total,anx_tscore, color=gender) ) +
  geom_point(alpha = 0.2,show.legend = FALSE) +
  stat_cor(aes(label= ..r.label.. ), method = "pearson", label.x = 35, na.rm = TRUE, size = 3, show.legend = FALSE) +
  geom_smooth(method='lm',show.legend = FALSE) +
  scale_x_continuous(limits = c(0, 65))

# st ~ iso
st_iso<- ggplot(dat, aes(st_total,soc_iso_tscore, color=gender) ) +
  geom_point(alpha = 0.2,show.legend = FALSE) +
  stat_cor(aes(label= ..r.label.. ), method = "pearson", label.x = 100, na.rm = TRUE, size = 3, show.legend = FALSE) +
  geom_smooth(method='lm',show.legend = FALSE)
# sm ~ iso
sm_iso<- ggplot(dat, aes(sm_total,soc_iso_tscore, color=gender) ) +
  geom_point(alpha = 0.2,show.legend = FALSE) +
  stat_cor(aes(label= ..r.label.. ), method = "pearson", label.x = 35, na.rm = TRUE, size = 3, show.legend = FALSE) +
  geom_smooth(method='lm',show.legend = TRUE) +
  scale_x_continuous(limits = c(0, 65))

# st ~ sleep
st_sleep <- ggplot(dat, aes(st_total,sleep_tscore, color=gender) ) +
  geom_point(alpha = 0.2,show.legend = FALSE) +
  stat_cor(aes(label= ..r.label.. ), method = "pearson", label.x = 100, na.rm = TRUE, size = 3, show.legend = FALSE) +
  geom_smooth(method='lm',show.legend = FALSE)
# sm ~ sleep
sm_sleep <- ggplot(dat, aes(sm_total,sleep_tscore, color=gender) ) +
  geom_point(alpha = 0.2,show.legend=FALSE) +
  stat_cor(aes(label= ..r.label.. ), method = "pearson", label.x = 35, na.rm = TRUE, size = 3, show.legend = FALSE) +
  geom_smooth(method='lm', show.legend = FALSE) +
  scale_x_continuous(limits = c(0, 65))

gender.scatters<-st_dep + sm_dep + st_anx + sm_anx + st_iso + sm_iso + st_sleep + sm_sleep

tiff("gender.scatters.tiff", units="in", width=7, height=5, res=300) 
gender.scatters
dev.off()
