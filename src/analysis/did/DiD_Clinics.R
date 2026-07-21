#Packages
library(here)
library(dplyr)
library(tidyverse) #Remove after test period
library(did)
library(haven)
library(ggplot2)
library(ggtext)
library(fixest)
library(HonestDiD)
library(plm)
library(panelView)
library(modelsummary)
library(estimatr)
library(DIDmultiplegtDYN)
library(RColorBrewer)
library(PanelMatch)
library(fect)
library(geosphere)
library(Cairo)
library(openxlsx)
library(rddensity)
library(rdrobust)
library(rdpower)
library(MatchIt)
library(cobalt)
library(kableExtra)
library(sandwich)
library(lmtest)
library(modelsummary)

#File Path
project_dir <- "C:/MYFILEPATH"

#Working Directory
setwd(project_dir)

#Load dataset
load("ZimServices.Rda")
gc()

CONST <- CONST %>%
  dplyr::mutate(clinics_outcome = clinics_outcome/(asqkm/100)) #Clinics per 100 square km, i.e., standardized

box.clinics <- ggplot(data = CONST,
                      mapping = aes(x = month,
                                    y = clinics_outcome,
                                    group = month))

pdf(file = file.path(project_dir, "Output", "Clinics_Boxplots.pdf"))

box.clinics + geom_boxplot() +
  scale_x_date(expand = expansion(mult = c(0.0125, 0.0625))) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20),
                     expand = expansion(mult = c(0.0125, 0))) +
  labs(x = "Months",
       y = "Health facilities per 100 square kilometers") +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5,
                                        linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

CONST <- CONST %>%
  dplyr::arrange(month,constID) %>%
  group_by(month) %>%
  dplyr::mutate(t = ifelse(row_number() == 1,1,0)) %>%
  ungroup %>%
  dplyr::mutate(time = cumsum(t)) %>%
  dplyr::arrange(constID,month) %>%
  dplyr::rename(treat = clinic,
                treatO = clinico,
                placebo = plcb_clinic,
                placeboO = plcb_clinico)

CONST <- subset(CONST, select = -c(t))

CONST <- CONST %>%
  dplyr::mutate(alt_treat = treat) %>%
  dplyr::mutate(alt_treat = ifelse(treatO == 1,2,alt_treat))

table(CONST$treat, useNA = "always")
table(CONST$treatO, useNA = "always")
table(CONST$alt_treat, useNA = "always")

#Treatment assignment plot
panelview(clinics_outcome ~ treatO,
          data = CONST,
          index = c("constID","time"), 
          xlab = "Month",
          ylab = "Electoral district",
          main = "",
          axis.lab = "time",
          axis.lab.gap = c(1,0),
          display.all = TRUE,
          gridOff = TRUE,
          by.timing = TRUE,
          color = c("gray98","black"),
          legend.labs = c("No question","Oral question"),
          background = "white")

pdf(file = file.path(project_dir, "Output", "Clinics_TreatAssign.pdf"))

panelview(clinics_outcome ~ factor(alt_treat),
          data = CONST,
          index = c("constID","time"), 
          xlab = "Month",
          ylab = "Electoral district",
          main = "",
          axis.lab = "time",
          axis.lab.gap = c(1,0),
          display.all = TRUE,
          gridOff = TRUE,
          by.timing = TRUE,
          color = c("gray98","gray54","black"),
          legend.labs = c("No question","Written question","Oral question"),
          background = "white")

dev.off()

#Spaghetti plot
CONST <- CONST %>%
  dplyr::arrange(constID,month) %>%
  group_by(constID) %>%
  dplyr::mutate(streat = ifelse(treat == 1,1,NA)) %>%
  dplyr::mutate(streat = ifelse(row_number() == 1 & is.na(streat),0,streat)) %>%
  fill(c("streat"), .direction = "down") %>%
  dplyr::mutate(ctreat = cumsum(treat)) %>%
  ungroup

pdf(file = file.path(project_dir, "Output", "Clinics_Spaghetti.pdf"))

panelview(clinics_outcome ~ streat,
          data = CONST,
          #Y='roads',
          #D='streat',
          index=c("constID","time"),
          by.timing = TRUE,
          display.all = TRUE,
          type = "outcome", 
          by.group = FALSE,
          by.cohort = FALSE,
          ylim = c(0,100),
          xlim = c(0,100),
          axis.lab.gap = c(0,0),
          ylab = "Health facilities per 100 square kilometers",
          xlab = "Month",
          main = "",
          color = c("gray70","black","gray90"),
          #legend.labs = c("After first question","Before first question","Controls"),
          background = "white")

dev.off()

#Preparing controls
CONST <- CONST %>%
  dplyr::arrange(constID,month) %>%
  group_by(constID) %>%
  dplyr::mutate(t = mean(treat, rm.na = TRUE)) %>%
  ungroup %>%
  dplyr::mutate(t = ifelse(t > 0,1,0)) %>%
  dplyr::mutate(opp = ifelse(zanupf == 0,1,0)) %>%
  dplyr::mutate(opp = ifelse(is.na(zanupf),NA,opp)) %>%
  dplyr::mutate(across(.cols = c("local","wtr","hw"),
                       ~ ifelse(is.na(.x),0,.x),
                       .names = NULL)) %>%
  fill(c("gender","diff","ccc","ind","alliance","mdct","npf","zanupf","opp"), .direction = "down") %>%
  dplyr::mutate(gender = ifelse(gender == "f",1,0))

#Identifying the lengths of gaps between one-shot treatments:
GAPS <- CONST %>%
  dplyr::arrange(constID,month) %>%
  group_by(constID) %>%
  dplyr::mutate(c = cumsum(treat)) %>%
  ungroup %>%
  group_by(constID,c) %>%
  dplyr::mutate(z = 1) %>%
  dplyr::mutate(gap = cumsum(z)) %>%
  dplyr::mutate(mgap = max(gap)) %>%
  filter(row_number() == 1) %>%
  slice(1) %>%
  ungroup

summary(GAPS$mgap)
GAPS <- subset(GAPS, mgap < max(time))

summary(GAPS$mgap)

GAPS <- GAPS %>%
  dplyr::mutate(color = ifelse(mgap < 9,"Below 9 months","At least 12 months")) %>%
  dplyr::mutate(color = ifelse(mgap >= 9 & mgap < 12,"Below 12 months",color))

gapp <- ggplot(data = GAPS,
               mapping = aes(x = mgap,
                             color = color,
                             fill = color))

pdf(file = file.path(project_dir, "Output", "Clinics_Gaps.pdf"))

gapp + geom_bar(width = .5) +
  scale_fill_manual(values = c("black", "gray60", "gray90")) +
  scale_color_manual(values = c("black", "gray60", "gray90")) +
  scale_x_continuous(limits = c(0,95), breaks = seq(0, 95, 5),
                     expand = expansion(mult = c(0, 0.0125))) +
  scale_y_continuous(limits = c(0,25), breaks = seq(0, 25, 5),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Gaps between treatments in months",
       y = "Count of spells") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

rm(gapp,GAPS)

#Descriptives
CONST <- CONST %>%
  dplyr::arrange(constID,month) %>%
  group_by(constID) %>%
  dplyr::mutate(tgroup = mean(treat)) %>%
  dplyr::mutate(tgroup = ifelse(tgroup > 0,1,0)) %>%
  dplyr::mutate(tgroupc = ifelse(tgroup == 1,"Treated","Control")) %>%
  ungroup

CONST <- CONST %>%
  dplyr::arrange(constID,month) %>%
  group_by(constID) %>%
  dplyr::mutate(ydiff = clinics_outcome[month == last(month)] - clinics_outcome[month == first(month)]) %>%
  ungroup

summary(CONST$ydiff)
table(CONST$const[CONST$ydiff < 0])

#Distances b/t Harare (Bulawayo) & leader's birthplace, minister's birthplace
CONST <- CONST %>%
  dplyr::mutate(HarareLeaderDist = distHaversine(cbind(31.059587168720384, -17.826373964585894), cbind(dLong, dLat))) %>%
  dplyr::mutate(BlwyLeaderDist = distHaversine(cbind(28.590884310867544, -20.144264297329126), cbind(dLong, dLat))) %>%
  dplyr::mutate(HarareLocalDist = distHaversine(cbind(31.059587168720384, -17.826373964585894), cbind(localLong, localLat))) %>%
  dplyr::mutate(BlwyLocalDist = distHaversine(cbind(28.590884310867544, -20.144264297329126), cbind(localLong, localLat))) %>%
  dplyr::mutate(HarareHlthDist = distHaversine(cbind(31.059587168720384, -17.826373964585894), cbind(hlthLong, hlthLat))) %>%
  dplyr::mutate(BlwyHlthDist = distHaversine(cbind(28.590884310867544, -20.144264297329126), cbind(hlthLong, hlthLat))) %>%
  dplyr::mutate(HarareBlwyDist = distHaversine(cbind(31.059587168720384, -17.826373964585894), cbind(28.590884310867544, -20.144264297329126))) %>%
  dplyr::mutate(HarareLeaderDist = HarareLeaderDist/1000) %>%
  dplyr::mutate(BlwyLeaderDist = BlwyLeaderDist/1000) %>%
  dplyr::mutate(HarareLocalDist = HarareLocalDist/1000) %>%
  dplyr::mutate(BlwyLocalDist = BlwyLocalDist/1000) %>%
  dplyr::mutate(HarareHlthDist = HarareHlthDist/1000) %>%
  dplyr::mutate(BlwyHlthDist = BlwyHlthDist/1000) %>%
  dplyr::mutate(HarareBlwyDist = HarareBlwyDist/1000)

table(CONST$HarareLeaderDist)
table(CONST$HarareLeaderDist[CONST$Mugabe == 1])  #Robert Mugabe
table(CONST$HarareLeaderDist[CONST$Mugabe == 0])  #Emmerson Mnangagwa

MugabeHarare <- min(CONST$HarareLeaderDist)
MnangagwaHarare <- max(CONST$HarareLeaderDist)

table(CONST$BlwyLeaderDist)
table(CONST$BlwyLeaderDist[CONST$Mugabe == 1])  #Robert Mugabe
table(CONST$BlwyLeaderDist[CONST$Mugabe == 0])  #Emmerson Mnangagwa

MugabeBlwy <- max(CONST$BlwyLeaderDist)
MnangagwaBlwy <- min(CONST$BlwyLeaderDist)

table(CONST$HarareLocalDist)
table(CONST$HarareLocalDist[CONST$month < as.Date("2017-11-30")])   #Saviour Kasukuwere
table(CONST$HarareLocalDist[CONST$month >= as.Date("2017-11-30")])  #July Moyo

KasukuwereHarare <- min(CONST$HarareLocalDist)
MoyoJHarare <- max(CONST$HarareLocalDist)

table(CONST$BlwyLocalDist)
table(CONST$BlwyLocalDist[CONST$month < as.Date("2017-11-30")])   #Saviour Kasukuwere
table(CONST$BlwyLocalDist[CONST$month >= as.Date("2017-11-30")])  #July Moyo

KasukuwereBlwy <- max(CONST$BlwyLocalDist)
MoyoBlwy <- min(CONST$BlwyLocalDist)

table(CONST$HarareHlthDist)
table(CONST$HarareHlthDist[CONST$month < as.Date("2018-09-06")])   #David Parirenyatwa
table(CONST$HarareHlthDist[CONST$month >= as.Date("2018-09-07") & CONST$month <= as.Date("2020-08-03")])  #Obadiah Moyo
table(CONST$HarareHlthDist[CONST$month >= as.Date("2020-08-04")])  #Constantino Chiwenga

MoyoOHarare <- max(CONST$HarareHlthDist)
ParirenyatwaHarare <- min(CONST$HarareHlthDist)
ChiwengaHarare <- median(CONST$HarareHlthDist)

table(CONST$BlwyHlthDist)
table(CONST$BlwyHlthDist[CONST$month < as.Date("2018-09-06")])   #David Parirenyatwa
table(CONST$BlwyHlthDist[CONST$month >= as.Date("2018-09-07") & CONST$month <= as.Date("2020-08-03")])  #Obadiah Moyo
table(CONST$BlwyHlthDist[CONST$month >= as.Date("2020-08-04")])  #Constantino Chiwenga

MoyoBlwy <- min(CONST$BlwyHlthDist)
ParirenyatwaBlwy <- max(CONST$BlwyHlthDist)
ChiwengaBlwy <- median(CONST$BlwyHlthDist)

table(CONST$HarareBlwyDist)

HarareBlwy <- mean(CONST$HarareBlwyDist)

table(CONST$mssn[CONST$month == as.Date("2015-09-15")])
CONST <- CONST %>%
  mutate(mssn = if_else(mssn >= 1,1,0))

#Harare
CONST <- CONST %>%
  dplyr::mutate(opp_b = ifelse(opp == 1,as.character(expression("Opposition")),as.character(expression("ZANU(PF)"))))

p.harare <- ggplot(data = CONST,
                   mapping = aes(x = Harare,
                                 y = ydiff,
                                 group = tgroupc,
                                 color = tgroupc,
                                 fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_Harare.pdf"))

p.harare + geom_point(aes(shape = opp_b)) +
  geom_smooth(method = "loess",
              se = TRUE) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_color_manual(values = c("gray70", "black")) +
  scale_x_continuous(limits = c(0,600), breaks = seq(0, 600, 100),
                     expand = expansion(mult = c(0, 0.0125))) +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Distance from Harare in kilometers",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) + #Road~expansion~(y[i,~2023]-y[i,~2015])
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank()) 

dev.off()

#Bulawayo
p.bulawayo <- ggplot(data = CONST,
                     mapping = aes(x = Bulawayo,
                                   y = ydiff,
                                   group = tgroupc,
                                   color = tgroupc,
                                   fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_Bulawayo.pdf"))

p.bulawayo + geom_point(aes(shape = opp_b)) +
  geom_smooth(method = "loess",
              se = TRUE) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_color_manual(values = c("gray70", "black")) +
  scale_x_continuous(limits = c(0,600), breaks = seq(0, 600, 100),
                     expand = expansion(mult = c(0, 0.0125))) +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  geom_segment(aes(x = 0, y = 54, xend = HarareBlwy, yend = 54),
               color = "black",
               linewidth = 1,
               arrow = arrow(angle = 30,
                             length = unit(.1, "cm"),
                             type = "closed",
                             ends = "last")) +
  annotate(geom = "text", 
           x=HarareBlwy-.5*HarareBlwy, y=57.5, 
           label="Harare",
           color="black") +
  labs(x = "Distance from Bulawayo in kilometers",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) + #Road~expansion~(y[i,~2023]-y[i,~2015])
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank()) 

dev.off()

#Infant mortality u5 (base)
p.infm <- ggplot(data = CONST,
               mapping = aes(x = u5mr_smooth,
                             y = ydiff,
                             group = tgroupc,
                             color = tgroupc,
                             fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_InfMortality.pdf"))

p.infm + geom_point(aes(shape = opp_b)) +
  geom_smooth(method = "loess",
              se = TRUE) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_color_manual(values = c("gray70", "black")) +
  scale_x_continuous(limits = c(0,160), breaks = seq(0, 160, 20),
                     expand = expansion(mult = c(0.0125, 0.0125))) +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Infant mortality rate under five years",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) + #Road~expansion~(y[i,~2023]-y[i,~2015])
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank()) 

dev.off()

#Standardized population size per electoral district (2012)
CONST <- CONST %>%
  dplyr::mutate(spop = log(pop/asqkm)) %>%
  dplyr::mutate(h200 = ifelse(Harare <= 200,as.character(expression("\u2264 200km from Harare")),as.character(expression("> 200km from Harare"))))

p.spop <- ggplot(data = CONST,
                 mapping = aes(x = spop,
                               y = ydiff,
                               group = tgroupc,
                               color = tgroupc,
                               fill = tgroupc))

cairo_pdf(filename = "C:/Users/Admin/Dropbox/Manuscripts/Query Sessions/Service Provision/Output/Clinics_Population.pdf")

p.spop + geom_point(aes(shape = opp_b)) +
  geom_smooth(method = "loess",
              se = TRUE) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_color_manual(values = c("gray70", "black")) +
  scale_x_continuous(limits = c(0,10), breaks = seq(0, 10, 2),
                     expand = expansion(mult = c(0, 0.025))) +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Logged population density",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank()) 

dev.off()

#Distance to leader's birthplace
p.dDist <- ggplot(data = CONST,
                  mapping = aes(x = dDist,
                                y = ydiff,
                                group = tgroupc,
                                color = tgroupc,
                                fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_DistanceD.pdf"))

p.dDist + geom_point(aes(shape = opp_b)) +
  geom_smooth(method = "loess",
              se = TRUE) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_color_manual(values = c("gray70", "black")) +
  scale_x_continuous(limits = c(0,600), breaks = seq(0, 600, 100),
                     expand = expansion(mult = c(0, 0.0125))) +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  geom_segment(aes(x = 0, y = 54, xend = MugabeHarare, yend = 54),
               color = "black",
               linewidth = 1,
               arrow = arrow(angle = 30,
                             length = unit(.1, "cm"),
                             type = "closed",
                             ends = "last")) +
  annotate(geom = "text", 
           x=MugabeHarare+25, y=57.5, 
           label="Mugabe's birthplace to Harare",
           color="black") +
  geom_segment(aes(x = 0, y = 35, xend = MnangagwaHarare, yend = 35),
               color = "black",
               linewidth = 1,
               arrow = arrow(angle = 30,
                             length = unit(.1, "cm"),
                             type = "closed",
                             ends = "last")) +
  annotate(geom = "text", 
           x=MnangagwaHarare-115, y=38, 
           label="Mnangagwa's birthplace to Harare",
           color="black") +
  labs(x = "Distance to the leader's birthplace",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank()) 

dev.off()

#Distance to minister's birthplace (health)
p.hlthDist <- ggplot(data = CONST,
                     mapping = aes(x = hlthDist,
                                   y = ydiff,
                                   group = tgroupc,
                                   color = tgroupc,
                                   fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_DistanceHlth.pdf"))

p.hlthDist + geom_point(aes(shape = opp_b)) +
  geom_smooth(method = "loess",
              se = TRUE) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_color_manual(values = c("gray70", "black")) +
  scale_x_continuous(limits = c(0,700), breaks = seq(0, 700, 100),
                     expand = expansion(mult = c(0, 0.0125))) +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  geom_segment(aes(x = 0, y = 65, xend = ChiwengaHarare, yend = 65),
               color = "black",
               linewidth = 1,
               arrow = arrow(angle = 30,
                             length = unit(.1, "cm"),
                             type = "closed",
                             ends = "last")) +
  annotate(geom = "text", 
           x=ChiwengaHarare+.25*ChiwengaHarare, y=67.5, 
           label="Chiwenga's birthplace to Harare",
           color="black") +
  geom_segment(aes(x = 0, y = 50, xend = MoyoOHarare, yend = 50),
               color = "black",
               linewidth = 1,
               arrow = arrow(angle = 30,
                             length = unit(.1, "cm"),
                             type = "closed",
                             ends = "last")) +
  annotate(geom = "text", 
           x=MoyoOHarare-.25*MoyoOHarare, y=52.5, 
           label="Moyo's birthplace to Harare",
           color="black") +
  geom_segment(aes(x = 0, y = 35, xend = ParirenyatwaHarare, yend = 35),
               color = "black",
               linewidth = 1,
               arrow = arrow(angle = 30,
                             length = unit(.1, "cm"),
                             type = "closed",
                             ends = "last")) +
  annotate(geom = "text", 
           x=ParirenyatwaHarare+.75*ParirenyatwaHarare, y=38, 
           label="Parirenyatwa's birthplace to Harare",
           color="black") +
  labs(x = "Distance to a minister's birthplace (health)",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank()) 

dev.off()

#Distance to minister's birthplace (local govt)
p.localDist <- ggplot(data = CONST,
                      mapping = aes(x = localDist,
                                    y = ydiff,
                                    group = tgroupc,
                                    color = tgroupc,
                                    fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_DistanceLocal.pdf"))

p.localDist + geom_point(aes(shape = opp_b)) +
  geom_smooth(method = "loess",
              se = TRUE) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_color_manual(values = c("gray70", "black")) +
  scale_x_continuous(limits = c(0,700), breaks = seq(0, 700, 100),
                     expand = expansion(mult = c(0, 0.0125))) +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  geom_segment(aes(x = 0, y = 50, xend = KasukuwereHarare, yend = 50),
               color = "black",
               linewidth = 1,
               arrow = arrow(angle = 50,
                             length = unit(.1, "cm"),
                             type = "closed",
                             ends = "last")) +
  annotate(geom = "text", 
           x=KasukuwereHarare+.25*KasukuwereHarare, y=53, 
           label="Kasukuwere's birthplace to Harare",
           color="black") +
  geom_segment(aes(x = 0, y = 30, xend = MoyoJHarare, yend = 30),
               color = "black",
               linewidth = 1,
               arrow = arrow(angle = 30,
                             length = unit(.1, "cm"),
                             type = "closed",
                             ends = "last")) +
  annotate(geom = "text", 
           x=MoyoJHarare-.5*MoyoJHarare, y=33, 
           label="Moyo's birthplace to Harare",
           color="black") +
  labs(x = "Distance to a minister's birthplace (local govt)",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank()) 

dev.off()

#Difference in vote shares
p.diff <- ggplot(data = CONST,
                 mapping = aes(x = diff,
                               y = ydiff,
                               group = tgroupc,
                               color = tgroupc,
                               fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_VoteShare.pdf"))

p.diff + geom_point(aes(shape = opp_b)) +
  geom_smooth(method = "loess",
              se = TRUE) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_color_manual(values = c("gray70", "black")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, .2),
                     expand = expansion(mult = c(0.0125, 0.0125))) +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Differences in vote shares",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank()) 

dev.off()

#Roads network
CONST <- CONST %>%
  dplyr::mutate(roads_outcome = roads_outcome/1000,
                roads_outcome = roads_outcome/asqkm)

p.road <- ggplot(data = CONST,
                 mapping = aes(x = roads_outcome,
                               y = ydiff,
                               group = tgroupc,
                               color = tgroupc,
                               fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_Roads.pdf"))

p.road + geom_point(aes(shape = opp_b)) +
  geom_smooth(method = "loess",
              se = TRUE) +
  scale_fill_manual(values = c("gray70", "black")) +
  scale_color_manual(values = c("gray70", "black")) +
  scale_x_continuous(limits = c(0,20), breaks = seq(0, 20, 5),
                     expand = expansion(mult = c(0.0125, 0.0125))) +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Roads per square kilometer",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank()) 

dev.off()

#Political affiliation
CONST <- CONST %>%
  dplyr::mutate(oppc = ifelse(opp == 1,"Opposition","ZANU(PF)"))

p.opp <- ggplot(data = CONST,
                mapping = aes(x = oppc,
                              y = ydiff,
                              fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_OppvsRegime.pdf"))

p.opp + geom_boxplot(position = position_dodge(width=1), 
                     width = 0.8) +
  scale_fill_manual(values=c("Control"="white",
                             "Treated"="gray70")) +
  scale_x_discrete() +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Political affiliation",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#MP as Heavyweights
CONST <- CONST %>%
  dplyr::mutate(hwc = ifelse(hw == 1,"Yes","No"))

p.hw <- ggplot(data = CONST,
               mapping = aes(x = hwc,
                             y = ydiff,
                             fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_Heavyweights.pdf"))

p.hw + geom_boxplot(position = position_dodge(width=1), 
                    width = 0.8) +
  scale_fill_manual(values=c("Control"="white",
                             "Treated"="gray70")) +
  scale_x_discrete() +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Legislator is a heavyweight of their political party",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#Minister with Health Portfolio as Heavyweights
CONST <- CONST %>%
  dplyr::mutate(hlthHWc = ifelse(hlthHW == 1,"Yes","No"))

p.hlthHW <- ggplot(data = CONST,
                   mapping = aes(x = hlthHWc,
                                 y = ydiff,
                                 fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_HeavyweightHlth.pdf"))

p.hlthHW + geom_boxplot(position = position_dodge(width=1), 
                       width = 0.8) +
  scale_fill_manual(values=c("Control"="white",
                             "Treated"="gray70")) +
  scale_x_discrete() +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Minister (health) is a ZANU(PF) heavyweight",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#Committee membership: health
CONST <- CONST %>%
  dplyr::mutate(hlthc = if_else(hlth == 1,"Yes","No"))

p.hlth <- ggplot(data = CONST,
                mapping = aes(x = hlthc,
                              y = ydiff,
                              fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_Hlth.pdf"))

p.hlth + geom_boxplot(position = position_dodge(width=1), 
                     width = 0.8) +
  scale_fill_manual(values=c("Control"="white",
                             "Treated"="gray70")) +
  scale_x_discrete() +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Legislator is a member of the portfolio committee on health",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#Committee membership: local govt
CONST <- CONST %>%
  dplyr::mutate(localc = ifelse(local == 1,"Yes","No"))

p.local <- ggplot(data = CONST,
                  mapping = aes(x = localc,
                                y = ydiff,
                                fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_Local.pdf"))

p.local + geom_boxplot(position = position_dodge(width=1), 
                       width = 0.8) +
  scale_fill_manual(values=c("Control"="white",
                             "Treated"="gray70")) +
  scale_x_discrete() +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Legislator is a member of the portfolio committee on local govt",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#Historical Missions
CONST <- CONST %>%
  dplyr::mutate(mssnc = ifelse(mssn == 1,"Mission","No Mission"))

p.mssn <- ggplot(data = CONST,
                 mapping = aes(x = mssnc,
                               y = ydiff,
                               fill = tgroupc))

pdf(file = file.path(project_dir, "Output", "Clinics_Mission.pdf"))

p.mssn + geom_boxplot(position = position_dodge(width=1), 
                    width = 0.8) +
  scale_fill_manual(values=c("Control"="white",
                             "Treated"="gray70")) +
  scale_x_discrete() +
  scale_y_continuous(limits = c(-10,70), breaks = seq(-10, 70, 10),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Historical mission in electoral district",
       y = expression(Clinics~expansion~(y[2023]-y[2015]))) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#################
##  12 Months ###
#################

CONST <- CONST %>%
  dplyr::arrange(constID,month) %>%
  group_by(constID) %>%
  dplyr::mutate(c = cumsum(treat)) %>%
  ungroup %>%
  group_by(constID,c) %>%
  dplyr::mutate(z = 1) %>%
  dplyr::mutate(gap = cumsum(z)) %>%
  dplyr::mutate(mgap = max(gap)) %>%
  dplyr::mutate(lgap12 = ifelse(mgap >= 12,1,0)) %>%
  ungroup

#Matching & DiD (Imai, Kim, & Wang 2023):
is.pbalanced(CONST$constID,CONST$time)
is.pbalanced(CONST$constID,CONST$month)
is.pbalanced(CONST)

CONST <- make.pbalanced(CONST,
                        balance.type = c("fill"), 
                        index = c("constID","time"))

is.integer(CONST$constID)
is.integer(CONST$time)
is.integer(CONST$treatO)
is.numeric(CONST$clinics_outcome)

CONST$constID <- as.integer(CONST$constID)
CONST$time <- as.integer(CONST$time)
CONST$treatO <- as.integer(CONST$treatO)

CONST <- CONST %>%
  dplyr::mutate(y = ifelse(lgap12 == 1,clinics_outcome,NA))
summary(CONST$clinics_outcome)
summary(CONST$y)

CLINICS.PANEL <- PanelData(panel.data = CONST,
                           unit.id = "constID", #must be integer
                           time.id = "time", #must be integer
                           treatment = "treatO", #must be integer
                           outcome = "y") #must be numeric

#No refinement
PM.none <- PanelMatch(panel.data = CLINICS.PANEL,
                      lag = 4,
                      refinement.method = "none",
                      match.missing = FALSE,
                      covs.formula = ~ I(lag(y, 1:4)) +
                        I(lag(roads_outcome, 0:4)) +
                        u5mr_smooth +
                        I(lag(diff, 0:4)),
                      exact.match.variables = c("opp"),
                      forbid.treatment.reversal = FALSE,
                      qoi = "att",
                      lead = 0:11)

plot(PM.none)
summary(PM.none)
PM.none.msets <- extract(PM.none)
PM.none.wts <- weights(PM.none.msets)
PM.none.wts[["552090209.29"]]

#Mahalanobis
PM.maha <- PanelMatch(panel.data = CLINICS.PANEL,
                      lag = 4,
                      refinement.method = "mahalanobis",
                      size.match = 5, #Only relevant for matching methods
                      use.diagonal.variance.matrix = TRUE, #Only relevant for Mahalanobis
                      match.missing = FALSE,
                      covs.formula = ~ I(lag(y, 1:4)) +
                        I(lag(roads_outcome, 0:4)) +
                        u5mr_smooth +
                        I(lag(diff, 0:4)),
                      exact.match.variables = c("opp"),
                      forbid.treatment.reversal = FALSE,
                      qoi = "att",
                      lead = 0:11)

#Propensity score matching
PM.ps <- PanelMatch(panel.data = CLINICS.PANEL,
                    lag = 4,
                    refinement.method = "ps.match",
                    size.match = 5, #Only relevant for matching methods
                    match.missing = FALSE,
                    covs.formula = ~ I(lag(y, 1:4)) +
                      I(lag(roads_outcome, 0:4)) +
                      u5mr_smooth +
                      I(lag(diff, 0:4)),
                    exact.match.variables = c("opp"),
                    forbid.treatment.reversal = FALSE,
                    qoi = "att",
                    lead = 0:11)

#Covariate-balanced propensity score matching
PM.cbps <- PanelMatch(panel.data = CLINICS.PANEL,
                      lag = 4,
                      refinement.method = "CBPS.match",
                      size.match = 5, #Only relevant for matching methods
                      match.missing = FALSE,
                      covs.formula = ~ I(lag(y, 1:4)) +
                        I(lag(roads_outcome, 0:4)) +
                        u5mr_smooth +
                        I(lag(diff, 0:4)),
                      exact.match.variables = c("opp"),
                      forbid.treatment.reversal = FALSE,
                      qoi = "att",
                      lead = 0:11)

#Propensity score weighting
PM.psw <- PanelMatch(panel.data = CLINICS.PANEL,
                     lag = 4,
                     refinement.method = "ps.weight",
                     match.missing = FALSE,
                     covs.formula = ~ I(lag(y, 1:4)) +
                       I(lag(roads_outcome, 0:4)) +
                       u5mr_smooth +
                       I(lag(diff, 0:4)),
                     exact.match.variables = c("opp"),
                     forbid.treatment.reversal = FALSE,
                     qoi = "att",
                     lead = 0:11)

#Covariate-balanced propensity score weighting
PM.cbpsw <- PanelMatch(panel.data = CLINICS.PANEL,
                       lag = 4,
                       refinement.method = "CBPS.weight",
                       match.missing = FALSE,
                       covs.formula = ~ I(lag(y, 1:4)) +
                         I(lag(roads_outcome, 0:4)) +
                         u5mr_smooth +
                         I(lag(diff, 0:4)),
                       exact.match.variables = c("opp"),
                       forbid.treatment.reversal = FALSE,
                       qoi = "att",
                       lead = 0:11)

##########################
# Inspections of Overlap #
##########################
OLAP <- CLINICS.PANEL %>%
  arrange(constID,time) %>%
  group_by(constID) %>%
  mutate(y_l1 = lag(y,1),
         y_l2 = lag(y,2),
         y_l3 = lag(y,3),
         y_l4 = lag(y,4),
         
         road_l0 = roads_outcome,
         road_l1 = lag(roads_outcome,1),
         road_l2 = lag(roads_outcome,2),
         road_l3 = lag(roads_outcome,3),
         road_l4 = lag(roads_outcome,4),
         
         diff_l0 = diff,
         diff_l1 = lag(diff,1),
         diff_l2 = lag(diff,2),
         diff_l3 = lag(diff,3),
         diff_l4 = lag(diff,4)) %>%
  ungroup

OLAP <- OLAP %>%
  filter(!is.na(y_l4),
         !is.na(road_l4),
         !is.na(diff_l4))

OLAP$opp <- as.factor(OLAP$opp)

ps_mod <- glm(treat ~ y_l1 + y_l2 + y_l3 + y_l4 +
                
                road_l0 + road_l1 + road_l2 + road_l3 + road_l4 +
                
                u5mr_smooth +
                
                diff_l0 + diff_l1 + diff_l2 + diff_l3 + diff_l4 +
                
                opp,
              family = binomial(),
              data = OLAP)

OLAP$ps <- predict(ps_mod, type = "response")

#Density
p.overlap <- ggplot(data = OLAP,
                    mapping = aes(x = ps,
                                  fill = factor(treatO)))

pdf(file = file.path(project_dir, "Output", "Clinics_OverlapDensity.pdf"))

p.overlap +
  geom_density(alpha = 0.4) +
  labs(fill = "Treatment",
       x = "Estimated Propensity Score",
       y = "Density") +
  scale_fill_discrete(labels = c("0" = "Not treated",
                                 "1" = "Treated")) +
  scale_x_continuous(limits = c(0,0.010), breaks = seq(0, 0.010, 0.002),
                     expand = expansion(mult = c(0.0125, 0.0125))) +
  scale_y_continuous(limits = c(0,800), breaks = seq(0, 800, 200),
                     expand = expansion(mult = c(0.0125, 0))) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#Histogram
p.overlap +
  geom_histogram(position = "identity",
                 alpha = 0.4,
                 bins = 50) +
  coord_cartesian(xlim = c(0,.012)) +
  theme_minimal()

#Numerical Support (Q: max(control) < min(treated)?)
OLAP %>%
  group_by(treatO) %>%
  summarize(min_ps = min(ps),
            p1 = quantile(ps, .01),
            p5 = quantile(ps, .05),
            median = median(ps),
            p95 = quantile(ps, .95),
            p99 = quantile(ps, .99),
            max_ps = max(ps))

#Problematic Observations (= expected result given the data structure)
OLAP %>%
  filter(ps > .99 | ps < .01) %>%
  select(constID, time, treatO, ps, opp)

#Incremental Inclusion of Covariates
glm(treatO ~ y_l1 + y_l2 + y_l3 + y_l4,
    family = binomial(),
    data = OLAP)

glm(treatO ~ y_l1 + y_l2 + y_l3 + y_l4 + opp,
    family = binomial(),
    data = OLAP)

glm(treatO ~ y_l1 + y_l2 + y_l3 + y_l4 + opp +
      u5mr_smooth,
    family = binomial(),
    data = OLAP)

glm(treatO ~ y_l1 + y_l2 + y_l3 + y_l4 + opp +
      u5mr_smooth + road_l0 + road_l1 + road_l2 + road_l3 + road_l4,
    family = binomial(),
    data = OLAP)

glm(treatO ~ y_l1 + y_l2 + y_l3 + y_l4 + opp +
      u5mr_smooth + road_l0 + road_l1 + road_l2 + road_l3 + road_l4 +
      diff_l0 + diff_l1 + diff_l2 + diff_l3 + diff_l4,
    family = binomial(),
    data = OLAP)

#Simple Correlations
cor(OLAP$y_l1,OLAP$y_l2)
cor(OLAP$y_l2,OLAP$y_l3)
cor(OLAP$y_l3,OLAP$y_l4)

cor(OLAP$diff_l0,OLAP$diff_l1)
cor(OLAP$diff_l1,OLAP$diff_l2)
cor(OLAP$diff_l2,OLAP$diff_l3)
cor(OLAP$diff_l3,OLAP$diff_l4)

cor(OLAP$road_l0,OLAP$road_l1)
cor(OLAP$road_l1,OLAP$road_l2)
cor(OLAP$road_l2,OLAP$road_l3)
cor(OLAP$road_l3,OLAP$road_l4)

##############################
# REVISED MATCHING STRUCTURE #
##############################

#No refinement
PM.none <- PanelMatch(panel.data = CLINICS.PANEL,
                      lag = 2,
                      refinement.method = "none",
                      match.missing = FALSE,
                      covs.formula = ~ I(lag(y, 1:2)) +
                        I(lag(roads_outcome, 0:2)) +
                        u5mr_smooth +
                        diff,
                      exact.match.variables = c("opp"),
                      forbid.treatment.reversal = FALSE,
                      qoi = "att",
                      lead = 0:11)

plot(PM.none)
summary(PM.none)
PM.none.msets <- extract(PM.none)
PM.none.wts <- weights(PM.none.msets)
PM.none.wts[["552090209.29"]]

#Mahalanobis
PM.maha <- PanelMatch(panel.data = CLINICS.PANEL,
                      lag = 2,
                      refinement.method = "mahalanobis",
                      size.match = 5, #Only relevant for matching methods
                      use.diagonal.variance.matrix = TRUE, #Only relevant for Mahalanobis
                      match.missing = FALSE,
                      covs.formula = ~ I(lag(y, 1:2)) +
                        I(lag(roads_outcome, 0:2)) +
                        u5mr_smooth +
                        diff,
                      exact.match.variables = c("opp"),
                      forbid.treatment.reversal = FALSE,
                      qoi = "att",
                      lead = 0:11)

plot(PM.maha)

#Propensity score matching
PM.ps <- PanelMatch(panel.data = CLINICS.PANEL,
                    lag = 2,
                    refinement.method = "ps.match",
                    size.match = 5, #Only relevant for matching methods
                    match.missing = FALSE,
                    covs.formula = ~ I(lag(y, 1:2)) +
                      I(lag(roads_outcome, 0:2)) +
                      u5mr_smooth +
                      diff,
                    exact.match.variables = c("opp"),
                    forbid.treatment.reversal = FALSE,
                    qoi = "att",
                    lead = 0:11)

#Covariate-balanced propensity score matching
PM.cbps <- PanelMatch(panel.data = CLINICS.PANEL,
                      lag = 2,
                      refinement.method = "CBPS.match",
                      size.match = 5, #Only relevant for matching methods
                      match.missing = FALSE,
                      covs.formula = ~ I(lag(y, 1:2)) +
                        I(lag(roads_outcome, 0:2)) +
                        u5mr_smooth +
                        diff,
                      exact.match.variables = c("opp"),
                      forbid.treatment.reversal = FALSE,
                      qoi = "att",
                      lead = 0:11)

#Propensity score weighting
PM.psw <- PanelMatch(panel.data = CLINICS.PANEL,
                     lag = 2,
                     refinement.method = "ps.weight",
                     match.missing = FALSE,
                     covs.formula = ~ I(lag(y, 1:2)) +
                       I(lag(roads_outcome, 0:2)) +
                       u5mr_smooth +
                       diff,
                     exact.match.variables = c("opp"),
                     forbid.treatment.reversal = FALSE,
                     qoi = "att",
                     lead = 0:11)

#Covariate-balanced propensity score weighting
PM.cbpsw <- PanelMatch(panel.data = CLINICS.PANEL,
                       lag = 2,
                       refinement.method = "CBPS.weight",
                       match.missing = FALSE,
                       covs.formula = ~ I(lag(y, 1:2)) +
                         I(lag(roads_outcome, 0:2)) +
                         u5mr_smooth +
                         diff,
                       exact.match.variables = c("opp"),
                       forbid.treatment.reversal = FALSE,
                       qoi = "att",
                       lead = 0:11)

#Covariate balance
PM.covbal <- get_covariate_balance(PM.none, PM.maha, PM.ps, PM.cbps, PM.psw, PM.cbpsw,
                                   panel.data = CLINICS.PANEL,
                                   covariates = c("y","u5mr_smooth","roads_outcome","diff","opp"),
                                   include.unrefined = TRUE) 

PM.balance.none <-as.data.frame(PM.covbal[[1]]$att)
PM.balance.maha <-as.data.frame(PM.covbal[[2]]$att)
PM.balance.ps <-as.data.frame(PM.covbal[[3]]$att)
PM.balance.cbps <-as.data.frame(PM.covbal[[4]]$att)
PM.balance.psw <-as.data.frame(PM.covbal[[5]]$att)
PM.balance.cbpsw <-as.data.frame(PM.covbal[[6]]$att)

#Unrefined set
PM.balance.none <- PM.balance.none %>%
  rownames_to_column(., var = "time") %>%
  gather(., key = "variable", value = "value",-time)

# Convert time to a factor with levels ordered from t to t-1
PM.balance.none$time <- factor(PM.balance.none$time,
                               levels = c("t_2","t_1","t_0"),
                               labels = c("t-2","t-1","t"))

PM.balance.none$variable <- case_match(PM.balance.none$variable,
                                       "y" ~ "Clinics per 100 square km",
                                       "u5mr_smooth" ~ "Infant mortality",
                                       "roads_outcome" ~ "Road per square kilometer",
                                       "diff" ~ "Diff. in vote share",
                                       "opp" ~ "Opposition",
                                       .default = PM.balance.none$variable)

# Plot the data
cov.balance.none <- ggplot(data = PM.balance.none, 
                           mapping = aes(x = time,
                                         y = value,
                                         color = variable,
                                         group = variable))

pdf(file = file.path(project_dir, "Output", "Clinics12_CovBalNone.pdf"))

cov.balance.none +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  scale_fill_brewer(palette = "Spectral",
                    aesthetics = c("colour","fill")) +
  geom_hline(yintercept = 0,
             linetype = "longdash",
             linewidth = 0.75) +
  scale_x_discrete(expand = expansion(mult = c(0.05,0.05))) +
  scale_y_continuous(limits = c(-.25,.25), breaks = seq(-0.25, 0.25, .05),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Months before question", 
       y = "Standardized mean differences", 
       title = "") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) 

dev.off()

#Mahalanobis
PM.balance.maha <- PM.balance.maha %>%
  rownames_to_column(., var = "time") %>%
  gather(., key = "variable", value = "value",-time)

PM.balance.maha$time <- factor(PM.balance.maha$time,
                               levels = c("t_2","t_1","t_0"),
                               labels = c("t-2","t-1","t"))

PM.balance.maha$variable <- case_match(PM.balance.none$variable,
                                       "y" ~ "Clinics per 100 square km",
                                       "u5mr_smooth" ~ "Infant mortality",
                                       "roads_outcome" ~ "Road per square kilometer",
                                       "diff" ~ "Diff. in vote share",
                                       "opp" ~ "Opposition",
                                       .default = PM.balance.none$variable)

cov.balance.maha <- ggplot(data = PM.balance.maha, 
                           mapping = aes(x = time,
                                         y = value,
                                         color = variable,
                                         group = variable))

pdf(file = file.path(project_dir, "Output", "Clinics12_CovBalMaha.pdf"))

cov.balance.maha +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  scale_fill_brewer(palette = "Spectral",
                    aesthetics = c("colour","fill")) +
  geom_hline(yintercept = 0,
             linetype = "longdash",
             linewidth = 0.75) +
  scale_x_discrete(expand = expansion(mult = c(0.05,0.05))) +
  scale_y_continuous(limits = c(-.25,.25), breaks = seq(-0.25, 0.25, .05),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Months before question", 
       y = "Standardized mean differences", 
       title = "") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

dev.off()

#Propensity Score
PM.balance.ps <- PM.balance.ps %>%
  rownames_to_column(., var = "time") %>%
  gather(., key = "variable", value = "value",-time)

PM.balance.ps$time <- factor(PM.balance.ps$time,
                             levels = c("t_2","t_1","t_0"),
                             labels = c("t-2","t-1","t"))

PM.balance.ps$variable <- case_match(PM.balance.none$variable,
                                     "y" ~ "Clinics per 100 square km",
                                     "u5mr_smooth" ~ "Infant mortality",
                                     "roads_outcome" ~ "Road per square kilometer",
                                     "diff" ~ "Diff. in vote share",
                                     "opp" ~ "Opposition",
                                     .default = PM.balance.none$variable)

cov.balance.ps <- ggplot(data = PM.balance.ps, 
                         mapping = aes(x = time,
                                       y = value,
                                       color = variable,
                                       group = variable))

pdf(file = file.path(project_dir, "Output", "Clinics12_CovBalPS.pdf"))

cov.balance.ps +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  scale_fill_brewer(palette = "Spectral",
                    aesthetics = c("colour","fill")) +
  geom_hline(yintercept = 0,
             linetype = "longdash",
             linewidth = 0.75) +
  scale_x_discrete(expand = expansion(mult = c(0.05,0.05))) +
  scale_y_continuous(limits = c(-.25,.25), breaks = seq(-0.25, 0.25, .05),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Months before question", 
       y = "Standardized mean differences", 
       title = "") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

dev.off()

#Covariate-Balanced Propensity Score
PM.balance.cbps <- PM.balance.cbps %>%
  rownames_to_column(., var = "time") %>%
  gather(., key = "variable", value = "value",-time)

PM.balance.cbps$time <- factor(PM.balance.cbps$time,
                               levels = c("t_2","t_1","t_0"),
                               labels = c("t-2","t-1","t"))

PM.balance.cbps$variable <- case_match(PM.balance.none$variable,
                                       "y" ~ "Clinics per 100 square km",
                                       "u5mr_smooth" ~ "Infant mortality",
                                       "roads_outcome" ~ "Road per square kilometer",
                                       "diff" ~ "Diff. in vote share",
                                       "opp" ~ "Opposition",
                                       .default = PM.balance.none$variable)

cov.balance.cbps <- ggplot(data = PM.balance.cbps, 
                           mapping = aes(x = time,
                                         y = value,
                                         color = variable,
                                         group = variable))

pdf(file = file.path(project_dir, "Output", "Clinics12_CovBalCBPS.pdf"))

cov.balance.cbps +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  scale_fill_brewer(palette = "Spectral",
                    aesthetics = c("colour","fill")) +
  geom_hline(yintercept = 0,
             linetype = "longdash",
             linewidth = 0.75) +
  scale_x_discrete(expand = expansion(mult = c(0.05,0.05))) +
  scale_y_continuous(limits = c(-.25,.25), breaks = seq(-0.25, 0.25, .05),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Months before question", 
       y = "Standardized mean differences", 
       title = "") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

dev.off()

#Propensity Score Weighting
PM.balance.psw <- PM.balance.psw %>%
  rownames_to_column(., var = "time") %>%
  gather(., key = "variable", value = "value",-time)

PM.balance.psw$time <- factor(PM.balance.psw$time,
                              levels = c("t_2","t_1","t_0"),
                              labels = c("t-2","t-1","t"))

PM.balance.psw$variable <- case_match(PM.balance.none$variable,
                                      "y" ~ "Clinics per 100 square km",
                                      "u5mr_smooth" ~ "Infant mortality",
                                      "roads_outcome" ~ "Road per square kilometer",
                                      "diff" ~ "Diff. in vote share",
                                      "opp" ~ "Opposition",
                                      .default = PM.balance.none$variable)

cov.balance.psw <- ggplot(data = PM.balance.psw, 
                          mapping = aes(x = time,
                                        y = value,
                                        color = variable,
                                        group = variable))

pdf(file = file.path(project_dir, "Output", "Clinics12_CovBalPSw.pdf"))

cov.balance.psw +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  scale_fill_brewer(palette = "Spectral",
                    aesthetics = c("colour","fill")) +
  geom_hline(yintercept = 0,
             linetype = "longdash",
             linewidth = 0.75) +
  scale_x_discrete(expand = expansion(mult = c(0.05,0.05))) +
  scale_y_continuous(limits = c(-.25,.25), breaks = seq(-0.25, 0.25, .05),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Months before question", 
       y = "Standardized mean differences", 
       title = "") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

dev.off()

#Covariate-Balanced Propensity Score Weighting
PM.balance.cbpsw <- PM.balance.cbpsw %>%
  rownames_to_column(., var = "time") %>%
  gather(., key = "variable", value = "value",-time)

PM.balance.cbpsw$time <- factor(PM.balance.cbpsw$time,
                                levels = c("t_2","t_1","t_0"),
                                labels = c("t-2","t-1","t"))

PM.balance.cbpsw$variable <- case_match(PM.balance.none$variable,
                                        "y" ~ "Clinics per 100 square km",
                                        "u5mr_smooth" ~ "Infant mortality",
                                        "roads_outcome" ~ "Road per square kilometer",
                                        "diff" ~ "Diff. in vote share",
                                        "opp" ~ "Opposition",
                                        .default = PM.balance.none$variable)

cov.balance.cbpsw <- ggplot(data = PM.balance.cbpsw, 
                            mapping = aes(x = time,
                                          y = value,
                                          color = variable,
                                          group = variable))

pdf(file = file.path(project_dir, "Output", "Clinics12_CovBalCBPSw.pdf"))

cov.balance.cbpsw +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  scale_fill_brewer(palette = "Spectral",
                    aesthetics = c("colour","fill")) +
  geom_hline(yintercept = 0,
             linetype = "longdash",
             linewidth = 0.75) +
  scale_x_discrete(expand = expansion(mult = c(0.05,0.05))) +
  scale_y_continuous(limits = c(-.25,.25), breaks = seq(-0.25, 0.25, .05),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Months before question", 
       y = "Standardized mean differences", 
       title = "") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

dev.off()

#Matched Set Sizes
msets <- PM.maha$att
set_sizes <- sapply(msets, length)
matched_sizes <- data.frame(treated_set = seq_along(set_sizes),
                            size = set_sizes)

p.msizes <- ggplot(data = matched_sizes,
                   mapping = aes(x = size))

pdf(file = file.path(project_dir, "Output", "Clinics12_MatchedSizes.pdf"))

p.msizes +
  geom_histogram(binwidth = 1,
                 fill = "black",
                 color = "white",
                 alpha = 0.9) +
  geom_vline(aes(xintercept = mean(size)),
             linetype = "dashed",
             linewidth = 1,
             color = "darkgray") +
  annotate("text",
           x = mean(matched_sizes$size),
           y = Inf,
           label = paste0(
             "Mean = ",
             round(mean(matched_sizes$size), 2)),
           vjust = 4.5,
           color = "darkgray") +
  scale_x_continuous(limits = c(0,180),
                     breaks = seq(0, max(matched_sizes$size), 10),
                     expand = expansion(mult = c(0.0125, 0.0125))) +
  scale_y_continuous(limits = c(0,2),
                     breaks = seq(0,2,1),
                     expand = expansion(mult = c(0.0125, 0))) +
  labs(x = "Number of matched controls",
       y = "Frequency",
       title = "") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

dev.off()

#Pooled ATT estimate: Mahalanobis
PM.results.pool <- PanelEstimate(PM.maha,
                                 panel.data = CLINICS.PANEL,
                                 pooled = TRUE)

summary(PM.results.pool)

#Dynamic ATTs
PM.results.dyn <- PanelEstimate(PM.maha,
                                panel.data = CLINICS.PANEL,
                                pooled = FALSE)

PM.maha.plcb <- PanelMatch(panel.data = CLINICS.PANEL,
                           lag = 2,
                           refinement.method = "mahalanobis",
                           size.match = 5, #Only relevant for matching methods
                           use.diagonal.variance.matrix = TRUE, #Only relevant for Mahalanobis
                           match.missing = FALSE,
                           covs.formula = ~ I(lag(y, 1:2)) +
                             I(lag(roads_outcome, 0:2)) +
                             u5mr_smooth +
                             diff,
                           exact.match.variables = c("opp"),
                           forbid.treatment.reversal = FALSE,
                           qoi = "att",
                           lead = 0:11,
                           placebo.test = TRUE)

PM.results.plcb <- placebo_test(PM.maha.plcb,
                                panel.data = CLINICS.PANEL,
                                plot = FALSE)

PM.maha.est_lead <- as.vector(PM.results.dyn$estimate)
PM.maha.est_lag <- as.vector(PM.results.plcb$estimates)
PM.maha.sd_lead <- apply(PM.results.dyn$bootstrapped.estimates,2,sd)
PM.maha.sd_lag <- apply(PM.results.plcb$bootstrapped.estimates,2,sd)
PM.maha.coef <- c(PM.maha.est_lag, 0, PM.maha.est_lead)
PM.maha.sd <- c(PM.maha.sd_lag, 0, PM.maha.sd_lead)
PM.maha.output <- cbind.data.frame(ATT = PM.maha.coef,
                                    se = PM.maha.sd,
                                    t = c(-1:12))

# Event study plot
PM.maha.output <- PM.maha.output %>%
  dplyr::mutate(lb = ATT - 1.96 * se) %>%
  dplyr::mutate(ub = ATT + 1.96 * se)

p.PM.maha <- ggplot(data = PM.maha.output,
                    mapping = aes(x = t,
                                  y = ATT))

pdf(file = file.path(project_dir, "Output", "Clinics12_ESPlotMaha.pdf"))

p.PM.maha +
  geom_hline(yintercept = 0, colour = "gray50", linewidth = 1, linetype = "dashed") +
  geom_point(size = 1.5) +
  geom_errorbar(mapping = aes(ymin = lb, 
                              ymax = ub, 
                              width = 0)) +
  scale_x_continuous(limits = c(-1,12), breaks = seq(-1,12,1)) +
  scale_y_continuous(limits = c(-2,2), breaks = seq(-2,2,.5)) +
  labs(x = "Months relative to treatment",
       y = "Clinics per 100 square kilometers") +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

########################
## Diagnostics Table  ##
########################

orig_covars <- c("y","u5mr_smooth","roads_outcome","opp")
rev_covars <- c("y","u5mr_smooth","roads_outcome","opp")

orig_label <- "Original"
rev_label <- "Revised"

# Helper function
extract_pm_diagnostics <- function(pm_obj,
                                   method_name,
                                   panel.data,
                                   covariates,
                                   #covariate_set,
                                   lagged_y_pattern = "Clinics per square km") {
  
  # Extract balance table
  bal <- get_covariate_balance(pm_obj,
                               panel.data = panel.data,
                               covariates = covariates)
  
  # Convert to data frame
  bal_df <- as.data.frame(bal)
  
  # Standardized mean differences
  # Adjust column name if necessary, usually "std.eff.sz"
  smd <- abs(as.matrix(bal_df))
  max_smd  <- max(smd, na.rm = TRUE)
  mean_smd <- mean(smd, na.rm = TRUE)
  
  # Lagged outcome balance
  lagged_y_balance <- max(abs(bal_df$att.y))
  
  # Donor pool size
  # (1) Matched sets
  matched_sets <- pm_obj$att
  donor_sizes <- sapply(matched_sets, length)
  min_donors <- min(donor_sizes, na.rm = TRUE)
  avg_donors <- mean(donor_sizes, na.rm = TRUE)
  
  # (2) Return summary row
  tibble(Method = method_name,
         `Max |SMD|` = round(max_smd, 3),
         `Mean |SMD|` = round(mean_smd, 3),
         #`Covariate Set` = covariate_set,
         `Lagged Y Balance` = round(lagged_y_balance, 3),
         `Min Donor Pool` = min_donors,
         `Avg. Donor Pool` = round(avg_donors, 2))
}

# Build diagnostics table
diag_table <- bind_rows(extract_pm_diagnostics(PM.maha,  "Mahalanobis",
                                               panel.data = CLINICS.PANEL,
                                               covariates = orig_covars),
                                               #covariate_set = orig_label),
                        extract_pm_diagnostics(PM.ps, "PS Matching",
                                               panel.data = CLINICS.PANEL,
                                               covariates = orig_covars),
                                               #covariate_set = orig_label),
                        extract_pm_diagnostics(PM.psw, "PS Weighting",
                                               panel.data = CLINICS.PANEL,
                                               covariates = orig_covars),
                                               #covariate_set = orig_label),
                        extract_pm_diagnostics(PM.cbps, "CBPS Matching",
                                               panel.data = CLINICS.PANEL,
                                               covariates = orig_covars),
                                               #covariate_set = orig_label),
                        extract_pm_diagnostics(PM.cbpsw, "CBPS Weighting",
                                               panel.data = CLINICS.PANEL,
                                               covariates = orig_covars))
                                               #covariate_set = orig_label))

diag_table <- diag_table %>%
  dplyr::select(Method,
                #`Covariate Set`,
                `Lagged Y Balance`,
                `Max |SMD|`,
                `Mean |SMD|`,
                `Min Donor Pool`,
                `Avg. Donor Pool`)

# Print LaTeX table
diag_table %>%
  kbl(format = "latex",
      booktabs = TRUE,
      caption = "Balance Diagnostics Across Refinement Methods",
      align = "lcccc") %>%
  kable_styling(latex_options = c("hold_position"))

######################
##  Size.match = 10 ##
######################

PM.maha10 <- PanelMatch(panel.data = CLINICS.PANEL,
                        lag = 2,
                        refinement.method = "mahalanobis",
                        size.match = 10, #Only relevant for matching methods
                        use.diagonal.variance.matrix = TRUE, #Only relevant for Mahalanobis
                        match.missing = FALSE,
                        covs.formula = ~ I(lag(y, 1:2)) +
                          I(lag(roads_outcome, 0:2)) +
                          u5mr_smooth +
                          diff,
                        exact.match.variables = c("opp"),
                        forbid.treatment.reversal = FALSE,
                        qoi = "att",
                        lead = 0:11)

#Pooled ATT estimate: Mahalanobis
PM.results.pool <- PanelEstimate(PM.maha10,
                                 panel.data = CLINICS.PANEL,
                                 pooled = TRUE)

summary(PM.results.pool)

#Dynamic ATTs
PM.results.dyn <- PanelEstimate(PM.maha10,
                                panel.data = CLINICS.PANEL,
                                pooled = FALSE)

PM.maha10.plcb <- PanelMatch(panel.data = CLINICS.PANEL,
                             lag = 2,
                             refinement.method = "mahalanobis",
                             size.match = 10, #Only relevant for matching methods
                             use.diagonal.variance.matrix = TRUE, #Only relevant for Mahalanobis
                             match.missing = FALSE,
                             covs.formula = ~ I(lag(y, 1:2)) +
                               I(lag(roads_outcome, 0:2)) +
                               u5mr_smooth +
                               diff,
                             exact.match.variables = c("opp"),
                             forbid.treatment.reversal = FALSE,
                             qoi = "att",
                             lead = 0:11,
                             placebo.test = TRUE)

PM.results.plcb <- placebo_test(PM.maha10.plcb,
                                panel.data = CLINICS.PANEL,
                                plot = FALSE)

PM.maha10.est_lead <- as.vector(PM.results.dyn$estimate)
PM.maha10.est_lag <- as.vector(PM.results.plcb$estimates)
PM.maha10.sd_lead <- apply(PM.results.dyn$bootstrapped.estimates,2,sd)
PM.maha10.sd_lag <- apply(PM.results.plcb$bootstrapped.estimates,2,sd)
PM.maha10.coef <- c(PM.maha10.est_lag, 0, PM.maha10.est_lead)
PM.maha10.sd <- c(PM.maha10.sd_lag, 0, PM.maha10.sd_lead)
PM.maha10.output <- cbind.data.frame(ATT = PM.maha10.coef,
                                      se = PM.maha10.sd,
                                      t = c(-1:12))

# Event study plot
PM.maha10.output <- PM.maha10.output %>%
  dplyr::mutate(lb = ATT - 1.96 * se) %>%
  dplyr::mutate(ub = ATT + 1.96 * se)

p.PM.maha10 <- ggplot(data = PM.maha10.output,
                      mapping = aes(x = t,
                                    y = ATT))

pdf(file = file.path(project_dir, "Output", "Clinics12_ESPlotMaha10.pdf"))

p.PM.maha10 +
  geom_hline(yintercept = 0, colour = "gray50", linewidth = 1, linetype = "dashed") +
  geom_point(size = 1.5) +
  geom_errorbar(mapping = aes(ymin = lb, 
                              ymax = ub, 
                              width = 0)) +
  scale_x_continuous(limits = c(-1,12), breaks = seq(-1,12,1)) +
  scale_y_continuous(limits = c(-2,2), breaks = seq(-2,2,.5)) +
  labs(x = "Months relative to treatment",
       y = "Clinics per 100 square kilometers") +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

###############
##  Lags = 4 ##
###############

PM.mahaL4 <- PanelMatch(panel.data = CLINICS.PANEL,
                        lag = 4,
                        refinement.method = "mahalanobis",
                        size.match = 5, #Only relevant for matching methods
                        use.diagonal.variance.matrix = TRUE, #Only relevant for Mahalanobis
                        match.missing = FALSE,
                        covs.formula = ~ I(lag(y, 1:4)) +
                          I(lag(roads_outcome, 0:4)) +
                          u5mr_smooth +
                          diff,
                        exact.match.variables = c("opp"),
                        forbid.treatment.reversal = FALSE,
                        qoi = "att",
                        lead = 0:11)

#Pooled ATT estimate: Mahalanobis
PM.results.pool <- PanelEstimate(PM.mahaL4,
                                 panel.data = CLINICS.PANEL,
                                 pooled = TRUE)

summary(PM.results.pool)

#Dynamic ATTs
PM.results.dyn <- PanelEstimate(PM.mahaL4,
                                panel.data = CLINICS.PANEL,
                                pooled = FALSE)

PM.mahaL4.plcb <- PanelMatch(panel.data = CLINICS.PANEL,
                             lag = 4,
                             refinement.method = "mahalanobis",
                             size.match = 5, #Only relevant for matching methods
                             use.diagonal.variance.matrix = TRUE, #Only relevant for Mahalanobis
                             match.missing = FALSE,
                             covs.formula = ~ I(lag(y, 1:4)) +
                               I(lag(roads_outcome, 0:4)) +
                               u5mr_smooth +
                               diff,
                             exact.match.variables = c("opp"),
                             forbid.treatment.reversal = FALSE,
                             qoi = "att",
                             lead = 0:11,
                             placebo.test = TRUE)

PM.results.plcb <- placebo_test(PM.mahaL4.plcb,
                                panel.data = CLINICS.PANEL,
                                plot = FALSE)

PM.mahaL4.est_lead <- as.vector(PM.results.dyn$estimate)
PM.mahaL4.est_lag <- as.vector(PM.results.plcb$estimates)
PM.mahaL4.sd_lead <- apply(PM.results.dyn$bootstrapped.estimates,2,sd)
PM.mahaL4.sd_lag <- apply(PM.results.plcb$bootstrapped.estimates,2,sd)
PM.mahaL4.coef <- c(PM.mahaL4.est_lag, 0, PM.mahaL4.est_lead)
PM.mahaL4.sd <- c(PM.mahaL4.sd_lag, 0, PM.mahaL4.sd_lead)
PM.mahaL4.output <- cbind.data.frame(ATT = PM.mahaL4.coef,
                                      se = PM.mahaL4.sd,
                                      t = c(-3:12))

# Event study plot
PM.mahaL4.output <- PM.mahaL4.output %>%
  dplyr::mutate(lb = ATT - 1.96 * se) %>%
  dplyr::mutate(ub = ATT + 1.96 * se)

p.PM.mahaL4 <- ggplot(data = PM.mahaL4.output,
                       mapping = aes(x = t,
                                     y = ATT))

pdf(file = file.path(project_dir, "Output", "Clinics12_ESPlotMahaL4.pdf"))

p.PM.mahaL4 +
  geom_hline(yintercept = 0, colour = "gray50", linewidth = 1, linetype = "dashed") +
  geom_point(size = 1.5) +
  geom_errorbar(mapping = aes(ymin = lb, 
                              ymax = ub, 
                              width = 0)) +
  scale_x_continuous(limits = c(-3,12), breaks = seq(-3,12,1)) +
  scale_y_continuous(limits = c(-2,2), breaks = seq(-2,2,.5)) +
  labs(x = "Months relative to treatment",
       y = "Boreholes per 100 square kilometers") +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

##################################
##  Simple Mahalanobis Matching ##
##################################

SIMPLE <- CONST %>%
  group_by(constID) %>%
  summarise(treatO = as.integer(any(treatO == 1)),
            treat = as.integer(any(treat == 1)),
            y_2015 = clinics_outcome[month == as.Date("2015-09-15")],
            y_2023 = clinics_outcome[month == as.Date("2023-08-15")],
            delta_y = y_2023 - y_2015,
            u5mr_smooth = first(u5mr_smooth),
            u5mr_imputed = first(u5mr_imputed),
            u5mr = first(u5mr),
            diff = first(diff),
            roads = first(roads_outcome),
            opp = first(opp)) %>%
  ungroup()

m.out <- matchit(treatO ~ y_2015 +
                   u5mr_smooth +
                   diff +
                   roads,
                 data = SIMPLE,
                 method = "nearest",
                 distance = "mahalanobis",
                 exact = "opp",
                 ratio = 1)

summary(m.out)
plot(m.out, type = "qq")

table(SIMPLE$treatO,
      SIMPLE$opp)

opp_data <- subset(SIMPLE, opp == 1)

summary(opp_data[, c("roads",
                     "diff",
                     "u5mr_smooth",
                     "y_2015")])

aggregate(cbind(roads,
                diff,
                u5mr_smooth,
                y_2015) ~ treatO,
          data = opp_data,
          mean)

bal.tab(m.out,
        un = TRUE,
        m.threshold = .20)

love.plot(m.out,
          abs = TRUE,
          threshold = .20)

matched_data <- match.data(m.out)
table(matched_data$treatO)

bal <- bal.tab(m.out,
               un = TRUE,
               disp.means = FALSE)

balance_table <- data.frame(Variable = rownames(bal$Balance),
                            SMD_Unmatched = bal$Balance$Diff.Un,
                            SMD_Matched   = bal$Balance$Diff.Adj)

balance_table <- balance_table %>%
  mutate(Abs_SMD_Unmatched = abs(SMD_Unmatched),
         Abs_SMD_Matched   = abs(SMD_Matched))

balance_table <- balance_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

balance_table <- balance_table %>%
  mutate(
    Assessment = case_when(
      Abs_SMD_Matched < .10 ~ "Excellent",
      Abs_SMD_Matched < .15 ~ "Good",
      Abs_SMD_Matched < .20 ~ "Acceptable",
      TRUE ~ "Concern"
    )
  )

latex_bal <- balance_table %>%
  select(
    Variable,
    Abs_SMD_Unmatched,
    Abs_SMD_Matched,
    Assessment
  ) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Absolute Standardized Mean Differences Before and After Matching",
    col.names = c(
      "Variable",
      "|SMD| Before",
      "|SMD| After",
      "Assessment"
    )
  )

latex_bal

#Additional Diagnostics on Outcome at Base
bal.tab(
  m.out,
  un = TRUE,
  stats = c("mean.diffs", "variance.ratios")
)

love.plot(
  m.out,
  stats = c("mean.diffs", "variance.ratios")
)

matched_data %>%
  group_by(treatO) %>%
  summarise(
    n = n(),
    mean = mean(y_2015),
    sd = sd(y_2015),
    min = min(y_2015),
    max = max(y_2015)
  )

ggplot(matched_data,
       aes(x = y_2015,
           fill = factor(treatO))) +
  geom_density(alpha = .4)

## Full OLS Model
ols_full <- lm(delta_y ~
                 treatO +
                 y_2015 +
                 u5mr_smooth +
                 diff +
                 roads +
                 opp,
               data = SIMPLE)

coeftest(ols_full,
         vcov = vcovHC(ols_full, type = "HC3"))

## Difference in Means after Mahalanobis Matching
att_match <- lm(delta_y ~ treatO,
                data = matched_data,
                weights = weights)

coeftest(att_match,
         vcov = vcovHC(att_match, type = "HC3"))

## Doubly Adjusted Model
att_adjusted <- lm(delta_y ~
                     treatO +
                     y_2015 +
                     u5mr_smooth +
                     diff +
                     roads,
                   data = matched_data,
                   weights = weights)

coeftest(att_adjusted,
         vcov = vcovHC(att_adjusted, type = "HC3"))

## Ancova
ancova <- lm(y_2023 ~
               treatO +
               y_2015 +
               u5mr_smooth +
               diff +
               roads +
               opp,
             data = SIMPLE)

coeftest(ancova,
         vcov = vcovHC(ancova, type = "HC3"))

## Summary
modelsummary(
  list("OLS Full Sample" = ols_full,
       "Matched ATT" = att_match,
       "Matched + Adjustment" = att_adjusted,
       "Ancova" = ancova),
  vcov = list(vcovHC(ols_full, type = "HC3"),
              vcovHC(att_match, type = "HC3"),
              vcovHC(att_adjusted, type = "HC3"),
              vcovHC(ancova, type = "HC3")),
  estimate  = "{estimate} [{conf.low}, {conf.high}]",
  conf_level = 0.95,
  statistic = NULL,
  stars = FALSE,
  fmt = fmt_decimal(digits = 2),
  output = "latex")

##Coefficient Plots
models <- list("OLS Full Sample"      = ols_full,
               "Matched ATT"          = att_match,
               "Matched + Adjustment" = att_adjusted,
               "ANCOVA"               = ancova)

vcovs <- list(vcovHC(ols_full, type = "HC3"),
              vcovHC(att_match, type = "HC3"),
              vcovHC(att_adjusted, type = "HC3"),
              vcovHC(ancova, type = "HC3"))

coef_df <- map2_dfr(
  models,
  vcovs,
  ~ get_estimates(
    .x,
    vcov = .y,
    conf_level = .95
  ),
  .id = "Model"
)

# Plot only treatment effect across specifications
treat_plot <- coef_df %>%
  filter(term == "treatO") %>%
  mutate(Model = factor(Model,
                        levels = c("OLS Full Sample",
                                   "Matched ATT",
                                   "Matched + Adjustment",
                                   "ANCOVA")))

p.treatplot <- ggplot(treat_plot,
                      aes(x = estimate,
                          y = Model))

pdf(file = file.path(project_dir, "Output", "Clinics_SimpleTreatPlot.pdf"))

p.treatplot +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "grey50") +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high),
                 height = .15,
                 linewidth = .8) +
  geom_point(size = 3,
             shape = 21,
             fill = "black") +
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,2.5)) +
  labs(x = "Estimated effect of service request",
       y = NULL) +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#Full Coefficient Plot across Specifications
plot_df <- coef_df %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(
      term,
      treatO      = "Service request",
      y_2015      = "Clinics (2015)",
      u5mr_smooth = "Infant mortality",
      diff        = "Diff. in vote shares",
      roads       = "Road density",
      opp         = "Opposition"))

p.coefplot <- ggplot(plot_df,
                     aes(x = estimate,
                         y = term,
                         colour = Model))

pdf(file = file.path(project_dir, "Output", "Clinics_SimpleCoefPlot.pdf"))

p.coefplot +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "grey50") +
  geom_point(position = position_dodge(width = .6),
             size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high),
                 position = position_dodge(width = .6),
                 height = .15) +
  scale_x_continuous(limits = c(-20,20), breaks = seq(-20,20,5)) +
  labs(x = "Coefficient estimate",
       y = NULL,
       colour = NULL) +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#Attempt to Improve Balance: Optimal Mahalanobis
#(potentially need to load package optmatch)
m.out.opt <- matchit(treatO ~ y_2015 +
                       u5mr_smooth +
                       diff +
                       roads,
                     data = SIMPLE,
                     method = "optimal",
                     distance = "mahalanobis",
                     exact = "opp")

bal <- bal.tab(m.out.opt,
               un = TRUE,
               disp.means = FALSE)

balance_table <- data.frame(Variable = rownames(bal$Balance),
                            SMD_Unmatched = bal$Balance$Diff.Un,
                            SMD_Matched   = bal$Balance$Diff.Adj)

balance_table <- balance_table %>%
  mutate(Abs_SMD_Unmatched = abs(SMD_Unmatched),
         Abs_SMD_Matched   = abs(SMD_Matched))

balance_table <- balance_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

balance_table <- balance_table %>%
  mutate(
    Assessment = case_when(
      Abs_SMD_Matched < .10 ~ "Excellent",
      Abs_SMD_Matched < .15 ~ "Good",
      Abs_SMD_Matched < .20 ~ "Acceptable",
      TRUE ~ "Concern"
    )
  )

latex_bal_opt <- balance_table %>%
  select(
    Variable,
    Abs_SMD_Unmatched,
    Abs_SMD_Matched,
    Assessment
  ) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Absolute Standardized Mean Differences Before and After Matching",
    col.names = c(
      "Variable",
      "|SMD| Before",
      "|SMD| After",
      "Assessment"
    )
  )

latex_bal_opt

#Attempt to Improve Balance: Nearest Mahalanobis w/ Replacement
m.out.rep <- matchit(treatO ~ y_2015 +
                       u5mr_smooth +
                       diff +
                       roads,
                     data = SIMPLE,
                     method = "nearest",
                     replace = TRUE,
                     distance = "mahalanobis",
                     exact = "opp")

bal <- bal.tab(m.out.rep,
               un = TRUE,
               disp.means = FALSE)

balance_table <- data.frame(Variable = rownames(bal$Balance),
                            SMD_Unmatched = bal$Balance$Diff.Un,
                            SMD_Matched   = bal$Balance$Diff.Adj)

balance_table <- balance_table %>%
  mutate(Abs_SMD_Unmatched = abs(SMD_Unmatched),
         Abs_SMD_Matched   = abs(SMD_Matched))

balance_table <- balance_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

balance_table <- balance_table %>%
  mutate(
    Assessment = case_when(
      Abs_SMD_Matched < .10 ~ "Excellent",
      Abs_SMD_Matched < .15 ~ "Good",
      Abs_SMD_Matched < .20 ~ "Acceptable",
      TRUE ~ "Concern"
    )
  )

latex_bal_rep <- balance_table %>%
  select(
    Variable,
    Abs_SMD_Unmatched,
    Abs_SMD_Matched,
    Assessment
  ) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Absolute Standardized Mean Differences Before and After Matching",
    col.names = c(
      "Variable",
      "|SMD| Before",
      "|SMD| After",
      "Assessment"
    )
  )

latex_bal_rep

##############################
##  Liberal research design ##
##############################

is.pbalanced(CONST$constID,CONST$time)
is.pbalanced(CONST$constID,CONST$month)
is.pbalanced(CONST)

CONST <- make.pbalanced(CONST,
                        balance.type = c("fill"), 
                        index = c("constID","time"))

is.integer(CONST$constID)
is.integer(CONST$time)
is.integer(CONST$treat)
is.numeric(CONST$y)

CONST$constID <- as.integer(CONST$constID)
CONST$time <- as.integer(CONST$time)
CONST$treat <- as.integer(CONST$treat)

CLINICS.PANEL <- PanelData(panel.data = CONST,
                         unit.id = "constID", #must be integer
                         time.id = "time", #must be integer
                         treatment = "treat", #must be integer
                         outcome = "y") #must be numeric

#Mahalanobis
is.integer(CLINICS.PANEL$constID)
is.integer(CLINICS.PANEL$time)
is.integer(CLINICS.PANEL$treat)
is.numeric(CLINICS.PANEL$y)

CLINICS.PANEL <- PanelData(panel.data = CLINICS.PANEL,
                         unit.id = "constID", #must be integer
                         time.id = "time", #must be integer
                         treatment = "treat", #must be integer
                         outcome = "y") #must be numeric

PM.all.maha <- PanelMatch(panel.data = CLINICS.PANEL,
                          lag = 2,
                          refinement.method = "mahalanobis",
                          size.match = 5, #Only relevant for matching methods
                          use.diagonal.variance.matrix = TRUE, #Only relevant for Mahalanobis
                          match.missing = FALSE,
                          covs.formula = ~ I(lag(y, 1:2)) +
                            I(lag(roads_outcome, 0:2)) +
                            u5mr_smooth +
                            diff,
                          exact.match.variables = c("opp"),
                          forbid.treatment.reversal = FALSE,
                          qoi = "att",
                          lead = 0:11)

plot(PM.all.maha)
summary(PM.all.maha)

#Matched Set Sizes
msets <- PM.all.maha$att
set_sizes <- sapply(msets, length)
matched_sizes <- data.frame(treated_set = seq_along(set_sizes),
                            size = set_sizes)

p.all.msizes <- ggplot(data = matched_sizes,
                       mapping = aes(x = size))

pdf(file = file.path(project_dir, "Output", "Clinics12_All_MatchedSizes.pdf"))

p.all.msizes +
  geom_histogram(binwidth = 1,
                 fill = "black",
                 color = "white",
                 alpha = 0.9) +
  geom_vline(aes(xintercept = mean(size)),
             linetype = "dashed",
             linewidth = 1,
             color = "darkgray") +
  annotate("text",
           x = mean(matched_sizes$size),
           y = Inf,
           label = paste0(
             "Mean = ",
             round(mean(matched_sizes$size), 2)),
           vjust = 3,
           color = "darkgray") +
  scale_x_continuous(limits = c(0,180),
                     breaks = seq(0, max(matched_sizes$size), 10),
                     expand = expansion(mult = c(0.0125, 0.0125))) +
  scale_y_continuous(limits = c(0,4),
                     breaks = seq(0,4,1),
                     expand = expansion(mult = c(0.0125, 0))) +
  labs(x = "Number of matched controls",
       y = "Frequency",
       title = "") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

dev.off()

#Covariate balance (Mahalanobis Revised)
PM.covbal <- get_covariate_balance(PM.all.maha,
                                   panel.data = CLINICS.PANEL,
                                   covariates = c("y","roads_outcome","u5mr_smooth","diff","opp"),
                                   include.unrefined = TRUE) 

PM.balance.all.maha <-as.data.frame(PM.covbal[[1]]$att)

PM.balance.all.maha <- PM.balance.all.maha %>%
  rownames_to_column(., var = "time") %>%
  gather(., key = "variable", value = "value",-time)

PM.balance.all.maha$time <- factor(PM.balance.all.maha$time,
                                   levels = c("t_2","t_1","t_0"),
                                   labels = c("t-2","t-1","t"))

PM.balance.all.maha$variable <- case_match(PM.balance.all.maha$variable,
                                           "y" ~ "Clinics per 100 square km",
                                           "u5mr_smooth" ~ "Infant mortality",
                                           "roads_outcome" ~ "Road per square kilometer",
                                           "diff" ~ "Diff. in vote share",
                                           "opp" ~ "Opposition",
                                           .default = PM.balance.all.maha$variable)

cov.balance.all.maha <- ggplot(data = PM.balance.all.maha, 
                               mapping = aes(x = time,
                                             y = value,
                                             color = variable,
                                             group = variable))

pdf(file = file.path(project_dir, "Output", "Clinics12_All_CovBalMaha.pdf"))

cov.balance.maha +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  scale_fill_brewer(palette = "Spectral",
                    aesthetics = c("colour","fill")) +
  geom_hline(yintercept = 0,
             linetype = "longdash",
             linewidth = 0.75) +
  scale_x_discrete(expand = expansion(mult = c(0.05,0.05))) +
  scale_y_continuous(limits = c(-.25,.25), breaks = seq(-0.25, 0.25, .05),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Months before question", 
       y = "Standardized mean differences", 
       title = "") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

dev.off()

#Pooled ATT estimate: Mahalanobis
PM.results.pool <- PanelEstimate(PM.all.maha,
                                 panel.data = CLINICS.PANEL,
                                 pooled = TRUE)

summary(PM.results.pool)

#Dynamic ATTs
PM.results.dyn <- PanelEstimate(PM.all.maha,
                                panel.data = CLINICS.PANEL,
                                pooled = FALSE)

PM.all.maha.plcb <- PanelMatch(panel.data = CLINICS.PANEL,
                               lag = 2,
                               refinement.method = "mahalanobis",
                               size.match = 5, #Only relevant for matching methods
                               use.diagonal.variance.matrix = TRUE, #Only relevant for Mahalanobis
                               match.missing = FALSE,
                               covs.formula = ~ I(lag(y, 1:2)) +
                                 I(lag(roads_outcome, 0:2)) +
                                 u5mr_smooth +
                                 diff,
                               exact.match.variables = c("opp"),
                               forbid.treatment.reversal = FALSE,
                               qoi = "att",
                               lead = 0:11,
                               placebo.test = TRUE)

PM.results.plcb <- placebo_test(PM.all.maha.plcb,
                                panel.data = CLINICS.PANEL,
                                plot = FALSE)

PM.all.maha.est_lead <- as.vector(PM.results.dyn$estimate)
PM.all.maha.est_lag <- as.vector(PM.results.plcb$estimates)
PM.all.maha.sd_lead <- apply(PM.results.dyn$bootstrapped.estimates,2,sd)
PM.all.maha.sd_lag <- apply(PM.results.plcb$bootstrapped.estimates,2,sd)
PM.all.maha.coef <- c(PM.all.maha.est_lag, 0, PM.all.maha.est_lead)
PM.all.maha.sd <- c(PM.all.maha.sd_lag, 0, PM.all.maha.sd_lead)
PM.all.maha.output <- cbind.data.frame(ATT = PM.all.maha.coef,
                                        se = PM.all.maha.sd,
                                        t = c(-1:12))

# Event study plot
PM.all.maha.output <- PM.all.maha.output %>%
  dplyr::mutate(lb = ATT - 1.96 * se) %>%
  dplyr::mutate(ub = ATT + 1.96 * se)

p.PM.all.maha <- ggplot(data = PM.all.maha.output,
                        mapping = aes(x = t,
                                      y = ATT))

pdf(file = file.path(project_dir, "Output", "Clinics12_All_ESPlotMaha.pdf"))

p.PM.all.maha +
  geom_hline(yintercept = 0, colour = "gray50", linewidth = 1, linetype = "dashed") +
  geom_point(size = 1.5) +
  geom_errorbar(mapping = aes(ymin = lb, 
                              ymax = ub, 
                              width = 0)) +
  scale_x_continuous(limits = c(-1,12), breaks = seq(-1,12,1)) +
  scale_y_continuous(limits = c(-.6,.6), breaks = seq(-.6,.6,.3)) +
  labs(x = "Months relative to treatment",
       y = "Clinics per 100 square kilometers") +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

######################
##  Size.match = 10 ##
######################

PM.all.maha10 <- PanelMatch(panel.data = CLINICS.PANEL,
                            lag = 2,
                            refinement.method = "mahalanobis",
                            size.match = 10, #Only relevant for matching methods
                            use.diagonal.variance.matrix = TRUE, #Only relevant for Mahalanobis
                            match.missing = FALSE,
                            covs.formula = ~ I(lag(y, 1:2)) +
                              I(lag(roads_outcome, 0:2)) +
                              u5mr_smooth +
                              diff,
                            exact.match.variables = c("opp"),
                            forbid.treatment.reversal = FALSE,
                            qoi = "att",
                            lead = 0:11)

#Pooled ATT estimate: Mahalanobis
PM.results.pool <- PanelEstimate(PM.all.maha10,
                                 panel.data = CLINICS.PANEL,
                                 pooled = TRUE)

summary(PM.results.pool)

#Dynamic ATTs
PM.results.dyn <- PanelEstimate(PM.all.maha10,
                                panel.data = CLINICS.PANEL,
                                pooled = FALSE)

PM.all.maha10.plcb <- PanelMatch(panel.data = CLINICS.PANEL,
                                  lag = 2,
                                  refinement.method = "mahalanobis",
                                  size.match = 10, #Only relevant for matching methods
                                  use.diagonal.variance.matrix = TRUE, #Only relevant for Mahalanobis
                                  match.missing = FALSE,
                                  covs.formula = ~ I(lag(y, 1:2)) +
                                    I(lag(roads_outcome, 0:2)) +
                                    u5mr_smooth +
                                    diff,
                                  exact.match.variables = c("opp"),
                                  forbid.treatment.reversal = FALSE,
                                  qoi = "att",
                                  lead = 0:11,
                                  placebo.test = TRUE)

PM.results.plcb <- placebo_test(PM.all.maha10.plcb,
                                panel.data = CLINICS.PANEL,
                                plot = FALSE)

PM.all.maha10.est_lead <- as.vector(PM.results.dyn$estimate)
PM.all.maha10.est_lag <- as.vector(PM.results.plcb$estimates)
PM.all.maha10.sd_lead <- apply(PM.results.dyn$bootstrapped.estimates,2,sd)
PM.all.maha10.sd_lag <- apply(PM.results.plcb$bootstrapped.estimates,2,sd)
PM.all.maha10.coef <- c(PM.all.maha10.est_lag, 0, PM.all.maha10.est_lead)
PM.all.maha10.sd <- c(PM.all.maha10.sd_lag, 0, PM.all.maha10.sd_lead)
PM.all.maha10.output <- cbind.data.frame(ATT = PM.all.maha10.coef,
                                          se = PM.all.maha10.sd,
                                          t = c(-1:12))

# Event study plot
PM.all.maha10.output <- PM.all.maha10.output %>%
  dplyr::mutate(lb = ATT - 1.96 * se) %>%
  dplyr::mutate(ub = ATT + 1.96 * se)

p.PM.all.maha10 <- ggplot(data = PM.all.maha10.output,
                          mapping = aes(x = t,
                                        y = ATT))

pdf(file = file.path(project_dir, "Output", "Clinics12_All_ESPlotMaha10.pdf"))

p.PM.all.maha10 +
  geom_hline(yintercept = 0, colour = "gray50", linewidth = 1, linetype = "dashed") +
  geom_point(size = 1.5) +
  geom_errorbar(mapping = aes(ymin = lb, 
                              ymax = ub, 
                              width = 0)) +
  scale_x_continuous(limits = c(-1,12), breaks = seq(-1,12,1)) +
  scale_y_continuous(limits = c(-.6,.6), breaks = seq(-.6,.6,.3)) +
  labs(x = "Months relative to treatment",
       y = "Clinics per 100 square kilometers") +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

###############
##  Lags = 4 ##
###############

PM.all.mahaL4 <- PanelMatch(panel.data = CLINICS.PANEL,
                            lag = 4,
                            refinement.method = "mahalanobis",
                            size.match = 5, #Only relevant for matching methods
                            use.diagonal.variance.matrix = TRUE, #Only relevant for Mahalanobis
                            match.missing = FALSE,
                            covs.formula = ~ I(lag(y, 1:4)) +
                              I(lag(roads_outcome, 0:4)) +
                              u5mr_smooth +
                              diff,
                            exact.match.variables = c("opp"),
                            forbid.treatment.reversal = FALSE,
                            qoi = "att",
                            lead = 0:11)

#Pooled ATT estimate: Mahalanobis
PM.results.pool <- PanelEstimate(PM.all.mahaL4,
                                 panel.data = CLINICS.PANEL,
                                 pooled = TRUE)

summary(PM.results.pool)

#Dynamic ATTs
PM.results.dyn <- PanelEstimate(PM.all.mahaL4,
                                panel.data = CLINICS.PANEL,
                                pooled = FALSE)

PM.all.mahaL4.plcb <- PanelMatch(panel.data = CLINICS.PANEL,
                                 lag = 4,
                                 refinement.method = "mahalanobis",
                                 size.match = 5, #Only relevant for matching methods
                                 use.diagonal.variance.matrix = TRUE, #Only relevant for Mahalanobis
                                 match.missing = FALSE,
                                 covs.formula = ~ I(lag(y, 1:4)) +
                                   I(lag(roads_outcome, 0:4)) +
                                   u5mr_smooth +
                                   diff,
                                 exact.match.variables = c("opp"),
                                 forbid.treatment.reversal = FALSE,
                                 qoi = "att",
                                 lead = 0:11,
                                 placebo.test = TRUE)

PM.results.plcb <- placebo_test(PM.all.mahaL4.plcb,
                                panel.data = CLINICS.PANEL,
                                plot = FALSE)

PM.all.mahaL4.est_lead <- as.vector(PM.results.dyn$estimate)
PM.all.mahaL4.est_lag <- as.vector(PM.results.plcb$estimates)
PM.all.mahaL4.sd_lead <- apply(PM.results.dyn$bootstrapped.estimates,2,sd)
PM.all.mahaL4.sd_lag <- apply(PM.results.plcb$bootstrapped.estimates,2,sd)
PM.all.mahaL4.coef <- c(PM.all.mahaL4.est_lag, 0, PM.all.mahaL4.est_lead)
PM.all.mahaL4.sd <- c(PM.all.mahaL4.sd_lag, 0, PM.all.mahaL4.sd_lead)
PM.all.mahaL4.output <- cbind.data.frame(ATT = PM.all.mahaL4.coef,
                                          se = PM.all.mahaL4.sd,
                                          t = c(-3:12))

# Event study plot
PM.all.mahaL4.output <- PM.all.mahaL4.output %>%
  dplyr::mutate(lb = ATT - 1.96 * se) %>%
  dplyr::mutate(ub = ATT + 1.96 * se)

p.PM.all.mahaL4 <- ggplot(data = PM.all.mahaL4.output,
                          mapping = aes(x = t,
                                        y = ATT))

pdf(file = file.path(project_dir, "Output", "Clinics12_All_ESPlotMahaL4.pdf"))

p.PM.all.mahaL4 +
  geom_hline(yintercept = 0, colour = "gray50", linewidth = 1, linetype = "dashed") +
  geom_point(size = 1.5) +
  geom_errorbar(mapping = aes(ymin = lb, 
                              ymax = ub, 
                              width = 0)) +
  scale_x_continuous(limits = c(-3,12), breaks = seq(-3,12,1)) +
  scale_y_continuous(limits = c(-.6,.6), breaks = seq(-.6,.6,.3)) +
  labs(x = "Months relative to treatment",
       y = "Clinics per 100 square kilometers") +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#############################################
## Liberal RD: Simple Mahalanobis Matching ##
#############################################

m.all.out <- matchit(treat ~ y_2015 +
                       u5mr_smooth +
                       diff +
                       roads,
                     data = SIMPLE,
                     method = "nearest",
                     distance = "mahalanobis",
                     exact = "opp",
                     ratio = 1)

summary(m.all.out)
plot(m.all.out, type = "qq")

table(SIMPLE$treat,
      SIMPLE$opp)

opp_data <- subset(SIMPLE, opp == 1)

summary(opp_data[, c("u5mr_smooth",
                     "diff",
                     "roads")])

aggregate(cbind(u5mr_smooth,
                diff,
                roads) ~ treat,
          data = opp_data,
          mean)

bal.tab(m.all.out,
        un = TRUE,
        m.threshold = .20)

love.plot(m.all.out,
          abs = TRUE,
          threshold = .20)

matched_data <- match.data(m.all.out)
table(matched_data$treat)

bal <- bal.tab(m.all.out,
               un = TRUE,
               disp.means = FALSE)

balance_table <- data.frame(Variable = rownames(bal$Balance),
                            SMD_Unmatched = bal$Balance$Diff.Un,
                            SMD_Matched   = bal$Balance$Diff.Adj)

balance_table <- balance_table %>%
  mutate(Abs_SMD_Unmatched = abs(SMD_Unmatched),
         Abs_SMD_Matched   = abs(SMD_Matched))

balance_table <- balance_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

balance_table <- balance_table %>%
  mutate(
    Assessment = case_when(
      Abs_SMD_Matched < .10 ~ "Excellent",
      Abs_SMD_Matched < .15 ~ "Good",
      Abs_SMD_Matched < .20 ~ "Acceptable",
      TRUE ~ "Concern"
    )
  )

latex_all_bal <- balance_table %>%
  select(
    Variable,
    Abs_SMD_Unmatched,
    Abs_SMD_Matched,
    Assessment
  ) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Absolute Standardized Mean Differences Before and After Matching",
    col.names = c(
      "Variable",
      "|SMD| Before",
      "|SMD| After",
      "Assessment"
    )
  )

latex_all_bal

## Full OLS Model
ols_all_full <- lm(delta_y ~
                     treat +
                     y_2015 +
                     u5mr_smooth +
                     diff +
                     roads +
                     opp,
                   data = SIMPLE)

coeftest(ols_all_full,
         vcov = vcovHC(ols_all_full, type = "HC3"))

## Difference in Means after Mahalanobis Matching
att_all_match <- lm(delta_y ~ treat,
                    data = matched_data,
                    weights = weights)

coeftest(att_all_match,
         vcov = vcovHC(att_all_match, type = "HC3"))

## Doubly Adjusted Model
att_all_adjusted <- lm(delta_y ~
                         treat +
                         y_2015 +
                         u5mr_smooth +
                         diff +
                         roads,
                       data = matched_data,
                       weights = weights)

coeftest(att_all_adjusted,
         vcov = vcovHC(att_all_adjusted, type = "HC3"))

## Ancova
ancova_all <- lm(y_2023 ~
                   treat +
                   y_2015 +
                   u5mr_smooth +
                   diff +
                   roads +
                   opp,
                 data = SIMPLE)

coeftest(ancova_all,
         vcov = vcovHC(ancova_all, type = "HC3"))

## Summary
modelsummary(
  list("OLS Full Sample" = ols_all_full,
       "Matched ATT" = att_all_match,
       "Matched + Adjustment" = att_all_adjusted,
       "Ancova" = ancova_all),
  vcov = list(vcovHC(ols_all_full, type = "HC3"),
              vcovHC(att_all_match, type = "HC3"),
              vcovHC(att_all_adjusted, type = "HC3"),
              vcovHC(ancova_all, type = "HC3")),
  estimate = "{estimate} [{conf.low}, {conf.high}]",
  conf_level = 0.95,
  statistic = NULL,
  stars = FALSE,
  fmt = fmt_decimal(digits = 2),
  output = "latex")

##Coefficient Plots
models_all <- list("OLS Full Sample"      = ols_all_full,
                   "Matched ATT"          = att_all_match,
                   "Matched + Adjustment" = att_all_adjusted,
                   "ANCOVA"               = ancova_all)

vcovs_all <- list(vcovHC(ols_all_full, type = "HC3"),
                  vcovHC(att_all_match, type = "HC3"),
                  vcovHC(att_all_adjusted, type = "HC3"),
                  vcovHC(ancova_all, type = "HC3"))

coef_df <- map2_dfr(
  models_all,
  vcovs_all,
  ~ get_estimates(
    .x,
    vcov = .y,
    conf_level = .95
  ),
  .id = "Model"
)

# Plot only treatment effect across specifications
treat_plot <- coef_df %>%
  filter(term == "treatO") %>%
  mutate(Model = factor(Model,
                        levels = c("OLS Full Sample",
                                   "Matched ATT",
                                   "Matched + Adjustment",
                                   "ANCOVA")))

p.treatplot_all <- ggplot(treat_plot,
                          aes(x = estimate,
                              y = Model))

pdf(file = file.path(project_dir, "Output", "Clinics_All_SimpleTreatPlot.pdf"))

p.treatplot_all +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "grey50") +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high),
                 height = .15,
                 linewidth = .8) +
  geom_point(size = 3,
             shape = 21,
             fill = "black") +
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,2.5)) +
  labs(x = "Estimated effect of service request",
       y = NULL) +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#Full Coefficient Plot across Specifications
plot_df <- coef_df %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(
      term,
      treat       = "Service request",
      y_2015      = "Clinics (2015)",
      u5mr_smooth = "Infant mortality",
      diff        = "Diff. in vote shares",
      roads       = "Road density",
      opp         = "Opposition"))

p.coefplot_all <- ggplot(plot_df,
                         aes(x = estimate,
                             y = term,
                             colour = Model))

pdf(file = file.path(project_dir, "Output", "Clinics_All_SimpleCoefPlot.pdf"))

p.coefplot_all +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "grey50") +
  geom_point(position = position_dodge(width = .6),
             size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high),
                 position = position_dodge(width = .6),
                 height = .15) +
  scale_x_continuous(limits = c(-20,20), breaks = seq(-20,20,5)) +
  labs(x = "Coefficient estimate",
       y = NULL,
       colour = NULL) +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "gray90",
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "gray90"),
        axis.ticks = element_blank())

dev.off()

#Attempt to Improve Balance: Optimal Mahalanobis
#(potentially need to load package optmatch)
m.all.out.opt <- matchit(treat ~ y_2015 +
                           u5mr_smooth +
                           diff +
                           roads,
                         data = SIMPLE,
                         method = "optimal",
                         distance = "mahalanobis",
                         exact = "opp")

bal <- bal.tab(m.all.out.opt,
               un = TRUE,
               disp.means = FALSE)

balance_table <- data.frame(Variable = rownames(bal$Balance),
                            SMD_Unmatched = bal$Balance$Diff.Un,
                            SMD_Matched   = bal$Balance$Diff.Adj)

balance_table <- balance_table %>%
  mutate(Abs_SMD_Unmatched = abs(SMD_Unmatched),
         Abs_SMD_Matched   = abs(SMD_Matched))

balance_table <- balance_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

balance_table <- balance_table %>%
  mutate(
    Assessment = case_when(
      Abs_SMD_Matched < .10 ~ "Excellent",
      Abs_SMD_Matched < .15 ~ "Good",
      Abs_SMD_Matched < .20 ~ "Acceptable",
      TRUE ~ "Concern"
    )
  )

latex_all_bal_opt <- balance_table %>%
  select(
    Variable,
    Abs_SMD_Unmatched,
    Abs_SMD_Matched,
    Assessment
  ) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Absolute Standardized Mean Differences Before and After Matching",
    col.names = c(
      "Variable",
      "|SMD| Before",
      "|SMD| After",
      "Assessment"
    )
  )

latex_all_bal_opt

#Attempt to Improve Balance: Nearest Mahalanobis w/ Replacement
m.all.out.rep <- matchit(treat ~ y_2015 +
                           u5mr_smooth +
                           diff +
                           roads,
                         data = SIMPLE,
                         method = "nearest",
                         replace = TRUE,
                         distance = "mahalanobis",
                         exact = "opp")

bal <- bal.tab(m.all.out.rep,
               un = TRUE,
               disp.means = FALSE)

balance_table <- data.frame(Variable = rownames(bal$Balance),
                            SMD_Unmatched = bal$Balance$Diff.Un,
                            SMD_Matched   = bal$Balance$Diff.Adj)

balance_table <- balance_table %>%
  mutate(Abs_SMD_Unmatched = abs(SMD_Unmatched),
         Abs_SMD_Matched   = abs(SMD_Matched))

balance_table <- balance_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

balance_table <- balance_table %>%
  mutate(
    Assessment = case_when(
      Abs_SMD_Matched < .10 ~ "Excellent",
      Abs_SMD_Matched < .15 ~ "Good",
      Abs_SMD_Matched < .20 ~ "Acceptable",
      TRUE ~ "Concern"
    )
  )

latex_all_bal_rep <- balance_table %>%
  select(
    Variable,
    Abs_SMD_Unmatched,
    Abs_SMD_Matched,
    Assessment
  ) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Absolute Standardized Mean Differences Before and After Matching",
    col.names = c(
      "Variable",
      "|SMD| Before",
      "|SMD| After",
      "Assessment"
    )
  )

latex_all_bal_rep