library(ggplot2)
library(reshape2)
library(plyr)

setwd()


#######################
###### Figure 5 ######
#######################

#Read file with data
days_afa = read.table(file="figure5.csv",sep=",",header=TRUE)

#Subset columns with relevant data
days_afa<- days_afa[,c("sample","days_after_first_anthesis", "pollen_germination")]
days_afa$days_after_first_anthesis <- factor(days_afa$days_after_first_anthesis, levels = c("2","3","4"))

#Obtain average germination per sample (droplet)
days_afa.means <- aggregate(data = days_afa, pollen_germination ~ 
                              sample+days_after_first_anthesis, FUN = function(i)mean(i))

#Graph
figure5<-ggplot(days_afa.means,aes(x=days_after_first_anthesis,y=pollen_germination)) +
  geom_boxplot(aes(fill=days_after_first_anthesis)) + 
  geom_point(color="black", pch=21, size = 2, position=position_dodge(width = 0.65)) + 
  theme_bw(base_size = 14) + 
  labs(y = "Pollen germination (%)", x ="Days After First Anthesis") +
  theme(axis.text.x = element_text(size = 20,face = "bold"), axis.text.y = element_text(size = 20,face = "bold"),
        axis.title.x=element_text(size="24",face="bold"),
        axis.title.y=element_text(size="24",face="bold"),  
        strip.text.x = element_text(size = "24", face = "bold"),
        plot.title = element_text(size = "28", face = "bold"),
        legend.position = "none")+
  scale_y_continuous(limits = c(20,80), breaks = seq(20, 80, by = 20)) 

figure5
ggsave("figure5.pdf",plot = figure5,width=7,height = 7,dpi=300)

#Summary statistics
days_afa.means.sub <- days_afa.means[,c("days_after_first_anthesis", "pollen_germination")]
days_afa.means.sub_melted <- melt(days_afa.means.sub, id.vars = c("days_after_first_anthesis"))
ddply(days_afa.means.sub_melted, c("days_after_first_anthesis","variable"), summarise, 
      n = length(value), mean = mean(value), median = median(value), std_dev = sd(value),
      sem = sd(value)/sqrt(length(value)))

#Pairwise comparisons with Welch's t-test
f5 <- factor(days_afa.means$days_after_first_anthesis)
p.adjust.method <- "none"
vals <- days_afa.means$pollen_germination
compare.levels.p.values <- function(i, j) {
  xi <- vals[as.integer(f5) == i]
  xj <- vals[as.integer(f5) == j]
  t.test(xi, xj,)$p.value
}
PVAL5 <- pairwise.table(compare.levels.p.values, levels(f5), p.adjust.method)
PVAL5


#######################
###### Figure 3 ######
#######################

#Read file with data
timecourse = read.table(file="figure3.csv",sep=",",header=TRUE)

#Subset columns with relevant data
timecourse<- timecourse[,c("sample","hours_incubation", "percent_germinated")]
timecourse$hours_incubation <- factor(timecourse$hours_incubation, levels = c("1","18","24","48","72"))

#Obtain average germination per sample (droplet)
timecourse.means <- aggregate(data = timecourse, percent_germinated ~ 
                                sample+hours_incubation, FUN = function(i)mean(i))

#Graph
figure3<-ggplot(timecourse.means,aes(x=hours_incubation,y=percent_germinated)) +
  geom_boxplot(aes(fill=hours_incubation)) + 
  geom_point(color="black", pch=21, size = 2, position=position_dodge(width = 0.65)) + 
  theme_bw(base_size = 14) + 
  labs(y = "Pollen germination (%)", x ="Incubation Time") +
  theme(axis.text.x = element_text(size = 20,face = "bold"), axis.text.y = element_text(size = 20,face = "bold"),
        axis.title.x=element_text(size="24",face="bold"),
        axis.title.y=element_text(size="24",face="bold"),  
        strip.text.x = element_text(size = "24", face = "bold"),
        plot.title = element_text(size = "28", face = "bold"),
        legend.position = "none")+
  scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20)) 

figure3
ggsave("figure3.pdf",plot = figure3,width=7,height = 7,dpi=300)

#Summary statistics
timecourse.means.sub <- timecourse.means[,c("hours_incubation", "percent_germinated")]
timecourse.means.sub_melted <- melt(timecourse.means.sub, id.vars = c("hours_incubation"))
ddply(timecourse.means.sub_melted, c("hours_incubation","variable"), summarise, 
      n = length(value), mean = mean(value), median = median(value), std_dev = sd(value),
      sem = sd(value)/sqrt(length(value)))

#Pairwise comparisons with Welch's t-test
f3 <- factor(timecourse.means$hours_incubation)
p.adjust.method <- "none"
vals <- timecourse.means$percent_germinated
compare.levels.p.values <- function(i, j) {
  xi <- vals[as.integer(f3) == i]
  xj <- vals[as.integer(f3) == j]
  t.test(xi, xj,)$p.value
}
PVAL3 <- pairwise.table(compare.levels.p.values, levels(f3), p.adjust.method)
PVAL3


#######################
###### Figure 2a ######
#######################

#Read file with data
germination = read.table(file="figure2a.csv",sep=",",header=TRUE)

#Subset columns with relevant data to graph
germinationa<- germination[,c("sample", "treatment", "germination_percent")]
germinationa$treatment <- factor(germinationa$treatment, levels = c("8","16","24","32"))

#Obtain average germination per sample (droplet)
germination.means <- aggregate(data = germinationa, germination_percent ~ 
                              sample+treatment, FUN = function(i)mean(i))


figure2a <- ggplot(germination.means,aes(x=treatment,y=germination_percent,group=treatment)) +
  geom_boxplot(aes(fill=treatment)) + 
  geom_point(color="black", pch=21, size = 2, position=position_dodge(width = 0.65)) + 
  theme_bw(base_size = 14) + 
  labs(title = "QQ74", y = "Pollen germination (%)", x ="Sucrose (%)") +
  theme(axis.text.x = element_text(size = 20,face = "bold"), axis.text.y = element_text(size = 20,face="bold"),
        axis.title.x=element_text(size="24",face="bold"),
        axis.title.y=element_text(size="24",face="bold"),  
        strip.text.x = element_text(size = "24", face = "bold"),
        plot.title = element_text(size = "28", face = "bold"),
        legend.position = "none")+
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20)) 

figure2a
ggsave("figure2a.pdf",plot = figure2a,width=7,height = 7,dpi=300)

#Summary statistics
sucrose.means.sub <- germination.means[,c("treatment", "germination_percent")]
sucrose.means.sub_melted <- melt(sucrose.means.sub, id.vars = c("treatment"))
ddply(sucrose.means.sub_melted, c("treatment","variable"), summarise, 
      n = length(value), mean = mean(value), median = median(value), std_dev = sd(value),
      sem = sd(value)/sqrt(length(value)))

#Pairwise comparisons with Welch's t-test
f2a <- factor(germination.means$treatment)
p.adjust.method <- "none"
vals <- germination.means$germination_percent
compare.levels.p.values <- function(i, j) {
  xi <- vals[as.integer(f2a) == i]
  xj <- vals[as.integer(f2a) == j]
  t.test(xi, xj,)$p.value
}
PVAL2a <- pairwise.table(compare.levels.p.values, levels(f2a), p.adjust.method)
PVAL2a


#######################
###### Figure 2b ######
#######################

#Read file with data
boric.acid = read.table(file="figure2b.csv",sep=",",header=TRUE)

#Subset columns with relevant data to graph
boric.acid<- boric.acid[,c("sample", "treatment", "germination_percent")]
boric.acid$treatment <- factor(boric.acid$treatment, levels = c("0","0.01","0.02","0.03","0.04"))

#Obtain average germination per sample (droplet)
boric.acid.means <- aggregate(data = boric.acid, germination_percent ~ 
                                 sample+treatment, FUN = function(i)mean(i))


figure2b <- ggplot(boric.acid.means,aes(x=treatment,y=germination_percent,group=treatment)) +
  geom_boxplot(aes(fill=treatment)) + 
  geom_point(color="black", pch=21, size = 2, position=position_dodge(width = 0.65)) + 
  theme_bw(base_size = 14) + 
  labs(title = "QQ74", y = "Pollen germination (%)", x ="Boric acid (%)") +
  theme(axis.text.x = element_text(size = 20,face = "bold"), axis.text.y = element_text(size = 20,face="bold"),
        axis.title.x=element_text(size="24",face="bold"),
        axis.title.y=element_text(size="24",face="bold"),  
        strip.text.x = element_text(size = "24", face = "bold"),
        plot.title = element_text(size = "28", face = "bold"),
        legend.position = "none")+
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20)) 

figure2b
ggsave("figure2b.pdf",plot = figure2b,width=7,height = 7,dpi=300)

#Summary statistics
boric.acid.means.sub <- boric.acid.means[,c("treatment", "germination_percent")]
boric.acid.means.sub_melted <- melt(boric.acid.means.sub, id.vars = c("treatment"))
ddply(boric.acid.means.sub_melted, c("treatment","variable"), summarise, 
      n = length(value), mean = mean(value), median = median(value), std_dev = sd(value),
      sem = sd(value)/sqrt(length(value)))

#Pairwise comparisons with Welch's t-test
f2b <- factor(boric.acid.means$treatment)
p.adjust.method <- "none"
vals <- boric.acid.means$germination_percent
compare.levels.p.values <- function(i, j) {
  xi <- vals[as.integer(f2b) == i]
  xj <- vals[as.integer(f2b) == j]
  t.test(xi, xj,)$p.value
}
PVAL2b <- pairwise.table(compare.levels.p.values, levels(f2b), p.adjust.method)
PVAL2b


#######################
###### Figure 2c ######
#######################

#Read file with data
ph = read.table(file="figure2c.csv",sep=",",header=TRUE)

#Subset columns with relevant data to graph
pha<- ph[,c("sample","genotype", "treatment", "pollen_germination")]
pha$treatment <- factor(pha$treatment, levels = c("5.5","6.5"))
pha$genotype <- factor(pha$genotype, levels = c("QQ74","Cherry vanilla"))

#Obtain average germination per sample (droplet)
ph.means <- aggregate(data = pha, pollen_germination ~ 
                                 sample+treatment+genotype, FUN = function(i)mean(i))

figure2c<-ggplot(ph.means,aes(x=treatment,y=pollen_germination)) +
  geom_boxplot(aes(fill=treatment)) + 
  geom_point(aes(fill=treatment), color="black", pch=21, size = 2, position=position_dodge(width = 0.65)) + 
  theme_bw(base_size = 14) + 
  labs(y = "Pollen Germination (%)", x ="") +
  theme(axis.title.x=element_text(face="bold"),
        axis.title.y=element_text(size="16",face="bold"),  
        strip.text.x = element_text(size = "14", face = "bold"),
        plot.title = element_text(size = "18", face = "bold"),
        legend.position =  "none") +
  scale_y_continuous(limits = c(0,100),breaks = seq(0, 100, by = 20)) +
  facet_wrap(~ genotype)

figure2c
ggsave("figure2c.pdf",plot = figure2c,width=7,height = 7,dpi=300)

#Summary statistics
ph.means.sub <- ph.means[,c("genotype", "treatment", "pollen_germination")]
ph.means.sub_melted <- melt(ph.means.sub, id.vars = c("genotype", "treatment"))
ddply(ph.means.sub_melted, c("genotype", "treatment", "variable"), summarise, 
      n = length(value), mean = mean(value), median = median(value), std_dev = sd(value),
      sem = sd(value)/sqrt(length(value)))

#Welch's t-test
ph.means.qq74 <- ph.means[ph.means$genotype == 'QQ74',]
ph.means.qq74.5.5 <- ph.means.qq74[ph.means.qq74$treatment == '5.5',]
ph.means.qq74.6.5 <- ph.means.qq74[ph.means.qq74$treatment == '6.5',]
ph.means.cv <- ph.means[ph.means$genotype == 'Cherry vanilla',]
ph.means.cv.5.5 <- ph.means.cv[ph.means.cv$treatment == '5.5',]
ph.means.cv.6.5 <- ph.means.cv[ph.means.cv$treatment == '6.5',]

t.test(ph.means.qq74.5.5$pollen_germination, ph.means.qq74.6.5$pollen_germination)
t.test(ph.means.cv.5.5$pollen_germination, ph.means.cv.6.5$pollen_germination)
