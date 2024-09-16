##Script for Figure 2

library(dplyr)
library(tidyr)
library(ggplot2)


sample<- read.csv("output/posteriors_cattle_forest_model.csv")


###SD values of cattle and forest
forest<-c(0,0.5,1,1.5,2,0,0.5,1,1.5,2,0,0.5,1,1.5,2)
cattle<-c(0,0,0,0,0,1,1,1,1,1,-1,-1,-1,-1,-1)


tbla<-data.frame(forest=forest, cattle=cattle)

##calculating the risk 
n_sample<-unique(sample$X)

lista<-list()

for (i in n_sample) {
  
  sub<-  sample %>% filter (X==i)
  
  lista[[i]]<-tbla %>% 
    rowwise() %>% 
    mutate(risk= exp(sub$forest_samps*forest + sub$cattle_samps*cattle + sub$for_cattle_samps*cattle*forest),
           sample=i)
}


big_data<-bind_rows(lista)

##risk summary
risk_ic<- big_data %>%
     group_by(forest, cattle) %>%
       summarise(Median = stats::median(risk, na.rm = T),
                   LI = stats::quantile(risk, probs = 0.025, na.rm = T),
                   LS = stats::quantile(risk, probs = 0.975, na.rm = T),
                   LIb = stats::quantile(risk, probs = 0.25, na.rm = T),
                   LSb = stats::quantile(risk, probs = 0.75, na.rm = T),
                   .groups = "drop")

###risk in % of Cl increase
risk_ic2<- risk_ic %>%
           mutate(Median_p= case_when(Median>=1 ~ (Median-1)*100,
                                      Median<1 ~ -(1-Median) *100),
                  LI_p= case_when(Median>=1 ~ (LI-1)*100,
                                      Median<1 ~ -(1-LI) *100),
                  LS_p= case_when(Median>=1 ~ (LS-1)*100,
                                      Median<1 ~ -(1-LS) *100)
                  ) %>%
  mutate(Median_p=round(Median_p,2),
         LI_p=round(LI_p,2),
         LS_p=round(LS_p,2)
         )


####Plot

ggplot(risk_ic2)+
  geom_line(aes(x=forest, y=Median_p, colour=as.factor(cattle), group = as.factor(cattle)),linewidth = 0.6)+
  geom_ribbon(aes(x=forest, ymin=LI_p, ymax=LS_p, fill=as.factor(cattle), group = as.factor(cattle)), alpha = 0.2)+
  scale_color_manual(name= 'Standardized nº of cattle heads' ,values = c(
    "1"='red4',
    "0" = 'blue4',
    "-1" = 'goldenrod3'),
    breaks = c("-1", "0","1"))+
  scale_fill_manual(name= 'Standardized nº of cattle heads', values = c(
    "1"='red4',
    "0" = 'blue4',
    "-1" = 'goldenrod3'),
    breaks = c("-1", "0","1"))+
  #labs(color = 'Standardized nº of cattle heads', fill="")+
  # ylim(c(-16,100))+
  # labs(color=NULL)+
  #  scale_color_viridis(discrete=TRUE)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom", text = element_text(size = 15))+
  ylab("Increase in CL cases (%)")+
  xlab("Standardized Forest Cover")



    