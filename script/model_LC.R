library(geobr)
library(spdep)
library(dplyr)
library(INLA)

#function to filter the data from Amazon cities

get.amazon<-function(data, muni){
  
  cities_am<-read.csv("dados_tratados/cities_am.csv")
  cities_am$code_muni2<-substr(cities_am$code_muni,1,6)
  am<-c(cities_am$code_muni2)
  
  cut_am<- data %>% filter ({{muni}} %in% am)
  
  return(cut_am)
  
}


##reading cenralized data
df<- read.csv("data/data_centralized.csv")

##List of municipalities in the Amazon region of Brazil
mun_am<-read.csv("data/cities_am.csv")
cod_am<-as.list(mun_am$code_muni)

##BR municipalities shape

mun<-read_municipality(code_muni="all", year=2017) %>%
  filter (code_muni %in% cod_am)

##Change the code of the municipality to match the code of our data.
mun$cod_mun<-as.factor(substr(mun$code_muni,1,6))

df$cod_mun<-as.factor(df$cod_mun)

map1 <- left_join(mun, df, by="cod_mun")


###Removing some municipalities with no data

map1<- map1 %>% filter (cod_mun!= 171630 & cod_mun!=150640 & cod_mun!=170305) %>%  
  filter (cod_mun!=150475) %>%    
  filter (cod_mun!=150475) %>% 
  filter (cod_mun!=	510454 & cod_mun!=510452)

###Spatial neighborhood matrices

map<- map1 %>% filter(year==2017)

nb <- poly2nb(map)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

##creating id area and idtime for spatial and temporal randon effects
map1$year<-as.numeric(map1$year)
map1$idarea <- rep(1:length(unique(map1$cod_mun)), each=17) 
map1$idarea1<- map1$idarea
map1$idtime <- 1 + map1$year - min(map1$year)
map1$idtime1<-map1$idtime
map1$idtime.int<-map1$idtime
map1$idarea2<-map1$idarea

###model

model <- inla(cases ~  t_mean + pre_total + ips_n_hum_bas + p_male + Natural.Forest + 
                 log_forestloss + log_perm_crop + log_ntfp +  log_cattle + Natural.Forest:log_cattle + 
                Natural.Forest:log_perm_crop +
                 f(idarea, model = "bym", graph = g) + 
                 f(idtime, model = "iid")+
                 f(idtime1, model = "rw1")+
                f(idarea1,model="iid", 
                 group=idtime.int,
                  control.group=list(model="rw1")),
               family = "nbinomial", data = map1, E=E,
               control.predictor = list(compute = TRUE), control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE, config=TRUE), 
               inla.mode = "classic")


summary(model)

####Sampling from the approximate posterior distribution using INLA
srag.samples0.list <- INLA::inla.posterior.sample(n = 1000, model)

# pull out samples for the effects, for the figure 2
all_samps <- data.frame(
  int_samps=as.numeric(inla.posterior.sample.eval("(Intercept)", srag.samples0.list)),
  forest_samps=as.numeric(inla.posterior.sample.eval("Natural.Forest", srag.samples0.list)),
  cattle_samps=as.numeric(inla.posterior.sample.eval("log_cattle", srag.samples0.list)),
  for_cattle_samps=as.numeric(inla.posterior.sample.eval("Natural.Forest:log_cattle", srag.samples0.list)))
head(all_samps)

write.csv(all_samps, "output/posteriors_cattle_forest_model.csv")



