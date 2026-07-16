library(dplyr)
library(scales)
library(ggplot2)
## change modifiers here
prod_eff_growth = 0
prod_eff_cap_base = 0.9
factory_output = 4.5
factory_output_base = 1.78
industry_tech_level = 4
current_eff = 0.8
## if you don't want to do retention calculation then set this to 0
retention_base = 0.3
## days of calculation
days = 1000
## days to plot in case the break-even point is too small
plot_days = 1000
if(industry_tech_level == 0){
  industry_tech_year = 1934
}else if(industry_tech_level == 1){
  industry_tech_year = 1936
}else if(industry_tech_level == 2){
  industry_tech_year = 1937
}else if(industry_tech_level == 3){
  industry_tech_year = 1939
}else if(industry_tech_level == 4){
  industry_tech_year = 1941
}else if(industry_tech_level == 5){
  industry_tech_year = 1943
}
prod_eff_cap = prod_eff_cap_base+0.1*industry_tech_level
factory_output_c = factory_output_base+0.15*industry_tech_level
factory_output_d = factory_output_base+0.1*industry_tech_level
factory_retention = 0.1*industry_tech_level
if(current_eff <= 0.1){
  prod_eff_base_d = 0.1+0.05*industry_tech_level
  prod_eff_base_c = 0.1
}else{
  prod_eff_base_d = current_eff*(retention_base+(1-retention_base)*factory_retention)
  prod_eff_base_c = current_eff*retention_base
}
ic_c = vector("numeric", days)
ic_d = vector("numeric", days)
prod_eff_c = vector("numeric", days)
prod_eff_d = vector("numeric", days)
prod_eff_c[1] = prod_eff_base_c
prod_eff_d[1] = prod_eff_base_d
for(i in 1:days){
  ic_c[i] = factory_output*factory_output_c*prod_eff_c[i]
  ic_d[i] = factory_output*factory_output_d*prod_eff_d[i]
  if(i != days){
    prod_eff_c[i+1] = min(prod_eff_c[i]+0.001*(prod_eff_cap)^2/prod_eff_c[i]*(1+prod_eff_growth),prod_eff_cap)
    prod_eff_d[i+1] = min(prod_eff_d[i]+0.001*(prod_eff_cap)^2/prod_eff_d[i]*(1+prod_eff_growth),prod_eff_cap)
  }
}
ic_cum_c = ic_c
ic_cum_d = ic_d
for(i in 2:days){
  ic_cum_c[i] = ic_cum_c[i-1]+ic_c[i]
  ic_cum_d[i] = ic_cum_d[i-1]+ic_d[i]
}
intersection_label = min(which(ic_cum_c >= ic_cum_d))
plot_data = data.frame(day=rep(1:plot_days,times=2),ic=c(ic_cum_c[1:plot_days],ic_cum_d[1:plot_days]),type=rep(c('Concentrated','Dispersed'),each=plot_days))
ggplot(plot_data) + geom_line(aes(x=day,y=ic,colour=type))+
  geom_label(aes(x=day,y=ic,label = format(round(ic,2),big.mark=",",scientific=FALSE)), 
             data = . %>% filter(row_number() == plot_days),nudge_x=-10, nudge_y=100) + 
  labs(title = paste0(industry_tech_year, " level industry tech, ",
                      label_percent()(factory_output_c)," Concentrated factory output,\n",
                      label_percent()(prod_eff_cap)," Production Efficiency Cap, ",
                      label_percent()(prod_eff_growth)," Extra Production Efficiency Growth,\n",
                      label_percent()(current_eff)," Current Production Efficiency level, ",
                      label_percent()(retention_base)," Base Retention Rate,\nConcentrated exceeds Dispersed at day ", 
                      intersection_label, "\nBy day ", plot_days, " Concentrated IC overproduces Dispersed IC by ",
                      label_percent(accuracy=0.01)(ic_cum_c[plot_days]/ic_cum_d[plot_days]-1))) +
  theme_minimal()

