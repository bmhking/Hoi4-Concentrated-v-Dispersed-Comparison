library(ggplot2)
prod_eff_growth = 0
prod_eff_cap_base = 0.9
factory_output = 4.5
factory_output_base = 1.78
industry_tech_level = 5
days = 500
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
prod_eff_base_d = 0.1+0.05*industry_tech_level
prod_eff_base_c = 0.1
factory_output_c = factory_output_base+0.15*industry_tech_level
factory_output_d = factory_output_base+0.1*industry_tech_level
ic_c = vector("numeric", days)
ic_d = vector("numeric", days)
for(i in 1:500){
  ic_c[i] = factory_output*factory_output_c*prod_eff_base_c
  ic_d[i] = factory_output*factory_output_d*prod_eff_base_d
  prod_eff_base_c = min(prod_eff_base_c+0.001*(prod_eff_cap)^2/prod_eff_base_c*(1+prod_eff_growth),prod_eff_cap)
  prod_eff_base_d = min(prod_eff_base_d+0.001*(prod_eff_cap)^2/prod_eff_base_d*(1+prod_eff_growth),prod_eff_cap)
}
ic_cum_c = ic_c
ic_cum_d = ic_d
for(i in 2:days){
  ic_cum_c[i] = ic_cum_c[i-1]+ic_c[i]
  ic_cum_d[i] = ic_cum_d[i-1]+ic_d[i]
}
plot_data = data.frame(day=rep(1:days,times=2),ic=c(ic_cum_c,ic_cum_d),type=rep(c('Concentrated','Dispersed'),each=days))
intersection_label = min(which(ic_cum_c >= ic_cum_d))
ggplot(plot_data) + geom_line(aes(x=day,y=ic,colour=type))+
  geom_label(aes(x=day,y=ic,label = format(round(ic,2),big.mark=",",scientific=FALSE)), 
             data = . %>% filter(row_number() == days),nudge_x=-10, nudge_y=100) + 
  labs(title = paste0(industry_tech_year, " level industry tech, ",
                      label_percent()(factory_output_c)," Concentrated factory output,\n",
                      label_percent()(prod_eff_cap)," Production Efficiency Cap, ",
                      label_percent()(prod_eff_growth)," Extra Production Efficiency Growth\nConcentrated exceeds Dispersed at day ", intersection_label)) +
  theme_minimal()

