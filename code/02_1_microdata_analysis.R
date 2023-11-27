#Load packages
source('./fun/fun.R')


# 1. Read the data --------------------------------------------------------

#Set
code_state<-23
char_state<-"CE"
code_muni_<-2304400

#Census microdata
census_micro<-readRDS("data/00_microdados/census_micro_data_treated.RDS")


#Census tracts
census_tract<-read_census_tract(code_tract = code_state,year = 2010) %>% 
  filter(code_muni==code_muni_) %>%
  st_transform(31984)


census_tract_data<- readRDS("data/01_census/census_tract_data.RDS")

#Income data
data_income<- read.csv(paste0("data/01_census/PessoaRenda_",char_state,".csv"),sep=";") %>%
  mutate(Code_muni = substr(as.factor(Cod_setor),1,7)) %>%
  filter(Code_muni==as.character(code_muni_)) %>% 
  select(Cod_setor,Code_muni,V020,V021,V022) %>%
  mutate(across(V020:V022,~as.numeric(ifelse(.x == "X" | is.na(.x), NA,
                                             sub(",", ".", sub(".", "", .x, fixed=TRUE), fixed=TRUE)))))%>%
  rename(code_tract=Cod_setor)


data_income<- data_income %>% mutate(mean_income = V022/V020/510,
                                     mean_income_cr = V022/V021/510)






data_income_resp<- read.csv(paste0("data/01_census/ResponsavelRenda_",char_state,".csv"),sep=";") %>%
  mutate(Code_muni = substr(as.factor(Cod_setor),1,7)) %>%
  filter(Code_muni==as.character(code_muni_)) %>% 
  select(Cod_setor,Code_muni,V020,V021,V022) %>%
  mutate(across(V020:V022,~as.numeric(ifelse(.x == "X" | is.na(.x), NA,
                                             sub(",", ".", sub(".", "", .x, fixed=TRUE), fixed=TRUE)))))%>%
  rename(code_tract=Cod_setor) %>% 
  mutate(mean_income = V022/V020/510,
        mean_income_cr = V022/V021/510)



data_basico_census<-read.csv("data/01_census/Basico_CE.csv",sep=";", encoding = "latin1") %>%
  rename(code_tract=Cod_setor)%>%
  mutate(
    code_tract= as.factor(code_tract),
    across(V001:V012,~as.numeric(ifelse(.x == "X" | is.na(.x), NA,
                                        sub(",", ".", sub(".", "", .x, fixed=TRUE), fixed=TRUE))))
  ) %>%
  filter(!is.na(V011) & !is.na(V012)) %>%
  select(code_tract,V001,V002,V005,V007,V009,V010,V011,V012) %>%
  mutate(renda_media09 = V009/510,
         renda_media11 = V011/510,
         renda_media_resp = V005/510,
         renda_media_resp_cr = V007/510,
         variancia09 = V010/(510*510),
         variancia11= V012/(510*510)
  ) %>%
  mutate(across(renda_media09:variancia11,~ifelse(is.na(.x),0,.x)))


#Income data per capita
data_income_pc<- read_excel(paste0("data/01_census/Entorno04_",char_state,".xls")) %>%
  mutate(Code_muni = substr(as.factor(Cod_setor),1,7)) %>%
  filter(Code_muni==as.character(code_muni_)) %>%
  select(Cod_setor,Code_muni,V683:V694) %>%
  mutate(across(V683:V694,~as.numeric(as.character(.x)))) %>%
  mutate(
    inc_pc_0 = V693+V694,
    inc_pc_1_4 = V683+V684,
    inc_pc_1_2 = V685+V686,
    inc_pc_1 = V687+V688,
    inc_pc_2 = V689+V690,
    inc_pc_2_plus = V691+V692
  ) %>%
  rename(code_tract=Cod_setor) %>%
  select(code_tract,inc_pc_0:inc_pc_2_plus)

#Head of the home data
data_hous_resp<- read.csv(paste0("data/01_census/ResponsavelRenda_",char_state,".csv"),sep=";")%>%
  mutate(Code_muni = substr(as.factor(Cod_setor),1,7)) %>%
  filter(Code_muni==as.character(code_muni_)) %>% 
  select(Cod_setor,Code_muni,V001:V010,V042,V064) %>%
  rename(code_tract=Cod_setor)

colnames(data_hous_resp)
names(data_hous_resp)<- c("code_tract", "Code_muni","Resp_R_0_5",
                          "Resp_R_1_0","Resp_R_2_0","Resp_R_3_0","Resp_R_5_0","Resp_R_10_0",
                          "Resp_R_15_0","Resp_R_20_0",
                          "Resp_R_20mais_0","Resp_R_0","Resp_masc","Resp_fem")


#Hex grid
hex_grid <-
  readRDS("data/04_land_use/hex_agregado_for_09_2019.rds")

#Microdata microsimulation
data_microssimulation<-readRDS("data/03_microsimulated2/complete/data.micro.final2.RDS")





# pre analysis ------------------------------------------------------------

colnames(census_micro)
str(census_micro)
census_micro_resp_home<- census_micro %>% filter(resp_home!="Resp_dep")
census_micro_resp_dep<-census_micro %>% filter(resp_home=="Resp_dep")

cor.test(census_micro_resp_home$Rend_pes,census_micro_resp_home$Rend_pc)
cor.test(census_micro_resp_dep$Rend_pes,census_micro_resp_dep$Rend_pc)


census_micro %>% group_by(resp_home) %>% summarise(cor(Rend_pes,Rend_pc))





# 2. Validation -----------------------------------------------------------



# 2.1 Data ----------------------------------------------------------------


data_microssimulation_resp<- data_microssimulation %>% filter(resp_home!="Resp_dep")%>%
  filter(renda_class!="Total_u10") %>%
  select(id_g:Rend_pc,V6036)





list_tracts<- as.character(unique(data_microssimulation_resp$code_tract))
n_resp<- data_income_resp %>% filter(code_tract %in% list_tracts)


erro_count_resp<- (sum(n_resp$V020)-nrow(data_microssimulation_resp))/sum(n_resp$V020)




ddd<- data_basico_census %>% filter(code_tract %in% list_tracts)


sum(ddd$V001)





#individuos removidos da analise
dd_above_65<- readRDS("data/03_microsimulated2/complete/data.micro.final2.RDS") %>% 
  filter(renda_class!="Total_u10") %>%
  select(id_g:Rend_pc,V6036,V0648,V0628) %>%
  rename(age = V6036,
         job_status = V0648,
         study_status = V0628) %>% filter(resp_home!="Resp_dep",age>65)

dd_under_18<- readRDS("data/03_microsimulated2/complete/data.micro.final2.RDS") %>% 
  filter(renda_class!="Total_u10") %>%
  select(id_g:Rend_pc,V6036,V0648,V0628) %>%
  rename(age = V6036,
         job_status = V0648,
         study_status = V0628) %>% filter(resp_home!="Resp_dep",age<18)


dd_poor<- readRDS("data/03_microsimulated2/complete/data.micro.final2.RDS") %>% 
  filter(renda_class!="Total_u10") %>%
  select(id_g:Rend_pc,V6036,V0642,V0648,V0628) %>%
  rename(age = V6036,
         job_status = V0648,
         job_yes_not = V0642,
         study_status = V0628) %>% filter(resp_home!="Resp_dep") %>%
  mutate(inc_pc_dol = Rend_pc*minimum_salary*monetary_correction/dolar)%>%
  filter(inc_pc_dol<poverty_line_inc)

colnames(dd_poor)



sum(is.na(dd_above_65$job_status))

nrow(dd_above_65)+

sum(data_microssimulation_resp$educ=="ND")

nrow(dd_under_18)


nrow(data.micro.final_resp)-nrow(data_ind_access)

# data.micro.final_test<- data.micro.final %>% 
#   filter(renda_class!="Total_u10")%>%
#   left_join(data_basico,by=c("code_tract")) %>%
#   group_by(code_tract) %>% 
#   summarise(n= n(),
#             mean_rend = mean(Rend_pes),
#             mean_rend_pc = mean(Rend_pc),
#             renda_media09 = mean(renda_media09),
#             renda_media11 = mean(renda_media11)) %>%
#   mutate(dif_pes = mean_rend-renda_media09,
#          dif_pc = mean_rend_pc-renda_media09,
#          dif_perc_pes = dif_pes/renda_media09*100,
#          dif_perc_pc = dif_pc/renda_media09*100) %>%
#   mutate(across(mean_rend:dif_perc_pc,~round(.x,2)))
# 
# 
# 
# data.micro.final_test_2<- data.micro.final %>%
#   filter(renda_class!="Total_u10", Rend_pes!=0)%>%
#   left_join(data_income %>% mutate(code_tract=as.factor(code_tract)),by=c("code_tract")) %>%
#   group_by(code_tract) %>%
#   summarise(n= n(),
#             mean_rend = mean(Rend_pes),
#             mean_rend_pc = mean(Rend_pc),
#             mean_income = mean(mean_income)) %>%
#   mutate(dif_pes = mean_rend-mean_income,
#          dif_pc = mean_rend_pc-mean_income,
#          dif_perc_pes = dif_pes/mean_income*100,
#          dif_perc_pc = dif_pc/mean_income*100)%>%
#   mutate(across(mean_rend:dif_perc_pc,~round(.x,2)))
# 


# 2.2 External validation -------------------------------------------------


#correlation test
cor.test(data.micro.final_test$mean_rend,data.micro.final_test$renda_media09)

cor.test(data.micro.final_test_2$mean_rend,data.micro.final_test_2$mean_income)

cor.test(data.micro.final_test_2$mean_rend_pc,data.micro.final_test_2$mean_income)





#T test by census tract
#list_tracts<- as.character(unique(data_microssimulation_resp$code_tract))

data_ttest_all<-map_df(list_tracts,t_test_census_resp)
data_ttest_all<- data_ttest_all %>% mutate(dif_inc_abs = abs(dif_inc))


perc_tracts_approved_all<-sum(data_ttest_all$p_value>0.05)/length(list_tracts)
perc_tracts_not_approved_all<-1-perc_tracts_approved_all

perc_tracts_approved_all_99<-sum(data_ttest_all$p_value>0.01)/length(list_tracts)
perc_tracts_not_approved_all_99<-1-perc_tracts_approved_all_99


rejected_tracts<- data_ttest_all %>% filter(p_value<0.05) %>% 
  mutate(dif = (mean_real-mean_estimate),
         dif_perc = dif/mean_real*100,
         dif_abs = abs(dif)) %>% 
  select(-c("conf_int"))
summary(rejected_tracts)

median(abs(rejected_tracts$dif_perc))


data_resp_agg<- data_microssimulation_resp %>% group_by(code_tract) %>%
                summarise(n_resp = n(), mean_income = mean(Rend_pes)) %>%
                left_join(data_basico_census %>% select(code_tract,renda_media_resp),
                          by=c("code_tract"))%>%
                rename(mean_income_census = renda_media_resp)
data_resp_agg<- data_resp_agg %>% mutate(dif_inc = mean_income-mean_income_census,
                                         dif_inc_perc = dif_inc/mean_income_census*100)

summary(data_resp_agg)
median(abs(data_resp_agg$dif_inc))*510
median(abs(data_resp_agg$dif_inc))*510/4.97
median(abs(data_resp_agg$dif_inc_perc))

colnames(data_microssimulation_resp)
list_major<- data_resp_agg %>% filter(dif_inc_perc>20)
list_major<- data_microssimulation_resp %>% 
              filter(code_tract %in% list_major$code_tract) %>%
              group_by(code_tract) %>%
              summarise(min(Rend_pes),
                        median(Rend_pes),
                        max(Rend_pes))
sum(list_major$`max(Rend_pes)`>30)/nrow(list_major)
summary(list_major)
# 
# 
# ks.test(data.micro.final_test$dif_pc,"pnorm",
#         mean= mean(data.micro.final_test$dif_pc),
#         sd = sd(data.micro.final_test$dif_pc))
# 
# sum(data.micro.final_test$dif_perc_pc>150)
# 
# summary
# # mean(data.micro.final_test$dif_pes)*510*monetary_correction
# # mean(data.micro.final_test$dif_pes)*510*monetary_correction/dolar
# 
# median(data.micro.final_test$dif_pes)*510*monetary_correction
# median(data.micro.final_test$dif_pes)*510*monetary_correction/dolar
# median(data.micro.final_test$dif_perc_pes)
# 
# 
# 
# median(data.micro.final_test$dif_pc)*510*monetary_correction
# 
# 
# ks.test(data.micro.final_test$dif_pes)
# 
# 
# t.test(data.micro.final_test$dif_perc_pes,conf.level = 0.95)
# 
# summary(data.micro.final_test)

# 
# t.test(data_resp_agg$dif_inc,mu=0,conf.level = 0.99)
# z.test(data_resp_agg$dif_inc,mu=0,sigma.x = sd(data_resp_agg$dif_inc),
#        conf.level = 0.99)



list_files_val<-list.files("data/03_microsimulated2",
                       full.names = TRUE,
                       include.dirs = TRUE,
                       pattern = ".RDS")

#Internal validation
data_internal<- purrr::map_df(list_files_val[grepl("intv",list_files_val)],
                              readRDS)
summary(data_internal)

#External validation
data_external<- purrr::map_df(list_files_val[grepl("extv",list_files_val) & !grepl("comp",list_files_val)],
                              readRDS)
summary(data_external)




summary(data.micro.final_test)

#correlation test
cor(data.micro.final_test$mean_rend,data.micro.final_test$renda_media09)
cor(data.micro.final_test$mean_rend,data.micro.final_test$mean_rend_pc)

cor.test(data.micro.final_test$mean_rend,data.micro.final_test$renda_media09)
cor.test(data.micro.final_test$mean_rend,data.micro.final_test$mean_rend_pc)
#t test
t.test(data.micro.final_test$mean_rend,data.micro.final_test$renda_media09)

t.test(data.micro.final_test$mean_rend_pc,data.micro.final_test$renda_media09)

dd<- data.micro.final_test %>% filter(abs(dif_perc)<10)
de<- data.micro.final_test %>% mutate(decil = ntile(mean_rend_pc,10))%>% filter(abs(dif_perc)>10) 


#Hexágonos com erro acima de 10%
# nrow(de)/nrow(data.micro.final_test)

#Pessoas em hexágonos com erro acima de 10%
# sum(de$n)/sum(data.micro.final_test$n)


#participação dos hexágonos dos decis de renda entre 8 e 10 salários mínimos no erro
# de_decil<-de %>% group_by(decil) %>% summarise(n(),median(dif_perc))
# 
# sum(de_decil$`n()`[de_decil$decil %in% c(8,9,10)])/nrow(de)
# 
# de_8_10<- de %>% filter(decil %in% c(8,9,10))
# hist(de_8_10$dif_perc)


#% de pessoas acima de x salários mínimos


data_ind_access %>% filter(status=="worker") %>% group_by(decil) %>% summarise(mean(Rend_pc))


cor(de$renda_media09,abs(de$dif_perc))

nrow(de)/nrow(data.micro.final_test)
t.test(old_micro$dif_perc)

summary(data.micro.final_test)



quantile(data.micro.final_test$dif_perc,seq(0,1,0.1))

# summary(data.micro.final_test)
# sum(abs(data.micro.final_test$dif_perc)<=10)/nrow(data.micro.final_test)


# dd_test<- data.micro.final_test %>% filter(abs(dif_perc)>10)
# 
# census_tract_geo<- read_census_tract(code_tract = code_muni_,year = 2010)
# 
# dd_test<- census_tract_geo %>% filter(code_tract %in% unique(dd_test$code_tract)) %>%
#   left_join(dd_test,by="code_tract")
# mapview(dd_test,zcol="dif")

# colnames(census_tract_geo)
# dd_test
# summary(dd_test)


#Graphs
data_resp_agg$dif_inc
graph_0001_val_hist <- ggplot(data_resp_agg, aes(x = dif_inc_perc)) +
  geom_histogram(binwidth = 1, fill = "grey99", color = "grey30", alpha = 0.5) +
  labs(x = "Difference between modeled and observed mean income by code tract (%)",
       y = "Frequency (n. code tracts)") +
  theme_minimal()


# Create the boxplot plot
graph_0001_val_box <- ggplot(data_resp_agg, aes(y = dif_inc_perc)) +
  geom_boxplot(fill = "white", color = "grey70",width = 0.15) +
  labs(x = "",
       y = "Difference (%) - modeled and census average income of h.h. by code tract") +
  theme_void() +
  coord_flip()

# Arrange the plots using patchwork
graph_0001_val <-  graph_0001_val_box + graph_0001_val_hist +
  patchwork::plot_layout(nrow =  2,heights = c(1,4))

graph_0001_val

ggsave("graphs/graph_0001_val.png",plot = graph_0001_val,scale=1.5,dpi=500)

t.test(data.micro.final_test2$mean_rend_pc,data.micro.final_test2$renda_media09)
t.test(data.micro.final_test2$mean_rend_pc,data.micro.final_test2$renda_media11)







graph_0001_val_hist1 <- ggplot(data_resp_agg, aes(x = dif_inc)) +
  geom_histogram(binwidth = 0.1, fill = "grey99", color = "grey30", alpha = 0.5) +
  labs(x = "Difference between modeled and observed mean income by code tract (%)",
       y = "Frequency (n. code tracts)") +
  theme_minimal()


# Create the boxplot plot
graph_0001_val_box1 <- ggplot(data_resp_agg, aes(y = dif_inc)) +
  geom_boxplot(fill = "white", color = "grey70",width = 0.15) +
  labs(x = "",
       y = "Difference (%) - modeled and census average income of h.h. by code tract") +
  theme_void() +
  coord_flip()

# Arrange the plots using patchwork
graph_0001_val <-  graph_0001_val_box + graph_0001_val_hist +
  patchwork::plot_layout(nrow =  2,heights = c(1,4))










# 2.3 Internal validation -------------------------------------------------
list_files<-list.files("data/03_microsimulated2",
                       full.names = TRUE,
                       include.dirs = TRUE,
                       pattern = ".RDS")

#Internal validation
data_internal<- purrr::map_df(list_files[grepl("intv",list_files)],
                              readRDS)
summary(data_internal)

