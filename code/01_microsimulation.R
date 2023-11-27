#Load packages
source('./fun/fun.R')
source('./fun/book_functions.R')
source('./fun/microsim_func.R')

# 01. Load census geodata -------------------------------------------------

#Set
code_state<-23
char_state<-"CE"
code_muni_<-2304400

#If it causes microsimulation problems
remove_income_pc<- TRUE


#Census tracts
census_tract<-read_census_tract(code_tract = code_state,year = 2010) %>% 
              filter(code_muni==code_muni_) %>%
              st_transform(31984)

# Public Use Microdata Areas (PUMAs)
puma<- read_weighting_area(code_weighting = code_state,year = 2010) %>% 
  filter(code_muni==code_muni_) %>% 
  select(code_weighting) %>% 
  st_transform(31984) %>%
  rename("CD_APONDE"="code_weighting")

# Add PUMA id to census tract
census_tract<- census_tract %>%
                st_join(puma, left = TRUE, largest=TRUE)


# 02. Load census tracts data ---------------------------------------------

#Gender and age data - Men
data_gen_age_m<- read.csv(paste0("data/01_census/Pessoa11_",char_state,".csv"),sep=";") %>%
                      mutate(Code_muni = substr(as.factor(Cod_setor),1,7)) %>%
                      filter(Code_muni==as.character(code_muni_)) %>% 
                      select(Cod_setor,Code_muni,V022, V035:V134) %>%
                      rename("code_tract"= "Cod_setor")

#Gender and age data - Women
data_gen_age_w<- read.csv(paste0("data/01_census/Pessoa12_",char_state,".csv"),sep=";") %>%
  mutate(Code_muni = substr(as.factor(Cod_setor),1,7)) %>%
  filter(Code_muni==as.character(code_muni_)) %>% 
  select(Cod_setor,Code_muni,V022, V035:V134) %>%
  rename("code_tract"= "Cod_setor")


#Income data
data_income<- read.csv(paste0("data/01_census/PessoaRenda_",char_state,".csv"),sep=";") %>%
              mutate(Code_muni = substr(as.factor(Cod_setor),1,7)) %>%
              filter(Code_muni==as.character(code_muni_)) %>% 
              select(Cod_setor,Code_muni,V001:V010) %>%
              rename(code_tract=Cod_setor)

#Head of the home data
data_hous_resp<- read.csv(paste0("data/01_census/ResponsavelRenda_",char_state,".csv"),sep=";")%>%
  mutate(Code_muni = substr(as.factor(Cod_setor),1,7)) %>%
  filter(Code_muni==as.character(code_muni_)) %>% 
  select(Cod_setor,Code_muni,V001:V010,V042,V064) %>%
  rename(code_tract=Cod_setor)

#Number of individuals in collective households
data_gen_age_m_col<- read.csv(paste0("data/01_census/Pessoa11_",char_state,".csv"),sep=";") %>%
  mutate(Code_muni = substr(as.factor(Cod_setor),1,7)) %>%
  filter(Code_muni==as.character(code_muni_)) %>% 
  select(Cod_setor,Code_muni,V001, V002) %>%
  rename("code_tract"= "Cod_setor") %>%
  mutate(across(V001:V002,~as.numeric(as.character(.x))))%>%
  filter(!is.na(V002)) %>%
  mutate(n_col_m = V001 - V002)

data_gen_age_w_col<- read.csv(paste0("data/01_census/Pessoa12_",char_state,".csv"),sep=";") %>%
  mutate(Code_muni = substr(as.factor(Cod_setor),1,7)) %>%
  filter(Code_muni==as.character(code_muni_)) %>% 
  select(Cod_setor,Code_muni,V001, V002) %>%
  rename("code_tract"= "Cod_setor") %>%
  mutate(across(V001:V002,~as.numeric(as.character(.x))))%>%
  filter(!is.na(V002)) %>%
  mutate(n_col_w = V001 - V002)

data_ind_col<- data_gen_age_m_col %>% left_join(data_gen_age_w_col,
                                                by=c("code_tract"="code_tract")) %>%
  mutate(n_col = n_col_m+n_col_w) %>%
  select(code_tract,n_col)

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

   #Add number of individuals in collective households, who was not countet in this variable
data_income_pc <- data_income_pc %>% left_join(data_ind_col,by=c("code_tract"="code_tract"))

#Race
data_race<- read.csv(paste0("data/01_census/Pessoa03_",char_state,".csv"),sep=";") %>%
  mutate(Code_muni = substr(as.factor(Cod_setor),1,7)) %>%
  filter(Code_muni==as.character(code_muni_)) %>% 
  select(Cod_setor,Code_muni,V001:V006) %>%
  rename("code_tract"= "Cod_setor")


# 03. Data census treatment -----------------------------------------------

#Code tracts with people, using income data
census_tract_with_people<-census_tract %>% 
  left_join(data_income %>% 
              mutate(code_tract=as.character(code_tract)),
              by=c("code_tract"="code_tract")) %>%
  mutate(tot = rowSums(across(V001:V010,
                 ~as.numeric(as.character(.x))))) %>%
  filter(tot>0) %>% 
  select(colnames(census_tract))

#Aggregate gender and age - men
class_age<-20
dd_m<- func_age(data_gen_age_m,age = class_age,first_col = 3,col_name = "Total_")

census_tract_data <- census_tract_with_people %>%
  mutate(code_tract = as.numeric(as.character(code_tract))) %>%
  left_join(dd_m %>% 
              select(code_tract,Total_20:Total_100), by = "code_tract") %>%
  mutate(
    code_tract= as.factor(code_tract),
    across(Total_20:Total_100,~as.factor(ifelse(.x == "X" | is.na(.x), 0, .x)))
  )  %>%
  st_make_valid() %>%
  mutate(Total_80 =  as.numeric(as.character(Total_80))+
           as.numeric(as.character(Total_100))) %>%
  select(-Total_100)
 
#Aggregate gender and age - Women
dd_w<- func_age(data_gen_age_w,age = class_age,first_col = 3,col_name = "Total_w")

census_tract_data <- census_tract_data %>%
  mutate(code_tract = as.numeric(as.character(code_tract))) %>%
  left_join(dd_w %>% 
              select(code_tract,Total_w20:Total_w100), by = "code_tract") %>%
  mutate(
    code_tract= as.factor(code_tract),
    across(Total_w20:Total_w100,~as.factor(ifelse(.x == "X" | is.na(.x), 0, .x)))
  )  %>%
  st_make_valid() %>%
  mutate(Total_w80 =  as.numeric(as.character(Total_w80))+
           as.numeric(as.character(Total_w100))) %>%
  select(-Total_w100)

#Aggregate Income
names(data_income)<- c("code_tract", "Code_muni","R_0_5",
                              "R_1_0","R_2_0","R_3_0","R_5_0","R_10_0",
                              "R_15_0","R_20_0",
                              "R_20mais_0","R_0")
data_income<-data_income %>% #Ordering the data
  select("code_tract", "Code_muni","R_0","R_0_5","R_1_0",
         "R_2_0","R_3_0","R_5_0","R_10_0",
         "R_15_0","R_20_0",
         "R_20mais_0")

census_tract_data <- census_tract_data %>%
  mutate(code_tract = as.numeric(as.character(code_tract))) %>%
  left_join(data_income %>% select(code_tract,R_0:R_20mais_0), by = "code_tract") %>%
  mutate(
    code_tract= as.factor(code_tract),
    across(R_0:R_20mais_0,~as.factor(ifelse(.x == "X" | is.na(.x), 0, .x)))
  )  %>%
  st_make_valid()

#Aggregate income data not computed - People under 10 years old
data_10_y<-data_gen_age_m %>% select(code_tract,V022, V035:V043)%>%
  mutate(across(V022:V043,~as.numeric(.x))) %>%
  mutate(Total_hom = select(., 2:11) %>% rowSums(na.rm = TRUE)) %>% select(code_tract,Total_hom)

data_10_y<-data_10_y %>% 
  left_join(data_gen_age_w %>% 
  select(code_tract,V022, V035:V043),by="code_tract") %>%
  mutate(across(V022:V043,~as.numeric(.x))) %>%
  mutate(Total_mul = select(., 3:12) %>% rowSums(na.rm = TRUE)) %>% select(code_tract,Total_hom,Total_mul) %>%
  mutate(Total_u10 = Total_hom+Total_mul)%>%
  mutate(Total_u10=ifelse(is.na(Total_u10),0,Total_u10))

census_tract_data <- census_tract_data %>%
  mutate(code_tract = as.numeric(as.character(code_tract))) %>%
  left_join(data_10_y %>% select(code_tract,Total_u10), by = "code_tract") %>%
  mutate(Total_u10=ifelse(is.na(Total_u10),0,Total_u10)) %>%
  mutate(
    code_tract= as.factor(code_tract),
    Total_u10=as.factor(Total_u10)) %>%
  st_make_valid()

#Aggregate head of the home data
names(data_hous_resp)<- c("code_tract", "Code_muni","Resp_R_0_5",
                              "Resp_R_1_0","Resp_R_2_0","Resp_R_3_0","Resp_R_5_0","Resp_R_10_0",
                              "Resp_R_15_0","Resp_R_20_0",
                              "Resp_R_20mais_0","Resp_R_0","Resp_masc","Resp_fem")

data_hous_resp<-data_hous_resp %>% #Ordering the data
  select("code_tract", "Code_muni","Resp_R_0","Resp_R_0_5",
         "Resp_R_1_0","Resp_R_2_0","Resp_R_3_0","Resp_R_5_0",
         "Resp_R_10_0",
         "Resp_R_15_0","Resp_R_20_0",
         "Resp_R_20mais_0","Resp_masc","Resp_fem")

census_tract_data <- census_tract_data %>%
  mutate(code_tract = as.numeric(as.character(code_tract))) %>%
  left_join(data_hous_resp %>% select(code_tract,Resp_R_0:Resp_fem), by = "code_tract") %>%
  mutate(
    code_tract= as.factor(code_tract),
    across(Resp_R_0:Resp_fem,~as.factor(ifelse(.x == "X" | is.na(.x), 0, .x)))
  )  %>%
  st_make_valid()

census_tract_data<-census_tract_data %>%
  mutate (across(Total_20:Total_w80,~as.numeric(as.character(sub(",", ".", sub(".", "", .x, fixed=TRUE), fixed=TRUE))))) %>%
  mutate (across(R_0:Total_u10,~as.numeric(as.character(sub(",", ".", sub(".", "", .x, fixed=TRUE), fixed=TRUE))))) %>%
  mutate (across(Resp_R_0:Resp_fem,~as.numeric(as.character(sub(",", ".", sub(".", "", .x, fixed=TRUE), fixed=TRUE))))) %>%
  st_drop_geometry()

census_tract_data<-sub_function(21,30,census_tract_data,by = 11)

#Remove Census Tract with no people
census_tract_data<-census_tract_data[rowSums(census_tract_data[,13:41]) != 0,]

#Adjust data for those not responsible for the household 
census_tract_data <- census_tract_data %>%
  mutate(Resp_dep = rowSums(across(Total_20:Total_w80,
                                   ~as.numeric(as.character(.x))))-Resp_masc-Resp_fem)

#Aggregate per capita income data
census_tract_data <- census_tract_data %>%
  mutate(code_tract = as.numeric(as.character(code_tract))) %>%
  left_join(data_income_pc, by = "code_tract") %>%
  mutate(
    code_tract= as.factor(code_tract),
    across(inc_pc_0:n_col,~as.factor(ifelse(.x == "X" | is.na(.x), 0, .x)))
  )

#Correcting the sum of individuals
census_tract_data<- census_tract_data %>% 
  #Adding to "n_col" sectors/individuals where per capita income  data was not collected
        mutate(income_total = rowSums(across(R_0:Resp_R_20mais_0,
                                             ~as.numeric(as.character(.x)))),
               income_pc_total = rowSums(across(inc_pc_0:inc_pc_2_plus,
                                                ~as.numeric(as.character(.x))))) %>%
  
        mutate(n_col = ifelse(income_pc_total==0,as.character(income_total),
                                                    as.character(n_col))) %>%
  #Adding to "n_col" sectors/individuals where percapita income  data was not fully collected
        mutate(income_pc_total = rowSums(across(inc_pc_0:n_col,
                                          ~as.numeric(as.character(.x))))) %>%
        mutate(n_col = as.factor(ifelse(income_pc_total<income_total,as.character(income_total-income_pc_total+as.numeric(n_col)),
                                  as.character(n_col))))

#Adding race data
names(data_race)<- c("code_tract",
                     "Code_muni",
                     "total_cor",
                     "brancos",
                     "pretos",
                     "amarelos",
                     "pardos",
                     "indigenas")
  
data_race<- data_race %>% 
            mutate(across(total_cor:indigenas,
                          ~as.numeric(as.character(.x)))) %>%
            mutate(total_sum_cor = rowSums(across(brancos:indigenas))) %>%
            select(code_tract,total_cor:total_sum_cor) %>%
            gather("cor","total",-c("code_tract","total_cor","total_sum_cor")) %>%
            mutate(perc = total/total_sum_cor) %>%
            drop_na() %>%
            group_by(code_tract) %>%
            #Correcting the difference 
            mutate(total_correct = sfsmisc::roundfixS(perc * total_cor)) %>%
            select(code_tract,cor,total_correct) %>%
            spread(cor,total_correct) %>%
            select(c("code_tract",
                     "brancos",
                     "pretos",
                     "amarelos",
                     "pardos",
                     "indigenas")) %>%
            #Grouping race classes 
            mutate(pard_am_ing = as.character(rowSums(across(amarelos:indigenas)))) %>%
            select(code_tract,brancos,pretos,pard_am_ing)

census_tract_data <- census_tract_data %>%
  mutate(code_tract = as.numeric(as.character(code_tract))) %>%
  left_join(data_race, by = "code_tract") %>%
  mutate(
    code_tract= as.factor(code_tract),
    across(brancos:pard_am_ing,~as.factor(ifelse(.x == "X" | is.na(.x), 0, as.character(.x)))
  ))

#Checking if variables are consistent
check_data<- census_tract_data %>% 
  mutate(across(Total_20:pard_am_ing,~as.numeric(as.character(.x)))) %>%
  mutate(age_total = rowSums(across(Total_20:Total_w80)),
         income_total = rowSums(across(R_0:Resp_R_20mais_0)),
         income_pc_total = rowSums(across(inc_pc_0:n_col)),
         income_2 = rowSums(across(R_3_0:Resp_R_20mais_0))+rowSums(across(Resp_R_3_0:Resp_R_20mais_0)),
         race_total = rowSums(across(brancos:pard_am_ing))) %>%
  select(code_tract,age_total,income_total,race_total,income_pc_total,income_2,inc_pc_2_plus)

check_data[check_data$age_total!=check_data$race_total,]

# Adjusting classes



# census_tract_data<- census_tract_data %>%
#   mutate(inc_pc_2 = as.factor(as.character(rowSums(across(inc_pc_0:inc_pc_2,
#                                        ~as.numeric(as.character(.x))))))) %>%
#   select(-c("inc_pc_0","inc_pc_1_4","inc_pc_1_2","inc_pc_1")) %>%
#   
#   mutate(R_5mais_0 = as.factor(as.character(rowSums(across(R_10_0:R_20mais_0,
#                                                           ~as.numeric(as.character(.x))))))) %>%
#   select(-c("R_10_0","R_15_0","R_20_0","R_20mais_0")) %>%
#   
#   mutate(Resp_R_5mais_0 = as.factor(as.character(rowSums(across(Resp_R_10_0:Resp_R_20mais_0,
#                                                            ~as.numeric(as.character(.x))))))) %>%
#   select(-c("Resp_R_10_0","Resp_R_15_0","Resp_R_20_0","Resp_R_20mais_0"))

# selec_cols<- colnames(census_tract_data)
# selec_cols<-selec_cols[c(1:26,45,27:33,46,34:44)]
# census_tract_data <- census_tract_data %>% select(selec_cols)

#Save
saveRDS(census_tract_data,"data/01_census/census_tract_data.RDS")

# 04. Set  microdata ------------------------------------------------------
census_micro<-readRDS("data/00_microdados/01_data_micro_p.RDS")

#Select variables
census_micro<- census_micro %>% select(V0011,V0502,V0601,V6036,V0606,V6528,V6532,V6400,V0616,id_g) %>% 
                                drop_na(V0011,V0502,V0601,V6036,V6532)

names(census_micro)<-c("APONDE","Resp","Sex","Idade","cor","Rend_pes",
                       "Rend_pc","educ","deficiencia","id_g")

census_micro <- census_micro %>% mutate(
  Rend_pes = as.numeric(ifelse(is.na(Rend_pes), 0, Rend_pes)),
  Rend_pc = as.numeric(ifelse(is.na(Rend_pc), 0, Rend_pc)),
  across(APONDE:cor,~as.factor(.x)),
  Resp= as.factor(ifelse(Resp==1,1,0)),
  across(educ:id_g,~as.factor(.x))
  )

perc_amostra<-nrow(census_micro)/sum(data_gen_age_m_col$V001+data_gen_age_w_col$V001) #Percentual da pop na amostra

#Adjusting classes
breaks_age<-c(seq(0,70,class_age),150)
breaks_age[1]<-(-1)

breaks_income<- c(-1,0,0.5,1,2,3,5,10,15,20,3000)
breaks_income_pc<- c(-1,0,0.25,0.5,1,2,3000000)

labels_income<- c("R_0","R_0_5","R_1_0",
                  "R_2_0","R_3_0","R_5_0","R_10_0","R_15_0","R_20_0","R_20mais_0")

labels_income_resp<- c("Resp_R_0","Resp_R_0_5",
                       "Resp_R_1_0","Resp_R_2_0","Resp_R_3_0","Resp_R_5_0","Resp_R_10_0",
                       "Resp_R_15_0","Resp_R_20_0","Resp_R_20mais_0")

labels_income_pc<- c("inc_pc_0","inc_pc_1_4","inc_pc_1_2","inc_pc_1","inc_pc_2",
                     "inc_pc_2_plus")

census_micro$age_sex<-""
census_micro<- census_micro %>%
  mutate(age_sex=as.character(age_sex)) %>%
  mutate(age_sex=ifelse(Sex==2,
                        as.character(cut(as.numeric(as.character(sub(",", ".", sub(".", "", Idade, fixed=TRUE), fixed=TRUE))),
                              breaks = breaks_age,
                              labels = sapply(c(seq(class_age,90,class_age)),
                                              function(x) paste0("Total_w", as.character(x))))),
                        as.character(cut(as.numeric(as.character(sub(",", ".", sub(".", "", Idade, fixed=TRUE), fixed=TRUE))),
                              breaks = breaks_age,
                              labels = sapply(c(seq(class_age,90,class_age)),
                                              function(x) paste0("Total_", as.character(x)))))
                        )
         ) %>%
  mutate(age_sex=as.factor(age_sex)) %>%
  mutate(renda_class=ifelse(as.numeric(as.character(sub(",", ".", sub(".", "", Idade, fixed=TRUE), fixed=TRUE)))>9,
                            as.character(cut(Rend_pes,
                                             breaks = breaks_income,
                                             labels = labels_income)),
                            "Total_u10"
           )) %>%
  mutate(renda_class=ifelse(is.na(renda_class),"R_0_5",renda_class)) %>%
  mutate(renda_class= ifelse(Resp==1,
                             as.character(cut(Rend_pes,
                                              breaks = breaks_income,
                                              labels = labels_income_resp)),
                             renda_class)) %>%
  mutate(resp_home=ifelse(Resp==1 & Sex==2,"Resp_fem",
                          ifelse(Resp==1 & Sex==1,"Resp_masc","Resp_dep"))) 
    
  # mutate(renda_class_pc = as.character(cut(Rend_pc,
  #                                     breaks = breaks_income_pc,
  #                                     labels = labels_income_pc))) 
  
census_micro<- census_micro %>% drop_na() %>% 
  select(c("APONDE","age_sex","renda_class","resp_home","renda_class_pc","cor",
           "Rend_pes","Rend_pc","educ","deficiencia","id_g")) %>%
  filter(cor!="9")


#Remove renda_class_pc if it causes problems in microsimulation
if(remove_income_pc){
census_micro<- census_micro %>% select(-c("renda_class_pc"))
}

#Adjusting race class
levels(census_micro$cor)<-c("brancos","pretos",rep("pard_am_ing",length(levels(census_micro$cor))-2))

#Adding clone census micro with n_col individuals
# perc_col<- sum(as.numeric(as.character(census_tract_data$n_col)))/sum(as.numeric(as.character(census_tract_data$inc_pc_2))+as.numeric(as.character(census_tract_data$inc_pc_2_plus)))
# 
# census_micro_clone<- census_micro %>% mutate(renda_class_pc = as.factor("n_col")) %>% sample_n(round(nrow(census_micro)*perc_col))
# census_micro<- rbind(census_micro,census_micro_clone)

#Save
saveRDS(census_micro,"data/00_microdados/census_micro_data_treated.RDS")

# 05. Set constraints data ------------------------------------------------

#View columns names
table(census_micro$age_sex)
table(census_micro$renda_class)
table(census_micro$resp_home)
#table(census_micro$renda_class_pc)
table(census_micro$cor)

colnames(census_cons_orig)

census_cons_orig<- census_tract_data %>% select(c("code_tract","CD_APONDE",
                                             colnames(census_tract_data)[13:20],
                                             colnames(census_tract_data)[21:30],
                                             colnames(census_tract_data)[32:41],
                                             colnames(census_tract_data)[31],
                                             colnames(census_tract_data)[42:44],
                                             colnames(census_tract_data)[45:51],
                                             colnames(census_tract_data)[54:56]
                                             
))
colnames(census_cons_orig)
#Remove renda_class_pc if it causes problems in microsimulation
if(remove_income_pc) {
  census_cons_orig <- census_cons_orig %>% select(-c("inc_pc_0",
                                                     "inc_pc_1_4", 
                                                     "inc_pc_1_2", 
                                                     "inc_pc_1",
                                                     "inc_pc_2",
                                                     "inc_pc_2_plus",
                                                     "n_col"))
}

# 06. Spatial microssimulation --------------------------------------------
census_cons<-census_cons_orig
puma_list<-unique(census_cons$CD_APONDE)

#Set
data_basico_census<-paste0("data/01_census/Basico_",char_state,".csv")
data_basico<- func_data_basico(data_basico_census)
pb <- progress_bar$new(total = length(puma_list))
options(dplyr.summarise.inform = FALSE)

start_time<-Sys.time()
print(start_time)

purrr::walk(puma_list,ifelse(remove_income_pc,microsim_func2,microsim_func))

end_time <- Sys.time()
time_lapse <- end_time - start_time
time_lapse_formatted <- format(time_lapse, format = "%H:%M:%S")
cat("Time lapse:", time_lapse_formatted, "\n")
beep(sound=8)

# 07. View results --------------------------------------------------------
list_files<-list.files("data/03_microsimulated2",
                       full.names = TRUE,
                       include.dirs = TRUE,
                       pattern = ".RDS")

#Microdata
microsim_data<- purrr::map_df(list_files[grepl("01_microsim_puma_",list_files)],
                              readRDS)
summary(microsim_data)

folder<-"data/03_microsimulated2/complete"
if (!file.exists(folder)) dir.create(folder)
write_rds(microsim_data,paste0(folder,"/","microsim_data2.RDS"))

#Internal validation
data_internal<- purrr::map_df(list_files[grepl("intv",list_files)],
                              readRDS)
summary(data_internal)

#External validation
data_external<- purrr::map_df(list_files[grepl("extv",list_files) & !grepl("comp",list_files)],
                              readRDS)
summary(data_external)






