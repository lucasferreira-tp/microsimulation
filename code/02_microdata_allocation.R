#Load packages
source('./fun/fun.R')


# 01. Read the data -------------------------------------------------------
#Set
city<-"Fortaleza"
char_state<-"CE"
code_state<-23
code_muni_<-2304400

#If it causes microsimulation problems
remove_income_pc<- TRUE


#Hex grid with data
grid_with_data<-grid_with_data(state = char_state,city_name = city)

#Microsimulation data
list_files<-list.files("data/03_microsimulated2",
                       full.names = TRUE,
                       include.dirs = TRUE,
                       pattern = ".RDS")
microsim_data<- purrr::map_df(list_files[grepl("01_microsim_puma_",list_files)],
                              readRDS)

#Census tract data
census_tract<- read_census_tract(code_tract =code_state,year = 2010) %>% 
                filter(code_muni==code_muni_) %>%
                st_transform(31984)

# 02. Preparing the data --------------------------------------------------

set.seed(300)
sample_sequence<-sample(nrow(microsim_data))
microsim_data<-microsim_data[sample_sequence,] 
row.names(microsim_data)<-1:nrow(microsim_data)

microsim_data_men<-microsim_data %>% filter (!grepl("w",age_sex))
microsim_data_wom<-microsim_data %>% filter (grepl("w",age_sex))

# 4. Allocate data to Hex -------------------------------------------------

#Hex by census tract
hex_tract <-
  hex_per_tract(
    state = char_state,
    hex_grid_path = paste0("data/02_shp/", tolower(substr(city, 1, 3)), "_res_9/grid_raw.rds"),
    microsim_data_path = "data/03_microsimulated2/complete/microsim_data2.RDS"
  )

#Allocate individuals to Hex
lista_setores<-unique(hex_tract$code_tract)
purrr::walk(lista_setores,allocate_ind)

beep(sound=8)

#Create micro database
list.files<-list.files(path = "data/03_microsimulated2/complete/allocated/",pattern = ".RDS",full.names=TRUE)

plan(multisession, workers = 14)
options(future.globals.maxSize = 700 * 1024 * 1024)

data.micro.final<-future_map_dfr(list.files,readRDS)
beep(sound=8)
plan(sequential)

if(remove_income_pc){

data.micro.final<- data.micro.final %>% 
                  select(id_g,code_tract,hex,age_sex,renda_class,
                         resp_home,educ,deficiencia,cor,Rend_pes,Rend_pc)
}else{
data.micro.final<- data.micro.final %>% 
    select(id_g,code_tract,hex,age_sex,renda_class,renda_class_pc,
           resp_home,educ,deficiencia,cor,Rend_pes,Rend_pc)                          
                         }

data.micro.final<- data.micro.final %>% 
  mutate (educ= cut(as.numeric(as.character(educ)), breaks = c(0,2,3,4,6),
                     labels=c("Baixo","Medio","Alto","ND")))
              # Educação
              
              #Baixo - Inferior ao ensino médio (nível 1)
              #Médio - Ensino médio completo (nível 2)
              #Alto - Superior completo (nível 3)

census_micro_complete<-readRDS("data/00_microdados/01_data_micro_p.RDS") %>% mutate(id_g = as.factor(as.character(id_g)))

data.micro.final<- data.micro.final %>% left_join(census_micro_complete ,by=c("id_g"))

saveRDS(data.micro.final,"data/03_microsimulated2/complete/data.micro.final2.RDS")
rm(data.micro.final)





# Graphical and statistical analysis --------------------------------------

#Income hex data

data_basico_census<-paste0("data/01_census/Basico_",char_state,".csv")
data_basico<- func_data_basico(data_basico_census) %>%
  select(code_tract,V002) %>%
  rename(population = V002)



data_income<- func_data_income(paste0("data/01_census/PessoaRenda_",char_state,".csv"))

data_tract<- data_basico %>% left_join(data_income,by=c("code_tract"))


data_tract <- read_census_tract(code_muni_, year = 2010) %>%
  left_join(data_tract, by = c("code_tract")) %>%
  select(population,total_income) %>%
  st_transform(31984) %>%
  st_make_valid()
colnames(data_tract)

hex_grid<-readRDS(paste0("data/02_shp/", tolower(substr(city, 1, 3)), "_res_9/grid_raw.rds"))%>%
  st_transform(31984)


hex_grid_<-st_interpolate_aw(data_tract, hex_grid, extensive = TRUE)

hex_grid<- hex_grid %>%
  st_join(hex_grid_,join=st_equals)%>%
  mutate(mean_income = round(total_income/population/510,2))


stat_grid<- st_read("data/02_shp/grade_id77/grade_id77.shp")%>%
st_transform(31984) %>% st_make_valid()
stat_grid<- stat_grid %>% select(POP) %>% mutate(area=st_area(.)) %>% filter(POP>=1)

mapview(hex_grid,zcol="mean_income")

hg_1<- st_interpolate_aw(stat_grid, hex_grid, extensive = TRUE)

hg_1<- hex_grid %>% st_join(hg_1,join=st_equals)

saveRDS(hex_grid %>%
          st_drop_geometry(),"data/02_shp/hex_grid_income.RDS")


hex_grid %>% filter(population>0) %>% mapView(zcol = "mean_income")
# 
# # Validation --------------------------------------------------------------
# 
# data.micro.final_test<- readRDS("data/03_microsimulated2/complete/data.micro.final2.RDS")%>%
#   #filter(renda_class!="Total_u10") %>%
#   select(id_g:Rend_pc,V6036)
# 
# data_basico_census<-paste0("data/01_census/Basico_",char_state,".csv")
# data_basico<- func_data_basico(data_basico_census) %>%
#   select(code_tract,renda_media09,renda_media11)
# 
# data.micro.final_test_2<- data.micro.final_test %>% 
#   filter(renda_class!="Total_u10")%>%
#   left_join(data_basico,by=c("code_tract")) %>%
#   group_by(code_tract) %>% 
#   summarise(n= n(),
#             mean_rend = mean(Rend_pes),
#             mean_rend_pc = mean(Rend_pc),
#             renda_media09 = mean(renda_media09)) %>%
#   mutate(dif = mean_rend-renda_media09,
#          dif_perc = dif/renda_media09*100)
# 
# summary(data.micro.final_test_2)
# cor(data.micro.final_test_2$mean_rend,data.micro.final_test_2$renda_media09)
# t.test(data.micro.final_test_2$mean_rend,data.micro.final_test_2$renda_media09)
# 
# #Graphs
# histogram_plot <- ggplot(data.micro.final_test_2, aes(x = dif_perc)) +
#   geom_histogram(binwidth = 1, fill = "white", color = "grey30", alpha = 0.7) +
#   labs(x = "Difference between modeled and observed mean income by code tract (%)",
#        y = "Frequency") +
#   theme_minimal()
# 
# # Create the boxplot plot
# boxplot_plot <- ggplot(data.micro.final_test_2, aes(y = dif_perc)) +
#   geom_boxplot(fill = "white", color = "grey70",width = 0.2) +
#   labs(x = "",
#        y = "Difference between modeled and observed mean income by code tract (%)") +
#   theme_void() +
#   coord_flip()
# 
# # Arrange the plots using patchwork
# combined_plots <-  boxplot_plot + histogram_plot +
#   patchwork::plot_layout(nrow =  2,heights = c(1,4))
# 
# # Display the combined plots
# print(combined_plots)


