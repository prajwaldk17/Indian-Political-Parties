rm(list = ls(all = TRUE))


library(data.table)
library(igraph)
library(stringr)
library(splitstackshape)
library(dplyr)

############## Data prep

#Imprting data
rainfall = fread(file = "rain_information.csv", header = TRUE)
districts = fread(file = "district_information.csv", header = TRUE)
borders=fread(file = "border_information.csv", header = TRUE)

Election_years<-sort(unique(districts$year))

#Creating variable for election#
rainfall$Election_period<- ifelse(rainfall$year<=Election_years[1],0,
                                  ifelse(rainfall$year>Election_years[1] & rainfall$year<=Election_years[2],1,
                                  ifelse(rainfall$year>Election_years[2] & rainfall$year<=Election_years[3],2,
                                  ifelse(rainfall$year>Election_years[3] & rainfall$year<=Election_years[4],3,
                                  ifelse(rainfall$year>Election_years[4] & rainfall$year<=Election_years[5],4,
                                  ifelse(rainfall$year>Election_years[5] & rainfall$year<=Election_years[6],5,
                                  ifelse(rainfall$year>Election_years[6] & rainfall$year<=Election_years[7],6,
                                  ifelse(rainfall$year>Election_years[7] & rainfall$year<=Election_years[8],7,
                                  ifelse(rainfall$year>Election_years[8] & rainfall$year<=Election_years[9],8,
                                  ifelse(rainfall$year>Election_years[9] & rainfall$year<=Election_years[10],9,
                                  ifelse(rainfall$year>Election_years[10] & rainfall$year<=Election_years[11],10,
                                  ifelse(rainfall$year>Election_years[11] & rainfall$year<=Election_years[12],11,
                                  ifelse(rainfall$year>Election_years[12] & rainfall$year<=Election_years[13],12,13)))))))))))))

#Getting required stats for each district per election period
data.table(rainfall)
Avg_rainfall<-rainfall %>% 
  group_by(district,Election_period) %>%
  summarise(year = max(year),total_rainfall=sum(rain),spi_avg=mean(spi),number_of_election_yrs=n())

#Merging required tables
merged_rainfall<-merge(x=districts,y=Avg_rainfall)

#Scatter plot
plot(merged_rainfall$new_parties,merged_rainfall$total_rainfall)
plot(merged_rainfall$new_parties,merged_rainfall$spi_avg)
########### 1b ##########################
#Merging and summarising to get desired dataset
neighbors_rainfall<-merge(x=borders,y=rainfall,allow.cartesian = TRUE)
neighbors_rainfall_2<-neighbors_rainfall %>% 
  group_by(focal_district,Election_period) %>%
  summarise(year = max(year),total_rainfall_neighbors=sum(rain),spi_avg_neighbors=mean(spi))
colnames(neighbors_rainfall_2)<-c("district","Election_period","year","total_rainfall_neighbors","spi_avg_neighbors")

#Creating lag variable and cleaning data
merged_rainfall_neighbors<-merge(x=merged_rainfall,y=neighbors_rainfall_2)
merged_rainfall_neighbors$previous_rainfall_neighbor<-lag(merged_rainfall_neighbors$total_rainfall_neighbors,1)
merged_rainfall_neighbors$previous_rainfall<-lag(merged_rainfall_neighbors$total_rainfall,1)
merged_rainfall_neighbors$previous_district<-lag(merged_rainfall_neighbors$district,1)
merged_rainfall_neighbors$previous_rainfall_2<-ifelse(merged_rainfall_neighbors$district==merged_rainfall_neighbors$previous_district,merged_rainfall_neighbors$previous_rainfall,0)
merged_rainfall_neighbors_2 <-merged_rainfall_neighbors[merged_rainfall_neighbors$previous_rainfall_2>0]

install.packages("plm")
library(plm)
#plm(outcome variable predictor variables, data, effect = "twoways", model = "within", index = "district")
#summary(plm(total_rainfall~previous_rainfall+previous_rainfall_neighbor, data=merged_rainfall_neighbors_2, effect = "twoways", model = "within", 
            #index = c("district","year","Election_period.y")))
#Thus current rainfall is dependent on previous year rainfall and neighbors previous rainfall
summary(plm(total_rainfall~previous_rainfall+previous_rainfall_neighbor+number_of_election_yrs, data=merged_rainfall_neighbors_2, effect = "twoways", model = "within", 
            index = c("district","Election_period.y", "number_of_election_yrs")))
# Residuals:
#   Min.   1st Qu.    Median   3rd Qu.      Max. 
# -9174.327  -432.199   -37.287   465.402  6474.218 
# 
# Coefficients:
#   Estimate Std. Error t-value  Pr(>|t|)    
# previous_rainfall          0.250420   0.026765  9.3563 < 2.2e-16 ***
#   previous_rainfall_neighbor 0.017121   0.005386  3.1787  0.001507 ** 
############# 1c ##############
#Creating extreme weather flag
rainfall$drought_flood_flag<-ifelse(abs(rainfall$spi)>1,1,0)
#Summarising to get desired stats
rainfall_df<-rainfall %>% 
  group_by(district,Election_period) %>%
  summarise(year = max(year),total_flood_drought=sum(drought_flood_flag))

#Merging required tables
merged_rainfall_df<-merge(x=merged_rainfall_neighbors_2,y=rainfall_df)
#merged_rainfall_df_2<-merged_rainfall_df[merged_rainfall_df$total_flood_drought>0]
install.packages("pglm")
library(pglm)
summary(pglm(total_flood_drought~previous_rainfall+previous_rainfall_neighbor, data=merged_rainfall_df, effect = "twoways", model = "within", 
     index =c("district","Election_period.y"), family = "poisson"))
#summary(pglm(total_flood_drought~previous_rainfall+previous_rainfall_neighbor+number_of_election_yrs, data=merged_rainfall_df, effect = "twoways", model = "within", 
             #index =c("district","Election_period.y"), family = "poisson"))

# Log-Likelihood: -1919.067 
# 2  free parameters
# Estimates:
#   Estimate Std. error t value  Pr(> t)    
# previous_rainfall           7.366e-05  1.942e-05   3.792 0.000149 ***
#   previous_rainfall_neighbor -1.621e-05  3.943e-06  -4.111 3.95e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

############## Question 2 ####################
summary(pglm(new_parties~total_flood_drought+number_of_election_yrs, data=merged_rainfall_df, effect = "twoways", model = "within", 
             index =c("district","Election_period.y","number_of_election_yrs"), family = "poisson"))

# Estimates:
#   Estimate Std. error t value  Pr(> t)    
# total_flood_drought      0.05011    0.02016   2.486 0.012926 *  
#   number_of_election_yrs2  0.75994    0.23087   3.292 0.000996 ***
#   number_of_election_yrs3  1.04316    0.23074   4.521 6.15e-06 ***
#   number_of_election_yrs4  0.52083    0.22908   2.274 0.022989 *  
#   number_of_election_yrs5  0.51120    0.23350   2.189 0.028578 *  
#   number_of_election_yrs6 -0.05500    0.24239  -0.227 0.820509    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Caste_parties
summary(pglm(new_parties_caste~total_flood_drought+number_of_election_yrs, data=merged_rainfall_df, effect = "twoways", model = "within", 
             index =c("district","Election_period.y"), family = "poisson"))
# Estimate Std. error t value Pr(> t)    
# total_flood_drought    -0.06418    0.05789  -1.109   0.268    
# number_of_election_yrs -0.37315    0.03474 -10.742  <2e-16 ***

#Socialist_parties
summary(pglm(new_parties_socialist~total_flood_drought+number_of_election_yrs, data=merged_rainfall_df, effect = "twoways", model = "within", 
             index =c("district","Election_period.y"), family = "poisson"))
# Estimates:
#   Estimate Std. error t value  Pr(> t)    
# total_flood_drought     0.13015    0.04762   2.733  0.00628 ** 
#   number_of_election_yrs -0.25885    0.03182  -8.136 4.09e-16 ***

#Communist_parties
summary(pglm(new_parties_communist~total_flood_drought+number_of_election_yrs, data=merged_rainfall_df, effect = "twoways", model = "within", 
             index =c("district","Election_period.y"), family = "poisson"))
# Estimates:
#   Estimate Std. error t value Pr(> t)
# total_flood_drought    -0.10148    0.07080  -1.433   0.152
# number_of_election_yrs -0.03140    0.04516  -0.695   0.487

#Secular
summary(pglm(new_parties_secular~total_flood_drought+number_of_election_yrs, data=merged_rainfall_df, effect = "twoways", model = "within", 
             index =c("district","Election_period.y"), family = "poisson"))
# Estimates:
#   Estimate Std. error t value Pr(> t)    
# total_flood_drought     0.20915    0.08054   2.597 0.00941 ** 
#   number_of_election_yrs -0.32077    0.05297  -6.056 1.4e-09 ***

#Nationalist,
summary(pglm(new_parties_nationalist~total_flood_drought+number_of_election_yrs, data=merged_rainfall_df, effect = "twoways", model = "within", 
             index =c("district","Election_period.y"), family = "poisson"))
#total_flood_drought    -0.02349    0.10095  -0.233    0.816 

#Liberal
summary(pglm(new_parties_liberal~total_flood_drought+number_of_election_yrs, data=merged_rainfall_df, effect = "twoways", model = "within", 
             index =c("district","Election_period.y"), family = "poisson"))
#otal_flood_drought    -0.04507    0.03770  -1.196   0.232

#Religious

summary(pglm(new_parties_religious~total_flood_drought+number_of_election_yrs, data=merged_rainfall_df, effect = "twoways", model = "within", 
             index =c("district","Election_period.y"), family = "poisson"))
#total_flood_drought     0.15336    0.12924   1.187   0.235

#Farleft,Farright
summary(pglm(new_parties_farleft~total_flood_drought+number_of_election_yrs, data=merged_rainfall_df, effect = "twoways", model = "within", 
             index =c("district","Election_period.y"), family = "poisson"))
#total_flood_drought     0.06633    0.03871   1.713   0.0866 .  
summary(pglm(new_parties_farright~total_flood_drought+number_of_election_yrs, data=merged_rainfall_df, effect = "twoways", model = "within", 
             index =c("district","Election_period.y"), family = "poisson"))
#otal_flood_drought    -0.005927   0.091353  -0.065    0.948 

#It is more likely that Socialist, secular or farleft parties are formed after a period of drought or flood
############################################


#########  Question 3############################################
#Merging tables to get desired data
neighbors_rainfall<-merge(x=borders,y=rainfall,allow.cartesian = TRUE)
neighbors_rainfall_drfl<-neighbors_rainfall %>% 
  group_by(focal_district,Election_period) %>%
  summarise(year = max(year),total_drfl=sum(drought_flood_flag))
colnames(neighbors_rainfall_drfl)<-c("district","Election_period","year","total_dfl")

#Creating lag and cleaning
merged_drfl_neighbors<-merge(x=merged_rainfall,y=neighbors_rainfall_drfl)
merged_drfl_neighbors$previous_neighbor_drfl<-shift(merged_drfl_neighbors$total_dfl,1)
merged_drfl_neighbors$previous_district<-shift(merged_drfl_neighbors$district,1)
merged_drfl_neighbors$previous_neighbor_drfl_2<-ifelse(merged_drfl_neighbors$district==merged_drfl_neighbors$previous_district,merged_drfl_neighbors$previous_neighbor_drfl,0)
merged_drfl_neighbors_2 <-merged_drfl_neighbors[merged_drfl_neighbors$previous_neighbor_drfl_2>0]



summary(pglm(new_parties~previous_neighbor_drfl+total_dfl, data=merged_drfl_neighbors_2, effect = "twoways", model = "within", 
              index =c("district","Election_period.y" ,"number_of_election_yrs"), family = "poisson"))
# 
# Estimates:
#   Estimate Std. error t value Pr(> t)   
# previous_neighbor_drfl  0.017331   0.005893   2.941 0.00327 **
#   total_dfl              -0.008428   0.005172  -1.629 0.10321  

#############  Question 4 #############################

summary(pglm(political_concentration~previous_neighbor_drfl+total_dfl, data=merged_drfl_neighbors_2, effect = "twoways", model = "within", 
             index =c("district","number_of_election_yrs"), family = "poisson"))
# Estimates:
# Estimate Std. error t value Pr(> t)    
# previous_neighbor_drfl 0.0212998  0.0001816   117.3  <2e-16 ***
#   total_dfl              0.0260027  0.0001714   151.7  <2e-16 ***


############### Question 5 #####################
new_parties_csv = fread(file = "new_parties_in_each_district_by_candidate.csv", header = TRUE)
new_parties_csv$Election_period<- ifelse(new_parties_csv$year<=Election_years[1],0,
                                         ifelse(new_parties_csv$year>Election_years[1] & new_parties_csv$year<=Election_years[2],1,
                                                ifelse(new_parties_csv$year>Election_years[2] & new_parties_csv$year<=Election_years[3],2,
                                                       ifelse(new_parties_csv$year>Election_years[3] & new_parties_csv$year<=Election_years[4],3,
                                                              ifelse(new_parties_csv$year>Election_years[4] & new_parties_csv$year<=Election_years[5],4,
                                                                     ifelse(new_parties_csv$year>Election_years[5] & new_parties_csv$year<=Election_years[6],5,
                                                                            ifelse(new_parties_csv$year>Election_years[6] & new_parties_csv$year<=Election_years[7],6,
                                                                                   ifelse(new_parties_csv$year>Election_years[7] & new_parties_csv$year<=Election_years[8],7,
                                                                                          ifelse(new_parties_csv$year>Election_years[8] & new_parties_csv$year<=Election_years[9],8,
                                                                                                 ifelse(new_parties_csv$year>Election_years[9] & new_parties_csv$year<=Election_years[10],9,
                                                                                                        ifelse(new_parties_csv$year>Election_years[10] & new_parties_csv$year<=Election_years[11],10,
                                                                                                               ifelse(new_parties_csv$year>Election_years[11] & new_parties_csv$year<=Election_years[12],11,
                                                                                                                      ifelse(new_parties_csv$year>Election_years[12] & new_parties_csv$year<=Election_years[13],12,13)))))))))))))
#Summarising and merging tables to get required data
new_parties<-new_parties_csv %>% 
group_by(district,Election_period,party_name) %>%
  summarise(year = max(year))
colnames(new_parties)<-c("focal_district","Election_period","party_name","year")

new_parties_neighbors<-merge(x=borders,y=new_parties,allow.cartesian = TRUE)
new_parties_2=new_parties
colnames(new_parties_2)<-c("district","Election_period","party_name_2","year_2")
new_parties_neighbors_2<-merge(x=new_parties_neighbors,y=new_parties_2,
                               by.x="district",by.y ="district", allow.cartesian = TRUE)

#Check to see if desired table is obtained
#test<-new_parties_neighbors_2[new_parties_neighbors_2$focal_district=="Adilabad"]
#unique(test$district)
#unique(test$party_name)
#unique(test$party_name_2)

new_parties_neighbors_3<-new_parties_neighbors_2[new_parties_neighbors_2$Election_period.x>new_parties_neighbors_2$Election_period.y]
new_parties_neighbors_3$flag<-ifelse(new_parties_neighbors_3$party_name==new_parties_neighbors_3$party_name_2,1,0)
new_parties_neighbors_4<-new_parties_neighbors_3 %>% 
  group_by(focal_district,Election_period.x,party_name) %>%
  summarise(year = max(year),same_parties=sum(flag))
new_parties_neighbors_4$same_parties_flag<-ifelse(new_parties_neighbors_4$same_parties>0,1,0)

new_parties_neighbors_5<-new_parties_neighbors_4 %>% 
  group_by(focal_district,Election_period.x) %>%
  summarise(year = max(year),same_parties_count=sum(same_parties_flag))

colnames(new_parties_neighbors_5)<-c("district","Election_period.x","year","same_parties_count")


new_parties_neighbors_fnl<-merge(x=merged_drfl_neighbors_2,y=new_parties_neighbors_5)
#brand new parties count
new_parties_neighbors_fnl$new_parties_count<-new_parties_neighbors_fnl$new_parties-new_parties_neighbors_fnl$same_parties_count


summary(pglm(new_parties_count~previous_neighbor_drfl+total_dfl+number_of_election_yrs, data=new_parties_neighbors_fnl, effect = "twoways", model = "within", 
             index =c("district","Election_period.y","number_of_election_yrs"), family = "poisson"))
# Estimate Std. error t value Pr(> t)   
# previous_neighbor_drfl  -0.002199   0.007620  -0.289 0.77292   
# total_dfl                0.006692   0.007110   0.941 0.34661   
# number_of_election_yrs2 -0.208647   0.294679  -0.708 0.47892   
# number_of_election_yrs3  0.311142   0.273789   1.136 0.25578   
# number_of_election_yrs4 -0.307444   0.291641  -1.054 0.29180   
# number_of_election_yrs5 -0.300995   0.280497  -1.073 0.28324   
# number_of_election_yrs6 -0.784656   0.291792  -2.689 0.00716 **

summary(pglm(same_parties_count~previous_neighbor_drfl+total_dfl+number_of_election_yrs, data=new_parties_neighbors_fnl, effect = "twoways", model = "within", 
             index =c("district","Election_period.y"), family = "poisson"))
# #                         Estimate Std. error t value  Pr(> t)    
# previous_neighbor_drfl -0.021476   0.010092  -2.128   0.0333 *  
#   total_dfl              -0.008873   0.011464  -0.774   0.4389    
# number_of_election_yrs -0.169452   0.034280  -4.943 7.69e-07 ***
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1