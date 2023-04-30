library(tidyr)
library(dplyr)
library(ggplot2)
library(moments)
library(caret)
install.packages("zoo")
install.packages("strucchange")

library(zoo)

df <- NDAP
sdp <- SDP
gini <- GINI
# ID
remove <- select(df, -ROWID)
dupli <- distinct(remove)
df_sum <- dupli %>%
  group_by(District, SourceYear, State) %>%
  summarize(Reservoir.Water.Level = sum(Reservoir.Water.Level)) %>%
  mutate(District_Year = paste(District, SourceYear, sep = "_"))
# SDP Merging
df_sum$State_Year = paste(df_sum$State, df_sum$SourceYear, sep="_")
sdp <- sdp %>%
  separate(YEAR, c("Year1", "Year2"), sep = "-") %>%
  mutate(Year1 = as.integer(Year1),
         Year2 = as.integer(Year2))
sdp <- sdp %>% 
  mutate_if(is.character, as.integer) %>%
  mutate_if(is.numeric, as.integer)
sdp_long <- sdp %>%
  pivot_longer(cols = -c(Year1, Year2), names_to = "States", values_to = "SDP") %>%
  mutate(States = gsub("\\.", " ", States))
sdp_long$State_Year2 = paste(sdp_long$States, sdp_long$Year1, sep="_")
merged_df <- df_sum %>%
  inner_join(sdp_long, by = c("State_Year" = "State_Year2")) %>%
  select(State, District, SourceYear, Reservoir.Water.Level, District_Year, SDP)
# Gini Index Merging
gini <- subset(gini, select = c(-X, -X.1))
gini <- na.omit(gini)
gini$District <- toupper(iconv(gini$District, from = "UTF-8", to = "ASCII", sub = ""))
merged = merge(x = merged_df, y = gini, by = "District")
merged <- subset(merged, select = c(-Sno))
write.csv(merged, "C:/Users/aditi/Downloads/DATASETS/Dataset.csv", row.names=FALSE)

#importing Data
df <- read.csv("C:/Users/aditi/Downloads/DATASETS/group_25.csv")
View(df)
#q5
# summary
summary(df)

# histograms
hist(df$Reservoir.Water.Level, main="Reservoir Water Level Histogram", xlab="Reservoir Water Level", ylab="Frequency")
hist(df$SDP, main="SDP Histogram", xlab="SDP", ylab="Frequency")
hist(df$Gini, main="Gini Index Histogram", xlab="Gini Index", ylab="Frequency")

# box plot & Outliers
boxplot(df$Reservoir.Water.Level, main="Reservoir Water Level BoxPlot")
abline(h=boxplot.stats(df$Reservoir.Water.Level)$out, col="red") +
mtext(paste("Skewness:", round(moments::skewness(df$Reservoir.Water.Level, na.rm = TRUE),2)), side=3, line=-2)
boxplot(df$SDP, main="SDP Box Plot")
abline(h=boxplot.stats(df$SDP)$out, col="red") +
mtext(paste("Skewness:", round(moments::skewness(df$SDP, na.rm = TRUE),2)), side=3, line=-2)
boxplot(df$Gini, main="Gini Index Box Plot")
abline(h=boxplot.stats(df$Gini)$out, col="red") +
mtext(paste("Skewness:", round(moments::skewness(df$Gini, na.rm = TRUE),2)), side=3, line=-2)

#q6
sdplm<-lm(formula = Reservoir.Water.Level ~ SDP, data = df,na.action = na.exclude)
summary(sdplm)

#Q7
residual <- resid(sdplm)

#plotting environmental quality indicator on Y-axis and SDP on the X-axis
plot(df$SDP,df$Reservoir.Water.Level, ylab='EQI', xlab='SDP')

#ûi,t on Y-axis and SDP on x-axis
plot(df$SDP,residual, ylab='û', xlab='SDP')

#predicted values of the environmental quality indicator on Y-axis and true values of the environmental quality indicator on X-axis.
plot(df$Reservoir.Water.Level,predict(sdplm), ylab='predicted Values', xlab='Actual Values')

#q8
hist(residual,xlab = "û",col = "red",border = "black")
check <- data.frame(residual)
sum(residual,na.rm=TRUE)

#Q9
SDP2 <-df$SDP^2
finaldata <- cbind(df,SDP2)
SDP3 <-finaldata$SDP^3
finaldata <- cbind(finaldata,SDP3)
cols <- c("SDP", "SDP2", "SDP3")
process <- preProcess(finaldata[cols], method = c("range"))
norm_scale <- predict(process, finaldata[cols])
data_scaled <- cbind(norm_scale, finaldata[setdiff(names(finaldata), cols)])
sdplm_new<-lm(formula = Reservoir.Water.Level ~ SDP + SDP2 + SDP3 + Gini, data = data_scaled,na.action = na.exclude)
summary(sdplm_new)

# PART 2

check <- group_25
check <- check %>% select(-State_yearID, -ID,-District)
check <- check %>% group_by(State, SourceYear) %>% summarise_all(mean)

lit <- Literacy
merged_df <- merge(check, lit, by = c("State", "SourceYear"), all.x=TRUE)
merged_df$Literacy <- na.locf(merged_df$Literacy)
merged_df <- merged_df %>% select(-sno)

rain <- rainfall
rain$State <- gsub('Coastal Andhra Pradesh', 'Andhra Pradesh', rain$State)
rain$State <- gsub('Gujarat Region', 'Gujarat', rain$State)
rain$State <- gsub('Coastal Karnataka', 'Karnataka', rain$State)
rain$State <- gsub('West Madhya Pradesh', 'Madhya Pradesh', rain$State)
rain$State <- gsub('Madhya Maharashtra', 'Maharashtra', rain$State)
rain$State <- gsub('West Rajasthan', 'Rajasthan', rain$State)
rain$State <- gsub('East Uttar Pradesh', 'Uttar Pradesh', rain$State)
rain$State <- gsub('Sub Himalayan West Bengal & Sikkim', 'West Bengal', rain$State)

rain <- rain %>% mutate(State = toupper(State))
rain <- subset(rain, SourceYear>2010)
merged_df <- merge(merged_df, rain, by = c("State", "SourceYear"), all.x=TRUE)
merged_df$Rainfall <- na.locf(merged_df$Rainfall)

hydro <- hydropower
names(hydro) <- as.character(unlist(hydro[1,]))
hydro <- hydro[-1,]

hydro <- hydro %>% 
  pivot_longer(cols = -State, names_to = "SourceYear", values_to = "HydroPower") %>% 
  mutate(SourceYear = as.integer(SourceYear))

hydro <- hydro %>% mutate(State = toupper(State))
merged_df <- merge(merged_df, hydro, by = c("State", "SourceYear"), all.x=TRUE)
merged_df$HydroPower <- na.locf(merged_df$HydroPower, na.rm = FALSE)
View(merged_df)

gdpPerCapita <- GDP
names(gdpPerCapita) <- as.character(unlist(gdpPerCapita[1,]))
gdpPerCapita <- gdpPerCapita[-1,]

gdpPerCapita <- gdpPerCapita %>% 
  pivot_longer(cols = -State, names_to = "SourceYear", values_to = "PerCapitaIncome") %>% 
  mutate(SourceYear = as.integer(SourceYear))

gdpPerCapita <- gdpPerCapita %>% mutate(State = toupper(State))
merged_df <- merge(merged_df, gdpPerCapita, by = c("State", "SourceYear"), all.x=TRUE)
merged_df$PerCapitaIncome <- na.locf(merged_df$PerCapitaIncome, na.rm = FALSE)
merged_df$PerCapitaIncome <- gsub("\\₹|,", "", merged_df$PerCapitaIncome)
merged_df$PerCapitaIncome <- as.integer(merged_df$PerCapitaIncome)

mor <- Mortal
names(mor) <- as.character(unlist(mor[1,]))
mor <- mor[-1,]

mor <- mor %>% 
  pivot_longer(cols = -State, names_to = "SourceYear", values_to = "MortalityRate") %>% 
  mutate(SourceYear = as.integer(SourceYear))

mor <- mor %>% mutate(State = toupper(State))
merged_df <- merge(merged_df, mor, by = c("State", "SourceYear"), all.x=TRUE)
merged_df$MortalityRate <- na.locf(merged_df$MortalityRate)

unem <- Unemploy
unem$V3 <- as.integer(unem$V3)
unem$V5 <- as.integer(unem$V5)
names(unem) <- as.character(unlist(unem[1,]))
unem <- unem[-1,]

unem <- unem %>% 
  pivot_longer(cols = -State, names_to = "SourceYear", values_to = "Unemployment") %>% 
  mutate(SourceYear = as.integer(SourceYear))

unem <- unem %>% mutate(State = toupper(State))
merged_df <- merge(merged_df, unem, by = c("State", "SourceYear"), all.x=TRUE)
merged_df$Unemployment <- na.locf(merged_df$Unemployment)

eep <- EEP
eep <- eep %>% mutate(State = toupper(State))
merged_df <- merge(merged_df, eep, by = c("State", "SourceYear"), all.x=TRUE)
merged_df$EEP <- na.locf(merged_df$EEP, na.rm = FALSE)

gini <- ginistate
gini <- gini %>% mutate(State = toupper(State))
merged_df <- merge(merged_df, gini, by = c("State", "SourceYear"), all.x=TRUE)
merged_df$GiniState <- na.locf(merged_df$GiniState, na.rm = FALSE)

View(merged_df)
write.csv(merged_df, "C:/Users/aditi/OneDrive/Documents/DATASETS/final.csv", row.names=FALSE)

finaldata <- merged_df
#chow test
library(strucchange)

sctest(finaldata$Reservoir.Water.Level ~ finaldata$Literacy , type = "Chow")

sctest(finaldata$Reservoir.Water.Level ~ finaldata$Rainfall , type = "Chow")

sctest(finaldata$Reservoir.Water.Level ~ finaldata$HydroPower , type = "Chow")

sctest(finaldata$Reservoir.Water.Level ~ finaldata$PerCapitaIncome , type = "Chow")

sctest(finaldata$Reservoir.Water.Level ~ finaldata$Unemployment , type = "Chow")

sctest(finaldata$Reservoir.Water.Level ~ finaldata$MortalityRate , type = "Chow")

sctest(finaldata$Reservoir.Water.Level ~ finaldata$EEP , type = "Chow")


#T-Test
t.test(finaldata$Reservoir.Water.Level,finaldata$Literacy)

t.test(finaldata$Reservoir.Water.Level,finaldata$Rainfall)

t.test(finaldata$Reservoir.Water.Level,finaldata$HydroPower)

t.test(finaldata$Reservoir.Water.Level,finaldata$PerCapitaIncome)

t.test(finaldata$Reservoir.Water.Level,finaldata$Unemployment)

t.test(finaldata$Reservoir.Water.Level,finaldata$MortalityRate)

t.test(finaldata$Reservoir.Water.Level,finaldata$EEP)

SDP2 <-finaldata$SDP^2
finaldata2 <- cbind(finaldata,SDP2)
SDP3 <-finaldata2$SDP^3
finaldata2 <- cbind(finaldata2,SDP3)
cols <- c("SDP", "SDP2", "SDP3")
process <- preProcess(finaldata2[cols], method = c("range"))
norm_scale <- predict(process, finaldata2[cols])
data_scaled <- cbind(norm_scale, finaldata2[setdiff(names(finaldata2), cols)])
sdplm_new<-lm(formula = Reservoir.Water.Level ~ SDP + SDP2 + SDP3 + GiniState, data = data_scaled,na.action = na.exclude)
summary(sdplm_new)

sdplm<-lm(formula = Reservoir.Water.Level ~ SDP + GiniState + Literacy + Rainfall + HydroPower + PerCapitaIncome + MortalityRate + Unemployment + EEP,data = finaldata,na.action = na.exclude)
summary(sdplm)

#Q7
#for south
library(dplyr)
final_zonal_data<- finaldata %>%
  mutate(Dsouth = case_when(State=="ANDHRA PRADESH" ~ 1,
                            State=="KERALA" ~ 1,
                            State=="TAMIL NADU" ~ 1,
                            State=="TELANGANA" ~ 1,
                            State=="KARNATAKA" ~ 1,
                            TRUE ~ 0))
final_zonal_data$Dsouth <-as.numeric(final_zonal_data$Dsouth)

unique(finaldata$State)

#for northeast
final_zonal_data<- final_zonal_data %>%
  mutate(Dnortheast = case_when(State=="Assam" ~ 1,
                                State=="Manipur" ~ 1,
                                State=="Tripura" ~ 1,
                                State=="Sikkim" ~ 1,
                                State=="Meghalaya" ~ 1,
                                State=="NAGALAND" ~ 1,
                                State=="Arunachal Pradesh" ~ 1,
                                State=="Mizoram" ~ 1,
                                TRUE ~ 0))
final_zonal_data$Dnortheast <-as.numeric(final_zonal_data$Dnortheast)

#for centre
final_zonal_data<- final_zonal_data %>%
  mutate(Dcentre = case_when(State=="MADHYA PRADESH" ~ 1,
                             State=="Delhi" ~ 1,
                             State=="CHHATTISGARH" ~ 1,
                             TRUE ~ 0))
final_zonal_data$Dcentre <-as.numeric(final_zonal_data$Dcentre)

#for west
final_zonal_data<- final_zonal_data %>%
  mutate(Dwest = case_when(State=="Goa" ~ 1,
                           State=="MAHARASHTRA" ~ 1,
                           State=="RAJASTHAN" ~ 1,
                           State=="GUJARAT" ~ 1,
                           TRUE ~ 0))
final_zonal_data$Dwest <-as.numeric(final_zonal_data$Dwest)

#for north
final_zonal_data<- final_zonal_data %>%
  mutate(Dnorth = case_when(State=="Jammu and Kashmir" ~ 1,
                            State=="UTTAR PRADESH" ~ 1,
                            State=="HIMACHAL PRADESH" ~ 1,
                            State=="UTTARAKHAND" ~ 1,
                            State=="Haryana" ~ 1,
                            State=="Punjab" ~ 1,
                            TRUE ~ 0))
final_zonal_data$Dnorth <-as.numeric(final_zonal_data$Dnorth)

#for east
final_zonal_data<- final_zonal_data %>%
  mutate(Deast = case_when(State=="WEST BENGAL" ~ 1,
                           State=="ODISHA" ~ 1,
                           State=="JHARKHAND" ~ 1,
                           State=="Bihar" ~ 1,
                           TRUE ~ 0))
final_zonal_data$Deast <-as.numeric(final_zonal_data$Deast)


data_south <- subset(final_zonal_data, Dsouth == 1)
data_east <- subset(final_zonal_data, Deast == 1)
data_west <- subset(final_zonal_data, Dwest == 1)
data_north <- subset(final_zonal_data, Dnorth == 1)
data_northeast <- subset(final_zonal_data, Dnortheast == 1)
data_centre <- subset(final_zonal_data, Dcentre == 1)



model_south<-lm(formula = Reservoir.Water.Level ~ SDP + SDP2 + SDP3 + Gini +Literacy+HydroPower+PerCapitaIncome+Unemployment+EEP+Dsouth, data = final_zonal_data,na.action = na.exclude)

model_north<-lm(formula = Reservoir.Water.Level ~ SDP + SDP2 + SDP3 + Gini +Literacy+HydroPower+PerCapitaIncome+Unemployment+EEP+Dnorth, data = final_zonal_data,na.action = na.exclude)

model_east<-lm(formula = Reservoir.Water.Level ~ SDP + SDP2 + SDP3 + Gini +Literacy+HydroPower+PerCapitaIncome+Unemployment+EEP+Deast, data = final_zonal_data,na.action = na.exclude)

model_west<-lm(formula = Reservoir.Water.Level ~ SDP + SDP2 + SDP3 + Gini +Literacy+HydroPower+PerCapitaIncome+Unemployment+EEP+Dwest, data = final_zonal_data,na.action = na.exclude)

model_northeast<-lm(formula = Reservoir.Water.Level ~ SDP + SDP2 + SDP3 + Gini +Literacy+HydroPower+PerCapitaIncome+Unemployment+EEP+Dnortheast, data = final_zonal_data,na.action = na.exclude)

model_centre<-lm(formula = Reservoir.Water.Level ~ SDP + SDP2 + SDP3 + Gini +Literacy+HydroPower+PerCapitaIncome+Unemployment+EEP+Dcentre, data = final_zonal_data,na.action = na.exclude)

#variance in the state group
anova(model_south,model_north,model_northeast,model_east,model_west,model_centre)

#Q5
install.packages("datastructures")
library(datastructures)

N = 100

new1 <- finaldata[-sample(1:nrow(finaldata), nrow(finaldata)*0.05), ]
new2 <- new1[-sample(1:nrow(new1), nrow(new1)*0.05), ]
new3 <- new2[-sample(1:nrow(new2), nrow(new2)*0.05), ]
new <- new3[-sample(1:nrow(new3), nrow(new3)*0.05), ]


model3 <- lm(formula = Reservoir.Water.Level ~ SDP, data = finaldata,na.action = na.exclude)
summary(model3)
a_0 = coef(model3)[1]
a_1 = coef(model3)[2]

SDP <- new$SDP
v43_new <- new$Reservoir.Water.Level
a_0array = numeric(N)
a_1array = numeric(N)

for(j in 1: N){
  my_df <- data.frame(v43_new,SDP)
  model5 <- lm(v43_new ~ SDP, my_df)
  v43_new = c()
  a_0 = coef(model5)[1]
  a_1 = coef(model5)[2]
  for(i in 1: nrow(new)){
    if(!is.na(new[i,]$Reservoir.Water.Level))
      v43_new = append(v43_new, a_0 + a_1*SDP[i])
    else 
      v43_new = append(v43_new, NA)
  }
  a_0array[[j]] = as.vector(model5$coefficients[1])
  a_1array[[j]] = as.vector(model5$coefficients[2])
}

mean(a_0array,na.rm = TRUE)
mean(a_1array,na.rm = TRUE)

sd(a_0array,na.rm = TRUE)
sd(a_1array,na.rm = TRUE)

sd(a_0array,na.rm = TRUE)/sqrt(N)
sd(a_1array,na.rm = TRUE)/sqrt(N)

plot(density(a_1array))
plot(density(a_0array))

