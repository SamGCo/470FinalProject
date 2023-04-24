library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)
library(knitr)
brfss2021<-read_xpt("C:/Users/squam/OneDrive/Documents/470FinalProject/LLCP2021.XPT")
#brfss2021<-read.table("C:/Users/squam/OneDrive/Documents/470FinalProject/LLCP2021.asc")
brfss2017<-read_xpt("C:/Users/squam/OneDrive/Documents/470FinalProject/LLCP2017.XPT")
brfss2018<-read_xpt("C:/Users/squam/OneDrive/Documents/470FinalProject/LLCP2018.XPT")
brfss2019<-read_xpt("C:/Users/squam/OneDrive/Documents/470FinalProject/LLCP2019.XPT")
expanded<-read.csv("C:/Users/squam/OneDrive/Documents/470FinalProject/raw_data.csv")
expanded<-expanded[!grepl("1,",expanded$Footnotes),]
expanded<-subset(expanded, Location !="Pennsylvania" )
expanded<-subset(expanded, Location !="Alaska" )
freq_table <- table(expanded$Status.of.Medicaid.Expansion.Decision)
freq_df <- as.data.frame(freq_table)
colnames(freq_df) <- c("MedicaidExpansionStatus", "Frequency")
plot1<-ggplot(freq_df, aes(x = MedicaidExpansionStatus, y = Frequency)) +
  geom_bar(stat = "identity")
crosswalk<-crosswalkr::stcrosswalk
crosswalk<-crosswalk[,c(1,3)]
names(crosswalk)[2]<-"State"
names(crosswalk)[1]<-"_STATE"
names(expanded)[1]<-"State"
expanded<-expanded[,1:2]
finalData<-brfss2017%>%
  select(`_STATE`,IYEAR,HLTHPLN1,SMOKE100,DRNK3GE5,EXERANY2,FLUSHOT6,HIVRISK5)
finalData$IYEAR<-2017
cleaned2018<-brfss2018%>%
  select(`_STATE`,IYEAR,HLTHPLN1,SMOKE100,DRNK3GE5,EXERANY2,FLUSHOT6,HIVRISK5)
cleaned2018$IYEAR<-2018
cleaned2019<-brfss2019%>%
  select(`_STATE`,IYEAR,HLTHPLN1,SMOKE100,DRNK3GE5,EXERANY2,"FLUSHOT6"=FLUSHOT7,HIVRISK5)
cleaned2019$IYEAR<-2019
finalData<-rbind(finalData,cleaned2018)
finalData<-rbind(finalData,cleaned2019)

finalData<-left_join(finalData,crosswalk)
finalData<-left_join(finalData,expanded)
colnames(finalData)<-c("StateNum","Year","Health Coverage","Smoker","BingeDrink","Exercise","FluVax","HIVRisk","State","Expansion")
hlthPln<-subset(finalData,(HLTHPLN1 ==1 |HLTHPLN1 ==2)&(FLUSHOT6 ==1 |FLUSHOT6 ==2))
Flu<-subset(finalData,FLUSHOT6 ==1 |FLUSHOT6 ==2)
tableVAX<-table(hlthPln$HLTHPLN1,hlthPln$FLUSHOT6)
kableVax<-kable(tableVAX)

finalData<-subset(finalData,(`Health Coverage` ==1 |`Health Coverage` ==2)&(HLTHPLN1 ==1 |HLTHPLN1 ==2)&(FLUSHOT6 ==1 |FLUSHOT6 ==2)&(HIVRISK5 ==1 |HIVRISK5 ==2)&(EXERANY2 ==1 |EXERANY2 ==2)&(SMOKE100 ==1 |SMOKE100 ==2))

summarizedData<-finalData%>%
  group_by(State, Year,Expansion) %>%
  mutate(Uninsured=case_when((`Health Coverage`==2)~TRUE,TRUE~FALSE))%>%
  summarize_at(vars(c("Smoker","Exercise","FluVax","HIVRisk","Uninsured")), ~ mean(. == 1, na.rm = TRUE) * 100)

means_table <- aggregate(cbind(Smoker, Exercise,`Flu Vax`,`HIV Risk`) ~ Expansion, data = summarizedData, mean)
means_table

ivData<-summarizedData%>%
  filter(!is.na(Expansion))
ivSmoke<-feols(Smoker ~ 1 | Uninsured ~ (Expansion), data = ivData)
ivExer<-feols(Exercise ~ 1 | Uninsured ~ (Expansion), data = ivData)
ivFlu<-feols(FluVax ~ 1 | Uninsured ~ (Expansion), data = ivData)
ivHIV<-feols(HIVRisk ~ 1 | Uninsured ~ (Expansion), data = ivData)
regList<-c(ivSmoke,ivExer,ivFlu,ivHIV)
summary(ivSmoke)
modelsummary::modelsummary(list(ivSmoke,ivExer,ivFlu,ivHIV))
save.image("workspace.RData")



