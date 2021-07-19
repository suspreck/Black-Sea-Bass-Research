CRP.Vent.Descend.Data.Updated.Mar.31.2021_HS <- read.csv("C:/Users/bookw/Desktop/CMAST_Research/Black Sea Bass/CRP Vent Descend Data Updated Mar 31 2021_HS.csv")

data = CRP.Vent.Descend.Data.Updated.Mar.31.2021_HS
data$RecapDate = ifelse(data$RecapDate == "12/31/2020", "7/31/2021", data$RecapDate)
data$RecapDate = ifelse(data$RecapDate == "8/31/2021", "7/31/2021", data$RecapDate)

write.csv(data, "C:/Users/bookw/Desktop/CMAST_Research/Black Sea bass/CRP Vent Descend Data Updated Jun 2 2021_HS.csv")