#installing packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotrix")
install.packages("DescTools")
install.packages("lubridate")
install.packages("crayon")
install.packages("psych")
install.packages("plotly")
install.packages("reshape2")
library(dplyr)
library(ggplot2)
library(plotrix)
library(DescTools)
library(lubridate)
library(crayon)
library(psych)
library(plotly)
library(reshape2)

#-------------------------------------------------------------------------------
#function

no_format <- function(x) {
  format(x, scientific = FALSE)
}
#-------------------------------------------------------------------------------

#violin graph
#bubble chart
#density graph
#correlation plot

#-------------------------------------------------------------------------------
#IMPORT DATA
#-------------------------------------------------------------------------------
house_data_v0 = read.csv("C:\\Users\\TUF\\OneDrive - Asia Pacific University\\YEAR 2\\SEM 1\\PFDA\\assignment\\House_Rent_Dataset.csv"
                         ,header = TRUE)

dim(house_data_v0)
colnames(house_data_v0)
View(describe(house_data_v0))

#-------------------------------------------------------------------------------
#DATA CLEANING
#-------------------------------------------------------------------------------
#remove outliers
#Rent outlier


#before clean
#RAW
#histogram
ggplot(house_data_v0, mapping = aes(x=Rent))+
  geom_histogram(color="black",fill="red", alpha=0.4, binwidth = 5000)+
  ggtitle("Rent data and their frequency in a 5000 binwidth")+
  scale_x_continuous(labels = no_format)+
  theme_minimal()

#boxplot
ggplot(house_data_v0, mapping = aes(y=Rent))+
  geom_boxplot(colour="black",fill="red",alpha=0.4)+
  ggtitle("Rent data and their data distribution")+
  scale_y_continuous(labels = no_format)+
  theme_minimal()

#brief cut
house_data_v0_for_show = filter(house_data_v0,house_data_v0$Rent<500000)

#histogram
ggplot(house_data_v0_for_show, mapping = aes(x=Rent))+
  geom_histogram(color="black",fill="red",alpha=0.4, binwidth = 4500)+
  ggtitle("Rent data and their frequency in a 5000 binwidth")+
  scale_x_continuous(labels = no_format)+
  theme_minimal()

#boxplot
ggplot(house_data_v0_for_show, mapping = aes(y=Rent))+
  geom_boxplot(colour="black",fill="red",alpha=0.4)+
  scale_y_continuous(labels = no_format)+
  ggtitle("Rent data and their data distribution")+
  theme_minimal()



#cleaning (replace)
house_data_v1 = house_data_v0
quartiles = quantile(house_data_v1$Rent,probs = c(0.25,0.5,0.75), na.rm = TRUE)
quartiles

n_outlier_Rent = nrow(filter(house_data_v0, Rent<=0 | Rent > 100000))
n_outlier_Rent

house_data_v1[house_data_v1$Rent>100000,]$Rent = sample(quartiles[1]:quartiles[3],n_outlier_Rent,TRUE)
View(house_data_v1)

message("No. of lines removed : ", nrow(house_data_v0)- nrow(house_data_v1))

#after clean
#histogram
ggplot(data =house_data_v1, mapping = aes(x=Rent))+
  geom_histogram(aes(y=..count..), color="black", fill="green",alpha=0.4, binwidth = 4000)+
  geom_density(aes(y= 3500 *..count..), alpha=0.8,fill="skyblue")+
  ggtitle("Rent freqency distribiton (Rent cleaned)")+
  theme_minimal()

#boxplot
ggplot(house_data_v1, mapping = aes(y=Rent))+
  geom_boxplot(colour="black", fill="green", alpha=0.4)+
  scale_y_continuous(labels = no_format)+
  ggtitle("Rent frequency distribution (Rent cleaned)")+
  theme_minimal()

dim(house_data_v1)

#-------------------------------------------------------------------------------

#Size outlier

#before clean
#histogram
ggplot(house_data_v1, mapping = aes(x=Size))+
  geom_histogram(color="black", fill="red", alpha=0.4, binwidth = 100)+
  ggtitle("Size frequency distribution with 100 binwidth")+
  scale_x_continuous(labels = no_format)+
  theme_minimal()

#boxplot
ggplot(house_data_v1, mapping = aes(y=Size))+
  geom_boxplot(colour="black",fill="red",alpha=0.4)+
  scale_y_continuous(labels = no_format)+
  ggtitle("Size frequency distribution")+
  theme_minimal()


#cleaning (replace)
house_data_v2 = house_data_v1
quartiles = quantile(house_data_v2$Size,probs = c(0.25,0.5,0.75), na.rm = TRUE)
quartiles

n_outlier_Size = nrow(filter(house_data_v1, Size<=0 | Size > 3500))
n_outlier_Size

house_data_v2[house_data_v2$Size>3500,]$Size = sample(quartiles[1]:quartiles[3],n_outlier_Size,TRUE)
View(house_data_v2)

message("No. of lines removed : ", nrow(house_data_v1)- nrow(house_data_v2))

#after clean
#histogram
ggplot(data =house_data_v2, mapping = aes(x=Size))+
  geom_histogram(aes(y=..count..), color="black",fill="green", alpha=0.4, binwidth = 100)+
  geom_density(aes(y= 80 *..count..), alpha=0.8,fill="skyblue")+
  ggtitle("Size frequency distribution (Size cleaned)")+
  theme_minimal()

#boxplot
ggplot(house_data_v2, mapping = aes(y=Size))+
  geom_boxplot(colour="black",fill="green",alpha=0.4)+
  scale_y_continuous(labels = no_format)+
  ggtitle("Size frequency distribution (Size cleaned)")+
  theme_minimal()


dim(house_data_v2)
#-------------------------------------------------------------------------------
#attribute uniqueness for qualitative data
#-------------------------------------------------------------------------------

#BHK
n_uniques_BHK = n_distinct(house_data_v2$BHK)
n_uniques_BHK

df_count_by_BHK = house_data_v2 %>% group_by(BHK) %>% summarize(count = n())
df_count_by_BHK
#No problem

#pie chart
a = as.numeric(t(df_count_by_BHK[2]))
b = paste0(as.character(t(df_count_by_BHK[1])),"\n",round(100 * a/sum(a), 2),"%\n  ",a)
pie(a, labels = b, main = "Bathroom")
#bar chart
ggplot(data = df_count_by_BHK, mapping = aes(x = as.character(t(df_count_by_BHK[1])), y = a, fill=a))+
  geom_bar(stat = "identity",colour="black")+
  labs(title = "BHK frequency distribution", x = "BHK", y = "Count") +
  theme_minimal()

#-------------------------------------------------------------------------------
#Floor
n_uniques_Floor = n_distinct(house_data_v2$Floor)
n_uniques_Floor

df_count_by_Floor = house_data_v2 %>% group_by(Floor) %>% summarize(count = n())
df_count_by_Floor
#to be preprocess

#View(print(df_count_by_Floor, n= nrow(df_count_by_Floor)))
#-------------------------------------------------------------------------------
#Area Type
n_uniques_AreaType = n_distinct(house_data_v2$Area.Type)
n_uniques_AreaType

df_count_by_AreaType = house_data_v2 %>% group_by(Area.Type) %>% summarize(count = n())
df_count_by_AreaType
#Outlier "Built Area" Frequency

#before clean
#pie chart
a = as.numeric(t(df_count_by_AreaType[2]))
b = paste0(as.character(t(df_count_by_AreaType[1])),"\n",round(100 * a/sum(a), 2),"%\n",a)
pie(a, labels = b, main="Area Type", radius = 1)

#cleaning (replace)

house_data_v3 = house_data_v2
house_data_v3[house_data_v3$Area.Type=="Built Area",6] = "Carpet Area" #documentation explain

#after clean
df_count_by_AreaType_cleaned = house_data_v3 %>% group_by(Area.Type) %>% summarize(count = n())
df_count_by_AreaType_cleaned
#pie chart
a = as.numeric(t(df_count_by_AreaType_cleaned[2]))
b = paste0(as.character(t(df_count_by_AreaType_cleaned[1])),"\n",round(100 * a/sum(a), 2),"%\n  ",a)
pie(a, labels = b, main="Area Type (Cleaned)", radius = 1)

#-------------------------------------------------------------------------------
#Area locality
n_data_type_uniquesness_Area_locality = class(house_data_v3$Area.Locality)
n_data_type_uniquesness_Area_locality

#cleaning numeric value in this "character" attribute
#before clean
house_data_v3[filter(house_data_v3, is.na(as.numeric(house_data_v3$Area.Locality))==FALSE)]
house_data_v3[is.na(as.numeric(house_data_v2$Area.Locality))==FALSE, ]

#cleaning (replace)
house_data_v4 = house_data_v3
house_data_v4[is.na(as.numeric(house_data_v4$Area.Locality))==FALSE, 7] = "NA"

#after clean
if(nrow(house_data_v4[is.na(as.numeric(house_data_v4$Area.Locality))==FALSE, ])==0){
  message(bgYellow$blue$bold$underline(" Empty "))
}


#-------------------------------------------------------------------------------
#City
n_uniques_City = n_distinct(house_data_v4$City)
n_uniques_City

df_count_by_City = house_data_v4 %>% group_by(City) %>% summarize(count = n())
df_count_by_City
#No problem

#pie chart
a= as.vector(t(df_count_by_City[,2]))
b= paste0(as.character(t(df_count_by_City[,1])),"\n",round(100 * a/sum(a), 2),"%\n  ",a)
pie(a,labels = b, main="City",radius=2)
pie3D(a,labels=b, explode=.5, main="City")

#-------------------------------------------------------------------------------
#Furnishing Status
n_uniques_Furnished = n_distinct(house_data_v4$Furnishing.Status)
n_uniques_Furnished

df_count_by_Furnished = house_data_v4 %>% group_by(Furnishing.Status) %>% summarize(count = n())
df_count_by_Furnished
#No problem

#pie chart
a= as.vector(t(df_count_by_Furnished[,2]))
b= paste0(as.character(t(df_count_by_Furnished[,1])),"\n",round(100 * a/sum(a), 2),"%\n  ",a)
pie(a,labels = b, main="Furnishing Status")

#-------------------------------------------------------------------------------
#Tenants Preferred
n_uniques_Tenants = n_distinct(house_data_v4$Tenant.Preferred)
n_uniques_Tenants

df_count_by_Tenants = house_data_v4 %>% group_by(Tenant.Preferred) %>% summarize(count = n())
df_count_by_Tenants
#No problem

#pie chart
a= as.vector(t(df_count_by_Tenants[,2]))
b= paste0(as.character(t(df_count_by_Tenants[,1])),"\n",round(100 * a/sum(a), 2),"%\n  ",a)
pie(a,labels = b, main="Tenants Preferred")

#-------------------------------------------------------------------------------
#Bathroom
n_uniques_Bathroom = n_distinct(house_data_v4$Bathroom)
n_uniques_Bathroom

df_count_by_Bathroom = house_data_v4 %>% group_by(Bathroom) %>% summarize(count = n())
df_count_by_Bathroom
#'10' bathroom need clean

#before cleaning
#pie chart
a= as.vector(t(df_count_by_Bathroom[,2]))
b= paste0(as.character(t(df_count_by_Bathroom[,1])),"\n",round(100 * a/sum(a), 2),"%\n  ",a)
pie(a,labels = b, main = "Bathroom")

#bar chart
ggplot(data= df_count_by_Bathroom, mapping=aes(x=b,y=a,fill=a)) +
  geom_bar(stat = "identity", color="yellow")+
  geom_text(aes(label=a)) + 
  labs(title= "Bathroom", x="Number of Bathrooms", y="Bathroom Percentage")

#cleaning (remove)
house_data_v5 = filter(house_data_v4, house_data_v4$Bathroom != 10)
house_data_v5

message("No. of lines removed : ", nrow(house_data_v4)- nrow(house_data_v5))

#after clean
df_count_by_Bathroom_cleaned = house_data_v5 %>% group_by(Bathroom) %>% summarize(count = n())
df_count_by_Bathroom_cleaned
#pie chart
a= as.vector(t(df_count_by_Bathroom_cleaned[,2]))
b= paste0(as.character(t(df_count_by_Bathroom_cleaned[,1])),"\n",round(100 * a/sum(a), 2),"%\n  ",a)
pie(a,labels = b, main = "Bathroom")

#bar chart
ggplot(data= df_count_by_Bathroom_cleaned, mapping=aes(x=b,y=a,fill=a)) +
  geom_bar(stat = "identity", color="yellow")+
  geom_text(aes(label=a)) + 
  labs(title= "Bathroom", x="Number of Bathrooms", y="Bathroom Percentage")

dim(house_data_v5)

#-------------------------------------------------------------------------------
#Point of contact
n_uniques_Contact = n_distinct(house_data_v5$Point.of.Contact)
n_uniques_Contact

df_count_by_Contact = house_data_v5 %>% group_by(Point.of.Contact) %>% summarize(count = n())
df_count_by_Contact
#Outlier "Contact Builder" frequency

#Before cleaning
#Pie chart
a= as.vector(t(df_count_by_Contact[,2]))
b= paste0(as.character(t(df_count_by_Contact[,1])),"\n",round(100 * a/sum(a), 2),"%\n  ",a)
pie(a,labels = b, main="Point of Contact")

#cleaning 
Point_of_Contact_mode = Mode(house_data_v5$Point.of.Contact)
Point_of_Contact_mode

house_data_v6 = house_data_v5
house_data_v6[house_data_v6$Point.of.Contact=="Contact Builder", 12] = Point_of_Contact_mode

#after clean
df_count_by_Contact_cleaned = house_data_v6 %>% group_by(Point.of.Contact) %>% summarize(count = n())
df_count_by_Contact_cleaned
#pie chart
a= as.vector(t(df_count_by_Contact_cleaned[,2]))
b= paste0(as.character(t(df_count_by_Contact_cleaned[,1])),"\n",round(100 * a/sum(a), 2),"%\n  ",a)
pie(a,labels = b, main="Point of Contact")

#-------------------------------------------------------------------------------

#remove na
missing_counts = colSums(is.na(house_data_v6))
missing_counts

house_data_v6_noChange = na.omit(house_data_v6)
message(nrow(house_data_v6) - nrow(house_data_v6_noChange))
#no na record


#-------------------------------------------------------------------------------
#remove duplicate
house_data_v6_noChange = unique(house_data_v6)
message(nrow(house_data_v6)-nrow(house_data_v6_noChange))
#no duplicate record

#-------------------------------------------------------------------------------

#change format
#-------------------------------------------------------------------------------
# date format
class(house_data_v6$Posted.On) #should be date

# Loop through each element and parse the date with different formats
parsed_dates = sapply(house_data_v6$Posted.On, function(date_str) {
  # Try parsing with different formats
  parsed_date = parse_date_time(date_str, orders = c("mdY", "mdy", "Ymd"))
  
  # Format the parsed date as YYYY-MM-DD
  formatted_date = format(parsed_date, "%Y-%m-%d")
  return(formatted_date)
})

house_data_v7 = house_data_v6
house_data_v7[,1] = as.Date(parsed_dates)
View(house_data_v7)
#-------------------------------------------------------------------------------

class(house_data_v7$Posted.On)
class(house_data_v7$BHK)
class(house_data_v7$Rent)
class(house_data_v7$Size)
class(house_data_v7$Floor)
class(house_data_v7$Area.Type)
class(house_data_v7$Area.Locality)
class(house_data_v7$City)
class(house_data_v7$Furnishing.Status)
class(house_data_v7$Tenant.Preferred)
class(house_data_v7$Bathroom)
class(house_data_v7$Point.of.Contact)
#ALL NO PROBLEM

#-------------------------------------------------------------------------------

#change name
house_data_v8 = data.frame(Date_Posted        = house_data_v6$Posted.On, 
                           BHK                = house_data_v6$BHK, 
                           Rent_Price         = house_data_v6$Rent,
                           House_Size         = house_data_v6$Size,
                           Floor_level        = house_data_v6$Floor,
                           Area_Type          = house_data_v6$Area.Type,
                           Area_Locality      = house_data_v6$Area.Locality,
                           City               = house_data_v6$City,
                           Furnishing_Status  = house_data_v6$Furnishing.Status,
                           Tenant_Preferences = house_data_v6$Tenant.Preferred,
                           No_of_Bathroom     = house_data_v6$Bathroom,
                           Point_of_Contact   = house_data_v6$Point.of.Contact)
View(house_data_v8)

#-------------------------------------------------------------------------------

#data pre-processing

#-------------------------------------------------------------------------------
#categorizing House-size

summary(house_data_v8$House_Size)
size_category = vector("character", nrow(house_data_v8))
for (i in 1:nrow(house_data_v8)){
  if (house_data_v8$House_Size[i] < 700){
    size_category[i]="Small"
  }
  
  else if (house_data_v8$House_Size[i] >= 700 && house_data_v8$House_Size[i] < 1200){
    size_category[i]="Medium"
  }
  else if (house_data_v8$House_Size[i] >= 1200){
    size_category[i]="Large"
  }
  else {
    message("Error occur")
  }
}

house_data_v9 = cbind(house_data_v8, size_category)
View(house_data_v9)

#Pie chart
small = nrow(house_data_v9[house_data_v9$size_category=="Small",])
medium = nrow(house_data_v9[house_data_v9$size_category=="Medium",])
large = nrow(house_data_v9[house_data_v9$size_category=="Large",])

a = c(small,medium,large)
b = paste0(c("small","medium","large"),"\n",round(100 * a/sum(a), 2),"%\n", a)
pie(a,labels = b, radius=1, main = "Size Category")
#-------------------------------------------------------------------------------
#modifying floor level

floor_category = vector("character",nrow(house_data_v9))

for (i in 1:nrow(house_data_v9)){
  x = strsplit(house_data_v9$Floor_level[i]," ")
  floor_category[i]=x[[1]][1]
}

house_data_v10 = cbind(house_data_v9,floor_category)
house_data_v10[grepl("ground",house_data_v9$Floor_level,ignore.case = TRUE),14] = "Ground"
house_data_v10[startsWith(house_data_v9$Floor_level,"Lower Basement"),14] = "Lower Basement"
house_data_v10[startsWith(house_data_v9$Floor_level,"Upper Basement"),14] = "Upper Basement"
View(house_data_v10[c("Floor_level","floor_category")])

n_distinct(house_data_v10$floor_category)
df_count_by_Floor_cleaned = house_data_v10 %>% group_by(floor_category) %>% summarize(count = n())
View(df_count_by_Floor_cleaned)

arrange(df_count_by_Floor_cleaned, desc(count))

#-------------------------------------------------------------------------------
#Removing unused column for data mining analysis

house_data_v11 = select(house_data_v10, -c("Date_Posted","Floor_level","Area_Locality"))
Group = sort(unique(house_data_v11$size_category), decreasing = TRUE)

house_data_v11[,10] = factor(house_data_v11[,10], levels = Group, ordered = TRUE)
View(house_data_v11)
dim(house_data_v11)

#-------------------------------------------------------------------------------
#Objective Analysis

#-------------------------------------------------------------------------------
# --Smaller size house make more profit compare to bigger size--

#-------------------------------------------------------------------------------
#City record distribution
a= as.vector(t(df_count_by_City[,2]))
b= paste0(as.character(t(df_count_by_City[,1])),"\n",round(100 * a/sum(a), 2),"%\n  ",a)
pie(a,labels = b, main="City")
pie3D(a,labels=b, explode=.5, main="City")


ggplot(data = df_count_by_City, mapping = aes(x = City,y=count,fill=count))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=count),vjust=-0.5)+
  ggtitle("City record frquency distribution")+
  scale_fill_gradient(low="lightgreen",high="blue")+
  theme_minimal()

#-------------------------------------------------------------------------------

#City vs Size
category =unique(arrange(house_data_v11,City)$City)
Group = sort(unique(house_data_v11$size_category), decreasing = TRUE)
Group = factor(Group,levels = Group, ordered = TRUE)
vec_count = vector()
#city vs size category

for (i in 1:length(category)){
  for (k in 1:length(Group)){
    condition = filter(house_data_v11,house_data_v11$City == category[i] & house_data_v11$size_category == Group[k])
    current_count = nrow(condition)
    vec_count = append(vec_count, current_count)
  }
}

  #dataframe to store
category_vecCount_group = data.frame(category = rep(category, each = 3),
                                     Group = rep(Group, times = 3),
                                     vec_count)
category_vecCount_group

  #complex bar chart - City vs Size vs Count_of_record
ggplot(data = category_vecCount_group, mapping = aes(x = category, y = vec_count, fill = Group))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "City vs Record Distribution in each Size category", x = "City", y = "Count") +
  scale_fill_discrete(name = "Group") +
  geom_text(aes(label=vec_count), position = position_dodge(width=0.9),vjust=-0.5) +
  theme_minimal()


#-------------------------------------------------------------------------------

#size vs rent 

#House Size vs rent
ggplot(data = house_data_v11, mapping = aes(x=House_Size,y=Rent_Price))+
  geom_point(aes(color=House_Size))+
  geom_smooth(method = "gam")+
  ggtitle("House Size vs Rent Price")+
  theme_minimal()

  #House Size vs Rent Price for each city
ggplotly(
  ggplot(data = house_data_v11, mapping = aes(x=House_Size,y=Rent_Price))+
    geom_point(aes(colour=City))+
    geom_smooth(method = "lm")+
    ggtitle("House Size vs Rent Price for each city")+
    facet_wrap(~City)
)

  #House Size vs Rent Price for each size_category
ggplot(data = house_data_v11, mapping = aes(x=House_Size,y=Rent_Price))+
  geom_point(aes(colour=size_category,shape=size_category))+
  geom_smooth(method = "glm")+
  ggtitle("House Size vs Rent Price for each size_category")+
  facet_grid(~size_category)

  #size_category vs Rent Price
ggplotly(
  ggplot(data = house_data_v11, mapping = aes(x=size_category,y=Rent_Price))+
    geom_boxplot(aes(fill=size_category))+
    ggtitle("size_category vs Rent Price")+
    scale_fill_manual(values=c("purple","seagreen4","yellow"))
)

#median & mean Rent_Price for each City
median_city_rent = vector()
mean_city_rent = vector()
for (i in 1:6){
  median_city_rent = append(median_city_rent, median(house_data_v11[house_data_v11$City==category[i],]$Rent_Price))
  mean_city_rent = append(mean_city_rent, mean(house_data_v11[house_data_v11$City==category[i],]$Rent_Price))
}

  #put in documentation
dff_meanrent_city = data.frame(City = category,
                               Median_Rent = median_city_rent,
                               Mean_Rent = mean_city_rent)

  #City vs mean rent
ggplot(data = dff_meanrent_city, mapping = aes(x= City, y=Mean_Rent))+
  geom_bar(stat="identity",width=0.5)+
  geom_text(aes(label=round(Mean_Rent,0)),vjust = -0.5)+
  ggtitle("City vs mean rent")+
  theme_bw()

  #Mumbai highest mean rent_price
  #City vs Rent Price
ggplotly(
  ggplot(data = house_data_v11, mapping = aes(x=City,y=Rent_Price,fill=City))+
    geom_boxplot()+
    ggtitle("City vs Rent_Price")
    )



#gradient for each size_category of each city
gradient = vector()
for(i in 1:length(category)){
  for(k in 1:length(Group)){
    gradient = append(gradient,unname(coef(lm(house_data_v11[house_data_v11$size_category == Group[k] & house_data_v11$City == category[i],]$Rent_Price
                       ~house_data_v11[house_data_v11$size_category == Group[k] & house_data_v11$City == category[i],]$House_Size)))[2])
  }
}
category_vecCount_group_gradient = cbind(category_vecCount_group,gradient = gradient)
category_vecCount_group_gradient

#House Size vs Rent Price for each city and each size
ggplotly(
  ggplot(data = house_data_v11, mapping = aes(x=House_Size,y=Rent_Price))+
    geom_point(aes(fill = size_category))+
    geom_smooth(method = "lm")+
    ggtitle("House Size vs Rent Price for each city and each size")+
    facet_grid(size_category~City)+
    scale_fill_manual(values = c("purple","seagreen4","yellow"))
  )

#House Size vs Rent Price for each size_category & each City
ggplotly(
  ggplot(data = filter(house_data_v11,Furnishing_Status=="Furnished"), mapping = aes(x=size_category,y=Rent_Price))+
    geom_boxplot(aes(fill=size_category))+
    ggtitle("House Size vs Rent Price for each size_category & each City")+
    facet_wrap(~City)+
    scale_fill_manual(values = c("purple","seagreen4","yellow"))
)
#-------------------------------------------------------------------------------

#House Size vs BHK
median_BHK_Size = vector()
for (i in 1:6){
  median_BHK_Size = append(median_BHK_Size,median(house_data_v11[house_data_v11$BHK == i,]$House_Size))
}
df_BHK_Median = data.frame(BHK = sort(unique(house_data_v11$BHK)),
                           Median = median_BHK_Size)


ggplotly(
ggplot(data = house_data_v11, mapping = aes(x=as.character(BHK),y=House_Size))+
  geom_boxplot(aes(fill=as.character(BHK)))+
  geom_line(data = df_BHK_Median, mapping = aes(x=BHK,y=Median,group=1))+
  labs(title = "BHK vs House Size",x="BHK")+
  scale_fill_discrete(name = "BHK categoies")
  )

  #BHK vs House Size for each City
ggplot(data = house_data_v11, mapping = aes(x=as.character(BHK),y=House_Size))+
  geom_boxplot(aes(fill=as.character(BHK)))+
  labs(title = "BHK vs House Size for each City",x="BHK")+
  scale_fill_discrete(name = "BHK categoies")+
  facet_wrap(~City)
  
#size category vs BHK 
BHK_size_mean = vector()

for (i in 1:3){
  condition = mean(house_data_v11[house_data_v11$size_category == Group[i],]$BHK)
  BHK_size_mean = append(BHK_size_mean,condition)
}
df_BHK_size_mean = data.frame(size = Group,
                    BHK_size_mean)

#Size category vs BHK (+mean(avrg)) line
ggplot(data = house_data_v11, mapping = aes(x=size_category,y=BHK))+
  geom_violin(aes(fill=size_category),linewidth=1.5)+
  geom_line(data = df_BHK_size_mean, mapping = aes(x=size,y=BHK_size_mean, group = 1), linewidth=1.2, colour="lightskyblue")+
  geom_point(data = df_BHK_size_mean, mapping = aes(x=size,y=BHK_size_mean), size=3, stroke=1.5, colour="darkblue", shape=13)+
  labs(title = "Size category vs BHK (+mean(avrg)) line")+
  scale_fill_manual(values = c("purple","seagreen4","yellow"))+
  theme_minimal()


#Size category vs BHK (+mean(avrg)) line for each city
  #preparation
city_category = sort(unique(house_data_v11$City))
BHK_city_size_mean = vector()

for (j in 1:6){
  for (i in 1:3){
    condition = mean(house_data_v11[house_data_v11$size_category == Group[i] & house_data_v11$City == city_category[j],]$BHK)
    BHK_city_size_mean = append(BHK_city_size_mean,condition)
  }
}

df_BHK_city_size_mean = data.frame(City = rep(city_category,each=3),
                                   size = rep(Group, times=3),
                                   BHK_city_size_mean)

  #Size category vs BHK (+mean(avrg)) line for each city  
ggplot(data = house_data_v11, mapping = aes(x=size_category,y=BHK))+
  geom_violin(aes(fill=size_category),linewidth=1.1)+
  geom_line(data = df_BHK_city_size_mean, mapping = aes(x=size,y=BHK_city_size_mean, group = 1), linewidth=1.2, colour="lightskyblue")+
  geom_point(data = df_BHK_city_size_mean, mapping = aes(x=size,y=BHK_city_size_mean), size=2.5, stroke=1.4, colour="darkblue", shape=13)+
  labs(title = "Size category vs BHK (+mean(avrg)) line for each City")+
  scale_fill_manual(values = c("purple","seagreen4","yellow"))+
  theme_minimal()+
  facet_wrap(~City)

#-------------------------------------------------------------------------------

#House Size vs Area Type
category_Area = unique(house_data_v11$Area_Type)

median_Area_Size = vector()
for (i in 1:length(category_Area)){
  median_Area_Size = append(median_Area_Size,median(house_data_v11[house_data_v11$Area_Type == category_Area[i],]$House_Size))
}
df_Area_Median = data.frame(Area_Type = category_Area,
                           Median = median_Area_Size)
df_Area_Median


ggplot(data = house_data_v11, mapping = aes(x=Area_Type,y=House_Size))+
  geom_violin(aes(fill=Area_Type, alpha = 0.3))+
  geom_boxplot(aes(fill=Area_Type), linewidth = 1)+
  geom_line(data = df_Area_Median, mapping = aes(x=Area_Type,y=Median,group=1), linewidth = 1.3, color = "red3")+
  labs(title = "Area_Type vs House Size",x="Area_Type")+
  scale_fill_discrete(name = "Area_Type categoies")

#Area_Type vs House Size for each City
ggplot(data = house_data_v11, mapping = aes(x=Area_Type,y=House_Size))+
  geom_violin(aes(fill=Area_Type, alpha = 0.8))+
  geom_boxplot(aes(fill=Area_Type), alpha = 0.9,linewidth = 0.7)+
  labs(title = "Area_Type vs House Size for each City",x="Area_Type")+
  scale_fill_discrete(name = "Area_Type categoies")+
  facet_wrap(~City)

#size category vs Area Type pie distribution
  #small
small_AreaType = c(nrow(house_data_v11[house_data_v11$Area_Type=="Super Area" & house_data_v11$size_category=="Small",]),nrow(house_data_v11[house_data_v11$Area_Type=="Carpet Area" & house_data_v11$size_category=="Small",]))
  #medium
medium_AreaType = c(nrow(house_data_v11[house_data_v11$Area_Type=="Super Area" & house_data_v11$size_category=="Medium",]),nrow(house_data_v11[house_data_v11$Area_Type=="Carpet Area" & house_data_v11$size_category=="Medium",]))
  #large
large_AreaType = c(nrow(house_data_v11[house_data_v11$Area_Type=="Super Area" & house_data_v11$size_category=="Large",]),nrow(house_data_v11[house_data_v11$Area_Type=="Carpet Area" & house_data_v11$size_category=="Large",]))

small_AreaType_label = paste0(c("Super Area","Carpet Area"),"\n",round(100 * small_AreaType/sum(small_AreaType), 2),"%\n",small_AreaType)
medium_AreaType_label = paste0(c("Super Area","Carpet Area"),"\n",round(100 * small_AreaType/sum(medium_AreaType), 2),"%\n",medium_AreaType)
large_AreaType_label = paste0(c("Super Area","Carpet Area"),"\n",round(100 * small_AreaType/sum(large_AreaType), 2),"%\n",large_AreaType)

pie(small_AreaType,labels=small_AreaType_label, main="Small")
pie(medium_AreaType,labels=medium_AreaType_label, main="Medium")
pie(large_AreaType,labels=large_AreaType_label, main="Large")
#-------------------------------------------------------------------------------

#House_Size vs Furnishing Status
ggplot(data = house_data_v11, mapping = aes(x=Furnishing_Status,y=House_Size))+
  geom_violin(aes(fill=Furnishing_Status, alpha = 0.8))+
  geom_boxplot(aes(fill=Furnishing_Status), alpha = 0.9,linewidth = 0.7)+
  labs(title = "Furnishing_Status vs House Size",x="Furnishing_Status")+
  scale_fill_discrete(name = "Furnishing_Status categoies")

#House_Size vs Furnishing Status for each City
ggplot(data = house_data_v11, mapping = aes(x=Furnishing_Status,y=House_Size))+
  geom_violin(aes(fill=Furnishing_Status, alpha = 0.8))+
  geom_boxplot(aes(fill=Furnishing_Status), alpha = 0.9,linewidth = 0.7)+
  labs(title = "Furnishing_Status vs House Size for each City",x="Furnishing_Status")+
  scale_fill_discrete(name = "Furnishing_Status categoies")+
  facet_wrap(~City)


#Furnishing Status vs Size category vs Record Distribution for each City
uni_furnished_status = unique(arrange(house_data_v11,Furnishing_Status)$Furnishing_Status)
uni_size = sort(unique(house_data_v11$size_category), decreasing = FALSE)
uni_city = unique(arrange(house_data_v11,City)$City)
vec_count_furn_size_city = vector()


for (i in 1:length(uni_city)){
  for (k in 1:length(uni_furnished_status)){
    for (j in 1:length(uni_size)){
      condition = filter(house_data_v11,house_data_v11$Furnishing_Status == uni_furnished_status[k] & house_data_v11$size_category == uni_size[j] & house_data_v11$City == uni_city[i])
      current_count = nrow(condition)
      vec_count_furn_size_city = append(vec_count_furn_size_city, current_count)
    }
  }
}

#dataframe to store
category_vecCountFurnSize_size = data.frame(uni_city = rep(uni_city, each = 9),
                                            uni_furnished_status = rep(rep(uni_furnished_status, each = 3), times = 3),
                                            uni_size = rep(uni_size, times = 18),
                                            vec_count_furn_size_city)
category_vecCountFurnSize_size[,3] = factor(category_vecCountFurnSize_size[,3], levels = Group, ordered = TRUE)
View(category_vecCountFurnSize_size)
category_vecCountFurnSize_sizeee = house_data_v11 %>% group_by(City,Furnishing_Status,size_category) %>% summarise(count = n())

ggplot(data = category_vecCountFurnSize_sizeee, mapping = aes(x = Furnishing_Status, y = count, fill = size_category))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Furnishing Status vs Record Distribution vs size category in each City category", x = "City", y = "Count") +
  scale_fill_discrete(name = "Size Category") +
  geom_text(aes(label=count), position = position_dodge(width=0.9)) +
  theme_minimal()+
  facet_wrap(~City)

#complex bar chart - Furnishing status vs Size vs Count_of_record in each City
ggplot(data = category_vecCountFurnSize_size, mapping = aes(x = uni_furnished_status, y = vec_count_furn_size_city, fill = uni_size))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Furnishing Status vs Record Distribution vs size category in each City category", x = "City", y = "Count") +
  scale_fill_discrete(name = "Size Category") +
  geom_text(aes(label=vec_count_furn_size_city), position = position_dodge(width=0.9)) +
  theme_minimal()+
  facet_wrap(~uni_city)
#-------------------------------------------------------------------------------

#House Size vs Tenants Preference
ggplot(data = house_data_v11, mapping = aes(x=Tenant_Preferences,y=House_Size))+
  geom_violin(aes(fill=Tenant_Preferences, alpha = 0.8))+
  geom_boxplot(aes(fill=Tenant_Preferences), alpha = 0.9,linewidth = 0.7)+
  labs(title = "Tenant_Preferences vs House Size",x="Tenant_Preferences")+
  scale_fill_discrete(name = "Tenant_Preferences categoies")

#Tenant_Preferences vs House Size for each City
ggplot(data = house_data_v11, mapping = aes(x=Tenant_Preferences,y=House_Size))+
  geom_violin(aes(fill=Tenant_Preferences, alpha = 0.8))+
  geom_boxplot(aes(fill=Tenant_Preferences), alpha = 0.9,linewidth = 0.7)+
  labs(title = "Tenant_Preferences vs House Size for each City",x="Tenant_Preferences")+
  scale_fill_discrete(name = "Tenant_Preferences categoies")+
  facet_wrap(~City)

#-------------------------------------------------------------------------------

#House Size vs Bathrooms

median_bathroom_Size = vector()
for (i in 1:length(unique(house_data_v11$No_of_Bathroom))){
  median_bathroom_Size = append(median_bathroom_Size,median(house_data_v11[house_data_v11$No_of_Bathroom == i,]$House_Size))
}
df_bathroom_Median = data.frame(No_of_Bathroom = sort(unique(house_data_v11$No_of_Bathroom)),
                                Median = median_bathroom_Size)


ggplotly(
  ggplot(data = house_data_v11, mapping = aes(x=as.character(No_of_Bathroom),y=House_Size))+
    geom_boxplot(aes(fill = as.character(No_of_Bathroom)), linewidth = 1)+
    geom_line(data = df_bathroom_Median, mapping = aes(x=No_of_Bathroom,y=Median,group=1))+
    labs(x="No_of_Bathroom", title = "House size vs Bathroom")+
    scale_fill_discrete(name="No_of_Bathroom")
)
  

#-------------------------------------------------------------------------------

#House Size vs Point of Contact
ggplot(data = house_data_v11, mapping = aes(x=Point_of_Contact , y= House_Size))+
  geom_violin(aes(fill = Point_of_Contact), alpha=0.6)+
  geom_boxplot(aes(fill = Point_of_Contact), alpha=0.8)+
  labs(title="Point of Contact vs House Size")

#Point of Contact vs House Size in each City
ggplot(data = house_data_v11, mapping = aes(x=Point_of_Contact , y= House_Size))+
  geom_violin(aes(fill = Point_of_Contact), alpha=0.6)+
  geom_boxplot(aes(fill = Point_of_Contact), alpha=0.8)+
  labs(title="Point of Contact vs House Size in each City")+
  facet_wrap(~City)

#-------------------------------------------------------------------------------

#House Size vs Floor category

df_size_floor = house_data_v11[c("House_Size","floor_category","City")]
df_size_floor[df_size_floor$floor_category=="Ground",]$floor_category = "0"
df_size_floor[df_size_floor$floor_category=="Upper Basement",]$floor_category = "-1"
df_size_floor[df_size_floor$floor_category=="Lower Basement",]$floor_category = "-2"
df_size_floor$floor_category = as.numeric(df_size_floor$floor_category)

coef_size_floor = coef(lm(df_size_floor$House_Size~df_size_floor$floor_category))

ggplotly(
  ggplot(data = df_size_floor, mapping = aes(x = floor_category, y=House_Size))+
    geom_point(colour = "blue4", fill = "white",shape = 1, stroke = 0.3, size = 0.9)+
    geom_smooth(method = "lm")+
    #geom_abline(intercept = coef_size_floor[1], slope = coef_size_floor[2], colour = "green4")+
    labs(title="Floor Category vs House Size")+
    theme_bw()
)

#-------------------------------------------------------------------------------
#correlation
walao = house_data_v11
walao[walao$floor_category=="Ground",]$floor_category = "0"
walao[walao$floor_category=="Upper Basement",]$floor_category = "-1"
walao[walao$floor_category=="Lower Basement",]$floor_category = "-2"
walao$floor_category = as.numeric(walao$floor_category)

walao[walao$Furnishing_Status=="Unfurnished",]$Furnishing_Status = "0"
walao[walao$Furnishing_Status=="Semi-Furnished",]$Furnishing_Status = "1"
walao[walao$Furnishing_Status=="Furnished",]$Furnishing_Status = "2"
walao$Furnishing_Status = as.numeric(walao$Furnishing_Status)

walao[walao$Point_of_Contact=="Contact Owner",]$Point_of_Contact = "0"
walao[walao$Point_of_Contact=="Contact Agent",]$Point_of_Contact = "1"
walao$Point_of_Contact = as.numeric(walao$Point_of_Contact)

walao[walao$Tenant_Preferences=="Bachelors",]$Tenant_Preferences = "0"
walao[walao$Tenant_Preferences=="Bachelors/Family",]$Tenant_Preferences = "1"
walao[walao$Tenant_Preferences=="Family",]$Tenant_Preferences = "2"
walao$Tenant_Preferences = as.numeric(walao$Tenant_Preferences)

walao[walao$Area_Type=="Super Area",]$Area_Type = "0"
walao[walao$Area_Type=="Carpet Area",]$Area_Type = "1"
walao$Area_Type = as.numeric(walao$Area_Type)

kk = cor(walao[,c(1,2,3,4,6,7,8,9,11)])
kk = round(kk,2)

#Heatmap of attribute correlation
ggplot(data = melt(kk), aes(x = Var1, y = Var2))+
  geom_tile(aes(fill = value))+
  geom_text(aes(label = value))+
  scale_fill_gradient(high = "blue3", low = "white")+
  labs(title="Heatmap of attribute correlation")

#Heatmap of attribute correlation in Mumbai
kk = cor(walao[walao$City == "Mumbai",c(1,2,3,4,6,7,8,9,11)])
kk = round(kk,2)

  #Heatmap of attribute correlation in Mumbai
ggplot(data = melt(kk), aes(x = Var1, y = Var2))+
  geom_tile(aes(fill = value))+
  geom_text(aes(label = value))+
  scale_fill_gradient(high = "blue3", low = "white")+
  labs(title="Heatmap of attribute correlation in Mumbai")

#-------------------------------------------------------------------------------
#Heatmap for the gradient of all attribute
pp = walao[,c(1,2,3,4,6,7,8,9,11)] #used for all city
      #or
pp = walao[walao$City=="Mumbai",c(1,2,3,4,6,7,8,9,11)] #used for all Mumbai
col_name = colnames(pp)
gradients = vector()

for (z in 1:nrow(pp)){
  pp[z,]$BHK = pp[z,]$BHK / max(pp$BHK)
  pp[z,]$Rent_Price = pp[z,]$Rent_Price / max(pp$Rent_Price)
  pp[z,]$House_Size = pp[z,]$House_Size / max(pp$House_Size)
  pp[z,]$Area_Type = pp[z,]$Area_Type / max(pp$Area_Type)
  pp[z,]$Furnishing_Status = pp[z,]$Furnishing_Status / max(pp$Furnishing_Status)
  pp[z,]$Tenant_Preferences = pp[z,]$Tenant_Preferences / max(pp$Tenant_Preferences)
  pp[z,]$No_of_Bathroom = pp[z,]$No_of_Bathroom / max(pp$No_of_Bathroom)
  pp[z,]$Point_of_Contact = pp[z,]$Point_of_Contact / max(pp$Point_of_Contact)
  pp[z,]$floor_category = pp[z,]$floor_category / max(pp$floor_category)
}
View(pp)

for (i in 1:ncol(pp)){
  for (k in 1:ncol(pp)){
    gradients = append(gradients, unname(coef(lm(pp[,col_name[k]]~pp[,col_name[i]])))[2])
  }
}
df_gradient = data.frame(Var1 = rep(col_name,each=9),
                         Var2 = rep(col_name,times=9),
                         gradients = round(gradients,2))

ggplot(data = df_gradient, aes(x = Var1, y = Var2))+
  geom_tile(aes(fill = gradients))+
  geom_text(aes(label = gradients))+
  scale_fill_gradient(high = "blue3", low = "white")+
  labs(title="Heatmap of gradient relationship in Mumbai") #or Heatmap of gradient relationship in Mumbai

#-------------------------------------------------------------------------------
#statistical testing
  #t-test for line 736, (Area Type sv House Size)
t.test(house_data_v11[house_data_v11$Area_Type=="Carpet Area",]$House_Size,house_data_v11[house_data_v11$Area_Type=="Super Area",]$House_Size)
#-------------------------------------------------------------------------------
#question & analysis
  #Which City & which size is the most ideal to get most profit? why?

#-------------------------------------------------------------------------------
#manual calculate (to deprecated)
intercept_city = vector()
gradient_city = vector()
for (z in 1:6){
  condition = unname(coef(lm(house_data_v11[house_data_v11$City == sort(unique(house_data_v11$City))[z],]$Rent_Price~house_data_v11[house_data_v11$City == sort(unique(house_data_v11$City))[z],]$House_Size)))
  intercept_city = append(intercept_city, condition[1])
  gradient_city = append(gradient_city, condition[2])
}

intercept_city
gradient_city
center_size = c(500,850,1967)
suppose_mean_of_size = vector()
for (i in 1:6){
  for (a in 1:3){
    condition = (gradient_city[i] * center_size[a]) + intercept_city[i]
    suppose_mean_of_size = append(suppose_mean_of_size, condition)
  }
}
df_suppose_mean_of_size = data.frame(City = rep(city_category, each=3,),
                                     Size = rep(Group, times=6),
                                     suppose_mean_of_size)

mean_condition = vector()
number_count = vector()
profit = vector()
suppose_mean_of_size_collect = vector()
for (a in 1:6){
  for (b in 1:3){
    for (c in 1:6){
      for (d in 1:2){
        for (e in 1:3){
          for (f in 1:7){
            condition = house_data_v11[house_data_v11$City == sort(unique(house_data_v11$City))[a] &
                                         house_data_v11$size_category == sort(unique(house_data_v11$size_category))[b] &
                                         house_data_v11$BHK == sort(unique(house_data_v11$BHK))[c] &
                                         house_data_v11$Area_Type == sort(unique(house_data_v11$Area_Type))[d] &
                                         house_data_v11$Furnishing_Status == sort(unique(house_data_v11$Furnishing_Status))[e] &
                                         house_data_v11$No_of_Bathroom == sort(unique(house_data_v11$No_of_Bathroom))[f]
                                       ,2]
            number_count =append(number_count,length(condition))
            suppose_mean_of_size_collect = append(suppose_mean_of_size_collect,df_suppose_mean_of_size[df_suppose_mean_of_size$City == sort(unique(house_data_v11$City))[a] &
                                                                                                         df_suppose_mean_of_size$Size == sort(unique(house_data_v11$size_category))[b]
                                                                                                       ,]$suppose_mean_of_size)
            mean_condition = append(mean_condition, mean(condition))
            profit = mean_condition - suppose_mean_of_size_collect
            
          }
        }
      }
    }
  }
}


df_optimum  = data.frame(number_count,
                         City = rep(sort(unique(house_data_v11$City)),each = 756),
                         Size = rep(rep(sort(unique(house_data_v11$size_category)),each = 252),times = 6),
                         BHK = rep(rep(sort(unique(house_data_v11$BHK)),each = 42),times = 18),
                         Area_Type = rep(rep(sort(unique(house_data_v11$Area_Type)),each = 21),times = 108),
                         Furnishing_Status = rep(rep(sort(unique(house_data_v11$Furnishing_Status)),each = 7),times = 216),
                         No_of_Bathroom = rep(sort(unique(house_data_v11$No_of_Bathroom)),times = 648),
                         mean_condition,
                         suppose_mean_of_size_collect,
                         profit
                         )
df_optimum = arrange(filter(df_optimum, number_count > 10), desc(profit))
View(df_optimum)
#-------------------------------------------------------------------------------
#important (alternative to above)
df_profit_data = data.frame()
for (i in 1:6){
  City_dataset = filter(house_data_v11, City==uni_city[i])
  loess_model = loess(Rent_Price~House_Size, data =City_dataset)
  predicted_rent = predict(loess_model, newdata = City_dataset)
  rent_difference = City_dataset$Rent_Price - predicted_rent
  
  profit_data = cbind(City_dataset,predicted_rent,rent_difference)
  
  df_profit_data = rbind(df_profit_data,
                            profit_data %>%
                               select(City,size_category,BHK,Area_Type,Furnishing_Status,No_of_Bathroom,Rent_Price,predicted_rent,rent_difference)%>%
                               group_by(City,size_category,BHK,Area_Type,Furnishing_Status,No_of_Bathroom)%>%
                               summarise(count = n(), Mean_Rent_Price=mean(Rent_Price),predicted_rent = mean(predicted_rent) ,Profit = mean(rent_difference))%>%
                               filter(count>10)
  )
}
View(df_profit_data)

#to say that no influence by BHK
BHK_1_test = filter(house_data_v11,City=="Mumbai"&size_category=="Small"&Area_Type=="Carpet Area"&Furnishing_Status=="Furnished"&Point_of_Contact == "Contact Agent"&No_of_Bathroom=="2",BHK==1)$Rent_Price
BHK_2_test = filter(house_data_v11,City=="Mumbai"&size_category=="Small"&Area_Type=="Carpet Area"&Furnishing_Status=="Furnished"&Point_of_Contact == "Contact Agent"&No_of_Bathroom=="2",BHK==2)$Rent_Price

t.test(BHK_1_test,BHK_2_test)

#Analysis 1 to say that Mumbai has a lot of furnished house at small
df_city_furnishingstat = house_data_v11 %>%
  filter(size_category == "Small") %>%
  select(City, Furnishing_Status) %>%
  group_by(City, Furnishing_Status) %>%
  summarise(count = n()) %>%
  group_by(City) %>%
  mutate(total_count = sum(count), percentage = count/total_count)

ggplot(data = filter(df_city_furnishingstat, Furnishing_Status=="Furnished"), mapping = aes(x=City, y=percentage))+
  geom_bar(stat="identity",position = "dodge", aes(fill=City))+
  labs(title="City vs Percentage count distribution for Furnishing Status = Furnished")+
  geom_text(aes(label=paste0(round(percentage*100,2),"%")),vjust=1.5,size=10)+
  theme_minimal()

#Analysis 2 to say that Mumbai has a lot of Carpet Area at small
df_city_AreaType = house_data_v11 %>%
  filter(size_category == "Small") %>%
  select(City, Area_Type) %>%
  group_by(City, Area_Type) %>%
  summarise(count = n()) %>%
  group_by(City) %>%
  mutate(total_count = sum(count), percentage = count/total_count)

ggplot(data = filter(df_city_AreaType, Area_Type=="Carpet Area"), mapping = aes(x=City, y=percentage))+
  geom_bar(stat="identity",position = "dodge", aes(fill=City))+
  labs(title="City vs Percentage count distribution for Area Type = Carpet Area")+
  geom_text(aes(label=paste0(round(percentage*100,2),"%")),vjust=1.5,size=10)+
  theme_minimal()





#-------------------------------------------------------------------------------
#Hypothesis Proving


df_above_line_data = data.frame()
for (i in 1:6){
  City_dataset = filter(house_data_v11, City==uni_city[i])
  lm_model = lm(Rent_Price~House_Size, data =City_dataset)
  predicted_rent = predict(lm_model, newdata = City_dataset)
  rent_difference = City_dataset$Rent_Price - predicted_rent
  
  above_line_data = City_dataset %>% 
    mutate(Above_Line = ifelse(rent_difference > 0, "Above Line", "Below Line"))
  
  df_above_line_data = rbind(df_above_line_data,
                             above_line_data %>%
                              filter(Furnishing_Status=="Furnished", size_category=="Small")%>%
                              select(City,Furnishing_Status,Above_Line,size_category)%>%
                              group_by(City,Furnishing_Status,size_category)%>%
                              summarise(count = n(), Above_Line)%>%
                              filter(Above_Line == "Above Line")%>%
                              group_by(City,Furnishing_Status,Above_Line,size_category,count)%>%
                              summarise(Profit_Count = n())%>%
                              group_by(City,Furnishing_Status,Above_Line,size_category,Profit_Count,count)%>%
                              summarise(Percentage = Profit_Count/count)
    )
}
View(df_above_line_data)

#House Size vs Rent Price for each Size Category for Furnishing Status == Furnished
ggplot(data = filter(house_data_v11, Furnishing_Status=="Furnished"), mapping = aes(x=House_Size, y=Rent_Price))+
  geom_point(aes(color=size_category))+
  geom_smooth(method = "lm")+
  facet_wrap(~City)+
  labs(title = "House Size vs Rent Price for each Size Category for Furnishing Status == Furnished")




#-------------------------------------------------------------------------------
