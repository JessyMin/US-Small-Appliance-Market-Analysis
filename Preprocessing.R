# Setup
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)

##### Scrapping Data #####
# URL for Scrapping
base_url1 <- "https://www.surlatable.com/category/TCA-"
base_url2 <- "&No=0&Nrpp=200&pCat=CAT-351560"
category_list <- c("257800/Blenders?N=17841", 
                   "257802/Food+Processors?N=17842", 
                   "333580/Hand+Mixers?N=17843",
                   "333582/Immersion+Blenders?N=17844", 
                   "257820/Toasters+&+Ovens?N=17847", 
                   "257818/Multi+Cookers+&+Slow+Cookers?N=17857", 
                   "258207/Coffee+Makers?N=18011", 
                   "258225/Espresso+Machines?N=18012")


# Scrap data from website
df <- data.frame()

for (i in 1:length(category_list)) {
        category <- category_list[i]
        raw_data <- read_html(paste0(base_url1, category, base_url2))
        
        # Get Item name
        name <- html_nodes(raw_data, css='.name') %>% html_text() %>%
                str_split(" ", 2)
        
        # Extract Brand name
        brand = lapply(name, function(x) {x[c(1)]}) %>% unlist() 
        category_data <- data.frame("brand" = brand)
        
        # Extract Product name
        product_name <- lapply(name, function(x) {x[c(2)]}) %>% unlist()
        category_data$product_name <- product_name
        
        # Extract Price
        price <- html_nodes(raw_data, css='.product-display-price') %>% html_text()
        price <- substring(price, 2)
        category_data$price <- price
        
        # Set Category
        category_data$category <- category
        
        # Merge dataframe 
        df <- rbind(df, category_data)
        }

rm(base_url1, base_url2, category, category_list, category_data, i, name, brand, 
   price, product_name) 


###### Cleaning Data #####
# Clean Category name
df$category <- str_sub(df$category, 8) %>%
        str_sub(end = -9) %>%
        str_replace_all("\\+", " ")

# Clean Price
df$price <- str_sub(df$price, end = 7) %>% #최저가 추출 
        str_replace_all(",", "") %>% 
        str_replace_all("-", "") 

df$price <- as.numeric(df$price)

# Clean Brand name
df$brand <- df$brand %>% str_replace("®", "")

unique(df$brand)
filter(df, brand == 'The')
df$brand[df$brand == 'The'] <- 'Dualit'

# Save backup data
date <- Sys.Date()
write.csv(df, file=paste0("df_backup_",date), row.names=F)


##### Remove overlapping Data #####
#제품 중복여부 확인
length(df$product_name) - length(unique(df$product_name)) 

df <- group_by(df, product_name) %>%
        mutate(overlap = n()) #제품 개수 카운트
df <- data.frame(df)

#중복된 제품 별도 추출
df_overlap <- filter(df, overlap == 2) 
ggplot(df_overlap, aes(category)) + 
        geom_bar(stat="count", fill="steelblue", width=0.4) +
        geom_text(stat="count", aes(label=..count..), vjust=-0.2, size=3.5)


# 제거할 제품 리스트만 남기기
remove_list <- filter(df_overlap, category != "Espresso Machines")
remove_list <- remove_list[-(remove_list$category == "Food Processors"),]

# 전체 데이터셋에서 제거 
df <- setdiff(df, remove_list)
df <- df[,-5]
rm(df_overlap, remove_list)


##### Filtering Data #####
# 분석대상 브랜드 필터링
brand_list = c("Breville", "Vitamix", "Kitchenaid", "All-Clad","Braun", "Cuisinart",
               "De’Longhi","Dualit","JURA","Kenwood","Miele","Nespresso","SMEG",
               "Technivorm","Wolf")

over_5 <- df %>% group_by(brand) %>%
        summarise(num_product = length(product_name)) %>%
        filter(num_product >= 5)

brand_list <- union(brand_list, unlist(over_5[,1]))

df$brand <- as.character(df$brand)
df <- filter(df, brand %in% brand_list)

rm(brand_list, over_5)


##### Add region column ######
df <- mutate(df, region = case_when(
        df$brand %in% c("All-Clad","Capresso","Cuisinart","KitchenAid","Omega", 
                        "Vitamix","Wolf") ~ "US",
        df$brand %in% c("Braun","De’Longhi","Dualit","Gaggia","JURA","Kenwood",
                        "Krups","Miele","Nespresso","Smeg","Technivorm") ~ "Europe",
        df$brand == "Breville" ~ "Australia")
        )

##### Factorize some variables #####
df$brand <- as.factor(df$brand)
df$category <- factor(df$category, 
                      c("Blenders","Immersion Blenders","Hand Mixers",
                        "Food Processors","Coffee Makers","Espresso Machines",
                        "Multi Cookers & Slow Cookers","Toasters & Ovens"))
df$region <- as.factor(df$region)


##### Save File #####
date <- Sys.Date()
write.csv(df, file=paste0("Small_Appliance_",date), row.names=F)