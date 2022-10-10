library(dplyr)
data("Forbes2000", package = "HSAUR3")
str(Forbes2000)


#--------1----------------------------------------------------------------------
Forbes2000$expenses <- Forbes2000$sales - Forbes2000$profits
head(Forbes2000$expenses)

Forbes2000$profitable <- ifelse(Forbes2000$profits > 0, "Profitable", "Unprofitable")
addmargins(table(Forbes2000$profitable))

Forbes2000$US <- ifelse(Forbes2000$country == "United States", "American", "Non-American")
addmargins(table(Forbes2000$US))
round(addmargins(prop.table(table(Forbes2000$US))),2)

nrow(subset(Forbes2000, US == "American" & profitable == "Unprofitable"))

Forbes2000$expenses <- NULL


#--------2----------------------------------------------------------------------
top_10_ql <- head(Forbes2000[, c("name","country","category")], 10); top_10_ql

top_10_qnt <- head(Forbes2000[, c("name","sales","profits","assets")], 10);
top_10_qnt


#--------3----------------------------------------------------------------------
Forbes3 <- subset(Forbes2000, select = -c(assets, marketvalue), rank > 100 &
                  US == "Non-American")
Forbes3 <- head(Forbes3[order(-Forbes3$profits), ], -100)


#--------4----------------------------------------------------------------------
world_banks <- subset(Forbes2000, category == "Telecommunications services")
world_banks <- world_banks[order(-world_banks$sales), ]
nrow(world_banks[world_banks$country == "Greece",])
world_banks[world_banks$country == "Greece", c("rank","name")]


#--------5----------------------------------------------------------------------
Forbes1000GR <- subset(head(Forbes2000, 1000), select = -assets, country == "Greece")

nrow(Forbes2000[Forbes2000$country == "Greece" & Forbes2000$category == "Banking",])
nrow(Forbes1000GR[Forbes1000GR$country == "Greece" & Forbes1000GR$category == "Banking",])


#--------6----------------------------------------------------------------------
Forbes6A <- head(Forbes2000, -1)
Forbes6B <- head(Forbes2000, -2)


#--------7----------------------------------------------------------------------
FGI <- subset(Forbes2000, country %in% c("France", "Germany", "Italy"))
US <- subset(Forbes2000, country == "United States")

FGIUS <- rbind(FGI, US)

#FGI sales
FGI$sales_cat <- cut(FGI$sales,
                     breaks = seq(0, ceiling(max(FGI$sales)), max(FGI$sales)/3),
                     labels = c("low", "medium", "high"))
summary(FGI$sales_cat)
#frequencies table
addmargins(table(droplevels(FGI$country), FGI$sales_cat))


#--------8----------------------------------------------------------------------
Forbes8A <- subset(Forbes2000, select = c("name", "country", "sales"), rank == 1001:1100)
Forbes8B <- subset(Forbes2000, select = c("name","sales","marketvalue"), is.na(Forbes2000$profits))


#--------9----------------------------------------------------------------------
spain_all <- Forbes2000[Forbes2000$country == "Spain",]
spain_dmgs <- Forbes2000[Forbes2000$country == "Spain" & Forbes2000$profitable == "Unprofitable",] #percentage
cat(round((nrow(spain_dmgs) / nrow(spain_all)) * 100, 2), "%")


#--------11---------------------------------------------------------------------
#--------1----------------------------------------------------------------------
Forbes2000 <- Forbes2000 %>% mutate(Forbes2000, expenses = sales - profits)
Forbes2000 <- Forbes2000 <- Forbes2000 %>%
  mutate(profitable = factor(ifelse(profits > 0, "Profitable", "Unprofitable")))

Forbes2000$profitable %>% table() %>% addmargins()

Forbes2000 <- Forbes2000 %>%
              mutate(US = factor(ifelse(
                country == "United States", "American","Non-American")))

Forbes2000$US %>% table() %>% addmargins()

Forbes2000 %>% filter(US == "American" & profitable == "Unprofitable") %>% nrow()

Forbes2000 <- Forbes2000 %>% select(-expenses)


#--------11---------------------------------------------------------------------
#--------2----------------------------------------------------------------------
top_10_ql <- Forbes2000 %>% select(name, country, category) %>% head(10)
top_10_qnt <- Forbes2000 %>% select(name, sales, profits, assets) %>% head(10)


#--------11---------------------------------------------------------------------
#--------3----------------------------------------------------------------------
Forbes3 <- Forbes2000 %>% filter(rank > 100 & US == "Non-American") %>% 
          select(-c(assets, marketvalue))
           

Forbes3 <- Forbes3[order(-Forbes3$profits),] %>% head(-100)


#--------11---------------------------------------------------------------------
#--------4----------------------------------------------------------------------
world_banks <- Forbes2000 %>% filter(category == "Telecommunications services")
world_banks <- world_banks %>% arrange(-sales)
world_banks %>% filter(country == "Greece") %>% nrow()
world_banks %>% filter(country == "Greece") %>% select(c(rank, name)) 
world_banks[world_banks$country == "Greece", c("rank","name")]

#--------11---------------------------------------------------------------------
#--------5----------------------------------------------------------------------
Forbes1000GR <- Forbes2000 %>% filter(country == "Greece") %>%
                select(-assets) %>% head(1000)

Forbes2000 %>% filter(country == "Greece" & category == "Banking")

Forbes2000 %>% filter(country == "Greece" & category == "Banking") %>% nrow()
Forbes1000GR %>% filter(country == "Greece" & category == "Banking") %>% nrow()


#--------11---------------------------------------------------------------------
#--------6----------------------------------------------------------------------
Forbes6A <- Forbes2000 %>% head(-1)
Forbes6B <- Forbes2000 %>% head(-2)


#--------11---------------------------------------------------------------------
#--------7----------------------------------------------------------------------
FGI <- Forbes2000 %>% filter(country %in% c("France", "Germany", "Italy"))
US <- Forbes2000 %>% filter(country == "United States")

FGIUS <- rbind(FGI, US)

#FGI sales
FGI <- FGI %>% mutate(sales_cat = cut(sales, breaks = seq(0, ceiling(max(FGI$sales)), max(FGI$sales)/3),
                            labels = c("$0-$52.4m", "$52.4m-$105m", "$105m-$157m")))

summary(FGI$sales_cat)
#frequencies table
FGI %>% mutate(country = droplevels(country)) %>%
        select(c(country, sales_cat)) %>% table() %>% addmargins()


#--------11---------------------------------------------------------------------
#--------8----------------------------------------------------------------------
Forbes8A <- Forbes2000 %>% filter(rank == 1001:1100) %>% select(c(name, country, sales))
Forbes8B <- Forbes2000 %>% filter(is.na(profits)) %>% select(c(name, sales, marketvalue))


#--------11---------------------------------------------------------------------
#--------9----------------------------------------------------------------------
spain_all <- Forbes2000 %>% filter(country == "Spain")
spain_dmgs <- Forbes2000 %>% filter(country == "Spain" & profitable == "Unprofitable")
cat(round((nrow(spain_dmgs) / nrow(spain_all)) * 100, 2), "%")

