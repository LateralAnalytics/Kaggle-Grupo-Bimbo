
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(treemap)
library(stringr)
library(tm)
library(SnowballC)
library(skmeans)
library(mice)
library(VIM)
library(data.table)
library(xgboost)
library(Matrix)
library(caret)
library(tidyr)


### Set Working directory
setwd("C:/Users/John Doe/Google Drive/Kaggle/GrupoBimbo")

rm(list=ls(all=TRUE))
gc()
train <- read_csv("Data/train.csv")
train <- data.table(train)
test <- read_csv("Data/test.csv")
test <- data.table(test)
client <- read_csv("Data/cliente_tabla.csv")
client <- data.table(client)
product <- read_csv("Data/producto_tabla.csv")
product <- data.table(product)
town <- read_csv("Data/town_state.csv")
town <- data.table(town)
gc()

if(FALSE){
  ########################
  ### Data Exploration ###
  ########################
  # https://www.kaggle.com/fabienvs/grupo-bimbo-inventory-demand/grupo-bimbo-data-analysis/comments
  
  ### Weeks
  ### Thursday is day 3.  The largest orders occur on Thursday and Friday.
  ### This makes sense, because they need time to stock it for the weekend.
  ggplot(train %>% sample_frac(0.05))+
    geom_histogram(aes(x=Semana), fill="red")+
    scale_x_continuous(breaks=1:10)+
    scale_y_continuous(name="Client / Product deliveries")+
    theme_bw()
  
  
  # Agencies & States
  ### There are many agencies with a small amount of orders and a few with large orders.
  agencias <- train %>%
    group_by(Agencia_ID) %>%
    summarise(Units = sum(Venta_uni_hoy),
              Pesos = sum(Venta_hoy),
              Return_Units = sum(Dev_uni_proxima),
              Return_Pesos = sum(Dev_proxima),
              Net = sum(Demanda_uni_equil)) %>%
    mutate(Net_Pesos = Pesos - Return_Pesos,
           Return_Rate = Return_Units / (Units+Return_Units)) %>%
    arrange(desc(Units)) %>%
    inner_join(town, by="Agencia_ID")
  
  ggplot(agencias, aes(x=Units/7))+
    geom_histogram(fill="red", color="gray", binwidth=10000)+
    scale_x_continuous(name="Units / Week", labels=function(x)paste(x/1000, "k"))+
    scale_y_continuous(name="Agencias")+
    theme_bw()
  
  ### Agencia 1114 has a large volume and a high percentage of returns.
  treemap(agencias[1:100, ], 
          index=c("Agencia_ID"), vSize="Units", vColor="Return_Rate", 
          palette=c("#FFFFFF","#FFFFFF","#FF0000"),
          type="value", title.legend="Units return %", title="Top 100 agencias")
  
  
  top30agencias <- agencias$Agencia_ID[1:30]
  top100agencias <- agencias$Agencia_ID[1:100]
  
  ### Not all agencia's follow the same trend by weekday.
  agencias.history <- train %>%
    group_by(Agencia_ID, Semana) %>%
    summarise(Units = sum(Venta_uni_hoy),
              Pesos = sum(Venta_hoy),
              Return_Units = sum(Dev_uni_proxima),
              Return_Pesos = sum(Dev_proxima),
              Net = sum(Demanda_uni_equil)) %>%
    mutate(Net_Pesos = Pesos - Return_Pesos,
           Avg_Pesos = Pesos / Units,
           Return_Rate = Return_Units / (Units+Return_Units)) %>%
    arrange(Agencia_ID, Semana) %>%
    inner_join(town, by="Agencia_ID")
  
  ggplot(agencias.history %>% filter(Agencia_ID %in% top30agencias))+
    geom_bar(aes(x=Semana, y=Units, fill=Return_Rate), stat="identity", color="black")+
    facet_wrap(~Agencia_ID)+
    scale_y_continuous(labels=function(x)paste(x/1000, "k"))+
    scale_fill_gradient(name="Units\nReturn %", low="white", high="red")+
    ggtitle("Top 30 agencias")+
    theme_bw()
  
  ### There are a few states that have most of the sales.
  ### Mexico City has a large return percentage on Wednesday.
  states <- agencias.history %>%
    group_by(State, Semana) %>%
    summarise(Units = sum(Units),
              Pesos = sum(Pesos),
              Return_Units = sum(Return_Units),
              Return_Pesos = sum(Return_Pesos),
              Net = sum(Net)) %>%
    mutate(Avg_Pesos = Pesos / Units,
           Return_Rate = Return_Units / (Units+Return_Units)) %>%
    arrange(desc(Units))
  
  ggplot(states)+
    geom_bar(aes(x=Semana, y=Units, fill=Return_Rate), stat="identity", color="black")+
    scale_y_continuous(labels=function(x)paste(x/1e6, "m"))+
    scale_fill_gradient(name="Units\nReturn %", low="white", high="red")+
    facet_wrap(~State)+
    ggtitle("States")+
    theme_bw()
  
  
  
  
  ### Canals
  ### Sales Channel 1 is massive compared to the others
  
  canals <- train %>%
    group_by(Canal_ID, Semana) %>%
    summarise(Units = sum(Venta_uni_hoy),
              Pesos = sum(Venta_hoy),
              Return_Units = sum(Dev_uni_proxima),
              Return_Pesos = sum(Dev_proxima),
              Net = sum(Demanda_uni_equil)) %>%
    mutate(Net_Pesos = Pesos - Return_Pesos,
           Avg_Pesos = Pesos / Units,
           Return_Rate = Return_Units / (Units+Return_Units)) %>%
    arrange(desc(Units))
  
  treemap(canals, index=c("Canal_ID"), vSize="Units", type="index", title="Canals repartition")
  
  ### Sales channel 5 has a lot of returns on Wednesday.
  ggplot(canals)+
    geom_bar(aes(x=Semana, y=Units, fill=Return_Rate), stat="identity", color="black")+
    scale_y_continuous(labels=function(x)paste(x/1e6, "m"))+
    scale_fill_gradient(name="Units\nReturn %", low="white", high="red")+
    facet_wrap(~Canal_ID, scale="free")+
    theme_bw()
  
  
  ### Canals x Agencies
  ### Most agencia's only have 1 canal, but a few have more.
  agencias.canals <- train %>%
    group_by(Agencia_ID) %>%
    summarise(n_canals = n_distinct(Canal_ID))
  
  ggplot(agencias.canals)+
    geom_bar(aes(x=Agencia_ID, y=n_canals), stat="identity", color="black", alpha = 0.25)+
    scale_y_continuous(labels=function(x)paste(x))+
    theme_bw()
  
  
  
  
  
  #### Routes
  ### Most routes have a small sales volume each week, but a few have more
  routes <- train %>% group_by(Ruta_SAK) %>%
    summarise(n_Agencias = n_distinct(Agencia_ID),
              n_Clients = n_distinct(Cliente_ID),
              Units=sum(Venta_uni_hoy),
              Return_Units = sum(Dev_uni_proxima)) %>%
    mutate(Return_Rate = Return_Units / (Units+Return_Units)) %>%
    arrange(desc(Units))
  
  ggplot(routes, aes(x=Units/7))+
    geom_histogram(fill="red", color="gray", binwidth=5000)+
    scale_x_continuous(name="Units / Week", labels=function(x)paste(x/1000, "k"))+
    scale_y_continuous(name="Routes")+
    theme_bw()
  
  
  top100routes <- routes$Ruta_SAK[1:100]
  
  
  ### Routes x Agencies
  ### Some sales depots have a lot of routes
  ### Some routes have a lot of sales depots
  ### Most don't overlap very much
  ### The lower numbers for both seem to overlap more
  routes.agencias <- train %>% group_by(Ruta_SAK, Agencia_ID) %>%
    summarise(count=n(),
              n_Clients = n_distinct(Cliente_ID),
              Units=sum(Venta_uni_hoy),
              Return_Units = sum(Dev_uni_proxima)) %>%
    mutate(Return_Rate = Return_Units / (Units+Return_Units)) %>%
    arrange(desc(Units))
  
  #ggplot(routes.agencias %>% 
  #         filter(Ruta_SAK %in% top100routes, Agencia_ID %in% top100agencias))+
    ggplot(routes.agencias)+  
    geom_point(aes(x=as.character(Ruta_SAK), 
                   y=as.character(Agencia_ID), 
                   size=Units, color=Return_Rate))+
    scale_x_discrete(name="Routes")+
    scale_y_discrete(name="Agencies")+
    scale_color_gradient(name="Return Rate", low="blue", high="red")+
    ggtitle("agencies & routes")+
    theme_bw()+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank())
  
  
  ### Clients
  ### Puebla Reissoni dominates as a client
  ### It's bigger than all the various Walmart variations combined.
  ### It's probalby not a real client, but some detail in Grupo Bimbo's supply chain.
    sales <- train %>%
    group_by(Cliente_ID) %>%
    summarise(Units = sum(Venta_uni_hoy),
              Pesos = sum(Venta_hoy),
              Return_Units = sum(Dev_uni_proxima),
              Return_Pesos = sum(Dev_proxima),
              Net = sum(Demanda_uni_equil)) %>%
    mutate(Return_Rate = Return_Units / (Units+Return_Units),
           Avg_Pesos = Pesos / Units) %>%
    mutate(Net_Pesos = Pesos - Return_Pesos) %>%
    inner_join(client, by="Cliente_ID") %>%
    arrange(desc(Pesos))
  
  treemap(sales[1:100, ], 
          index=c("NombreCliente"), vSize="Units", vColor="Return_Rate", 
          palette=c("#FFFFFF","#FFFFFF","#FF0000"),
          type="value", title.legend="Units return %", title="Top 100 clients")
  
  ### Most clients sell a small amount and a few sell a lot
  sales$Cum_Units <- cumsum(sales$Units) / sum(sales$Units)
  s <- seq(1, 800000, 100)
  ggplot()+geom_line(aes(x=s, y=sales$Cum_Units[s]))+
    scale_x_continuous(name="Clients", labels=function(x) paste(x/1000, "k"))+
    scale_y_continuous(name="Cumulative share (units)", labels=percent)+
    ggtitle("Clients repartition")+
    theme_bw()
  
  sales$share <- sales$Units / sum(sales$Units)
  
  
  
  ### Clients x Agencies
  ### Most clients have a small number of agencies
  ### A few have a lot
  agencias.by.client <- train %>%
    group_by(Cliente_ID) %>%
    summarise(n_agencias = n_distinct(Agencia_ID)) %>%
    inner_join(client, by="Cliente_ID")
  
  table(agencias.by.client$n_agencias)
  
  agencias.by.client %>% filter(n_agencias %in% c(5, 9, 62))
  
  
  
  
  ### Clients x Canals
  ### Most clients only have 1 sales channel, but some have more.
  clients.canals <- train %>%
    group_by(Cliente_ID) %>%
    summarise(n_canals = n_distinct(Canal_ID))
  
  table(clients.canals$n_canals)
  
  
  
  
  ### Clients x Routes
  ### Most clients have a small number of routes
  clients.routes <- train %>%
    group_by(Cliente_ID) %>%
    summarise(n_routes = n_distinct(Ruta_SAK))
  
  ggplot(clients.routes)+
    geom_histogram(aes(x=n_routes), fill="red", color="black", alpha="0.3", binwidth=1)+
    scale_x_continuous(name="Number of routes")+
    scale_y_continuous(name="Number of clients", labels=function(x)paste(x/1000, "k"))+
    theme_bw()
  
  
  ### Products
  ### There are a small number of big items and a lot of small ones
  ### The low sales items have higher return percentages, which makes sense
  products <- train %>% group_by(Producto_ID) %>%
    summarise(Units = sum(Venta_uni_hoy),
              Pesos = sum(Venta_hoy),
              Return_Units = sum(Dev_uni_proxima),
              Return_Pesos = sum(Dev_proxima),
              Net = sum(Demanda_uni_equil)) %>%
    mutate(Avg_Pesos = Pesos / Units,
           Return_Rate = Return_Units / (Units+Return_Units)) %>%
    filter(!is.nan(Avg_Pesos)) %>%
    inner_join(product, by="Producto_ID") %>%
    arrange(desc(Units))
  
  products$NombreProducto <- factor(as.character(products$NombreProducto), levels=products$NombreProducto)
  
  treemap(products[1:100, ], 
          index=c("NombreProducto"), vSize="Units", vColor="Return_Rate", 
          palette=c("#FFFFFF","#FFFFFF","#FF0000"),
          type="value", title.legend="Units return %", title="Top 100 products")
  
  ### Most products cost around 10 pesos
  ggplot(products, aes(x=Avg_Pesos))+
    geom_histogram(aes(y=..density..), fill="gray", color="black", alpha="0.3")+
    geom_density(fill="red", alpha="0.3")+
    scale_x_continuous(name="Products average price", lim=c(0, 50))+
    scale_y_continuous(name="Density", labels=percent)+
    theme_bw()
  
  
  top100products <- products$Producto_ID[1:100]
  
  
  ### Products x Agencies
  ### Most agencies have between 50-220 products
  agencias.products <- train %>% group_by(Agencia_ID) %>%
    summarise(n_products = n_distinct(Producto_ID))
  
  ggplot(agencias.products)+
    geom_histogram(aes(x=n_products), fill="red", color="black", alpha="0.3", binwidth=10)+
    scale_x_continuous(name="Number of products")+
    scale_y_continuous(name="Number of agencies")+
    theme_bw()
  
  
  ### Products x Canals
  ### Most products only have one sales channel
  canals.products <- train %>% group_by(Producto_ID) %>%
    summarise(n_canals = n_distinct(Canal_ID))
  
  ggplot(canals.products)+
    geom_histogram(aes(x=n_canals), fill="red", color="black", alpha="0.3", binwidth=1)+
    scale_x_continuous(name="Number of canals", breaks=1:10, lim=c(1, 10))+
    scale_y_continuous(name="Number of products")+
    theme_bw()
  
  
  ### Products x Routes
  ### Most products only have 1 route
  routes.products <- train %>% group_by(Producto_ID) %>%
    summarise(n_routes = n_distinct(Ruta_SAK))
  
  ggplot(routes.products)+
    geom_histogram(aes(x=n_routes), fill="red", color="black", alpha="0.3", binwidth=10)+
    scale_x_continuous(name="Number of routes")+
    scale_y_continuous(name="Number of products")+
    theme_bw()
  
  routes.products <- train %>% group_by(Ruta_SAK) %>%
    summarise(n_products = n_distinct(Producto_ID))
  
  ggplot(routes.products)+
    geom_histogram(aes(x=n_products), fill="red", color="black", alpha="0.3", binwidth=10)+
    scale_x_continuous(name="Number of products")+
    scale_y_continuous(name="Number of routes")+
    theme_bw()
  
  ### Some products are on a lot of routes.
  ### Most are not.
  ### Higher route numbers display different behavior
  ### They must be for a different customer or sales channel
  routes.products <- train %>% group_by(Ruta_SAK, Producto_ID) %>%
    summarise(count=n(),
              n_Agencias = n_distinct(Agencia_ID),
              n_Clients = n_distinct(Cliente_ID),
              Units=sum(Venta_uni_hoy),
              Return_Units = sum(Dev_uni_proxima)) %>%
    mutate(Return_Rate = Return_Units / (Units+Return_Units)) %>%
    arrange(desc(Units))
  
  ggplot(routes.products %>% 
           filter(Ruta_SAK %in% top100routes, Producto_ID %in% top100products))+
    geom_point(aes(x=as.character(Ruta_SAK), 
                   y=as.character(Producto_ID), 
                   size=Units, color=Return_Rate))+
    scale_x_discrete(name="Ruta SAK")+
    scale_y_discrete(name="Product ID")+
    scale_color_gradient(name="Return Rate", low="blue", high="red")+
    ggtitle("Top 100 products & routes")+
    theme_bw()+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank())
  
  
  ### Products x Clients
  ### Most clients have a small amount of products
  products.by.client <- train %>%
    group_by(Cliente_ID) %>%
    summarise(n_products = n_distinct(Producto_ID)) %>%
    inner_join(client, by="Cliente_ID")
  
  ggplot(products.by.client)+
    geom_histogram(aes(x=n_products), fill="red", color="black", alpha="0.3", binwidth=2)+
    scale_x_continuous(name="Number of products by client", lim=c(0, 150))+
    scale_y_continuous(name="Number of clients", labels=function(x)paste(x/1000, "k"))+
    theme_bw()
  
  
  remove(agencias, top30agencias, top100agencias, agencias.history, states, canals, agencias.canals, 
         routes, top100routes,        routes.agencias, sales, agencias.by.client, clients.canals, 
         clients.routes, products, top100products, agencias.products, canals.products, routes.products, 
         products.by.client)
}
  
  
### Data Exploration Results ###
### This data is as to be expected for retail supply chain data.  It is often skewed right
### It will need to be tranformed to approach the normal distribution

#########################################
### Missing Data / Feature Generation ###
#########################################
  
  len_train <- nrow(train)
  len_complete <- nrow(na.omit(train))
  ### len_train = 74180464 = len_complete
  ### There isn't any missing data
  
  ##########################
  ### Feature Generation ###
  ##########################
  
  ### Break out the numbers at the beginning of the Town field of the town dataframe
  # returns string w/o leading or trailing whitespace
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  town$town_num <- trim(str_extract(town$Town,"[0-9]{1,7}"))
  
  ### Break the product name into different parts in the product table
  # https://www.kaggle.com/caiotaniguchi/grupo-bimbo-inventory-demand/basic-preprocessing-for-prods-modified
  
  preprocess_products <- function(product) {
    
    product_names <- as.character(product$NombreProducto)
    weight <- unlist(lapply(product_names, extract_weight))
    pieces <- unlist(lapply(product_names, extract_pieces))
    brand <- unlist(lapply(product_names, extract_brand))
    is_beverage <- unlist(lapply(product_names, grepl, pattern='\\d+ml'))
    is_healthy <- unlist(lapply(product_names, grepl, pattern='(100|0)pct|Multigrano|Integral|Fibra'))
    has_choco <- unlist(lapply(product_names, grepl, pattern="Choco"))
    has_vanilla <- unlist(lapply(product_names, grepl, pattern="Va(i)?nilla"))
    
    df = data.frame(
      ID=product$Producto_ID,
      product_name=product_names,
      brand=brand,
      weight=weight,
      pieces=pieces,
      weight_per_piece=weight/pieces,
      is_beverage=is_beverage,
      is_healthy=is_healthy,
      has_choco=has_choco,
      has_vanilla=has_vanilla
    )
    df[df$is_beverage, 'weight_per_piece'] = df[df$is_beverage, 'weight']
    df[df$is_beverage, 'weight'] = df[df$is_beverage, 'weight'] * df[df$is_beverage, 'pieces']
    df
  }
  
  extract_weight <- function(product_name) {
    weight_str <- strsplit(tolower(product_name), ' ')
    groups <- str_match_all(weight_str, '(\\d+)(k?g|ml)')
    weight <- strtoi(groups[[1]][2])
    unit <- groups[[1]][3]
    ifelse(unit == 'kg', 1000 * weight, weight)
  }
  
  extract_pieces <- function(product_name) {
    pieces_str <- str_match_all(tolower(product_name), '(\\d+)(p)\\b')[[1]][2]
    num_pieces = strtoi(pieces_str)
    if (is.na(num_pieces)) return(1)
    num_pieces
  }
  
  extract_brand <- function(product_name) {
    tokens <- strsplit(product_name, ' ')[[1]]
    tokens[length(tokens) - 1]
  }
  
  preprocessed <- preprocess_products(product)
  
  products <- product
  
  
  # Cluster produts together
  # https://www.kaggle.com/ybabakhin/grupo-bimbo-inventory-demand/products-clustering/code
  #' Title
  #'
  #' @param product_name 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  extract_shortname <- function(product_name) {
    # Split the name
    tokens <- strsplit(product_name, " ")[[1]]
    
    # Delete ID
    tokens <- head(tokens, length(tokens) - 1)
    
    # Delete Brands (name till the last token with digit)
    digit_indeces <- grep("[0-9]", tokens)
    
    # Product names without digits
    digit_index <- ifelse(length(digit_indeces) == 0, 1,
                          max(digit_indeces))
    paste(tokens[1:digit_index], collapse = " ")
  }
  
  
  products <- product 
  # Delete product with no name
  products <- products[2:nrow(products),]
  
  products$product_shortname <- unlist(lapply(products$NombreProducto, extract_shortname))
  
  # Short Names Preprocessing
  CorpusShort <- Corpus(VectorSource(products$product_shortname))
  CorpusShort <- tm_map(CorpusShort, tolower)
  CorpusShort <- tm_map(CorpusShort, PlainTextDocument)
  
  # Remove Punctuation
  CorpusShort <- tm_map(CorpusShort, removePunctuation)
  
  # Remove Stopwords
  CorpusShort <- tm_map(CorpusShort, removeWords, stopwords("es"))
  
  # Stemming
  CorpusShort <- tm_map(CorpusShort, stemDocument, language="es")
  
  # Create DTM
  CorpusShort <- Corpus(VectorSource(CorpusShort))
  dtmShort <- DocumentTermMatrix(CorpusShort)
  
  # Delete Sparse Terms (all the words now)
  sparseShort <- removeSparseTerms(dtmShort, 0.9999)
  ShortWords <- as.data.frame(as.matrix(sparseShort))
  
  # Create valid names
  colnames(ShortWords) <- make.names(colnames(ShortWords))
  
  # Spherical k-means for product clustering (30 clusters at the moment)
  set.seed(123)
  mod <- skmeans(as.matrix(ShortWords), 30, method = "genetic")
  products$cluster <- mod$cluster
  
  products$NombreProducto <- NULL
  
  
  ### Finalize Product Features ###
  product <- left_join(preprocessed, products, c("ID" = "Producto_ID"))
  
  ### Check for the percent missing values in the product dataframe with custom function
  pMiss <- function(x){sum(is.na(x))/length(x)*100}
  apply(product,2,pMiss)
  ### View with Mice package
  md.pattern(product)
  ### View with the VIM package
  aggr_plot <- aggr(product, col=c('navyblue','red'), numbers=TRUE, 
                    sortVars=TRUE, labels=names(product), cex.axis=.7, 
                    gap=3, ylab=c("Histogram of missing data","Pattern"))
  
  
  
  ### Very little data is missing.  
  ### Give the missing cluster value a new cluster value one greater than the current largest
  
  
  ### Update "NA" cluster value with current max cluster value plus 1
  product <- product %>%
    mutate(cluster = ifelse(is.na(cluster),max(product$cluster,na.rm=T)+1,cluster))
  
  ### Update "NA" product_shortname with "placeholder"
  product <- product %>%
    mutate(product_shortname = ifelse(is.na(product_shortname),"placeholder",product_shortname))
  
  
  ### Let's impute the rest with the Mice package
  product <- mice(product,m=1,maxit=1,meth='pmm',seed=500)
  product <- complete(product,1)
  
  remove(preprocessed, products, ShortWords, aggr_plot, CorpusShort, dtmShort, len_complete,
         len_train, mod, s, sparseShort)
  
  
  gc()
  ### Create variables for the train dataset ###
  # https://www.kaggle.com/bpavlyshenko/grupo-bimbo-inventory-demand/bimbo-xgboost-r-script-lb-0-457/code
  
  # Cut the train set to 8 and 9 weeks (Semana) for using only one week lags for target variable.
  # If you have enough memory, you can set up condition Semana>3 on the next row for using lagged values of target variable for 5 weeks. 
  train=train[Semana>3,]
  # remove train variables that aren't present in the test dataset
  train[,Venta_uni_hoy:=NULL]
  train[,Venta_hoy:=NULL]
  train[,Dev_uni_proxima:=NULL]
  train[,Dev_proxima:=NULL]
  
  # Modify train and test so that can be combined
  train$id=0
  train[,target:=Demanda_uni_equil]
  train[,Demanda_uni_equil:=NULL]
  train[,tst:=0]
  test$target=0
  test[,tst:=1]
  data=rbind(train,test)
  rm(test)  
  rm(train)
  
  # Take the log
  data$target=log(data$target+1)
  gc()
  
  
  # Creating features for one week lagged values of target variable 
  data1<-data[,.(Semana=Semana+1,Cliente_ID,Producto_ID,target,Ruta_SAK,Canal_ID)]
  data=merge(data,data1[Semana>8,.(targetl1=mean(target)), by=.(Semana,Cliente_ID,Producto_ID,Ruta_SAK,Canal_ID)],all.x=T,by=c("Semana","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID"))
  gc()
  
  # If you have enough memory, you can set up value TRUE in the following condition to make it possible to use lagged values of target variable 
  # for lagged 2-5 weeks as additional features 
  #y<-inner_join(dat, dat2, by=c("Parameter_Name", "Pollutant_Standard", "Sample_Duration"))
  
    data <- data.table(data)
    gc()
      data1<-data.table(data[,.(Semana=Semana+2,Cliente_ID,Producto_ID,target)])
      gc()
      data1<-data.table(data1[Semana>8,.(targetl2=mean(target)), by=.(Semana,Cliente_ID,Producto_ID)])
      gc()
      #data<-left_join(data, data1, by=c("Semana","Cliente_ID","Producto_ID"))
      data<-data1[data, on = c("Semana","Cliente_ID","Producto_ID")]
    gc()
      data1<-data.table(data[,.(Semana=Semana+3,Cliente_ID,Producto_ID,target)])
      gc()
      data1<-data.table(data1[Semana>8,.(targetl3=mean(target)), by=.(Semana,Cliente_ID,Producto_ID)])
      #data<-left_join(data, data1, by=c("Semana","Cliente_ID","Producto_ID"))
      data<-data1[data, on = c("Semana","Cliente_ID","Producto_ID")]
    gc()
      data1<-data.table(data[,.(Semana=Semana+4,Cliente_ID,Producto_ID,target)])
      gc()
      data1<-data.table(data1[Semana>8,.(targetl4=mean(target)), by=.(Semana,Cliente_ID,Producto_ID)])
      #data<-left_join(data, data1, by=c("Semana","Cliente_ID","Producto_ID"))
      gc()
      data<-data1[data, on = c("Semana","Cliente_ID","Producto_ID")]
    gc()
    
      data1<-data.table(data[,.(Semana=Semana+5,Cliente_ID,Producto_ID,target)])
      gc()
      data1<-data.table(data1[Semana>8,.(targetl5=mean(target)), by=.(Semana,Cliente_ID,Producto_ID)])
      #data<-inner_join(data, data1, by=c("Semana","Cliente_ID","Producto_ID"))
      data<-data1[data, on = c("Semana","Cliente_ID","Producto_ID")]
    gc()



    
    
    
  rm(data1)
  data=data[Semana>8,]
  gc()
  
  
  # Creating frequency features for some factor variables
  nAgencia_ID=data[,.(nAgencia_ID=.N),by=.(Agencia_ID,Semana)]
  nAgencia_ID=data.table(nAgencia_ID[,.(nAgencia_ID=mean(nAgencia_ID,na.rm=T)),by=Agencia_ID])
  #data=merge(data,nAgencia_ID,by='Agencia_ID',all.x=T)
  gc()
  data<-nAgencia_ID[data, on = "Agencia_ID"]
  remove(nAgencia_ID)
  gc()
  
  nRuta_SAK=data[,.(nRuta_SAK=.N),by=.(Ruta_SAK,Semana)]
  nRuta_SAK=data.table(nRuta_SAK[,.(nRuta_SAK=mean(nRuta_SAK,na.rm=T)),by=Ruta_SAK])
  #data=merge(data,nRuta_SAK,by='Ruta_SAK',all.x=T)
  gc()
  data<-nRuta_SAK[data, on = "Ruta_SAK"]
  remove(nRuta_SAK)
  gc()
  
  nCliente_ID=data[,.(nCliente_ID=.N),by=.(Cliente_ID,Semana)]
  nCliente_ID=data.table(nCliente_ID[,.(nCliente_ID=mean(nCliente_ID,na.rm=T)),by=Cliente_ID])
  #data=merge(data,nCliente_ID,by='Cliente_ID',all.x=T)
  gc()
  data<-nCliente_ID[data, on = "Cliente_ID"]
  remove(nCliente_ID)
  gc()
  
  nProducto_ID=data[,.(nProducto_ID=.N),by=.(Producto_ID,Semana)]
  nProducto_ID=data.table(nProducto_ID[,.(nProducto_ID=mean(nProducto_ID,na.rm=T)),by=Producto_ID])
  #data=merge(data,nProducto_ID,by='Producto_ID',all.x=T)
  gc()
  data<-data.table(data)
  data<-nProducto_ID[data, on = "Producto_ID"]
  remove(nProducto_ID)
  gc()
  
  
  nCanal_ID=data[,.(nCanal_ID=.N),by=.(Canal_ID,Semana)]
  nCanal_ID=nCanal_ID[,.(nCanal_ID=mean(nCanal_ID,na.rm=T)),by=Canal_ID]
  data=merge(data,nCanal_ID,by='Canal_ID',all.x=T)
  
  gc()
  
  ### Add in town features
  data=merge(data,town,by='Agencia_ID',all.x=T)
  data$Town <- NULL

### Add in product features
gc()
names(product)[names(product) == 'ID'] <- 'Producto_ID'
data=merge(data,product,by='Producto_ID',all.x=T)
data$product_name <- NULL
gc()


### Take the log
data$Producto_ID=log(data$Producto_ID+1) 
data$Agencia_ID=log(data$Agencia_ID+1)
data$Canal_ID=log(data$Canal_ID+1)
data$Cliente_ID=log(data$Cliente_ID+1)
data$Ruta_SAK=log(data$Ruta_SAK+1)
data$nAgencia_ID=log(data$nAgencia_ID+1)
data$nRuta_SAK=log(data$nRuta_SAK+1)
data$nCliente_ID=log(data$nCliente_ID+1)
data$nProducto_ID=log(data$nProducto_ID+1)
data$nCanal_ID=log(data$nCanal_ID+1)
data$town_num=log(as.integer(data$town_num)+1)
data$weight=log(data$weight+1)
data$weight_per_piece=log(data$weight_per_piece+1)
data$cluster=log(data$cluster+1)

### Remove "ID" because it doesn't add any value, and may add noise
data[,id:=NULL]

### Add in Client Name as a feature
### Some Client ID's show up more than once. Filter out at the end.
### Revisit this later
data<-unique(client[data, on = "Cliente_ID"])

data_train=data[tst==0,]
data_test=data[tst==1,]

features=names(data_train)[!(names(data_train) %in% c("target",'tst'))] 

rm(data)

set.seed(123)
wltst=sample(nrow(data_train),30000)  

dval<-xgb.DMatrix(data=data.matrix(data_train[wltst,features,with=FALSE]),
                  label=data.matrix(data_train[wltst,target]),missing=NA)
watchlist<-list(dval=dval)

clf <- xgb.train(params=list(booster = "gbtree",
                             eta = 0.1,
                             gamma = 0.75,
                             max_depth = 10,
                             min_child_weight = 1,
                             subsample = 1,
                             colsample_bytree = 1.25,
                             objective = "reg:linear",
                             base_score = 0.5,
                             eval_metric = 'rmse'),
                 data = xgb.DMatrix(data=data.matrix(data_train[-wltst,features,with=FALSE]),
                                    label=data.matrix(data_train[-wltst,target]),missing=NA), 
                 nrounds = 75, 
                 watchlist = watchlist,
                 verbose = 2,
                 print_every_n = 1,
                 early_stopping_rounds = 10,
                 maximize = FALSE,
                 nthread = 4)


# https://github.com/dmlc/xgboost/blob/master/R-package/vignettes/xgboostPresentation.Rmd
### View feature importance
importance_matrix <- xgb.importance(model = clf)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)








                                  
                                    
                                    
# Make prediction for the 10th week
gc()
data_test1=data_test[Semana==10,]
pred<-predict(clf,xgb.DMatrix(data.matrix(data_test1[,features,with=FALSE]),missing=NA))
res=exp(round(pred,5))-1

# Create lagged values of target variable which will be used as a feature for the 11th week prediction 
data_test_lag1=data_test1[,.(Cliente_ID,Producto_ID)]
data_test_lag1$targetl1=res
data_test_lag1=data_test_lag1[,.(targetl1=mean(targetl1)), by=.(Cliente_ID,Producto_ID)]


results=data.frame(id=data_test1$id,Demanda_uni_equil=res)

data_test2=data_test[Semana==11,]
data_test2[,targetl1:=NULL]


# Merge lagged values of target variable to test the set for the 11th week
data_test2=merge(data_test2,data_test_lag1,all.x=T,by=c('Cliente_ID','Producto_ID'))
pred<-predict(clf,xgb.DMatrix(data.matrix(data_test2[,features,with=FALSE]),missing=NA))
res=exp(round(pred,5))-1
res.df=data.frame(id=data_test2$id,Demanda_uni_equil=res)
results=rbind(results, res.df)



results[results[,2]<0,2]=0
results[,2]=round(results[,2],1)
results[,1]=as.integer(results[,1])
class(results[,1])='int32'
options(digits=18)
results<-unique(results)
write.csv(results,file='results1_modified.csv',row.names=F)



