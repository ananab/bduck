library(feather)
library(tidyverse)

data <- read_feather("results/data.feather")

pjts <- data %>% 
  group_by(d_r_uuid,version,license_id) %>% 
  summarise(count = n()) %>% 
  arrange(desc(version))

# version_dists <- unlist(lapply(1:nrow(pjts)-1, function(i) agrepl(pjts$version[i], pjts$version[i+1])))

head(data)
pjt_by_lic <- data %>% 
  group_by(license_id) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

pjt_by_dns <- data %>% 
  group_by(dns) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

pjt_by_dws <- data %>% 
  group_by(dws) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

pjt_by_so <- data %>% 
  group_by(so) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

pjt_by_uuid <- data %>% 
  group_by(d_r_uuid, dns) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Hatice

data_unique <- data %>% unique() 

data_counts<- data %>% 
  group_by(d_r_uuid, version, license_id) %>% 
  mutate(license_id_c = n())%>%  
  ungroup() %>% 
  group_by(d_r_uuid, version, dns) %>% 
  mutate(dns_c=n()) %>% 
  ungroup() %>% 
  group_by(d_r_uuid, version, dws) %>% 
  mutate(dws_c=n()) %>% 
  ungroup() %>% 
  group_by(d_r_uuid, version, so) %>% 
  mutate(so_c=n()) %>% 
  ungroup() %>% 
  select(d_r_uuid,version, dns_c, dws_c, so_c, license_id_c)

data_small <- data_counts[1:500, 3:6]

# plot(data_small$so_c,data_small$license_id_c)


cov_matrix <- cov(data_small)
eigen_vectors <- eigen(cov_matrix)$vectors
eigen_values <- eigen(cov_matrix)$values
eigen_values[1]/sum(eigen_values)

y <- data.frame(pc1=as.matrix(data_small) %*% eigen_vectors[,1], pc2=as.matrix(data_small) %*% eigen_vectors[,2])

ggplot(y, aes(x=pc1,y=pc2)) + geom_jitter()

# center the data

data_small_c <- scale(data_small, center = TRUE)
cov_matrix_c <- cov(data_small_c)
eigen_vectors_c <- eigen(cov_matrix_c)$vectors
eigen_values_c <- eigen(cov_matrix_c)$values
(eigen_values_c[1] +eigen_values_c[2])/sum(eigen_values_c)

y_c <- data.frame(pc1_c=as.matrix(data_small_c) %*% eigen_vectors_c[,1], pc2_c=as.matrix(data_small_c) %*% eigen_vectors_c[,2])

ggplot(y_c, aes(x=pc1_c,y=pc2_c)) + geom_jitter()

# Factor1 explains dws_c and so_c  
fact_analysis <- factanal(data_small_c, factor=1, rotation = "varimax")
fact_analysis$loadings

# Plotting dws_c vs license_id_c

ggplot(as.data.frame(data_small), aes(x=license_id_c,y=dws_c)) + geom_jitter() + ylim(1,10)

# Plotting dns_c vs license_id_c

ggplot(as.data.frame(data_small), aes(x=license_id_c,y=dns_c)) + geom_jitter() 

# cluster analyis

m_clust <- Mclust(data_small)
clusters <- m_clust$classification
head(clusters)
table(clusters)

# kmeans (confirming two clusters)

d <- dist(data_small, method = "euclidean")
a <- pam(d, k = 2)
plot(a)

data_small$cluster <- m_clust$classification

##################################################
# GROUP MEETING (03/17/2017)
################################################

prj <- data %>% 
  group_by(d_r_uuid,version) %>% 
  summarise(total_num=n()) %>% 
  arrange(desc(total_num))

# short_list <- as.vector(prj[prj$total_num>12740,]$d_r_uuid)

short_list <- prj$d_r_uuid[1:5]

short_data <- data %>% 
  filter(d_r_uuid %in% short_list)

# trial with 16 projects
# distance_matrix <- matrix(rep(0,256), nrow=16, ncol=16)

distance_matrix <- matrix(nrow = nrow(short_data),ncol = nrow(short_data))

for(i in 1:16){
  selected_data_left <- short_data %>% 
    filter(d_r_uuid %in% c(short_list[i])) %>% 
    select(-d_r_uuid,-version)
  for(j in 1:16){
    selected_data_right <- short_data %>% 
      filter(d_r_uuid %in% c(short_list[j])) %>% 
      select(-d_r_uuid,-version)
    data_join<- inner_join(selected_data_left, selected_data_right)
    distance_matrix[i,j] <- nrow(data_join)
  }
}
a <- pam(distance_matrix, k = 2)
plot(a)



#data <- data %>%
#  mutate(d_r_uuid = as.character(d_r_uuid), version = as.character(version))

# set1 <- data %>% filter(version == "8u72-b15", d_r_uuid == "2ff38a7c-238b-487f-af9b-64a2ec81d81c")

glimpse(data)

prj1 <- data %>% 
  group_by(d_r_uuid, version, license_id) %>% 
  summarise(n())









pjt_by_so %>% 
  ggplot(aes(x = count))+
  geom_histogram()

pjt_by_dns %>% 
  ggplot(aes(x = count))+
  geom_histogram()

pjt_by_dws %>% 
  ggplot(aes(x = count))+
  geom_histogram()

pjt_by_uuid %>% 
  ggplot(aes(x = count))+
  geom_histogram()

pjt_by_lic %>% 
  ggplot(aes(x = count))+
  geom_histogram()

data %>% 
  ggplot(aes(x = license_id))+geom_bar()

top_lic <- left_join(head(pjt_by_lic,100), data)

top_uuid <- data %>% 
  filter(d_r_uuid %in% pjt_by_uuid$d_r_uuid) %>% 
  
  top_dns <- data %>% 
  arrange(dns) %>% 
  head(100)

top_dws <- data %>% 
  arrange(dws) %>% 
  head(100)

top_so <- data %>% 
  arrange(so) %>% 
  head(100)












