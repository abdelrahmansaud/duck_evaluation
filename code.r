# Aggregation of count numbers by participants
my_data <- my_data %>% mutate(google_rel = google_harry+google_cats+google_swimmer)
my_data <- my_data %>% mutate(duck_rel = duck_harry+duck_cats+duck_swimmer)

# Plotting summed data, to check its distribution
ggplot(my_data, aes(x=google_rel))+geom_histogram(aes(y=..density..), colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")
ggplot(my_data, aes(x=duck_rel))+geom_histogram(aes(y=..density..), colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")

# Running Wilcox test as the samples are not normally distributed
wilcox.test(my_data$google_rel, my_data$duck_rel, paired = TRUE, alternative = "less")

# Checking results for search relevance
table(my_data$` Search results from Google were relevant.`)
table(my_data$` Search results from DuckDuckGo were relevant.`)

# Running Fisher's test due to low sample size
fisher.test( rbind( c(12,0,4,12), c(13,1,5,9)))

# Checking answers for search results quality
table(my_data$` Google gives duplicate results. (Repeated images)`)
table(my_data$` DuckDuckGo gives duplicate results.(Repeated images)`)
# Running Fisher's test
fisher.test( rbind( c(9,8,7,1,3), c(8,8,9,2,1) ))

# Checking response time answers
table(my_data$` Response time is important to me.`)

# Checking likeliness of using each search engine
table(my_data$` How likely are you going to use Google in the future? [.]`)
table(my_data$` How likely are you going to use DuckDuckGo in future? [.]`)
# Running test
fisher.test( rbind( c(10,8,1,8,0), c(5,5,5,10,1) ))