#bar graph for session actions
data$session_actions <- data$session_comments_given + data$session_likes_given + data$session_projects_added
x = data$session_id
y = data$session_actions
sessionAction = barplot(y,x,main="Session Actions per User")
d[is.na(d)] <- 0
mean(data$session_actions)
sd(data$session_actions)

#line graph for total session actions per day
sessionActionss = aggregate(data$customer_id, by=list(data$session_actions), FUN=sum)
OctoberActions = aggregate(data$login_date, by=list(data$session_action), FUN=sum)
plot(OctoberActions$Group.1, OctoberActions$x,type = "o",main="Session Actions per Day in October",xlab="Date", ylab="Total Session Actions")

#stacked bar graph for active duration
data$activeDuration <- data$session_duration - data$inactive_duration
data$activeDuration <- ifelse(data$activeDuration < 0, 0, data$activeDuration)
userActiveDuration = aggregate(cbind(active= data$activeDuration,inactive = data$inactive_duration), by = list(Customer = data$customer_id), sum)
userActiveDuration$Customer <- 1:nrow(userActiveDuration) 
install.packages("ggplot2")
library("ggplot2")
activeChart = barplot(userActiveDuration$active,name = userActiveDuration$Customer,main="Active Duration per User",xlab = "Customers", xaxt = "n",ylab = "Active Duration(seconds)")
inactiveChart = barplot(userActiveDuration$inactive,name = userActiveDuration$Customer,main="Inactive Duration per User",xlab = "Customers", xaxt = "n",ylab = "Inactive Duration(seconds)")
activityChart = ggplot(userActiveDuration, aes(fill=inactive, y=active, x = Customer)) + 
  geom_bar(position = "stack",stat="identity")
activityChart + labs(y="Active Duration(seconds)", x = "Customer #")+ ggtitle("Active/Inactive duration per Customer")

#charts for user frequency
frequency = aggregate(data$login_date~data$customer_id, unique(data), FUN=list)
lengths(frequency$`data$login_date`, use.names = TRUE)
frequency$freq <- lengths(frequency$`data$login_date`, use.names = TRUE)
frequencyChart = barplot(height = frequency$freq,names = frequency$`data$customer_id`,main="Visit Frequency",xlab = "Customers", xaxt = "n",ylab = "Number of Unique Visits")
frequencyChart + theme(axis.title.x = element_blank())
mean(frequency$freq)
