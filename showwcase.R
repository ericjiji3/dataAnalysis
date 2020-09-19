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
OctoberActions = aggregate(data$session_actions, by=list(data$login_date), FUN=sum)
plot(OctoberActions$Group.1, OctoberActions$x,type = "o",main="Session Actions per Day in October",xlab="Date", ylab="Total Session Actions")

#stacked bar graph for active duration
data$activeDuration <- data$session_duration - data$inactive_duration
data$activeDuration <- ifelse(data$activeDuration < 0, 0, data$activeDuration)
userActiveDuration = aggregate(data$customer_id, by=list(data$activeDuration), FUN=sum)

install.packages("ggplot2")
library("ggplot2")
ggplot(data, aes(fill=session_duration, y=userActiveDuration$x, x=userActiveDuration$Group.1)) + geom_bar(position="stack", stat="identity")

