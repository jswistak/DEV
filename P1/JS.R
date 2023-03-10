library(ggplot2)


data <- read.csv("/Users/jakubswistak/Developer/Studia/DEV/P1/winequality-all.csv")

# a) investigating the distribution of alcohol variable 
a <- function(){
  ggplot(data, aes(x=alcohol)) + 
    geom_histogram(color="black", fill="red", bins=20) +
    labs(title = "Alcohol Content", x = "Alcohol %", y = "Count") +
    theme_bw()
}



# b) comparing the distribution of alcohol variable between two types of wine i.e. red and white 

b <- function(){
  ggplot(data, aes(x=color, y=alcohol)) +
    geom_boxplot(color="black", fill="gray") +
    labs(title = "Alcohol Content by Type", x = "Type", y = "Alcohol %") +
    theme_bw()
}


# c) comparing the distribution of alcohol variable in each of possible quality group defined by response variable

c_func <- function(){
  ggplot(data, aes(x=response, y=alcohol, group=response)) +
    geom_boxplot(color="black", fill="gray") +
    labs(title = "Alcohol Content by Quality", x = "Quality", y = "Alcohol %") +
    theme_bw()
}



# d) percentage of red and white wines within each quality group 

d <- function(){
  freq_table <- table(data$response, data$color) / nrow(data)
  df <- as.data.frame.table(freq_table)
  
  names(df) <- c("Quality", "Color", "Count")
  
  ggplot(df, aes(x=Quality, y=Count, fill=Color)) +
    geom_bar(stat="identity", color="black", position=position_dodge()) +
    labs(title = "Percentage of Red and White Wines by Quality", x = "Quality", y = "%") +
    scale_fill_manual(values = c("red", "white")) +
    theme_bw()
    
}


# freq_table <- table(data$response, data$color) / nrow(data)
# df <- as.data.frame.table(freq_table)
# names(df)
# colnames(df) <- c("name1", "name2", "name3")
# 
# 
# 
# 
# barplot <- ggplot(df, aes(x=Quality, y=Count, fill=Color)) +
#   geom_bar(stat="identity", color="black", position=position_dodge()) +
#   labs(title = "Percentage of Red and White Wines by Quality", x = "Quality", y = "%") +
#   scale_fill_manual(values = c("red", "white")) +
#   theme_bw()
# print(barplot)


# e) investigating the relationship between variables describing acidity of the wines

e <- function(){
  ggplot(data, aes(x=fixed.acidity, y=volatile.acidity)) +
    geom_point(color="blue") +
    labs(title = "Scatter Plot of Fixed Acidity vs. Volatile Acidity", x = "Fixed Acidity", y = "Volatile Acidity") +
    theme_bw()
}





