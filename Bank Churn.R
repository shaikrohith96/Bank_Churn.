# Cost sensitive learing for bank churn prediciton 
library(caret) #confusion matrix, splitting data
library(MASS) # logistic regression
library(e1071) # naive bayes
library(rpart) # dicesion tree
library(rpart.plot)
library(caTools)# KNN
library(class)#KNN
library(pROC)

library(tidyverse) 
library(forcats) 
library(psych) 
library(gridExtra) 
library(rlang)
library(gghalves)
library(ggrepel)
library(ggplot2)
#load data
bank_data_raw<-read.csv("BankChurners.csv")
names(bank_data_raw)

# Remove the unnecessary information 
bank_data_raw<-bank_data_raw %>% 
  select(-c(
    CLIENTNUM,
    Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1,
    Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2
  ))

# Gender
bank_data_raw$Gender<-ifelse(bank_data_raw$Gender=="M","Male","Female")
bank_data_raw$Attrition_Flag<-ifelse(bank_data_raw$Attrition_Flag == "Attrited Customer", 1,0)
head(bank_data_raw)

#charater to factor varaibles 
bank_data<-bank_data_raw
bank_data<-bank_data%>%
  mutate_if(is.character, as.factor)
summary(bank_data)
class(bank_data)
str(bank_data)

bank_data$Attrition_Flag<-as_factor(bank_data$Attrition_Flag)
bank_data$Gender<-as_factor(bank_data$Gender)

bank_data$Education_Level<-as_factor(bank_data$Education_Level)
bank_data$Education_Level<-fct_relevel(bank_data$Education_Level, "Unknown","Uneducated","High School","College","Graduate","Post-Graduate","Doctorate")

bank_data$Marital_Status<-as_factor(bank_data$Marital_Status)
bank_data$Marital_Status<-fct_relevel(bank_data$Marital_Status, "Unknown","Single",
  "Married","Divorced")
bank_data$Income_Category<-as_factor(bank_data$Income_Category)
bank_data$Income_Category<-fct_relevel(bank_data$Income_Category, "Unknown","Less than $40K","$40K-$60K","$60K-$80K","$80K-$120K","$120K +")

bank_data$Card_Category<-as_factor(bank_data$Card_Category)
bank_data$Card_Category<-fct_relevel(bank_data$Card_Category, "Blue", "Silver", "Gold", "Platinum")

cc<-summary(bank_data)
write.csv(cc, file = "summary.csv",row.names = TRUE)

#Exploratory Data Analysis
#Sociodemographic and service usage 

options(rep.plot.width=14, repr.plot.height=8)
gender_fill_scale<-scale_fill_manual(
  values = c("#4292c6", "#fb6a4a"),
  breaks = c("M","F"),
  labels = c("M","F")
)

card_fill_scale<-scale_fill_manual(
  values = c("#08519c","#bbbbbb","#d3a826","#f6f9fb"),
  breaks = c("Blue", "Silver","Gold","Platinum"),
  labels = c("Blue","Sliver","Gold","Platinum")
)

attrition_color_scale <- scale_color_manual(
  values = c("#7FC97F", "#FDC086"), 
  breaks = c(0, 1), 
  labels = c("0", "1")
)

#Histogram

get_histogram <- function(bank_data, var, binwidth = 30) {
  p <- bank_data %>%
    ggplot() +
    geom_histogram(aes(x = get(var)), color = "black", fill = "#f6f9fb", binwidth = binwidth) +
    geom_vline(aes(xintercept = summary(get(var))[4], linetype="mean"), size=1.3) +
    geom_vline(aes(xintercept = summary(get(var))[3], linetype="median"), size=1.3) +
    theme_classic() +
    labs(
      x = var,
      y = "frequency",
      title = paste(var, "distribution"),
      linetype = "measure"
    )
  
  return(p)
}

#Box Plot

get_boxplot <- function(bank_data, var) {
  bank_data %>%
    ggplot() +
    geom_boxplot(aes(x = get(var)), alpha = 0.7, color = "black", fill = "#f6f9fb") +
    theme_classic() +
    labs(
      x = var,
      title = paste(var, "distribution")
    )
}

#Barplot with absolute and percentage frequencies utility function 

get_var_freq_mixed <- function(data, var, palette) {
  
  p <- bank_data %>%
    count(get(var)) %>%
    mutate(pct = n / sum(n),
           pctlabel = paste0(n, " (", round(pct*100, 2), "%)")) %>%
    ggplot(aes(x = factor(`get(var)`), y = n, fill = factor(`get(var)`))) + 
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = pctlabel), vjust = -0.25, size = 4) +
    theme_classic() +
    labs(
      x = var,
      y = "Absolute frequency",
      fill = var,
      title = paste(var, "distribution")
    )
  
  if(hasArg(palette)) {
    p <- p + scale_fill_brewer(palette=palette)
  }
  
  return(p)
}

#sociodeograhpic 

summary(bank_data$Customer_Age)
sd(bank_data$Customer_Age)
options(rep.plot.width=20, repr.plot.height=14)
sociodeograhpic<- grid.arrange(
  nrow = 2,
  get_histogram(bank_data, var = ("Customer_Age"), binwidth = 1),
  get_var_freq_mixed(bank_data, var = "Gender") + gender_fill_scale,
  get_var_freq_mixed(bank_data, var = "Dependent_count", palette = "Blues"),
  get_var_freq_mixed(bank_data, var = "Education_Level", palette = "Reds"),
  get_var_freq_mixed(bank_data, var = "Marital_Status", palette = "Spectral"),
  get_var_freq_mixed(bank_data, var = "Income_Category", palette = "Greens")
)
ggsave("sociodeograhpic.png",plot= sociodeograhpic, width = 20, height = 14, dpi = 700)

#Service usage
summary(bank_data$Months_on_book)
sd(bank_data$Months_on_book)
service<-grid.arrange(
  ncol=3,
  (get_var_freq_mixed(bank_data, var = "Card_Category")+card_fill_scale),
  (get_histogram(bank_data, var = "Months_on_book", binwidth = 1)),
  (get_var_freq_mixed(bank_data, var = "Total_Relationship_Count", palette = "Blues")),
  (get_var_freq_mixed(bank_data, var = "Months_Inactive_12_mon", palette = "Reds")),
  (get_histogram(bank_data, var = "Months_Inactive_12_mon",binwidth = 1)),
  (get_var_freq_mixed(bank_data, var = "Contacts_Count_12_mon", palette = "Greens"))
)
ggsave("service.png",plot= service, width = 20, height = 14, dpi = 700)

#Credit
summary(bank_data$Credit_Limit)
IQR(bank_data$Credit_Limit)
credit<-grid.arrange(
  ncol=2,
  get_histogram(bank_data, var = "Credit_Limit", binwidth = 1000),
  get_boxplot(bank_data, var = "Credit_Limit")
)
ggsave("credit.png",plot= credit, width = 20, height = 14, dpi = 700)

#1st mode
bank_data %>%
  filter(Total_Revolving_Bal %in% c(0:50)) %>%
  count(Total_Revolving_Bal) %>%
  arrange(desc(n)) %>%
  head(1)

#2nd mode
bank_data %>%
  filter(Total_Revolving_Bal %in% c(2000:2517)) %>%
  count(Total_Revolving_Bal) %>%
  arrange(desc(n)) %>%
  head(1)

summary(bank_data$Total_Revolving_Bal)
IQR(bank_data$Total_Revolving_Bal)
trb<-grid.arrange(
  nrow=2,
  get_histogram(bank_data, var = "Total_Revolving_Bal",binwidth = 75),
  get_boxplot(bank_data, var = "Total_Revolving_Bal")
)
ggsave("trb.png",plot= trb, width = 20, height = 14, dpi = 700)

summary(bank_data$Avg_Open_To_Buy)
IQR(bank_data$Avg_Open_To_Buy)
get_histogram(bank_data, var = "Avg_Open_To_Buy",binwidth = 1000)
get_boxplot(bank_data, var = "Avg_Open_To_Buy")

summary(bank_data$Total_Amt_Chng_Q4_Q1)
IQR(bank_data$Total_Amt_Chng_Q4_Q1)
get_histogram(bank_data, var = "Total_Amt_Chng_Q4_Q1", binwidth = 1)
get_boxplot(bank_data, var = "Total_Amt_Chng_Q4_Q1")

summary(bank_data$Total_Trans_Amt)
IQR(bank_data$Total_Trans_Amt)
get_histogram(bank_data, var = "Total_Trans_Amt", binwidth = 500)
get_boxplot(bank_data, var = "Total_Trans_Amt")


summary(bank_data$Total_Trans_Ct)
ttc<-grid.arrange(
  get_histogram(bank_data, var = "Total_Trans_Ct",binwidth = 5),
  get_histogram(bank_data%>%filter(Total_Trans_Ct %in% c(30:90)),var = "Total_Trans_Ct",
                binwidth = 1)
)
ggsave("ttc.png",plot= ttc, width = 20, height = 14, dpi = 700)

summary(bank_data$Total_Ct_Chng_Q4_Q1)
IQR(bank_data$Total_Ct_Chng_Q4_Q1)
get_histogram(bank_data, var = "Total_Ct_Chng_Q4_Q1", binwidth = 1)
get_boxplot(bank_data,var = "Total_Ct_Chng_Q4_Q1")

summary(bank_data$Avg_Utilization_Ratio)
get_histogram(bank_data, var = "Avg_Utilization_Ratio", binwidth = 0.05)

#Conditional Analysis 

options(repr.plot.width = 20, repr.plot.height = 10)

# Percentage bar plot conditioned to Attrition_Flag helper function
get_var_freq_by_attrition <- function(bank_data, var, palette) {
  p <- bank_data %>%
    count(Attrition_Flag, get(var)) %>%
    group_by(Attrition_Flag) %>%
    mutate(pct = n / sum(n),
           pctlabel = paste0(round(pct*100, 2), "%")) %>%
    ggplot(aes(x = factor(`get(var)`), y = pct, fill = factor(`get(var)`))) + 
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = pctlabel), vjust = -0.20) +
    theme_classic() +
    facet_wrap(~ Attrition_Flag, nrow = 2) +
    labs(
      x = var,
      y = "Percentage frequency",
      fill = var,
      title = paste("Percentage frequencies of", var, "by Attrition_Flag")
    ) 
  
  if(hasArg(palette)) {
    p <- p + scale_fill_brewer(palette=palette)
  }
  
  return(p)
}

# Box plot conditioned to Attrition_Flag helper function
get_boxplot_by_attrition <- function(bank_data, var) {
  bank_data %>%
    ggplot() +
    geom_boxplot(aes(y = Attrition_Flag, x = get(var), fill = Attrition_Flag), 
    outlier.size = 0.5) +
    attrition_fill_scale +
    theme_classic() +
    labs(
      x = var,
      title = paste(var, "by Attrition_Flag")
    )
}

get_boxviolinplot_by_attrition <- function(bank_data, var) {
  p <- bank_data %>%
    ggplot() +
    geom_half_violin(aes(x = Attrition_Flag, y = get(var), fill = Attrition_Flag), alpha = 0.3, colour = "grey", side = "r", width = 1.3) +
    geom_half_boxplot(aes(x = Attrition_Flag, y = get(var)), width = 0.3, outlier.size = 0.) +
    attrition_fill_scale +
    attrition_color_scale +
    theme_classic() +
    coord_flip() +
    labs(
      y = var,
      title = paste(var, "by Attrition_Flag")
    )
  
  return(p)
}

#discrete variable

Dis_GDE<-grid.arrange(
  ncol = 3, 
  get_var_freq_by_attrition(bank_data, var = "Gender") + gender_fill_scale,
  get_var_freq_by_attrition(bank_data, var = "Dependent_count", palette = "Blues"),
  get_var_freq_by_attrition(bank_data, var = "Education_Level", palette = "Reds")
)
ggsave("Dis_GDE.png",plot= Dis_GDE, width = 20, height = 14, dpi = 700)

Dis_MIC<-grid.arrange(
  ncol = 3, 
  get_var_freq_by_attrition(bank_data, var = "Marital_Status", palette = "Spectral"),
  get_var_freq_by_attrition(bank_data, var = "Income_Category", palette = "Greens"),
  get_var_freq_by_attrition(bank_data, var = "Card_Category") + card_fill_scale
)
ggsave("Dis_MIC.png",plot= Dis_MIC, width = 20, height = 14, dpi = 700)

Dis_RIC<-grid.arrange(
  ncol = 3, 
  get_var_freq_by_attrition(bank_data, var = "Total_Relationship_Count", palette = "Blues"),
  get_var_freq_by_attrition(bank_data, var = "Months_Inactive_12_mon", palette = "Reds"),
  get_var_freq_by_attrition(bank_data, var = "Contacts_Count_12_mon", palette = "Blues")
)
ggsave("Dis_RIC.png",plot= Dis_RIC, width = 20, height = 14, dpi = 700)

#Continuous varaibles

options(repr.plot.width = 20, repr.plot.height = 10)
get_boxplot_attrition<- function(bank_data, var) {
  bank_data%>% 
    ggplot(aes(x= !!sym(var), y= Attrition_Flag,fill= Attrition_Flag))+
    geom_boxplot(outlier.size = 0.5)+
    labs(
      x = var,
      Y= "Percentage_Frequency",
      title=paste(var, "by Attrition_Flag")
      )+
      theme_classic()
}

Cont_cust<-grid.arrange(
  nrow = 3,
  ncol=4,
  get_boxplot_attrition(bank_data, "Customer_Age"),
  get_boxplot_attrition(bank_data, "Months_on_book"),
  get_boxplot_attrition(bank_data, "Credit_Limit"),
  get_boxplot_attrition(bank_data, "Total_Revolving_Bal"),
  get_boxplot_attrition(bank_data, "Avg_Open_To_Buy"),
  get_boxplot_attrition(bank_data, "Total_Amt_Chng_Q4_Q1"),
  get_boxplot_attrition(bank_data, "Total_Trans_Amt"),
  get_boxplot_attrition(bank_data, "Total_Trans_Ct"),
  get_boxplot_attrition(bank_data, "Total_Ct_Chng_Q4_Q1"),
  get_boxplot_attrition(bank_data, "Avg_Utilization_Ratio")
)
ggsave("Cont_cust.png",plot= Cont_cust, width = 20, height = 14, dpi = 700)

get_boxviolinplot_attrition <- function(bank_data, var){
  plot <- bank_data %>%
    ggplot() +
    geom_half_violin(aes(x = Attrition_Flag, y = !!sym(var), fill = Attrition_Flag), alpha = 0.3, side = "r", width = 1.3) +
    geom_half_boxplot(aes(x = Attrition_Flag, y = !!sym(var), fill = Attrition_Flag), width = 0.3, outlier.size = 0) +
    scale_fill_manual(values = c("#0859c9", "#bbbbbb")) +
    scale_color_manual(values = c("#0859c9", "#bbbbbb")) +
    theme_classic() +
    coord_flip() +
    labs(
      y = var,
      title = paste(var, "by Attrition_Flag")
    )
  return(plot)
}

Cont_S<-suppressWarnings(
  grid.arrange(
    nrow = 2,
    get_boxviolinplot_attrition(bank_data, "Customer_Age"),
    get_boxviolinplot_attrition(bank_data, "Months_on_book"),
    get_boxviolinplot_attrition(bank_data, "Credit_Limit"),
    get_boxviolinplot_attrition(bank_data, "Total_Revolving_Bal"),
    get_boxviolinplot_attrition(bank_data, "Avg_Open_To_Buy"),
    get_boxviolinplot_attrition(bank_data, "Total_Amt_Chng_Q4_Q1"),
    get_boxviolinplot_attrition(bank_data, "Total_Trans_Amt"),
    get_boxviolinplot_attrition(bank_data, "Total_Trans_Ct"),
    get_boxviolinplot_attrition(bank_data, "Total_Ct_Chng_Q4_Q1"),
    get_boxviolinplot_attrition(bank_data, "Avg_Utilization_Ratio")
  )
)
ggsave("Cont_S.png",plot= Cont_S, width = 20, height = 14, dpi = 700)

Cont_card<-grid.arrange(
  nrow = 2,
  get_boxplot_attrition(bank_data, "Total_Revolving_Bal"),
  get_boxplot_attrition(bank_data, "Total_Trans_Amt"),
  get_boxplot_attrition(bank_data, "Total_Trans_Ct"),
  get_boxplot_attrition(bank_data, "Total_Ct_Chng_Q4_Q1")
)
ggsave("Cont_card.png",plot= Cont_card, width = 20, height = 14, dpi = 700)

Cont_scard<-suppressWarnings(
  grid.arrange(
    nrow = 2,
    get_boxviolinplot_attrition(bank_data, "Total_Revolving_Bal"),
    get_boxviolinplot_attrition(bank_data, "Total_Trans_Amt"),
    get_boxviolinplot_attrition(bank_data, "Total_Trans_Ct"),
    get_boxviolinplot_attrition(bank_data, "Total_Ct_Chng_Q4_Q1")
  )
)
ggsave("Cont_scard.png",plot= Cont_scard, width = 20, height = 14, dpi = 700)


#predictive analysis

set.seed(123)
se1<-sample(1:nrow(bank_data),size = (nrow(bank_data)*75)/100, replace = FALSE)

#train - test split(75%-25%)
train<-bank_data[se1, ]
test<-bank_data[-se1, ]
table(bank_data$Attrition_Flag)
table(train$Attrition_Flag)
table(test$Attrition_Flag)

options(repr.plot.width = 14, repr.plot.height = 8)
get_var_freq_mixed(bank_data, "Attrition_Flag")
ggsave("Attrition_Flag.png")

#classification metrics helper fuction
classification_metrics<-function(confusion_matrix){
  metrics<-data.frame(matrix(0,nrow = 1,ncol = 5))
  metrics[1:4]<-confusion_matrix$byClass[c(2,6,5,7)]
  names(metrics)<-c("Specificity","Recall","Precision","F1","Accuracy")
  return(metrics)
}

#LR 
model1 = glm(formula=Attrition_Flag ~ . , family = binomial(link='logit'),data = train) %>% stepAIC()
summary(model1)

probabilities = predict(model1,newdata=test,type='response')

prob <- ifelse(probabilities >= 0.5, 1,0)
table(prob)

tab <- table(actual= test$Attrition_Flag,predicted=prob)
cm<-caret::confusionMatrix(tab,positive = "1")
cm$table

#Decision Tress
#Full Tree
set.seed(123)

#tree training
tree_0<-rpart(Attrition_Flag~., data = train, method = "class", cp=0.001)

#predict on test set
test_pred_tree_0<-predict(tree_0, newdata = test, type = "class")

#confusion matrix (Naive matrix)
conf_matrix_tree_0<-caret::confusionMatrix(test_pred_tree_0, test$Attrition_Flag, positive = "1", mode ="everything")
conf_matrix_tree_0$table

#get `classification 
(metrics_tree_O<-classification_metrics(conf_matrix_tree_0))

#another way 
metrics_tree_0 <- conf_matrix_tree_0$byClass
print(metrics_tree_0)

#Pruned Tree
options(repr.plot.width = 16, repr.plot.height = 18)
plotcp(tree_0)
ggsave("tree.png")

#tree pruned
tree_1<-prune(tree_0,cp=0.046)
test_pred_tree_1<-predict(tree_1,newdata=test,type="class")

conf_matrix_tree_1<-caret::confusionMatrix(test_pred_tree_1, test$Attrition_Flag, positive = "1", mode="everything")
conf_matrix_tree_1$table
(metric_tree_1<-classification_metrics(conf_matrix_tree_1))

metrics_tree_1 <- conf_matrix_tree_1$byClass
print(metrics_tree_1)

#prior Tree
set.seed(123)

#tree training with explicit class frequency 
tree_2<-rpart(Attrition_Flag~., data=train, method = "class", parms = list(prior=c(0.16,0.84)))
test_pred_tree_2<-predict(tree_2,newdata = test, type = "class")
conf_matrix_tree_2<-caret::confusionMatrix(test_pred_tree_2, reference=test$Attrition_Flag,positive="1")
conf_matrix_tree_2$table
(metric_tree_2<-classification_metrics(conf_matrix_tree_2))

metric_tree_2<-conf_matrix_tree_2$byClass
print(metric_tree_2)

#cost-sesitivity optimization 

#missclassification costs between 1 and 4 with step 0.1
costs_tree<-seq(1,4,0.1)

#optimization results
optimization_results_tree<-data.frame(matrix(0, nrow=length(costs_tree), ncol=6))
names(optimization_results_tree)<-c("Cost","Specificity","Recall","Precision","F1","Accuracy")

#create a tree for each i-th cost
for (i in seq_along(costs_tree)) {
  set.seed(123)
  
#i-th tree with i=th cost
  tree_opt<-rpart(Attrition_Flag~., data=train, method = "class",parms = list(loss=c(0,costs_tree[i],1,0)))
  
  tree_opt_pred<-predict(tree_opt, newdata = test, type="class")
  
  conf_matrix_tree_opt<-caret::confusionMatrix(tree_opt_pred, test$Attrition_Flag, positive = "1", mode= "everything")
  
  optimization_results_tree[i,1]<-costs_tree[i]
  optimization_results_tree[i,2:6]<-classification_metrics(conf_matrix_tree_opt)
}
optimization_results_tree
write.csv(optimization_results_tree,file = "optimization_results_tree.csv",row.names = TRUE)

optimization_results_tree_plot_data <- optimization_results_tree %>%
  pivot_longer(cols = c(-Cost), names_to = "metric", values_to = "value")

optimization_results_tree_plot_data %>%
  ggplot() +
  geom_line(aes(x = Cost, y = value, color = metric), linewidth = 1, alpha = 0.4) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  geom_text_repel(
    data = optimization_results_tree_plot_data %>% filter(Cost %in% c(1.0, 1.3, 1.6, 2.0, 2.7, "3.8")) %>% mutate(value = round(value,3)), 
    aes(x = Cost, y = value, label = value, color = metric),
    direction = "y",
    show.legend = F
  ) +  
  theme_classic() +
  scale_color_brewer(palette="Set1") +
  labs(
    x = "Cost",
    y = "Value",
    color = "Metric",
    title = "Metrics trends by varying the missclassification cost - classification tree"
  )
ggsave("Metrics_trend.png")

optimization_results_tree_indexes_f1 <- c(7,11,18,29)
optimization_results_tree_indexes_f1_selected <- c(4)

optimization_results_tree_results_pr <- optimization_results_tree
optimization_results_tree_results_pr <- optimization_results_tree_results_pr %>% 
  mutate(selected = if_else(row_number() %in% optimization_results_tree_indexes_f1_selected, 1, 0))

optimization_results_tree_results_pr %>%
  ggplot() +
  geom_point(aes(x = Recall, y = Precision, color = F1), size = 4) +
  geom_line(aes(x = Recall, y = Precision), size = 0.8, alpha = 0.15) +
  geom_label_repel(
    data = optimization_results_tree_results_pr %>% 
      filter(row_number() %in% c(optimization_results_tree_indexes_f1, optimization_results_tree_indexes_f1_selected)), 
    aes(
      x = Recall, 
      y = Precision, 
      label = paste0("c = ", Cost), 
      fill = factor(selected)
    ), 
    size = 4
  ) +
  theme_classic() +
  scale_colour_gradientn(colours = c("#ff0000","#ff9c00","#fffca6"),
                         values = c(1.0,0.8,0))  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_fill_manual(
    values = c("grey", "white"), 
    breaks = c(1, 0), 
    labels = c(1, 0)
  ) +
  labs(
    x = "Recall",
    y = "Precision",
    title = "Recall, Precision and F1 trends by varying the missclassification cost - classification tree"
  ) +
  guides(fill=FALSE)
ggsave("Recall vs Precision.png")

# Selected procedures
optimization_results_tree[c(4,11,18,29),]

set.seed(123)

# tree with cost 1.3
tree_opt_1 <- rpart(Attrition_Flag ~ ., data = train, method = "class", parms=list(loss=c(0,1.3,1,0)))
tree_opt_1_pred <- predict(tree_opt_1, newdata = test, type = "class") 
conf_matrix_tree_opt_1 <- caret::confusionMatrix(tree_opt_1_pred, test$Attrition_Flag, positive = "1", mode = "everything")
metrics_tree_opt_1 <- classification_metrics(conf_matrix_tree_opt_1)

# Pruned Tree
conf_matrix_tree_0$table
metrics_tree_0

# Tree with cost 1.3
conf_matrix_tree_opt_1$table
metrics_tree_opt_1

#Variables importance
options(repr.plot.width = 18, repr.plot.height = 8)

tree_opt_1_varimp <- data.frame(tree_opt_1$variable.importance)
tree_opt_1_varimp$variable <- rownames(tree_opt_1_varimp)
names(tree_opt_1_varimp) <- c("importance", "variable")

tree_opt_1_varimp %>%
  ggplot(aes(x = importance, y = reorder(variable, importance))) +
  geom_bar(aes(fill = importance), stat = "identity", color = "black") +
  geom_text(aes(label = round(importance, 3)), vjust = 0.20, hjust = -0.20) +
  scale_fill_gradient(low = "#c7e9c0", high = "#00441b") +
  theme_classic() +
  labs(
    title = "Variables importance - Classification tree",
    x = "Importance",
    y = "Variable",
    fill = "Importance"
  ) + guides(fill = FALSE)
ggsave("Classification.png")

#Decision Tree Summary
comparison_tree_opt <- data.frame(matrix(nrow = 7, ncol = 6))
names(comparison_tree_opt) <- c("Cost", "Specificity", "Recall", "Precision", "F1", "Accuracy")

comparison_tree_opt[1,1] <- "Full Tree"
comparison_tree_opt[1,2:6] <- metrics_tree_0

comparison_tree_opt[2,1] <- "Pruned Tree"
comparison_tree_opt[2,2:6] <- metrics_tree_1

# Serialize the tree_1 model and save it to a file
#saveRDS(tree_1, "tree_1_model.rds")

# Assign "Pruned Tree" to the cell in comparison_tree_opt
#comparison_tree_opt[2, 1] <- "Pruned Tree"

# Load the model back when needed
#loaded_tree_1 <- readRDS("tree_1_model.rds")

# Assign the loaded model to the cell in comparison_tree_opt
#comparison_tree_opt[2, 2] <- loaded_tree_1


comparison_tree_opt[3,1] <- "Prior Tree"
comparison_tree_opt[3,2:6] <- metric_tree_2

comparison_tree_opt[4,] <- optimization_results_tree[4, ]
comparison_tree_opt[5,] <- optimization_results_tree[11, ]
comparison_tree_opt[6,] <- optimization_results_tree[18, ]
comparison_tree_opt[7,] <- optimization_results_tree[29, ]

comparison_tree_opt
write.csv(comparison_tree_opt,file = "comparison1.csv", row.names = TRUE, col.names = TRUE)
dev.off
