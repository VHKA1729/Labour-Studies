library("FactoMineR")
library("factoextra")
library("data.table")
library("mltools")
 library("jaccard")
library("lsr")
data_c= read.csv("Downloads/Option1_HRM/cab.csv",stringsAsFactors = FALSE)[,-c(1,3,61)]## last column doesnt have any value
data_c=data_c[,!(names(data_c) %in% "Q31B_Pension")]
data_c$Q14_TrainingCommunication[data_c$Q14_TrainingCommunication =="999.0"]<- "missing"
data_c$Q14B_TrainingGrooming[data_c$Q14B_TrainingGrooming =="999.0"]<- "missing"
data_c$Q14C_TrainingSafety[data_c$Q14C_TrainingSafety =="999.0"]<- "missing"

colnames(data_c)[colSums(is.na(data_c)) > 0]
res.famd <- FAMD(data_c, graph = FALSE)
print(res.famd)


eig.val <- get_eigenvalue(res.famd)
head(eig.val)
fviz_screeplot(res.famd)

# Plot of variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)

quanti.var <- get_famd_var(res.famd, "quanti.var")
quanti.var

###
fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var = "black")
fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

########  
library("lsr")
library("caret")
## Q3, Q1
## Q20, Q21, Q22, Q23

data_c1= read.csv("Downloads/Option1_HRM/cab.csv")[,-c(1,3,61)]## 
encoded=one_hot(as.data.table(data_c1))
ncol(encoded)

##### Q3 vs Q20
#### checking different education levels vs opinions association

fisher.test(table(data_c$Q3_Education,data_c$Q20_Opinion_BestSegment)) # no association

## checking different education levels vs long term career plans
##### Q3 vs Q21
table1=table(data_c$Q3_Education,data_c$Q21_LongTerm_CareerPlan)
fisher.test(table1)        # There is no association b/t education and long term career plan
#### Q3 vs Q23
table2=table(data_c$Q3_Education,data_c$Q23_Monthly_Income)
fisher.test(table2)        # there is no assocaition b/t monthly income and education
#### Q3 vs Q22
table3=table(data_c$Q3_Education,data_c$Q22_Investment_PersonalDevelopment)
fisher.test(table3)        # there is no assocaition b/t PersonalDevelopment and education

#### Q1 vs Q20  couldn't test because samples are too small
table4=table(data_c$Q1_Age,data_c$Q20_Opinion_BestSegment)
fisher.test(table4)        # there is no assocaition b/t opinion and age
##### Q1 vs Q21
table5=table(data_c$Q1_Age,data_c$Q21_LongTerm_CareerPlan)
fisher.test(table5)  # there is no assocaition b/t longterm career and age

#### Q1 vs Q23
table6=table(data_c$Q1_Age,data_c$Q23_Monthly_Income)
fisher.test(table6)        # there is no assocaition b/t monthly income and age

#### Q1 vs Q22
table7=table(data_c$Q1_Age,data_c$Q22_Investment_PersonalDevelopment)
fisher.test(table7) # there is no assocaition b/t investment personal developmenty and age

################ odds ratio test ######
#### using fisher exact test becasue of lesser sample
## H0 if the people with graduation/ dploma holders choose to go for personal business in long term career
## H0: oddsratio=1 where odds of ppl  without graduation/diploma getting personal business = odds of people with graduation/diploma getting into personal business
### H1 : odds ratio<1 where odds of ppl without graduation/diploma a getting personal business < odds of people with graduation/diploma getting into personal business
table8 = table(encoded$`Q3_Education_Graudate/diploma`,encoded$`Q21_LongTerm_CareerPlan_Will switch to personal business`)

fisher.test(table8, alternative = "less")  # null hypothesis is not rejected 
## There is no difference between people with diploma/ graduation and lower education level towards starting personal business

## H0 if the people of age 20-25 choose to go for personal business in long term career
## H0: oddsratio=1 where odds of age group 25+ getting personal business = odds of people in age group 20-25 getting into personal business
### H1 : odds ratio<1 where where odds of age group 25+ getting personal business < odds of people in age group 20-25 getting into personal business
table9 = table(encoded$`Q1_Age_20-25`,encoded$`Q21_LongTerm_CareerPlan_Will switch to personal business`)

fisher.test(table9, alternative = "less") # null hypothesis is not rejected 
## There is no difference between people of youger age and other age group towards starting personal business

## H0 if the people of age 26-30 choose to go for personal business in long term career
## H0: oddsratio=1 where odds of age group 30+ getting personal business = odds of people in age group 26-30 getting into personal business
### H1 : odds ratio<1 where where odds of age group 30+ getting personal business < odds of people in age group 26-30 getting into personal business
table10 = table(encoded$`Q1_Age_26-30`,encoded$`Q21_LongTerm_CareerPlan_Will switch to personal business`)

fisher.test(table10, alternative = "less") # null hypothesis is not rejected 
## There is no difference between people of youger age and other age group towards starting personal business


################## test of independence #################################
table1=table(encoded$`Q3_Education_10th pass`,data_c1$Q20_Opinion_BestSegment)
fisher.test(table1)        # There is no association 10 th education level and best segment
table2=table(encoded$`Q3_Education_12th pass`,data_c1$Q20_Opinion_BestSegment)
fisher.test(table2)
table3=table(encoded$`Q3_Education_Graudate/diploma`,data_c1$Q20_Opinion_BestSegment)
fisher.test(table3)
table4=table(encoded$`Q3_Education_Stand. 1-5`,data_c1$Q20_Opinion_BestSegment)
fisher.test(table4) ## there is a strong association b/t 1-5 th std studied people and thier opions
table5 = table(encoded$`Q3_Education_Stand. 6-9`,data_c1$Q20_Opinion_BestSegment)
fisher.test(table5)


