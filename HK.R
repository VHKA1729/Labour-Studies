library("FactoMineR")
library("factoextra")
library("data.table")
library("mltools")
library("jaccard")
library("lsr")
data_c= read.csv("P:/CAL_Sem/HRA/PROJECT/DataHRM/Option1_HRM/HK.csv",stringsAsFactors = FALSE)[,-c(1)]## last column doesnt have any value
ncol(data_c)
data_c$Q12_Reason[data_c$Q12_Reason =="999.0"]<- "missing"
data_c$Q26_OtherEarning_Members[data_c$Q26_OtherEarning_Members =="999.0"]<- "missing"
data_c$Q32C_Attrition_Rate[data_c$Q32C_Attrition_Rate =="999.0"]<- "missing"
data_c$Q32E_Recommend_Friend[data_c$Q32E_Recommend_Friend =="999.0"]<- "missing"

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

data_c2= read.csv("P:/CAL_Sem/HRA/PROJECT/DataHRM/Option1_HRM/HK.csv",stringsAsFactors = FALSE)[,-c(1,9,16,17,18,19,21,26,30,43)]## last column doesnt have any value
data_c2
ncol(data_c2)
dummy <- dummyVars(" ~ .", data=data_c2)
newdata <- data.frame(predict(dummy, newdata = data_c2)) 
ncol(newdata)
head(newdata)


##### Q3 vs Q20
#### checking different education language spoken and type of employ

#fisher.test(table(data_c1$Q6_Marital_status,data_c1$Q9_Agency_Name_Type)) # no association
table10=table(data_c1$Q3_Education,data_c1$Q20_LongTerm_CareerPlan)
table10
fisher.test(table10)        # There is no association b/t education and long term career plan


##Different education levels vs long term career plans (Q3 vs Q20)
table1=table(data_c1$Q3_Education,data_c1$Q20_LongTerm_CareerPlan)
table1
fisher.test(table1)        # There is no association b/t education and long term career plan



####Education and monthly income (Q3 vs Q22)

table2=table(data_c1$Q3_Education,data_c1$Q22_Monthly_Income)
table2
fisher.test(table2)        # there is no assocaition b/t monthly income and education


#### Education and personal development (Q3 vs Q21)
table3=table(data_c$Q3_Education,data_c$Q21_Investment_PersonalDevelopment)
table3
fisher.test(table3)        # there is no assocaition b/t PersonalDevelopment and education

#### Age and long-term career plan (Q1 vs Q20)
table4=table(data_c$Q1_Age,data_c$Q16C_Comm._Skill)
fisher.test(table4)        # there is no assocaition b/t opinion and age



##Age and monthly income (Q1 vs Q20)
table5=table(data_c1$Q1_Age,data_c1$Q20_LongTerm_CareerPlan)
table5
fisher.test(table5)  # there is no assocaition b/t longterm career and age

#### Q1 vs Q22 
table6=table(data_c$Q1_Age,data_c$Q22_Monthly_Income)
table6
fisher.test(table6)        # there is no assocaition b/t monthly income and age



####  Personal development and age (Q1 vs Q21)
table7=table(data_c2$Q1_Age,data_c$Q21_Investment_PersonalDevelopment)
table7
fisher.test(table7) # there is no assocaition b/t investment personal developmenty and age


#### Marital status and Long-term career plan (Q5 vs Q20)
table8=table(data_c2$Q5_Marital_status,data_c$Q20_LongTerm_CareerPlan)
table8
fisher.test(table8) # there is no assocaition b/t Q5_Marital_status and Q20_LongTerm_CareerPlan


#### Long-term career plan and job satisfaction (Q20 vs Q31A)
table8=table(data_c2$Q31A_JobSatisfaction,data_c$Q20_LongTerm_CareerPlan)
table8
fisher.test(table8) # there is no assocaition b/t Q5_Marital_status and Q20_LongTerm_CareerPlan

