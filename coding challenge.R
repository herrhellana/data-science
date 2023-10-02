setwd("/Users/asya/Dropbox/_ jobs/Lab @ DC")
recert_baseline <- read.csv("Recert_BaselinenewFall2022.csv")
recert_outcomes <- read.csv("Recert_OutcomesnewFall2022.csv")

head(recert_baseline)

### (Q6) 
# the closest service center to the most of the households 
# = the most frequent value of the service_center column
sort(table(recert_baseline$service_center), decreasing = TRUE)[1]


###################################################################
###################################################################


### (Q7)
# we need to merge two datasets by ID and retrieve the outcome column:
nrow(recert_baseline) # 3385 obs
nrow(recert_outcomes) # 3385 obs

data <- merge(recert_baseline, recert_outcomes, by = c("ic_case_id", "ic_case_id"))
nrow(data) # 3382 obs -- we lost three observations. Let's try to fix it. 

# let's see what are the different ID values: 
setdiff(recert_baseline$ic_case_id, recert_outcomes$ic_case_id)
setdiff(recert_outcomes$ic_case_id, recert_baseline$ic_case_id)

# it seems like the string values are identical with the only difference of 
# the second dataset including asterisks in the beginning of each string -- let's remove it:
gsub("\\*", "", setdiff(recert_outcomes$ic_case_id, recert_baseline$ic_case_id))

# now the values are identical:
intersect(setdiff(recert_baseline$ic_case_id, recert_outcomes$ic_case_id), 
          gsub("\\*", "", setdiff(recert_outcomes$ic_case_id, recert_baseline$ic_case_id)))

# commit the changes to the outcomes dataset:
asterisks <-  setdiff(recert_outcomes$ic_case_id, recert_baseline$ic_case_id)
no_asterisks <- gsub("\\*", "", setdiff(recert_outcomes$ic_case_id, recert_baseline$ic_case_id))
recert_outcomes[recert_outcomes$ic_case_id %in% asterisks, 1] <- no_asterisks

# merge corrected datasets
data <- merge(recert_baseline, recert_outcomes, by = c("ic_case_id", "ic_case_id"))
nrow(data) # 3385 obs, nice

# both conditions must be met simultaneously: 
nrow(data[(data$service_center == "H_Street") & (data$recertified == FALSE), ]) # 334


###################################################################
###################################################################

### (Q8): estimated effect of the treatment letter on recertification
# but we have two names for the Taylor_Street site:
unique(data$service_center)

# let's solve the issue:
data[data$service_center == "TaylorStreet", 4] <- "Taylor_Street"


# OLS
library(stargazer)
model1 <- lm(data=data, recertified ~ treatment + service_center) # service_center is already read as a factor
stargazer(model1, type = "text") # and Anacostia is automatically set as the reference site, i.e. the baseline category 

# => the treatment effect is 0.062

###################################################################
###################################################################

### (Q9): 
summary(model1)$coefficients[2] + 1.96*summary(model1)$coefficients[2, 2] # 0.09784166
summary(model1)$coefficients[2] - 1.96*summary(model1)$coefficients[2, 2] # 0.0268181
# The 95% confidence interval is [0.0268181, 0.09784166]


###################################################################
###################################################################

### (Q10): 
model2 <- lm(data=data, recertified ~ treatment + service_center + age)
summary(model2)
# estimated effect of age = 0.0114324

### (Q11):  
summary(model2)$coefficients[7] + 1.96*summary(model2)$coefficients[7, 2] # 0.01262964
summary(model2)$coefficients[7] - 1.96*summary(model2)$coefficients[7, 2] # 0.01023526
# The 95% confidence interval is [0.01023526, 0.01262964]

### (Q12): compare R^2
summary(model2)$r.squared - summary(model1)$r.squared # 0.093587805 for age


### (Q13): 
# The average effect of one-unit change in treatment on recertification is 
# robust across two models (around 0.06) and significant on the 95% confidence level,
# i.e. getting a letter positively influences recertification. 
# The second model with a high R-square suggests that the variable age adds 
# information to the model, but it still explains only 9% of the sample variation 
# in recertification by treatment received. 
# Overall, both models have poor explanation power. 

# Moreover, significance of the correlation does not imply existence of a meaningful 
# link between two variables, it just shows that variables are linearly related. 
# The correlation can be spurious. The beta effect size is small, and 
# the calculated confidence intervals hover near zero, not including it though 
# (i.e. the coefficients are still statistically significant, but the effects are not substantial). 

