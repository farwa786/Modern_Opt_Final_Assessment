# Load the dataset
df <- read.csv("E:/farwa_project/diabetes2.csv")

print(head(df))
print(str(df))
print(summary(df))
df$Outcome <- as.factor(df$Outcome)
#check for any missing values 
print(any(is.na(df)))
print(any(is.null(df)))



# Save the cleaned dataset
write.csv(df, "E:/farwa_project/clean_data.csv", row.names = FALSE)
