library(conjoint)
setwd("/Users/mac/Desktop/conjoint project")
####choice based conjoint analysis methodology aiming foor the quantify 
##the "utility" of various job attributer 
## for economic and finance students, to identify how much salary sacrifice
# for work- life balance or remote flexibility. 

############################################
#### STEP 1: Design matrix generation    ###
############################################


experiment <- expand.grid(
  Base_Salary = c("25 000€", "30 000€", "33 000€"),
  Work_Location = c("in office", "hybrid", "remote"),
  Annual_Bonus = c("0% of base salary", "10% of base salary", "25% of performance"),
  Company_Size = c("small company", "mid-company", "big company")
)


# View the first few rows
head(experiment)

### to generate a new design matrix (the result is not unique)
# This is more likely to respect the '12' limit
design <- caFactorialDesign(data = experiment, type = "orthogonal")

nrow(design)
# Should now show 9
### save the design on a file, to use the same data in all the steps of the analysis)
### Suggestion: comment this line after run, in order to not overwright the saved file
write.csv(design, file="csv/design.csv")

############################################
#### STEP 2: Data upload and analysis    ###
############################################

### Upload the saved  design matrix 
design<-read.csv("/Users/mac/Desktop/conjoint project/conjoint analysis/csv/design.csv")

prof<-caEncodedDesign(design[,-1])
colnames(prof)<-c("Base_Salary","Work_Location","Annual_Bonus","Company_Size")
prof

# Upload collected preference data - Rating (score) (caRankToScore for data conversion)
pref<-read.table("/Users/mac/Desktop/conjoint project/conjoint analysis/text rank pref/pref.txt", header=T)

# Partial utilities (individual and average) 
lev<-c("25 000€", "30 000€", "33 000€",
       "in office", "hybrid", "remote",
       "0% of base salary", "10% of base salary", "25% of performance",
       "small company", "mid-company", "big company")
ind<-caPartUtilities(t(pref),prof,lev)

aver <- apply(pref, 1, mean)
pref_aver <- cbind(pref, aver)

ind_aver<-caPartUtilities(t(pref_aver),prof,lev)
ind_aver
##intercept = mean rank

#write.csv(ind[33,-1],file="partial.csv")
##barplot(ind_aver[1:7,-1], main="Partial utilities", beside=T,
    #    cex.main=1, cex.names = 0.8,
   #     col=c(rep("grey",6),"red"))

#barplot(ind[1:6,-1], main="Partial utilities", beside=T,
     #   cex.main=1, cex.names = 0.6,
      #  col=c("red","green","orange","blue","pink","purple"))


#average importance of attributes

imp<-caImportance(pref, prof)
imp
names(imp)<-c("Base_Salary","Work_Location","Annual_Bonus","Company_Size")
barplot(imp, main = "Average importance of attributes", cex.main=1,
        cex.names = 0.8, 
        col = c("red", "purple", "orange","blue"))

# All results plots for whole sample
res<-Conjoint(pref, prof, lev, y.type="score")












# 1. Clear any previous plot settings to ensure a fresh start
dev.off() 

# 2. Manually input the utilities from your 'lm' summary (the 'utls' column)
# This ensures the graph and the mathematical truth are 100% aligned
model_utils <- c(0.0588, 0.0882, -0.1471,  # Base Salary
                 0.1961, 0.3431, -0.5392,  # Work Location
                 -0.0098, -0.2451, 0.2549,  # Annual Bonus
                 -0.0098, 0.3431, -0.3333)  # Company Size

# 3. Create a list for grouping
attr_groups <- list(
  Salary = model_utils[1:3],
  Location = model_utils[4:6],
  Bonus = model_utils[7:9],
  Company = model_utils[10:12]
)

# 4. Set up the Plot Environment (2x2 grid)
par(mfrow=c(2,2), mar=c(7, 4, 4, 2)) # Increased bottom margin for long labels

# 5. Loop through and plot with the "Highest Value" highlight
for(i in 1:4) {
  vals <- attr_groups[[i]]
  labs <- lev[((i-1)*3+1) : (i*3)]
  
  # Highlight the highest bar in green, others in grey
  colors <- ifelse(vals == max(vals), "darkolivegreen3", "lightgrey")
  
  barplot(vals, 
          names.arg = labs, 
          col = colors, 
          main = names(attr_groups)[i],
          ylab = "Utility Score", 
          las = 2,           # Rotate labels to be vertical
          cex.names = 0.7,   # Scale label size
          border = "black")
  
  # Add a reference line at zero
  abline(h = 0, lwd = 1.5)
}

# Reset the plot environment back to a single plot
par(mfrow=c(1,1))

