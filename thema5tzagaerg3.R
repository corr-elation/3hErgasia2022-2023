#Includes functions for apriori algorithm
getwd(); graphics.off() ; rm(list = ls(all = TRUE)) ; cat("\014");
install.packages("arules")
library(arules)
# Set a working directory
setwd("C:\\Users\\zimou\\Downloads")

# Read the data
fertilitydata = read.csv("fertility_Diagnosis.txt", header=FALSE)

#V1: εποχή κατά την οποία πραγματοποιήθηκε η ανάλυση: χειμώνας(-1), άνοιξη(-0,33), καλοκαίρι(0,33), φθινόπωρο(1)
#V2: ηλικία κατά τη στιγμή της ανάλυσης : μεταξύ 18 και 36 ετών με κλίμακα [0, 1]
#V3: παιδικές ασθένειες (π.χ. ανεμοβλογιά, ιλαρά, παρωτίτιδα, πολιομυελίτιδα) : ναι (0), όχι (1)
#V4: ατύχημα ή σοβαρό τραύμα : ναι (0), όχι (1)
#V5: χειρουργική επέμβαση : ναι (0), όχι (1)
#V6: υψηλός πυρετός κατά το τελευταίο έτος : λιγότερο από τρεις μήνες πριν (-1), περισσότερο από τρεις μήνες πριν (0), όχι (1)
#V7: συχνότητα κατανάλωσης αλκοόλ : αρκετές φορές την ημέρα (0), κάθε μέρα (0,2), αρκετές φορές την εβδομάδα (0,4),
# μία φορά την εβδομάδα (0,6), σχεδόν ποτέ (0,8) ή ποτέ (1)
#V8: συνήθεια καπνίσματος ποτέ (-1), περιστασιακά (0) ή καθημερινά (1)
#V9: αριθμός ωρών καθισμάτων ανά ημέρα μεταξύ 1 και 16 με κλίμακα [0, 1].
#V10: η ετικέτα είναι "Ν" όταν το σπέρμα είναι φυσιολογικό και "Ο" αν είναι αλλοιωμένο

###QUESTION I
#remove from the dataset the 2nd and the 9th column.
fertilitydata=fertilitydata[,-2]
fertilitydata=fertilitydata[,-8]
attach(fertilitydata)

# Now looking at the data, some columns have as value '?' which basically signifies missing/non-existent data.
# We need to get rid of lines that have at least one such value in any column because we don't want
# to interpret such values as data.
#

#Add headers to data.
colnames(fertilitydata) <- c("Season","child_diseases","Accident_or_serious_trauma","Surgical_intervention","High_fevers","Frequency_of_alcohol_consumption","Smoking_habit","Diagnosis")

#Take a quick look at the data.
head(fertilitydata)

#για να εκτελεσουμε το apriori πρεπει να μετατρεψουμε τα data σε factor
#converting variables into factors and removing each time the old column
##in order to execute apriori 
fertilitydata$Season1<-factor(fertilitydata$Season)
fertilitydata=fertilitydata[,-1]
fertilitydata$child_diseases1<-factor(fertilitydata$child_diseases)
fertilitydata=fertilitydata[,-1]
fertilitydata$Accident_trauma1<-factor(fertilitydata$Accident_or_serious_trauma)
fertilitydata=fertilitydata[,-1]
fertilitydata$Surgical1<-factor(fertilitydata$Surgical_intervention)
fertilitydata=fertilitydata[,-1]
fertilitydata$High1<-factor(fertilitydata$High_fevers)
fertilitydata=fertilitydata[,-1]
fertilitydata$Frequency_alcohol_consumption1<-factor(fertilitydata$Frequency_of_alcohol_consumption)
fertilitydata=fertilitydata[,-1]
fertilitydata$Smoking_habit1<-factor(fertilitydata$Smoking_habit)
fertilitydata=fertilitydata[,-1]
str(fertilitydata)



# Execute initially the apriori algorithm WITHOUT any threshold for support or confidence. 
# This means that no minsup and minconf threshold is provided and 
# hanece that ALL POSSIBLE rules will be generated - WARNING: number of rules will be HUGE!
rules <- apriori(fertilitydata)
rules ##number of rules


###QUESTION 2
# Ok, lets execute apriori again but this time with some minimum threshold for support, confidence
# and some other parameters. 
# We execute apriori with the following thresholds: minimum support 2% (supp=0.02) and
# minimum confidence=100% (conf=1) 
# We also set parameters for the following: we want on the LHS (Left hand side - i.d. the antecedent) 
# to have  at least 2 items (minlen=2) and on the right-hand-side (RHS - i.d. consequent) only 
# values of the party variable i.e. we want only rules of the form 
#   {X,Y,...}->{Diagnosis} 
# should be shown only for altered(O)
# You may ofcourse specify different criteria
rules <- apriori(fertilitydata, parameter = list(minlen=2, supp=0.02, conf=1), appearance = list(rhs=c("Diagnosis=O"), default="lhs"))
rules##number of rules

# Lets see some of derived association rules. We display here only the first 10 rules.We do this because
# even with the parameters, the number of rules in really large.
# For each rule, it's support, confidence and lift are displayed.
# Again, note that there are many, many more but we opt to display ONLY
# the first 10 rules due to the sheer number of rules.
inspect( head(rules, 10) )

##return all 23 rules 
inspect( head(rules, 23) )

# You may also display individual rules using indexing as follows
# e.g. inspect rule at posotion 20
inspect( rules[20])



###QUESTION III
# You may also sort the rules based on lift in descending order
# Rules with higher lift will be displayed in higher position.
sortedRulesByLift <- sort (rules, by="lift", decreasing=TRUE)

# Display sorted rules from rules from highest to lowest lift.
# NOTE: we don't diplay all rules, just the 20 first. Why? Because
# number of rules is huge and we want to take a peek at the rules
# with the highest lift value.
inspect( head(sortedRulesByLift, 20) )


#find redunand rules
subset.matrix<-is.subset(sortedRulesByLift,sortedRulesByLift)
subset.matrix[lower.tri(subset.matrix,diag = T)]<-NA
redundant<-colSums(subset.matrix,na.rm=T)>=1
which(redundant)

#remove redundant rules
rules.pruned<- sortedRulesByLift[redundant]
inspect(rules.pruned)
##number of rules after remove redundant rules
rules.pruned

#visualize rules without redundant
#install.packages("arulesViz")
library(arulesViz)
plot(rules.pruned)
