cps = read.csv("/Users/Ajay/Downloads/Chicago_Public_Schools_-_Progress_Report_Cards__2011-2012_.csv")
incomeTemp = read.csv("/Users/Ajay/Downloads/incomebycommunityarea.csv")
vectorOfVars = c("COMMUNITY.AREA.NAME", "PER.CAPITA.INCOME")
incomeByArea = incomeTemp[vectorOfVars]
colnames(incomeByArea)[1] = "Community.Area.Name"
cps$PER.CAPITA.INCOME = NA

for (i in 1:nrow(incomeByArea)){
  neighborhood = as.character(incomeByArea$Community.Area.Name[i])
  neighborhood = toupper(neighborhood)
  for (j in 1:nrow(cps)){
    if (neighborhood == as.character(cps$Community.Area.Name[j])){
      cps$PER.CAPITA.INCOME[j] = as.integer(incomeByArea$PER.CAPITA.INCOME[i])
    }
  }
}

cpsHighSchoolCollegeEnrollment = subset(cps, Elementary..Middle..or.High.School == "HS" & College.Enrollment.Rate.. != "NDA")
income = cpsHighSchoolCollegeEnrollment$PER.CAPITA.INCOME
collegeEnrollmentRate = as.numeric(as.character(cpsHighSchoolCollegeEnrollment$College.Enrollment.Rate..))
fit = lm(collegeEnrollmentRate ~ income)
summary(fit)
ggplot() + geom_point(aes(x = income, y = collegeEnrollmentRate)) + xlab("Income") + ylab("College Enrollment Rate")

cpsHighSchoolGradRate = subset(cps, Elementary..Middle..or.High.School == "HS" & Graduation.Rate.. != "NDA")
gradRate = as.numeric(as.character(cpsHighSchoolGradRate$Graduation.Rate..))
income = cpsHighSchoolGradRate$PER.CAPITA.INCOME
fitGradRate = lm(gradRate ~ income)
summary(fitGradRate)
ggplot() + geom_point(aes(x = income, y = gradRate)) + xlab("Income") + ylab("Grad Rate")

cpsHighSchoolInstructionScore = subset(cps, Elementary..Middle..or.High.School == "HS" & Instruction.Score != "NDA")
instructionscore = as.numeric(as.character(cpsHighSchoolInstructionScore$Instruction.Score))
income = cpsHighSchoolInstructionScore$PER.CAPITA.INCOME
fitInstructionScore = lm(instructionscore ~ income)
summary(fitInstructionScore)
ggplot() + geom_point(aes(x = income, y = instructionscore)) + xlab("Income") + ylab("Instruction Score")
 
cpsHighSchoolACT = subset(cps, Elementary..Middle..or.High.School == "HS" & X11th.Grade.Average.ACT..2011. != "NDA")
act = as.numeric(as.character(cpsHighSchoolInstructionScore$X11th.Grade.Average.ACT..2011.))
income = cpsHighSchoolInstructionScore$PER.CAPITA.INCOME
fitACT = lm(act ~ income)
summary(fitACT)
ggplot() + geom_point(aes(x = income, y = act)) + xlab("Income") + ylab("ACT score")

cpsHighSchoolInstructionScoreACT = subset(cps, Elementary..Middle..or.High.School == "HS" & Instruction.Score != "NDA" & X11th.Grade.Average.ACT..2011. != "NDA")
instructionscore = as.numeric(as.character(cpsHighSchoolInstructionScore$Instruction.Score))
act = as.numeric(as.character(cpsHighSchoolInstructionScore$X11th.Grade.Average.ACT..2011.))
fitInstructionScoreACT = lm(act ~ instructionscore)
summary(fitInstructionScoreACT)
ggplot() + geom_point(aes(x = instructionscore, y = act)) + xlab("Instruction Score") + ylab("ACT score")

ggplot(cps) + geom_point(aes(x = PER.CAPITA.INCOME, y = Safety.Score)) + xlab("Per Capita Income") + ylab("Safety Score")
fitSafety = lm(cps$Safety.Score ~ cps$PER.CAPITA.INCOME)
summary(fitSafety)

cpsHighSchoolActPlot = cpsHighSchoolACT
cpsHighSchoolActPlot$X11th.Grade.Average.ACT..2011. = as.integer(as.character(cpsHighSchoolActPlot$X11th.Grade.Average.ACT..2011.))
ggplot(cpsHighSchoolActPlot) + geom_bar(aes(x = X11th.Grade.Average.ACT..2011.)) + xlab("ACT score")
mean(cpsHighSchoolActPlot$X11th.Grade.Average.ACT..2011.)