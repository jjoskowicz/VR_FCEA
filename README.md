# Assessment of Virtual Reality and Metaverse Applications in Higher Education
IEEE Latin America Transactions, submission ID: 10156 


![Graphic Abstract](/Graphic_Abstract_v5_HQ.png)


Authors:
* Jose Joskowicz (josej@fing.edu.uy)
* Fabricio Gonzalez (fabricio.g.antuna@gmail.com)
* Inés Urrestarazu (ines.urrestarazu@fcea.edu.uy)
* Lucía Tafernaberry (lucia.tafernaberry@fcea.edu.uy)


Data Sets:
* AcademicResults.xlsx   : results of academic knowledge tests
* AvatarsinVRGroup.xlsx  : QoE data for Avatars in VR Group
* InPersonGroup.xlsx     : QoE data for In Person Group
* RemoteAssistancetoVRGroup.xlsx   : QoE data for Remote assistance to VR Group
* VideoConferenceGroup.xlsx        : QoE data for Video Conference Group

Code:
* sum_aov.R


The sum_aov.R script analyzes academic performance data from an Excel file generating descriptive statistics and ANOVA/ANCOVA models. It computes an Incremental Score (IS) as the difference between final and initial diagnostic tests scores and provides descriptive statistics for IS and final exam scores. Also, it analyzes learning gains by experimental Group and Dimensionality of the experience (Dim) evaluating whether these factors have a statistically significant effect on academic performance. The analysis includes models with and without covariate adjustment (initial diagnostic test score) and repeats all tests for the full sample and for students who met class attendance requirements, checking standard assumptions (normality and homoscedasticity) for each model.




