# PCA-level-on-the-blood-test-
Factors which might affect the PCA level in the blood test 
Introduction:  
Prostate cancer is main cancer among men. The number of deaths was 20.7 per 100,000 men per year. These rates are age-adjusted and based on 2009-2013 cases and deaths. Lifetime Risk of Developing Cancer: Approximately 14.0 percent of men will be diagnosed with prostate cancer at some point during their lifetime[1]. Prostate cancer starts from the prostate gland, which is the gland that is situated underneath the bladder. Prostate cancer usually grows very slowly in some men that even if it is not treated will not threaten their lives. In some men, it grows very faster and distributes to other parts of the body. In this situation, it would be better to detect these cancers in their early stages and given with effective treatment. So, it has become the goal of using Prostate-specific anti-gen (PSA) testing to screen people with prostate cancer without any symptoms. Therefore, re-searchers are eager to discover which factors have a direct impact on PSA level. Though PSA test-ing has helped tremendously in identifying prostate cancer in the earlier stage and reduce the death rates in this regard.
The prostate-specific antigen is present in the blood released by the prostate gland. The PSA levels are observed to be at high levels than normal levels in men with prostate cancer, with benign pros-tatic hyperplasia and inflammation of the prostate.
PSA is commonly used as detecting prostate cancer. However, to design an efficient screening tool it is important that we understand how PSA levels relate to factors that may determine prognosis and outcome. Although this study has its limitation, for instance, a number of predictor variable and the total number of response and data set. 
Usually, most doctors considered PSA levels of 4.0 ng/mL and lower as normal. Therefore, if a man had a PSA level above 4.0 ng/mL, doctors would often recommend a prostate biopsy to determine whether prostate cancer was present. Also, a high PSA level of ≥ 4 ng/ml automatically doesn’t mean a patient has cancer. Different factors can cause PSA elevation, such as benign enlargement of the prostate (BPH), a urinary tract infection or a prostate infection. For example, a large tumor may invade surrounding tissue and penetrate the wall of the prostate Also, benign hyperplasia is associat-ed with higher PSA levels, but is non-cancerous. 

Questions: 
1.    What’s the most important factor that has the greatest influence on PSA level and if that varia-ble increase the PSA increase dramatically and is a good indicator?
Yes, when the cancer volume is high then it has the greatest effect on PSA level. These fac-tors will help the physician to diagnose and evaluate the patient more accurately and in a timely manner, also it will decrease the cost of treatment. Therefore, based on these study and data analysis the cancer volume, seminal vesicle invasion, benign prostatic hyperplasia, and Gleason score have the highest positive correlation with PSA level.
2.    Does age (after 40) playing a critical role in prostate cancer and has the greatest impact on PSA level?
The discriminating ability of PSA will be enhanced by the age-specific ranges of PSA levels as PSA increases with age. cor(Y,X3) [1] 0.1606458 . Even though we have high collinearity in our data set but still there is no strong correlation between PSA level and age. Moreover, by analyzing the graph, there is no strong correlation between age and PSA level. And also age is not one of the final variables in our best sub-set model. Therefore, patients with a high risk of prostate cancer should always do regular checkup after 40, as most physicians recommend. 
3.    Is there any direct high correlation between cancer volume and PSA level?
This is the most critical step in diagnosing prostate cancer in an early stage because most pa-tients usually don’t have any symptom. Therefore, if a physician can use the certain level of PSA to predict a cancer tissue in patient prostate then they can perform an early biopsy to diagnose cancer in an early stage. 
4.    Which factors should be tested at first to determine the cancer level and decrease the cost of ex-amination by not testing too many factors which actually doesn’t play an important role? 
By conducting and having such a model we can decrease the cost and the time of diagnostic by eliminating other less important variables. Also, physicians will be able to evaluate the patients who have prostate cancer with higher accuracy and be able to diagnose cancer before higher stages in patients.  
5.    High level of cancer volume cause benign prostate hyperplasia?
The physician may use this correlation to analyze and predict any symptom that patient with cancer will develop when cancer cell tissue volume increase. 
However, based on analysis and graphs this correlation doesn’t seem to be held, so a high level of cancer volume does not always cause the benign prostate hyperplasia. 
Methods:
Multiple regression models have been established to analyses and find the best subset model and also find outliers and collinearity which existed in our data set. First, we dropped the patient ID which is not important in our data analysis. Then used check for normality and we used different techniques to normalize the Y value, for instance, using logY. Then we sketch the scatter plot, box plot of X variables and then recognize that we need to use a scale because values and the units for X variables are different. Using the different graph to detect the model distribution and normality was used to conclude our report accurately. The best sub-set method was used to calculate the best mod-el for this data set. We have an outlier in our data set, for instance, for weight variable; however weight variable is not one of the main variables that we construct by the end as the best subset mod-el. Different type of plot was constructed to analyze the data; for instance, box plot, scatter plot. The best subset method was used to construct the final model and different test was done to evaluate and confirm the best model by the end; for instance, Cp, R square, Ra square, AIC and BIC, Press. Also calculating cook distance and multicollinearity using VIF #variance inflation factors. 

Conclusion:
Although the scientific report has stated that multiple variables were used for creating risk models for prostate cancer, on this study that had been done by these variables only cancer vol-ume, benign prostate hyperplasia, and Gleason score are has a high direct impact on PSA level. Meta-analysis was done using these variables in creating several models that can detect clinically significant pros-tate cancer. 
    Therefore far 4 critical factors which have the greatest influence in PSA level were recognized base on our best subset analysis and different method to confirm our test that is available in appen-dix one. 
However, these models were decided to be sent for clinical efficiency before they are recommended to be used for concluding the detection and continue with biopsy. Therefore, the model that is best recommended to accompany PSA testing in order to detect prostate cancer could have these varia-bles to show better results. 
