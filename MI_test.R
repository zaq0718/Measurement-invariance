library(lavaan)
#1. The configural model
# a variance of an item is fixed as 1; an intercept of an item is fixed as 0; a mean and a variance of a factor is fixed as 0
# recommended by Millsap & Yun-Tein (2004)

configural_theta <- "
OS =~ q10 + q11 + q12 + q13 + q14 + q15
q10 | t1 + t2 +t3 +t4 
q11 | t1 + t2 +t3 +t4 
q12 | t1 + t2 +t3 +t4 
q13 | t1 + t2 +t3 +t4 
q14 | t1 + t2 +t3 +t4 
q15 | t1 + t2 +t3 +t4 
q10 ~~ c(1, 1,1)*q10
q11 ~~ c(1, 1,1)*q11
q12 ~~ c(1, 1,1)*q12
q13 ~~ c(1, 1,1)*q13
q14 ~~ c(1, 1,1)*q14
q15 ~~ c(1, 1,1)*q15
q10 ~ c(0, 0,0)*1
q11 ~ c(0, 0,0)*1
q12 ~ c(0, 0,0)*1
q13 ~ c(0, 0,0)*1
q14 ~ c(0, 0,0)*1
q15 ~ c(0, 0,0)*1
OS ~~ c(1,1,1)*OS
OS ~ c(0, 0,0)*1
"
m_conf_theta <- lavaan(configural_theta, 
                       data = All_C, group = "CNT", 
                       parameterization="theta", estimator="wlsmv",
                       auto.fix.first = FALSE)
summary(m_conf_theta,fit.measures=TRUE)# CFI:0.993;TLI:0.989;RMSEA=0.109;SRMR=0.046

# 2. The Threshold model
# All thresholds, means and variances of a factor were constrained across groups 
# Only constrained intercepts and residual variance for a reference group

Threshold_theta <- "
OS =~ q10 + q11 + q12 + q13 + q14 + q15
q10 | c(t1_1,t1_1,t1_1)*t1 + c(t2_1, t2_1,t2_1)*t2 +c(t3_1, t3_1,t3_1)*t3 +c(t4_1, t4_1,t4_1)*t4 
q11 | c(t1_2,t1_2,t1_2)*t1 + c(t2_2, t2_2,t2_2)*t2 +c(t3_2, t3_2,t3_2)*t3 +c(t4_2, t4_2,t4_2)*t4 
q12 | c(t1_3,t1_3,t1_3)*t1 + c(t2_3,t2_3,t2_3)*t2 +c(t3_3,t3_3,t3_3)*t3 +c(t4_3,t4_3,t4_3)*t4 
q13 | c(t1_4,t1_4,t1_4)*t1 + c(t2_4,t2_4,t2_4)*t2 +c(t3_4,t3_4,t3_4)*t3 +c(t4_4,t4_4,t4_4)*t4 
q14 | c(t1_5,t1_5,t1_5)*t1 + c(t2_5,t2_5,t2_5)*t2 +c(t3_5,t3_5,t3_5)*t3 +c(t4_5,t4_5,t4_5)*t4 
q15 | c(t1_6,t1_6,t1_6)*t1 + c(t2_6,t2_6,t2_6)*t2 +c(t3_6,t3_6,t3_6)*t3 +c(t4_6,t4_6,t4_6)*t4  
q10 ~~ c(1,NA,NA)*q10
q11 ~~ c(1,NA,NA)*q11
q12 ~~ c(1,NA,NA)*q12
q13 ~~ c(1,NA,NA)*q13
q14 ~~ c(1,NA,NA)*q14
q15 ~~ c(1,NA,NA)*q15
q10 ~ c(0,NA,NA)*1
q11 ~ c(0,NA,NA)*1
q12 ~ c(0,NA,NA)*1
q13 ~ c(0,NA,NA)*1
q14 ~ c(0,NA,NA)*1
q15 ~ c(0,NA,NA)*1
OS ~~ c(1,1,1)*OS
OS ~ c(0, 0,0)*1
"
m_Threshold_theta <- lavaan(Threshold_theta, 
                       data = All_C, group = "CNT", 
                       parameterization="theta", estimator="wlsmv",
                       auto.fix.first = FALSE)
summary(m_Threshold_theta,fit.measures=TRUE)# CFI:0.992;TLI:0.993;RMSEA=0.085;SRMR=0.046

#3. The Threshold and loading model
# All thresholds, loadings, means of a factor  were constrained across groups 
# Only constrained intercepts, residual variance, and a variance of a factor for a reference group

loadings_theta <- "
OS =~ c(l1,l1,l1)*q10 + c(l2,l2,l2)*q11 + c(l3,l3,l3)*q12 + c(l4,l4,l4)*q13 + c(l5,l5,l5)*q14 + c(l6,l6,l6)*q15
q10 | c(t1_1,t1_1,t1_1)*t1 + c(t2_1, t2_1,t2_1)*t2 +c(t3_1, t3_1,t3_1)*t3 +c(t4_1, t4_1,t4_1)*t4 
q11 | c(t1_2,t1_2,t1_2)*t1 + c(t2_2, t2_2,t2_2)*t2 +c(t3_2, t3_2,t3_2)*t3 +c(t4_2, t4_2,t4_2)*t4 
q12 | c(t1_3,t1_3,t1_3)*t1 + c(t2_3,t2_3,t2_3)*t2 +c(t3_3,t3_3,t3_3)*t3 +c(t4_3,t4_3,t4_3)*t4 
q13 | c(t1_4,t1_4,t1_4)*t1 + c(t2_4,t2_4,t2_4)*t2 +c(t3_4,t3_4,t3_4)*t3 +c(t4_4,t4_4,t4_4)*t4 
q14 | c(t1_5,t1_5,t1_5)*t1 + c(t2_5,t2_5,t2_5)*t2 +c(t3_5,t3_5,t3_5)*t3 +c(t4_5,t4_5,t4_5)*t4 
q15 | c(t1_6,t1_6,t1_6)*t1 + c(t2_6,t2_6,t2_6)*t2 +c(t3_6,t3_6,t3_6)*t3 +c(t4_6,t4_6,t4_6)*t4  
q10 ~~ c(1,NA,NA)*q10
q11 ~~ c(1,NA,NA)*q11
q12 ~~ c(1,NA,NA)*q12
q13 ~~ c(1,NA,NA)*q13
q14 ~~ c(1,NA,NA)*q14
q15 ~~ c(1,NA,NA)*q15
q10 ~ c(0,NA,NA)*1
q11 ~ c(0,NA,NA)*1
q12 ~ c(0,NA,NA)*1
q13 ~ c(0,NA,NA)*1
q14 ~ c(0,NA,NA)*1
q15 ~ c(0,NA,NA)*1
OS ~~ c(1,NA,NA)*OS
OS ~ c(0, 0,0)*1
"
m_loadings_theta <- lavaan(loadings_theta, 
                            data = All_C, group = "CNT", 
                            parameterization="theta", estimator="wlsmv",
                            auto.fix.first = FALSE)
summary(m_loadings_theta,fit.measures=TRUE)# CFI:0.992;TLI:0.994;RMSEA=0.079;SRMR=0.046



#4. The Threshold,loading ,and intercept model
# All thresholds, loadings, means of a factor, and intercepts=0  were constrained across groups 
# Only constrained intercepts, residual variance, and a variance of a factor for a reference group

intercepts_theta <- "
OS =~ c(l1,l1,l1)*q10 + c(l2,l2,l2)*q11 + c(l3,l3,l3)*q12 + c(l4,l4,l4)*q13 + c(l5,l5,l5)*q14 + c(l6,l6,l6)*q15
q10 | c(t1_1,t1_1,t1_1)*t1 + c(t2_1, t2_1,t2_1)*t2 +c(t3_1, t3_1,t3_1)*t3 +c(t4_1, t4_1,t4_1)*t4 
q11 | c(t1_2,t1_2,t1_2)*t1 + c(t2_2, t2_2,t2_2)*t2 +c(t3_2, t3_2,t3_2)*t3 +c(t4_2, t4_2,t4_2)*t4 
q12 | c(t1_3,t1_3,t1_3)*t1 + c(t2_3,t2_3,t2_3)*t2 +c(t3_3,t3_3,t3_3)*t3 +c(t4_3,t4_3,t4_3)*t4 
q13 | c(t1_4,t1_4,t1_4)*t1 + c(t2_4,t2_4,t2_4)*t2 +c(t3_4,t3_4,t3_4)*t3 +c(t4_4,t4_4,t4_4)*t4 
q14 | c(t1_5,t1_5,t1_5)*t1 + c(t2_5,t2_5,t2_5)*t2 +c(t3_5,t3_5,t3_5)*t3 +c(t4_5,t4_5,t4_5)*t4 
q15 | c(t1_6,t1_6,t1_6)*t1 + c(t2_6,t2_6,t2_6)*t2 +c(t3_6,t3_6,t3_6)*t3 +c(t4_6,t4_6,t4_6)*t4  
q10 ~~ c(1,NA,NA)*q10
q11 ~~ c(1,NA,NA)*q11
q12 ~~ c(1,NA,NA)*q12
q13 ~~ c(1,NA,NA)*q13
q14 ~~ c(1,NA,NA)*q14
q15 ~~ c(1,NA,NA)*q15
q10 ~ c(0,0,0)*1
q11 ~ c(0,0,0)*1
q12 ~ c(0,0,0)*1
q13 ~ c(0,0,0)*1
q14 ~ c(0,0,0)*1
q15 ~ c(0,0,0)*1
OS ~~ c(1,NA,NA)*OS
OS ~ c(0, NA,NA)*1
"
m_intercepts_theta <- lavaan(intercepts_theta, 
                           data = All_C, group = "CNT", 
                           parameterization="theta", estimator="wlsmv",
                           auto.fix.first = FALSE)
summary(m_intercepts_theta ,fit.measures=TRUE)# CFI:0.987;TLI:0.992;RMSEA=0.093;SRMR=0.047

#5. The Threshold,loading ,intercept and residuals model
# All thresholds, loadings, means of a factor, intercepts=0, and residuals =1 were constrained across groups 
# Only constrained intercepts and a variance of a factor for a reference group

residuals_theta <- "
OS =~ c(l1,l1,l1)*q10 + c(l2,l2,l2)*q11 + c(l3,l3,l3)*q12 + c(l4,l4,l4)*q13 + c(l5,l5,l5)*q14 + c(l6,l6,l6)*q15
q10 | c(t1_1,t1_1,t1_1)*t1 + c(t2_1, t2_1,t2_1)*t2 +c(t3_1, t3_1,t3_1)*t3 +c(t4_1, t4_1,t4_1)*t4 
q11 | c(t1_2,t1_2,t1_2)*t1 + c(t2_2, t2_2,t2_2)*t2 +c(t3_2, t3_2,t3_2)*t3 +c(t4_2, t4_2,t4_2)*t4 
q12 | c(t1_3,t1_3,t1_3)*t1 + c(t2_3,t2_3,t2_3)*t2 +c(t3_3,t3_3,t3_3)*t3 +c(t4_3,t4_3,t4_3)*t4 
q13 | c(t1_4,t1_4,t1_4)*t1 + c(t2_4,t2_4,t2_4)*t2 +c(t3_4,t3_4,t3_4)*t3 +c(t4_4,t4_4,t4_4)*t4 
q14 | c(t1_5,t1_5,t1_5)*t1 + c(t2_5,t2_5,t2_5)*t2 +c(t3_5,t3_5,t3_5)*t3 +c(t4_5,t4_5,t4_5)*t4 
q15 | c(t1_6,t1_6,t1_6)*t1 + c(t2_6,t2_6,t2_6)*t2 +c(t3_6,t3_6,t3_6)*t3 +c(t4_6,t4_6,t4_6)*t4  
q10 ~~ c(1,1,1)*q10
q11 ~~ c(1,1,1)*q11
q12 ~~ c(1,1,1)*q12
q13 ~~ c(1,1,1)*q13
q14 ~~ c(1,1,1)*q14
q15 ~~ c(1,1,1)*q15
q10 ~ c(0,0,0)*1
q11 ~ c(0,0,0)*1
q12 ~ c(0,0,0)*1
q13 ~ c(0,0,0)*1
q14 ~ c(0,0,0)*1
q15 ~ c(0,0,0)*1
OS ~~ c(1,NA,NA)*OS
OS ~ c(0, NA,NA)*1
"
m_residuals_theta <- lavaan(residuals_theta, 
                             data = All_C, group = "CNT", 
                             parameterization="theta", estimator="wlsmv",
                             auto.fix.first = FALSE)
summary(m_residuals_theta ,fit.measures=TRUE)

