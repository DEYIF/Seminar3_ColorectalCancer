## Background: Colorectal Cancer

Colorectal cancer (CRC), one of the deadliest forms of cancer, is projected to increase in incidence over the coming decades, with a rise particularly among younger adults.

Early detection is critical for successful treatment, but it remains challenging due to difficulty distinguishing CRC symptoms from other diseases (e.g., gastrointestinal disease). CRC also has a high recurrence rate and often develops treatment resistance.

You are part of a team of clinical data modelers focusing on CRC, tasked with analyzing and interpreting clinical study data.


## Task 1: Detecting Colorectal Cancer (CRC)

A routine screening tool for CRC could help with early detection, potentially improving treatment outcomes. A study of **710 individuals** was conducted to develop a screening method for CRC based on primary care bio-analysis data.

- **Dataset**: data_task1.csv
  - The dataset includes a CRC classifier (dependent variable, 'DV') and various biomarkers.
- **Your Objectives**:
  - Develop a diagnostic model for CRC.
  - Evaluate the model’s performance.
  - Identify aspects that could inform a threshold value for the model.
  - Consider the practicality of implementing this model in a clinical setting.

### Table: Variable Explanations

| Variable | Unit      | Explanation                                       |
|----------|-----------|---------------------------------------------------|
| DV       |           | Dependent variable, CRC classifier (1: yes, 0: no)|
| ALT      | U/L       | Alanine transaminase                              |
| AST      | U/L       | Aspartate transaminase                            |
| GGT      | U/L       | -glutamyltransferase                              |
| TC       | mmol/L    | Total cholesterol                                 |
| TG       | mmol/L    | Triglycerides                                     |
| HDL      | mmol/L    | High-density lipoprotein                          |
| LDL      | mmol/L    | Low-density lipoprotein                           |
| CRP      | mg/L      | hs-CRP: high-sensitivity C-reactive protein       |
| APOA1    | g/L       | ApoA1: apolipoprotein A1                          |
| LPA      | g/L       | Lp(a): lipoprotein A                              |
| CEA      | ng/mL     | Carcinoembryonic antigen                          |
| WBC      | 10⁹/L     | White blood cells                                 |
| RBC      | 10¹²/L    | Red blood cells                                   |
| NEU      | 10⁹/L     | Neutrophils                                       |
| LYM      | 10⁹/L     | Lymphocytes                                       |
| MONO     | 10⁹/L     | Monocytes                                         |
| HGB      | g/L       | Haemoglobin                                       |
| PLT      | 10⁹/L     | Platelets                                         |


## Task 2: Colorectal Cancer Staging

The prevalence of CRC among younger adults is increasing. To investigate causes of late-stage CRC detection, a study of **200 individuals** examined factors influencing CRC stage at diagnosis.

- **Dataset**: data_task2.csv
- **Objectives**:
  - Analyze trends in CRC stage based on the data.
  - Recommend interventions based on findings.


## Task 3: Clinical Study on Drug Exposure

CRC is often treated with surgery followed by chemotherapy. Prior studies have shown that women may experience higher toxicity during chemotherapy.

A study of **45 participants** assessed the concentration-time profiles of the drug fluorouracil, given in a fixed intravenous bolus dose.

- **Dataset**: data_task3.csv
  - Contains concentration (DV; mg/L), observation time (Time; hours), dose (Dose; mg), body weight (BW; kg), sex (SEX; 0 - female and 1 - male), and age (AGE; years).
- **Objectives**:
  - Develop a model of the log-transformed concentration over time.
  - Identify factors affecting drug exposure.
  - Draw conclusions based on analysis.


## Task 4: Clinical Study of an Experimental Treatment vs. Control

This study investigates the impact of a novel treatment on overall survival compared to a control.

- **Dataset**: data_task4.csv
  - Survival data is indicated by the variable `status` (1: death) at a given time (`Time`; months) for control and treatment groups (`Group` variable).
- **Objective**: Analyze the efficacy of the experimental treatment compared to the control.
