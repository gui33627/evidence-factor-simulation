"""
Parse Framingham data
"""

# imports
import pandas as pd
import numpy as np


def process_data():
    """
    Function to process the longitudinal data and combine rows corresponding
    to the first 2 time points of each individual
    """

    # read in the data
    orig_data = pd.read_csv("framingham_data.csv")
    
    # only keep individuals with at least one follow up
    # since it's a longitudinal study we'll collect
    # data on age, smoking, and blood pressure at time point 1, and the outcome diabetes at time point 2
    counts = orig_data["RANDID"].value_counts()
    num_no_follow_up = 0
    ids = {i: 0 for i in counts.index if counts[i] > 1}

    # info collected on each individual
    variables = ["age_cont_1", "smoke_bin_1", "smoke_cont_1",  "sex_bin_1", "bmi_cont_1", "prevchd_bin_1", "prevhyp_bin_1", "prediabetes_cont_1", "hyperten_bin_2", "diabetes_bin_2", "diabetes_cont_2", "bp_cont_2"]
    data = {v: [] for v in variables}


    # info collected at second time point
    for index, row in orig_data.iterrows():
        individual_id = row["RANDID"]
        if individual_id not in ids:
            continue

        # if encountering first time step
        if ids[individual_id] == 0:
            ids[individual_id] += 1
            data["age_cont_1"].append(row["AGE"]) # age          
            data["smoke_cont_1"].append(row["CIGPDAY"]) # cigs per day
            data["smoke_bin_1"].append(int(row["CURSMOKE"])) # smoke yes or no
            data["sex_bin_1"].append(row["SEX"]) # sex
            data["bmi_cont_1"].append(row["BMI"]) # bmi
            data["prevchd_bin_1"].append(row["PREVCHD"]) # prevalent coronary heart disease
            data["prevhyp_bin_1"].append(row["PREVHYP"]) # prevalent hypertensive
            data["prediabetes_cont_1"].append(row["GLUCOSE"])
            


        elif ids[individual_id] == 1:
            ids[individual_id] += 1
            data["hyperten_bin_2"].append(row["HYPERTEN"]) # hypertensive
            data["diabetes_cont_2"].append(row["GLUCOSE"])
            data["diabetes_bin_2"].append(int(row["DIABETES"]))
            data["bp_cont_2"].append(row["DIABP"]) # diastolic blood pressure


    # put all the variables together in a data frame
    data_processed = pd.DataFrame(data)

    # drop missing rows and return
    data_processed = data_processed.dropna()

    # create binary versions of variables
    # data_processed["age_bin"] = (data_processed["age_cont"] >= np.median(data_processed["age_cont"])).astype(int)
    # data_processed["bp_bin"] = (data_processed["bp_cont"] >= 90).astype(int)

    return data_processed


if __name__ == "__main__":

    data = process_data()
    print(data.columns)
    print(data)
    data.to_csv("framingham_data_out.csv", index=False)
