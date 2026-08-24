# Doesn't run on raw TXT site files but on the resulting files produced by birdnet_analysis_thresholds.py
# Takes results from each site and converts species-specific detection data into table of overall average detections per day at each site
# Run code with cmd:   python3 birdnet_sites_datetimes.py your_results_folder


import sys
import os
import pandas as pd
import numpy as np

sites = []
avg_detections_per_day = []

folder = sys.argv[1]
site_type = folder[:folder.find("_")]

for file in os.listdir(folder):

    if file.startswith("."):   # ignore hidden files
        continue

    sites.append(file[:5])

    path = os.path.join(folder, file)
    site_results = pd.read_csv(path)

    # Removing non-rare species that didn't have any detections above thresholds (NA) and summing total detections for each species

    site_results = site_results.dropna()

    totals = []

    for idx in site_results.index.tolist():
        if site_results["Rare"][idx] == "no":
            total = site_results["Number of Instances, 60% Confidence"][idx] + \
                                                   site_results["Number of Instances, 65% Confidence"][idx] + \
                                                   site_results["Number of Instances, 70% Confidence"][idx] + \
                                                   site_results["Number of Instances, 75% Confidence"][idx] + \
                                                   site_results["Number of Instances, 80% Confidence"][idx] + \
                                                   site_results["Number of Instances, 85% Confidence"][idx] + \
                                                   site_results["Number of Instances, 90% Confidence"][idx] + \
                                                   site_results["Number of Instances, 95% Confidence"][idx]
            totals.append(total)

    site_results["Total Instances"] = totals
    smalldf = site_results.loc[:, ["Common Name", "Total Instances", "List of Datetimes"]]

    # Creating smaller table with parsed date info
    # Regardless of species, calculating how many detections each day at current site
    # Averaging the number of detections per day at current site and adding to overall DPD list

    datelist = []
    countlist = []

    for idx in smalldf.index.tolist():
        datetimes = smalldf["List of Datetimes"][idx][2:-2]
        split_datetimes = datetimes.split("], [")

        dates = []
        counts = []

        for dt in split_datetimes:
            split_dt = dt.split(", ")
            dt = split_dt[0]
            date = dt[1:9]
            count = int(split_dt[1])
            dates.append(date)
            counts.append(count)

        datelist.extend(dates)
        countlist.extend(counts)

    dtdfdata = {"Dates": datelist, "Counts": countlist}
    dtdf = pd.DataFrame(data=dtdfdata)

    dtdf_unique = dtdf.groupby(["Dates"]).agg("sum")

    avg_detections_per_day.append(np.mean(dtdf_unique["Counts"].tolist()))

    print(f"Done processing site {file[:5]}!")

# Creating table with site numbers in one column and average detections per day at each site in second column

overalldata = {"Site": sites, "Average_Detections_Per_Day": avg_detections_per_day}
overall_dpd = pd.DataFrame(data=overalldata)

filename = f"birdnet_avg_detections_per_day_{site_type}.csv"
overall_dpd.to_csv(filename, index=False)

print(f"Done running birdnet_sites_datetimes.py, and {filename} saved to current directory!")
