# Version of birdnetanalysis.py where bird observations in a given site are filtered according to previously calculated threshold values for each species.
# Run code with cmd:   python3 birdnet_analysis_thresholds.py your_folder your_results_folder

# Importing packages

import sys
import os
import pandas as pd

# Opening file containing bird species and their confidence score threshold values at 50%, 55%, 60%, 65%, 70%, 75%, 80%, 85%, 90%, and 95% confidence levels

threshold_table = pd.read_csv("threshold_table.csv")
for idx in threshold_table.index.tolist():
    species = threshold_table["Common.Name"][idx]
    species = species.replace("1", "'")
    if species == "Black-crowned Night Heron":
        species = "Black-crowned Night-Heron"
    threshold_table.loc[idx, "Common.Name"] = species

# Converting bird species and thresholds dataframe into a dictionary object that's easier to use for later

thresholds = {}
for i in range(0, len(threshold_table)):
    species = threshold_table["Common.Name"][i]
    species = species.replace("1", "'")

    t50 = float(threshold_table["t50"][i])   # convert strings to floats (characters to numbers)
    t55 = float(threshold_table["t55"][i])
    t60 = float(threshold_table["t60"][i])
    t65 = float(threshold_table["t65"][i])
    t70 = float(threshold_table["t70"][i])
    t75 = float(threshold_table["t75"][i])
    t80 = float(threshold_table["t80"][i])
    t85 = float(threshold_table["t85"][i])
    t90 = float(threshold_table["t90"][i])
    t95 = float(threshold_table["t95"][i])
    if t50 < 0.25:                           # if anything below 0.25, defaults to 0.25
        t50 = 0.25
    if t55 < 0.25:
        t55 = 0.25
    if t60 < 0.25:
        t60 = 0.25
    if t65 < 0.25:
        t65 = 0.25
    if t70 < 0.25:
        t70 = 0.25
    if t75 < 0.25:
        t75 = 0.25
    if t80 < 0.25:
        t80 = 0.25
    if t85 < 0.25:
        t85 = 0.25
    if t90 < 0.25:
        t90 = 0.25
    if t95 < 0.25:
        t95 = 0.25

    thresholds[species] = [t50, t55, t60, t65, t70, t75, t80, t85, t90, t95]

# Creating list of species that don't have thresholds by collecting species names from file names in relevant directory

rare_species = []
for f in os.listdir("Finalized_Threshold_Tables/Species_Without_Thresholds"):
    fname = f[:-4]
    name = fname.replace("_", " ")    # replacing underscores in file names with spaces
    name = name.replace("1", "'")    # replacing 1's in file names with apostrophes
    rare_species.append(name)

# Opening file containing positive (valid = 1) rare bird detections with info about common name and site number of detections

rare_species_table = pd.read_csv("validated_rare_species.csv")

# Looping through site files begins

folder = sys.argv[1]  # folder with sites to loop through

results_folder = sys.argv[2]  # folder to store code results in

for file in os.listdir(folder):

    if file.startswith("."):   # ignore hidden files
        continue

    # Reading site file and parsing it

    path = os.path.join(folder, file)
    with open(path) as f:
        txt = f.read()

    rowlist = []
    for line in txt.split('\n'):
        row = line.split('\t')
        if row[-1] == '':
            row[-1] = 2
        rowlist.append(row)

    # Loading data into pandas dataframe and dropping null values

    df = pd.DataFrame(data=rowlist[1:], columns=rowlist[0]).dropna()

    # Filtering data to only include species that currently have threshold values or are rare species

    idx_to_keep = []
    df["Rare"] = ""  # creating column to denote whether species is rare (placeholder value to begin with)
    for idx in df.index.tolist():
        species = df["Common Name"][idx]
        if species in threshold_table["Common.Name"].tolist():
            idx_to_keep.append(idx)
            df.loc[idx, "Rare"] = "no"
        elif species in rare_species:
            idx_to_keep.append(idx)
            df.loc[idx, "Rare"] = "yes"
    newdf = df.loc[idx_to_keep]

    # Calculating number of observations for each species where the confidence scores are >= t50, t55, t60, t65, t70, t75, t80, t85, t90, t95 of that species

    # Selecting one observation per species with highest confidence level (considering species with confidence score of at least 0.25)

    confdict = {} # confdict[species] = [conf, idx] want to show highest conf for each species
    instances = {} # instances[species] = [t50, t55, t60, t65, t70, t75, t80, t85, t90, t95] number of detections at each threshold
    rareinstances = {} # rareinstances[species] = count    // total (>= 0.25) detections for rare species only
    for idx in newdf.index.tolist():
        species = newdf["Common Name"][idx]
        rare = newdf["Rare"][idx]
        conf = float(newdf["Confidence"][idx])


        if rare == "no":   # following section only for species that have thresholds

            if species in confdict:
                if conf > confdict[species][0]:      # only if current conf greater than old conf
                    confdict[species] = [conf, idx]    # update so that current conf and current idx are saved
            else:
                if conf >= 0.25:                     # only add species w/ conf of 0.25 or greater
                    confdict[species] = [conf, idx]      # if not in dict, add species and conf/idx info fresh

            # t50 section

            if species in instances:
                if conf >= thresholds[species][0] and conf < thresholds[species][1]:  # only if species obs has conf equal/greater than species's t50 threshold and less than t55 threshold
                    instances[species][0] += 1         # update so that each such observation of species is counted
            else:
                if conf >= thresholds[species][0] and conf < thresholds[species][1]:  # same as above if statement, but applies when adding species for first time
                    instances[species] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]     # initialize empty list w/ placeholder values
                    instances[species][0] = 1          # add first detection

            # t55 section

            if species in instances:
                if conf >= thresholds[species][1] and conf < thresholds[species][2]:  # only if species obs has conf equal/greater than species's t55 threshold and less than t60 threshold
                    instances[species][1] += 1         # update so that each such observation of species is counted
            else:
                if conf >= thresholds[species][1] and conf < thresholds[species][2]:  # same as above if statement, but applies when adding species for first time
                    instances[species] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]     # initialize empty list w/ placeholder values
                    instances[species][1] = 1          # add first detection

            # t60 section

            if species in instances:
                if conf >= thresholds[species][2] and conf < thresholds[species][3]:  # only if species obs has conf equal/greater than species's t60 threshold and less than t65 threshold
                    instances[species][2] += 1         # update so that each such observation of species is counted
            else:
                if conf >= thresholds[species][2] and conf < thresholds[species][3]:  # same as above if statement, but applies when adding species for first time
                    instances[species] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]     # initialize empty list w/ placeholder values
                    instances[species][2] = 1          # add first detection

            # t65 section

            if species in instances:
                if conf >= thresholds[species][3] and conf < thresholds[species][4]:  # only if species obs has conf equal/greater than species's t65 threshold and less than t70 threshold
                    instances[species][3] += 1         # update so that each such observation of species is counted
            else:
                if conf >= thresholds[species][3] and conf < thresholds[species][4]:  # same as above if statement, but applies when adding species for first time
                    instances[species] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]     # initialize empty list w/ placeholder values
                    instances[species][3] = 1          # add first detection

            # t70 section

            if species in instances:
                if conf >= thresholds[species][4] and conf < thresholds[species][5]:  # only if species obs has conf equal/greater than species's t70 threshold and less than t75 threshold
                    instances[species][4] += 1         # update so that each such observation of species is counted
            else:
                if conf >= thresholds[species][4] and conf < thresholds[species][5]:  # same as above if statement, but applies when adding species for first time
                    instances[species] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]     # initialize empty list w/ placeholder values
                    instances[species][4] = 1          # add first detection

            # t75 section

            if species in instances:
                if conf >= thresholds[species][5] and conf < thresholds[species][6]:  # only if species obs has conf equal/greater than species's t75 threshold and less than t80 threshold
                    instances[species][5] += 1         # update so that each such observation of species is counted
            else:
                if conf >= thresholds[species][5] and conf < thresholds[species][6]:  # same as above if statement, but applies when adding obs for first time
                    instances[species] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]     # initialize empty list w/ placeholder values
                    instances[species][5] = 1          # add first detection

            # t80 section

            if species in instances:
                if conf >= thresholds[species][6] and conf < thresholds[species][7]:  # only if species obs has conf equal/greater than species's t80 threshold and less than t85 threshold
                    instances[species][6] += 1         # update so that each such observation of species is counted
            else:
                if conf >= thresholds[species][6] and conf < thresholds[species][7]:  # same as above if statement, but applies when adding species for first time
                    instances[species] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]     # initialize empty list w/ placeholder values
                    instances[species][6] = 1          # add first detection

            # t85 section

            if species in instances:
                if conf >= thresholds[species][7] and conf < thresholds[species][8]:  # only if species obs has conf equal/greater than species's t85 threshold and less than t90 threshold
                    instances[species][7] += 1         # update so that each such observation of species is counted
            else:
                if conf >= thresholds[species][7] and conf < thresholds[species][8]:  # same as above if statement, but applies when adding species for first time
                    instances[species] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]     # initialize empty list w/ placeholder values
                    instances[species][7] = 1          # add first detection

            # t90 section

            if species in instances:
                if conf >= thresholds[species][8] and conf < thresholds[species][9]:  # only if species obs has conf equal/greater than species's t90 threshold and less than t95 threshold
                    instances[species][8] += 1         # update so that each such observation of species is counted
            else:
                if conf >= thresholds[species][8] and conf < thresholds[species][9]:  # same as above if statement, but applies when adding species for first time
                    instances[species] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]     # initialize empty list w/ placeholder values
                    instances[species][8] = 1          # add first detection

            # t95 section

            if species in instances:
                if conf >= thresholds[species][9]:  # only if species obs has conf equal/greater than species's t95 threshold
                    instances[species][9] += 1         # update so that each such observation of species is counted
            else:
                if conf >= thresholds[species][9]:  # same as above if statement, but applies when adding species for first time
                    instances[species] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]     # initialize empty list w/ placeholder values
                    instances[species][9] = 1          # add first detection

        else: # only if species is rare, aka if rare == "yes"

            # determine WAV file slice from current site file detection
            file_path = newdf["Begin Path"][idx]
            file_offset = newdf["File Offset (s)"][idx]
            start_pos = file_path.find("2MM")
            wav_wo_offset = file_path[start_pos:-4]
            wav_file = wav_wo_offset + "_" + file_offset

            # check if current rare species detection found in validated rare species df
            # by matching site number, common name, and WAV file slice of detection with row in validated rs df

            filtered_rare_sp = rare_species_table[(rare_species_table["Site Number"] == int(file[:-9])) & \
                                    (rare_species_table["Common Name"] == species) & \
                                    (rare_species_table["WAV File"] == wav_file)]

            if len(filtered_rare_sp) == 1 or len(filtered_rare_sp) == 2: # if detection was found in validated rare species df

                if species in confdict:
                    if conf > confdict[species][0]:      # only if current conf greater than old conf
                        confdict[species] = [conf, idx]    # update so that current conf and current idx are saved
                else:
                    confdict[species] = [conf, idx]      # if not in dict, add species and conf/idx info fresh

                if species in rareinstances:
                    rareinstances[species] += 1  # add detection to count
                else:         # if not in dict, add species and count info fresh
                    rareinstances[species] = 1

    # Determining which rows to keep and which to discard from intermediate dataframe

    idx_to_keep = []
    for i in confdict:
        idx_to_keep.append(confdict[i][1])

    # Filtering dataframe, adding Instances column for each threshold value, adding Total Instances column for rare species and summing thresholds for non-rare species, and sorting by Common Name

    filtered_df = newdf.loc[idx_to_keep]

    totals = []
    for idx in filtered_df.index.tolist():
        species = filtered_df["Common Name"][idx]
        rare = filtered_df["Rare"][idx]
        if rare == "yes":
            totals.append(rareinstances[species])
        else:
            try: # for species with thresholds that have at least one observation passing lowest threshold
                filtered_df.loc[idx, "Number of Instances, 50% Confidence"] = instances[species][0]
                filtered_df.loc[idx, "Number of Instances, 55% Confidence"] = instances[species][1]
                filtered_df.loc[idx, "Number of Instances, 60% Confidence"] = instances[species][2]
                filtered_df.loc[idx, "Number of Instances, 65% Confidence"] = instances[species][3]
                filtered_df.loc[idx, "Number of Instances, 70% Confidence"] = instances[species][4]
                filtered_df.loc[idx, "Number of Instances, 75% Confidence"] = instances[species][5]
                filtered_df.loc[idx, "Number of Instances, 80% Confidence"] = instances[species][6]
                filtered_df.loc[idx, "Number of Instances, 85% Confidence"] = instances[species][7]
                filtered_df.loc[idx, "Number of Instances, 90% Confidence"] = instances[species][8]
                filtered_df.loc[idx, "Number of Instances, 95% Confidence"] = instances[species][9]

                totals.append("TBD") # will be calculated in R (simpler)
            except: # for species with thresholds that don't have even a single observation passing lowest threshold
                totals.append("NA") # species will be dropped anyways
    filtered_df["Total Instances"] = totals

    final_df = filtered_df.sort_values(by="Common Name")[["Common Name", "Confidence", \
                                                          "Number of Instances, 50% Confidence", \
                                                          "Number of Instances, 55% Confidence", \
                                                          "Number of Instances, 60% Confidence", \
                                                          "Number of Instances, 65% Confidence", \
                                                          "Number of Instances, 70% Confidence", \
                                                          "Number of Instances, 75% Confidence", \
                                                          "Number of Instances, 80% Confidence", \
                                                          "Number of Instances, 85% Confidence", \
                                                          "Number of Instances, 90% Confidence", \
                                                          "Number of Instances, 95% Confidence", \
                                                          "Total Instances", "Rare"]]

    # NA / NaN values in the Total Instances column in final dataframe means that species has thresholds and was detected but confidence scores of observations did not pass any threshold values
    # ie, all observations less than 50% confidence (so species will be removed from data table in Shiny)

    # Saving dataframe to csv file

    filename = file[:-4] + "_threshold_results.csv"
    results_path = os.path.join(results_folder, filename)
    final_df.to_csv(results_path, index=False)

    print(filename, f"has been saved to your {results_folder} directory!")

print("Done running birdnet_analysis_thresholds.py!")
