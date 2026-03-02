# Loops through folder containing species without thresholds (aka rare species) and extracts only positive detections (valid = 1)
# Rows from original files where valid = 1 are combined into single dataframe with additional columns for common name & site number

import sys
import os
import pandas as pd

folder = sys.argv[1]  # Finalized_Threshold_Tables/Species_Without_Thresholds

dflist = [] # List of dataframes, one for each file in folder

for file in os.listdir(folder):
    
    if file.startswith("."):   # ignore hidden files
        continue
    
    # Reading and parsing species file
    
    path = os.path.join(folder, file)
    
    with open(path) as f:
        txt = f.read()

    rowlist = []
    for line in txt.split('\n'):
        row = line.split('\t')
        if row[-1] == '':
            row[-1] = 2
        rowlist.append(row)

    # Extracting common name from file
    
    fname = file[:-4]
    name = fname.replace("_", " ")    # replacing underscores in file names with spaces
    name = name.replace("1", "'")    # replacing 1's in file names with apostrophes
    
    # Loading data into pandas dataframe, dropping null values and keeping rows where valid = 1

    df = pd.DataFrame(data=rowlist[1:], columns=rowlist[0]).dropna()
    df = df[df["Valid"] == "1"]
    
    # Adding common name to df and extracting site number, wav file from each row and adding to new columns
    
    df["Common Name"] = name
    df["Site Number"] = ""
    df["WAV File"] = ""
    for idx in df.index.tolist():
        beginfile = df["Begin File"][idx]
        pos = beginfile.find("2MM")
        wavpos = beginfile.find("0s_")
        site = beginfile[pos+3:pos+8]
        wavfile = beginfile[pos:wavpos]
        df.loc[idx, "Site Number"] = site
        df.loc[idx, "WAV File"] = wavfile
    
    dflist.append(df) # Adding df to list of dfs

# Combining all dfs into one single df and saving as csv to current directory

combined_df = pd.concat(dflist, ignore_index=True)
combined_df.to_csv("validated_rare_species.csv", index=False)