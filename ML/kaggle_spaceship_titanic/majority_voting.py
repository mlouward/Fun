import pandas as pd
import os

df = pd.DataFrame()
# load each csv file into the df
for filename in os.listdir("./"):
    print(filename)
    if filename.startswith("test_"):
        tmp = pd.read_csv(filename, index_col=0)
        df = pd.concat([df, tmp], axis=1, ignore_index=True)

# create a new columns with the majority of each row
df["Transported"] = df.apply(lambda x: x.mode().iloc[0], axis=1)
print(df.head())
df.to_csv("majority_voting.csv", columns=["Transported"], index=True)
