# %%
import numpy as np
import optuna
import pandas as pd
from catboost import CatBoostClassifier, Pool
from sklearn.experimental import enable_iterative_imputer  # noqa
from sklearn.impute import IterativeImputer, SimpleImputer
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split

# %%
train = pd.read_csv("train.csv", sep=",")
test = pd.read_csv("test.csv", sep=",")


# %% [markdown]
# # Data Exploration
#

# %% [markdown]
# ## Basic
#

# %%
# Columns VIP, CryoSleep have type object, convert to bool
train[["VIP", "CryoSleep"]] = train[["VIP", "CryoSleep"]].astype(bool)
test[["VIP", "CryoSleep"]] = test[["VIP", "CryoSleep"]].astype(bool)


# %%

# %%
# All columns besides age have a really uneven distribution,
# we will take the log1p of the value for these columns
numerical_cols = train.select_dtypes(include="number").columns
for col in numerical_cols:
    if col != "Age":
        train[col] = np.log1p(train[col])
        test[col] = np.log1p(test[col])


# %% [markdown]
# ## Feature Engineering

# %%
def add_features(df):
    # Add the number of people in the same cabin as the passenger
    df["CabinCount"] = df.groupby("Cabin")["PassengerId"].transform("count")
    df["IsAlone"] = (df["CabinCount"] == 1).astype(bool)

    # Add the number of people going to the same destination as the passenger
    df["DestinationCount"] = df.groupby("Destination")["PassengerId"].transform("count")

    # Bin age groups
    df["AgeBin"] = pd.cut(df.Age, bins=[0, 18, 35, 50, 80], include_lowest=True)
    df["Premium"] = df["RoomService"] + df["Spa"] + df["VRDeck"]
    df["Essential"] = df["FoodCourt"] + df["ShoppingMall"]

    return df


try:
    train = add_features(train)
    test = add_features(test)
except KeyError:
    print("Already added features")


# %% [markdown]
# ## Categorical Features transformations
#

# %%
def preprocess_passenger_id(df: pd.DataFrame, test_set=False):
    """
    Preprocess the passenger ID column by splitting it into two columns:
    - GroupID: the group ID of the passenger
    - PassID: the individual ID of the passenger within the group
    """
    df[["GroupID", "PassID"]] = df["PassengerId"].str.split("_", expand=True)
    df["GroupID"] = df["GroupID"].astype(int)
    df["PassID"] = df["PassID"].astype(int)
    if test_set:
        # set aside the ID for prediction later
        passenger_id = df["PassengerId"].copy()
    df.drop("PassengerId", axis=1, inplace=True)
    return (df, passenger_id) if test_set else (df, None)


def preprocess_cabin(df: pd.DataFrame):
    """
    Preprocess the cabin column by splitting it into three columns:
    - CabinDeck: the first letter of the cabin
    - CabinNum: the rest of the cabin
    - CabinSide: the side of cabin
    """
    df["Cabin"] = df["Cabin"].fillna("missing/-1/missing")
    df[["CabinDeck", "CabinNum", "CabinSide"]] = df["Cabin"].str.split("/", expand=True)
    df["CabinNum"] = df["CabinNum"].astype(int)
    # Bin Cabin Numbers
    df["CabinNumberBin"] = pd.cut(
        df.CabinNum, bins=[0, 270, 600, 1150, 1500, 2000], include_lowest=True
    )
    df.drop("Cabin", axis=1, inplace=True)
    return df


# %%
# ID column to be separated between before and after the "_"
try:
    train, _ = preprocess_passenger_id(train, test_set=False)
    test, passenger_id = preprocess_passenger_id(test, test_set=True)
except KeyError:
    print("ID already split")

# Cabin column to be split into 3, separated by "/" (deck/num/side)
# We replace NaN by missing/-1/missing to signal missing cabin information
try:
    train = preprocess_cabin(train)
    test = preprocess_cabin(test)
except KeyError:
    print("Cabin already split")

# Processing of the Name column: keep only last name ?
# Drop completely because same family is probably in the same cabin ?
# For now, 2nd option
try:
    train.drop("Name", axis=1, inplace=True)
    test.drop("Name", axis=1, inplace=True)
except KeyError:
    print("Name already dropped")


# %%
def impute_home_planet(df):
    """
    Replace NaN for HomePlanet by the value of the other people in the group
    if possible (takes max of the other values if multiple planets)
    """
    df_grp = (
        df.groupby(["GroupID", "HomePlanet"])["HomePlanet"].size().unstack().fillna(0)
    )
    ids_of_nan_in_group = df[df["HomePlanet"].isna()][
        (df[df["HomePlanet"].isna()]["GroupID"]).isin(df_grp.index)
    ].index

    df.loc[ids_of_nan_in_group, "HomePlanet"] = df.iloc[ids_of_nan_in_group, :][
        "GroupID"
    ].map(lambda x: df_grp.idxmax(axis=1)[x])

    return df


try:
    train = impute_home_planet(train)
    test = impute_home_planet(test)
except AttributeError:
    print("Already imputed")


# %%
# Transform categories to OrdinalEncoding
train[["AgeBin", "CabinNumberBin"]] = train[["AgeBin", "CabinNumberBin"]].apply(
    lambda x: x.cat.codes
)
test[["AgeBin", "CabinNumberBin"]] = test[["AgeBin", "CabinNumberBin"]].apply(
    lambda x: x.cat.codes
)

# %%
object_cols = train.select_dtypes(["object"]).columns

for col in object_cols:
    print(col, ":", train[col].nunique(), "unique values")
# few unique values: one Hot encoding is suitable


# %% [markdown]
# ## Train & Validation sets
#

# %%
X_train, X_valid, y_train, y_valid = train_test_split(
    train.drop("Transported", axis=1),
    train["Transported"],
    test_size=0.2,
)


# %% [markdown]
# ## Imputation
#

# %%
X_train_impute, X_valid_impute = X_train.copy(), X_valid.copy()
X_test_impute = test.copy()


# %%
# imputation of categorical columns with "missing" str
imputer = SimpleImputer(strategy="most_frequent")
X_train_impute[object_cols] = imputer.fit_transform(
    X_train_impute[object_cols]
    .to_numpy()
    .reshape(-1, X_train_impute[object_cols].shape[1])
)
X_valid_impute[object_cols] = imputer.transform(
    X_valid_impute[object_cols]
    .to_numpy()
    .reshape(-1, X_valid_impute[object_cols].shape[1])
)
X_test_impute[object_cols] = imputer.transform(
    X_test_impute[object_cols]
    .to_numpy()
    .reshape(-1, X_test_impute[object_cols].shape[1])
)

# Iterative imputation for numerical columns
numerical_cols = train.select_dtypes(include=["float64", "int32", "int8"]).columns
imputer = IterativeImputer(max_iter=100)
X_train_impute[numerical_cols] = imputer.fit_transform(
    X_train_impute[numerical_cols]
    .to_numpy()
    .reshape(-1, X_train_impute[numerical_cols].shape[1])
)
X_valid_impute[numerical_cols] = imputer.transform(
    X_valid_impute[numerical_cols]
    .to_numpy()
    .reshape(-1, X_valid_impute[numerical_cols].shape[1])
)
X_test_impute[numerical_cols] = imputer.transform(
    X_test_impute[numerical_cols]
    .to_numpy()
    .reshape(-1, X_test_impute[numerical_cols].shape[1])
)

# Recreate DataFrames
X_train_impute = pd.DataFrame(X_train_impute, columns=X_train.columns)
X_valid_impute = pd.DataFrame(X_valid_impute, columns=X_valid.columns)
X_test_impute = pd.DataFrame(X_test_impute, columns=test.columns)


# %%
# Children under 13 should not have paid anything
# Replace values of RoomService, FoodCourt, ShoppingMall, Spa, VRDeck by 0 for children under 13


def replace_children(df, threshold=13):
    df.loc[
        df["Age"] < threshold,
        ["RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck"],
    ] = 0
    return df


X_train_impute = replace_children(X_train_impute)
X_valid_impute = replace_children(X_valid_impute)
X_test_impute = replace_children(X_test_impute)

# Same for people in CryoSleep
def replace_cryo(df):
    df.loc[
        df["CryoSleep"] == True,
        ["RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck"],
    ] = 0
    return df


X_train_impute = replace_cryo(X_train_impute)
X_valid_impute = replace_cryo(X_valid_impute)
X_test_impute = replace_cryo(X_test_impute)

# %%
# Do not use OHE, but use pandas categories and let lightgbm handle them
def process_categorical(df):
    for c in df.columns:
        col_type = df[c].dtype
        if col_type == "object" or col_type.name == "category":
            df[c] = df[c].astype("category")

    return df


X_train_impute = process_categorical(X_train_impute)
X_valid_impute = process_categorical(X_valid_impute)
X_test_impute = process_categorical(X_test_impute)

# %%
# get categorical features
cat_features_indexes = lambda df: df.select_dtypes(
    include=["object", "category"]
).columns.tolist()

train_dataset_cat = Pool(
    X_train_impute, label=y_train, cat_features=cat_features_indexes(X_train)
)
valid_dataset_cat = Pool(
    X_valid_impute, label=y_valid, cat_features=cat_features_indexes(X_valid)
)
test_dataset_cat = Pool(X_test_impute, cat_features=cat_features_indexes(test))

full_train_dataset_cat = Pool(
    pd.concat([X_train_impute, X_valid_impute]),
    label=pd.concat([y_train, y_valid]),
    cat_features=cat_features_indexes(pd.concat([X_train_impute, X_valid_impute])),
)

# %%
def objective_cat(trial):
    param = {
        "loss_function": "Logloss",
        "eval_metric": "Accuracy",
        "iterations": trial.suggest_int("iterations", 500, 2000),
        "l2_leaf_reg": trial.suggest_float("l2_leaf_reg", 1e-2, 10.0, log=True),
        "bootstrap_type": trial.suggest_categorical(
            "bootstrap_type", ["Bayesian", "Bernoulli", "MVS"]
        ),
        "depth": trial.suggest_int("depth", 6, 16),
        "grow_policy": trial.suggest_categorical(
            "grow_policy", ["Depthwise", "Lossguide"]
        ),
        "one_hot_max_size": 5,
        "leaf_estimation_method": trial.suggest_categorical(
            "leaf_estimation_method", ["Newton", "Gradient"]
        ),
        "leaf_estimation_iterations": trial.suggest_int(
            "leaf_estimation_iterations", 1, 10
        ),
        # Unavailable on GPU:
        # "rsm": trial.suggest_float("rsm", 0.2, 1),
        # "model_shrink_mode": trial.suggest_categorical(
        #     "model_shrink_mode", ["Constant", "Decreasing"]
        # ),
        # "model_shrink_rate": trial.suggest_float("model_shrink_rate", 0.01, 0.5),
    }

    if param["grow_policy"] == "Depthwise":
        param["score_function"] = trial.suggest_categorical(
            "score_function", ["Cosine", "L2"]
        )
        param["min_child_samples"] = trial.suggest_int("min_child_samples", 10, 100)
        param["boosting_type"] = "Plain"

    elif param["grow_policy"] == "Lossguide":
        param["max_leaves"] = trial.suggest_int("max_leaves", 16, 64)
        param["score_function"] = "L2"
        param["min_child_samples"] = trial.suggest_int("min_child_samples", 10, 100)
        param["boosting_type"] = "Plain"

    # elif param["grow_policy"] == "SymmetricTree":
    #     param["boosting_type"] = trial.suggest_categorical(
    #         "boosting_type", ["Plain", "Ordered"]
    #     )
    #     if param["boosting_type"] == "Plain":
    #         param["score_function"] = trial.suggest_categorical(
    #             "score_function", ["Cosine", "L2"]
    #         )
    #     else:
    #         param["score_function"] = "Cosine"

    if param["bootstrap_type"] == "Bayesian":
        param["bagging_temperature"] = trial.suggest_float("bagging_temperature", 0, 10)
    elif param["bootstrap_type"] in ("Bernoulli", "MVS"):
        param["subsample"] = trial.suggest_float("subsample", 0.5, 1)

    gbm = CatBoostClassifier(**param, task_type="GPU")

    gbm.fit(
        train_dataset_cat,
        eval_set=valid_dataset_cat,
        verbose=0,
        early_stopping_rounds=100,
    )

    preds = gbm.predict(valid_dataset_cat) == "True"
    accuracy = accuracy_score(y_valid, preds)
    return accuracy


# %%
study_cat = optuna.create_study(direction="maximize")
study_cat.optimize(objective_cat, timeout=3600, n_jobs=11)

print("Number of finished trials: {}".format(len(study_cat.trials)))

print("Best trial:")
trial = study_cat.best_trial

print("  Value: {}".format(trial.value))

print("  Params: ")
for key, value in trial.params.items():
    print("    {}: {}".format(key, value))

# %%
best_model_catboost = CatBoostClassifier(
    **trial.params,
)

# train on full dataset
best_model_catboost.fit(
    full_train_dataset_cat,
    verbose=False,
)

# %%
# Write predictions on the test set to a new dataframe with PassengerId and the result of the model
test_pred = pd.DataFrame(passenger_id, columns=["PassengerId"])
test_pred["Transported"] = best_model_catboost.predict(test_dataset_cat)
test_pred.to_csv("test_pred_catboost_optuna_100iter.csv", index=False)
