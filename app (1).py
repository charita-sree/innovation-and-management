import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score
import numpy as np

# -------------------------------------------------
# PAGE CONFIG
# -------------------------------------------------
st.set_page_config(page_title="Walmart Sales Dashboard", layout="wide")
st.title("📊 Walmart Sales Prediction Dashboard")

# -------------------------------------------------
# LOAD DATA
# -------------------------------------------------
df = pd.read_csv("Walmart.csv")

# Convert Date
df['Date'] = pd.to_datetime(df['Date'], dayfirst=True)

# -------------------------------------------------
# FEATURE ENGINEERING (SAME AS JUPYTER)
# -------------------------------------------------

# Time Features
df['Year'] = df['Date'].dt.year
df['Month'] = df['Date'].dt.month
df['Week'] = df['Date'].dt.isocalendar().week
df['Quarter'] = df['Date'].dt.quarter

# Season Feature
def get_season(month):
    if month in [12,1,2]:
        return 1
    elif month in [3,4,5]:
        return 2
    elif month in [6,7,8]:
        return 3
    else:
        return 4

df['Season'] = df['Month'].apply(get_season)

# Fuel Price Change
df['Fuel_Price_Change'] = df['Fuel_Price'].diff()

# Store Performance Proxy
df['Store_Avg_Sales'] = df.groupby('Store')['Weekly_Sales'].transform('mean')

# Fill NaN
df.fillna(0, inplace=True)

# -------------------------------------------------
# SIDEBAR FILTERS
# -------------------------------------------------
st.sidebar.header("Filters")

selected_store = st.sidebar.selectbox(
    "Select Store",
    sorted(df['Store'].unique())
)

selected_month = st.sidebar.selectbox(
    "Select Month",
    sorted(df['Month'].unique())
)

filtered_df = df[
    (df['Store'] == selected_store) &
    (df['Month'] == selected_month)
]

st.subheader("Filtered Data")
st.dataframe(filtered_df.head())

# -------------------------------------------------
# SALES TREND
# -------------------------------------------------
st.subheader("Monthly Average Sales")

monthly_sales = df.groupby('Month')['Weekly_Sales'].mean()

fig, ax = plt.subplots()
monthly_sales.plot(ax=ax)
ax.set_xlabel("Month")
ax.set_ylabel("Average Weekly Sales")
st.pyplot(fig)

# -------------------------------------------------
# MODEL TRAINING
# -------------------------------------------------
X = df.drop(['Weekly_Sales', 'Date'], axis=1)
y = df['Weekly_Sales']

X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42
)

model = RandomForestRegressor(n_estimators=100, random_state=42)
model.fit(X_train, y_train)

y_pred = model.predict(X_test)
r2 = r2_score(y_test, y_pred)

st.subheader("Model Accuracy")
st.write(f"R² Score: {round(r2, 3)}")

# -------------------------------------------------
# PREDICTION SECTION
# -------------------------------------------------
st.subheader("Predict Weekly Sales")

store_input = st.number_input(
    "Store",
    min_value=int(df['Store'].min()),
    max_value=int(df['Store'].max())
)

holiday_input = st.selectbox("Holiday Flag (0 or 1)", [0, 1])

temp_input = st.number_input(
    "Temperature",
    value=float(df['Temperature'].mean())
)

fuel_input = st.number_input(
    "Fuel Price",
    value=float(df['Fuel_Price'].mean())
)

cpi_input = st.number_input(
    "CPI",
    value=float(df['CPI'].mean())
)

unemp_input = st.number_input(
    "Unemployment",
    value=float(df['Unemployment'].mean())
)

month_input = st.number_input("Month", min_value=1, max_value=12)
year_input = st.number_input("Year", value=2026)

# AUTO CALCULATED FEATURES
week_input = 1   # Default (since no exact date provided)
quarter_input = (month_input - 1) // 3 + 1
season_input = get_season(month_input)

fuel_price_change_input = 0   # Default (since future diff unknown)

store_avg_sales_input = df[df['Store'] == store_input]['Weekly_Sales'].mean()

if st.button("Predict Sales"):

    input_df = pd.DataFrame([{
        'Store': store_input,
        'Holiday_Flag': holiday_input,
        'Temperature': temp_input,
        'Fuel_Price': fuel_input,
        'CPI': cpi_input,
        'Unemployment': unemp_input,
        'Year': year_input,
        'Month': month_input,
        'Week': week_input,
        'Quarter': quarter_input,
        'Season': season_input,
        'Fuel_Price_Change': fuel_price_change_input,
        'Store_Avg_Sales': store_avg_sales_input
    }])

    prediction = model.predict(input_df)

    st.success(f"Predicted Weekly Sales: ₹ {prediction[0]:,.2f}")