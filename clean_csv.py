import pandas as pd
import rpy2.robjects as ro
from rpy2.robjects.conversion import localconverter
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri
from datetime import date


def clean_data(path):
	df = pd.read_csv(path)
	species_name = df['scientificName'][1]
	df = df[['decimalLongitude', 'decimalLatitude', 'eventDate']]
	df.columns = ['Longitude', 'Latitude', 'after2000']
	df = df.dropna()
	df = df[pd.to_numeric(df['Longitude'], errors='coerce').notnull()]
	df = df[pd.to_numeric(df['Latitude'], errors='coerce').notnull()]
	df.insert(0, "Species", species_name) 
	df.insert(3, "Occur", "1")
	after_2000 = "2000-1-1"

	df['after2000'] = df['after2000'] >= after_2000
	df_hist = df.loc[df['after2000'] == False]
	df_curr = df.loc[df['after2000'] == True]
	df_hist.drop(columns = ['after2000'])
	df_curr.drop(columns = ['after2000'])

	save_path_hist = ("./data/{}_before2000.csv".format(species_name)).replace(" ", "_")
	save_path_curr = ("./data/{}_after2000.csv".format(species_name)).replace(" ", "_")
	#print(df)
	df_hist.to_csv(save_path_hist, index=0)
	df_curr.to_csv(save_path_curr, index=0)
	#print("Occurrence Count: {}".format(len(df)))
	#print("Cleaned Data Saved to: {}".format(save_path))

def evaluate_data(path):
	df = pd.read_csv(path)
	curr = len(df)
	print("Current Occurrences: {}".format(curr))

	df = df.loc[df['Aegilops_Triuncialis_current'] >= 0.65]
	pred = len(df)
	print("Correctly Predicted Occurrences: {}".format(pred))
	accuracy_rate = round(abs(((pred - curr)/curr)*100), 2)
	print("Percent Error: {}%".format(accuracy_rate))


def plotly_map(df):
	import plotly.graph_objects as go
	fig = go.Figure(go.Densitymapbox(lat=df.Latitude, lon=df.Longitude, radius=7))
	fig.update_layout(mapbox_style="stamen-terrain", mapbox_center_lon=-95, mapbox_center_lat=37)
	#mapbox_center = {"lat": 37.0902, "lon": -95.7129}
	fig.update_layout(margin={"r":0,"t":0,"l":0,"b":0})
	fig.update_geos(scope="usa")
	fig.show()

path = './data/Dreissena_polymorpha.csv'
eval_path = './results/Aegilops_Triuncialis_result.csv'
#clean_data(path)
evaluate_data(eval_path)
