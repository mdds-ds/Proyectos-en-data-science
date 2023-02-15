#!/usr/bin/env python
# coding: utf-8

# In[1]:


get_ipython().system('pip install pandas')
get_ipython().system('pip install spotipy')
get_ipython().system('pip install unidecode')
get_ipython().system('pip install seaborn')


# In[2]:


get_ipython().system('pip freeze')


# In[3]:


# Acknowledgements:
# https://medium.com/swlh/how-to-leverage-spotify-api-genius-lyrics-for-data-science-tasks-in-python-c36cdfb55cf3


# In[4]:


import pandas as pd
import re
import os
import spotipy
from spotipy.oauth2 import SpotifyClientCredentials
from bs4 import BeautifulSoup as BS
import requests
import time
from unidecode import unidecode #Will make it easier to deal with songs with non english alphabet characters
import seaborn as sns
import matplotlib.pyplot as plt
from collections import Counter
import sys


# In[5]:


cid = '***'
secret = '***'

client_credentials_manager = SpotifyClientCredentials(client_id=cid, client_secret=secret)
sp = spotipy.Spotify(client_credentials_manager = client_credentials_manager)


# In[6]:


# I'll get the track IDs from the "This is {artist}" playlist

def getTrackIDs(playlist_id):
    ids = []
    playlist = sp.playlist(playlist_id)
    for song in playlist['tracks']['items']:
        track = song['track']
        ids.append(track['id'])
    print('I could find {num_tracks} songs'.format(num_tracks=len(ids)))
    return ids


# In[7]:


# Next, I build a function to retrieve all the useful info for my dataset:
unwanted = "[',!@#$;:!*%)(&^~]"

def getTrackFeatures(item):
    metadata = sp.track(item)
    features = sp.audio_features(item)
    
    #metadata:
    name = unidecode(metadata['name'])
    name = re.sub(r'[\(\[].*?[\)\]]', '', name) #Delete regular expressions from name
    name = re.sub(unwanted,'', name) #Delete other undesired regular expressions
    sep = " - "
    name = name.split(sep, 1)[0] # Many songs have non useful info after a "-", for example: " - 2015 Remastered"
    
    album = unidecode(metadata['album']['name'])
    album_cover = metadata['album']['images'][0]['url']
    artist = unidecode(metadata['album']['artists'][0]['name'])
    release_date = metadata['album']['release_date']
    length = metadata['duration_ms']
    popularity = metadata['popularity']
    sample = metadata['preview_url']
    
    #audio analysis features:
    acousticness = features[0]['acousticness']
    danceability = features[0]['danceability']
    energy = features[0]['energy']
    instrumentalness = features[0]['instrumentalness']
    liveness = features[0]['liveness']
    loudness = features[0]['loudness']
    speechiness = features[0]['speechiness']
    tempo = features[0]['tempo']
    time_signature = features[0]['time_signature']
    
    track = [name,album,album_cover,artist,sample,release_date,length,popularity,acousticness,danceability,energy,instrumentalness,liveness,loudness,speechiness,tempo,time_signature]
    return track


# In[8]:


# Now, I add lyrics scraping from genius.com
def scrape_lyrics(artist,song):
    artistname = str(artist.replace(' ','-') if ' ' in artist else str(artist))
    
    #Building url
    songname = str(song.replace(' ','-') if ' ' in song else str(song)) # Useful for building the url
    
    page = requests.get('https://genius.com/'+ artistname + '-' + songname + '-' + 'lyrics',headers={'User-Agent': 'Mozilla/5.0'})
    #print('https://genius.com/'+ artistname + '-' + songname + '-' + 'lyrics')
    html = BS(page.text,'html.parser')
    #print(html)
    #time.sleep(3)
    for br in html.find_all("br"):
        br.replace_with("\n")
    lyrics = html.find_all('div', {"data-lyrics-container":"true","class":'Lyrics__Container-sc-1ynbvzw-6 YYrds'})
    lyrics_list = []
    for text in lyrics:
        #remove identifiers like chorus, verse, regular expressions, etc
        verse = text.get_text(separator=" ").strip()
        verse = re.sub(r'[\(\[].*?[\)\]]', '', verse)
        verse = re.sub(unwanted, '', verse)
        #print(verse)
        lyrics_list.append(verse)
    lyrics_text = " ".join(f"{row}" for row in lyrics_list)
    #print("done")
    return lyrics_text


# In[9]:


def lyrics_into_dataframe(df,artist):
    for i,song in enumerate(df['track']):
        lyrics = scrape_lyrics(artist,song)
        df.loc[i,'lyrics'] = lyrics
        #print(i)
    return df


# In[10]:


def normalize_lyrics(df):
    df["lyrics"] = df["lyrics"].apply(unidecode)
    df["lyrics"] = df["lyrics"].str.lower()
    return df


# In[11]:


class Tracks():
    ## INNER CLASS ##
      
    def __init__(self,data):
        self.name = data['track']
        self.album = data['album']
        self.artist = data['artist']
        self.sample = data['sample']
        self.tempo = data['tempo']
        self.lyrics = data['lyrics']


# In[12]:


@pd.api.extensions.register_dataframe_accessor("spotify")
# https://pandas.pydata.org/pandas-docs/stable/development/extending.html#subclassing-pandas-data-structures
class LyricsDataframe() :
    ## OUTER CLASS ##
    
    def __init__(self,data):
        self._data = data
        self.tracks = self._data.apply(Tracks,axis=1)
    def get_counter(self, column):
        return dict(Counter(self._data[column]))
    def get_stats(self):
        return self._data.describe()
    def get_columns(self):
        return list(self._data.columns)
    def get_hist(self, column, height, width):
        plt.figure(figsize=(height, width), dpi=80)
        sns.set()
        self._data[column].hist(bins=100)
        plt.title(f"{column} Histogram")
        plt.xlabel(f'{column}')
        plt.ylabel('Number of records')
        plt.show()
    def get_boxplot_of_categories(self, categorical_column, numerical_column, limit, height, width):
        keys = []
        for i in dict(Counter(self._data[categorical_column].values).most_common(limit)):
            keys.append(i)
        data_new = self._data[self._data[categorical_column].isin(keys)]
        plt.figure(figsize=(height, width), dpi=80)
        sns.set()
        sns.boxplot(x = data_new[categorical_column], y = data_new[numerical_column])
        plt.show()


# In[13]:


def retrieve_lyrics(artist):
    #artist = input() #Type name of the artist to search for "This is {artist}" playlist

    playlist = f"This is {artist}"
    results = sp.search(q='playlist:' + playlist, type='playlist') #Results of the query

    artist_search = sp.search(q='artist:' + artist, type='artist')
    artist = str(artist_search['artists']['items'][0]['name']) # Name of the artist (prevents typing errors)
    print(f"I'm searching for {artist}")

    uri_ti_artist = str(results['playlists']['items'][0]['uri']) # URI of "This is {artist}" playlist
    
    ids = getTrackIDs(uri_ti_artist)
    
    unwanted = "[',!@#$;:!*%)(&^~]"
    tracks = []
    for item in ids:
        track = getTrackFeatures(item)
        #time.sleep(3)
        tracks.append(track)
    
    df = pd.DataFrame(tracks, 
                  columns = ["track","album","album_cover","artist","sample","release_date","length","popularity","acousticness","danceability","energy","instrumentalness","liveness","loudness","speechiness","tempo","time_signature"])
    df = lyrics_into_dataframe(df,artist)
    df = normalize_lyrics(df)

    path=f"/home/matias_sarm/Escritorio/Proyectos-en-data-science/Spotify_lyrics_generator/generated_data/{artist}.csv"
    df.to_csv(path, sep = ',')
    
    data = pd.read_csv(path)
    
    return data


# In[14]:


#data_soda_stereo = retrieve_lyrics("soda stereo")


# In[15]:


#data_soda_stereo.shape


# In[16]:


#data_soda_stereo.spotify.get_boxplot_of_categories('album','tempo',5, 20, 10)


# In[17]:


#tracks_soda_stereo = data_soda_stereo.spotify.tracks
#print(tracks_soda_stereo[0].lyrics)

