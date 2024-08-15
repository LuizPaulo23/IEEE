#' @title CIS IEEE UNB - Mall Customer Segmentation Data
#' @author Luiz Paulo Tavares Gonçalves 

import warnings
warnings.filterwarnings("ignore")

from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans

import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np
import janitor

# Functions *\
# get_load_cleaned: imports, cleans, and organizes the dataset  

def get_load_cleaned(db: str) -> pd.DataFrame:
    data_raw = pd.read_csv(db)

    data_cleaned = data_raw \
                  .clean_names().drop(columns = ['customerid']) \
                  .rename(columns = {
                               "annual_income_k$_": "income", 
                               "spending_score_1_100_": "score" 
               }
            )
    return data_cleaned

# get_standardize: simply standardize the data 

def get_standardize(data_cleaned, method): 

    """"
    Standardize only numeric columns 
    """
    scaler = StandardScaler()

    numeric_cols = data_cleaned.select_dtypes(include = ['number']).columns
    data_cleaned[numeric_cols] = scaler.fit_transform(data_cleaned[numeric_cols])
    return data_cleaned
    
# Clustering

def get_clusters(data_clean, k_optimal): 

    """
    Clustering: K-means ++
    Calculate the centroids
    Plot clusters, clusters with centroids
    """

    kmeans = KMeans(n_clusters = k_optimal, init = 'k-means++', random_state = 123)
    clusters = kmeans.fit_predict(data_clean)
    data_clean['cluster'] = clusters

    # print the centroids *\

    centroids = kmeans.cluster_centers_
    print("Centroids: \n", centroids)

    # Plotar o gráfico com os clusters *\

    plt.figure(figsize = (10, 8))

    sns.scatterplot(data = data_clean, 
                    x = 'income', 
                    y = 'score',
                    hue = 'cluster', palette = 'viridis', legend = 'full')
    
    # plot centroids *\

    plt.scatter(centroids[:, 0], centroids[:, 1], s = 200, c = 'red', marker = 'X', label='Centroids')

    # Clusters *\*

    plt.title('Clusters Formados pelo K-means')
    plt.xlabel('Renda')
    plt.ylabel('Score')
    plt.grid(True)
    plt.show()

    return data_clean


def get_sse(data_clean, k_max):

    sse = []
    k_range = range(1, k_max + 1)

    for k in k_range: 
        kmeans = KMeans(n_clusters = k, random_state = 123)
        kmeans.fit(data_clean)
        sse.append(kmeans.inertia_)

    return sse

# get_optimal_k: optimal point for the k 
# Forma analítica 

def get_optimal_k(input): 
    
    """"
    Optimal point for the k
    """



# Pipeline *\*




if __name__ == "__main__": 

    # Importing and visualizing (number 1)

    data_cleaned = get_load_cleaned(db = "mall_customers.csv")

    print(data_cleaned)
    print(data_cleaned.describe())

    sns.pairplot(data = data_cleaned, hue = 'gender')
    plt.show()
        
    # Standardizing and selecting variables (number 2)

    # data_scaled = get_standardize(data_cleaned)
    # data_select = data_scaled[["income", "score"]]

    # Applying natural logarithm

    data_select = data_cleaned[["income", "score"]].apply(np.log1p)
    print(data_select)
 
    # Clustering: K-means (number 3 e 4)

    k = 10
    sse_values = get_sse(data_clean = data_select, k_max = k)

    plt.plot(range(1, k+1), sse_values, marker='o')
    plt.xlabel('Número de Clusters')
    plt.ylabel('Soma dos Erros Quadráticos (SSE)')
    plt.title('Método do Cotovelo para Determinar o Número Ideal de Clusters')
    plt.grid(True)
    plt.show()

    # Clustering with k-max = 10

    # get_clusters(data_clean = data_select, k_optimal = k)

    # Clustering: optimal point for the k 


    # with the ideal number of clusters 

    get_clusters(data_clean = data_select, k_optimal = 2)
    get_clusters(data_clean = data_select, k_optimal = 3)
    # get_clusters(data_clean = data_select, k_optimal = 4)

    plt.show()
