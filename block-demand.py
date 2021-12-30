#!/usr/bin/env python3

import urllib.request
import json
import matplotlib.pyplot as plt
import numpy as np

def fetch_demand_hist(url = "https://mempool.space/api/mempool"):
    obj = json.load(urllib.request.urlopen(url))
    return obj['fee_histogram']

def plot_demand_hist(hist):
    y = [ p[0] for p in hist ]
    x = np.cumsum([ p[1] for p in hist ])
    plt.grid(True)
    plt.axis([0.0, 8.0e6, 0.0, 1.05 * np.max(y)])
    plt.plot(x, y)
    plt.show()

plot_demand_hist(fetch_demand_hist())
