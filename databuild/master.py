# Preliminary ####
from google.oauth2 import service_account
from google.cloud import bigquery
from openai import OpenAI
from collections import OrderedDict
import os
import glob
import re
import subprocess
import sys
import time
import json
import pandas as pd
import numpy as np
from census import Census
import geopandas as gpd
from us import states
import duckdb
import pyreadr
import pyarrow as pa
import pyarrow.parquet as pq

# Define paths
base_dir = os.path.abspath(sys.argv[0])
project_dir = base_dir + '/../..'


# TransUnion-Based Queries ####
# Define paths
code_dir = project_dir + '/code/databuild/transunion_queries'
raw_data_dir = project_dir + '/data/raw'
raw_pwb_data_dir = raw_data_dir + '/PWB'
raw_tu_data_dir = raw_data_dir + '/TU'
auxiliary_data_dir = project_dir + '/data/auxiliary'
working_data_dir = project_dir + '/data/analysis/transunion'

# First do the set up
with open(code_dir + '/setup_portland.py', 'r') as file:
    code = file.read()
    exec(code)

with open(code_dir + '/setup_transunion.py', 'r') as file:
    code = file.read()
    exec(code)

with open(code_dir + '/setup_census.py', 'r') as file:
    code = file.read()
    exec(code)

# Then run the queries to download the data
with open(code_dir + '/query.py', 'r') as file:
    code = file.read()
    exec(code)
    

# SERVUS-Based Queries ####
# Define paths
code_dir = project_dir + '/code/databuild/servus_queries'
raw_data_dir = project_dir + '/data/raw'
raw_aspire_north_data_dir = raw_data_dir + '/Aspire North'
working_data_dir = project_dir + '/data/analysis/servus'

# First do the set up
with open(code_dir + '/setup_servus.py', 'r') as file:
    code = file.read()
    exec(code)

with open(code_dir + '/setup_census.py', 'r') as file:
    code = file.read()
    exec(code)

with open(code_dir + '/aspire_data_type.py', 'r') as file:
    code = file.read()
    exec(code)

with open(code_dir + '/setup_aspire_north.py', 'r') as file:
    code = file.read()
    exec(code)

# Then run the queries to download the data
with open(code_dir + '/query.py', 'r') as file:
    code = file.read()
    exec(code)