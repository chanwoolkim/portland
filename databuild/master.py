# Preliminary ####
# Preliminary ####
from google.oauth2 import service_account
from google.cloud import bigquery
import os
import glob
import re
import subprocess
import sys
import pandas as pd
import duckdb
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

with open(code_dir + "/setup_portland.py", "r") as file:
    code = file.read()
    exec(code)

with open(code_dir + "/query.py", "r") as file:
    code = file.read()
    exec(code)
    

# SERVUS-Based Queries ####
working_data_dir = project_dir + '/data/analysis/servus'

with open(code_dir + "/query.py", "r") as file:
    code = file.read()
    exec(code)
