
echo $DISPLAY
export DISPLAY=localhost:11.0
module load daint-gpu
module load cray-python
module load ncview

# Install X quatz!!!
ncview /store/c2sm/pr04/jvergara/RUNS_IN_SCRATCH/MAC1/lm_f/1h/lffd20110811090000.nc




import os
os.getcwd()

os.chdir('/Users/gao/OneDrive - whu.edu.cn/ETH/1. Lernveranstaltungen/3. Semester/trip_info_imputation')



import numpy as np
import pandas as pd

tp_py = pd.read_csv("3_output/tp_py.csv")

# tp_py['ID'] = tp_py['ID'].astype("category")


















