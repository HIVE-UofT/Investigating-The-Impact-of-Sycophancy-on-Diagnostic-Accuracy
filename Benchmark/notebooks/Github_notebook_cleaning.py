import json

with open('04_Evaluation_Metrics_and_Results_new.ipynb', 'w') as f:
    nb = json.load(open('04_Evaluation_Metrics_and_Results.ipynb'))
    nb['metadata'].pop('widgets', None)
    json.dump(nb, f)
