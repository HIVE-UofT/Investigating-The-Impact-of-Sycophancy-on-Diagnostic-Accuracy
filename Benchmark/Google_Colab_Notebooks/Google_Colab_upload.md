
## Step 1: Put Your Project in Google Drive

First, upload your entire project folder to your Google Drive.
Your Google Drive should look something like this:


```
My Drive/
└── 📂 Reusable Benchmark/
    ├── 📄 config.yaml
    ├── 📂 data/
    ├── 📂 notebooks/
    └── 📂 results/
```


## Step 2: Connect to Google Drive in Your Notebook

Now, at the very top of each of your notebooks (01, 02, 03, etc.), add this small code block. This cell "mounts" your Google Drive, making it accessible like a local disk.

```python
# Add this cell to the top of your Colab notebooks
from google.colab import drive
drive.mount('/content/drive')
```

When you run this, a pop-up will ask you to authorize access. Just follow the prompts.

## Step 3: Update Your config.yaml Paths for Drive

This is the final key step. Since your files are now in a new location (/content/drive/MyDrive/...), you need to update your config.yaml to point to their full paths. The ../ is no longer needed.

Open your config.yaml file (in Google Drive) and change the paths to look like this:

#### BEFORE (Your local paths):

```yaml
dataset:
  path: "../data/Sycophancy_Main.xlsx"

output:
  prompts_csv: "data/prompts/full_cases_and_prompts.csv"
  main_results_file: "results/sycophancy_results_with_inference.csv"

```
#### AFTER (Your new Google Drive paths):

```yaml
# Note: Adjust "Reusable Benchmark" if your folder has a different name
dataset:
  path: "/content/drive/MyDrive/Reusable Benchmark/data/Sycophancy_Main.xlsx"

output:
  prompts_csv: "/content/drive/MyDrive/Reusable Benchmark/data/prompts/full_cases_and_prompts.csv"
  main_results_file: "/content/drive/MyDrive/Reusable Benchmark/results/sycophancy_results_with_inference.csv"
```


