import os
import sys
from pathlib import Path
import papermill as pm

def main():
    notebooks = [
        "01_Preprocessing_and_Prompt_Generation.ipynb",
        "02_Open_Source_Models_Inference.ipynb",                     
        "03_Commerical_Models_Inference.ipynb",  
        "04_Evaluation_Metrics_Accuracy_and_Flip_Rate_Analysis.ipynb",
        "05_Evaluation_Metrics_Consistency_Transformation_Rate_Analysis.ipynb"
    ]

    current_dir = Path(__file__).parent
    print("Starting pipeline execution...")

    for i, notebook in enumerate(notebooks, 1):
        notebook_path = current_dir / notebook

        if not notebook_path.exists():
            print(f"Notebook not found: {notebook_path}")
            print(f"Please ensure the notebook exists in {current_dir}")
            sys.exit(1)

        output_notebook_path = notebook_path.with_name(
            f"{notebook_path.stem}_output.ipynb"
        )

        print(f"Step {i}/4: {notebook}")
        try:
            pm.execute_notebook(
                input_path=str(notebook_path),
                output_path=str(output_notebook_path),  
                kernel_name="conda-base"   
            )
            print(f"Successfully completed: {output_notebook_path.name}")
        except Exception as e:
            print(f"Error running {notebook_path.name}: {e}")
            sys.exit(1)

        print("-" * 40)

    print("Pipeline completed successfully!")

if __name__ == "__main__":
    main()
