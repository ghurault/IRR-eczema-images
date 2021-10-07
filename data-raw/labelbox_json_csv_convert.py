import json
import os
import pandas as pd

LABELBOX_JSON_EXPORT = './LabelBoxJsonExportFile.json'
LABELBOX_EXPORT_DIR = "./data"
OUTPUT_CSV_FILENAME = "meta.csv"

# Check if file path exist:
if not os.path.exists(LABELBOX_EXPORT_DIR):
    os.makedirs(LABELBOX_EXPORT_DIR)

# Read exported labelbox .json file:
with open(LABELBOX_JSON_EXPORT) as json_file:
    raw_data = json.load(json_file)

def get_meta(row):

    meta = {}
    meta["ref_id"] = row['External ID']
    meta["created_at"] = row['Created At']
    meta["updated_at"] = row['Updated At']

    for col in row['Label']['classifications']:
        if 'answer' in col:
            if col['title'] == 'Comments':
                tmp_answers = col['answer']
            else:
                tmp_answers = col['answer']['title']
            meta[col['title']] = tmp_answers
        elif 'answers' in col:
            tmp_answers = []
            for answer in col['answers']:
                tmp_answers.append(answer['title'])
            meta[col['title']] = tmp_answers

    return meta

def main():

    meta_data = pd.DataFrame()

    for row in raw_data:
        meta = get_meta(row)    
        tmp_df = pd.DataFrame({
            "ref_id": meta['ref_id'],
            "created_at": meta['created_at'],
            "updated_at": meta['updated_at'],
            "Evaluability (Quality)": [meta['Evaluability (Quality)'] if 'Evaluability (Quality)' in meta else ""],
            "Bad Image (?)": [meta['Bad Image (?)'] if 'Bad Image (?)' in meta else ""],
            "Comments": [meta['Comments'] if 'Comments' in meta else ""],
            "100% AD": [meta['100% AD'] if '100% AD' in meta else ""],
            "Label Time (Sec)": [row["Seconds to Label"]]
        })
        meta_data = meta_data.append(tmp_df, ignore_index=True)

    meta_data.to_csv(os.path.join(LABELBOX_EXPORT_DIR,OUTPUT_CSV_FILENAME))

if __name__ == "__main__":
    main()