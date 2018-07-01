import os, csv
from PIL import Image

metadata = {}
DIR_RAW_DATA = "../data/satellite"

def crop_image(item):
    with Image.open(item["Path"]) as img:
        width, height = img.size
        print(item["Landsat Product Identifier"], width, height)

def read_data():
    global metadata
    metadata_file = os.path.join(DIR_RAW_DATA, "landsat_metadata.csv")
    reader = csv.reader(open(metadata_file), delimiter=',')
    keys = next(reader)
    # print(keys)

    for r in reader:
        filepath = os.path.join(DIR_RAW_DATA, "landsat", "{}.jpg".format(r[0]))
        if os.path.isfile(filepath):
            metadata[r[0]] = {keys[i]:r[i] for i in range(len(keys))}
            metadata[r[0]]["Path"] = filepath
    # print(metadata)

read_data()
test = list(metadata.values())[0]
print(test)
crop_image(test)
