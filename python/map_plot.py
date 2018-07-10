import os, csv
from PIL import Image

metadata = {}
DIR_RAW_DATA = "../data/satellite"
DIR_CROP_DATA = "../data/cropped"

lakeGeorge = {
    "top": -34.75, "left": 149.35,
    "bottom": -35.35, "right": 149.80,
}

def crop_image(item):
    with Image.open(item["Path"]) as img:
        width, height = img.size
        print(item["Landsat Product Identifier"], width, height)
        ULy, ULx = float(item["UL Corner Lat dec"]), float(item["UL Corner Long dec"])
        URy, URx = float(item["UR Corner Lat dec"]), float(item["UR Corner Long dec"])
        LLy, LLx = float(item["LL Corner Lat dec"]), float(item["LL Corner Long dec"])
        LRy, LRx = float(item["LR Corner Lat dec"]), float(item["LR Corner Long dec"])
        CTy, CTx = float(item["Center Latitude dec"]), float(item["Center Longitude dec"])
        # print("UL", ULx, ULy)
        # print("UR", URx, URy)
        # print("LL", LLx, LLy)
        # print("LR", LRx, LRy)
        # print("Center", CTx, CTy)
        # avgLeft, avgRight = (ULx+LLx)/2, (URx+LRx)/2
        # avgTop, avgBottom = (ULy+URy)/2, (LLy+LRy)/2
        avgLeft, avgRight = ULx, LRx
        avgTop, avgBottom = ULy, LRy
        avgWidth = abs(avgLeft-avgRight)
        avgHeight = abs(avgTop-avgBottom)
        wScale, hScale = width/avgWidth, height/avgHeight
        # print("avgWidth", avgWidth, width/avgWidth)
        # print("avgHeight", avgHeight, height/avgHeight)
        # print(avgTop, avgBottom, avgLeft, avgRight)
        mtop = (avgTop-lakeGeorge["top"])*hScale
        mbottom = (lakeGeorge["bottom"]-avgBottom)*hScale
        mleft = (lakeGeorge["left"]-avgLeft)*wScale
        mright = (avgRight-lakeGeorge["right"])*wScale
        # print(mleft, mtop, mright, mbottom)
        newimg = img.crop((mleft, mtop, width-mright, height-mbottom))
        newimg.save(os.path.join(DIR_CROP_DATA, "{}.jpg".format(item["Landsat Product Identifier"])))
        pix = newimg.load()
        width, height = newimg.size
        print(width, height)
        for i in range(0,width):
            print([pix[i, j] for j in range(0,height)])
        # print(pix)

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
v = metadata["LT05_L1TP_090084_19900708_20170129_01_T2"]
crop_image(v)
# for k, v in list(metadata.items())[:10]:
    # if not k.split("_")[0] == "LM05":
    #     crop_image(v)
