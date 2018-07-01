import os, sys, csv
from operator import itemgetter

DIR_NAME = "landsat"
filelist = [f for f in os.listdir(DIR_NAME) if f.split('.')[-1] == "csv"]
print(filelist)

metadata = {}
keys = [
    'Landsat Product Identifier',
    'Landsat Scene Identifier',
    'Acquisition Date',
    # 'Spacecraft Identifier',
    'Collection Category',
    'Date L-1 Generated',
    # 'Sensor Mode',
    'WRS Path', 'WRS Row',
    'Center Latitude dec', 'Center Longitude dec',
    'UL Corner Lat dec', 'UL Corner Long dec',
    'UR Corner Lat dec', 'UR Corner Long dec',
    'LL Corner Lat dec', 'LL Corner Long dec',
    'LR Corner Lat dec', 'LR Corner Long dec'
]
for f in filelist:
    path = os.path.join(DIR_NAME, f)
    reader = csv.reader(open(path), delimiter=',')
    header = next(reader)
    print(f, header)
    idx = [header.index(k) for k in keys]
    print(idx)
    for r in reader:
        metadata[r[0]] = [r[i] for i in idx]

print(len(metadata))
with open('landsat_metadata.csv', 'w') as csvfile:
    writer = csv.writer(csvfile, delimiter=',')
    writer.writerow(keys)
    for v in sorted(metadata.values(), key=itemgetter(2)):
        writer.writerow(v)
