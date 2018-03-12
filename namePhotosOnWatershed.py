
import os
import pandas
import shutil



hardPath = r'C:/Users/sky.jones/Desktop/unnamedFieldPhotoRenamer/WTSP.xlsx'
outputLoc = r'C:/Users/sky.jones/Desktop/unnamedFieldPhotoRenamer/NamedFiles'

os.chdir(outputLoc)

df = pandas.read_excel(hardPath, sheet_name='Sheet1')

for index, row in df.iterrows():
    #newName = os.path.join(outputLoc, row['Output'])
    newName = outputLoc + '/' + str(row['Output']) + '.jpg'
    oldLoc = row['Path']
    oldLoc = oldLoc.replace('\\','/')
    
    shutil.copy(oldLoc,newName)
    




