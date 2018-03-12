## HOW TO USE THE PHOTO RENAMER

'''
1. Double click to main_branch.py file to launch the Python executable
2. You will be asked where your file geodatabases are located. Type this in, or drag the folder into the pop up windows, then hit enter.
3. Do the same for the output location.
4. The program will load all modules (this may take ~20 seconds) and then give an estimate of the program's runtime (good down to six orders of magnitude)
5. The pictures will be located in the output folder specified along with a log file that shows what was and was not written as well as why

COMMON PROBLEMS:
Misnamed fields in the file geodatabases
Incorrectly specifying input and output folders
'''

print('Loading modules....')
try:
    print('os')
    import os
    print('sys')
    import sys
    print('logging')
    import logging
    print('string')
    import string
    print('arcpy')
    import arcpy
except ImportError:
    input('Error importing modules')
    raise

# to do: reading EXIF data


#hardPath = r"C:\Users\Randall.Jones\Desktop\MtHopeStones6192015.gdb" # hard path to your file geodatabase
#outputLoc = r"C:\Users\Randall.Jones\Desktop\target" # where the output will be written to

hardPath = input('Enter full file geodatabase filepath:\n') # hard path to your file geodatabase
outputLoc = input('Enter full output filepath:\n') # where the output will be written to

os.chdir(outputLoc)
logging.basicConfig(filename='photo_renaming_LOG.log',filemode='w',level=logging.INFO,format='%(levelname)s:%(message)s')

arcpy.env.workspace = hardPath
def listFcsInGDB():
    ''' set your arcpy.env.workspace to a gdb before calling '''
    for fds in arcpy.ListDatasets('','feature') + ['']:
        for fc in arcpy.ListFeatureClasses('','',fds):
            yield os.path.join(arcpy.env.workspace, fds, fc)
features = []
i = 0
print('Your feature classes are...\n')
for filepath in listFcsInGDB():
    thisFeature = os.path.basename(filepath)
    print(str(i+1) + ' - ' + thisFeature)
    features.append(thisFeature)
    i += 1
    dirname = os.path.dirname(filepath)
yourFeatureNum = input('\nPlease enter the number of your target feature class:\n')
yourFeature = features[int(yourFeatureNum)-1]
filepath = dirname+'\\'+yourFeature

ID = input('ID Field Name:\n') # specifies the field name that contains the stream ID
DATE = input('DATE Field Name:\n') # specifies the field name that contains the date the photo was taken/uploaded



#ID = 'OBJECTID' # specifies the field name that contains the stream ID
#DATE = 'DOBDay' # specifies the field name that contains the date the photo was taken/uploaded



##########################################################################





# need to pull attribute ID, date, and BLOB data
# then convert the BLOB to an image and write it (rotation TBD) along with a log file
# skip writing files that have previously been written

attacher = '__ATTACH'
alphabet = string.ascii_lowercase


#for filepath in listFcsInGDB():
rownum = 0
arcpy.MakeTableView_management(os.path.basename(filepath), "dataTable")
arcpy.MakeTableView_management(os.path.basename(filepath) + attacher, "attachTable")
nrowData = int(arcpy.GetCount_management("dataTable").getOutput(0))
nrowAttach = int(arcpy.GetCount_management("attachTable").getOutput(0))

timePerRowPerAttachment = 54.5/385./402. # test case where 385 data rows, with 402 potential attachments were written in 54.5 seconds - O(n^2) D:
estimatedTime = timePerRowPerAttachment*nrowData*nrowAttach

print('Working on ' + os.path.basename(filepath) + ' - ' + str(nrowData) + ' features found, with ' + str(nrowAttach) + ' possible attachments. Estimated time to write: ' + str(estimatedTime) + ' seconds')
#theseFields = [f.name for f in arcpy.ListFields(filepath)]
#print(theseFields)
try:
    #with arcpy.da.SearchCursor(filepath,['ID','CreationDate']) as cursor1:
    with arcpy.da.SearchCursor(filepath,[ID,DATE,'GlobalID']) as cursor1:
        for row in cursor1:
            rownum += 1
            try:
                thisID = row[0]
                thisDate = row[1]
                rowGlobal = row[2]
                alpher = -1
                rowCount = 0
                didWrite = False
                with arcpy.da.SearchCursor(filepath + attacher,['DATA','REL_GLOBALID']) as cursor2:
                    for gow in cursor2:
                        binaryRep = gow[0]
                        gowGlobal = gow[1]
                        rowCount += 1
                        if rowGlobal == gowGlobal:
                            if alpher >= 0:
                                thisID = str(thisID) + alphabet[alpher]

                            try:
                                thisDate = thisDate.strftime('%m-%d-%Y')
                            except AttributeError:
                                print('Doubles!')
                            filename = os.path.basename(filepath) + '_' + str(thisID) + '_' + str(thisDate)
                            alpher += 1
                            didWrite = True

                            open(outputLoc + os.sep + filename + '.jpg', 'wb').write(binaryRep.tobytes()) # rotation?
                            successInfo = 'WRITE ' + filename + ' from FEATURE CLASS ' + os.path.basename(filepath) + ', row ' + str(rownum)
                            #print(successInfo)
                            logging.info(successInfo)

                            del filename
                        if rowCount == nrowAttach and didWrite == False:
                            nomatchString = 'NO ATTACHMENT FOUND for row ' + str(rownum) + ' in FEATURE CLASS ' + os.path.basename(filepath)
                            logging.warning(nomatchString)
                        del gow
                        del binaryRep
                        del gowGlobal
                del alpher
                del thisID
                del thisDate
                del row
                del rowGlobal
            except ImportError:
                rowWarning = 'NOWRITE ' + thisID + ', row ' + str(rownum) +': REASON UNKNOWN'
                print(rowWarning)
                logging.warning(rowWarning)
                
except RuntimeError:
    fieldsWarning = 'ALLFILE NOWRITE ' + os.path.basename(filepath) # this problem usually arises from misnamed/missing fields
    print(fieldsWarning)
    logging.warning(fieldsWarning)

completeText = 'Writing complete.'
print(completeText)
logging.info(completeText)


input()
