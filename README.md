# odds-and-ends

## Various one-off scripts. May or may not work.

banker.R - an attempt to find river bankfulls algorithmically

filegeodatabasePhotoRenamer.py - renames photos in a file geodatabase based on two fields. Don't remember why I wrote this. Uses arcpy (and therefore you must use a Python 2.x interpreter)

nameOnWatershed_SpreadsheetExample.xlsx - spreadsheet with data that is fed into namePhotosOnWatershed.py

nameOnWatershed.py - renames georeferences photos depending on what watershed they fall in and the date-time they were taken. The script itself actual just renames photos using a prefilled field in the spreadsheet; the analysis to find what watershed the photo falls in is typically done in ArcGIS. Could be extended to do this part programatically. Does not use arcpy

riffleGA.R - a genetic algorithm for designing riffle cross sections. Optimizes parameters well but does not produce realistic channel shapes

riffleGA_trap.R - the above but modified to produce trapezoidal channels. Still does not produce realistic shapes

visvalingam.R - a modification of visvalingam's algorithm for reducing the number of points in a shape. Implemented specifically for river cross sections. Always keeps the thalweg
