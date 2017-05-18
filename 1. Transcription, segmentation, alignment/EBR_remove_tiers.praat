######################################################################
# EBR_remove_tiers.praat - June 2015
# Eva Bosch i Roura (eva.bosch.roura@gmail.com)
# Universitat de Barcelona - Departament de Filologia Catalana
#
# Script adapted from Katherine Crosswhite's text_grid_maker.praat
# (www.linguistics.ucla.edu/faciliti/facilities/acoustic/text_grid_maker.txt) and
# from Wendy Elvira-Garcia's remove-tiers.praat (http://stel.ub.edu/labfon/en/praat-scripts)
#
# This script asks the user for an 'inputDir' directory where TextGrids to be modified are found,
# and for which 3 tiers the user would like to remove (more can be added through the form).
# The script loops through the directory, gets all the TextGrids on it, removes the defined
# tiers and saves the new TextGrid in the desired 'outputDir' directory.
#
# Begin removing tiers from biggest number to smaller (e.g. first 3, then 2, then 1) to avoid
# renumbering issues and already removed tiers.
#
#################################################################


# Form: ask user for input and output directories, and which tiers should be removed, by their number.

form Enter directory
	comment Directory
	sentence inputDir /Users/pathtofolder/whereyourinputis/
	sentence outputDir /Users/pathtofolder/whereyouroutput/willgo/
	comment Which tiers do you want to remove?
	integer out_tier1 1
	integer out_tier2 2
	integer out_tier3 3
endform

# Make a list of all TextGrids in the directory

Create Strings as file list... list 'inputDir$'*.TextGrid



# loop for all files

numberOfFiles = Get number of strings

# set count
file_count = 0

for ifile from 1 to numberOfFiles

	# add file to file_count
	file_count = file_count + 1

	# string variable (current_file$) to store 1st filename from the list
	select Strings list
	current_file$ = Get string... ifile

	# read that 1st file we just named
	Read from file... 'inputDir$''current_file$'

	# name selected TextGrid object, withouth the extension
	fileName$ = selected$ ("TextGrid")

	# remove tiers selected in the initial form (begin by biggest number)
	Remove tier... out_tier3
	Remove tier... out_tier2
	Remove tier... out_tier1

	# save the new TextGrid in the 'outputDir' directory, and name it by its new name (INFORMANTgr_Test_tiers.TextGrid)

	finalfilename$ = replace_regex$ ("'fileName$'", "_sppas", "_tiers", 0)
	Save as text file... 'outputDir$''finalfilename$'.TextGrid

# end the loop, move on to the next file. Remove the objects we don't need anymore
select all
minus Strings list
Remove

endfor

# Clean up the Praat objects window
select all
Remove

# Display a message that will tell you 1) How many files have been processed and 2) Praat is done.
printline All 'file_count' files from 'inputDir$' have been processed!
printline The new TextGrids have been saved in 'outputDir$'.
