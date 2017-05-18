######################################################################
# EBR_boundaries_to_zerocrossing.praat - June 2015
# Author: Eva Bosch i Roura (eva.bosch.roura@gmail.com)
#         Universitat de Barcelona - Departament de Filologia Catalana
#
# The script loops through TextGrids, gets a sound with the same name and
# moves all the interval boundaries to the nearest zero crossing.
#
#################################################################


# Form: ask user for input and output directories, and for the extension of sound files

form Enter directory
  comment Directory
  sentence inputDir /Users//Users/pathtofolder/whereyourinputis/
  sentence outputDir /Users/pathtofolder/whereyouroutput/willgo/
  sentence Sound_extn .wav
endform

# MAKE A LIST OF ALL TEXTGRIDS IN THE FOLDER
Create Strings as file list... list 'inputDir$'*.TextGrid
file_list = selected("Strings")
file_count = Get number of strings

# set count
files_processed = 0

# LOOP THROUGH THE LIST OF FILES...
for current_file from 1 to file_count

# add file to count
files_processed = files_processed + 1

    # READ IN THE TEXTGRID & CORRESPONDING SOUND...
    select Strings list
    gridname$ = Get string... current_file
    Read from file... 'inputDir$''gridname$'
    filename$ = selected$ ("TextGrid", 1)
    Read from file... 'inputDir$''filename$''sound_extn$'

    printline 'filename$'

    select TextGrid 'filename$'

    # Find out the number of tiers
    ntiers = Get number of tiers

    # Loop through the tiers
    for t to ntiers
    interval = Is interval tier... t

      # find all the intervals in tiers
      if interval

        nintervals = Get number of intervals... t

        # get the label of each interval, save to recover later
        for i to nintervals
          imoved = 0
          label$[i] = Get label of interval... t i
        endfor

        # find what the interval end point is, and where is the nearest zero crossing
        for i to nintervals-1
          select TextGrid 'filename$'
          boundary = Get end point... t i
          select Sound 'filename$'
          zero = Get nearest zero crossing... 1 boundary

          # If the boundary is not already there, move it to the nearest zero crossing, established above ('zero')
          # this removes the boundary, and the whole label of the interval is removed
          if boundary != zero
            select TextGrid 'filename$'
            Remove right boundary... t i
            Insert boundary... t zero
            imoved = imoved + 1
          endif



        endfor

       printline 'imoved' boundaries were moved in tier 't'

       # put label back to each interval
        select TextGrid 'filename$'
        for i to nintervals
          name$ = label$[i]
          Set interval text... t i 'name$'
        endfor

        Save as text file: outputDir$ + filename$ + "_ajustat" + ".TextGrid"

      endif

    endfor

# End the loop and move on to the next file. Remove the objects we don't need anymore.
select all
minus Strings list
Remove

  # End object loop
endfor

select all
Remove

printline Done! Boundaries were moved to nearest zero crossings in 'files_processed' files.