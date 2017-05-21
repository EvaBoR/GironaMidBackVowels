# ########################################################################################################################
# PRAAT SCRIPT "EBR SEMI-AUTO FORMANT EXTRACTOR"
#
# SCRIPT ADAPTED BY EVA BOSCH-ROURA (Universitat de Barcelona, eva.bosch.roura@gmail.com), 20/06/2015,
# HEAVILY BASED ON AND VERY SIMILAR TO D. MCCLOY AND A. MCGRATH'S SemiAutoFormantExtractor.praat, version 0.3
# SEE https://github.com/drammock/praat-semiauto FOR THE ORIGINAL
#
# This script semi-automates measuring formants from sound files with labeled TextGrids. It loops through a directory of
# TextGrids, finds sound files with the same name, opens them one at a time, displays a table of formant values for
# the interval at specified time points and asks the user to accept the formant measurements, adjust the settings and
# redraw the estimation, or mark the interval as unmeasurable, before continuing on to the next interval or file. At each
# interval, comments can be included to make revisions easier.
#
# For further information on the specific use of this script, see:
#	 	BOSCH-ROURA, Eva (2017). «Les vocals mitjanes posteriors en el català de Girona. Anàlisi de la producció
# 		i la percepció.» Tesi doctoral. Universitat de Barcelona.
#
# #######################################################################################################################

# COLLECT ALL THE USER INPUT
form Select directories for TextGrids and Sound files
		sentence inputDir /Users/pathtofolder/whereyourinputis/
		sentence Sound_extension .wav
		integer Starting_file_number 1
	comment Inicia el comptador de tokens a (últim token analitzat, # Start token counter at (last analyzed token,
	comment NO PAS el primer per analitzar): # NOT first token to be analyzed)
		integer starting_token_count 0
		real Zoom_duration 0.5
		sentence Output_file /Users/pathtofolder/whereyouroutput/willgo/(ARXIPRESTAT)_1formants.txt # # Change output file name as necessary
		positive Default_max_formant 5500 # default formant tracker settings, both max formant and number of formants can be changed during the analysis
		integer Default_formant_number 5
		real Time_step 0.1
		real Preemphasis_from 50
		positive Window_length 0.015
		positive Dynamic_range 50
		positive Dot_size 0.6
		optionmenu Interval_measurement_option: 8
			option midpoint
			option onset, midpoint, offset
			option 20%, 50%, 80%
			option 25%, 50%, 75%
			option 10%, 30%, 50%, 70%, 90%
			option 5%, 10%, 20%, 50%, 80%, 90%, 95%
			option 20%, 30%, 40%, 50%, 60%, 70%, 80%
			option 10%, 20%, 30%, 40%, 50%, 60%, 70%, 80%, 90%
	comment Si l'Interval measurement option no és l'última (10-90%) ## If Interval measurement is not the last option,
	comment v. línia 294 del script. # see line 294 of this script
endform

# SET APPROPRIATE TIERS
label_tier = 1
word_tier = 2
wordref_tier = 3
contextnum_tier = 4
prevcontext_tier = 5
postcontext_tier = 6
symmetry_tier = 7
sylltype_tier = 8
syllpos_tier = 9
entontype_tier = 10
entonpos_tier = 11
test_tier = 12


# RUN SOME FUNCTIONS ON THE USER INPUT (TO BE USED LATER)
call pointsPerInterval # see procedures starting at line 374

# BE FORGIVING IF THE USER FORGOT TRAILING PATH SLASHES OR LEADING FILE EXTENSION DOTS
call cleanPath 'inputDir$'
textgrid_dir$ = "'cleanPath.out$'"
call cleanExtn 'sound_extension$'
sound_extn$ = "'cleanExtn.out$'"

# DEFINE DUMMY COUNTER VARIABLES FOR END-OF-SCRIPT REPORT
vowel_count = 0
token_count = starting_token_count

# GET TIME, OS AND PRAAT VERSION
rundate$ = date$ ()
rundate$ = replace_regex$ ("'rundate$'", "(Mon|Tue|Wed|Thu|Fri|Sat|Sun)( )(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)( )(\d+)( )(\d{2}:\d{2}:\d{2})( )(\d{4})", "\5/\3/\9", 0)

if windows = 1
    os$ = "Windows"
elsif macintosh = 1
    os$ = "OSX"
elsif unix = 1
    os$ = "Linux"
endif

version$ = "'praatVersion'"
version$ = replace_regex$ ("'version$'", "(\d)(\d)(\d{2,2})", "\1.\2.\3", 0)

# INITIATE THE OUTPUT FILE
if fileReadable (output_file$)
	beginPause ("The output file already exists!")
		comment ("The output file already exists!")
		comment ("You can overwrite the existing file, or append new data to the end of it.")
	overwrite_setting = endPause ("Append", "Overwrite", 1)
	if overwrite_setting = 2
		filedelete 'output_file$'
		call initializeOutfile
	endif
else
	# THERE IS NOTHING TO OVERWRITE, SO CREATE THE HEADER ROW FOR THE NEW OUTPUT FILE
	call initializeOutfile
endif

# MAKE A LIST OF ALL TEXTGRIDS IN THE FOLDER
Create Strings as file list... list 'inputDir$'*.TextGrid
file_list = selected("Strings")
file_count = Get number of strings

# LOOP THROUGH THE LIST OF FILES...
for current_file from starting_file_number to file_count

	# READ IN THE TEXTGRID & CORRESPONDING SOUND...
	select Strings list
	gridname$ = Get string... current_file
	Read from file... 'inputDir$''gridname$'
	filename$ = selected$ ("TextGrid", 1)
	Open long sound file... 'inputDir$''filename$''sound_extn$'
	total_duration = Get total duration

		#GET SPEAKER ID FROM 1st PART OF THE NAME OF THE FILE, BY KEEPING ID CODE (beginning of the file name)
        speaker$ = replace_regex$ ("'filename$'", "(\D+-FE(1|2)-(D|H)[1-4])\S*", "\1", 0) # sp. ID is made up of a 2 or 3 survey point code, FE1 or FE2 depending on age, H if man or D if woman, plus a number between 1 and 4

	# BOOLEAN TO PREVENT OPENING MULTIPLE EDITORS FOR THE SAME LONGSOUND
	new_file = 1

	# FIND THE LABELED INTERVAL...
	select TextGrid 'filename$'
	num_intervals = Get number of intervals... label_tier
	for interval to num_intervals
		select TextGrid 'filename$'
		label$ = Get label of interval... label_tier interval

		# IF THE LABEL IS NON-EMPTY
		if label$ <> ""

			#INCREMENT VOWEL COUNT
            vowel_count = vowel_count + 1
            token_count = token_count + 1

            #GET ENDPOINTS AND DURATION
			start = Get starting point... label_tier interval
			end = Get end point... label_tier interval
			midpoint = (start+end)/2
			duration = (end-start)
            duration_ms = duration*1000

            #FROM WHAT WORD DOES THE VOWEL COME FROM?
        	word = Get interval at time... word_tier midpoint
        	word_label$ = Get label of interval... word_tier word

        	#WHAT IS THE WORD REFERENCE NUMBER FOR THIS VOWEL?
        	wordref = Get interval at time... wordref_tier midpoint
	        wordref_label$ = Get label of interval... wordref_tier wordref

	        #WHAT IS THE CONTEXT REFERENCE NUMBER FOR THIS VOWEL IN THIS WORD?
	        contextnum = Get interval at time... contextnum_tier midpoint
	        contextnum_label$ = Get label of interval... contextnum_tier contextnum

	        #WHAT IS THE ACTUAL PREVIOUS SOUND FOR THIS VOWEL IN THIS UTTERANCE?
	        prevcontext = Get interval at time... prevcontext_tier midpoint
	        prevcontext_label$ = Get label of interval... prevcontext_tier prevcontext

	        #WHAT IS THE ACTUAL FOLLOWING SOUND FOR THIS VOWEL IN THIS UTTERANCE?
	        postcontext = Get interval at time... postcontext_tier midpoint
	        postcontext_label$ = Get label of interval... postcontext_tier postcontext

	        # IS THE VOWEL IN A SYMMETRICAL (S) OR ASSYMMETRIC (A) SYLLABLE?
	        symmetry = Get interval at time... symmetry_tier midpoint
	        symmetry_label$ = Get label of interval... symmetry_tier symmetry

	        # IS THE VOWEL IN AN OPEN (O=oberta) OR CLOSED (T=travada) SYLLABLE?
	        sylltype = Get interval at time... sylltype_tier midpoint
	        sylltype_label$ = Get label of interval... sylltype_tier sylltype

	        # WHAT POSITION DOES THE VOWEL OCCUPY WITHIN THE SYLLABLE? INITIAL (SI = síl·laba inicial), MEDIAL (SI = síl·laba medial) or FINAL (SF = síl·laba final)
	        syllpos = Get interval at time... syllpos_tier midpoint
	        syllpos_label$ = Get label of interval... syllpos_tier syllpos

	        # THE REF. WORD IS FOUND IN WHAT TYPE OF ENTONATION PHRASE? Dec (declarativa), Int (interrogativa), Exc (exclamativa)
	        entontype = Get interval at time... entontype_tier midpoint
	        entontype_label$ = Get label of interval... entontype_tier entontype

	        # WHAT POSITION DOES THE REF. WORD OCCUPY WITHIN THE ENTONATION PHRASE? INITIAL (PI = prosòdia inicial), MEDIAL (PI = prosòdia medial) or FINAL (PF = prosòdia final)
	        entonpos = Get interval at time... entonpos_tier midpoint
	        entonpos_label$ = Get label of interval... entonpos_tier entonpos

	        #TO WHAT TEST DOES THE WORD BELONG?
	        test = Get interval at time... test_tier midpoint
	        test_label$ = Get label of interval... test_tier test

			# PREVENT ZOOM DURATION FROM EXTENDING BEYOND THE ENDS OF THE FILE, BUT TRY TO MAINTAIN THE DESIRED WINDOW SIZE
			if not zoom_duration = 0
				left_edge = midpoint - zoom_duration/2
				right_edge = midpoint + zoom_duration/2
				right_excess = right_edge - total_duration

				if left_edge < 0
					zoom_start = 0
					if zoom_duration > total_duration
						zoom_end = total_duration
					else
						zoom_end = zoom_duration
					endif
				elif right_edge > total_duration
					zoom_end = total_duration
					if left_edge > right_excess
						zoom_start = zoom_end - zoom_duration
					else
						zoom_start = 0
					endif
				else
					zoom_start = left_edge
					zoom_end = right_edge
				endif
			else  ;  zoom_duration = 0
				zoom_start = 0
				zoom_end = total_duration
			endif  ;  zoom_duration

			if new_file = 1
				# IF THIS IS THE FIRST INTERVAL OF THE CURRENT FILE, SHOW THE EDITOR WINDOW
				select LongSound 'filename$'
				plus TextGrid 'filename$'
				View & Edit

				# SINCE WE'RE IN THE FIRST LABELED INTERVAL, SET ALL THE SETTINGS
				editor TextGrid 'filename$'
					# FIRST, HIDE THE SPECTROGRAM ETC TO PREVENT ANNOYING FLICKERING
					Show analyses... no no no no no 10
					Zoom... zoom_start zoom_end

					# NOW SET ALL THE RELEVANT SETTINGS AND DISPLAY WIDEBAND SPECTROGRAM
					Spectrogram settings... 0 5000 0.005 50
					Advanced spectrogram settings... 1000 250 Fourier Gaussian yes 100 6 0
					Formant settings... default_max_formant default_formant_number window_length dynamic_range dot_size
					Advanced formant settings... burg preemphasis_from

					# SHOW THE FORMANT TRACKS
					if not zoom_duration = 0
						# MAKE SURE THE "MAX ANALYSIS" SETTING IS LONG ENOUGH SO THE SPECTROGRAM ACTUALLY SHOWS UP
						Show analyses... yes no no yes no zoom_duration*2
					else
						# WE ASSUME WE'RE WORKING ON A SINGLE WORD AT A TIME, OR A VERY SHORT FILE, so 10 seconds should be enough
						Show analyses... yes yes no yes no 10
					endif
				endeditor

			else
				# WE'RE NOT IN THE FIRST LABELED INTERVAL, SO EDITOR IS OPEN & SETTINGS ARE SET, SO JUST MOVE TO THE CURRENT INTERVAL
				editor TextGrid 'filename$'
					Zoom... zoom_start zoom_end
				endeditor
			endif
			new_file = 0

			# INITIALIZE SOME VARIABLES FOR THE PAUSE U.I.
			clicked = 0
			max_formant = default_max_formant
			formant_number = default_formant_number
			call getMeasureTimes
			call getFormants
			call makeFormantTable
			current_time_point = getMeasureTimes.time[1]
			current_interval_measurement = 1

			# PLACE CURSOR AT FIRST MEASUREMENT POINT
			editor TextGrid 'filename$'
				Move cursor to... current_time_point
			endeditor

			# SHOW A U.I. WITH FORMANT TRACKER SETTINGS & MEASURED FORMANT VALUES.
			# KEEP SHOWING IT UNTIL THE USER ACCEPTS OR CANCELS THE MEASUREMENT FOR THIS INTERVAL.
			repeat
				beginPause ("Adjust formant tracker settings")
					comment ("File 'filename$' (file number 'current_file' of 'file_count')")
					comment ("You can change these settings if the formant track doesn't look right.")
					integer ("New_max_formant", max_formant)
					integer ("New_number_formants", formant_number)
					comment ("Clicking PLAY will play the sound in the interval")
					comment ("Clicking REDRAW will redraw the formant tracks with the settings above")
					comment ("Cicking SKIP will record all formants as zero to mark for manual measurement")
					comment (" ")
					comment ("Formant measurements:")

					# CREATE THE FORMANT TABLE
					call getFormants
					call makeFormantTable
					comment ("'makeFormantTable.header$'")

						#If Interval_measurement_option: 1-7, uncomment:
						#comment ("'makeFormantTable.f4$'")
						#comment ("'makeFormantTable.f3$'")
						#comment ("'makeFormantTable.f2$'")
						#comment ("'makeFormantTable.f1$'")

						#If Interval_measurement_option: 8, uncomment:
						comment ("'makeFormantTable.p10$'")
						comment ("'makeFormantTable.p20$'")
						comment ("'makeFormantTable.p30$'")
						comment ("'makeFormantTable.p40$'")
						comment ("'makeFormantTable.p50$'")
						comment ("'makeFormantTable.p60$'")
						comment ("'makeFormantTable.p70$'")
						comment ("'makeFormantTable.p80$'")
						comment ("'makeFormantTable.p90$'")
					comment (" ")
					sentence ("Comentaris", "")
				clicked = endPause ("Play", "Redraw", "Skip", "Accept", 4)

				# IF THEY CLICKED "PLAY"
				if clicked = 1
					editor TextGrid 'filename$'
						Play... start end
					endeditor

				# IF THEY CLICKED "REDRAW"
				elif clicked = 2
					max_formant = new_max_formant
					formant_number = new_number_formants
					editor TextGrid 'filename$'
						Formant settings... max_formant formant_number window_length dynamic_range dot_size
					endeditor
				endif

			until clicked >2
			# END OF THE PAUSE U.I.

			# THE USER HAS EITHER ACCEPTED OR SKIPPED, SO WRITE OUT THE VALUES
			for i from 1 to pointsPerInterval.pts
				time = getMeasureTimes.time[i]
				intrvl = ((time-start)/(end-start))
				if clicked = 3
					# MARK FOR HAND MEASUREMENT
					f1 = 0
					f2 = 0
					f3 = 0
				elif clicked = 4
					# GET MEASURED VALUES
					f1 = getFormants.f1[i]
					f2 = getFormants.f2[i]
					f3 = getFormants.f3[i]
				endif

				# WRITE OUT TO FILE
				resultline$ = "'speaker$'" + tab$ + "'label$'" + tab$ + "'word_label$'" + tab$ + "'wordref_label$'" + tab$ + "'contextnum_label$'" + tab$ + "'prevcontext_label$'" + tab$ + "'postcontext_label$'" + tab$ + "'symmetry_label$'" + tab$ + "'sylltype_label$'" + tab$ + "'syllpos_label$'" + tab$ + "'entontype_label$'" + tab$ + "'entonpos_label$'" + tab$ + "'test_label$'" + tab$ + "'duration_ms:3'" + tab$ + "'start:3'" + tab$ + "'end:3'" + tab$ + "'token_count'" + tab$ + "'intrvl:2'" + tab$ + "'time'" + tab$ + "'f1:0'" + tab$ + "'f2:0'" + tab$ + "'f3:0'" + tab$ + "'max_formant'" + tab$ + "'formant_number'" + tab$ + "'comentaris$'" + tab$ + "'rundate$'" + tab$ + "'version$'" + tab$ + "'os$'" + newline$
				fileappend "'output_file$'" 'resultline$'

			endfor ; EACH POINT IN THE INTERVAL

		endif ; LABEL <> ""

	endfor ; EACH INTERVAL IN THE FILE

	# REMOVE ALL THE OBJECTS FOR THAT FILE AND GO ON TO THE NEXT ONE
	select LongSound 'filename$'
	plus TextGrid 'filename$'
	Remove
	select Strings list

endfor ; EACH FILE IN THE FOLDER

# REMOVE THE STRINGS LIST AND GIVE A SUCCESS MESSAGE
select Strings list
Remove
clearinfo
files_read = file_count - starting_file_number + 1
printline Done! 'vowel_count' vowels analyzed in 'files_read' files.'newline$'


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTIONS (A.K.A. PROCEDURES) THAT WERE CALLED EARLIER  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

procedure cleanPath .in$
	if not right$(.in$, 1) = "/"
		.out$ = "'.in$'" + "/"
	else
		.out$ = "'.in$'"
	endif
endproc

procedure cleanExtn .in$
	if not left$(.in$, 1) = "."
		.out$ = "." + "'.in$'"
	else
		.out$ = "'.in$'"
	endif
endproc

procedure initializeOutfile
	## HEADER COLUMN NAMES
	headerline$ = "INFORMANT" + tab$ + "VOCAL" + tab$ + "MOT" + tab$ + "N.MOT" + tab$ + "N.CONTEXT" + tab$ + "FONEMA_ANT." + tab$ + "FONEMA_POST." + tab$ + "SIMETRIA" + tab$ + "SÍL(tipus)" + tab$ + "SÍL(posició)" + tab$ + "ENT(tipus)" + tab$ + "ENT(posició)" + tab$ + "TEST" + tab$ + "DURADA(ms)" + tab$ + "INICI(s)" + tab$ + "FINAL(s)" + tab$ + "TOKEN" + tab$ + "INTERVAL" + tab$ + "TEMPS(s)" + tab$ + "F1" + tab$ + "F2" + tab$ + "F3" + tab$ + "LÍMIT(Hz)FORMANTS" + tab$ + "N.FORMANTS" + tab$ + "COMENTARIS" + tab$ + "DATA" + tab$ + "PRAAT" + tab$ + "OS" + newline$
	fileappend "'output_file$'" 'headerline$'
endproc

procedure pointsPerInterval
# CALCULATE HOW MANY FORMANT MEASUREMENTS PER INTERVAL
	if interval_measurement_option = 1
		.pts = 1
	elif interval_measurement_option = 2 or interval_measurement_option = 3 or interval_measurement_option = 4
		.pts = 3
	elif interval_measurement_option = 5
		.pts = 5
	elif interval_measurement_option = 6 or interval_measurement_option = 7
		.pts = 7
	elif interval_measurement_option = 8
		.pts = 9
	endif
endproc

procedure getMeasureTimes
	# MIDPOINT ONLY
	if interval_measurement_option = 1
		.time [1] = midpoint

	# ONSET-MIDPOINT-OFFSET
	elif interval_measurement_option = 2
		.time [1] = start
		.time [2] = midpoint
		.time [3] = end

	# 20-50-80
	elif interval_measurement_option = 3
		.time [1] = start + 0.2*(end-start)
		.time [2] = midpoint
		.time [3] = start + 0.8*(end-start)

	# 25-50-75
	elif interval_measurement_option = 4
		.time [1] = start + 0.25*(end-start)
		.time [2] = midpoint
		.time [3] = start + 0.75*(end-start)

	# 10-30-50-70-90
	elif interval_measurement_option = 5
		.time [1] = start + 0.1*(end-start)
		.time [2] = start + 0.3*(end-start)
		.time [3] = midpoint
		.time [4] = start + 0.7*(end-start)
		.time [5] = start + 0.9*(end-start)

	# 5-10-20-50-80-90-95
	elif interval_measurement_option = 6
		.time [1] = start + 0.05*(end-start)
		.time [2] = start + 0.1*(end-start)
		.time [3] = start + 0.2*(end-start)
		.time [4] = midpoint
		.time [5] = start + 0.8*(end-start)
		.time [6] = start + 0.9*(end-start)
		.time [7] = start + 0.95*(end-start)

	# 20%-30%-40%-50%-60%-70%-80%
    elif interval_measurement_option = 7
        .time [1] = start + 0.2*(end-start)
        .time [2] = start + 0.3*(end-start)
        .time [3] = start + 0.4*(end-start)
        .time [4] = midpoint
        .time [5] = start + 0.6*(end-start)
        .time [6] = start + 0.7*(end-start)
        .time [7] = start + 0.8*(end-start)

    # 10%-20%-30%-40%-50%-60%-70%-80%-90%
    elif interval_measurement_option = 8
        .time [1] = start + 0.1*(end-start)
        .time [2] = start + 0.2*(end-start)
        .time [3] = start + 0.3*(end-start)
        .time [4] = start + 0.4*(end-start)
        .time [5] = midpoint
        .time [6] = start + 0.6*(end-start)
        .time [7] = start + 0.7*(end-start)
        .time [8] = start + 0.8*(end-start)
        .time [9] = start + 0.9*(end-start)
	endif
endproc

procedure getFormants
	editor TextGrid 'filename$'
		for i from 1 to pointsPerInterval.pts
			Move cursor to... getMeasureTimes.time[i]
			.f4 [i] = Get fourth formant
			.f3 [i] = Get third formant
			.f2 [i] = Get second formant
			.f1 [i] = Get first formant
		endfor
	endeditor
endproc

procedure makeFormantTable
# NOTE: THE EXTRA SPACES ARE INTENTIONAL, TO GET EVERYTHING TO LINE UP PROPERLY IN COLUMNS IN THE PAUSE WINDOW
	# MIDPOINT ONLY
	if interval_measurement_option = 1
		.header$ = "'tab$''tab$'midpoint"
		.f3$ = "'tab$'F3'tab$' 'getFormants.f3[1]:0'"
		.f2$ = "'tab$'F2'tab$' 'getFormants.f2[1]:0'"
		.f1$ = "'tab$'F1'tab$' 'getFormants.f1[1]:0'"

	# ONSET-MIDPOINT-OFFSET
	elif interval_measurement_option = 2
		.header$ = "'tab$''tab$'onset'tab$' mid'tab$'offset"
		.f3$ = "'tab$'F3'tab$' 'getFormants.f3[1]:0''tab$' 'getFormants.f3[2]:0''tab$' 'getFormants.f3[3]:0'"
		.f2$ = "'tab$'F2'tab$' 'getFormants.f2[1]:0''tab$' 'getFormants.f2[2]:0''tab$' 'getFormants.f2[3]:0'"
		.f1$ = "'tab$'F1'tab$' 'getFormants.f1[1]:0''tab$' 'getFormants.f1[2]:0''tab$' 'getFormants.f1[3]:0'"

	# 20-50-80
	elif interval_measurement_option = 3
		.header$ = "'tab$''tab$' 20%'tab$' 50%'tab$' 80%"
		.f3$ = "'tab$'F3'tab$' 'getFormants.f3[1]:0''tab$' 'getFormants.f3[2]:0''tab$' 'getFormants.f3[3]:0'"
		.f2$ = "'tab$'F2'tab$' 'getFormants.f2[1]:0''tab$' 'getFormants.f2[2]:0''tab$' 'getFormants.f2[3]:0'"
		.f1$ = "'tab$'F1'tab$' 'getFormants.f1[1]:0''tab$' 'getFormants.f1[2]:0''tab$' 'getFormants.f1[3]:0'"

	# 25-50-75
	elif interval_measurement_option = 4
		.header$ = "'tab$''tab$' 25%'tab$' 50%'tab$' 75%"
		.f3$ = "'tab$'F3'tab$' 'getFormants.f3[1]:0''tab$' 'getFormants.f3[2]:0''tab$' 'getFormants.f3[3]:0'"
		.f2$ = "'tab$'F2'tab$' 'getFormants.f2[1]:0''tab$' 'getFormants.f2[2]:0''tab$' 'getFormants.f2[3]:0'"
		.f1$ = "'tab$'F1'tab$' 'getFormants.f1[1]:0''tab$' 'getFormants.f1[2]:0''tab$' 'getFormants.f1[3]:0'"

	# 10-30-50-70-90
	elif interval_measurement_option = 5
		.header$ = "'tab$''tab$' 10%'tab$' 30%'tab$' 50%'tab$' 70%'tab$' 90%"
		.f3$ = "'tab$'F3'tab$' 'getFormants.f3[1]:0''tab$' 'getFormants.f3[2]:0''tab$' 'getFormants.f3[3]:0''tab$' 'getFormants.f3[4]:0''tab$' 'getFormants.f3[5]:0'"
		.f2$ = "'tab$'F2'tab$' 'getFormants.f2[1]:0''tab$' 'getFormants.f2[2]:0''tab$' 'getFormants.f2[3]:0''tab$' 'getFormants.f2[4]:0''tab$' 'getFormants.f2[5]:0'"
		.f1$ = "'tab$'F1'tab$' 'getFormants.f1[1]:0''tab$' 'getFormants.f1[2]:0''tab$' 'getFormants.f1[3]:0''tab$' 'getFormants.f1[4]:0''tab$' 'getFormants.f1[5]:0'"

	# 5-10-20-50-80-90-95
	elif interval_measurement_option = 6
		.header$ = "'tab$''tab$' 5%'tab$''tab$' 10%'tab$' 20%'tab$' 50%'tab$' 80%'tab$' 90%'tab$' 95%"
		.f3$ = "'tab$'F3'tab$' 'getFormants.f3[1]:0''tab$' 'getFormants.f3[2]:0''tab$' 'getFormants.f3[3]:0''tab$' 'getFormants.f3[4]:0''tab$' 'getFormants.f3[5]:0''tab$' 'getFormants.f3[6]:0''tab$' 'getFormants.f3[7]:0'"
		.f2$ = "'tab$'F2'tab$' 'getFormants.f2[1]:0''tab$' 'getFormants.f2[2]:0''tab$' 'getFormants.f2[3]:0''tab$' 'getFormants.f2[4]:0''tab$' 'getFormants.f2[5]:0''tab$' 'getFormants.f2[6]:0''tab$' 'getFormants.f2[7]:0'"
		.f1$ = "'tab$'F1'tab$' 'getFormants.f1[1]:0''tab$' 'getFormants.f1[2]:0''tab$' 'getFormants.f1[3]:0''tab$' 'getFormants.f1[4]:0''tab$' 'getFormants.f1[5]:0''tab$' 'getFormants.f1[6]:0''tab$' 'getFormants.f1[7]:0'"

	elif interval_measurement_option = 7
		.header$ = "'tab$''tab$' 20%'tab$''tab$' 30%'tab$' 40%'tab$' 50%'tab$' 60% 'tab$' 70% 'tab$' 80%"
		.f4$ = "'tab$'F4'tab$' 'getFormants.f4[1]:0''tab$' 'getFormants.f4[2]:0''tab$' 'getFormants.f4[3]:0''tab$' 'getFormants.f4[4]:0''tab$' 'getFormants.f4[5]:0''tab$' 'getFormants.f4[6]:0''tab$' 'getFormants.f4[7]:0'"
		.f3$ = "'tab$'F3'tab$' 'getFormants.f3[1]:0''tab$' 'getFormants.f3[2]:0''tab$' 'getFormants.f3[3]:0''tab$' 'getFormants.f3[4]:0''tab$' 'getFormants.f3[5]:0''tab$' 'getFormants.f3[6]:0''tab$' 'getFormants.f3[7]:0'"
		.f2$ = "'tab$'F2'tab$' 'getFormants.f2[1]:0''tab$' 'getFormants.f2[2]:0''tab$' 'getFormants.f2[3]:0''tab$' 'getFormants.f2[4]:0''tab$' 'getFormants.f2[5]:0''tab$' 'getFormants.f2[6]:0''tab$' 'getFormants.f2[7]:0'"
		.f1$ = "'tab$'F1'tab$' 'getFormants.f1[1]:0''tab$' 'getFormants.f1[2]:0''tab$' 'getFormants.f1[3]:0''tab$' 'getFormants.f1[4]:0''tab$' 'getFormants.f1[5]:0''tab$' 'getFormants.f1[6]:0''tab$' 'getFormants.f1[7]:0'"

	elif interval_measurement_option = 8
		.header$ = "'tab$''tab$''tab$' F1'tab$''tab$''tab$' F2'tab$''tab$''tab$''tab$' F3'tab$''tab$''tab$''tab$' F4"
		.p10$ = "'tab$'10%'tab$' 'getFormants.f1[1]:0''tab$''tab$''tab$' 'getFormants.f2[1]:0''tab$''tab$''tab$' 'getFormants.f3[1]:0''tab$''tab$''tab$' 'getFormants.f4[1]:0'"
		.p20$ = "'tab$'20%'tab$' 'getFormants.f1[2]:0''tab$''tab$''tab$' 'getFormants.f2[2]:0''tab$''tab$''tab$' 'getFormants.f3[2]:0''tab$''tab$''tab$' 'getFormants.f4[2]:0'"
		.p30$ = "'tab$'30%'tab$' 'getFormants.f1[3]:0''tab$''tab$''tab$' 'getFormants.f2[3]:0''tab$''tab$''tab$' 'getFormants.f3[3]:0''tab$''tab$''tab$' 'getFormants.f4[3]:0'"
		.p40$ = "'tab$'40%'tab$' 'getFormants.f1[4]:0''tab$''tab$''tab$' 'getFormants.f2[4]:0''tab$''tab$''tab$' 'getFormants.f3[4]:0''tab$''tab$''tab$' 'getFormants.f4[4]:0'"
		.p50$ = "'tab$'50%'tab$' 'getFormants.f1[5]:0''tab$''tab$''tab$' 'getFormants.f2[5]:0''tab$''tab$''tab$' 'getFormants.f3[5]:0''tab$''tab$''tab$' 'getFormants.f4[5]:0'"
		.p60$ = "'tab$'60%'tab$' 'getFormants.f1[6]:0''tab$''tab$''tab$' 'getFormants.f2[6]:0''tab$''tab$''tab$' 'getFormants.f3[6]:0''tab$''tab$''tab$' 'getFormants.f4[6]:0'"
		.p70$ = "'tab$'70%'tab$' 'getFormants.f1[7]:0''tab$''tab$''tab$' 'getFormants.f2[7]:0''tab$''tab$''tab$' 'getFormants.f3[7]:0''tab$''tab$''tab$' 'getFormants.f4[7]:0'"
		.p80$ = "'tab$'80%'tab$' 'getFormants.f1[8]:0''tab$''tab$''tab$' 'getFormants.f2[8]:0''tab$''tab$''tab$' 'getFormants.f3[8]:0''tab$''tab$''tab$' 'getFormants.f4[8]:0'"
		.p90$ = "'tab$'90%'tab$' 'getFormants.f1[9]:0''tab$''tab$''tab$' 'getFormants.f2[9]:0''tab$''tab$''tab$' 'getFormants.f3[9]:0''tab$''tab$''tab$' 'getFormants.f4[9]:0'"
	endif
endproc
