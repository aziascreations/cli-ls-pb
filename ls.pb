;
;-- Setup --
;

; TODO: Le PrintHelpText() a une longue pause et ne quitte pas.

OpenConsole()

XIncludeFile "cli-args-pb\cli-args.pb"

;TODO: Fix the desc. for a and A.
RegisterCompleteOption('a', "all", "Do not ignore hidden entries and/or entries starting with .", #ARGV_NONE)
RegisterCompleteOption('A', "almost-all", "Do not ignore hidden entries and do not list implied . and ..", #ARGV_NONE)
RegisterCompleteOption('d', "debug", "Display some debugging informations and exit", #ARGV_NONE)
RegisterCompleteOption('F', "classify", "Append indicator (one of */=>@|) to entries", #ARGV_NONE)
RegisterCompleteOption('h', "human-readable", "With -l and/or -s, print human readable sizes (e.g., 1kB 234MB 2GB)", #ARGV_NONE)
RegisterCompleteOption('k', "kibibytes", "With -l and/or -s, default to 1024-byte blocks for disk usage and print ", #ARGV_NONE)

RegisterCompleteOption('l', "list", "Use a long listing format", #ARGV_NONE)
RegisterCompleteOption('m', "comma", "Fill width with a comma separated list of entries", #ARGV_NONE)
RegisterCompleteOption('p', "indicator-style", "Unfinished: Append / indicator to directories", #ARGV_NONE)
RegisterCompleteOption('R', "recursive", "List subdirectories recursively", #ARGV_NONE)

RegisterLongOption("show-control-chars", "TEMP: Used to prevent some aliases errors.", #ARGV_NONE)
;RegisterLongOption("","", #ARGV_NONE)
RegisterLongOption("help", "Display this help and exit", #ARGV_NONE)
RegisterLongOption("version", "Output version information and exit", #ARGV_NONE)


;
;-- Variables Setup --
;

; Constants
#SectionSpacing = 2
#MonthStr = "JanFebMarAprMayJunJulAugSepOctNovDec"
#ExecRegex = "^(bat|c(md|om|pl)|exe|gadget|i(ns|ns|su)|j(ob|se)|ms[cipt]|p(if|af|s1)|r(eg|gs)|s(ct|hh|hs)|u3p|vb(e|s(cript)?)?|wsf?)$"
#FileSizeUnitBasic = "B  KB MB GB TB PB EB ZB YB "
#FileSizeUnitKibib = "B  KiBMiBGiBTiBPiBEiBZiBYiB"

; Flag variables
; 0 if recursion is disabled use another value for testing to avoid the .git folder and subfolders in reursive mode.
Global MaxRecursionDepth.i = 0 ; -R option switch
Global UseHumanFileSize.b = #False ; -h option switch
Global AddExtraFileTypeStuff = #False ; -F option switch

; 0 -> Not hidden or .*
; 1(A) -> Not hidden
; 2(a) -> All
Global FileHiddingMode.b = 0 ; -a and -A thingy
Global DebugMode.b = #False	 ; -d/--debug thingy
; 0 - Simple list(default)
; 1 - Commas separated list
; 2 - Pretty / list
Global DirectoryDisplayMode.b = 0; -c/-l thingy

; Couldn't find a better way to do it...
Global UseKibibytes.b = #False
Global FileSizeDivider.i = 1000


; Other variables & stuff
Global StepCounter.i = 12

Structure DirEntry
	Name.s
	Extension.s
	Flags.b
	Type.b
	Size.i
	Date.i
EndStructure

Debug "Setting up regexes"
If CreateRegularExpression(0, #ExecRegex)
	Debug "Regex is love, regex is life"
Else
	PrintN("A fatal error occured while setting up stuff, now exiting...")
	Debug RegularExpressionError()
	PrintN(RegularExpressionError())
	End 1
EndIf


;
;-- Procedures --
;
;---- Name Printers ----
;
Procedure.s GetFormattedSize(FileSize.i)
	UnitSizeIndex.b = 0
	;FileSizeDivider
	
	While FileSize >= FileSizeDivider
		UnitSizeIndex = UnitSizeIndex + 1
		FileSize = FileSize / FileSizeDivider
	Wend
	
	If UseKibibytes
		ProcedureReturn Str(FileSize) + " " + RTrim(Mid(#FileSizeUnitKibib, UnitSizeIndex*3+1, 3))
	Else
		ProcedureReturn Str(FileSize) + " " + RTrim(Mid(#FileSizeUnitBasic, UnitSizeIndex*3+1, 3))
	EndIf
	
	ProcedureReturn "ERROR"
EndProcedure

Procedure PrintFormattedName(FileName.s, Extension.s, FileType.i, Spacing.i=-1)
	If Spacing > 0
		; Add spaces and shit
	EndIf
	
	Debug "Ext: "+Extension
	
	If FileType & #PB_DirectoryEntry_Directory
		ConsoleColor(3,0)
		Debug "Is a folder"
	ElseIf AddExtraFileTypeStuff
		Debug "Is something else..."
		If MatchRegularExpression(0, Extension)
			ConsoleColor(6,0)
			Debug "Is a executable"
		EndIf
		; Choose with extension
		
	EndIf
	
	Print(FileName)
	
	ConsoleColor(15,0)
	If AddExtraFileTypeStuff
		If FileType & #PB_DirectoryEntry_Directory
			Print("/")
		ElseIf MatchRegularExpression(0, Extension)
			Print("*")
		EndIf
	EndIf
	
	If Spacing > 0
		; Add spaces and shit
		
	Else
		PrintN("")
	EndIf
EndProcedure

Procedure PrintBasicDirectory(Path.s, CurrentDepth.i, List Entries.DirEntry(), LongestName.i)
	
EndProcedure

Procedure PrintPrettyDirectory(Path.s, CurrentDepth.i, List Entries.DirEntry())
	;Debug "Pretty printing start"
	
	; Printing current path if recursive mode is enabled
	; La condition après le AND est peut-être inutile
	If MaxRecursionDepth > 0 And CurrentDepth > 0
		PrintN("")
		ConsoleColor(3, 0)
		PrintN(""+Path+":")	
		ConsoleColor(15, 0)
	EndIf
	
	; Calculating size section width
	MaxSizeLength.b = 0
	
	ForEach Entries()
		If UseHumanFileSize
			If Len(GetFormattedSize(Entries()\Size)) > MaxSizeLength
				MaxSizeLength = Len(StrU(Entries()\Size))
			EndIf
		Else
			If Len(StrU(Entries()\Size)) > MaxSizeLength
				MaxSizeLength = Len(StrU(Entries()\Size))
			EndIf
		EndIf
	Next
	
	;Debug "Max: "+MaxSizeLength
	
	; This is a temporary solution, I will clean this up later
	ForEach Entries()
		; Flags section
		If Entries()\Type & #PB_DirectoryEntry_Directory
			Print(" d")
		Else
			Print(" -")
		EndIf
		
		If Entries()\Flags & #PB_FileSystem_Archive
			Print("a")
		Else
			Print("-")
		EndIf
		If Entries()\Flags & #PB_FileSystem_Hidden
			Print("h")
		Else
			Print("-")
		EndIf
		If Entries()\Flags & #PB_FileSystem_ReadOnly
			Print("r")
		Else
			Print("-")
		EndIf
		If Entries()\Flags & #PB_FileSystem_System
			Print("s")
		Else
			Print("-")
		EndIf
		
		; TODO: Seems to make a spacing of 1 between the flag and size section if kibibytes are used
		; Size Section
		If UseHumanFileSize
			For i=1 To MaxSizeLength - Len(GetFormattedSize(Entries()\Size)) + #SectionSpacing
				Print(" ")
			Next
			Print(GetFormattedSize(Entries()\Size))
		Else
			For i=1 To MaxSizeLength - Len(Str(Entries()\Size)) + #SectionSpacing
				Print(" ")
			Next
			Print(Str(Entries()\Size))
		EndIf
		
		For i=1 To #SectionSpacing
			Print(" ")
		Next
		
		; Modification date section
		Print(Mid(#MonthStr, (Month(Entries()\Date)-1)*3+1, 3)+" "+FormatDate("%dd", Entries()\Date))
		For i=1 To #SectionSpacing
			Print(" ")
		Next
		Print(FormatDate("%hh:%mm", Entries()\Date))
		
		;File name section
		For i=1 To #SectionSpacing
			Print(" ")
		Next
		PrintFormattedName(Entries()\Name, Entries()\Extension, Entries()\Type)
	Next
	
	;Debug "Pretty printing end"
EndProcedure

;
;---- Others ----
;

Procedure ProcessDirectory(Path.s, CurrentDepth.i)
	Debug "Processing "+Path+" at depth "+CurrentDepth
	If CurrentDepth > MaxRecursionDepth
		ProcedureReturn
	EndIf
	
	;Debug "Preparing variables"
	NewList Entries.DirEntry()
	CurrentId.i = StepCounter
	StepCounter = StepCounter + 1
	LongestName.i = 0
	
	Debug "Current ID: "+CurrentId
	If ExamineDirectory(CurrentId, Path, "*")
		Debug "Examining "+Path
		While NextDirectoryEntry(CurrentId)
			
			; Ignoring . and ..
			; TODO: check if -a or -A can show them.
			If DirectoryEntryName(CurrentId) = "." Or DirectoryEntryName(CurrentId) = ".."
				Continue
			EndIf
			
			If FileHiddingMode <= 1
				; -A -> Everything that doesn't have the "h" flag
				; Skip h ones
				If DirectoryEntryAttributes(CurrentId) & #PB_FileSystem_Hidden
					Continue
				EndIf
				
				If FileHiddingMode < 1
					; Skips the "h" flag and entries that starts with "."
					; -> Skips . ones
					If Left(DirectoryEntryName(CurrentId), 1) = "."
						Continue
					EndIf
				EndIf
			EndIf
			
			AddElement(Entries())
			Entries()\Name = DirectoryEntryName(CurrentId)
			Entries()\Type = DirectoryEntryType(CurrentId)
			Entries()\Flags = DirectoryEntryAttributes(CurrentId)
			Entries()\Size = DirectoryEntrySize(CurrentId)
			Entries()\Date = DirectoryEntryDate(CurrentId, #PB_Date_Modified)
			
			If Entries()\Type & #PB_DirectoryEntry_Directory
				Entries()\Extension = ""
			Else
				Entries()\Extension = GetExtensionPart(Entries()\Name)
			EndIf
			
			If Len(Entries()\Name) > LongestName
				LongestName = Len(Entries()\Name)
			EndIf
		Wend
		;Debug "Finished examining..."
		FinishDirectory(CurrentId)
		
		; Printing directory's content
		If DirectoryDisplayMode = 2
			PrintPrettyDirectory(Path, CurrentDepth, Entries())
		Else
			PrintBasicDirectory(Path, CurrentDepth, Entries(), LongestName)
		EndIf
		
		; Recursive walk check
		;Debug "Recursive walk check"
		If MaxRecursionDepth > 0
			ForEach Entries()
				If Entries()\Type & #PB_DirectoryEntry_Directory
					ProcessDirectory(Path+Entries()\Name+"\", CurrentDepth+1)
				EndIf
				Debug Entries()\Name
			Next
		EndIf
		
	Else
		Debug "Unable to examine "+Path
		If CurrentDepth = 0
			PrintN("Unable to walk trough "+Path)
			PrintN("Make sure that you entered the path correctly.")
			;TODO: Exit
		EndIf
	EndIf
	
	;Debug "Freeing list..."
	FreeList(Entries())
	Debug "Done"
EndProcedure

;
;-- IDK --
;

Debug "Parsing arguments..."
ParseArguments(#ARG_UNIX)
Debug "Arguments parsed !"

Debug "Reading arguments"
If IsOptionUsed("help")
	PrintHelpText()
	End
EndIf

; File hidding stuff
If IsOptionUsed("a")
	FileHiddingMode = 2
ElseIf IsOptionUsed("A")
	FileHiddingMode = 1
EndIf

; Debug Mode -> Why ?
If IsOptionUsed("d")
	DebugMode = #True
EndIf

; color and stuff
If IsOptionUsed("F")
	AddExtraFileTypeStuff = #True
EndIf

; Size related parameters
If IsOptionUsed("h")
	UseHumanFileSize = #True
EndIf
If IsOptionUsed("k")
	UseKibibytes = #True
	FileSizeDivider = 1024
EndIf

If IsOptionUsed("l")
	DirectoryDisplayMode = 2
ElseIf IsOptionUsed("m")
	DirectoryDisplayMode = 1
EndIf

If IsOptionUsed("R")
	MaxRecursionDepth = 9999
EndIf
Debug "Arguments read !"

ProcessDirectory(".\", 0)

;Debug "Waiting for user input"
;name$=Input()

; IDE Options = PureBasic 5.50 (Windows - x64)
; CursorPosition = 385
; FirstLine = 366
; Folding = -
; EnableXP