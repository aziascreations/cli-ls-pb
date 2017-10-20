;
;- Setup
;

If Not OpenConsole()
	;TODO: Play error sound ?
	End 1
EndIf

XIncludeFile "cli-args-pb\cli-args.pb"

;TODO: Fix the desc. for a and A.
RegisterCompleteOption('a', "all", "Do not ignore hidden entries and/or entries starting with .", #ARG_VALUE_NONE)
RegisterCompleteOption('A', "almost-all", "Do not ignore hidden entries and do not list implied . and ..", #ARG_VALUE_NONE)
RegisterLongOption("debug", "Display some debugging informations and exit", #ARG_VALUE_NONE)
RegisterShortOption('d', "Sorts directory content by putting the folders on top/at the beginning", #ARG_VALUE_NONE)
RegisterShortOption('D', "Sorts directory content by putting the folders at the bottom/at the end", #ARG_VALUE_NONE)
RegisterCompleteOption('F', "classify", "Append indicator (one of */=>@|) to entries", #ARG_VALUE_NONE)
RegisterCompleteOption('h', "human-readable", "With -l and/or -s, print human readable sizes (e.g., 1kB 234MB 2GB)", #ARG_VALUE_NONE)
RegisterCompleteOption('k', "kibibytes", "With -l and/or -s, default to 1024-byte blocks for disk usage and print ", #ARG_VALUE_NONE)

RegisterCompleteOption('l', "list", "Use a long listing format", #ARG_VALUE_NONE)
RegisterCompleteOption('m', "comma", "Fill width with a comma separated list of entries", #ARG_VALUE_NONE)
RegisterCompleteOption('p', "indicator-style", "Unfinished: Append / indicator to directories", #ARG_VALUE_NONE)
RegisterCompleteOption('R', "recursive", "List subdirectories recursively", #ARG_VALUE_NONE)

RegisterLongOption("show-control-chars", "TEMP: Used to prevent some aliases errors.", #ARG_VALUE_NONE)
;RegisterLongOption("","", #ARG_VALUE_NONE)
RegisterLongOption("help", "Display this help and exit", #ARG_VALUE_NONE)
RegisterLongOption("version", "Output version information and exit", #ARG_VALUE_NONE)

;
;- Variables Setup
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

; The #PB_DirectoryEntry constants will be used to set this variable
Global DirectorySortingMode.b = %00; -d/-D flag

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
;- Procedures
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
	; Unable to find a way to get the terminal size.
	PrintN("Please use the -l option")
EndProcedure

Procedure PrintPrettyDirectory(Path.s, CurrentDepth.i, List Entries.DirEntry())
	;
	; Printing current path if recursive mode is enabled
	; La condition après le AND est peut-être inutile
	If CurrentDepth > 0 ;And MaxRecursionDepth > 0
		PrintN("")
		ConsoleColor(3, 0)
		PrintN(Path+":")	
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
			Print(Space(MaxSizeLength - Len(GetFormattedSize(Entries()\Size)) + #SectionSpacing))
			Print(GetFormattedSize(Entries()\Size))
		Else
			Print(Space(MaxSizeLength - Len(Str(Entries()\Size)) + #SectionSpacing))
			Print(Str(Entries()\Size))
		EndIf
		
		; Modification date section
		Print(Space(#SectionSpacing))
		Print(Mid(#MonthStr, (Month(Entries()\Date)-1)*3+1, 3)+" "+FormatDate("%dd", Entries()\Date))
		Print(Space(#SectionSpacing) + FormatDate("%hh:%mm", Entries()\Date))
		
		;File name section
		Print(Space(#SectionSpacing))
		PrintFormattedName(Entries()\Name, Entries()\Extension, Entries()\Type)
	Next
EndProcedure

;
;- Others
;

Procedure ProcessDirectory(Path.s, CurrentDepth.i=0)
	Debug "Processing "+Path+" at depth "+CurrentDepth
	If CurrentDepth > MaxRecursionDepth
		ProcedureReturn
	EndIf
	
	Define *ListPointer.DirEntry
	
	; Used to store files or directories if -d or -D is used to add it to Entries() later.
	NewList AltEntries.DirEntry()
	NewList Entries.DirEntry()
	
	CurrentId.i = StepCounter
	StepCounter = StepCounter + 1
	
	; TODO: Might be useless if formatted names are used  with basic printing...
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
			
			; If -d is used and the entry is a directory or if -D is used and the entry is a file, Entries() will be used.
			; If neither -d or -D was used, will always be used 
			If Not DirectorySortingMode Or (DirectorySortingMode & #PB_DirectoryEntry_Directory And DirectoryEntryType(CurrentId) & #PB_DirectoryEntry_Directory) Or (DirectorySortingMode & #PB_DirectoryEntry_File And DirectoryEntryType(CurrentId) & #PB_DirectoryEntry_File)
				AddElement(Entries())
				*ListPointer = @Entries()
			Else
				AddElement(AltEntries())
				*ListPointer = @AltEntries()
			EndIf
			
			*ListPointer\Name = DirectoryEntryName(CurrentId)
			*ListPointer\Type = DirectoryEntryType(CurrentId)
			*ListPointer\Flags = DirectoryEntryAttributes(CurrentId)
			*ListPointer\Size = DirectoryEntrySize(CurrentId)
			*ListPointer\Date = DirectoryEntryDate(CurrentId, #PB_Date_Modified)
			
			; TODO: use the GetExtensionPart() procedure on directories and ignore it later when not needed.
			If *ListPointer\Type & #PB_DirectoryEntry_Directory
				*ListPointer\Extension = ""
			Else
				*ListPointer\Extension = GetExtensionPart(*ListPointer\Name)
			EndIf
			
			If Len(*ListPointer\Name) > LongestName
				LongestName = Len(*ListPointer\Name)
			EndIf
		Wend
		
		;Debug "Finished examining..."
		FinishDirectory(CurrentId)
		
		; Sorting directory's entries
		If ListSize(AltEntries()) > 0
			Debug "Combining lists"
			ForEach AltEntries()
				AddElement(Entries())
				Entries() = AltEntries()
			Next
		EndIf
		FreeList(AltEntries())
		
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
	
	FreeList(Entries())
EndProcedure

;
;- IDK 
;

Debug "Parsing arguments..."
ParseArguments(#ARG_PREFIX_UNIX)
Debug "Arguments parsed !"

Debug "Reading arguments"
If IsOptionUsed("help")
	PrintHelpText()
	End
ElseIf IsOptionUsed("version")
	PrintN("cli-ls v"+#PB_Editor_FileVersionNumeric+" x64 - ("+FormatDate("%dd/%mm/%yyyy %hh:%ii:%ss GMT", #PB_Compiler_Date)+")")
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

If IsOptionUsed("d")
	DirectorySortingMode = #PB_DirectoryEntry_Directory
ElseIf IsOptionUsed("D")
	DirectorySortingMode = #PB_DirectoryEntry_File
EndIf

If IsOptionUsed("R")
	; 500 should be enough with the 260 path length limit on windows but I let it at 9999 to be safe in the future.
	MaxRecursionDepth = 9999
EndIf
Debug "Arguments read !"

; This part will be revamped when non-flags arguments will be supported in cli-args
; cli-args shits out a usage error if a / is used inside the path.
; If CountProgramParameters() > 0 And Left(ProgramParameter(CountProgramParameters()-1), 1) <> "-"
; 	Path.s = ProgramParameter(CountProgramParameters()-1)
; 	
; 	If CreateRegularExpression(1, "^[A-Za-z]:[\\|\/].*")
; 		Debug "Full path regex created at 1"
; 	Else
; 		PrintN("Unable to create regex for path checking.")
; 		End 1
; 	EndIf
; 	
; 	If Not MatchRegularExpression(1, Path)
; 		Path = ".\"+Path
; 	EndIf
; 	
; 	Path = ReplaceString(Path, "/", "\")
; 	If Right(Path, 1) <> "\"
; 		Path = Path+"\"
; 	EndIf
; 	
; 	ProcessDirectory(Path, 0)
; Else
; 	ProcessDirectory(".\", 0)
; EndIf

If ListSize(TextArgs())
	PassNumber.b = 0
	
	ForEach TextArgs()
		Path.s = TextArgs()
		
		If CreateRegularExpression(1, "^[A-Za-z]:[\\|\/].*")
			Debug "Full path regex created at 1"
		Else
			PrintN("Unable to create regex for path checking.")
			End 1
		EndIf
		
		If Not MatchRegularExpression(1, Path)
			Path = ".\"+Path
		EndIf
		
		Path = ReplaceString(Path, "/", "\")
		If Right(Path, 1) <> "\"
			Path = Path+"\"
		EndIf
		
		; Couldn't find the right constant for newlines
		If PassNumber
			PrintN("")
			PrintN("")
		EndIf
		
		ProcessDirectory(Path)
		
		PassNumber = PassNumber +1
	Next
Else
	ProcessDirectory(".\")
EndIf


; IDE Options = PureBasic 5.50 (Windows - x64)
; CursorPosition = 461
; FirstLine = 442
; Folding = -
; EnableXP