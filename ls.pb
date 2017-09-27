OpenConsole()
PrintN("Purebasic ls version") ; Temp

;TODO: flags vars here.


; 0 if recursion is disabled
Global MaxRecursionDepth.i = 5 ; -R option switch

Global UsePrettyPrint.b = #True ; -l option switch
Global UseHumanFileSize.b = #True ; -h option switch

Global AddExtraFileTypeStuff = #True ; -F option switch

; Could be constants
#SectionSpacing = 2
#MonthStr = "JanFebMarAprMayJunJulAugSepOctNovDec"

; Wasn't able to use #PB_ANY for the CurrentId variable in ProcessDirectory(...).
; This should prevent any duplicate ids.
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
If CreateRegularExpression(0, "^(bat|c(md|om|pl)|exe|gadget|i(ns|ns|su)|j(ob|se)|ms[cipt]|p(if|af|s1)|r(eg|gs)|s(ct|hh|hs)|u3p|vb(e|s(cript)?)?|wsf?)$")
	Debug "Regex is love, regex is life"
Else
	PrintN("A fatal error occured while setting up stuff, now exiting...")
	
	Debug RegularExpressionError()
	PrintN(RegularExpressionError())
	
	End 1
EndIf


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
		; TODO: Check length with -F option (KB, KiB, ...)
		If Len(StrU(Entries()\Size)) > MaxSizeLength
			MaxSizeLength = Len(StrU(Entries()\Size))
			; Debug Entries()\Name + " -> " + Entries()\Size + " -> "+ Str(Entries()\Size)
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
		
		; Size Section
		For i=1 To MaxSizeLength - Len(Str(Entries()\Size)) + #SectionSpacing
			Print(" ")
		Next
		Print(Str(Entries()\Size))
		
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
		If UsePrettyPrint
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

Procedure ParseArguments()
	
EndProcedure

ProcessDirectory(".\", 0)

Debug "Waiting for user input"
name$=Input()

; IDE Options = PureBasic 5.50 (Windows - x64)
; CursorPosition = 19
; Folding = v
; EnableXP