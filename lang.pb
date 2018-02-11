; Original code by freak
;
; Link: http://www.purebasic.fr/english/viewtopic.php?f=12&t=26729
;
; A custom implementationof this code will be added in the utils later.

Global NbLanguageGroups, NbLanguageStrings

Structure LanguageGroup
	Name$
	GroupStart.l
	GroupEnd.l
	IndexTable.l[256]
EndStructure

Procedure LoadLanguage(FileName$ = "")
	NbLanguageGroups = 0
	NbLanguageStrings = 0
	
	Restore Language
	Repeat
		Read.s Name$
		Read.s String$
		
		Name$ = UCase(Name$)
		
		If Name$ = "_GROUP_"
			NbLanguageGroups + 1
		ElseIf Name$ = "_END_"
			Break
		Else
			NbLanguageStrings + 1
		EndIf
	ForEver
	
	Global Dim LanguageGroups.LanguageGroup(NbLanguageGroups)  ; all one based here
	Global Dim LanguageStrings.s(NbLanguageStrings)
	Global Dim LanguageNames.s(NbLanguageStrings)
	
	Group = 0
	StringIndex = 0  
	
	Restore Language
	Repeat
		Read.s Name$
		Read.s String$
		
		Name$ = UCase(Name$)
		
		If Name$ = "_GROUP_"
			LanguageGroups(Group)\GroupEnd   = StringIndex
			Group + 1
			LanguageGroups(Group)\Name$      = UCase(String$)
			LanguageGroups(Group)\GroupStart = StringIndex + 1
			For i = 0 To 255
				LanguageGroups(Group)\IndexTable[i] = 0
			Next i
		ElseIf Name$ = "_END_"
			Break
		Else
			StringIndex + 1
			LanguageNames(StringIndex)   = Name$ + Chr(1) + String$
		EndIf
	ForEver
	
	LanguageGroups(Group)\GroupEnd   = StringIndex
	
	For Group = 1 To NbLanguageGroups
		If LanguageGroups(Group)\GroupStart <= LanguageGroups(Group)\GroupEnd
			
			SortArray(LanguageNames(), 0, LanguageGroups(Group)\GroupStart, LanguageGroups(Group)\GroupEnd)
			
			char = 0
			For StringIndex = LanguageGroups(Group)\GroupStart To LanguageGroups(Group)\GroupEnd
				LanguageStrings(StringIndex) = StringField(LanguageNames(StringIndex), 2, Chr(1))
				LanguageNames(StringIndex)   = StringField(LanguageNames(StringIndex), 1, Chr(1))
				
				If Asc(Left(LanguageNames(StringIndex), 1)) <> char
					char = Asc(Left(LanguageNames(StringIndex), 1))
					LanguageGroups(Group)\IndexTable[char] = StringIndex
				EndIf
			Next StringIndex
		EndIf
	Next Group
	
	; Now try to load an external language file
	;       
	If FileName$ <> ""
		If OpenPreferences(FileName$)
			For Group = 1 To NbLanguageGroups
				If LanguageGroups(Group)\GroupStart <= LanguageGroups(Group)\GroupEnd
					PreferenceGroup(LanguageGroups(Group)\Name$)
					
					For StringIndex = LanguageGroups(Group)\GroupStart To LanguageGroups(Group)\GroupEnd
						LanguageStrings(StringIndex) = ReadPreferenceString(LanguageNames(StringIndex), LanguageStrings(StringIndex))
					Next StringIndex
				EndIf
			Next Group
			ClosePreferences()
			
			ProcedureReturn #True
		EndIf
	EndIf
	
	ProcedureReturn #True
EndProcedure

Procedure.s Language(Group$, Name$)
	Static Group.l
	Protected String$, StringIndex, Result
	
	Group$  = UCase(Group$)
	Name$   = UCase(Name$)
	String$ = "##### String not found! #####"
	
	If LanguageGroups(Group)\Name$ <> Group$
		For Group = 1 To NbLanguageGroups
			If Group$ = LanguageGroups(Group)\Name$
				Break
			EndIf
		Next Group
		
		If Group > NbLanguageGroups
			Group = 0
		EndIf
	EndIf
	
	If Group <> 0
		StringIndex = LanguageGroups(Group)\IndexTable[ Asc(Left(Name$, 1)) ]
		If StringIndex <> 0
			Repeat
				Result = CompareMemoryString(@Name$, @LanguageNames(StringIndex))
				
				If Result = 0
					String$ = LanguageStrings(StringIndex)
					Break
				ElseIf Result = -1
					Break
				EndIf
				
				StringIndex + 1
			Until StringIndex > LanguageGroups(Group)\GroupEnd
		EndIf
	EndIf
	
	ProcedureReturn String$
EndProcedure

; IDE Options = PureBasic 5.60 (Windows - x86)
; CursorPosition = 4
; Folding = -
; EnableXP
; CompileSourceDirectory
; EnableCompileCount = 0
; EnableBuildCount = 0