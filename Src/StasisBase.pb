; *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*
; StasisVM's basic definitions.
; Developed in 2009 by Chrono Syndrome.
; *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*

Enumeration ; OpCodes
#iNOP ; Filler byte.
;{ [Fully mutable instructions]
Macro FM_Codes(X) ; Definitions block.
; -Simple arithmetics-
#iAdd#X : #iSub#X : #iMul#X : #iDiv#X : #iNeg#X
#iInc#X : #iDec#X : #iAbs#X : #iSgn#X
; -Comparsion-
#iLess#X : #IGreater#X : #iLoE#X : #iGoE#X
; -Literals-
#iFalse#X : #iTrue#X : #iNegOne#X : #iTwo#X
; -Conversion-
#iB2#X : #iW2#X : #iC2#X : #iL2#X : #iQ2#X : #iI2#X
#iF2#X : #iD2#X : #iToBool#X
EndMacro
; Definitions for each type...
FM_Codes(B) : FM_Codes(W) : FM_Codes(C) : FM_Codes(L) : FM_Codes(Q) : FM_Codes(I)
FM_Codes(F) : FM_Codes(D);}
;{ [Integer-mutable instructions]
Macro IM_Codes(X) ; Definitions block.
#iLit#X ; Literal.
#iMod#X ; Arithmetic.
; -Stack management-
#iDup#X : #iOver#X : #iDrop#X : #iSwap#X : #iNip#X
#iRot#X : #iRRot#X
; -Memory management-
#iRead#X : #iWrite#X
; -Comparsion-
#iEqu#X : #iInequ#X : #iSearch#X
; -Binary logic-
#iAnd#X : #iOr#X : #iXOr#X : #iNot#X : #iShl#X : #iShr#X
#iToBin#X : #iTo2Bin#X
EndMacro
; Definitions for each type...
IM_Codes(B) : IM_Codes(W) : IM_Codes(C) : IM_Codes(L) : IM_Codes(Q) : IM_Codes(I) ;}
;{ [Float-only instructions]
Macro FO_Codes(X) ; Definitions block.
; -Complex arithmetics-
#iLog#X : #iLog10#X : #iSqr#X : #iPow#X : #iRound#X
; -Trigonometry-
#iACos#X : #iASin#X : #iATan#X
#iCos#X : #iSin#X : #iTan#X
EndMacro
; Definitions for each type...
FO_Codes(F) : FO_Codes(D) ;}
;{ [Non-mutable instructions]
; Pointer literal.
#iAbsPtr : #iRelPtr : #iDataPtr
#iLitS : #iLitSPtr : #iLitIPtr
; -Stack management-
#iDupS : #iDropS
; -Flow control-
#iHere : #iJmp : #iIF : #iIFZ
; -Calls management-
#iCall : #iRet : #iPushRS : #iPopRS
; -Memory management-
#iAllot : #iFreeMem
#iPushDS : #iPopDS
#iPushSS : #iPopSS
; -Depth retrievment-
#iDepth : #iRDepth : #iGDepth
#iPtrSize : #iCharSize
; -Threading-
#iNewThread : #iBye : #iKill : #iSleep
#iThreadz : #iBase
#iPushGS : #iPopGS
; -Mutexes-
#iMutexState : #iLockMutex : #iUnLockMutex
#iNewMutex : #iFreeMutex 
; -System calls-
#iSysCall : #iSysCallPtr
#iPushSC : #iPopSC : #iFindSC ;}
; == Finisher ==
#InstructionsCount = #PB_Compiler_EnumerationValue
EndEnumeration

Structure HeaderData
StructureUnion
*CodeStart  ; Pointer to start of code segment.
Signature.Q ; Signature byte sequence.
EndStructureUnion
BitFlags.b    ; Sequence of binary flags.
CodeSize.i    ; Size of code segment (to load).
DataSize.i    ; Size of virtual data space.
ImportTable.i ; Size of import table segment.
ExportTable.i ; Size of export table segment.
*EntryPoint   ; IP's offset on start.
EndStructure
; IDE Options = PureBasic 4.50 (Windows - x86)
; Folding = w
; EnableXP