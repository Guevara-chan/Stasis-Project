; *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*
; 'Stasis' Virtual Machine v0.095 (Alpha)
; Developed in 2009 by Guevara-chan.
; *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*

;{ =[TO.DO]=
; Вынести ядро в .DLL
; Реализовать многопоточность.
; Добавить загрузчик кода из исполняемого файла (EXE).
; Улучшить обработчики ошибок.
; Добавить поддержку Unicode.
; Доработать GUI.
; ...Избавиться от лишних операндов конверсии (iL2L f.e.)
; ...Добавить сборку мусора (мутексов и блоков памяти).
;} {End/TO.DO}

; --Preparations--
EnableExplicit
IncludeFile "StackLib.pb"
IncludeFile "StasisBase.pb"

;{ Definitions
;{ --Enumerations--
Enumeration 1 ; Exit codes
#cBye
#cKilled
#cInvalidOperand
#cInvalidCall
EndEnumeration

Enumeration ; Binary flags
#b64bitTarget
#bUnicodeTarget
EndEnumeration
;}
;{ --Constants--
#OpCodeSize = SizeOf(Word)
#HeaderSig = $5D5349534154535B ; [STASIS]
;}
;{ --Structures--
Structure SystemCall
Native.i ; Nativity flag.
*Address ; Destination address.
EndStructure

Structure UniPointer
StructureUnion
B.b : W.w : L.l : Q.q : F.f : D.d ; Fixed-size.
A.a : U.u : C.c : I.i ; Dynamical sized.
EndStructureUnion
EndStructure

Structure ThreadData
ExitCode.i
*SystemID ; Pointer to OS data for thread.
*Stack.StackData  ; Data stack.
*RStack.StackData ; Return stack.
*SStack.StackData ; Storage stack.
*IP.UniPointer ; Instruction pointer.
*CS ; Code segment.
*DS ; Data segment.
S.s ; Temporary string field.
StructureUnion ; Packing temporary data
; -Temporary values-
B.b : W.w : C.c : L.l : Q.q : F.f : D.d : I.i
; -Temporary pointers-
*UP.UniPointer : *Thread.ThreadData : *SC.SystemCall
EndStructureUnion
EndStructure

Structure VMData
*JMPTable[#InstructionsCount]
*GSMutex     ; Global stack's access mutex.
*SCMutex     ; System calls map access mutex.
*StackBase   ; Pointer to default stack level.
*GStack.StackData         ; Global stack.
Map SysCalls.SystemCall() ; System calls table.
List Threadz.ThreadData() ; List of threads.
EndStructure
;}
;{ --Varibales--
Global System.VMData
Global *ThisThread.ThreadData
;}
;} EndDefinitions

;{ Instruction table
; -Service macros-
Macro DefCode(OpCode)
System\JMPTable[#OpCode] = ?OpCode
If #False : OpCode:
EndMacro

Macro EndCode()
! JMP __Ret ; Спасибо Фреду за наше счастливое детство.
EndIf
EndMacro

; -Service procedures, shared with minimal calls set-
Macro DefSharedProcs(TT, X) ; Definitions.
Procedure Swap#X(A.X, B.X) ; Swaps top 2 values on stack.
Push#X(TT\Stack, B) : Push#X(TT\Stack, A)
EndProcedure
Procedure Over#X(A.X, B.X) ; Copy 2nd from top value of stack.
Push#X(TT\Stack, A) : Push#X(TT\Stack, B) : Push#X(TT\Stack, A)
EndProcedure
Procedure Rot#X(A.X, B.X, C.X) ; Rotate top 3 values on stack.
Push#X(TT\Stack, B) : Push#X(TT\Stack, C) : Push#X(TT\Stack, A)
EndProcedure
Procedure RRot#X(A.X, B.X, C.X) ; Rotate top 3 values on stack in reverse order.
Push#X(TT\Stack, C) : Push#X(TT\Stack, A) : Push#X(TT\Stack, B)
EndProcedure
EndMacro

; -OpCodes definitions-
;{ [Fully mutable instructions]
Macro FM_Defs(TT, X) ; Definitions block.
; -Simple arithmetics-
Procedure Search#X(Edge.i, Val.X) ; Search value in seqeunce.
If Edge => 0
While __StackDepth(TT\Stack) > Edge
If Pop#X(TT\Stack) = Val : SeekStack(TT\Stack, Edge) : ProcedureReturn #True : EndIf
Wend
EndIf
EndProcedure
DefCode(iAdd#X) : Push#X(TT\Stack, Pop#X(TT\Stack) + Pop#X(TT\Stack)) : EndCode()
DefCode(iSub#X) : Push#X(TT\Stack, -(Pop#X(TT\Stack) - Pop#X(TT\Stack))) : EndCode()
DefCode(iMul#X) : Push#X(TT\Stack, Pop#X(TT\Stack) * Pop#X(TT\Stack)) : EndCode()
DefCode(iDiv#X) : TT\X = Pop#X(TT\Stack) : Push#X(TT\Stack, Pop#X(TT\Stack) / TT\X) : EndCode()
DefCode(iInc#X) : Push#X(TT\Stack, Pop#X(TT\Stack) + 1) : EndCode()
DefCode(iDec#X) : Push#X(TT\Stack, Pop#X(TT\Stack) - 1) : EndCode()
DefCode(iAbs#X) : Push#X(TT\Stack, Abs(Pop#X(TT\Stack))) : EndCode()
DefCode(iNeg#X) : Push#X(TT\Stack, -Pop#X(TT\Stack)) : EndCode()
DefCode(iSgn#X) : Push#X(TT\Stack, Sign(Pop#X(TT\Stack))) : EndCode()
; -Comparsion-
DefCode(iLess#X)    : If Pop#X(TT\Stack) <= Pop#X(TT\Stack) : PushI(TT\Stack, #False)
Else : PushI(TT\Stack, #True) : EndIf : EndCode()
DefCode(iGreater#X) : If Pop#X(TT\Stack) >= Pop#X(TT\Stack) : PushI(TT\Stack, #False)
Else : PushI(TT\Stack, #True) : EndIf : EndCode()
DefCode(iLoE#X)     : If Pop#X(TT\Stack) < Pop#X(TT\Stack) : PushI(TT\Stack, #False)
Else : PushI(TT\Stack, #True) : EndIf : EndCode()
DefCode(iGoE#X)     : If Pop#X(TT\Stack) > Pop#X(TT\Stack) : PushI(TT\Stack, #False)
Else : PushI(TT\Stack, #True) : EndIf : EndCode()
; -Literals-
DefCode(iFalse#X)  : Push#X(TT\Stack, #False) : EndCode()
DefCode(iTrue#X)   : Push#X(TT\Stack, #True) : EndCode()
DefCode(iNegOne#X) : Push#X(TT\Stack, -1) : EndCode()
DefCode(iTwo#X)    : Push#X(TT\Stack, 2) : EndCode()
; -Conversion-
DefCode(iToBool#X) : If Pop#X(TT\Stack) : Push#X(TT\Stack, #True) 
Else : Push#X(TT\Stack, #False) : EndIf : EndCode()
DefCode(iB2#X) : Push#X(TT\Stack, PopB(TT\Stack)) : EndCode()
DefCode(iW2#X) : Push#X(TT\Stack, PopW(TT\Stack)) : EndCode()
DefCode(iC2#X) : Push#X(TT\Stack, PopC(TT\Stack)) : EndCode()
DefCode(iL2#X) : Push#X(TT\Stack, PopL(TT\Stack)) : EndCode()
DefCode(iI2#X) : Push#X(TT\Stack, PopI(TT\Stack)) : EndCode()
DefCode(iQ2#X) : Push#X(TT\Stack, PopQ(TT\Stack)) : EndCode()
DefCode(iF2#X) : Push#X(TT\Stack, PopF(TT\Stack)) : EndCode()
DefCode(iD2#X) : Push#X(TT\Stack, PopD(TT\Stack)) : EndCode()
EndMacro
; Definitions for each type.
FM_Defs(*ThisThread, B)
FM_Defs(*ThisThread, W)
FM_Defs(*ThisThread, C)
FM_Defs(*ThisThread, L)
FM_Defs(*ThisThread, Q)
FM_Defs(*ThisThread, I)
FM_Defs(*ThisThread, F)
FM_Defs(*ThisThread, D) ;}
;{ [Integer-mutable instructions]
Macro IM_Defs(TT, X, TypeName) ; Definitions block.
DefSharedProcs(TT, X)
DefCode(iLit#X) : Push#X(TT\Stack, TT\IP\X) : TT\IP + SizeOf(TypeName) : EndCode()
DefCode(iMod#X) : TT\X = Pop#X(TT\Stack) : Push#X(TT\Stack, Pop#X(TT\Stack) % TT\X) : EndCode()
; -Stack management-
DefCode(iDup#X)  : TT\X = Pop#X(TT\Stack) : TT\Stack\TOS + SizeOf(TypeName) : Push#X(TT\Stack, TT\X)
EndCode()
DefCode(iDrop#X) : Pop#X(TT\Stack) : EndCode()
DefCode(iSwap#X) : Swap#X(Pop#X(TT\Stack), Pop#X(TT\Stack)) : EndCode()
DefCode(iNip#X)  : TT\X = Pop#X(TT\Stack) : Pop#X(TT\Stack) : Push#X(TT\Stack, TT\X) : EndCode()
DefCode(iOver#X) : Over#X(Pop#X(TT\Stack), Pop#X(TT\Stack)) : EndCode()
DefCode(iRot#X)  : Rot#X(Pop#X(TT\Stack),  Pop#X(TT\Stack), Pop#X(TT\Stack)) : EndCode()
DefCode(iRRot#X) : RRot#X(Pop#X(TT\Stack), Pop#X(TT\Stack), Pop#X(TT\Stack)) : EndCode()
; -Memory management-
DefCode(iRead#X)  : TT\UP = PopI(TT\Stack) : Push#X(TT\Stack, TT\UP\X) : EndCode()
DefCode(iWrite#X) : TT\UP = PopI(TT\Stack) : TT\UP\X = Pop#X(TT\Stack) : EndCode()
; -Binary logic-
DefCode(iAnd#X)    : Push#X(TT\Stack, Pop#X(TT\Stack) & Pop#X(TT\Stack)) : EndCode()
DefCode(iOr#X)     : Push#X(TT\Stack, Pop#X(TT\Stack) | Pop#X(TT\Stack)) : EndCode()
DefCode(iXor#X)    : Push#X(TT\Stack, Pop#X(TT\Stack) ! Pop#X(TT\Stack)) : EndCode()
DefCode(iNot#X)    : Push#X(TT\Stack, ~Pop#X(TT\Stack)) : EndCode()
DefCode(iShl#X)    : TT\X = Pop#X(TT\Stack) : Push#X(TT\Stack, Pop#X(TT\Stack) << TT\X) : EndCode()
DefCode(iShr#X)    : TT\X = Pop#X(TT\Stack) : Push#X(TT\Stack, Pop#X(TT\Stack) >> TT\X) : EndCode()
DefCode(iToBin#X)  : If Pop#X(TT\Stack) : Push#X(TT\Stack, -1) : Else : Push#X(TT\Stack, #False) : EndIf
EndCode()
DefCode(iTo2Bin#X) : TT\X = Pop#X(TT\Stack) : If Pop#X(TT\Stack) : Push#X(TT\Stack, -1)
Else : Push#X(TT\Stack, #False) : EndIf 
If TT\X : Push#X(TT\Stack, -1) : Else : Push#X(TT\Stack, #False) : EndIf
EndCode()
; -Comparsion-
DefCode(iEqu#X)    : If Pop#X(TT\Stack) = Pop#X(TT\Stack) : PushI(TT\Stack, #True) 
Else : PushI(TT\Stack, #False) : EndIf : EndCode()
DefCode(iInequ#X)  : If Pop#X(TT\Stack) = Pop#X(TT\Stack) : PushI(TT\Stack, #False) 
Else : PushI(TT\Stack, #True) : EndIf : EndCode()
DefCode(iSearch#X) : PushI(TT\Stack, Search#X(PopI(TT\Stack), Pop#X(TT\Stack))) : EndCode()
EndMacro
; Definitions for each type.
IM_Defs(*ThisThread, B, Byte)
IM_Defs(*ThisThread, W, Word)
IM_Defs(*ThisThread, C, Character)
IM_Defs(*ThisThread, L, Long)
IM_Defs(*ThisThread, Q, Quad)
IM_Defs(*ThisThread, I, Integer) ;}
;{ [Float-only instructions]
Macro FO_Defs(TT, X) ; Definitions block.
; -Complex arithmetics-
DefCode(iLog#X)   : Push#X(TT\Stack, Log(Pop#X(TT\Stack))) : EndCode()
DefCode(iLog10#X) : Push#X(TT\Stack, Log10(Pop#X(TT\Stack))) : EndCode()
DefCode(iSqr#X)   : Push#X(TT\Stack, Sqr(Pop#X(TT\Stack))) : EndCode()
DefCode(iPow#X)   : Push#X(TT\Stack, Pow(Pop#X(TT\Stack), Pop#X(TT\Stack))) : EndCode()
DefCode(iRound#X) : Push#X(TT\Stack, Round(Pop#X(TT\Stack), Pop#X(TT\Stack))) : EndCode()
; -Trigonometry-
DefCode(iACos#X) : Push#X(TT\Stack, ACos(Pop#X(TT\Stack)) * 180 / #PI) : EndCode()
DefCode(iASin#X) : Push#X(TT\Stack, ASin(Pop#X(TT\Stack)) * 180 / #PI) : EndCode()
DefCode(iATan#X) : Push#X(TT\Stack, ATan(Pop#X(TT\Stack)) * 180 / #PI) : EndCode()
DefCode(iCos#X)  : Push#X(TT\Stack, Cos(Pop#X(TT\Stack) * #PI / 180)) : EndCode()
DefCode(iSin#X)  : Push#X(TT\Stack, Sin(Pop#X(TT\Stack) * #PI / 180)) : EndCode()
DefCode(iTan#X)  : Push#X(TT\Stack, Tan(Pop#X(TT\Stack) * #PI / 180)) : EndCode()
EndMacro
; Definitions for each type.
FO_Defs(*ThisThread, F)
FO_Defs(*ThisThread, D) ;}
;{ [Non-mutable instructions]
With *ThisThread
Macro __CallCode(TT, Dest) ; Service macro for procs calling.
PushI(TT\RStack, TT\IP) : PushI(TT\RStack, TT\DS) : TT\IP = Dest ; Dest
EndMacro
DefCode(iNop)  : EndCode() ; Filler instruction.
; -Literals-
DefCode(iAbsPtr)  : PushI(\Stack, \IP\I + \CS) : \IP + SizeOf(Integer) : EndCode()
DefCode(iRelPtr)  : PushI(\Stack, \IP\I + \IP) : \IP + SizeOf(Integer) : EndCode()
DefCode(iDataPtr) : PushI(\Stack, \IP\I + \DS) : \IP + SizeOf(Integer) : EndCode()
DefCode(iLitS)    : \S = PeekS(\IP) : PushS(\Stack, \S) : \IP + StringByteLength(\S) + 1 : EndCode()
DefCode(iLitSPtr) : PushS(\Stack, PeekS(\CS + \IP\I)) : \IP + SizeOf(Integer) : EndCode()
DefCode(iLitIPtr) : \UP = \CS + \IP\I : PushI(\Stack, \UP\I) : \IP + SizeOf(Integer) : EndCode()
; -Stack management-
DefCode(iDupS)  : \UP = \Stack\TOS : \S = PopS(\Stack) : \Stack\TOS = \UP : PushS(\Stack, \S) 
EndCode()
DefCode(iDropS) : PopS(\Stack) : EndCode()
; -Flow control-
DefCode(iHere)   : PushI(\Stack, \IP) : EndCode()
DefCode(iJmp)    : \IP = PopI(\Stack) : EndCode()
DefCode(iIF)     : \I = PopI(\Stack) : If PopI(\Stack) : \IP = \I : EndIf : EndCode()
DefCode(iIFZ)    : \I = PopI(\Stack) : If PopI(\Stack) = 0 : \IP = \I : EndIf : EndCode()
; -Calls management-
DefCode(iCall)   : __CallCode(*ThisThread, PopI(\Stack)) : EndCode()
DefCode(iRet)    : \DS = PopI(\RStack) : \IP = PopI(\RStack) : EndCode()
DefCode(iPushRS) : PushI(\Stack, PopI(\RStack)) : EndCode()
DefCode(iPopRS)  : PushI(\RStack, PopI(\Stack)) : EndCode()
EndWith
; -Memory management-
Procedure StorageAllot(Bytes.i)
With *ThisThread
Define *DataPtr = \SStack\TOS
Define NewSize = __StackDepth(\SStack) + Bytes
If NewSize > \SStack\Size : ResizeStack(\SStack, NewSize) : EndIf
FillMemory(*DataPtr, Bytes, 0)
ProcedureReturn *DataPtr
EndWith
EndProcedure
With *ThisThread ; Separated for new (4.50) version.
DisableDebugger
DefCode(iAllot)   : PushI(\Stack, AllocateMemory(PopI(\Stack))) : EndCode()
DefCode(iFreeMem) : FreeMemory(PopI(\Stack)) : EndCode()
EnableDebugger
DefCode(iPushDS)  : PushI(\Stack, \DS) : EndCode()
DefCode(iPopDS)   : \DS = PopI(\Stack) : EndCode()
DefCode(iPushSS)  : PushI(\Stack, StorageAllot(PopI(\Stack))) : EndCode()
DefCode(iPopSS)   : PopData(\SStack, #Null, PopI(\Stack)) : EndCode()
; -Depth retrievment-
DefCode(iDepth)    : PushI(\Stack, StackDepth(\Stack)) : EndCode()
DefCode(iRDepth)   : PushI(\Stack, StackDepth(\RStack)) : EndCode()
DefCode(iGDepth)   : LockMutex(System\GSMutex) : PushI(\Stack, StackDepth(System\GStack))
UnlockMutex(System\GSMutex) : EndCode()
DefCode(iPtrSize)  : PushI(\Stack, SizeOf(Integer)) : EndCode()
DefCode(iCharSize) : PushI(\Stack, SizeOf(Character)) : EndCode()
; -Threading-
Declare NewThread(*CodeSeg, *IP = 0, Param.i = #Null)
DefCode(iNewThread) : PushI(\Stack, NewThread(PopI(\Stack), PopI(\Stack), PopI(\Stack)))
EndCode()
DefCode(iKill)      : \Thread = PopI(\Stack) : KillThread(\Thread\SystemID)
\Thread\ExitCode = #cKilled : EndCode()
DefCode(iBye)       : \ExitCode = #cBye : EndCode()
DefCode(iThreadz)   : PushI(\Stack, ListSize(System\Threadz())) : EndCode()
DefCode(iBase)      : PushI(\Stack, \CS) : EndCode()
DefCode(iSleep)     : Delay(PopI(\Stack)) : EndCode()
DefCode(iPushGS)    : LockMutex(System\GSMutex) : PushI(\Stack, PopI(System\GStack)) 
UnlockMutex(System\GSMutex) : EndCode()
DefCode(iPopGS)     : LockMutex(System\GSMutex) : PushI(System\GStack, PopI(\Stack))
UnlockMutex(System\GSMutex) : EndCode()
; -Mutexes-
DefCode(iNewMutex)    : PushI(\Stack, CreateMutex()) : EndCode()
DefCode(iFreeMutex)   : FreeMutex(PopI(\Stack)) : EndCode()
DefCode(iMutexState)  : \UP = PopI(\Stack) : \I = TryLockMutex(\UP) : PushI(\Stack, \I)
If \I = #False : UnlockMutex(PopI(\UP)) : EndIf : EndCode()
DefCode(iLockMutex)   : LockMutex(PopI(\Stack)) : EndCode()
DefCode(iUnLockMutex) : UnlockMutex(PopI(\Stack)) : EndCode()
EndWith
; -System calls-
Macro FormatSCName(Name) : LCase(Trim(Name)) : EndMacro ; Pseudo-procedure.
Macro CallSystem(SC) ; Pseudo-procedure.
If SC ; Если имеются данные по вызову...
Define DestSC.SystemCall : CopyStructure(SC, @DestSC, SystemCall)
UnlockMutex(System\SCMutex) ; Разблокируем мутекс.
If DestSC\Native : EnableASM : JMP DestSC\Address : DisableASM
Else : __CallCode(*ThisThread, DestSC\Address)
EndIf ; Вовзращаем ошибку.
Else : *ThisThread\ExitCode = #cInvalidCall
EndIf 
EndMacro
Procedure AddSysCall(CName.S, *Base, Native = #True) ; Defines new system call.
With System
Define *Call.SystemCall = AddMapElement(\SysCalls(), CName)
*Call\Address = *Base : *Call\Native = Native
EndWith
EndProcedure
Procedure PopSysCall(CallName.s)
Define CallData.SystemCall ; Accumulator.
LockMutex(System\SCMutex)
PopData(*ThisThread\Stack, @CallData, SizeOf(SystemCall))
With CallData
If \Address ; Если не требуется удаление...
AddSysCall(CallName, \Address, \Native) : Else : DeleteMapElement(System\SysCalls(), CallName)
EndIf
EndWith
UnlockMutex(System\SCMutex)
EndProcedure
With *ThisThread ; Separated for new (4.50) version.
DefCode(iSysCall) : LockMutex(System\SCMutex) : \SC = System\SysCalls(FormatSCName(PopS(\Stack)))
Callsystem(\SC) : EndCode()
DefCode(iSysCallPtr) : LockMutex(System\SCMutex) : \SC = PopI(\Stack) : CallSystem(\SC) : EndCode()
DefCode(iPushSC) : LockMutex(System\SCMutex) : \SC = System\SysCalls(FormatSCName(PopS(\Stack))) : If \SC
PushData(\Stack, \SC, SizeOf(SystemCall)) : Else : PushI(\Stack, 0) : PushI(\Stack, 0) : EndIf 
UnlockMutex(System\SCMutex) : EndCode()
DefCode(iPopSC)  : PopSysCall(FormatSCName(PopS(\Stack))) : EndCode()
DefCode(iFindSC) : LockMutex(System\SCMutex) : PushI(\Stack, System\SysCalls(FormatSCName(PopS(\Stack))))
UnlockMutex(System\SCMutex) : EndCode()
EndWith ;}
;} End table

; ---------------------------------
;{ SysCalls table
; -Service macros-
Macro Quotes
"
EndMacro

Macro SysCall(CallName)
Define CName.s = Quotes#CallName#Quotes
AddSysCall(ReplaceString(FormatSCName(CName), "_", "."), ?SysCall_#CallName)
If 0 : SysCall_#CallName:
EndMacro

Macro EndCall()
! JMP __Ret ; Спасибо Фреду за наше счастливое детство.
EndIf
EndMacro

; -Minimal set definitions-
;{ [Debugging IO]
Procedure.C WaitChar() ; Waits for input of character.
Repeat : Delay(50)
Define Char.c = Asc(Inkey())
Until Char <> 0
ProcedureReturn Char
EndProcedure

With *ThisThread
SysCall(Minimal_Debug_TypeString)  : \S = PopS(\Stack) : CharToOem_(@\S, @\S) : Print(\S) : EndCall()
SysCall(Minimal_Debug_GetKey)      : Inkey() : PushC(\Stack, RawKey()) : EndCall()
SysCall(Minimal_Debug_StringInput) : \S = Input() : OemToChar_(@\S, @\S) : PushS(\Stack, \S) : EndCall()
SysCall(Minimal_Debug_WaitKey)     : PushC(\Stack, WaitChar()): EndCall()
EndWith ;}
;{ [String operations]
DefSharedProcs(*ThisThread, S)
Macro __StartSeqScan(Lim) ; Partializer.
If Lim >= 0
Repeat : Define SDepth.i = __StackDepth(*ThisThread\Stack)
If SDepth <= Lim : Break : EndIf
EndMacro
Macro __SeqScanBreaker(Lim) ; Partializer.
If __StackDepth(*ThisThread\Stack) = SDepth
SeekStack(*ThisThread\Stack, Lim) : Break
EndIf
EndMacro
Macro __StopSeqScan() ; Partializer.
ForEver : EndIf
EndMacro
Procedure SearchS(Edge.i, Val.s)
With *ThisThread
__StartSeqScan(Edge)
If PopS(\Stack) = Val : SeekStack(\Stack, Edge) : ProcedureReturn #True : EndIf
__SeqScanBreaker(Edge)
__StopSeqScan()
EndWith
EndProcedure
Procedure.s StringInsertion(Base.s, Fragment.s, IPos.i)
If Base
If Fragment : ProcedureReturn Left(Base, IPos) + Fragment + Mid(Base, IPos + 1)
Else : ProcedureReturn Base
EndIf
Else : ProcedureReturn Fragment
EndIf
EndProcedure
Procedure.s CutString(Txt.s, CutStart.i, CutSize.i)
If Txt
ProcedureReturn Left(Txt, CutStart - 1) + Mid(Txt, CutStart + CutSize)
EndIf
EndProcedure
With *ThisThread
; -Type conversion-
SysCall(Minimal_String_FromInteger) : PushS(\Stack, Str(PopI(\Stack)))   : EndCall()
SysCall(Minimal_String_FromByte)    : PushS(\Stack, Str(PopB(\Stack)))   : EndCall()
SysCall(Minimal_String_FromWord)    : PushS(\Stack, Str(PopW(\Stack)))   : EndCall()
SysCall(Minimal_String_FromLong)    : PushS(\Stack, Str(PopL(\Stack)))   : EndCall()
SysCall(Minimal_String_FromQuad)    : PushS(\Stack, Str(PopQ(\Stack)))   : EndCall()
SysCall(Minimal_String_FromFloat)   : PushS(\Stack, RTrim(RTrim(StrF(PopF(\Stack)), "0"), ".")) : EndCall()
SysCall(Minimal_String_FromDouble)  : PushS(\Stack, RTrim(RTrim(StrD(PopD(\Stack)), "0"), ".")) : EndCall()
SysCall(Minimal_String_FromChar)    : PushS(\Stack, Chr(PopC(\Stack)))   : EndCall()
SysCall(Minimal_String_ToInteger)   : PushI(\Stack, Val(PopS(\Stack)))   : EndCall()
SysCall(Minimal_String_ToByte)      : PushB(\Stack, Val(PopS(\Stack)))   : EndCall()
SysCall(Minimal_String_ToWord)      : PushW(\Stack, Val(PopS(\Stack)))   : EndCall()
SysCall(Minimal_String_ToLong)      : PushL(\Stack, Val(PopS(\Stack)))   : EndCall()
SysCall(Minimal_String_ToQuad)      : PushQ(\Stack, Val(PopS(\Stack)))   : EndCall()
SysCall(Minimal_String_ToFloat)     : PushF(\Stack, ValF(PopS(\Stack)))  : EndCall() ; !!
SysCall(Minimal_String_ToDouble)    : PushD(\Stack, ValD(PopS(\Stack)))  : EndCall() ; !!
SysCall(Minimal_String_ToChar)      : PushC(\Stack, Asc(Pops(\Stack)))   : EndCall()
; -Basic operations-
SysCall(Minimal_String_ToUpper)     : PushS(\Stack, UCase(PopS(\Stack))) : EndCall()
SysCall(Minimal_String_ToLower)     : PushS(\Stack, LCase(PopS(\Stack))) : EndCall()
SysCall(Minimal_String_Spaces)      : PushS(\Stack, Space(PopI(\Stack))) : EndCall()
SysCall(Minimal_String_TrimLeft)    : PushS(\Stack, LTrim(PopS(\Stack))) : EndCall()
SysCall(Minimal_String_TrimRight)   : PushS(\Stack, RTrim(PopS(\Stack))) : EndCall()
SysCall(Minimal_String_FullTrim)    : PushS(\Stack, Trim(PopS(\Stack)))  : EndCall()
SysCall(Minimal_String_GetLength)   : PushI(\Stack, Len(PopS(\Stack)))   : EndCall()
SysCall(Minimal_String_CountString) : PushI(\Stack, CountString(PopS(\Stack), PopS(\Stack)))
EndCall()
SysCall(Minimal_String_LeftPart)  : PushS(\Stack, Left(PopS(\Stack), PopI(\Stack))) : EndCall()
SysCall(Minimal_String_RightPart) : PushS(\Stack, Right(PopS(\Stack), PopI(\Stack))) : EndCall()
SysCall(Minimal_String_MidPart)   : PushS(\Stack, Mid(PopS(\Stack), PopI(\Stack), PopI(\Stack)))
EndCall()
SysCall(Minimal_String_Write)      : \I = PopI(\Stack) : PokeS(\I, PopS(\Stack)) : EndCall()
DisableDebugger
SysCall(Minimal_String_Read)       : PushS(\Stack, PeekS(PopI(\Stack))) : EndCall()
EnableDebugger
SysCall(Minimal_String_RemovePart) : PushS(\Stack, RemoveString(PopS(\Stack), PopS(\Stack)))
EndCall()
SysCall(Minimal_String_ReplacePart)
PushS(\Stack, ReplaceString(PopS(\Stack), PopS(\Stack), PopS(\Stack))) : EndCall()
SysCall(Minimal_String_LeftSet) : PushS(\Stack, LSet(PopS(\Stack), PopI(\Stack), Chr(PopC(\Stack))))
EndCall()
SysCall(Minimal_String_RightSet) : PushS(\Stack, RSet(PopS(\Stack), PopI(\Stack), Chr(PopC(\Stack))))
EndCall()
SysCall(Minimal_String_FindString)
PushI(\Stack, FindString(PopS(\Stack), PopS(\Stack), PopI(\Stack))) : EndCall()
SysCall(Minimal_String_Concatenate) : \S = PopS(\Stack) : PushS(\Stack, PopS(\Stack) + \S) : EndCall()
SysCall(Minimal_String_Compare)  : If PopS(\Stack) = PopS(\Stack) : PushI(\Stack, #True) : Else
PushI(\Stack, #False) : EndIf : EndCall()
; -Extended operations-
SysCall(Minimal_String_Insert) : PushS(\Stack, StringInsertion(PopS(\Stack), PopS(\Stack), PopI(\Stack)))
EndCall()
SysCall(Minimal_String_Cut)    : PushS(\Stack, CutString(PopS(\Stack), PopI(\Stack), PopI(\Stack))) : EndCall()
SysCall(Minimal_String_StackSearch) : PushI(\Stack, SearchS(PopI(\Stack), PopS(\Stack))) : EndCall()
SysCall(Minimal_String_Swap)        : SwapS(PopS(\Stack), PopS(\Stack)) : EndCall()
SysCall(Minimal_String_Nip)  : \S = PopS(\Stack) : PopS(\Stack) : PushS(\Stack, \S) : EndCode()
SysCall(Minimal_String_Over) : OverS(PopS(\Stack), PopS(\Stack)) : EndCall()
SysCall(Minimal_String_Rot)  : RotS(PopS(\Stack), PopS(\Stack), PopS(\Stack)) : EndCode()
SysCall(Minimal_String_RRot) : RRotS(PopS(\Stack), PopS(\Stack), PopS(\Stack)) : EndCode()
DisableDebugger
SysCall(Minimal_String_Box) : \I = AllocateMemory(PopI(\Stack) + 1) : \Stack\TOS + SizeOf(Integer) 
If \I : PokeS(\I, PopS(\Stack)) : EndIf : PushI(\Stack, \I) : EndCode()
SysCall(Minimal_String_UnBox) : \I = PopI(\Stack) : PushS(\Stack, PeekS(\I)) : FreeMemory(\I) 
EndCode()
SysCall(Minimal_String_Split) : \I = PopI(\Stack) : \S = PopS(\Stack) : PushS(\Stack, Left(\S, \I))
PushS(\Stack, Mid(\S, \I + 1)) : EndCall()
EnableDebugger
EndWith
;}
;{ [Sequence operations]
Macro __CorrectSeqLen(SeqLenAcum) ; Service macro for all sequence operations.
SeqLenAcum = __StackDepth(*ThisThread\Stack) - SeqLenAcum
EndMacro
Macro __SeqValid(SeqPtr, SeqLen) ; Service macro for all sequence operations.
(SeqPtr Or (SeqLen) = 0)
EndMacro
Macro __Data2Seq(Size = 0) ; Service macro for all sequence operations.
PushI(*ThisThread\Stack, __StackDepth(*ThisThread\Stack) - (Size))
EndMacro
Macro __PopSeqSize(SizeAcum) ; Service macro for all sequence operations.
Define SizeAcum = __StackDepth(*ThisThread\Stack) - PopI(*ThisThread\Stack) - SizeOf(Integer)
EndMacro
Macro __Sequence_Procs(Stack) ; 'With' replacer (for 4.50)
Procedure BoxSeq(SeqLen.i)
__CorrectSeqLen(SeqLen)
DisableDebugger
Define *Ptr = AllocateMemory(SeqLen)
If *Ptr : PopData(Stack, *Ptr, SeqLen) : ProcedureReturn *Ptr : EndIf
EnableDebugger
EndProcedure
Procedure DuplicateSeq(SeqLen.i, *TOS)
__CorrectSeqLen(SeqLen)
Define *Ptr = PopData(Stack, #Null, SeqLen)
If *Ptr : Stack\TOS = *TOS
PushData(Stack, *Ptr, SeqLen) 
PushI(Stack, *TOS - Stack\Base)
Else : __Data2Seq() : __Data2Seq()
EndIf
EndProcedure
Procedure CompareSeqs(FSeqLen.i)
__CorrectSeqLen(FSeqLen)
Define *Ptr1 = PopData(Stack, #Null, FSeqLen)
If __SeqValid(*Ptr1, FSeqLen)
__PopSeqSize(SSeqLen.i)
Define *Ptr2 = PopData(Stack, #Null, SSeqLen)
If __SeqValid(*Ptr2, SSeqLen)
If FSeqLen = SSeqLen : ProcedureReturn CompareMemory(*Ptr1, *Ptr2, FSeqLen) : EndIf
EndIf
EndIf
EndProcedure
Procedure ConcSeqs(FSeqLen.i)
__CorrectSeqLen(FSeqLen)
Define *TOS = Stack\TOS
If __SeqValid(PopData(Stack, #Null, FSeqLen), FSeqLen)
__PopSeqSize(SSeqLen.i)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
If PopData(Stack, #Null, SSeqLen) Or SSeqLen = 0 
Stack\TOS + SSeqLen : Else : SSeqLen = 0
EndIf
MoveMemory(Stack\TOS + SizeOf(Integer), Stack\TOS, FSeqLen)
Stack\TOS = *TOS - SizeOf(Integer)
__Data2Seq(FSeqLen + SSeqLen)
ProcedureReturn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
EndIf
Stack\TOS = *TOS
__Data2Seq()
EndProcedure
Procedure SearchSeq(SeqLen.i)
__CorrectSeqLen(SeqLen)
Define *Ptr = PopData(Stack, #Null, SeqLen)
If __SeqValid(*Ptr, SeqLen)
Define Edge = PopI(Stack)
__StartSeqScan(Edge)
__PopSeqSize(CSeqLen.I)
Define *Ptr2 = PopData(Stack, #Null, SeqLen)
__SeqScanBreaker(Edge)
If SeqLen = CSeqLen
If CompareMemory(*Ptr, *Ptr2, SeqLen) : SeekStack(Stack, Edge) : ProcedureReturn #True : EndIf
EndIf 
__StopSeqScan()
EndIf
EndProcedure
Procedure SeqOver(FSeqLen.i, *TOS)
__CorrectSeqLen(FSeqLen)
Define *Ptr = PopData(Stack, #Null, FSeqLen)
If __SeqValid(*Ptr, FSeqLen)
__PopSeqSize(SSeqLen)
Define *Ptr2 = PopData(Stack, #Null, SSeqLen)
If *Ptr2 : CopyMemory(Stack\TOS, *TOS, SSeqLen)
Stack\TOS = *TOS + SSeqLen
PushI(Stack, __StackDepth(Stack) - SSeqLen)
ProcedureReturn
EndIf
EndIf
Stack\TOS = *TOS
__Data2Seq()
EndProcedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Procedure SeqNip(FSeqLen.i)
__CorrectSeqLen(FSeqLen)
Define *Ptr = PopData(Stack, #Null, FSeqLen)
If __SeqValid(*Ptr, FSeqLen)
__PopSeqSize(SSeqLen)
If (__SeqValid(PopData(Stack, #Null, SSeqLen), SSeqLen)) 
MoveMemory(*Ptr, Stack\TOS, FSeqLen)
Stack\TOS + FSeqLen
__Data2Seq(FSeqLen)
ProcedureReturn
EndIf
EndIf
__Data2Seq()
EndProcedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Procedure SplitSeq(SeqLen.i, SplitPos.i)
__CorrectSeqLen(SeqLen)
If SeqLen <= __StackDepth(Stack)
If SplitPos < 0 : SplitPos = 0 : EndIf
If SplitPos < SeqLen : SeqLen - SplitPos
Define *TOS = Stack\TOS + SizeOf(Integer)
PopData(Stack, #Null, SeqLen)
MoveMemory(Stack\TOS, Stack\TOS + SizeOf(Integer), SeqLen)
__Data2Seq(SplitPos) : Stack\TOS = *TOS : __Data2Seq(SeqLen)
ProcedureReturn
EndIf
Stack\TOS + SizeOf(Integer)
Else : __Data2Seq()
EndIf
__Data2Seq()
EndProcedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Procedure SwapSeqs(FSeqLen.i, *Tmp)
__CorrectSeqLen(FSeqLen)
Define *Ptr = PopData(Stack, #Null, FSeqLen)
If __SeqValid(*Ptr, FSeqLen)
__PopSeqSize(SSeqLen)
Define *Ptr2 = PopData(Stack, #Null, SSeqLen)
If *Ptr2
Stack\TOS = *Tmp
PushData(Stack, *Ptr2, SSeqLen)
Stack\TOS = *Ptr2
Else : SSeqLen = 0
EndIf
MoveMemory(*Ptr, Stack\TOS, FSeqLen)
Stack\TOS + FSeqLen
__Data2Seq(FSeqLen)
PushData(Stack, *Tmp, SSeqLen)
__Data2Seq(SSeqLen)
ProcedureReturn
EndIf
__Data2Seq()
EndProcedure
EndMacro : __Sequence_Procs(*ThisThread\Stack)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
With *ThisThread ; Separated for new (4.50) version.
SysCall(Minimal_Sequence_Box)   : PushI(\Stack, BoxSeq(PopI(\Stack))) : EndCall()
SysCall(Minimal_Sequence_UnBox) : \I = PopI(\Stack) : If PushData(\Stack, \I, MemorySize(\I))
PushI(\Stack, __StackDepth(\Stack) - MemorySize(\I)) : Else : __Data2Seq() : EndIf : FreeMemory(\I)
EndCall()
SysCall(Minimal_Sequence_Drop)
PopData(\Stack, #Null, __StackDepth(\Stack) - PopI(\Stack) - SizeOf(Integer)) : EndCall()
SysCall(Minimal_Sequence_Dup)     : DuplicateSeq(PopI(\Stack), \Stack\TOS) : EndCall()
SysCall(Minimal_Sequence_Compare) : PushI(\Stack, CompareSeqs(PopI(\Stack))) : EndCall()
SysCall(Minimal_Sequence_Over)    : SeqOver(PopI(\Stack), \Stack\TOS) : EndCall()
SysCall(Minimal_Sequence_Nip)     : SeqNip(PopI(\Stack)) : EndCall()
SysCall(Minimal_Sequence_Concatenate) : ConcSeqs(PopI(\Stack)) : EndCall()
SysCall(Minimal_Sequence_Split)   : SplitSeq(PopI(\Stack), PopI(\Stack)) : EndCall()
SysCall(Minimal_Sequence_Swap)    : SwapSeqs(PopI(\Stack), \Stack\TOS) : EndCall()
EndWith
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Macro __Sequence_Procs2(Stack) ; 'With' replacer (for 4.50)
Procedure SeqMid(SeqLen.i, Start.i, Size.i)
__CorrectSeqLen(SeqLen)
If PopData(Stack, #Null, SeqLen)
If Start < SeqLen And Size > 0
If Start < 0 : Start = 0 : EndIf
If Start + Size > SeqLen : Size = SeqLen - Start : EndIf
MoveMemory(Stack\TOS + Start, Stack\TOS, Size)
Stack\TOS + Size : __Data2Seq(Size)
ProcedureReturn
EndIf
EndIf
__Data2Seq()
EndProcedure
Procedure SeqLeft(SeqLen.i, Size.i)
__CorrectSeqLen(SeqLen)
If PopData(Stack, #Null, SeqLen)
If Size > 0
If Size > SeqLen  : Size = SeqLen : EndIf
Stack\TOS + Size : __Data2Seq(Size)
ProcedureReturn
EndIf
EndIf
__Data2Seq()
EndProcedure
Procedure SeqRight(SeqLen.i, Size.i)
__CorrectSeqLen(SeqLen)
If PopData(Stack, #Null, SeqLen)
If Size > 0 : If Size < SeqLen
MoveMemory(Stack\TOS + (SeqLen - Size), Stack\TOS, Size)
Stack\TOS + Size : __Data2Seq(Size)
ProcedureReturn
EndIf : EndIf
EndIf
__Data2Seq()
EndProcedure
Procedure CutSeq(SeqLen.i, Start.i, Size.i)
__CorrectSeqLen(SeqLen)
If PopData(Stack, #Null, SeqLen)
If Start < SeqLen And Size > 0
If Start < 0 : Start = 0 : Else : Stack\TOS + Start : EndIf
If Start + Size > SeqLen : Size = SeqLen - Start : EndIf
SeqLen - Size : Define Corrector = SeqLen - Start
MoveMemory(Stack\TOS + Size, Stack\TOS, Corrector)
Stack\TOS + Corrector :  __Data2Seq(SeqLen)
ProcedureReturn
EndIf
EndIf
__Data2Seq()
EndProcedure
Procedure InsertSeq(*Tmp, ISeqLen.i, Pos.i)
__CorrectSeqLen(ISeqLen)
Define *Ptr = PopData(Stack, #Null, ISeqLen)
If __SeqValid(*Ptr, ISeqLen) : __PopSeqSize(BSeqLen)
If __SeqValid(PopData(Stack, #Null, BSeqLen), BSeqLen)
If Pos < BSeqLen ; Если размер подходит...
If Pos < 0 : Pos = 0 : EndIf ; Коректировка размера.
Define *Ptr2 = Stack\TOS + Pos : Stack\TOS = *Tmp
PushData(Stack, *Ptr, ISeqLen) : BSeqLen + ISeqLen ; Резервное копирование.
MoveMemory(*Ptr2, *Ptr2 + ISeqLen, BSeqLen - Pos) ; Сдивг.
CopyMemory(*Tmp, *Ptr2, ISeqLen) ; Копирование вставки.
Stack\TOS = *Tmp - SizeOf(Integer) : __Data2Seq(BSeqLen)
ProcedureReturn
EndIf
EndIf 
EndIf
__Data2Seq()
EndProcedure
EndMacro : __Sequence_Procs2(*ThisThread\Stack)
With *ThisThread ; Separated for new (4.50) version.
SysCall(Minimal_Sequence_MidPart)   : SeqMid(PopI(\Stack), PopI(\Stack), PopI(\Stack)) : EndCall()
SysCall(Minimal_Sequence_LeftPart)  : SeqLeft(PopI(\Stack), PopI(\Stack)) : EndCall()
SysCall(Minimal_Sequence_RightPart) : SeqRight(PopI(\Stack), PopI(\Stack)) : EndCall()
SysCall(Minimal_Sequence_Cut)       : CutSeq(PopI(\Stack), PopI(\Stack), PopI(\Stack)) : EndCall()
SysCall(Minimal_Sequence_Insert)    : InsertSeq(\Stack\TOS, PopI(\Stack), PopI(\Stack)) : EndCall()
SysCall(Minimal_Sequence_GetSize) : \I = PopI(\Stack) : \I = __StackDepth(\Stack) - \I 
If PopData(\Stack, #Null, \I) : PushI(\Stack, \I) : Else : PushI(\Stack, 0) : EndIf : EndCall()
SysCall(Minimal_Sequence_StackSearch) : PushI(\Stack, SearchSeq(PopI(\Stack))) : EndCall()
SysCall(Minimal_Sequence_Write) : \I = PopI(\Stack) 
PopData(\Stack, \I, __StackDepth(\Stack) - PopI(\Stack) - SizeOf(Integer)) : EndCall()
SysCall(Minimal_Sequence_Read) : \I = PopI(\Stack)
If PushData(\Stack, PopI(\Stack), PopI(\Stack)) : __Data2Seq(\I) : Else : __Data2Seq() : EndIf 
EndCall()
EndWith
;}
;{ [RunTime support]
Macro FreeStrings(Start, Count, Clear = #False) ; Service macro for 'ResizeArray'
Define *I.Integer, *ToFix = Start + Count * SizeOf(Integer) - SizeOf(Integer)
For *I = Start To *ToFix : FreeMemory(*I\I)
CompilerIf Clear : *I\I = #Null
CompilerEndIf
Next
EndMacro
Macro __ReInitArr(NewCells, CellSize, CellsVar, ArrPtr, Preserve = #False)
DisableDebugger
If NewCells > 0
If CellSize = -1 
If Preserve = #False : FreeStrings(ArrPtr\I, CellsVar\I)
ElseIf NewCells < CellsVar\I
FreeStrings(ArrPtr\I + NewCells * SizeOf(Integer), CellsVar\I - NewCells)
EndIf : CellSize = SizeOf(Integer)
EndIf : CellsVar\I = NewCells
;-----Optimizable[-----
If Preserve = #False : FreeMemory(ArrPtr\I) 
ArrPtr\I = AllocateMemory(NewCells * CellSize)
Else : ArrPtr\I = ReAllocateMemory(ArrPtr\I, NewCells * CellSize)
EndIf
;-----]Optimizable-----
Else : If CellSize = -1 : FreeStrings(ArrPtr\I, CellsVar\I) : EndIf
FreeMemory(ArrPtr\I) : ArrPtr\I = #Null : CellsVar\I = 0
EndIf
EnableDebugger
EndMacro
Procedure ResizeArray(NewCells,CellSize.b,*CellsVar.Integer,*ArrPtr.Integer,Preserve=#False)
__ReInitArr(NewCells, CellSize, *CellsVar, *ArrPtr, Preserve)
EndProcedure
Macro __CountSeqStrings(Stack, SeqLen)
Define NewCells.i, *TOS = Stack\TOS
__StartSeqScan(SeqLen)
PopS(Stack) 
__SeqScanBreaker(SeqLen)
NewCells + 1
__StopSeqScan()
Stack\TOS = *TOS
*TOS = NewCells * SizeOf(Integer)
EndMacro
Macro __WriteArrSeq(Stack, Start, Len, Flag, CorrectSize = #True) ; Service macro for 'WriteSeq2Arr'
If Flag
Define *StrPtr.Integer = Start
__StartSeqScan(Len)
Define StrSize.i = PopI(Stack) : Stack\TOS + SizeOf(Integer)
Define Str.s = PopS(Stack)
__SeqScanBreaker(Len)
*StrPtr - SizeOf(Integer)
DisableDebugger
If Flag = -2 : *StrPtr\I = AllocateMemory(StrSize + 1)
Else : *StrPtr\I = ReAllocateMemory(*StrPtr\I, StrSize + 1)
EndIf
EnableDebugger
PokeS(*StrPtr\I, Str)
__StopSeqScan()
Else
CompilerIf CorrectSize : PopData(Stack, Start, __StackDepth(Stack) - Len)
CompilerElse : PopData(Stack, Start, Len)
CompilerEndIf 
EndIf
EndMacro
Macro __Runtime_Procs(Stack) ; 'With' replacer (for 4.50)
Procedure WriteSeq2Arr(SeqLen.i, *Start, String.b)
If String : __CountSeqStrings(Stack, SeqLen) : EndIf
__WriteArrSeq(Stack, *Start + *TOS, SeqLen, String)
EndProcedure
Procedure ArrFromSeq(SeqLen.i, CellSize.b, *CellsVar.Integer, *ArrPtr.Integer)
Define StrFlag.b
If CellSize < 1 : __CountSeqStrings(Stack, SeqLen)
If CellSize = 0 : CellSize = SizeOf(Integer) : EndIf
StrFlag = -1
Else : SeqLen = __StackDepth(Stack) - SeqLen
NewCells = Round(SeqLen / CellSize, #PB_Round_Up)
StrFlag = 0
EndIf
__ReInitArr(NewCells, CellSize, *CellsVar, *ArrPtr)
__WriteArrSeq(Stack, *ArrPtr\I + *TOS, SeqLen, StrFlag, #False)
EndProcedure
Procedure PlaceString(String.s, *StrVar.Integer, Concatenate = #False)
DisableDebugger
If Concatenate : String = PeekS(*StrVar\I) + String : EndIf
FreeMemory(*StrVar\I)
EnableDebugger
*StrVar\I = AllocateMemory(StringByteLength(String) + 1)
PokeS(*StrVar\I, String)
EndProcedure
Procedure AddChar(Char.C, *StrVar.Integer)
Define *CPtr.Character = MemoryStringLength(*StrVar\I)
*StrVar\I = ReAllocateMemory(*StrVar\I, *CPtr + SizeOf(Character) + 1)
*CPtr + *StrVar\I : *CPtr\C = Char
EndProcedure
Procedure ResizeString(NewLen.i, *StrVar.Integer)
DisableDebugger
If NewLen > 0 : NewLen = NewLen * SizeOf(Character)
*StrVar\I = ReAllocateMemory(*StrVar\I, NewLen + 1)
Define *Zero.Byte = *StrVar\I + NewLen : *Zero\B = 0
Else : FreeMemory(*StrVar\I) : *StrVar\I = 0
EndIf
EnableDebugger
EndProcedure
Procedure ReadArrSeq(Start.i, Fin.i, *ArrStart, CellSize.b)
Define *Ptr.Integer, *ToFix, Edge.i = __StackDepth(Stack)
If CellSize < 1
If Start > Fin
*ToFix = *ArrStart + Fin * SizeOf(Integer) : *Ptr = *ToFix + (Start - Fin) * SizeOf(Integer)
DisableDebugger
While *Ptr => *ToFix : PushS(Stack, PeekS(*Ptr\I)) : *Ptr - SizeOf(Integer) : Wend
Else 
*Ptr = *ArrStart + Start * SizeOf(Integer) : *ToFix = *Ptr + (Fin - Start) * SizeOf(Integer)
While *Ptr <= *ToFix : PushS(Stack, PeekS(*Ptr\I)) : *Ptr + SizeOf(Integer) : Wend
EnableDebugger
EndIf
Else
If Start > Fin
*ToFix = *ArrStart + Fin * CellSize : *Ptr = *ToFix + (Start - Fin) * CellSize
While *Ptr >= *ToFix : PushData(Stack, *Ptr, CellSize) : *Ptr - CellSize : Wend
Else : PushData(Stack, *ArrStart + Start * CellSize, (Fin - Start + 1) * CellSize)
EndIf
EndIf
PushI(Stack, Edge)
EndProcedure
Procedure ArrInsertion(SeqLen.i, Index, CellSize.b, *CellsVar.Integer, *ArrPtr.Integer)
Define StrFlag.b, ShiftSize.i
If CellSize < 1 : __CountSeqStrings(Stack, SeqLen)
CellSize = SizeOf(Integer)
StrFlag = -2 : ShiftSize = NewCells * SizeOf(Integer)
Else : SeqLen = __StackDepth(Stack) - SeqLen
NewCells = Round(SeqLen / CellSize, #PB_Round_Up)
StrFlag = 0  : ShiftSize = SeqLen
EndIf
NewCells + *CellsVar\I
__ReInitArr(NewCells, CellSize, *CellsVar, *ArrPtr, #True)
Define *Pos = Index * CellSize + *ArrPtr\I
MoveMemory(*Pos, *Pos + ShiftSize, (NewCells - Index) * CellSize - ShiftSize)
__WriteArrSeq(Stack, *Pos + *TOS, SeqLen, StrFlag, #False)
EndProcedure
Procedure NewArrElement(Index, CellSize.b, *CellsVar.Integer, *ArrPtr.Integer)
Define StrFlag.b, ShiftSize.i
If CellSize < 1 : CellSize = SizeOf(Integer) : EndIf
Define NewCells = *CellsVar\I + 1
__ReInitArr(NewCells, CellSize, *CellsVar, *ArrPtr, #True)
Define *Pos.Integer = Index * CellSize + *ArrPtr\I
MoveMemory(*Pos, *Pos + CellSize, (NewCells - Index - 1) * CellSize)
*Pos\I = #Null
ProcedureReturn *Pos
EndProcedure
Procedure ClearArray(*ArrStart, ArrLen)
FreeStrings(*ArrStart, ArrLen, #True)
EndProcedure
Procedure StaticArrayGC(*ArrStart, ArrLen)
FreeStrings(*ArrStart, ArrLen)
EndProcedure
EndMacro : __Runtime_Procs(*ThisThread\Stack)
With *ThisThread
SysCall(Minimal_RunTime_ReInitArray) 
ResizeArray(PopI(\Stack), PopB(\Stack), PopI(\Stack), PopI(\Stack)) : EndCall()
SysCall(Minimal_RunTime_ResizeArray) 
ResizeArray(PopI(\Stack), PopB(\Stack), PopI(\Stack), PopI(\Stack), #True) : EndCall()
SysCall(Minimal_RunTime_PlaceString) : PlaceString(PopS(\Stack), PopI(\Stack)) : EndCall()
SysCall(Minimal_RunTime_AddString)   : PlaceString(PopS(\Stack), PopI(\Stack), #True) : EndCall()
SysCall(Minimal_RunTime_StrVarSize)  : \UP = PopI(\Stack)
PushI(\Stack, MemoryStringLength(\UP\I)) : EndCall()
SysCall(Minimal_RunTime_StrVarLen)   : \UP = PopI(\Stack)
CompilerIf SizeOf(Character) = 1 : PushI(\Stack, MemoryStringLength(\UP\I))
CompilerElse : PushI(\Stack, MemoryStringLength(\UP\I) << 1)
CompilerEndIf
EndCall()
DisableDebugger
SysCall(Minimal_RunTime_FreeStrVar)  : \UP = PopI(\Stack) : FreeMemory(\UP\I) : \UP\I = #Null
EndCall()
EnableDebugger
SysCall(Minimal_RunTime_StrCharPtr) : \UP = PopI(\Stack)
PushI(\Stack, (\UP\I + (PopI(\Stack) - 1) * SizeOf(Character))) : EndCall()
SysCall(Minimal_RunTime_AddStrChar) : AddChar(PopC(\Stack), PopI(\Stack)) : EndCall()
SysCall(Minimal_RunTime_ResizeStrVar) : ResizeString(PopI(\Stack), PopI(\Stack)) : EndCall()
SysCall(Minimal_RunTime_ReadArrSeq) 
ReadArrSeq(PopI(\Stack), PopI(\Stack), PopI(\Stack), PopB(\Stack)) : EndCall()
SysCall(Minimal_RunTime_WriteArrSeq)
WriteSeq2Arr(PopI(\Stack), PopI(\Stack), PopB(\Stack)) : EndCall()
SysCall(Minimal_RunTime_MakeArrFromSeq)
ArrFromSeq(PopI(\Stack), PopB(\Stack), PopI(\Stack), PopI(\Stack)) : EndCall()
SysCall(Minimal_RunTime_InsertSeq2Arr)
ArrInsertion(PopI(\Stack), PopI(\Stack), PopB(\Stack), PopI(\Stack), PopI(\Stack)) : EndCall()
SysCall(Minimal_RunTime_ArrInsertElement)
PushI(\Stack, NewArrElement(PopI(\Stack), PopB(\Stack), PopI(\Stack), PopI(\Stack))) : EndCall()
SysCall(Minimal_RunTime_ClearArray)    : ClearArray(PopI(\Stack), PopI(\Stack))    : EndCall()
SysCall(Minimal_RunTime_StaticArrayGC) : StaticArrayGC(PopI(\Stack), PopI(\Stack)) : EndCall()
EndWith
;}
;{ [Miscelanious]
With *ThisThread
SysCall(Minimal_Misc_RandomInt)      : PushI(\Stack, Random(PopI(\Stack))) : EndCall()
SysCall(Minimal_Misc_Millisecs)      : PushI(\Stack, ElapsedMilliseconds()) : EndCall()
SysCall(Minimal_Misc_OSVersion)      : PushI(\Stack, OSVersion()) : EndCall()
SysCall(Minimal_Misc_Platform)       : PushI(\Stack, #PB_Compiler_OS) : EndCall()
SysCall(Minimal_Misc_BufferSize)     : PushI(\Stack, MemorySize(PopI(\Stack))) : EndCall()
SysCall(Minimal_Misc_CompareMemory) 
PushI(\Stack, CompareMemory(PopI(\Stack), PopI(\Stack), PopI(\Stack))) : EndCall()
SysCall(Minimal_Misc_ResizeMemory)   : PushI(\Stack, ReAllocateMemory(PopI(\Stack), PopI(\Stack))) : EndCall()
SysCall(Minimal_Misc_MoveMemory)     : MoveMemory(PopI(\Stack), PopI(\Stack), PopI(\Stack)) : EndCall()
SysCall(Minimal_Misc_ByteFillMemory) : FillMemory(PopI(\Stack), PopI(\Stack), PopB(\Stack)) : EndCall()
SysCall(Minimal_Misc_WordFillMemory) : FillMemory(PopI(\Stack), PopI(\Stack), PopW(\Stack), #PB_Word):EndCall()
SysCall(Minimal_Misc_LongFillMemory) : FillMemory(PopI(\Stack), PopI(\Stack), PopL(\Stack), #PB_Long):EndCall()
EndWith
;}
;} End table
; ---------------------------------

;{ Procedures
; -Math & Logic-
Macro __CheckBit(Sequence, BitIndex) ; Pesudo-procedure.
(Sequence & (1 << BitIndex)) >> BitIndex
EndMacro

Procedure.s CatchString(*PtrRef.Integer)
Define String.s = PeekS(*PtrRef\I)
*PtrRef\I + StringByteLength(String) + SizeOf(Character)
ProcedureReturn String
EndProcedure

Procedure CatchInteger(*PtrRef.Integer)
Define *IntPtr.Integer = *PtrRef\I
*PtrRef\I + SizeOf(Integer)
ProcedureReturn *IntPtr\I
EndProcedure

Procedure.s ParseString(*FilePtr)
Repeat ; Reading loop.
Define StrBuf.s, Char.c = ReadCharacter(*FilePtr)
Select Char ; Analyzing input.
Case '' : ProcedureReturn StrBuf
Default : StrBuf + Chr(Char)
EndSelect
ForEver 
EndProcedure

; -VM core management-
ProcedureDLL InitVM()
With System
\GStack = AllocateStack()
\GSMutex = CreateMutex()
\SCMutex = CreateMutex()
EndWith
EndProcedure

; -Threadz management-
ProcedureDLL.s FormatExitReason(*Thread.ThreadData)
With *Thread
Select \ExitCode
Case #cInvalidOperand : ProcedureReturn "invalid operand encountered: " + Hex(\IP\u)
Case #cInvalidCall    : ProcedureReturn "invalid system call requested"
EndSelect
EndWith
EndProcedure

Macro __ReportError(TT) ; Pseudo-procedure
Define ErrMsg.s
If *ThisThread\ExitCode : ErrMsg = FormatExitReason(*ThisThread)
Else : ErrMsg = LCase(ErrorMessage(ErrorCode()))
If ErrorCode() = #PB_OnError_InvalidMemory
ErrMsg + " ($" + Hex(ErrorTargetAddress()) + ")"
EndIf
EndIf
If ErrMsg : ErrMsg = "Reason: " + ErrMsg + " !" + #CR$
ErrMsg + "Execution was teminated at IP = $" + Hex(TT\IP) + " [off:" + Hex(TT\IP-TT\CS) + "]"
MessageRequester("[StasisVM] critical error:", ErrMsg) : End
EndIf
EndMacro

ProcedureDLL Execute(*Thread.ThreadData)
*ThisThread = *Thread
OnErrorGoto(?__Finish)
With *ThisThread
Repeat ; Execution loop.
If \IP\u > #InstructionsCount : \ExitCode = #cInvalidOperand : Break : EndIf
Define *JMPDest = System\JMPTable[\IP\u]
\IP + #OpCodeSize ; Move to next instruction.
EnableASM : JMP *JMPDest : DisableASM
! __Ret: ; Returning destination.
Until \ExitCode
EndWith
__Finish: :__ReportError(*ThisThread)
EndProcedure

Macro __ThreadMaker__()
If *CodeSeg
AddElement(System\Threadz())
Define *Thread.ThreadData = System\Threadz()
*Thread\Stack = AllocateStack()
*Thread\RStack = AllocateStack()
*Thread\SStack = AllocateStack()
*Thread\CS = *CodeSeg
*Thread\IP = *IP
PushI(*Thread\Stack, Param)
Execute(*Thread) ; Temporary !
ProcedureReturn *Thread
EndIf
EndMacro

ProcedureDLL NewThread(*CodeSeg, *IP = 0, Param.i = #Null)
If *IP = 0 : *IP = *CodeSeg : EndIf ; Correct pointer.
__ThreadMaker__()
EndProcedure

ProcedureDLL ThreadFromHeader(*HD.HeaderData)
Define Param, *CodeSeg = *HD\CodeStart, *IP = *HD\EntryPoint
__ThreadMaker__()
EndProcedure

ProcedureDLL ClearDeadThreadz()
With System
ForEach \Threadz()
If \Threadz()\ExitCode : DeleteElement(\Threadz()) : EndIf 
Next
EndWith
EndProcedure

Macro __HeaderCheck__()
If *HD\Signature = #HeaderSig
If __CheckBit(*HD\BitFlags, #b64bitTarget) = SizeOf(Integer) >> 3
If __CheckBit(*HD\BitFlags, #bUnicodeTarget) = SizeOf(Character) >> 1
If *HD\DataSize >= 0 And *HD\EntryPoint >= 0
If *HD\CodeSize > 0 
*HD\CodeStart = AllocateMemory(*HD\CodeSize + *HD\ImportTable * SizeOf(Integer) + *HD\DataSize)
EndMacro

Macro __ReturnError__()
FreeMemory(*HD\CodeStart)
FreeMemory(*HD)
ProcedureReturn #Null
EndMacro

Macro __DoExport__(Source, StrReader, IntReader)
While *HD\ExportTable : *HD\ExportTable - 1
AddSysCall(FormatSCName(StrReader(Source)), *HD\CodeStart + IntReader(Source), #False)
Wend
EndMacro

Macro __DoImport__(Source, StrReader)
Define *ITable.Integer = *HD\CodeStart + *HD\CodeSize
While *HD\ImportTable ; Scanning table.
*ITable\I = System\SysCalls(FormatSCName(StrReader(Source)))
If *ITable\I = #Null : __ReturnError__() : EndIf
*ITable + SizeOf(Integer) : *HD\ImportTable - 1
Wend
EndMacro

Macro __ExtractionFinale__()
*HD\EntryPoint + *HD\CodeStart
ProcedureReturn *HD
EndIf
EndIf
EndIf
EndIf
EndIf
FreeMemory(*HD)
EndMacro

ProcedureDLL ParseHeader(FileName.S)
Define *FPtr = ReadFile(#PB_Any, FileName)
Define *HD.HeaderData
If *FPtr : *HD = AllocateMemory(SizeOf(HeaderData))
ReadData(*FPtr, *HD, SizeOf(HeaderData))
__HeaderCheck__()
ReadData(*FPtr, *HD\CodeStart, *HD\CodeSize)
__DoExport__(*FPtr, ParseString, ReadInteger)
__DoImport__(*FPtr, ParseString)
__ExtractionFinale__()
CloseFile(*FPtr)
EndIf
EndProcedure

ProcedureDLL CatchHeader(*Start)
Define *HD.HeaderData = AllocateMemory(SizeOf(HeaderData))
CopyMemory(*Start, *HD, SizeOf(HeaderData))
__HeaderCheck__()
CopyMemory(*Start + SizeOf(HeaderData), *HD\CodeStart, *HD\CodeSize)
Define *SrcPtr = *Start + SizeOf(HeaderData) + *HD\CodeSize
__DoExport__(@*SrcPtr, CatchString, CatchInteger)
__DoImport__(@*SrcPtr, CatchString)
__ExtractionFinale__()
EndProcedure

ProcedureDLL FreeHeader(*HD.HeaderData)
FreeMemory(*HD\CodeStart)
FreeMemory(*HD)
EndProcedure
;} EndProcedures

; ==Temporary GUI==
InitVM()
OpenConsole()
ConsoleTitle("[StasisVM] debugging console:")
Define *HD, FName.s = ProgramParameter()
If FName = "" : ReInput: :#FPattern = "Executable images (*.sVM)|*.sVM|All files (*.*)|*.*"
FName = Trim(OpenFileRequester("Select image for execution:", "", #FPattern, 0) )
If FName = "" : End : EndIf
EndIf : *HD = ParseHeader(FName)
If *HD : ConsoleColor(10, 0) : PrintN("Executing '" + FName + "'..." + #CRLF$)
Else : ConsoleColor(12, 0) : PrintN("ERROR: '" + FName + "' couldn't be loaded !") : Goto ReInput
EndIf : ConsoleColor(7, 0)
ThreadFromHeader(*HD)
; IDE Options = PureBasic 5.70 LTS (Windows - x86)
; Folding = DgTn9--
; EnableXP
; Executable = ..\StasisVM.exe
; CurrentDirectory = ..\