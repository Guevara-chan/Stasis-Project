; *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*
; StasisForth assembler v0.095 (Alpha)
; Developed in 2009 by Guevara-chan.
; *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*

;{ =[TO.DO]=
; Добавить поддержку строк фиксированной длины.
; Добавить возможность передачи массивов по указателю.
; Добавить возможность задавать изначальные значения переменным.
; Оптимизировать генерацию блоков локальных данных для процедур.
; Добавить директивы условной компиляции и циклы в интерпретатор.
; Добавить "ленивую" версию макросов и привести к ним внутренние.
; Добавить возможность поолучения следующего слова как аргумента.
; Подкорректировать возвращаемые адреса для включенных напрямую ресурсов.
; Улучшить поддержку механизмов ранней инциализации (в т.ч. для блоков переменных).
; Добавить возможность получения адреса слов по смещению.
; Добавить возможность отключения поддержки вложенных комментариев.
; Добавить возможность упаковки не используемых одновременно переменных.
; Оптимизировать блок {SWITCH:..;SWITCH с использованием указателей.
; Оптимизировать цикл [FOREACH:..NEXT] с использованием указателей.
; Добавить блок MOLD[..]MOLD для приведения ряда значений к строке.
; Улучшить вывод ошибок для некорректных числовых литерелов.
; Добавить тип данных для последовательностей.
; Продумать и вернуть операции поиска слов.
; Добавить поддержку перечислений (Enumerations).
; Добавить представление кода в качестве последовательностей.
; Улучшить поддержку управляющих символов в строках.
; Добавить возможность задания имени выходному файлу.
; Перевести встроенные слова в заголовочные файлы.
; Увеличить информативность сообщений об ошибках.
; Добавить поддержку связанных списков (ListOf).
; Заменить вызовы AddMapElement/FindMapElement.
; Доделать и улучшить поддержку Unicode/UTF8.
; Добавить многомерные массивы.
; Добавить поддержку структур.
; Добавить поддержку x64.
; Доработать GUI.
;} {End/TO.DO}

; --Preparations--
EnableExplicit
IncludeFile "StasisBase.pb"
IncludeFile "StackLib.pb"

;{ [Definitions]
;{ --Enumerations--
Enumeration ; Construction types.
#cBreak    ; Unconditional loop breaker.
#cIfBreak  ; Conditional loop breaker.
#cPass     ; Unconditional iteration skip.
#cIfPass   ; Conditional iteration skip.
#cIf       ; Condition starter (IF..(ELSE)..THEN).
#cBlockIf  ; Complex condition starter ([IF..(ELSE)..ENDIF]).
#cElse     ; Condition modifier (IF..(ELSE)..THEN).
#cBegin    ; Infinite loop starter (BEGIN..AGAIN).
#cTimes    ; Finite loop starter (TIMES[..]TIMES).
#cFor      ; Complex finite loop starter (FOR[..]ROF).
#cForEach  ; Array processing loop starter ([FOREACH:..NEXT]).
#cSkip     ; Code skipping block (SKIP[..]SKIP).
#cSwtOn    ; Complex condition case starter (;ON{..}DO:).
#cSwtDo    ; Complex condition case finisher (;ON{..}DO:).
#cDefault  ; Default case marker (}DO:..(;DEFAULT:)..;SWITCH).
#cSequence ; Data sequence block ({..}).
EndEnumeration

Enumeration 1 ; Error codes.
; -Assembling stage errors-
#eNoBegin    ; AGAIN without corresponding BEGIN
#eUntilOut   ; UNTIL without corresponding BEGIN
#eEndOut     ; END without corresponding BEGIN
#eNoFor      ; ]ROF without corresponding FOR[
#eElseNoIF   ; ELSE without corresponding ([)IF(Z)
#eThenNoIF   ; THEN without corresponding IF(Z)
#eEndIfOut   ; ENDIF] without corresponding [IF(Z)
#eNoTimes    ; ]TIMES without corresponding TIMES[
#eNoSkip     ; ]SKIP without corresponding SKIP[
#eNoForEach  ; NEXT] without corresponding [FOREACH:
#eSwitchNoDO ; ;SWITCH without corresponding }DO: or :DEFAULT;
#eONnoDO     ; ;ON{ without corresponding }DO:
#eDEFnoDO    ; ;DEFAULT: without corresponding }DO:
#eDOnoON     ; }DO: without corresponding ;ON{ or {SWITCH:
#eNoColon    ; ';' without corresponding ':'
#eNoContext  ; CONTEXT] without corresponding [CONTEXT:
#eNoBlock    ; ]] without corresponding [[
#eNoSeq      ; } without corresponding {
#eNoMold     ; ]MOLD without corresponding MOLD[
#eNoLambda   ; }] without corresponding [{
#eBreakOut   ; (?)BREAK outside of breakable construction
#ePassOut    ; (?)PASS outside of loop
#eReDefined  ; %word% already defined.
#eCntrUFlow  ; %name% - requested counter's level underflow
#eStepOut    ; [STEP] outside of FOR[..]ROF block
#eLimiterOut ; [LIM] outside of FOR[..]ROF block
#eSTmpOut    ; [SWITCHER] outside of {SWITCH:..;SWITCH block
#eITmpOut    ; [*] outside of [FOREACH..NEXT] block
#eUndefined  ; Undefined word.
#eEarlyReq   ; Word requested before definition.
#eFakeVal    ; Reference to undefined variable %word%
#eInvPReq    ; Pointer request for undefined word %word%
#eInvBReq    ; Body request for undefined word %word%
#eNoInclude  ; Unable to include %filename%
#eNoResource ; Unable to merge %filename%
#eNoDir      ; Invalid directory specified
#eInvSeq     ; Invalid data sequence specified
#eInvDelim   ; Invalid path delimiter
#eFakeResLbl ; Reference to undefined resource label
#eFakeProc   ; Reference to undefined procedure
#eMismatch   ; Word's type mismathes requested operation
#eNestColon  ; Nested colon definitons doesn't supported
#eReturnOut  ; RETURN outside of colon definition
#eNonIsoProc ; Info request for non-isolated procedure %procname%
#eVoidOut    ; ]VOID without corresponding VOID[
#eInterOut   ; ]> without corresponding <[
#eColonVoid1 ; VOID[ inside colon definition
#eColonVoid2 ; ]VOID inside colon definition
#eStaticOut  ; ]STAIC without corresponding STATIC[
#eStaticCln1 ; STATIC[ outside of colon definition
#eStaticCln2 ; ]STATIC outside of colon definition
#ePublicOut  ; ]PUBLIC[ outside of context definition level
#ePrivateOut ; ]PRIVATE[ outside of context definition level
#eForgetIt   ; Unable to forget about context inside it's definition
#eRootDef    ; Unable to redefine or extend ROOT context
#eNoValName  ; Invalid value name '%word%'
#eInvLitSize ; Literal size mismatch: %word%
#eProcEntry  ; Unable to set entry point inside procedure
#eInvEncod   ; Invalid source file encoding
#eStaticSize ; Unable to resize on run-time static array %word%
#eNegArrSize ; Unable to define negative sized array
; -Illegal words-
#eRem1Out  ; ) without corresponding (
#eRem2Out  ; */ without corresponding /*
#eDefOut   ; ]DEF without corresponding DEF[
; -State checking errors-
#eNoLabelName     ; No name for label presented 
#eNoVarName       ; No name for variable presented
#eNoConstName     ; No name for constant presented
#eNoMacroName     ; No name for macro presented.
#eNoStrVarName    ; No name for string variable presented
#eNoArrName       ; No name for array presented
#eNoProcName      ; No name for procedure presented
#eNoWordPtr       ; No word for pointer request presented
#eNoWordSize      ; No word for size request presented
#eNoResLabelName  ; No name for resource label presented
#eNoContextName   ; No name for context presented
#eNoSysCaller     ; No name for caller peresented
#eNoWordForEach   ; No data collection for loop iteration presented
#eNoSwitcher      ; No switcher presented
#eUnfinishedStr   ; Unfinished string literal
#eUnfinishedRem   ; Unfinished comment block
#eUnfinishedInc   ; Unfinished inclusion directive
#eUnfinishedIPath ; Unfinished inclusion path
#eUnfinishedMerge ; Unfinished merging directive
#eUnfinishedDelim ; Unfinished path delimiter
#eNoResFile       ; No filename for merging specified
; -Constructions checking errors-
#eNoAgain    ; BEGIN without corresponding AGAIN
#eNoRof      ; FOR[ without corresponding ]ROF
#eNoNext     ; [FOREACH: without corresponding NEXT]
#eNoThen     ; IF(Z) without corresponding THEN
#eNoEndif    ; [IF(Z) without corresponding ENDIF]
#eTimesOut   ; TIMES[ without corresponding ]TIMES
#eSkipOut    ; SKIP[ without corresponding ]SKIP
#eSwitchOut  ; {SWITCH: without corresponding ;SWITCH
#eColonOut   ; ':' without corresponding ';'
#eContextOut ; [CONTEXT: without corresponding CONTEXT]
#eBlockOut   ; [[ without corresponding ]]
#eSeqOut     ; { without corresponding }
#eMoldOut    ; MOLD[ without corresponding ]MOLD
#eLambdaOut  ; [{ without corresponding }]
#eNoVoid     ; VOID[ without corresponding ]VOID
#eNoStatic   ; STATIC[ without corresponding ]STATIC
#eNoDef      ; DEF[ without corresponding ]DEF
#eNoInter    ; <[ without corresponding ]>
; -Intepretation mode errors-
#eUnableInter  ; Unable to interpret %word%
#eISUnderflow  ; Interpretation stack underflow
#eConstantOut  ; Constant definition outside of interpretation block
#eMacroOut     ; Dynamical macro definition outside of interpretation block
#eSysCallerOut ; Dynamical caller definition outside of interpretation block
#eStaticArrOut ; Statical array definition outside of interpretation block
#eInvTypeCast  ; Unable to numerize %word%
#eMacroInfin   ; Unable to recursively expand %word%
#eCustomErr=-1 ; Coder-defined error
EndEnumeration

Enumeration ; Parsing states.
#sNormal         ; Default state.
#sCommented1     ; Commented block (with brackets).
#sCommented2     ; Commented block (with /*..*/).
#sLabelName      ; Parsing label's name.
#sProcedureName  ; Parsing procedure's name.
#sVariableName   ; Parsing variable's name to define.
#sVariableWrite  ; Parsing variable's name to reference.
#sPtrRequest     ; Parsing word's name for pointer extraction.
#sSizeRequest    ; Parsing word's name for size extraction.
#sBodyRequest    ; Parsing word's name for body extraction.
#sProtoGroup     ; Parsing group of protypes names to define.
#sValueGroup     ; Parsing group of varibale names to define.
#sArrayName      ; Parsing name for array to define.
#sArrayRequest   ; Parsing name of array to request operation.
#sStrRequest     ; Parsing name of string variable to request operation.
#sResLabelName   ; Parsing name to define resource label.
#sResLabelInfo   ; Parsing name of resource label to extract info.
#sIsoProcInfo    ; Parsing name of isolated procedure еo extract info.
#sCollectionReq  ; Parsing name of collection to iterate.
#sContextName    ; Parsing name of context to define.
#sSwitcherName   ; Parsing name of switcher.
#sConstantName   ; Parsing name of constant.
#sMacroName      ; Parsing name of macro.
#sSysCallerName  ; Parsing name of caller.
#sNextWord       ; Parsing name of next word.
#sWordRequest    ; Parsing wordname to check definition.
EndEnumeration

Enumeration ; Regular expressions.
#rExtension ; Extensions parser.
#rIsNumber  ; Numeral parser.
EndEnumeration

Enumeration ; Word types.
#wLabel
#wVariable
#wArray
#wProcedure
#wResLabel
#wDataLabel
#wContext
#wLiteral
#wDefiner
#wMacro
#wSysCall
EndEnumeration

Enumeration ; Value subtypes.
#vByte
#vWord
#vChar
#vLong
#vInteger
#vQuad
#vFloat
#vDouble
#vLabelPtr
#vProcPtr
#vString
EndEnumeration

Enumeration ; Binary flags.
#b64bitTarget
#bUnicodeTarget
EndEnumeration

Enumeration ; Primitive modifiers.
#mNone
#mColon
#mPrefix
#mPostfix
EndEnumeration

; --Constants--
#OpCodeSize = SizeOf(Word)
#HeaderSig  = $5D5349534154535B ; [STASIS]
#StrPrefix  = " SLit:"
#VarPrefix  = " Var:"
#ImportPrefix   = " Import:"
#ExportPrefix   = " Export:"
#VarPtrPrefix   = " StaticPtr:"
#ResEndPrefix   = " ResEND:"
#ResSizePrefix  = " ResSIZE:"
#ResCorrector   = " ResCorrector:"
#ArrPtrPrefix   = " ArrPTR:"
#ArrSizePrefix  = " ArrSIZE:"
#ArrDataPrefix  = " ArrDATA:"
#ArrCellsPrefix = " ArrCELLS:"
#ArrSpacePrefix = " ArrSPACE:"
#LitPtrPrefix   = " LitPTR:"
#LitLocPrefix   = " LitLOC:"
#IsoEndPrefix   = " IsoEND:"
#IsoSizePrefix  = " IsoSIZE:"
#PConstPrefix   = " P-C:"
#ReturnPrefix   = " Return:"
#PoolPrefix = #PConstPrefix + "LocalPool_"
#RootContext = "root"
#OwnContext  = "own"
#RootSpace   = " " + #RootContext
#DataReq     = " :DataRequester: "
#GenLiteral  = " :Literal: "
#GenDefiner  = " :Definer: "
#GenCaller   = " :SysCall: "
#VoidWord    = " :Word: "
#Uninitialized = -1
#NotArray = -1
#Isolated = -1
#NSDelimiter = #LF
#NSDelimiter$ = #LF$
#ITDelimiter = #TAB
#ITDelimiter$ = #TAB$
#MacroDelimiter = "  "
#Paranoic = 2
#PostPonedCall = 'Post'
#IntegerFlag = #Null ; Reserved.
; -Internal variables-
#ITemplate = "[*]"
#STemplate = "[switcher]"
#StepVar   = "[step]"
#LimitVar  = "[lim]"
; -Thank you, Fred !-
#Xorrection = "Kill him with fire"
;}
;{ --Structures--
Structure PositionData
LineNum.i ; Number of line in source file.
SrcName.s ; Displayed name of source file.
EndStructure

Structure MacroData
FullName.s ; Full name of macro (with path).
Name.s     ; Short name of macro (to visualize).
Rest.i     ; Number of character left to parsing.
EndStructure

Structure ConstructionData
Type.i   ; Construction's type.
Base.i   ; Construction's start.
Var.s    ; Name of first bound variable (if presented).
Var2.s   ; Name of second bound variable (if presented).
Loc.PositionData ; Location, where construction was defined.
EndStructure

Structure FixData
Base.i      ; Position for fixing.
Label.s     ; Corresponding label.
NotFound.i  ; Code of error to throw if label not found.
ITable.s    ; Import table on the moment where fixing was requested.
NameSpace.s ; Export space on the moment where fixing was requested.
Extra.i     ; Extra integer value for special cases.
Isolated.i  ; Flag for fixez in isolated code layer.
Loc.PositionData ; Location, where fix was requested.
EndStructure

Structure WordData
Type.i     ; Type of word.
StructureUnion ; Primary bound value.
IValue.i   ; Corresponding integer value.
QValue.q   ; Corresponding quad value.
DValue.d   ; Corresponding double value.
EndStructureUnion
SValue.s   ; Corresponding string value.
StructureUnion ; Secondary bound value.
Extra.q    ; Extra integer value for special cases.
Storage.i  ; Associated data storage's index.
TypeID.q   ; Literal's type identifier.
IsLazy.i   ; Laziness macro flag.
EndStructureUnion
Public.i   ; Flag for public definitions.
*ChainLink.WordData ; Link to corresponding word.
EndStructure

Structure SourceData
*File        ; Current source handle.
FullName.s   ; Full path to source file.
CharIncome.i ; Mode for character parsers.
ICharSize.i  ; Size of incoming character data (in bytes).
Loc.PositionData ; Location in source file.
EndStructure

Structure ResFileData
*Start ; Pointer to beginning of resource data.
Size.i ; Size of resource data.
EndStructure

Structure NameSpaceData
Name.s   ; Name of context (prefix).
ErrNum.i ; Error, generated in case of non-closing.
ITable.s ; Import table on moment of namespace definition.
PDef.i   ; Old state of 'DefPublic' flag.
Loc.PositionData ; Location, where exporting was started.
EndStructure

Structure Primitive
OpCode.u    ; Binary presentation.
IntVer.a    ; Flag for integer mutability.
FloatVer.a  ; Flag for float mutability.
NamingDir.a ; Naming convention directive.
EndStructure

Structure CompilerData
State.i        ; Parsing state.
SubState.q     ; Subtype of state.
PrevState.i    ; Previous parsing state.
Nested.i       ; Level of comment nesting.
ThisWord.S     ; Last parsed word.
TrueName.s     ; Long name of word.
OutputPath.s   ; Name of output directory.
*OutputFile    ; Output binary.
OutputName.s   ; Name of output file.
Offset.i       ; Current offset in output.
Reparse.i      ; Reparsing flag.
LoopsLvl.i     ; Current nest level of loops.
Warnings.i     ; Counter for encountered alerts.
Supressed.i    ; Counter for supressed alerts.
TmpSeed.i      ; Base counter for temporary labels.
Interpreting.i ; Flag for interpretation mode.
EarlyInitOp.q  ; Operation ident for early initializer.
EarlyInitFlag.q; Flag for early initializtion of value.
MacroBuf.s     ; Current macro buffer.
ImportSize.i   ; Size of import table (in entries).
ExportSize.i   ; Size of export table (in entries).
WordsTotal.i   ; Counter for parsed words.
FilesTotal.i   ; Counter for parsed source files.
CountDelay.i   ; Number of chars to be parsed before next word.
; *Basic IO flags*
CharSize.i     ; Size of character data (in bytes).
IntSize.i      ; Size of character data (in bytes).
CharOutcome.i  ; Mode for character emitters.
JumpSize.i     ; Cahced size of jump.
; *Namespaces management*
ImportTable.s   ; Current importing table.
NameSpace.s     ; Current export target.
DefPublic.i     ; Flag to make global definitions.
PathDelimiter.s ; String signature for splitting paths ("\\" by default).
; *Internal variables*
StepVar.S      ; Current step variable in FOR[..]ROF loop.
LimitVar.s     ; Current limit variable in FOR[..]ROF loop.
ITemplate.S    ; Current template for iterator in [FOREACH:..NEXT] loop.
STemplate.S    ; Current template for switcher in {SWITCH:..;SWITCH loop.
; *Procedures mangement*
ProcLevel.i    ; Flag for colon compilation.
ProcLoopsLvl.i ; Nest level of loops in proc.
ProcCounters.i ; Number of created counter inside of proc.
ProcOffset.i   ; Current offset in procedure.
ProcBase.i     ; Pointer to start of current procedure.
ProcName.s     ; Name of current procedure.
ProcNum.i      ; Number of current procedure.
ProcParent.s   ; Name of context where procedure was defined.
ProcPDef.i     ; Flag for global definitions in word.
VoidProcs.i    ; Flag for omitting pool allocation in procs.
DefStatic.i    ; Flag for defining static data in procs.
*OverlayFile   ; Object file for procs.
OverlayName.s  ; Name of object file for procs.
; *Resources maganement*
*ResourcesFile ; Temporary storage for included resource.
ResFileName.s  ; Name of resources file.
ResCount.i     ; Number of merged data chunks.
ResOffset.i    ; Current offset in resources file.
*ResSection    ; Pointer to beggining of resource array.
; *Options*
ShowWarnings.i  ; Flag to warn user on alerts.
Informing.i     ; Flag for informing about remarkable changes.
StrOpt.i        ; Flag for optimizing string literals (using 'iLitSPtr').
NumOpt.i        ; Flag for optimizing numerical literals (using 'constant' ones).
TimesHeadOpt.i  ; Flag for optimizing code of times[
TimesTailOpt.i  ; Flag for optimizing code of ]times
XInclusion.i    ; Flag for non-including same source twice.
XInfusion.i     ; Flag for non-infusing same datafile twice.
ArraysGC.i      ; Flag for collecting 'dead' sequences in arrays.
ColonGC.i       ; Flag for collecting 'dead' sequences in colon definitions.
FallThrough.i   ; Flag for continuous conditions.
Addbye.i        ; Flag for addition of 'BYE' operand to the end of output binary.
DirectMerging.i ; Flag for merging resources directly to code.
BigEndianStr.i  ; Flag for literalizing string in 'Big Endian' mode.
ForceImport.i   ; Flag to force import for every declared caller.
DefDataType.q   ; Default type identifier for literals.
DefLitBase.i    ; Default base identifier for literals.
; *Locationz*
StateLoc.PositionData  ; Location, where state was changed.
StaticLoc.PositionData ; Location, where static definitions started.
VoidLoc.PositionData   ; Location, where void option was apllied.
InterLoc.PositionData  ; Location, where intpretion mode was enabled.
; *Structured fields*
*RootContext.WordData ; Pointer to root context data.
*GenDataReq.WordData  ; Pointer to generic data requester.
*GenLiteral.WordData  ; Pointer to generic data storage.
*GenDefiner.WordData  ; Pointer to generic definer.
*GenCaller.WordData   ; Pointer to generic system caller.
*VoidWord.WordData    ; Pointer to completely useless word.
*Source.SourceData    ; Data of current source file.
*InterStack.StackData ; Data stack for interpretation mode.
*MacroPos.Character   ; Pointer to current character in macro buffer.
*ThisMacro.MacroData  ; Pointer to currently used macro.
*WordData.WordData    ; Data for current word.
Header.HeaderData     ; Header for output binary.
VoidMacro.MacroData   ; Generic macro accum.
; *Compound data*
Map Primitives.Primitive()            ; Primary instructions map.
Map Reflected.u()                     ; Map of reflected primitives.
Map Dictionary.WordData()             ; Compile-time definitions.
Map *IncludedFiles.SourceData()       ; List of already included files.
Map InfusedFiles.ResFileData()        ; List of already merged datafiles.
Map *ProcGCList.WordData()            ; Map for words, which require GC in procs.
List Counters.s()                     ; List of internal counter variables.
List Fixez.FixData()                  ; List of postponed code fixes.
List Sources.SourceData()             ; List of included source files.
List MacroStack.MacroData()           ; List of currently expanded macroes.
List Breakerz.ConstructionData()      ; List of breaking constructions in-use.
List Constructions.ConstructionData() ; List of currently used constructions.
List NSExportStack.NameSpaceData()    ; List of exported namespaces.
List *ImportStack.WordData()          ; List of imported system calls.
List *ExportStack.WordData()          ; List of exported system calls.
Array LocalPools.i(0)                 ; Data for all used local pools.
EndStructure

Structure UniType
TypeIdent.i ; Type identifier.
SValue.s    ; Corresponding string value.
StructureUnion ; Bound values.
QValue.q    ; Corresponding quad value.
DValue.d    ; Corresponding double value.
EndStructureUnion
EndStructure
;}
;{ --Varibales--
Global Compiler.CompilerData
;}
;} {End/Definitions}

;{ =/=/=[Parsing tables]=/=/=
;{ <<Service macros>>
Macro Void : EndMacro ; Temporary bugfix for v4.50

Macro Quotes
"
EndMacro

Macro Reflect(IName, OpCode)
Compiler\Reflected("#" + IName) = OpCode
EndMacro

Procedure.s NamePrimitive(Base.s, Modifier.a, Directive.i)
If Modifier ; Если есть, что добавлять...
Select Directive
Case #mColon   : ProcedureReturn Base + ":" + Chr(Modifier)
Case #mPrefix  : ProcedureReturn Chr(Modifier) + Base
Case #mPostfix : ProcedureReturn Base + Chr(Modifier)
EndSelect
EndIf : ProcedureReturn Base
EndProcedure

Macro PrimitiveDefiner(Base, Modifier, Dir, Code, Mutators)
Define *Prim.Primitive, Name.s = NamePrimitive(Base, Modifier, Dir)
*Prim = AddMapElement(Compiler\Primitives(), Name)
CompilerIf Modifier = '' And Dir <> #mNone
*Prim\NamingDir = Dir
*Prim\IntVer    = Mutators & %01
*Prim\FloatVer  = Mutators & %10
CompilerElse : *Prim\OpCode = Code
CompilerEndIf
Reflect(Name, Code) ; Reflecting it.
EndMacro

Macro DefPrim(IName, Code, IType, Prefix, Mutators)
PrimitiveDefiner(IName, Prefix, #mPrefix, Code#Void#IType, Mutators)
EndMacro

Macro DefRevPrim(IName, Code, IType, Postfix, Mutators)
PrimitiveDefiner(IName, Postfix, #mPostfix, Code#Void#IType, Mutators)
EndMacro

Macro DefColonPrim(IName, Code, IType, Postfix, Mutators)
PrimitiveDefiner(IName, Postfix, #mColon, Code#Void#IType, Mutators)
EndMacro

Macro DefPlainPrim(IName, Code)
PrimitiveDefiner(IName, '', #mNone, Code, 0)
EndMacro

Macro Emit(OpCode, Shift = #True) ; Pseudo-procedure
WriteUnicodeCharacter(Compiler\OutputFile, OpCode)
CompilerIf Shift : Compiler\Offset + #OpCodeSize
CompilerEndIf
EndMacro

Macro WriteCharacter(File, Number)
If Compiler\CharSize = SizeOf(Ascii) : WriteAsciiCharacter(File, Number)
Else : WriteUnicodeCharacter(File, Number) : EndIf
EndMacro

Macro WriteInteger(File, Number)
If Compiler\IntSize = SizeOf(Long) : WriteLong(File, Number)
Else : WriteQuad(File, Number) : EndIf
EndMacro
;} <<end/macros>>

; -=OpCodes definitions=-
;{ --Fully mutable instructions--
 ; Definitions block.
Macro FM_Defs(X, Y, Z) ; Definitions block.
; -Simple arithmetics-
DefPrim("+"  , #iAdd, X, Y, Z)
DefPrim("-"  , #iSub, X, Y, Z)
DefPrim("*"  , #iMul, X, Y, Z)
DefPrim("/"  , #iDiv, X, Y, Z)
DefPrim("neg", #iNeg, X, Y, Z)
DefPrim("1+" , #iInc, X, Y, Z)
DefPrim("++" , #iInc, X, Y, Z)
DefPrim("1-" , #iDec, X, Y, Z)
DefPrim("--" , #iDec, X, Y, Z)
DefPrim("abs", #iAbs, X, Y, Z)
DefPrim("sgn", #iSgn, X, Y, Z)
; -Comparsion-
DefPrim("<" , #iLess, X, Y, Z)
DefPrim(">" , #iGreater, X, Y, Z)
DefPrim("<=", #iLoE, X, Y, Z)
DefPrim("=>", #iLoE, X, Y, Z)
DefPrim(">=", #iGoE, X, Y, Z)
DefPrim("=>", #iGoE, X, Y, Z)
; -Literals-
DefColonPrim("null", #iFalse , X, Y, Z)
DefColonPrim("one" , #iTrue  , X, Y, Z)
DefColonPrim("two" , #iTwo   , X, Y, Z)
DefColonPrim("-one", #iNegOne, X, Y, Z)
; -Conversion-
CompilerIf Y ; Not works with templating.
DefRevPrim("b->", #iB2, X, Y, Z)
DefRevPrim("w->", #iW2, X, Y, Z)
DefRevPrim("c->", #iC2, X, Y, Z)
DefRevPrim("l->", #iL2, X, Y, Z)
DefRevPrim("i->", #iI2, X, Y, Z)
DefRevPrim("q->", #iQ2, X, Y, Z)
DefRevPrim("f->", #iF2, X, Y, Z)
DefRevPrim("d->", #iD2, X, Y, Z)
DefPrim("->bool", #iToBool, X, Y, Z)
CompilerEndIf
EndMacro
; Definitions for each type...
Macro FullyMutableBlock()
FM_Defs(B, 'b', %11) : FM_Defs(W, 'w', %11) : FM_Defs(C, 'c', %11) : FM_Defs(L, 'l', %11) : FM_Defs(Q, 'q', %11)
FM_Defs(I, 'i', %11) : FM_Defs(I, '' , %11) : FM_Defs(F, 'f', %11) : FM_Defs(D, 'd', %11)
EndMacro ;}
;{ --Integer-mutable instructions--
Macro IM_Defs(X, Y, Z) ; Definitions block.
DefPrim("%" , #iMod, X, Y, Z) ; Arithmetic.
; -Stack management-
DefColonPrim("dup" , #iDup , X, Y, Z)
DefColonPrim("drop", #iDrop, X, Y, Z)
DefColonPrim("swap", #iSwap, X, Y, Z)
DefColonPrim("nip" , #iNip , X, Y, Z)
DefColonPrim("over", #iOver, X, Y, Z)
DefColonPrim("rot" , #iRot , X, Y, Z)
DefColonPrim("-rot", #iRRot, X, Y, Z)
; -Memory management-
DefPrim("@", #iRead , X, Y, Z)
DefPrim("!", #iWrite, X, Y, Z)
; -Binary logic-
DefColonPrim("%and"  , #iAnd, X, Y, Z)
DefColonPrim("%or"   , #iOr, X, Y, Z)
DefColonPrim("%xor"  , #iXOr, X, Y, Z)
DefColonPrim("%not"  , #iNot, X, Y, Z)
DefPrim("<<"    , #iShl, X, Y, Z)
DefPrim(">>"    , #iShr, X, Y, Z)
DefPrim("->bin" , #iToBin, X, Y, Z)
DefPrim("->2bin", #iTo2Bin, X, Y, Z)
; -Comparsion-
DefPrim("=" , #iEqu, X, Y, Z)
DefPrim("<>", #iInequ, X, Y, Z)
DefPrim("><", #iInequ, X, Y, Z)
DefColonPrim("search", #iSearch, X, Y, Z)
EndMacro
; Definitions for each type...
Macro IntegerMutableBlock()
IM_Defs(B, 'b', 1) : IM_Defs(W, 'w', 1) : IM_Defs(L, 'l', 1) : IM_Defs(Q, 'q', 1)
IM_Defs(C, 'c', 1) : IM_Defs(I, 'i', 1) : IM_Defs(I, '' , 1)
EndMacro ;}
;{ --Float-only instructions--
Macro FO_Defs(X, Y, Z) ; Definitions block.
; -Complex arithmetics-
DefColonPrim("log"  , #iLog, X, Y, Z)
DefColonPrim("log10", #iLog10, X, Y, Z)
DefColonPrim("sqr"  , #iSqr, X, Y, Z)
DefColonPrim("round", #iRound, X, Y, Z)
DefPrim("**"   , #iPow, X, Y, Z)
; -Trigonometry-
DefColonPrim("acos", #iACos, X, Y, Z)
DefColonPrim("asin", #iASin, X, Y, Z)
DefColonPrim("atan", #iAtan, X, Y, Z)
DefColonPrim("sin" , #iSin, X, Y, Z)
DefColonPrim("cos" , #iCos, X, Y, Z)
DefColonPrim("tan" , #iTan, X, Y, Z)
EndMacro
; Definitions for each type...
Macro FloatOnlyBlock()
FO_Defs(F, 'f', %10) : FO_Defs(D, 'd', %10) : FO_Defs(F, '', %10)
EndMacro ;}
;{ --Non-mutable instructions--
Macro NonMutableBlock() ; Definitions block.
DefPlainPrim("nop", #iNop)
; -Stack management-
DefPlainPrim("dup:s" , #iDupS)
DefPlainPrim("drop:s", #iDropS)
; -Flow control-
DefPlainPrim("here", #iHere)
DefPlainPrim("jump", #iJmp)
DefPlainPrim("'if" , #iIF)
DefPlainPrim("'ifz", #iIFZ)
; -Calls management-
DefPlainPrim("exec", #iCall)
DefPlainPrim("ret" , #iRet)
DefPlainPrim("r>"  , #iPushRS)
DefPlainPrim(">r"  , #iPopRS)
; -Memory management-
DefPlainPrim("allot", #iAllot)
DefPlainPrim("free" , #iFreeMem)
DefPlainPrim("->ds" , #iPopDS)
DefPlainPrim("ds->" , #iPushDS)
DefPlainPrim("->ss" , #iPopSS)
DefPlainPrim("ss->" , #iPushSS)
; -Depth retrievment-
DefPlainPrim("depth" , #iDepth)
DefPlainPrim("rdepth", #iRDepth)
DefPlainPrim("gdepth", #iGDepth)
DefPlainPrim("cell"  , #iPtrSize)
DefPlainPrim("char"  , #iCharSize)
; -Threading-
;DefPlainPrim("newthread", #iNewThread)
;DefPlainPrim("kill"    , #iKill)
DefPlainPrim("bye"    , #iBye)
DefPlainPrim("threadz", #iThreadz)
DefPlainPrim("base"   , #iBase)
DefPlainPrim("ms"     , #iSleep)
DefPlainPrim("g>"     , #iPushGS)
DefPlainPrim(">g"     , #iPopGS)
; -Mutexes-
DefPlainPrim("mutexstate" , #iMutexState)
DefPlainPrim("lockmutex"  , #iLockMutex)
DefPlainPrim("unlockmutex", #iUnLockMutex)
DefPlainPrim("newmutex"   , #iNewMutex)
DefPlainPrim("freemutex"  , #iFreeMutex)
; -System calls-
DefPlainPrim("invoke", #iSysCall)
DefPlainPrim("invptr", #iSysCallPtr)
DefPlainPrim("sc@"   , #iPushSC)
DefPlainPrim("sc!"   , #iPopSC)
DefPlainPrim("?sc"   , #iFindSC)
EndMacro
;}
;{ --Literals--
Macro WriteLitData(TypeName, Value = 0) ; Writes data in compiler's output.
CompilerIf Defined(WriteUnicode#TypeName, #PB_Function)        : WriteCharacter(Compiler\OutputFile, Value)
Compiler\Offset + Compiler\CharSize ; Смещение.
CompilerElse : CompilerIf Defined(TypeName#Flag, #PB_Constant) : WriteInteger(Compiler\OutputFile, Value)
Compiler\Offset + Compiler\IntSize ; Смещение.
CompilerElse : Write#TypeName(Compiler\OutputFile, Value) : Compiler\Offset + SizeOf(TypeName)
CompilerEndIf
CompilerEndIf
EndMacro 

Macro DefLit(TypeLetter, TypeName, ModifyType = 0) ; Definitions block.
Procedure Literal#TypeLetter(Value.TypeLetter = 0, Optimize = #True)
If Optimize <> 'No' ; Если требуется сам литерал...
If Optimize
If Value = -1    : Emit(#iNegOne#TypeLetter) : ProcedureReturn #True
ElseIf Value = 0 : Emit(#iFalse#TypeLetter)  : ProcedureReturn #True
ElseIf Value = 1 : Emit(#iTrue#TypeLetter)   : ProcedureReturn #True
ElseIf Value = 2 : Emit(#iTwo#TypeLetter)    : ProcedureReturn #True
CompilerIf Defined(PB_#TypeLetter#nterface, #PB_Constant)
ElseIf Value = Compiler\IntSize : Emit(#iPtrSize) : ProcedureReturn #True
CompilerEndIf
EndIf
EndIf
CompilerSelect ModifyType
CompilerCase 1  : Emit(#iLitI)
CompilerCase 2  : Emit(#iLitQ)
CompilerDefault : Emit(#iLit#TypeLetter)
CompilerEndSelect
EndIf : WriteLitData(TypeName, Value) ; Записываем сами данные.
EndProcedure
EndMacro

Macro DefPtrLit(TypePrefix)
Procedure TypePrefix#Pointer(Offset.i = 0)
Emit(#i#TypePrefix#Ptr)
WriteLitData(Integer, Offset)
EndProcedure
EndMacro

; Definitions for each type...
Macro LiteralsBlock()
DefLit(B, Byte) : DefLit(W, Word) : DefLit(L, Long) : DefLit(Q, Quad)
DefLit(C, Character) : DefLit(I, Integer)
DefLit(F, Float, 1) : DefLit(D, Double, 2) ; Additional pseudo-float literals.
DefPtrLit(Abs) : DefPtrLit(Rel) : DefPtrLit(Data) : DefPtrLit(LitS) : DefPtrLit(LitI)
Reflect("litb", #iLitB)
Reflect("litc", #iLitC)
Reflect("litw", #iLitW)
Reflect("litl", #iLitL)
Reflect("litq", #iLitQ)
Reflect("liti", #iLitI)
Reflect("lits", #iLitS)
Reflect("litsptr", #iLitSPtr)
Reflect("litiptr", #iLitIPtr)
Reflect("absptr" , #iAbsPtr)
Reflect("relptr" , #iRelPtr)
Reflect("dataptr", #iDataPtr)
EndMacro
;}
;{ **Finalizer**
FullyMutableBlock()
IntegerMutableBlock()
FloatOnlyBlock()
NonMutableBlock()
LiteralsBlock()
;}
;}
;{ =/=/=[Parsing partializers]=/=/=
;{ --Predefined constants--
Macro DefPseudoConstant(Name)
Case Name : PostPoneEx(#PConstPrefix + UCase(Name), 'LitI')
EndMacro

Macro PsuedoConstantsBlock() ; Definitions.
DefPseudoConstant("#entrypoint")
DefPseudoConstant("#codesize")
DefPseudoConstant("#rdatasize")
DefPseudoConstant("#vdatasize")
DefPseudoConstant("#filesize")
EndMacro
;}
;{ --Comments--
Macro CommentsBlock() ; Partializer
Case "//", "\" : SwallowLine() ; Rest-line comment.
Case "("    : ChangeState(#sCommented1) ; Comments block (with brackets)
Case "/*"   : ChangeState(#sCommented2) ; Comments block (with /*..*/)
EndMacro
;}
;{ --Default literal type modifiers--
Macro DefTypeMod(Name, TypeID, TypeName)
Case Name : Compiler\DefDataType = TypeID 
Inform("Default data type set to '" + TypeName + "'")
EndMacro

Macro DefTypeModsBlock() ; Definitions
DefTypeMod("<deftype:b>", 'b', "byte")
DefTypeMod("<deftype:c>", 'c', "char")
DefTypeMod("<deftype:w>", 'w', "word")
DefTypeMod("<deftype:l>", 'l', "long")
DefTypeMod("<deftype:q>", 'q', "quad")
DefTypeMod("<deftype:f>", 'f', "float")
DefTypeMod("<deftype:d>", 'd', "double")
DefTypeMod("<deftype:i>", 'i', "integer")
DefTypeMod("<deftype:abs>", ':Abs', "absolute code pointer")
DefTypeMod("<deftype:rel>", ':Rel', "relative pointer")
DefTypeMod("<deftype:dat>", ':Dat', "dataspace pointer")
DefTypeMod("<deftype:str>", ':Str', "string pointer")
DefTypeMod("<deftype:int>", ':Int', "integer pointer")
EndMacro
;}
;}
;{ =/=/=[Procedures]=/=/=
;{ **Declarations**
Declare.s RecallStr()
Declare.i RecallInt()
Declare EmitSysCall(CallName.s)
Declare LiteralS(Text.S, Optimize = #True)
Declare UniPusher(TypeIdent, QValue.q, DValue.d = 0, SValue.s = "")
;}
;{ --Math&Logic--
Macro LCut(Text, Chars = 1) ; Pesudo-procedure.
Mid(Text, 1 + Chars)
EndMacro

Macro RCut(Text, Chars = 1) ; Pesudo-procedure.
Left(Text, Len(Text) - Chars)
EndMacro

Macro TypeOut(Text, Color = 7)
ConsoleColor(Color, 0) : PrintN(Text)
EndMacro

Procedure.s CombinePaths(MainPath.s, Addition.s)
CompilerSelect #PB_Compiler_OS
CompilerCase #PB_OS_Windows
Define Result.S{#MAX_PATH}
PathCombine_(@Result, @MainPath, @Addition)
ProcedureReturn Result
CompilerEndSelect
EndProcedure

Macro GetAbsolutePath(RelPath) ; Pesudo-procedure.
CombinePaths(GetCurrentDirectory(), RelPath)
EndMacro

Macro GetOutputPath(RelPath) ; Pesudo-procedure.
CombinePaths(Compiler\OutputPath, RelPath)
EndMacro

Macro SetBit(Sequence, BitIndex) ; Pesudo-procedure.
Sequence | (1 << BitIndex)
EndMacro

Procedure AppendFile(*BaseFile, Addition.s)
#BufferSize = 1024 * 64
Define *Source = ReadFile(#PB_Any, Addition)
If *Source
Define FSize, *Buffer = AllocateMemory(#BufferSize)
While Not Eof(*Source)
WriteData(*BaseFile, *Buffer, ReadData(*Source, *Buffer, #BufferSize))
Wend : FreeMemory(*Buffer)
FSize = Lof(*Source)
CloseFile(*Source)
ProcedureReturn FSize
Else : ProcedureReturn -1
EndIf
EndProcedure

Macro RemoveExtension(FileName) ; Pseudo-procedure.
ReplaceRegularExpression(#rExtension, FileName, "")
EndMacro

Procedure.S AddExtension(FileName.s, Ext.S)
Define *Char.Character = @FileName + StringByteLength(FileName) - SizeOf(Character)
If *Char\C <> '.' : FileName + "." : EndIf
ProcedureReturn FileNAme + Ext.S
EndProcedure

Procedure.s RefactoreString(Text.s, Limited = #False)
Define *ScanPos.Character = @Text
Define *DestPos.Character = @Text
With *ScanPos
While \C  ; Сканируем строку.
Select \C ; Анализ символа.
Case #LF, #CR, #TAB ; NOP
Default ; Other characters...
If Limited = #False Or \C <> ' '
*DestPos\C = \C : *DestPos + SizeOf(Character)
EndIf
EndSelect
*ScanPos + SizeOf(Character)
Wend : ProcedureReturn RTrim(Text)
EndWith
EndProcedure

Procedure.s SafePeekS(*Ptr)
If *Ptr : ProcedureReturn PeekS(*Ptr) : EndIf
EndProcedure

Procedure Max(A, B)
If A > B : ProcedureReturn A
Else     : ProcedureReturn B
EndIf
EndProcedure
;}
;{ --GUI management--
Macro FinishWork() ; Pseudo-procedure.
If IsFile(Compiler\ResourcesFile)
CloseFile(Compiler\ResourcesFile)
EndIf
If IsFile(Compiler\OverlayFile)
CloseFile(Compiler\OverlayFile)
EndIf
DeleteFile(Compiler\ResFileName)
DeleteFile(Compiler\OverlayName)
TypeOut("<Exiting in 3 seconds>")
Delay(3000)
End
EndMacro

Macro BrokenStruct(Presented, Missing) ; Pseudo-procedure.
Presented + " without corresponding " + Missing
EndMacro

Macro Outsider(Word, NormalEnv)
Word + " outside of " + NormalEnv
EndMacro

Macro NoName(MissingWord)
"No name for " + MissingWord + " presented"
EndMacro

Procedure.s ErrorNum2Message(ENumber.i)
Select ENumber
; -Assembling stage errors-
Case #eNoBegin    : ProcedureReturn BrokenStruct("AGAIN"    , "BEGIN")
Case #eUntilOut   : ProcedureReturn BrokenStruct("UNTIL"    , "BEGIN")
Case #eEndOut     : ProcedureReturn BrokenStruct("END"      , "BEGIN")
Case #eNoFor      : ProcedureReturn BrokenStruct("]ROF"     , "FOR[")
Case #eElseNoIF   : ProcedureReturn BrokenStruct("ELSE"     , "([)IF(Z)")
Case #eThenNoIF   : ProcedureReturn BrokenStruct("THEN"     , "IF(Z)")
Case #eEndIfOut   : ProcedureReturn BrokenStruct("ENDIF]"   , "[IF(Z)")
Case #eNoTimes    : ProcedureReturn BrokenStruct("]TIMES"   , "TIMES[")
Case #eNoSkip     : ProcedureReturn BrokenStruct("]SKIP"    , "SKIP[")
Case #eNoForEach  : ProcedureReturn BrokenStruct("]NEXT"    , "[FOREACH:")
Case #eSwitchNoDO : ProcedureReturn BrokenStruct(";SWITCH"  , "}DO: or :DEFAULT;")
Case #eONnoDO     : ProcedureReturn BrokenStruct(";ON{"     , "}DO:")
Case #eDEFnoDO    : ProcedureReturn BrokenStruct(";DEFAULT:", "}DO:")
Case #eDOnoON     : ProcedureReturn BrokenStruct("}DO:"     , ";ON{ or {SWITCH:")
Case #eNoColon    : ProcedureReturn BrokenStruct("';'"      , "':'")
Case #eNoContext  : ProcedureReturn BrokenStruct("CONTEXT]" , "[CONTEXT:")
Case #eNoBlock    : ProcedureReturn BrokenStruct("]]"       , "[[")
Case #eNoSeq      : ProcedureReturn BrokenStruct("}"        , "{")
Case #eNoMold     : ProcedureReturn BrokenStruct("]MOLD"    , "MOLD[")
Case #eNoLambda   : ProcedureReturn BrokenStruct("}]"       , "[{")
Case #eBreakOut   : ProcedureReturn Outsider("(?)BREAK", "breakable construction")
Case #ePassOut    : ProcedureReturn Outsider("(?)PASS" , "loop")
Case #eStepOut    : ProcedureReturn Outsider(#StepVar  , "FOR[..]ROF block")
Case #eLimiterOut : ProcedureReturn Outsider(#LimitVar , "FOR[..]ROF block")
Case #eITmpOut    : ProcedureReturn Outsider(#ITemplate, "[FOREACH..NEXT] block")
Case #eSTmpOut    : ProcedureReturn Outsider(#STemplate, "{SWITCH:..;SWITCH block")
Case #eInvSeq     : ProcedureReturn "Invalid data sequence specified"
Case #eNoResFile  : ProcedureReturn "No filename for merging specified"
Case #eInvDelim   : ProcedureReturn "Invalid path delimiter"
Case #eNestColon  : ProcedureReturn "Nested colon definitions doesn't supported"
Case #eReturnOut  : ProcedureReturn Outsider("RETURN"   , "colon definition")
Case #eVoidOut    : ProcedureReturn BrokenStruct("]VOID", "VOID[")
Case #eInterOut   : ProcedureReturn BrokenStruct("]>"   , "<[")
Case #eColonVoid1 : ProcedureReturn "VOID[ inside colon definition"
Case #eColonVoid2 : ProcedureReturn "]VOID inside colon definition"
Case #eStaticOut  : ProcedureReturn BrokenStruct("]STAIC", "STATIC[")
Case #eStaticCln1 : ProcedureReturn Outsider("STATIC["  , "colon definition")
Case #eStaticCln2 : ProcedureReturn Outsider("]STATIC"  , "colon definition")
Case #ePublicOut  : ProcedureReturn Outsider("]PUBLIC[" , "context definition level")
Case #ePrivateOut : ProcedureReturn Outsider("]PRIVATE[", "context definition level")
Case #eRootDef    : ProcedureReturn "Unable to redefine or extend 'ROOT' context"
Case #eProcEntry  : ProcedureReturn "Unable to set entry point inside procedure"
Case #eInvencod   : ProcedureReturn "Invalid source file encoding"
Case #eNegArrSize : ProcedureReturn "Unable to define negative sized array"
; -Illegal words-
Case #eRem1Out  : ProcedureReturn BrokenStruct(")"   , "(")
Case #eRem2Out  : ProcedureReturn BrokenStruct("*/"  , "/*")
Case #eDefOut   : ProcedureReturn BrokenStruct("]DEF", "DEF[")
; -State checking errors-
Case #eNoLabelName     : ProcedureReturn NoName("label")
Case #eNoVarName       : ProcedureReturn NoName("variable")
Case #eNoStrVarName    : ProcedureReturn NoName("string variable")
Case #eNoArrName       : ProcedureReturn NoName("array")
Case #eNoProcName      : ProcedureReturn NoName("procedure")
Case #eNoResLabelName  : ProcedureReturn NoName("resource label")
Case #eNoContextName   : ProcedureReturn NoName("context")
Case #eNoConstName     : ProcedureReturn NoName("constant")
Case #eNoMacroName     : ProcedureReturn NoName("macro")
Case #eNoSysCaller     : ProcedureReturn NoName("caller")
Case #eNoWordForEach   : ProcedureReturn "No data collection for loop iteration presented"
Case #eNoWordPtr       : ProcedureReturn "No word for pointer request presented"
Case #eNoWordSize      : ProcedureReturn "No array or variable for size request presented"
Case #eNoSwitcher      : ProcedureReturn "No switcher presented"
Case #eUnfinishedStr   : ProcedureReturn "Unfinished string literal"
Case #eUnfinishedRem   : ProcedureReturn "Unfinished comment block"
Case #eUnfinishedInc   : ProcedureReturn "Unfinished inclusion directive"
Case #eUnfinishedIPath : ProcedureReturn "Unfinished inclusion path"
Case #eUnfinishedMerge : ProcedureReturn "Unfinished merging directive"
Case #eUnfinishedDelim : ProcedureReturn "Unfinished path delimiter"
Case #eNoDef    : ProcedureReturn BrokenStruct("DEF["   , "]DEF")
Case #eNoVoid   : ProcedureReturn BrokenStruct("VOID["  , "]VOID")
Case #eNoStatic : ProcedureReturn BrokenStruct("STATIC[", "]STATIC")
Case #eNoInter  : ProcedureReturn BrokenStruct("<["     , "]>")
; -Constructions checking errors-
Case #eNoAgain     : ProcedureReturn BrokenStruct("BEGIN"    , "AGAIN, UNTIL or END")
Case #eNoRof       : ProcedureReturn BrokenStruct("FOR["     , "]ROF")
Case #eNoNext      : ProcedureReturn BrokenStruct("[FOREACH:", "NEXT]")
Case #eNoThen      : ProcedureReturn BrokenStruct("IF(Z)"    , "THEN")
Case #eNoEndif     : ProcedureReturn BrokenStruct("[IF(Z)"   , "ENDIF]")
Case #eTimesOut    : ProcedureReturn BrokenStruct("TIMES["   , "]TIMES")
Case #eSkipOut     : ProcedureReturn BrokenStruct("SKIP["    , "]SKIP")
Case #eSwitchOut   : ProcedureReturn BrokenStruct("{SWITCH:" , ";SWITCH")
Case #eColonOut    : ProcedureReturn BrokenStruct("':'"      , "';'")
Case #eContextOut  : ProcedureReturn BrokenStruct("[CONTEXT:", "CONTEXT]")
Case #eBlockOut    : ProcedureReturn BrokenStruct("[["       , "]]")
Case #eSeqOut      : ProcedureReturn BrokenStruct("{"        , "}")
Case #eMoldOut     : ProcedureReturn BrokenStruct("MOLD["    , "]MOLD")
Case #eLambdaOut   : ProcedureReturn BrokenStruct("[{"       , "}]")
; -Interpretation errors-
Case #eISUnderflow  : ProcedureReturn "Interpretation stack underflow"
Case #eConstantOut  : ProcedureReturn Outsider("Constant definition"        , "interpretation block")
Case #eMacroOut     : ProcedureReturn Outsider("Dynamical macro definition" , "interpretation block")
Case #eSysCallerOut : ProcedureReturn Outsider("Dynamical caller definition", "interpretation block")
Case #eStaticArrOut : ProcedureReturn Outsider("Statical array definition"  , "interpretation block")
EndSelect
EndProcedure

Macro FormatLocation(Line, File) ; Pseudo-procedure.
" on line #" + Str(Line) + " from '" + File + "'"
EndMacro

Macro ExpandLocation() ; Partializer
If SLine = -1 : SLine = Compiler\Source\Loc\LineNum : EndIf
If SourceName = "" : SourceName = Compiler\Source\Loc\SrcName : EndIf
EndMacro

Procedure ThrowErrorEx(ENumber.i, SLine = -1, SourceName.s= "",  ErrMsg.s = "")
If ErrMsg = "" : ErrMsg = ErrorNum2Message(ENumber) : EndIf
With Compiler
If ENumber = #eCustomErr : TypeOut("ERROR: " + ErrMsg, 12) 
Else : TypeOut("ERROR " + Str(ENumber) + ": " + ErrMsg + " !", 12) 
EndIf : ExpandLocation()
TypeOut("...assembling was halted" + FormatLocation(SLine, SourceName), 12)
CloseFile(\OutputFile)
DeleteFile(\OutputName)
FinishWork()
EndWith
EndProcedure

Macro ThrowError(ENumber, ErrMsg = "") ; Pseudo-procedure.
ThrowErrorEx(ENumber, -1, "", ErrMsg)
EndMacro

Macro TypeMismatchError(WName) ; Partializer.
ThrowError(#eMismatch, "'" + WName + "' - word mismatches requested operation")
EndMacro

Macro UndefinedWord(Category, WName) ; Partializer.
ThrowError(#eFakeVal, "Reference to undefined " + Category + " '" + WName + "'")
EndMacro

Macro UndefinedVal(WName) ; Partializer.
UndefinedWord("variable or array", WName)
EndMacro

Procedure ShowInformation(Message.s, ShowLocation.i)
TypeOut("[INFO:] " + Message + ".", 11)
If ShowLocation
With Compiler\Source\Loc
TypeOut("...It was happened" + FormatLocation(\LineNum, \SrcName), 11)
EndWith
EndIf
EndProcedure

Macro Inform(Msg, ShowLocation = #True) ; Pseudo-procedure.
If Compiler\Informing : ShowInformation(Msg, ShowLocation) : EndIf
EndMacro

Procedure Warn(Message.s, SLine = -1, SourceName.s= "")
If Compiler\ShowWarnings : ExpandLocation()
TypeOut("WARNING: " + Message + " !", 14)
TypeOut("...alert was encountered" + FormatLocation(SLine, SourceName), 14)
Else  : Compiler\Supressed + 1
EndIf : Compiler\Warnings + 1
EndProcedure

Macro OutCallAlert(LabelName, CallLine = -1, CallSrc = "") ; Pseudo-procedure.
Warn("Isolated label '" + LabelName + "' was called from outside", CallLine, CallSrc)
EndMacro

Procedure.s FormatCount(Amount.i, Naming.s)
If Amount % 10 = 1 : ProcedureReturn Str(Amount) + " " + Naming
Else               : ProcedureReturn Str(Amount) + " " + Naming + "s"
EndIf
EndProcedure

Macro WarningsInfo() ; Partializer.
If Compiler\Supressed ; Если были подавленные предупреждения.
If Compiler\Warnings = Compiler\Supressed : Define PostWarn.s = " (all supressed)"
Else : PostWarn = " (" + Str(Compiler\Supressed) + " supressed)" : EndIf
EndIf : TypeOut(FormatCount(Compiler\Warnings, "alert") + " were encountered at assembling" + PostWarn + ".", 14)
EndMacro

Procedure.s MakeArrow(Length, AddTip = #True)
If Length
Define *Char.Character, Arrow.s = Space(Length)
Define *ToFix.Character = @Arrow + StringByteLength(Arrow) - SizeOf(Character)
If AddTip : *ToFix\C = '>' : *ToFix - SizeOf(Character) : EndIf
For *Char = @Arrow To *ToFix : *Char\C = '-' : Next
ProcedureReturn Arrow
EndIf
EndProcedure

Macro ArrowSize() ; Pseudo-procedure.
ListSize(Compiler\Sources()) - 1
EndMacro

Procedure OpenSource(FileName.s)
If FileName
Define *Handle = ReadFile(#PB_Any, FileName)
If *Handle : AddElement(Compiler\Sources())
Define *NewSource.SourceData = Compiler\Sources()
With *NewSource
\File = *Handle
\Loc\SrcName = GetFilePart(FileName)
\Loc\LineNum = 1
;;;;;;;;;;;;;;;;;;;;;;;;;
\CharIncome = ReadStringFormat(*Handle)
Select \CharIncome ; Analyzing encoding...
Case #PB_Ascii, #PB_UTF8 : \ICharSize = 1
Case #PB_Unicode         : \ICharSize = 2
Default : ;ThrowError(#eInvEncoding)
EndSelect
;;;;;;;;;;;;;;;;;;;;;;;;;
\FullName = GetAbsolutePath(FileName)
AddMapElement(Compiler\IncludedFiles(), LCase(\FullName))
Compiler\IncludedFiles() = *NewSource
EndWith
Compiler\FilesTotal + 1
Compiler\Source = *NewSource
TypeOut(MakeArrow(ArrowSize()) + "Assembling '" + *NewSource\Loc\SrcName + "'... ")
ProcedureReturn #True
ElseIf UCase(Right(FileName, 3)) <> ".SF"
ProcedureReturn OpenSource(AddExtension(FileName, "SF"))
EndIf
EndIf
EndProcedure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Macro SetupCompiler() ; Pseudo-procedure.
Compiler\CharSize    = SizeOf(Ascii)
Compiler\CharOutcome = #PB_Ascii
; ---
Compiler\IntSize     = SizeOf(Long)
Compiler\JumpSize    = #OpCodeSize * 2 + Compiler\IntSize
EndMacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Macro FindInput() ; Pseudo-procedure.
Define FName.s = ProgramParameter()
If FName : Compiler\OutputPath = GetPathPart(ProgramFilename())
Else : Compiler\OutputPath = GetCurrentDirectory()
ReInput: #FPattern = "StasisForth programs (*.SF)|*.SF|Text files (*.TXT)|*.TXT|All files (*.*)|*.*"
FName = Trim(OpenFileRequester("Choose file to assemble:", "", #FPattern, 0) )
If FName = "" : End : EndIf
EndIf 
If OpenSource(FName) = #False
TypeOut("ERROR: '" + FName + "' couldn't be opened !", 12)
Goto ReInput
EndIf
EndMacro

Macro PrepareOutput() ; Pseudo-procedure.
Define FilePart.s = Compiler\Source\Loc\SrcName
FilePart = RTrim(RemoveExtension(FilePart))
Compiler\OutputName    = GetOutputPath(FilePart + ".sVM")
Compiler\OutputFile    = CreateFile(#PB_Any, Compiler\OutputName)
Compiler\ResFileName   = GetOutputPath(FilePart + ".res")
Compiler\ResourcesFile = CreateFile(#PB_Any, Compiler\ResFileName)
Compiler\OverlayName   = GetOutputPath(FilePart + ".obj")
Compiler\OverlayFile   = CreateFile(#PB_Any, Compiler\OverlayName)
WriteData(Compiler\OutputFile, Compiler\Header, SizeOf(HeaderData))
EndMacro

Macro DrawPropeller() ; Pseudo-procedure.
Define CIdx, Timer
ConsoleColor(7, 0) : Print(Mid("|/-\", CIdx + 1, 1) + #BS$)
If ElapsedMilliseconds() - Timer > 250 : Timer = ElapsedMilliseconds()
If CIdx = 3 : CIdx = 0 : Else : CIdx + 1 : EndIf
EndIf
EndMacro

Macro FormatSize(Size) ; Pseudo-procedure.
FormatCount(Size, "byte")
EndMacro

Macro EntryFormat(Amount) ; Pseudo-procedure.
" / " + Str(Amount) + " entries"
EndMacro
;}
;{ --Internal words--
Macro DefSysCall(Name, CallPath) ; Partializer.
Case Name
Compiler\GenCaller\SValue = CallPath
ProcedureReturn #GenCaller
EndMacro

Macro DefLiteral(Name, Value, Type = 'i') ; Partializer.
Case Name
CompilerIf Type = 'f' Or Type = 'd'
Compiler\GenLiteral\DValue = Value
CompilerElse : Compiler\GenLiteral\QValue = Value
CompilerEndIf
Compiler\GenLiteral\Extra = Type
ProcedureReturn #GenLiteral 
EndMacro

Macro DefSLiteral(Name, Value) ; Partializer.
Case Name
Compiler\GenLiteral\SValue = Value
Compiler\GenLiteral\Extra = 's'
ProcedureReturn #GenLiteral 
EndMacro

Macro DefCharLit(Name, Value) ; Partializer.
Case Name
Compiler\GenLiteral\QValue = Value
Compiler\GenLiteral\Extra = 'c'
ProcedureReturn #GenLiteral
DefSLiteral(Name + ":s", Chr(Value))
EndMacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Macro DefineSystemCalls() ; Partializer.
; -Debugging IO calls-
DefSysCall("type", "Minimal.Debug.TypeString")
DefSysCall("ask" , "Minimal.Debug.StringInput")
DefSysCall("key" , "Minimal.Debug.WaitKey")
; -Base string calls-
DefSysCall("s+"      , "Minimal.String.Concatenate")
DefSysCall("s="      , "Minimal.String.Compare")
DefSysCall("len"     , "Minimal.String.GetLength")
DefSysCall("s@"      , "Minimal.String.Read")
DefSysCall("s!"      , "Minimal.String.Write")
DefSysCall("swap:s"  , "Minimal.String.Swap")
DefSysCall("nip:s"   , "Minimal.String.Nip")
DefSysCall("rot:s"   , "Minimal.String.Rot")
DefSysCall("-rot:s"  , "Minimal.String.RRot")
DefSysCall("over:s"  , "Minimal.String.Over")
DefSysCall("search:s", "Minimal.String.StackSearch")
DefSysCall("box:s"   , "Minimal.String.Box")
DefSysCall("unbox:s" , "Minimal.String.UnBox")
; -Extended string operaions-
DefSysCall("left"   , "Minimal.String.LeftPart")
DefSysCall("right"  , "Minimal.String.RightPart")
DefSysCall("mid"    , "Minimal.String.MidPart")
DefSysCall("lset"   , "Minimal.String.LeftSet")
DefSysCall("rset"   , "Minimal.String.RightSet")
DefSysCall("ltrim"  , "Minimal.String.TrimLeft")
DefSysCall("rtrim"  , "Minimal.String.TrimRight")
DefSysCall("trim"   , "Minimal.String.FullTrim")
DefSysCall("spaces" , "Minimal.String.Spaces")
DefSysCall("lcase"  , "Minimal.String.ToLower")
DefSysCall("ucase"  , "Minimal.String.ToUpper")
DefSysCall("split:s", "Minimal.String.Split")
; -String conversion calls-
DefSysCall("c->s", "Minimal.String.FromChar")
DefSysCall("i->s", "Minimal.String.FromInteger")
DefSysCall("b->s", "Minimal.String.FromByte")
DefSysCall("w->s", "Minimal.String.FromWord")
DefSysCall("l->s", "Minimal.String.FromLong")
DefSysCall("q->s", "Minimal.String.FromQuad")
DefSysCall("f->s", "Minimal.String.FromFloat")
DefSysCall("d->s", "Minimal.String.FromDouble")
DefSysCall("s->c", "Minimal.String.ToChar")
DefSysCall("s->i", "Minimal.String.ToInteger")
DefSysCall("s->b", "Minimal.String.ToByte")
DefSysCall("s->w", "Minimal.String.ToWord")
DefSysCall("s->l", "Minimal.String.ToLong")
DefSysCall("s->q", "Minimal.String.ToQuad")
DefSysCall("s->f", "Minimal.String.ToFloat")
DefSysCall("s->d", "Minimal.String.ToDouble")
; -Memory management-
DefSysCall("movemem"  , "Minimal.Misc.MoveMemory")
DefSysCall("memsize"  , "Minimal.Misc.BufferSize")
DefSysCall("reallot"  , "Minimal.Misc.ResizeMemory")
DefSysCall("fillmem"  , "Minimal.Misc.ByteFillMemory")
DefSysCall("fillmem:b", "Minimal.Misc.ByteFillMemory")
DefSysCall("fillmem:w", "Minimal.Misc.WordFillMemory")
DefSysCall("fillmem:l", "Minimal.Misc.LongFillMemory")
; -Sequences management calls-
DefSysCall("{!}"     , "Minimal.Sequence.Write")
DefSysCall("{@}"     , "Minimal.Sequence.Read")
DefSysCall("{dup}"   , "Minimal.Sequence.Dup")
DefSysCall("{drop}"  , "Minimal.Sequence.Drop")
DefSysCall("{nip}"   , "Minimal.Sequence.Nip")
DefSysCall("{over}"  , "Minimal.Sequence.Over")
DefSysCall("{swap}"  , "Minimal.Sequence.Swap")
DefSysCall("{=}"     , "Minimal.Sequence.Compare")
DefSysCall("{+}"     , "Minimal.Sequence.Concatenate")
DefSysCall("{split}" , "Minimal.Sequence.Split")
DefSysCall("{mid}"   , "Minimal.Sequence.MidPart")
DefSysCall("{left}"  , "Minimal.Sequence.LeftPart")
DefSysCall("{right}" , "Minimal.Sequence.RightPart")
DefSysCall("{cut}"   , "Minimal.Sequence.Cut")
DefSysCall("{insert}", "Minimal.Sequence.Insert")
DefSysCall("{size}"  , "Minimal.Sequence.GetSize")
DefSysCall("{box}"   , "Minimal.Sequence.Box")
DefSysCall("{unbox}" , "Minimal.Sequence.UnBox")
DefSysCall("{search}", "Minimal.Sequence.StackSearch")
; -Misc calls-
DefSysCall("random"   , "Minimal.Misc.RandomInt")
DefSysCall("tickcount", "Minimal.Misc.Millisecs")
DefSysCall("osversion", "Minimal.Misc.OSVersion")
DefSysCall("osfamily" , "Minimal.Misc.Platform")
EndMacro

Macro DefineLiterals() ; Partializer.
; -OS constants literals-
DefLiteral("#os:windowsnt3.51"    , #PB_OS_Windows_NT3_51)
DefLiteral("#os:windows95"        , #PB_OS_Windows_95)
DefLiteral("#os:windowsnt4"       , #PB_OS_Windows_NT_4)
DefLiteral("#os:windows98"        , #PB_OS_Windows_98)
DefLiteral("#os:windowsme"        , #PB_OS_Windows_ME)
DefLiteral("#os:windows2000"      , #PB_OS_Windows_2000)
DefLiteral("#os:windowsxp"        , #PB_OS_Windows_XP)
DefLiteral("#os:windowsserver2003", #PB_OS_Windows_Server_2003)
DefLiteral("#os:windowvista"      , #PB_OS_Windows_Vista)
DefLiteral("#os:windowsserver2008", #PB_OS_Windows_Server_2008)
DefLiteral("#os:windowsfuture"    , #PB_OS_Windows_Future)
; -Platforms literals-
DefLiteral("#platform:windows", #PB_OS_Windows)
DefLiteral("#platform:linux"  , #PB_OS_Linux)
DefLiteral("#platform:macos"  , #PB_OS_MacOS)
DefLiteral("#platform:amigaos", #PB_OS_AmigaOS)
; -Encoding literals.
DefLiteral("#ascii"  , #PB_Ascii)
DefLiteral("#unicode", #PB_Unicode)
DefLiteral("#utf8"   , #PB_UTF8)
; -Types measurement.
DefLiteral("#sizeof:b", SizeOf(Byte))
DefLiteral("#sizeof:c", Compiler\CharSize)
DefLiteral("#sizeof:w", SizeOf(Word))
DefLiteral("#sizeof:l", SizeOf(Long))
DefLiteral("#sizeof:q", SizeOf(Quad))
DefLiteral("#sizeof:i", Compiler\IntSize)
DefLiteral("#sizeof:f", SizeOf(Float))
DefLiteral("#sizeof:d", SizeOf(Double))
DefLiteral("#sizeof:s", Compiler\IntSize)
DefLiteral("#opcode"  , #OpCodeSize)
; -Char codes.
DefCharLit("#nul", #NUL)
DefCharLit("#cr" , #CR)
DefCharLit("#lf" , #LF)
DefCharLit("#bs" , #BS)
DefCharLit("#bl" , ' ')
DefCharLit("#qt" , '"')
DefCharLit("#tab", #TAB)
DefCharLit("#esc", #ESC)
; -Math.
DefLiteral("#pi"  , #PI, 'f')
DefLiteral("#pi:d", #PI, 'd')
; -Misc.
DefLiteral("#true" , #True)
DefLiteral("#false", #False)
EndMacro
;}
;{ --Parsing management--
Macro IncCL() ; Pseudo-procedure.
Compiler\Source\Loc\LineNum + 1
EndMacro

Macro ReadingAvaible() ; Pseudo-procedure.
(Compiler\MacroPos Or Not Eof(Compiler\Source\File))
EndMacro

Macro ParsingMacro() ; Partializer.
Compiler\MacroPos
EndMacro

Procedure.C ParseChar()
With Compiler
If ParsingMacro() ; Если в буффере что-то есть...
Define Char.c = \MacroPos\C   ; Считываем символ.
\MacroPos + SizeOf(Character) ; Сдвигаем указатель.
\ThisMacro\Rest - 1           ; Понижаем счетчик.
If \ThisMacro\Rest = 0 ; Если символов больше не осталось...
If \MacroPos\C = 0 : \MacroBuf = "" : \MacroPos = 0 ; Сброс основных данных.
\ThisMacro = @Compiler\VoidMacro : ClearList(\MacroStack()) ; Сброс дополнительных данных.
Else : DeleteElement(\MacroStack()) : \ThisMacro = \MacroStack() ; Restoring previous macro
EndIf
EndIf
ElseIf \Source\CharIncome = #PB_Unicode : Char = ReadCharacter(\Source\File)
Else : Define Temp.a = ReadAsciiCharacter(\Source\File)
MultiByteToWideChar_(#CP_ACP, 0, @Temp, 1, @Char, 1)
EndIf
If Char = #LF : IncCL() : EndIf
If \CountDelay : \CountDelay - 1 : EndIf
ProcedureReturn Char
EndWith
EndProcedure

Macro DoControl() ; Partizlier
Select Char
Case ControlChar, Target : Raw + Chr(Char)
Case 'r', 'R' : Raw + #CR$
Case 'n', 'N' : Raw + #LF$
Case 'b', 'B' : Raw + #BS$
Case 't', 'T' : Raw + #TAB$
Case 'a', 'A' : Raw + #BEL$
Case 'f', 'F' : Raw + #FF$
Case 'v', 'V' : Raw + #VT$
Case ' '      : Raw + " "
Case '-'      : Flatten = 1
Case '+'      : Flatten = 0
Case '{'      : SkipTarget + 1
Case '}'      : If SkipTarget : SkipTarget - 1 ; Понижаем значение флага.
Else : Warn(BrokenStruct(Chr(ControlChar) + "}", Chr(ControlChar) + "{") + " encountered")
EndIf ; Рапортуем о некорректной последовательности:
Default : Warn("Cancelled inclusion of undefined control sequence " + Chr(ControlChar) + Chr(Char))
EndSelect : CFlag = #False
EndMacro

Procedure.s ParseToChar(Target.c, Flatten = #False, Error = #False, ControlChar = -1)
; Flatten = 0 - Parse all.
; Flatten = 1 - Do not parse line breaks.
; Flatten = 2 - ..same + Do not parse spaces and tabs.
Define CFlag.i, SkipTarget.i ; Флаги.
Define ParseStart = Compiler\Source\Loc\LineNum
With Compiler\ThisMacro
While ReadingAvaible()
Define Raw.s, Char.c = ParseChar()
If CFlag = #False ; Если флаг не взеведен...
Select Char
Case Target      : If SkipTarget = 0 : ProcedureReturn Raw : Else              : Raw + Chr(Char) : EndIf
Case #CR, #LF    : If Flatten = #False                                         : Raw + Chr(Char) : EndIf
Case ' ', #TAB   : If Flatten < 2 Or \Rest = 0 Or \Rest > Len(#MacroDelimiter) : Raw + Chr(Char) : EndIf
Case ControlChar : CFlag = #True
Default : Raw + Chr(Char)
EndSelect
Else : DoControl() ; Парсим контрольную последовательность.
EndIf
Wend
EndWith
If Error : ThrowerrorEx(Error, ParseStart) : EndIf
EndProcedure

Macro SwallowLine() ; Pseudo-procedure.
While ReadingAvaible() ; Scan.
Select ParseChar()     ; Analyze.
Case #CR, #LF : Break  ; Строка проглочена.
EndSelect 
Wend
EndMacro

Procedure.s NextWord(FollowUP = #False)
With Compiler
\ThisWord = ""
While ReadingAvaible()
Define Char.C = ParseChar()
Select Char
Case ' ', #CR, #LF, #TAB : If \ThisWord Or FollowUP : Break : EndIf
Default : \ThisWord + Chr(Char)
EndSelect
Wend
\ThisWord = LCase(\ThisWord)
ProcedureReturn \ThisWord
EndWith
EndProcedure

Procedure Paste2Code(Piece.s, DelayCount = #False)
With Compiler
If Piece ; Если там есть чего добавлять...
\MacroBuf = Piece + #MacroDelimiter + SafePeekS(\MacroPos)
\MacroPos = @\MacroBuf
Define BodyLen = Len(Piece) + Len(#MacroDelimiter)
\ThisMacro\Rest + BodyLen
If DelayCount : \CountDelay + BodyLen : EndIf
EndIf
EndWith
EndProcedure

Macro UnMacro(Body)
Paste2Code(Body, #True)
EndMacro

Macro ReParseWord(TPos = 0) ; Pseudo-procedure.
If TPos : Compiler\ThisWord = Mid(Compiler\ThisWord, TPos) : EndIf
Compiler\Reparse = #True
EndMacro

Procedure ValType2Size(TypeIdent)
Select TypeIdent
Case #vByte    : ProcedureReturn SizeOf(Byte)
Case #vWord    : ProcedureReturn SizeOf(Word)
Case #vChar    : ProcedureReturn Compiler\CharSize
Case #vLong, #vFloat  : ProcedureReturn SizeOf(Long)
Case #vQuad, #vDouble : ProcedureReturn SizeOf(Quad)
Default : ProcedureReturn Compiler\IntSize
EndSelect
EndProcedure

Procedure LitIdent2ValType(LitIdent)
Select LitIdent
Case 'b' : ProcedureReturn #vByte
Case 'w' : ProcedureReturn #vWord
Case 'c' : ProcedureReturn #vChar
Case 'l' : ProcedureReturn #vLong
Case 'q' : ProcedureReturn #vQuad
Case 'f' : ProcedureReturn #vFloat
Case 'd' : ProcedureReturn #vDouble
Case 's' : ProcedureReturn #vString
Case ':Lbl' : ProcedureReturn #vLabelPtr
Case ':Prc' : ProcedureReturn #vProcPtr
Default  : ProcedureReturn #vInteger
EndSelect
EndProcedure

Procedure ValType2LitIdent(ValType)
Select ValType
Case #vByte     : ProcedureReturn 'b'
Case #vWord     : ProcedureReturn 'w'
Case #vChar     : ProcedureReturn 'c'
Case #vLong     : ProcedureReturn 'l'
Case #vQuad     : ProcedureReturn 'q'
Case #vFloat    : ProcedureReturn 'f'
Case #vDouble   : ProcedureReturn 'd'
Case #vString   : ProcedureReturn 's'
Default         : ProcedureReturn 'i'
EndSelect
EndProcedure

Macro LitIdent2Size(LitIdent) ; Pseudo-procedure.
ValType2Size(LitIdent2ValType(LitIdent))
EndMacro

Procedure LitWritingBlock(*LitData.WordData, NumFlag, StrFlag)
With *LitData
Select \TypeID ; Identifier.
; -Basic types-
Case 'b' : ProcedureReturn LiteralB(\QValue, NumFlag)
Case 'w' : ProcedureReturn LiteralW(\QValue, NumFlag)
Case 'c' : ProcedureReturn LiteralC(\QValue, NumFlag) 
Case 'l' : ProcedureReturn LiteralL(\QValue, NumFlag)
Case 'i' : ProcedureReturn LiteralI(\QValue, NumFlag)
Case 'q' : ProcedureReturn LiteralQ(\QValue, NumFlag)
Case 'f' : ProcedureReturn LiteralF(\DValue, NumFlag)
Case 'd' : ProcedureReturn LiteralD(\DValue, NumFlag)
; -Extended types-
Case ':Abs' : AbsPointer(\QValue)
Case ':Rel' : RelPointer(\QValue)
Case ':Dat' : DataPointer(\QValue)
Case ':Str' : LitSPointer(\QValue)
Case ':Int' : LitIPointer(\QValue)
; -Compund types-
Case 's'    : ProcedureReturn LiteralS(\SValue, StrFlag)
EndSelect
EndWith
EndProcedure

Macro CompilingColon() ; Pseudo-procedure.
Compiler\ProcLevel
EndMacro

Macro GetWord(WordName) ; Pseudo-procedure.
FindMapElement(Compiler\Dictionary(), WordName)
EndMacro

Procedure.s FormatNS(Word_Path.s)
If Word_Path : ProcedureReturn Word_Path + #NSDelimiter$ : EndIf
ProcedureReturn Word_Path
EndProcedure

Macro FormatWord(Word_Name, ExportSpace = Compiler\NameSpace) ; Pseudo-procedure.
FormatNS(ExportSpace) + Word_Name
EndMacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Procedure.S WordFormatter(WordName.s, Simple = #False)
If Simple : ProcedureReturn FormatWord(WordName)
Else : ProcedureReturn FormatWord(WordName, Compiler\ProcParent)
EndIf
EndProcedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Macro IsData(WType) ; Pseudo-procedure.
(WType = #wVariable Or WType = #wArray)
EndMacro

Macro IsExec(WType) ; Pseudo-procedure.
(WType = #wLabel Or WType = #wProcedure)
EndMacro

Procedure.q CheckEIFlag()
Define Mode.q = Compiler\EarlyInitFlag
Compiler\EarlyInitFlag = #False
ProcedureReturn Mode
EndProcedure

Macro ReDefMessage(WordType, Changed = #True) ; Partializer.
CompilerIf Changed : Warn(WordType + " '" + Name + "' was redefined")
CompilerElse       : Warn(WordType + " '" + Name + "'")
ProcedureReturn TrueName
CompilerEndIf
EndMacro

Procedure.s AddWord(Name.s, Value.i = 0, Type.i = #wDataLabel, ExtraVal.q = 0, SValue.s = "")
If IsData(Type) Or CompilingColon() = #False : Define Parentless = #True : EndIf
If Type <> #wDataLabel ; Если слово пойдет в текущий конекст.
Define TrueName.s = WordFormatter(Name, Parentless) : Else : TrueName = Name
EndIf
Define *Word.WordData  = GetWord(TrueName)
Define *Char.Character = @Name
With *Word
If *Word
If IsData(Type) And Type = \Type And Value = \IValue
If *Char\C <> ' ' : ReDefMessage("Cancelled attempt to repeat definition of", #False) : EndIf
ElseIf Type = \Type And Type = #wLiteral And \Extra = ExtraVal : ReDefMessage("Constant")
ElseIf Type = \Type And Type = #wMacro   : ReDefMessage("Macro")
ElseIf Type = \Type And Type = #wSysCall : ReDefMessage("Caller")
ElseIf *Char\C <> ' ' : ThrowError(#eRedefined, "Unable to redefine '" + Name + "'")
Else : ProcedureReturn TrueName
EndIf
ElseIf FindMapElement(Compiler\Primitives(), Name)
Warn("Primitive '" + Name + "' was redefined")
ElseIf FindMapElement(Compiler\Reflected(), Name)
Warn("Reflected primitive '" + Name + "' was redefined")
EndIf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Define Public.i ; Флаг публичного определения.
If Type <> #wDataLabel
If ParentLess : Public = Compiler\DefPublic
Else          : Public = Compiler\ProcPDef
EndIf : Name = TrueName
Else : Public = #True
EndIf
*Word = AddMapElement(Compiler\Dictionary(), Name)
\Type   = Type
\IValue = Value
\SValue = SValue
\Public = Public
If ExtraVal : \Extra = ExtraVal
ElseIf IsExec(Type) Or (IsData(Type) And Compiler\VoidProcs = 0 And Compiler\DefStatic = 0)
If CompilingColon() : \Storage = Compiler\ProcNum : EndIf
EndIf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
EndWith
ProcedureReturn Name
EndProcedure

Procedure AddLink(Parent.s, Name.s, Value.i = 0, ExtraVal.q = 0, BackLink = #True)
Define *Word.WordData = GetWord(Name) ; Ищем ссылку в словаре.
With *Word ; Обрабатываем найденное.
If *Word = #Null ; Если такого слова еще нет...
Define *Parent.WordData = GetWord(Parent)
*Word = AddMapElement(Compiler\Dictionary(), Name)
\Type   = #wDataLabel
\Public = #True
\IValue = Value
\Extra  = ExtraVal
\ChainLink = *Parent
If BackLink : *Parent\ChainLink = *Word : EndIf
Else : \Extra = ExtraVal
EndIf
EndWith
EndProcedure

Macro AddLabel(Name) ; Pseudo-procedure.
AddWord(Name, Compiler\Offset, #wLabel)
EndMacro

Macro AddResLabel(Name) ; Pseudo-procedure.
AddWord(Name, Compiler\ResOffset, #wResLabel, Compiler\ResCount)
EndMacro

Procedure.s TmpLabelName(PreFix.s) : Compiler\TmpSeed + 1
ProcedureReturn " " + Prefix + ":" + Str(Compiler\TmpSeed)
EndProcedure

Macro StorePosition(LocData) ; Partializer.
LocData\LineNum = Compiler\Source\Loc\LineNum
LocData\SrcName = Compiler\Source\Loc\SrcName 
EndMacro

Macro SaveState() ; Partializer.
Compiler\PrevState = Compiler\State
EndMacro

Macro ResetState(FullReset = #False) ; Partializer.
CompilerIf FullReset : Compiler\State = #sNormal : Compiler\PrevState = 0
CompilerElse ; Мягкий режим сброса:
If Compiler\PrevState : Compiler\State = Compiler\PrevState : Else : Compiler\State = #sNormal : EndIf
CompilerEndIf
EndMacro

Procedure ChangeState(NewState, SubState.q = 0)
Compiler\State = NewState
Compiler\SubState = SubState
StorePosition(Compiler\StateLoc)
EndProcedure

Macro AddVariable(Name, Type = Compiler\SubState) ; Pseudo-procedure.
AddWord(Name, Type, #wVariable)
EndMacro

Procedure.s AddInternalVar(Name.s, Type = #vInteger)
If CompilingColon() And Compiler\VoidProcs = 0
ProcedureReturn AddWord(Name, Type, #wVariable, Compiler\ProcNum)
Else : ProcedureReturn AddVariable(Name, Type)
EndIf
EndProcedure

Macro AddVarPointer(VarName, VarData) ; Pseudo-procedure.
AddWord(VarName, VarData\IValue, #wVariable, VarData\Extra)
EndMacro

Macro AddArray(Name, Type = Compiler\SubState) ; Pseudo-procedure.
Define ArrName.s = AddWord(Name, Type, #wArray)
AddVariable(#ArrPtrPrefix  + ArrName, #vInteger)
AddVariable(#ArrSizePrefix + ArrName, #vInteger)
EndMacro

Macro AddStaticArray(Name, ArrSize, Type = Compiler\SubState) ; Pseudo-procedure.
Define ArrName.s = AddWord(Name, Type, #wArray)
AddLink(ArrName, #ArrDataPrefix + ArrName, 0, ArrSize)
EndMacro

Macro AddLiteral(Name, Type = 'i', IntValue = 0) ; Pseudo-procedure.
AddWord(Name, IntValue, #wLiteral, Type)
EndMacro

Macro AddMacro(Name, Body = "") ; Pseudo-procedure.
AddWord(Name, 0, #wMacro, 0, Body)
EndMacro

Macro AddSysCaller(Name, CallName) ; Pseudo-procedure.
AddWord(Name, 0, #wSysCall, 0, Trim(CallName))
EndMacro

Macro InitOperations(Storage) ; Partializer.
Case 'Val>' : ValWriter(Storage, Compiler\EarlyInitOp)
Case 'Str>' : ProcessString(Storage, Compiler\EarlyInitOp)
Case 'Ptr>' : ExtractPointer(Storage)
Case 'Size' : ExtractSize(Storage)
EndMacro

Macro InitNewVariable(VarName, VarType = Compiler\SubState) ; Pseudo-procedure.
AddVariable(VarName, VarType)
Select CheckEIFlag() ; Choosing proper initializer:
InitOperations(VarName) ; Common block.
EndSelect
EndMacro

Macro InitNewArray(Name, ArrType = Compiler\SubState, ArrSize = Compiler\VoidWord\ChainLink) ; Pseudo-procedure.
If ArrSize <> -1 : AddStaticArray(Name, ArrSize, ArrType)
Else : AddArray(Name, ArrType) ; Определяем динамический массив.
EndIf
Select CheckEIFlag() ; Choosing proper initializer:
InitOperations(ArrName) ; Common block.
Case 'Arr>' : ProcessArray(ArrName, Compiler\EarlyInitOp)
EndSelect
EndMacro

Macro InitNewResLabel(Name) ; Pseudo-procedure.
AddResLabel(Name)
Select CheckEIFlag() ; Choosing proper initializer:
Case 'Res>' : GetResourceInfo(Name, Compiler\EarlyInitOp)
Case 'Ptr>' : ExtractPointer(Name)
EndSelect
EndMacro

Macro InitNewLiteral(Name, LitType = ValType2LitIdent(Compiler\SubState)) ; Pseudo-procedure.
Define *Word.WordData = GetWord(AddLiteral(Name, LitType))
RecallLit(*Word) ; Инициализация.
EndMacro

Macro InitNewMacro(Name, Body = Compiler\VoidWord\SValue) ; Pseudo-procedure.
AddMacro(Name, Body)
Select CheckEIFlag() ; Choosing proper initializer:
Case 'Body' : ExhaleString(Body)
EndSelect
EndMacro

Macro InitNewCaller(Name, CallName = Compiler\VoidWord\SValue) ; Pseudo-procedure.
AddSysCaller(Name, CallName)
Select CheckEIFlag() ; Choosing proper initializer:
Case 'Body' : ExhaleString(Trim(CallName))
EndSelect
EndMacro

Procedure.s RequestCounter(Depth.i = 0)
With Compiler
If ListSize(\Counters()) - \ProcCounters > Depth
While Depth : Depth - 1
PreviousElement(\Counters())
Wend
Define CName.s = \Counters()
LastElement(\Counters())
ProcedureReturn CName
Else 
ThrowError(#eCntrUFlow, "'" + Chr('I' + Depth) + "' - requested counter's level underflow")
EndIf
EndWith
EndProcedure

Procedure.q WideTypeIdent(Word.s, DefBlock = #False)
Define WordSize = Len(Word)
If WordSize > 4 Or (DefBlock And WordSize = 4)
Define IDent.s = Right(Word, 4)
Select PeekQ(@IDent)
Case 'sba:' : ProcedureReturn ':Abs'
Case 'ler:' : ProcedureReturn ':Rel'
Case 'tad:' : ProcedureReturn ':Dat'
Case 'rts:' : ProcedureReturn ':Str'
Case 'tni:' : ProcedureReturn ':Int'
EndSelect
EndIf
EndProcedure

Procedure TypeIdent(Word.s, DefBlock = #False)
Define WordSize = Len(Word)
If WordSize > 2 Or (DefBlock And WordSize = 2)
Define Ident.s = Right(Word, 2)
Define *IChar.Character = @Ident
With *IChar
If \C = ':' : *IChar + SizeOf(Character)
Select \C : Case 'b', 'w', 'c', 'l', 'q', 'i', 'f', 'd'
ProcedureReturn \C
Case 's' : If DefBlock : ProcedureReturn \C : EndIf
EndSelect
EndWith
EndIf
EndIf
EndProcedure

Procedure BaseIdent(Word.s)
Define *IChar.Character = @Word
If *IChar\C = '%' Or *IChar\C = '$' : ProcedureReturn *IChar\C : EndIf
EndProcedure

Procedure SignIdent(Word.s)
Define *IChar.Character = @Word
If *IChar\C = '+' Or *IChar\C = '-' : ProcedureReturn *IChar\C : EndIf
EndProcedure

Macro StripIdent(String, ILen = 1, FromRight = #False, DefMode = #False) ; Partializer.
If Len(String) > ILen
CompilerIf FromRight : String = RCut(String, ILen) 
CompilerElse : String = LCut(String, ILen) : CompilerEndIf
Else ; Если слово некорректно названо:
CompilerIf DefMode
ThrowError(#eNoValName, "Invalid value name '" + ValName + "'")
CompilerElse : ProcedureReturn #False
CompilerEndIf
EndIf
EndMacro

Procedure Literalize(Word.S)
With Compiler
If \Interpreting : Define Original.s = Word : EndIf
Define SI = SignIdent(Word)
If SI : StripIdent(Word) : EndIf
Define TI.q = WideTypeIdent(Word)
If TI = 0 : TI = TypeIdent(Word) : EndIf
Define BI = BaseIdent(Word)
If Word = "." : ProcedureReturn #False : EndIf ; Выходим сразу, если точка.
Define Dots = CountString(Word, ".")           ; Считаем количество точек.
If TI ; If no type identifier found...
If TI > '  ' : StripIdent(Word, 4, #True) 
Else : StripIdent(Word, 2, #True)
EndIf
If \Interpreting : Define ShowWarn = #True : Goto IDetect : EndIf
Else     ; Definig it manually.
IDetect: 
Select Dots ; Analyzing dot count.
Case 0  : If BI = 0 And FindString(Word, "e", 1) : Goto IFloat : EndIf
If \Interpreting : TI = 'q' : Else : TI = \DefDataType : EndIf
Case 1  ; If it was identified as float number.
IFloat: : If \DefDataType = 'd' Or \Interpreting : TI = 'd' : Else : TI = 'f' : EndIf
Default : ProcedureReturn #False
EndSelect
EndIf
; Preparations for parsing literal.
If TI = 'c' And SI = '-' : ProcedureReturn #False : EndIf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Define IsNumber.i, Target.s ; Temporary accums.
If TI <> 'f' And TI <> 'd'  ; If it's integer value...
Define Val.q = Val(Word)    ; Считаем значение литерала.
Word = LTrim(Word, "0")     ; Обрезаем нули.
If BI = '$'     : Target = LCase(Hex(Val)) : StripIdent(Word)
ElseIf BI = '%' : Target = Bin(Val)        : StripIdent(Word)
Else            : Target = Str(Val)
EndIf           : Target = LTrim(Target, "0") ; Вырезаем нули.
If Target = Word : IsNumber = #True : EndIf   ; Проверяем валидность.
ElseIf BI : ProcedureReturn #False
Else : IsNumber = MatchRegularExpression(#rIsNumber, Word) ; Проверяем валидность.
EndIf
If IsNumber ; If it was identified as number...
If SI = '-' : IsNumber = -1 : EndIf ; флаг отрицательности.
\GenLiteral\Extra = TI  ; Выставляем идентефикатор.
If TI = 'f' Or TI = 'd' : \GenLiteral\DValue = ValD(Word) * IsNumber
Else                    : \GenLiteral\QValue = Val * IsNumber
EndIf
If ShowWarn : Warn("Type identifier was discarded from '" + Original + "'") : EndIf
ProcedureReturn #True
EndIf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
EndWith
EndProcedure

Procedure.q ExtraTypeIdent(Word.s)
If Len(Word) >= 4
Define IDent.s = Right(Word, 4)
Select PeekQ(@IDent)
Case 'crp:' : ProcedureReturn ':Prc'
Case 'lbl:' : ProcedureReturn ':Lbl'
EndSelect
EndIf
EndProcedure

Procedure ArrayIdent(Word.s)
Define WSize = Len(Word)
If WSize >= 2
If PeekU(@Word + WSize - 2) = '][' : ProcedureReturn 2 : EndIf
EndIf
EndProcedure

Macro AddValue(Name) ; Pseudo-procedure
Define IDSize.i = 0, ValName.s = Name, StorageType.q = ArrayIdent(Name)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
If StorageType : StripIdent(ValName, StorageType, #True, #True)
StorageType = 'Arr_' ; Выставляем тип хранилища.
EndIf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Define ValType.q = TypeIdent(ValName, #True) ; Поучаем идентефикатор.
If ValType = 0 ; Если не указан стандартный тип.
ValType = ExtraTypeIdent(ValName) ; Пробуем расширенные...
If ValType = 0 ; Если не указан и расширенный тип...
If Compiler\DefDataType < ' ' : ValType = Compiler\DefDataType : Else : ValType = 'i' : EndIf
Else : IDSize = 4 ; Идентефикатор = 4 символа.
EndIf
Else : IDSize = 2 ; Идентефикатор = 2 символа.
EndIf
If IDSize : StripIdent(ValName, IDsize, #True, #True) : EndIf
ValType = LitIdent2ValType(ValType) ; Преобразуем тип.
Select StorageType ; Создаем значение нужного типа:
Case 'Arr_' : AddArray(ValName, ValType)    ; Массив.
Default     : AddVariable(ValName, ValType) ; Переменная.
EndSelect
EndMacro

Macro ParseGroup(Name) ; Pseudo-procedure
Select Name
Case "]def" : ResetState(#True)
CommentsBlock() ; For comments insertion.
Default : AddValue(Name) ; Добавляем значение.
EndSelect
EndMacro

Macro StaticDefiner(StrData, DefType) ; Partialzer.
Compiler\VoidWord\SValue = StrData : Compiler\GenDefiner\Extra = DefType 
Compiler\VoidWord\Extra = #True    : ProcedureReturn 'Def'
EndMacro

Procedure ExportSysCall(CallName.s)
CallName = Trim(CallName)
If FindMapElement(Compiler\Dictionary(), #ExportPrefix + CallName)
Warn("Export table entry was redefined: " + #DQUOTE$ + CallName + #DQUOTE$)
EndIf : AddWord(#ExportPrefix + CallName, Compiler\Offset, #wDataLabel, Compiler\ProcNum)
EndProcedure

Macro TryCloseComment(Opener, Closer) ; Pseudo-procedure.
Define TPos.i = FindString(Compiler\ThisWord, Opener, 1)
Define WLen = Len(Compiler\ThisWord)
If TPos ; Если найден новый уровень блока комментариев... 
Compiler\Nested + 1 : RewindWord(Opener)
Else : TPos = FindString(Compiler\ThisWord, Closer, 1) 
If TPos ; Если найдено окончание блока комментариев... 
If Compiler\Nested = 0 : ResetState() : Else : Compiler\Nested - 1 : EndIf
RewindWord(Closer)
EndIf
EndIf
EndMacro

Macro ResInfoLabel(Prefix) ; Pseudo-procedure.
Prefix + Str(Compiler\ResCount)
EndMacro

Macro CheckExclusivity(Dict, FName, ActionName)
If Exclusive
Define *Res.ResFileData = FindMapElement(Dict(), LCase(GetAbsolutePath(FName)))
If *Res : Warn("Cancelled attempt to repeat " + ActionName + " of '" + FName + "'")
Define *Char.Character = @ActionName
If *Char\C = 'm'
Define OldOffset = Compiler\ResOffset
AddWord(#ResCorrector + Str(Compiler\ResCount), *Res\Start)
AddWord(ResInfoLabel(#ResSizePrefix), *Res\Size)
AddWord(ResInfoLabel(#ResEndPrefix), *Res\Start + *Res\Size)
Compiler\ResCount + 1
EndIf
ResetState() : ProcedureReturn #False
EndIf
EndIf
EndMacro

Procedure TryIncludeSource(FileName.s, Exclusive.i)
FileName = Trim(RefactoreString(FileName, #True))
If FileName : CheckExclusivity(Compiler\IncludedFiles, FileName, "inclusion")
If OpenSource(FileName) = #False
ThrowError(#eNoInclude, "Unable to include '" + FileName + "'")
Else : ProcedureReturn #True
EndIf
EndIf
EndProcedure

Macro TryChangeInclusionDir(NewDir) ; Pseudo-procedure.
Define Sequence.s = Trim(RefactoreString(NewDir, #True))
If Sequence ; Если есть, на что менять...
If SetCurrentDirectory(Sequence) = #False
ThrowError(#eNoDir, "'" + Sequence + "' - invalid directory specifed")
EndIf
EndIf
EndMacro

Macro PublishMergingResults(OffsetVar) ; Pseudo-procedure.
AddWord(ResInfoLabel(#ResSizePrefix), RSize) : OffsetVar + RSize
AddWord(ResInfoLabel(#ResEndPrefix), OffsetVar) : Compiler\ResCount + 1
If RSize : Inform(FormatSize(RSize) + " added to data section") : EndIf
EndMacro

Macro GetMergingTarget(FilePtrAcum, FileOffsetAcum) ; Partializer.
Define *FilePtrAcum, *FileOffsetAcum.Integer
If Compiler\DirectMerging : *FilePtrAcum = Compiler\OutputFile
*FileOffsetAcum = @Compiler\Offset
Else : *FilePtrAcum = Compiler\ResourcesFile
*FileOffsetAcum = @Compiler\ResOffset
EndIf
EndMacro

Procedure MergeFile(FileName.s, Exclusive.i)
GetMergingTarget(Target, TOffset)
CheckExclusivity(Compiler\InfusedFiles, FileName, "merging")
Define RSize = AppendFile(*Target, FileName)
If RSize > 0 : Inform("'" + FileName + "' succesfully infused", #False)
Define *ResFile.ResFileData = AddMapElement(Compiler\InfusedFiles(), LCase(GetAbsolutePath(FileName)))
*ResFile\Start = *TOffset\I
*ResFile\Size  = RSize
PublishMergingResults(*TOffset\I)
ElseIf RSize = 0 : Warn("Cancelled attempt to merge empty file ('" + FileName + "')")
Else   : ThrowError(#eNoResource, "Unable to merge '" + FileName + "'")
EndIf
EndProcedure

Macro TryMergeFile(FileName, Exclusive) ; Pseudo-procedure.
Define Sequence.s = Trim(RefactoreString(FileName, #True))
If Sequence : ResetState() : MergeFile(Sequence, Exclusive) 
Else : ThrowError(#eNoResFile)
EndIf
EndMacro

Procedure TryMergeSequence(Sequence.s, Type = 0) ; Pseudo-procedure.
; Type = 0 - hexadecimal data.
; Type = 1 - base64 data.
GetMergingTarget(Target, TOffset)
Define RSize : Sequence = RefactoreString(Sequence)
If Sequence : Define SeqSize = Len(Sequence)
Select Type
Case 0 ; Parse hexadecimal data sequence.
Sequence = UCase(Sequence)
Define *Char.Character = @Sequence, Byte.c, Flag.i
If SeqSize % 2 : Flag = 0 : Else : Flag = ~0 : EndIf
While *Char\C
Select *Char\C
Case '0' To '9' : Byte + *Char\C - '0'
Case 'A' To 'F' : Byte + *Char\C - 'A' + 10
Default : ThrowError(#eInvSeq)
EndSelect : Flag = ~Flag
If Flag : WriteByte(*Target, Byte) : Byte = 0 : Else : Byte * 16 : EndIf
*Char + SizeOf(Character)
Wend : RSize = Round(SeqSize / 2, #PB_Round_Up)
Case 1 ; Parse Base64 data sequence.
RSize = SeqSize * 1.5
Define *Buffer = AllocateMemory(RSize)
RSize = Base64Decoder(@Sequence, SeqSize, *Buffer, RSize)
If RSize : WriteData(*Target, *Buffer, RSize)
FreeMemory(*Buffer) 
Else : ThrowError(#eInvSeq)
EndIf
EndSelect
Else : Warn("Cancelled attempt to merge empty data sequence")
EndIf
PublishMergingResults(*TOffset\I)
EndProcedure

Macro MergeString(Text) ; Pseudo-procedure.
Define RSize = Len(Text) * Compiler\CharSize
GetMergingTarget(Target, TOffset)
WriteString(*Target, Text, Compiler\CharOutcome)
PublishMergingResults(*TOffset\I)
EndMacro

Macro ParsePathDelim(Delim) ; Pseudo-procedure.
Define Sequence.s = RefactoreString(Delim)
If Sequence : Compiler\PathDelimiter = Sequence
Inform("Context path delimiter set to '" + Sequence + "'")
Else : ThrowError(#eInvDelim)
EndIf
ResetState()
EndMacro

Macro ReportMessage(Text) ; Pseudo-procedure.
TypeOut("<" + Compiler\Source\Loc\SrcName + ">: " + Text, 15)
EndMacro

Macro Done() ; Partializer
ProcedureReturn 'Nop'
EndMacro

Procedure TryExtendedStringOp(StrData.s, Ident.s)
Select Ident
Case "/import:" : StaticDefiner(StrData, #wSysCall)
Case "/macro:"  : StaticDefiner(StrData, #wMacro)
Case "/invoke"  : If Compiler\Interpreting = #False : EmitSysCall(Trim(StrData)) : Done() : EndIf
Case "/export"       : ExportSysCall(StrData)                         : Done()
Case "/report"       : ReportMessage(StrData)                         : Done()
Case "/usedir"       : TryChangeInclusionDir(StrData)                 : Done()
Case "/include"      : TryIncludeSource(StrData, Compiler\XInclusion) : Done()
Case "/merge"        : MergeString(StrData)                           : Done()
Case "/merge:file"   : TryMergeFile(StrData, Compiler\XInfusion)      : Done()
Case "/merge:hex"    : TryMergeSequence(StrData)                      : Done()
Case "/merge:base64" : TryMergeSequence(StrData, 1)                   : Done()
Case "/pathdelim"    : ParsePathDelim(StrData)                        : Done()
EndSelect
EndProcedure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Procedure SeekSource(Shift.i)
With Compiler
If Not ReadingAvaible() : Shift - Sign(Shift) : EndIf
If \CountDelay : \CountDelay - Shift : EndIf ; Just to be sure.
If \MacroPos : \MacroPos + Shift * SizeOf(Character) : \ThisMacro\Rest - Shift
Else : FileSeek(\Source\File, Loc(\Source\File) + Shift * \Source\ICharSize)
EndIf
EndWith
EndProcedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Procedure.s ParseSequence(Word.s, *Char.Character, Target.c, CSymbol.i)
If Len(Word) > 1 : *Char + SizeOf(Character)
If *Char\C = Target : SeekSource(-Len(Word)+1)
ProcedureReturn "" ; Возвращаем пустую строку.
EndIf
EndIf
SeekSource(-Len(Word)) ; Сдвигаем для парсинга.
ProcedureReturn ParseToChar(Target, 0, #eUnfinishedStr, CSymbol) ; Парсим строку.
EndProcedure

Procedure TryCloseSLiteral(Word.s, *Char.Character, CSymbol = -1)
Static Sequence.s ; Аккумулятор символов.
Define Target.c = *Char\C ; Парсим отделители.
Sequence = ParseSequence(Word, *Char, Target, CSymbol) ; Парсим символы строки.
Define TI.q, Ident.s = NextWord(#True), ISize = TryExtendedStringOp(Sequence, Ident)
If ISize : ProcedureReturn ISize : Else : ISize = Len(IDent) : EndIf
If ISize = 2     : TI = TypeIdent(Ident, #True)     ; Простой идентефикатор.
ElseIf ISize = 4 : TI = WideTypeIdent(Ident, #True) ; Расширенный идентефикатор.
EndIf
If TI ; Если удалось получить идентефикатор...
Define LSize = Len(Sequence) * Compiler\CharSize ; Получаем размер литерала.
If LSize <= LitIdent2Size(TI) ; Если размер корректен...
Define Pool.q = 0 ; Аккумулятор данных.
If Compiler\BigEndianStr = #False    : Sequence = ReverseString(Sequence) : EndIf
If Compiler\CharSize = SizeOf(Ascii) : Word = Sequence : PokeS(@Sequence, Word, -1, #PB_Ascii) : EndIf
CopyMemory(@Sequence, @Pool, LSize)  ; Перемещаем строку в аккумулятор.
Compiler\GenLiteral\Extra  = TI
Compiler\GenLiteral\QValue = Pool
Else : ThrowError(#eInvLitSize, "Literal size mismatch: "+Chr(Target)+Sequence+Chr(Target)+Ident)
EndIf
Else : ; Организуем указатель на строку.
Compiler\GenLiteral\Extra  = 's'
Compiler\GenLiteral\SValue = Sequence
If Ident : ReParseWord() : EndIf ; Репарсинг.
EndIf
ProcedureReturn 'Lit' ; 100%
EndProcedure

Procedure QLiteralize(Word.S)
Define *Char.Character = @Word
Select *Char\C
Case '"'     : ProcedureReturn TryCloseSLiteral(Word, *Char)
Case 39      : ProcedureReturn TryCloseSLiteral(Word, *Char, '`')
EndSelect
EndProcedure

Macro CheckMirror(Word) ; Partialzer.
If FindMapElement(Compiler\Reflected(), Word)
Compiler\GenLiteral\Extra = 'w'
Compiler\GenLiteral\QValue = Compiler\Reflected(Word)
ProcedureReturn #GenLiteral
EndIf
EndMacro

Macro DefInternalVar(Name, RealName, ErrNum) ; Partializer.
Case Name
If Compiler\RealName
ProcedureReturn Compiler\RealName
Else : ThrowError(ErrNum)
EndIf
EndMacro

Macro DefCounterReq(Name) ; Partializer.
Case Name : ProcedureReturn RequestCounter(Asc(Name) - 'i')
EndMacro

Macro DefDefiner(Name, WordType, DataType = 0, Link = 0)
Case Name
Compiler\GenDefiner\Extra    = WordType
Compiler\GenDefiner\IValue   = DataType
Compiler\GenDefiner\ChainLink = Link
ProcedureReturn #GenDefiner
EndMacro

Macro DefDataType(Name, DataType, Mode = 'Base')
CompilerIf Mode = 'Base' ; If it's base type.
Case Name : Compiler\GenDataReq\IValue = DataType : ProcedureReturn #DataReq
CompilerEndIf
CompilerIf Mode <> 'i/o!'
DefDefiner(Name + ":", #wVariable, Datatype)
DefDefiner(Name + "s:", #wArray, Datatype)
DefDefiner("->" + Name + "s:", #wArray, Datatype, #True)
CompilerEndIf
CompilerIf Mode <> 'c/o!'
DefDefiner("->" + Name + ":", #wLiteral, Datatype)
CompilerEndIf
EndMacro

Procedure.s WordSubstitution(WordName.s)
Select WordName
; -Internal variables-
DefInternalVar(#StepVar, StepVar, #eStepOut)
DefInternalVar(#LimitVar, LimitVar, #eLimiterOut)
DefInternalVar(#ITemplate, ITemplate, #eITmpOut)
DefInternalVar(#STemplate, STemplate, #eSTmpOut)
; -Counter requests-
DefCounterReq("i")
DefCounterReq("k")
DefCounterReq("j")
; -Data types-
DefDataType("byte"   , #vByte)
DefDataType("word"   , #vWord)
DefDataType("char"   , #vChar)
DefDataType("long"   , #vLong)
DefDataType("quad"   , #vQuad)
DefDataType("float"  , #vFloat)
DefDataType("double" , #vDouble)
DefDataType("string" , #vString)
DefDataType("integer", #vInteger)
DefDataType("string" , #vString, 'None')
DefDataType("?label" , #vLabelPtr, 'c/o!')
DefDataType("?proc"  , #vProcPtr, 'c/o!')
; -Additional definers-
DefDefiner("reslabel:", #wResLabel)
DefDefiner("def["     , #wDefiner)
DefDefiner("->macro:" , #wMacro)
DefDefiner("->import:", #wSysCall)
; -Technic literals-
DefLiteral("#offset"       , Compiler\Offset)
DefLiteral("#resoffset"    , Compiler\ResOffset)
DefLiteral("#headersize"   , SizeOf(HeaderData))
DefLiteral("#tickcount"    , ElapsedMilliseconds())
DefLiteral("#linenum"      , Compiler\Source\Loc\LineNum)
DefSLiteral("#sourcefile"  , Compiler\Source\FullName)
DefSLiteral("#outputfile"  , Compiler\OutputName)
DefSLiteral("#outputdir"   , Compiler\OutputPath)
DefSLiteral("#thisproc"    , Compiler\ProcName)
DefSLiteral("#compilerfile", ProgramFilename())
DefSLiteral("#currentdir"  , GetCurrentDirectory())
; -Other blocks-
DefineSystemCalls()
DefineLiterals()
Default : CheckMirror(WordName) ; Проверка отражений.
If Literalize(WordName)  : ProcedureReturn #GenLiteral 
Else ; Пробуем, наконец, строчный литерал:
Select QLiteralize(WordName)
Case 'Lit' : ProcedureReturn #GenLiteral
Case 'Def' : ProcedureReturn #GenDefiner
Case 'Nop' : ProcedureReturn #VoidWord
Default    : ProcedureReturn WordName
EndSelect
EndIf
EndSelect
EndProcedure

Procedure.s FindParent(Word_Name.s, ITable.s, ExportSpace.s)
Define *Char.Character = @ITable, NS.s
While *Char\C
If *Char\C = #ITDelimiter : If NS : NS + #NSDelimiter$ + Word_Name 
Else : NS = Word_Name : EndIf
Define *Ptr.WordData = GetWord(NS)
If *Ptr
If *Ptr\Public Or CompareMemoryString(@NS, @ExportSpace, 0, Len(ExportSpace)) = 0
Compiler\TrueName = NS
ProcedureReturn NS
EndIf
EndIf : NS = ""
Else : NS + Chr(*Char\C)
EndIf
*Char + SizeOf(Character)
Wend
With Compiler
If FindString(Word_Name, \PathDelimiter, 1)
If Left(Word_Name, Len(\PathDelimiter)) = \PathDelimiter : Word_Name = ExportSpace + Word_Name : EndIf
ProcedureReturn FindParent(ReplaceString(Word_Name, \PathDelimiter, #NSDelimiter$), ITable, ExportSpace)
EndIf
EndWith
EndProcedure

Macro FindParentFast(WordName) ; Pseudo-procedure.
FindParent(WordName, Compiler\ImportTable, Compiler\NameSpace)
EndMacro

Macro LookUpDict(NameAcum, Recipient) ; Partializer.
NameAcum = FindParentFast(NameAcum) : Recipient = GetWord(NameAcum)
EndMacro

Macro FindWord(Recipient, WordName) ; Pseudo-procedure.
Define Recipient.WordData
Define __OldName.s = WordName
If Compiler\WordData And Compiler\ThisWord = WordName
Recipient = Compiler\WordData : WordName = Compiler\TrueName
Else : LookUpDict(WordName, Recipient)
EndIf
If Recipient = #Null
Select __OldName ; Check for replacing possiblity:
Case #RootContext : WordName = #RootSpace         : Recipient = Compiler\RootContext
Case #OwnContext  : WordName = Compiler\NameSpace : Recipient = Compiler\RootContext
Default ; Normal search procedures:
WordName = WordSubstitution(__OldName)
LookUpDict(WordName, Recipient)
EndSelect
EndIf
If Recipient
EndMacro

Macro FindWordFast(WordName) ; Pseudo-procedure.
GetWord(FindParentFast(WordName))
EndMacro

Macro NotCommented() ; Partializer
Compiler\State <> #sCommented1 And Compiler\State <> #sCommented2
EndMacro

Macro NotMacroCall()
Compiler\State<>#sMacroName And Compiler\State<>#sBodyRequest And Compiler\State<>#sWordRequest
EndMacro

Macro ExpandMacro(TrueName, ShortName, MacroWord) ; Pseudo-procedure.
Define *MacroData.MacroData
ForEach Compiler\MacroStack() : *MacroData = Compiler\MacroStack()
If *MacroData\FullName = TrueName ; Если такой макрос уже есть в стеке.
ThrowError(#eMacroInfin, "Unable to recursively expand '" + ShortName + "'")
EndIf
Next : AddElement(Compiler\MacroStack()) : Compiler\ThisMacro = Compiler\MacroStack()
Compiler\ThisMacro\FullName = TrueName : Compiler\ThisMacro\Name = ShortName
Paste2Code(MacroWord\SValue) ; Записываем код в буффер.
EndMacro

Macro IsAlias(WordData) ; Pseudo-procedure.
(WordData\Type = #wMacro And WordData\IsLazy = #False)
EndMacro

Procedure ParseNextWord()
With Compiler
NextWord:     ; Точка возврата
If NextWord() ; Если слово пропарсилось...
If NotCommented() And NotMacroCall() : \WordData = FindWordFast(\ThisWord)
If \WordData  ; Если слово получилось найти в словаре...
If IsAlias(\WordData) : ExpandMacro(\TrueName, \ThisWord, \WordData) : Goto NextWord : EndIf
EndIf
EndIf
If Compiler\CountDelay = 0 : Compiler\WordsTotal + 1 : EndIf ; Увеличиваем счетчик.
ProcedureReturn #True ; Рапортуем успех.
EndIf
EndWith
EndProcedure

Macro ParseVarName(Type) ; Pseudo-procedure.
ChangeState(#sVariableName, Type)
EndMacro

Macro ParseConstName(Type) ; Pseudo-procedure.
ChangeState(#sConstantName, Type)
EndMacro

Macro ParseArrayName(Type) ; Pseudo-procedure.
ChangeState(#sArrayName, Type)
EndMacro

Macro PrepareVarWriting(Operation = '') ; Pseudo-procedure.
ChangeState(#sVariableWrite, Operation)
EndMacro

Macro PrepareArrRequest(Operation = '') ; Pseudo-procedure.
ChangeState(#sArrayRequest, Operation)
EndMacro

Macro PrepareStrRequest(Operation = '') ; Pseudo-procedure.
ChangeState(#sStrRequest, Operation)
EndMacro

Macro PrepareResAnalysis(Operation = '') ; Pseudo-procedure.
ChangeState(#sResLabelInfo, Operation)
EndMacro

Macro PrepareProcAnalysis(Operation = '') ; Pseudo-procedure.
ChangeState(#sIsoProcInfo, Operation)
EndMacro

Macro ParseProcName(ColonDef = #False) ; Pseudo-procedure.
ChangeState(#sProcedureName, ColonDef)
EndMacro

Macro CheckPrefix(WordName, Prefix) ; Pseudo-procedure.
Left(WordName, Len(Prefix)) = Prefix
EndMacro

Procedure RollSourceBack()
With Compiler
If ListSize(\Sources()) > 1
LastElement(\Sources())
DeleteElement(\Sources())
\Source = \Sources()
TypeOut(MakeArrow(ArrowSize(), #False) + "Rollback to '" + \Source\Loc\SrcName + "'...")
EndWith
ProcedureReturn #True
EndIf
EndProcedure

Macro AbortParsing() ; Partializer
Inform("Parsing aborted due to '<EOF>' directive encounter") : Break
EndMacro
;}
;{ --Compilation management--
Macro CompileJump(Dest, OpCode = 'Jmp', Scope = 'Abs') ; Pseudo-procedure.
CompilerSelect Scope
CompilerCase 'Data' : DataPointer(Dest)
CompilerCase 'Rel'  : RelPointer(Dest)
CompilerDefault     : AbsPointer(Dest)
CompilerEndSelect
CompilerSelect OpCode
CompilerCase 'IF'   : Emit(#iIF)
CompilerCase 'IFZ'  : Emit(#iIFZ)
CompilerCase 'Call' : Emit(#iIFZ)
CompilerDefault     : Emit(#iJmp)
CompilerEndSelect
EndMacro

Macro InternalJump(Dest, OpCode = 'Jmp') ; Pseudo-procedure.
CompileJump(Dest - Compiler\Offset - #OpCodeSize, OpCode, 'Rel')
EndMacro

Procedure ToBeFixed(FixLabel.s, Position = -1, ErrCode = #eUndefined, Extra.i = 0)
AddElement(Compiler\Fixez())
Define *Fix.FixData = Compiler\Fixez()
If Position = -1 : Position = Compiler\Offset + #OpCodeSize : EndIf
With *Fix
\Label = FixLabel
\Base = Position
\NotFound = ErrCode
\Extra = Extra
\ITable = Compiler\ImportTable
\NameSpace = Compiler\NameSpace
StorePosition(\Loc)
If CompilingColon() : \Isolated = Compiler\ProcNum : EndIf
EndWith
EndProcedure

Macro ThrowFixError(Code, Message) ; Pseudo-procedure.
ThrowErrorEx(Code, *Fix\Loc\LineNum, *Fix\Loc\SrcName, Message)
EndMacro

Procedure.s ResOperation2Prefix(Operation)
Select Operation
Case 'size' : ProcedureReturn #ResSizePrefix
Case '@end' : ProcedureReturn #ResEndPrefix
EndSelect
EndProcedure

Procedure.s ProcOperation2Prefix(Operation)
Select Operation
Case 'size' : ProcedureReturn #IsoSizePrefix
Case '@end' : ProcedureReturn #IsoEndPrefix
EndSelect
EndProcedure

Macro IsolationRequired(ProcName) ; Pseudo-procedure.
ThrowError(#eNonIsoProc, "Info request for non-isolated procedure '" + ProcName + "'")
EndMacro

Macro CheckOutCall() ; Pseudo-procedure.
If *Fix\Extra = #PostPonedCall
If *Fix\Isolated <> *FixWord\Extra And *FixWord\Extra > 0
OutCallAlert(TrueName, *Fix\Loc\LineNum, *Fix\Loc\SrcName)
EndIf
EndIf
EndMacro

Procedure CheckFixez()
Define *Fix.FixData, *FixWord.WordData
ForEach Compiler\Fixez() : *Fix = Compiler\Fixez()
With *Fix
If CheckPrefix(\Label, #PConstPrefix) = #False
Define FullName.s = FindParent(\Label, \ITable, \NameSpace)
*FixWord = GetWord(FullName)
If *FixWord
If *FixWord\Type <> #wLabel And *FixWord\Type <> #wDataLabel And *FixWord\Type <> #wProcedure
If \NotFound = #eUndefined 
ThrowFixError(#eEarlyReq, "'" + \Label + "' was requested before definition")
EndIf
EndIf
; Type checking:
If *FixWord\Type <> #wProcedure And \NotFound = #eFakeProc : Goto Justice : EndIf
If *FixWord\Type <> #wResLabel And \NotFound = #eFakeResLbl : Goto Justice : EndIf
Define TrueName.s = \Label
\Label = FullName
; Correct fixing requests going here:
Select *FixWord\Type 
Case #wVariable : \Label = #VarPrefix + \Label : AddWord(\Label, *FixWord\IValue)
Case #wArray    : \Label = #VarPrefix + #ArrPtrPrefix + \Label
Case #wResLabel 
If \Extra : \Label = ResOperation2Prefix(\Extra) + Str(*FixWord\Extra) : EndIf
Case #wLabel    : CheckOutCall()
Case #wDataLabel ; Если это метка данных...
If CheckPrefix(\Label, #LitPtrPrefix) ; Если это указатель на литерал...
If *FixWord\ChainLink\Extra = 's'     ; ...И тот литерал - строковый...
Define TextLit.s = #StrPrefix + *FixWord\ChainLink\SValue
If *FixWord\ChainLink\ChainLink = #True : \Label = TextLit ; Optimized !
ElseIf GetWord(TextLit) : *FixWord\ChainLink\ChainLink = #True : \Label = TextLit
EndIf
EndIf
EndIf
Case #wProcedure ; Если это процедура...
If \Extra And \Extra <> #PostPonedCall 
If *FixWord\Extra <> #Isolated : IsolationRequired(\Label) : EndIf
\Label = ProcOperation2Prefix(\Extra) + \Label
Else#CheckOutCall()
EndSelect
Else ; If no corresponding words found...
Justice: :Select \NotFound
Case #eInvPReq
ThrowFixError(\NotFound, "Pointer request for undefined word: '" + \Label + "'")
Case #eFakeResLbl
ThrowFixError(\NotFound, "Invalid resource label requested: '" + \Label + "'")
Case #eFakeProc
ThrowFixError(\NotFound, "Invalid procedure requested: '" + \Label + "'")
Default : If \NotFound : ThrowFixError(\NotFound, "'" + \Label + "' isn't defined") : EndIf
EndSelect
EndIf
EndIf
EndWith
Next
EndProcedure

Macro EmitString(Text) ; Pseudo-procedure.
WriteString(Compiler\OutputFile, Text, Compiler\CharOutcome)
WriteCharacter(Compiler\OutputFile, 0)
Compiler\Offset + (Len(Text) + 1) * Compiler\CharSize
EndMacro

Macro InVirtualData(WordName, Word) ; Pseudo-procedure.
(CheckPrefix(WordName,#VarPrefix) And Word\Storage=0)Or(CheckPrefix(WordName,#ArrDataPrefix) And Word\ChainLink\Storage=0)
EndMacro

Procedure FixThings(IsolationStart)
Define *Fix.FixData, *FixWord.WordData
Define *DataStart = Compiler\Offset
With Compiler
; Adding export table.
\ExportSize - \Offset
ForEach \ExportStack()
*FixWord = \ExportStack()
ToBeFixed(#ExportPrefix + *FixWord\SValue, \Offset)
WriteLitData(Integer, 0)
EmitString(*FixWord\SValue)
*FixWord\Type = #wLabel
Next : \Header\ExportTable = ListSize(\ExportStack())
\ExportSize + \Offset
; Adding import table.
\ImportSize - \Offset
ForEach \ImportStack()
*FixWord = \ImportStack()
*FixWord\IValue = *DataStart
*DataStart + Compiler\IntSize
EmitString(*FixWord\SValue)
Next : \ImportSize + \Offset
; Fixing header.
FileSeek(\OutputFile, 0)
If Compiler\IntSize  = SizeOf(Quad)    : SetBit(\Header\BitFlags, #b64bitTarget) : EndIf
If Compiler\CharSize = SizeOf(Unicode) : SetBit(\Header\BitFlags, #bUnicodeTarget) : EndIf
\Header\DataSize = \LocalPools(0)
WriteData(\OutputFile, @\Header, SizeOf(HeaderData))
EndWith
; Fixing main code.
ForEach Compiler\Fixez() : *Fix = Compiler\Fixez()
With *Fix
*FixWord = GetWord(\Label)
If \Isolated : FileSeek(Compiler\OutputFile, \Base + SizeOf(HeaderData) + IsolationStart)
Else : FileSeek(Compiler\OutputFile, \Base + SizeOf(HeaderData))
EndIf ; Variables fixing:
If InVirtualData(\Label, *FixWord) : WriteInteger(Compiler\Outputfile, *DataStart + *FixWord\IValue)
ElseIf CheckPrefix(\Label, #ResEndPrefix) Or *FixWord\Type = #wResLabel ; Resource labels fixing:
Define *Corrector.WordData = GetWord(#ResCorrector + Str(*FixWord\Extra))
If *Corrector : WriteInteger(Compiler\OutputFile, Compiler\ResSection + *Corrector\IValue)
Else : WriteInteger(Compiler\Outputfile, Compiler\ResSection + *FixWord\IValue)
EndIf
ElseIf CheckPrefix(\Label, #IsoEndPrefix)
WriteInteger(Compiler\Outputfile, IsolationStart + *FixWord\IValue)
Else ; Jumps & datalabel fixing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
If IsExec(*FixWord\Type) And (*FixWord\Extra Or \Isolated = 0)
If *FixWord\Extra = \Isolated 
If \Extra : FileSeek(Compiler\OutputFile, Loc(Compiler\OutputFile) - #OpCodeSize)
Emit(#iRelPtr, #False)
EndIf
WriteInteger(Compiler\OutputFile, *FixWord\IValue - \Base)
Else : WriteInteger(Compiler\OutputFile, *FixWord\IValue + IsolationStart) 
EndIf
Else : WriteInteger(Compiler\OutputFile, *FixWord\IValue)
EndIf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
If *FixWord\Type = #wProcedure And \NotFound = #eUndefined : Emit(#iCall, #False) : EndIf
EndIf
EndWith
Next
EndProcedure

Macro LateBound(LabelName, Pos = -1, WType = #wLabel, WFlag = Compiler\Offset) ; Pseudo-procedure.
Define __LabelName.s = LabelName
ToBeFixed(__LabelName, Pos)
AddWord(__LabelName, WFlag, WType)
EndMacro

Macro PostPone(WordName, Position = -1, ErrCode = #eUndefined, Extra = 0) ; Pseudo-procedure.
ToBeFixed(WordName, Position, ErrCode, Extra) : AbsPointer()
EndMacro

Macro PostPoneEx(WordName, PtrType = 'Abs', ErrCode = #eUndefined, Extra = 0) ; Pseudo-procedure.
ToBeFixed(WordName, -1, ErrCode, Extra) 
Select PtrType
Case 'Data' : DataPointer()
Case 'Rel'  : RelPointer()
Case 'LitI' : LiteralI(0, #False)
Default     : AbsPointer()
EndSelect
EndMacro

Macro PostBound(LabelName, WFlag = Compiler\Offset, Position = -1) ; Pseudo-procedure.
LateBound(LabelName, Position, #wDataLabel, WFlag)
AbsPointer()
EndMacro

Macro ExceedInt(Text) ; Pseudo-procedure.
((Len(Text) * Compiler\CharSize) >= Compiler\IntSize)
EndMacro

Procedure LiteralS(Text.S, Optimize = #True)
If Optimize <> 'No' ; Если требуется запись операнда...
If Text = "" : Emit(#iFalseI) : ProcedureReturn : EndIf
If Optimize And ExceedInt(Text)
LateBound(#StrPrefix + Text, -1, #wDataLabel)
LitSPointer(0)
ProcedureReturn #True ; Рапортуем о косвенном включении.
Else : Emit(#iLitS) : EmitString(Text)
EndIf
Else : EmitString(Text) ; Просто вписываем данные.
EndIf
EndProcedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Procedure ImportSysCall(CallName.s) ; Pseudo-procedure.
CallName = #ImportPrefix + CallName
If FindMapElement(Compiler\Dictionary(), CallName) = #Null
Compiler\Header\ImportTable + 1 ; Increment calls count.
EndIf
LateBound(CallName, -1, #wDataLabel)
EndProcedure

Procedure EmitSysCall(CallName.s)
ImportSysCall(CallName)
LitIPointer() : Emit(#iSysCallPtr)
EndProcedure

Macro EmitCharPrinter(Char) ; Pseudo-procedure.
UnMacro(Str(Char) + ":c emit")
EndMacro

Macro EmitValueWriter(Type) ; Pseudo-procedure.
UnMacro(Type + "->s type")
EndMacro

Macro EmitStringReader() ; Pseudo-procedure.
EmitSysCall("Minimal.String.Read")
EndMacro

Macro EmitStringWriter() ; Pseudo-procedure.
EmitSysCall("Minimal.String.Write")
EndMacro

Macro EmitSeqDropper() ; Pseudo-procedure.
EmitSysCall("Minimal.Sequence.Drop")
EndMacro

Macro EmitSeqNipper() ; Pseudo-procedure.
EmitSysCall("Minimal.Sequence.Nip")
EndMacro

Macro EmitSeqIF(Reverse = #False) ; Pseudo-procedure.
AddIF(0, Reverse) : EmitSeqNipper() : InsertElse() : EmitSeqDropper() : CompileIf()
EndMacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Macro VarSelector(TypeIdent, Action, FullyMutable = #False) ; Partializer.
Select TypeIdent
Case #vByte   : Emit(Action#B)
Case #vWord   : Emit(Action#W)
Case #vChar   : Emit(Action#C)
CompilerIf FullyMutable
Case #vLong   : Emit(Action#L)
Case #vQuad   : Emit(Action#Q)
Case #vFloat  : Emit(Action#F)
Case #vDouble : Emit(Action#D)
CompilerElse
Case #vLong, #vFloat  : Emit(Action#L)
Case #vQuad, #vDouble : Emit(Action#Q)
CompilerEndIf
Case #vInteger, #vLabelPtr, #vProcPtr, #vString : Emit(Action#I)
EndSelect
EndMacro

Macro CellCorrection(Type) ; Partializer
Define ElSize = ValType2Size(Type)
If ElSize > 1 : LiteralI(ElSize) : Emit(#iMulI) : EndIf
EndMacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Macro ValPointer(Location, Value = 0) ; Partializer.
If Location : DataPointer(Value) : Else : AbsPointer(Value) : EndIf
EndMacro

Macro ArrStartRetriever(ArrayName, Link) ; Partializer.
If Link : Define *Chain.WordData = Link
ToBeFixed(#ArrDataPrefix + ArrayName)
ValPointer(*Chain\ChainLink\Extra) : *Chain\IValue = #True
Else : ValReader(#ArrPtrPrefix + ArrayName)
EndIf
EndMacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Macro ArrayElementPtr(Arr, Type, Link) ; Partializer
CellCorrection(Type)
ArrStartRetriever(Arr, Link)
Emit(#iAddI)
EndMacro

Procedure VarBound(VarName.s, *Word.WordData)
If CheckPrefix(VarName, #VarPtrPrefix)
VarName = LCut(VarName, Len(#VarPtrPrefix))
EndIf
With *Word
If \Storage ; If it's local data...
If CheckPrefix(VarName, #ArrPtrPrefix) Or \IValue = #vString
Compiler\ProcGCList(VarName) = *Word
EndIf
EndIf
VarName = #VarPrefix + VarName
ToBeFixed(VarName) : AddWord(VarName, \IValue, #wDataLabel, \Storage)
ValPointer(\Storage) ; Указатель на переменную.
EndWith
EndProcedure

Procedure ValReader(VarName.s, TypeIdent.i = #vInteger, ArraySize.i = #NotArray, ExecPtrs.i = #False)
If ArraySize <> #NotArray : ArrayElementPtr(VarName, TypeIdent, ArraySize)
Else : VarBound(VarName, FindWordFast(VarName))
EndIf : VarSelector(TypeIdent, #iRead)
Select TypeIdent
Case #vLabelPtr : If ExecPtrs : Emit(#iJmp)  : EndIf
Case #vProcPtr  : If ExecPtrs : Emit(#iCall) : EndIf
Case #vString   : EmitStringReader()
EndSelect
EndProcedure

Macro EmitVarPlacer(VType) ; Pseudo-procedure.
If VType = #vString : EmitSysCall("Minimal.RunTime.PlaceString")
Else : VarSelector(VType, #iWrite)
EndIf
EndMacro

Procedure ValWriter(VarName.s, Operation.i = '')
FindWord(*Word, VarName)
With *Word
If IsData(\Type)
If \Type = #wArray : ArrayElementPtr(VarName, \IValue, \ChainLink)
Else : VarBound(VarName, *Word)
EndIf
If Operation
If \IValue <> #vString
Emit(#iDupI) : Emit(#iPopRS)
VarSelector(\IValue, #iRead)
Select Operation
Case '+'  : VarSelector(\IValue, #iAdd, #True)
Case '-'  : VarSelector(\IValue, #iSwap) : VarSelector(\IValue, #iSub, #True)
Case '*'  : VarSelector(\IValue, #iMul, #True)
Case '/'  : VarSelector(\IValue, #iSwap) : VarSelector(\IValue, #iDiv, #True)
Case '+1' : VarSelector(\IValue, #iInc, #True)
Case '-1' : VarSelector(\IValue, #iDec, #True)
Case '+2' : VarSelector(\IValue, #iInc, #True) : VarSelector(\IValue, #iInc, #True)
Case '-2' : VarSelector(\IValue, #iDec, #True) : VarSelector(\IValue, #iDec, #True)
Default
EndSelect 
Emit(#iPushRS)
ElseIf Operation = '+' : EmitSysCall("Minimal.RunTime.AddString")
ProcedureReturn #True
Else : TypeMismatchError(__OldName)  
EndIf
EndIf
EmitVarPlacer(\IValue)
Else : TypeMismatchError(__OldName) 
EndIf
Else : UndefinedVal(__OldName)
EndWith
EndIf
EndProcedure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Macro DefinerCheck(NewState, Err, ParseStr = #True) ; Partializer.
If Compiler\Interpreting Or Compiler\VoidWord\Extra : ChangeState(NewState)
If Compiler\VoidWord\Extra = #False
CompilerIf ParseStr : Compiler\VoidWord\SValue = RecallStr()
CompilerEndIf
Else : Compiler\VoidWord\Extra = #False
EndIf
Else : ThrowError(Err)
EndIf
EndMacro

Macro UseDefiner(WordType, DataType, Modifier, EarlyInit = #False) ; Pseudo-procuedure.
Select WordType
; -------------------------
CompilerIf EarlyInit = #False Or EarlyInit = 'Body' ; Только для макросов и вызовов.
Case #wMacro   : DefinerCheck(#sMacroName, #eMacroOut)
Case #wSysCall : DefinerCheck(#sSysCallerName, #eSysCallerOut)
If Compiler\ForceImport : ImportSysCall(Compiler\VoidWord\SValue) : EndIf
CompilerEndIf
CompilerIf EarlyInit <> 'Body' ; Если это не операция извлечения тела...
Case #wLiteral 
If Compiler\Interpreting : ParseConstName(DataType) 
Else : ThrowError(#eConstantOut)
EndIf
CompilerIf EarlyInit <> 'Res>' ; Если это не ресурсная операция...
CompilerIf EarlyInit <> 'Arr>' ; Если это не инициализация массива.
Case #wVariable : ParseVarName(DataType)
CompilerEndIf ; Required in any case:
; -------------------------
Case #wArray ; Инициализация массива.  
If Modifier  ; Если предполагается статический...
If Compiler\Interpreting = #False : ThrowError(#eStaticArrOut) : EndIf
Compiler\VoidWord\ChainLink = RecallInt() ; Получаем предполагаемое кол-во ячеек.
If Compiler\VoidWord\ChainLink < 0 : ThrowError(#eNegArrSize) : EndIf
Else : Compiler\VoidWord\ChainLink = -1 ; Динамический размер.
EndIf : ParseArrayName(DataType) ; Парсинг имени массива.
CompilerEndIf
CompilerIf EarlyInit<>'Val>' And EarlyInit<>'Arr>' And EarlyInit<>'Str>'
Case #wResLabel : ChangeState(#sResLabelName)
CompilerEndIf
CompilerEndIf
CompilerIf EarlyInit = #False ; If no eraly init required...
Case #wDefiner  : ChangeState(#sValueGroup) : SaveState()
CompilerElse ; Is some errors could be shown:
Default : TypeMismatchError(__OldName)
CompilerEndIf
EndSelect
CompilerIf EarlyInit : Compiler\EarlyInitFlag = EarlyInit : CompilerEndIf
EndMacro

Macro SetEIOperation(Value) ; Abstractor
Compiler\EarlyInitOp = Value
EndMacro 

Procedure CheckValInitializer(WordName.S, Operation = '')
FindWord(*Word, WordName)
If *Word\Type = #wDefiner ; If it's definition word...
UseDefiner(*Word\Extra, *Word\IValue, *Word\ChainLink, 'Val>')
SetEIOperation(Operation)
ProcedureReturn #True
EndIf
EndIf
EndProcedure

Macro TryValWriting(VarName, Operation = '') ; Partializer
Repeat ; Just to enable 'break' instruction.
If CheckValInitializer(VarName, Operation) : Break : EndIf
ValWriter(VarName, Operation) : ResetState() ; Setting everything as planned.
Until #True ; There shouldn't be any additional code.
EndMacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Macro IsDisposable(Type) ; Pseudo-procedure
(Type = #vString)
EndMacro

Macro GetElementSize(Arr_Type, AltFlag = #True) ; Partializer.
; ----------------------
If Arr_Type = #vString And (Compiler\ArraysGC Or AltFlag = #Paranoic)
Emit(#iNegOneB)
CompilerIf AltFlag = #True
ElseIf Arr_Type = #vString : Emit(#iFalseB)
CompilerEndIf
Else : LiteralB(ValType2Size(Arr_Type), #True)
EndIf
; ----------------------
EndMacro

Macro PrepareArrResizing(Arr_Name, Arr_Type, AltFlag = #False) ; Partializer.
GetElementSize(Arr_Type, AltFlag)
VarBound(#ArrSizePrefix + Arr_Name, FindWordFast(#ArrSizePrefix + Arr_Name))
VarBound(#ArrPtrPrefix  + Arr_Name, FindWordFast(#ArrPtrPrefix  + Arr_Name))
EndMacro

Procedure ArrResizer(ArrName.s, ArrType, ClearArr.i = #False)
If ClearArr = #Paranoic
PrepareArrResizing(ArrName, ArrType, #Paranoic)
Else : PrepareArrResizing(ArrName, ArrType)
EndIf
If ClearArr : EmitSysCall("Minimal.RunTime.ReInitArray")
Else : EmitSysCall("Minimal.RunTime.ResizeArray")
EndIf
EndProcedure

Macro ArrCellsRetriever(ArrayName, Link) ; Partializer.
If Link : PostPoneEx(#ArrCellsPrefix + ArrayName, 'LitI', 0)
Else : ValReader(#ArrSizePrefix + ArrayName)
EndIf
EndMacro

Macro ArrSizeRetriever(ArrayName, ArrType, Link) ; Partializer.
If Link : PostPoneEx(#ArrSpacePrefix + ArrayName, 'LitI', 0)
Else : ArrCellsRetriever(ArrayName, Link) : CellCorrection(ArrType)
EndIf
EndMacro

Macro StrArrFlag(Arr_Type) ; Partializer.
If Arr_Type = #vString : Emit(#INegOneB) : Else : Emit(#iFalseB) : EndIf
EndMacro

Macro MakeArrayFromSequence(ArrName, ArrType) ; Partializer.
PrepareArrResizing(ArrName, ArrType, #True)
EmitSysCall("Minimal.RunTime.MakeArrFromSeq")
EndMacro

Macro StaticResizingErr(ArrName) ; Partializer.
ThrowError(#eStaticSize, "Unable to resize on run-time static array '" + ArrName + "'")
EndMacro

Macro StaticCheck() ; Partializer.
If *Word\ChainLink : StaticResizingErr(__OldName) : EndIf
EndMacro

Macro ByteFill() ; Partializer.
EmitSysCall("Minimal.Misc.ByteFillMemory")
EndMacro

Procedure ProcessArray(ArrName.s, Operation.q) ; Pseudo-procedure.
FindWord(*Word, ArrName)
With *Word
If \Type = #wArray
Select Operation
Case '@bnd' : ArrCellsRetriever(ArrName, \ChainLink)
Case '!arr' : StaticCheck() : ArrResizer(ArrName, \IValue, #True)
Case '!bnd' : StaticCheck() : ArrResizer(ArrName, \IValue)
Case '+bnd' : StaticCheck() : ArrCellsRetriever(ArrName, \ChainLink)
Emit(#iAddI) : ArrResizer(ArrName, \IValue)
Case '@1st' : ArrStartRetriever(ArrName, \ChainLink)
Case 'size' : ArrSizeRetriever(ArrName, \IValue, \ChainLink)
Case '@end' : ArrStartRetriever(ArrName, \ChainLink)
ArrSizeRetriever(ArrName, \IValue, \ChainLink) : Emit(#iAddI)
Case '?ele' : ArrayElementPtr(ArrName, \IValue, \ChainLink)
Case '@seq' : ArrStartRetriever(ArrName, \ChainLink) : GetElementSize(\IValue)
EmitSysCall("Minimal.RunTime.ReadArrSeq")
Case '!seq' : ArrayElementPtr(ArrName, \IValue, \ChainLink) : StrArrFlag(\IValue)
EmitSysCall("Minimal.RunTime.WriteArrSeq")
Case 's->a' : StaticCheck() : MakeArrayFromSequence(ArrName, \IValue)
Case '{>>}' : StaticCheck() : PrepareArrResizing(ArrName, \IValue, #True)
EmitSysCall("Minimal.RunTime.InsertSeq2Arr")
Case '->>*' : StaticCheck() : PrepareArrResizing(ArrName, \IValue, #True)
EmitSysCall("Minimal.RunTime.ArrInsertElement") : EmitVarPlacer(\IValue)
Case '0000' : ArrStartRetriever(ArrName, \ChainLink) 
If IsDisposable(\IValue) : ArrCellsRetriever(ArrName, \ChainLink)
EmitSysCall("Minimal.RunTime.ClearArray") ; Вызов очистки.
Else : ArrSizeRetriever(ArrName, \IValue, \ChainLink) : LiteralB() : ByteFill()
EndIf
EndSelect
ElseIf \Type = #wDefiner ; If should be defined first...
UseDefiner(\Extra, \IValue, \ChainLink, 'Arr>') ; Early array initializer.
SetEIOperation(Operation) ; Выставляем флаг для будущей операции.
ProcedureReturn #False ; State shouldn't be reset !
Else : TypeMismatchError(__OldName)
EndIf
Else : UndefinedWord("array", __OldName)
EndIf
EndWith
ProcedureReturn #True
EndProcedure

Macro StrSizeRetriever(StrVarName) ; Partializer.
EmitSysCall("Minimal.RunTime.StrVarSize")
EndMacro

Macro GetCharPointer(StrVarName) ; Partializer.
EmitSysCall("Minimal.RunTime.StrCharPtr")
EndMacro

Procedure ProcessString(SVarName.s, Operation.q)
FindWord(*Word, SVarName)
With *Word
If \IValue = #vString
Select \Type
Case #wArray    : ArrayElementPtr(SVarName, #vString, \ChainLink)
Case #wVariable : VarBound(SVarName, *Word)
Case #wDefiner  : UseDefiner(\Extra, \IValue, \ChainLink, 'Str>') ; Early value initializer.
SetEIOperation(Operation) : ProcedureReturn #False
Default : TypeMismatchError(__OldName) 
EndSelect
Select Operation
Case '@len' : EmitSysCall("Minimal.RunTime.StrVarLen")
Case '!len' : EmitSysCall("Minimal.RunTime.ResizeStrVar")
Case '@1st' : Emit(#iReadI)
Case 'size' : StrSizeRetriever(SVarName)
Case '@end' : Emit(#iDupI) : Emit(#iReadI) : Emit(#iSwapI)
StrSizeRetriever(SVarName) : Emit(#iAddI)
Case 'void' : EmitSysCall("Minimal.RunTime.FreeStrVar")
Case '@chr' : GetCharPointer(SVarName) : Emit(#iReadC)
Case '!chr' : GetCharPointer(SVarName) : Emit(#iWriteC)
Case '?chr' : GetCharPointer(SVarName)
Case '+chr' : EmitSysCall("Minimal.RunTime.AddStrChar")
EndSelect
Else : TypeMismatchError(__OldName) 
EndIf
Else : UndefinedWord("string variable", __OldName)
EndIf
EndWith
ProcedureReturn #True
EndProcedure

Procedure Operation2Lit(Operation) ; Pseudo-procedure.
Select Operation
Case 'size' : ProcedureReturn 'LitI'
Case '@end' : ProcedureReturn 'Abs'
EndSelect
EndProcedure

Procedure GetResourceInfo(ResLabel.s, Operation.q) ; Pseudo-procedure.
FindWord(*Word, ResLabel)
With *Word
Select \Type
Case #wResLabel : PostPoneEx(ResOperation2Prefix(Operation) + Str(\IValue), Operation2Lit(Operation))
Case #wDefiner : UseDefiner(\Extra, \IValue, \ChainLink, 'Res>') : SetEIOperation(Operation) : ProcedureReturn #False
Default : TypeMismatchError(__OldName)
EndSelect
Else : PostPoneEx(__OldName, Operation2Lit(Operation), #eFakeResLbl, Operation)
EndIf
EndWith
ProcedureReturn #True
EndProcedure

Macro GetProcInfo(ProcName, Operation) ; Pseudo-procedure.
FindWord(*Word, ProcName)
If *Word\Type = #wProcedure
If *Word\Extra <> #Isolated : IsolationRequired(ProcName) : EndIf
PostPoneEx(ProcOperation2Prefix(Operation) + ProcName, Operation2Lit(Operation))
Else : TypeMismatchError(__OldName)
EndIf
Else : PostPoneEx(__OldName, Operation2Lit(Operation), #eFakeProc, Operation)
EndIf
EndMacro

Procedure ExtractSize(WordName.s)
FindWord(*Word, WordName)
With *Word
Select *Word\Type
Case #wDefiner : UseDefiner(\Extra, \IValue, \ChainLink, 'Size') : ProcedureReturn #False
Case #wVariable, #wArray, #wDataLabel : LiteralI(ValType2Size(\IValue))
Case #wResLabel : GetResourceInfo(WordName, 'size')
Case #wLiteral : LiteralI(LitIdent2Size(\TypeID))
Default : TypeMismatchError(__OldNAme)
EndSelect
Else : UndefinedVal(__OldNAme)
EndIf
EndWith
ProcedureReturn #True
EndProcedure

Macro ExhaleString(Text) ; Partializer.
If Compiler\Interpreting
UniPusher('s', 0, 0, Text)
Else : LiteralS(Text)
EndIf
EndMacro

Macro ExhaleInteger(Value) ; Partializer.
If Compiler\Interpreting
UniPusher('q', Value)
Else : LiteralI(Value)
EndIf
EndMacro

Procedure ExtractBody(WordName.s)
FindWord(*Word, WordName)
With *Word
Select *Word\Type
Case #wDefiner : UseDefiner(\Extra, \IValue, \ChainLink, 'Body') : ProcedureReturn #False
Case #wMacro, #wSysCall : ExhaleString(*Word\SValue)
Default : TypeMismatchError(__OldName)
EndSelect 
Else : ThrowError(#eInvBReq, "Body request for undefined word '"+__OldName+"'")
EndIf
EndWith
ProcedureReturn #True
EndProcedure

Procedure FindWordHere(WordName.s)
Define *Word.WordData, Simple.i = #True
If CompilingColon() : *Word = GetWord(WordFormatter(WordName, #True))
If *Word : ProcedureReturn *Word : EndIf : Simple = #False
EndIf : ProcedureReturn GetWord(WordFormatter(WordName, Simple))
EndProcedure

Macro CheckDefinition(WordName, WholeTable = Compiler\SubState) ; Pseudo-procedure.
Define Found.i
If WholeTable : Found = FindWordFast(WordName) : Else : Found = FindWordHere(WordName) : EndIf
If Found : ExhaleInteger(#True) : Else : ExhaleInteger(#False) : EndIf
EndMacro

Macro Jump2Word(Word, FullName, ShortName, OpCode = 'Jmp') ; Pseudo-procedure.
If Word\Extra : ToBeFixed(FullName)
If Word\Extra = Compiler\ProcNum And CompilingColon()
CompileJump(0, OpCode, 'Rel')
ProcedureReturn #True
ElseIf Word\Extra > 0 : OutCallAlert(ShortName)
ElseIf FullName = Compiler\ProcName
Warn("Void procedure '" + ShortName + "' was called recursively")
EndIf
ElseIf CompilingColon() = 0
InternalJump(Word\IValue, OpCode)
ProcedureReturn #True
EndIf
CompileJump(Word\IValue, OpCode, 'Abs')
EndMacro

Macro StopImport(NS) ; Pseudo-procedure.
#IT$ = #ITDelimiter$
Compiler\ImportTable = ReplaceString(Compiler\ImportTable, #IT$ + NS + #IT$, #IT$)
EndMacro

Macro StartImport(NS) ; Pseudo-procedure.
StopImport(NS)
Compiler\ImportTable = #ITDelimiter$ + NS + Compiler\ImportTable
EndMacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Procedure.s LitLocationName(*LitWord.WordData) ; Pseudo-procedure.
With *LitWord
Define Type.q = \TypeID
Select Type ; Оптимизация... На будущее.
Case 'l' : If Compiler\IntSize = SizeOf(Long)                   : Type = 'i' : EndIf
Case 'q' : If Compiler\IntSize = SizeOf(Quad)                   : Type = 'i' : EndIf
Case 'b' : If Compiler\CharSize = SizeOf(Byte) And \QValue => 0 : Type = 'c' : EndIf
Case 'w' : If Compiler\CharSize = SizeOf(Word) And \QValue => 0 : Type = 'c' : EndIf
EndSelect
Select Type ; В зависимости от типа...
Case ' ' To '    ' : ProcedureReturn #LitLocPrefix + PeekS(@\Extra, 4) + Str(\QValue)
Case 'f' : ProcedureReturn #LitLocPrefix + "f" + StrF(\DValue)
Case 'd' : ProcedureReturn #LitLocPrefix + "d" + StrD(\DValue)
Case 's' : ProcedureReturn #LitLocPrefix + "s" + \SValue
Default  : ProcedureReturn #LitLocPrefix + Chr(Type) + Str(\QValue)
EndSelect
EndWith
EndProcedure

Macro LinkOffset(LitData, Offset) ; Partializer.
If LitData\ChainLink = #Null : LitData\ChainLink = Offset + #OpCodeSize : EndIf
EndMacro

Macro CopyLit(Parent, Child) ; Pseudo-procedure.
If Child\TypeID=0:Child\TypeID=Parent\TypeID:Child\QValue=Parent\QValue:Child\SValue=Parent\SValue:EndIf
EndMacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Macro RequestLit(LitData) ; Psudo-procedure.
Define *OffsetHere = Compiler\Offset ; Сохраняем смещение.
If LitWritingBlock(LitData, Compiler\NumOpt, Compiler\StrOpt) = #False ; If data was really written...
Define *Location.WordData = GetWord(AddWord(LitLocationName(LitData))) ; Создаем слово-указатель.
LinkOffset(*Location, *OffsetHere) : CopyLit(LitData, *Location)       ; Копируем данные литерала.
EndIf
EndMacro

Macro TryInlining(FullName, ShortName, MacroData) ; Partializer.
If MacroData <> Compiler\VoidWord : ExpandMacro(FullName, ShortName, MacroData) : EndIf
EndMacro

Procedure CheckDictionary(Word.S)
FindWord(*Word, Word)
With *Word
Select \Type
Case #wLabel     : Jump2Word(*Word, Word, __OldName)
Case #wProcedure : Jump2Word(*Word, Word, __OldName, 'Call')
Case #wVariable  : ValReader(Word, \IValue, #NotArray, #True)
Case #wArray     : ValReader(Word, \IValue, \ChainLink, #True)
Case #wResLabel  : PostPone(Word, -1, 0)
Case #wContext   : StartImport(Word)
Case #wLiteral   : RequestLit(*Word)
Case #wDataLabel : LiteralI(ValType2Size(*Word\IValue))
Case #wDefiner   : UseDefiner(\Extra, \IValue, \ChainLink)
Case #wSysCall   : EmitSysCall(\SValue)
Case #wMacro     : TryInlining(Word, __OldName, *Word)
EndSelect
EndWith
ProcedureReturn #True
EndIf
EndProcedure

Macro LitPointer(LitName, LitData) ; Pseudo-procedure.
Define FixName.s = LitName                        ; Аккумулятор имени.
FixName = LitLocationName(LitData)                ; Указатель на место.
Define *Word.WordData = GetWord(AddWord(FixName)) ; Создаем хранилище данных.
CopyLit(LitData, *Word)                           ; Копируем данные литерала.
AddLink(FixName,#LitPtrPrefix+FixName,0,0,#False) ; Связываем.
PostPone(#LitPtrPrefix+FixName)                   ; Adding fixer.
EndMacro

Procedure ExtractPointer(WordName.s)
FindWord(*Word, WordName)
With *Word
Select \Type
Case #wLabel, #wProcedure 
;;;;;;;;;;;;;;;;;;
If \Extra = 0 : AbsPointer(\IValue) : Else : PostPone(WordName) : EndIf
;;;;;;;;;;;;;;;;;;
Case #wVariable : VarBound(WordName, *Word)
Case #wResLabel : PostPone(WordName, -1, 0)
Case #wArray    : VarBound(#ArrPtrPrefix + WordName, *Word)
Case #wLiteral  : LitPointer(WordName, *Word)
Case #wDefiner  : UseDefiner(\Extra, \IValue, \ChainLink, 'Ptr>') ; Early value initializer.
ProcedureReturn #False ; Сигнализируем, что сбрасывать состояние пока рано.
Default : TypeMismatchError(__OldName)
EndSelect
;;;;;;;;;;;;;;;;;;;
Else : PostPone(__OldName, -1, #eInvPReq)
;;;;;;;;;;;;;;;;;;;
EndIf
EndWith
ProcedureReturn #True
EndProcedure

Procedure TypePriming(Type.q)
If Type > ' ' : ProcedureReturn 'i'
Else          : ProcedureReturn Type
EndIf
EndProcedure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Procedure.a FindMod(DefaultMod.q, *Prim.Primitive)
With *Prim
DefaultMod = TypePriming(DefaultMod)
Select DefaultMod ; Анализируем модификатор.
Case 'f' : If \FloatVer : ProcedureReturn 'f' : Else : ProcedureReturn 'l' : EndIf
Case 'd' : If \FloatVer : ProcedureReturn 'd' : Else : ProcedureReturn 'q' : EndIf
Default  : If \IntVer   : ProcedureReturn DefaultMod : Else : ProcedureReturn 'f' : EndIf
EndSelect
EndWith
EndProcedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Procedure TryPriming(Word.S)
With Compiler
If CheckDictionary(Word) : ProcedureReturn #True : EndIf
Define *Prim.Primitive = FindMapElement(\Primitives(), Word)
If *Prim ; Если это примитив...
If *Prim\NamingDir ; Если это просто шаблон...
*Prim.Primitive=FindMapElement(\Primitives(),NamePrimitive(Word,FindMod(\DefDataType,*Prim),*Prim\NamingDir))
EndIf : Emit(*Prim\OpCode) ; Компиляция.
ProcedureReturn #True
EndIf
EndWith
EndProcedure

Macro SetEntryPoint(Dest) ; Pseudo-procedure.
If CompilingColon() : ThrowError(#eProcEntry) : EndIf
If Compiler\Header\EntryPoint <> #Uninitialized
Warn("Entry point was redefined") : EndIf
Compiler\Header\EntryPoint = Dest
EndMacro

Macro ReportState(State, OptionName) ; Pseudo-procedure.
CompilerIf State : Inform(OptionName + " enabled")
CompilerElse : Inform(OptionName + " disabled")
CompilerEndIf
EndMacro

Macro SetWarningsState(State = #False)
ReportState(State, "Warning system")
Compiler\ShowWarnings = State
EndMacro

Macro SetXInclusionState(State = #False)
ReportState(State, "Exclusivity check on inclusion")
Compiler\XInclusion = State
EndMacro

Macro SetXInfusionState(State = #False)
ReportState(State, "Exclusivity check on datafile infusion")
Compiler\XInfusion = State
EndMacro

Macro SetSOState(State = #False)
ReportState(State, "String literals optimization")
Compiler\StrOpt = State
EndMacro

Macro SetNOState(State = #False)
ReportState(State, "Numerical literals optimization")
Compiler\NumOpt = State
EndMacro

Macro SetTHOState(State = #False)
ReportState(State, "TIMES[ performace optimization")
Compiler\TimesHeadOpt = State
EndMacro

Macro SetTTOState(State = #False)
ReportState(State, "]TIMES performace optimization")
Compiler\TimesTailOpt = State
EndMacro

Macro SetAGCState(State = #False)
ReportState(State, "Garbage collection support for arrays")
Compiler\ArraysGC = State
EndMacro

Macro SetCGCState(State = #False)
ReportState(State, "Garbage collection support for colon definitions")
Compiler\ColonGC = State
EndMacro

Macro SetFTState(State = #False)
ReportState(State, "Automatic 'fall-through' in {SWITCH:..;SWITCH blocks")
Compiler\FallThrough = State
EndMacro

Macro SetABState(State = #False)
ReportState(State, "Additional 'BYE' instruction appending")
Compiler\Addbye = State
EndMacro

Macro SetDMState(State = #False)
ReportState(State, "Merging resources directly with code")
Compiler\DirectMerging = State
EndMacro

Macro SetBEState(State = #False)
ReportState(State, "Big endian encoding for literalized strings")
Compiler\BigEndianStr = State
EndMacro

Macro SetFIState(State = #False)
ReportState(State, "Forced import for every defined caller")
Compiler\ForceImport = State
EndMacro

Macro PortBlock(Dir) ; Partializer.
ElseIf CheckPrefix(WName, #Dir#Prefix)
AddElement(Compiler\Dir#Stack())
Compiler\Dir#Stack() = Compiler\Dictionary()
Compiler\Dir#Stack()\SValue = LCut(WName, Len(#Dir#Prefix))
EndMacro

;;;;;
Macro FinalizeDict() ; Pseudo-procedure.
ForEach Compiler\Dictionary()
Define *Word.WordData = Compiler\Dictionary()
Define WName.S = MapKey(Compiler\Dictionary())
If CheckPrefix(WName, #StrPrefix) ; Если это данные строки.
*Word\IValue = Compiler\Offset
WName = LCut(WName, Len(#StrPrefix))
EmitString(WName)
ElseIf CheckPrefix(WName, #ArrDataPrefix) ; Если это статический массив...
Define ArrayName.s = LCut(WName, Len(#ArrDataPrefix))
AddWord(#ArrCellsPrefix + ArrayName, *Word\Extra)
Define ArrBytes = ValType2Size(*Word\ChainLink\IValue) * *Word\Extra
AddWord(#ArrSpacePrefix + ArrayName, ArrBytes)
Compiler\Dictionary(WName) ; Сброс позиции.
If *Word\IValue ; If it was referenced...
Define ValBase = Compiler\LocalPools(*Word\ChainLink\Extra)
Compiler\LocalPools(*Word\ChainLink\Extra) + ArrBytes
*Word\IValue = ValBase ; Начало области данных.
EndIf
ElseIf CheckPrefix(WName, #VarPrefix) ; Если это переменная...
ValBase = Compiler\LocalPools(*Word\Storage)
Compiler\LocalPools(*Word\Storage) + ValType2Size(*Word\IValue)
*Word\IValue = ValBase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ElseIf CheckPrefix(WName, #LitPtrPrefix)     ; Если это указатель на литерал...
If *Word\ChainLink\ChainLink                 ; Если литерал уже использовался...
*Word\IValue = *Word\ChainLink\ChainLink     ; Выставляем адрес.
Else : *Word\IValue = Compiler\Offset        ; Запоминаем текущее смещение.
LitWritingBlock(*Word\ChainLink, 'No', 'No') ; Вписываем данные литерала.
EndIf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PortBlock(Import)
PortBlock(Export)
EndIf
Next
EndMacro

Macro AddPConst(Name, Value) ; Pseudo-procedure.
AddWord(#PConstPrefix + Name, Value)
EndMacro
;}
;{ --Code structurizing--
Procedure AddStruct(SType.i, BoundVar.S = "", ExtraVar.s = "")
Define *Struct.ConstructionData
With *Struct
Select SType
Case #cBreak, #cIfBreak, #cPass, #cIfPass
AddElement(Compiler\Breakerz())
*Struct = Compiler\Breakerz()
\Base = Compiler\Offset + #OpCodeSize
Default
AddElement(Compiler\Constructions())
*Struct = Compiler\Constructions()
\Base = Compiler\Offset
\Var = BoundVar
\Var2 = ExtraVar
StorePosition(\Loc)
EndSelect
\Type = SType
EndWith
ProcedureReturn *Struct
EndProcedure

Macro StructComparsion() ; Partializer.
Define *Struct.ConstructionData = Compiler\Constructions()
If *Struct\Type = sType Or *Struct\Type = oType.i Or *Struct\Type = eType.i
ProcedureReturn *Struct : EndIf
EndMacro

Procedure CheckStructType(sType.i, oType.i = -1, eType.i = -1)
If ListSize(Compiler\Constructions())
StructComparsion()
EndIf
EndProcedure

Procedure ScanConstructions(sType.i, oType.i = -1, eType.i = -1)
If ListSize(Compiler\Constructions())
Repeat : StructComparsion()
Until PreviousElement(Compiler\Constructions()) = #Null
EndIf
EndProcedure

Procedure.s NewCounter()
With Compiler
\LoopsLvl + 1
Define CName.s = TmpLabelName("Counter:")
CName = AddInternalVar(CName)
AddElement(\Counters())
\Counters() = CName
EndWith
ProcedureReturn CName
EndProcedure

Macro RemoveCounter() ; Pseudo-procedure
Compiler\LoopsLvl - 1
DeleteElement(Compiler\Counters())
EndMacro

Macro ElementsCount(CName, Word) ; Partializer.
Select Word\Type
Case #wArray : ProcessArray(CName, '@bnd')
Default : TypeMismatchError(__OldName)
EndSelect
EndMacro

Macro RequestElement(CName, Word)
Select Word\Type
Case #wArray : ValReader(CName, Word\IValue, Word\ChainLink)
EndSelect
EndMacro

Procedure GetDataType(*Word.WordData)
Select *Word\Type
Case #wVariable, #wArray, #wDataLabel : ProcedureReturn *Word\IValue
Case #wLiteral : ProcedureReturn LitIdent2ValType(*Word\TypeID)
EndSelect
EndProcedure

Macro RequestVal(FullName, Word, ShortName) ; Partializer.
Select Word\Type
Case #wVariable : ValReader(FullName, Word\IValue)
Case #wArray    : ValReader(FullName, Word\IValue, Word\ChainLink)
Case #wLiteral  : RequestLit(Word)
Case #wDataLabel ; NOP
Default : TypeMismatchError(ShortName)
EndSelect
EndMacro

Macro NewCase(Selector, DCounter) ; Partializer.
AddStruct(#cSwtON, Selector, DCounter)
Emit(#iDepth)
ValWriter(DCounter)
EndMacro

Macro AddBegin()
Compiler\LoopsLvl + 1
AddStruct(#cBegin)
EndMacro

Macro AddBreak(Conditional = #False) ; Pseudo-procedure.
If Compiler\LoopsLvl = 0 : ThrowError(#eBreakOut) : EndIf
CompilerIf Conditional
AddStruct(#cIfBreak)
InternalJump(0, 'IF')
CompilerElse
AddStruct(#cBreak)
InternalJump(0)
CompilerEndIf
EndMacro

Macro AddPass(Conditional = #False) ; Pseudo-procedure.
If Compiler\LoopsLvl = 0 : ThrowError(#ePassOut) : EndIf
CompilerIf Conditional
AddStruct(#cIfPass)
InternalJump(0, 'IF')
CompilerElse
AddStruct(#cPass)
InternalJump(0)
CompilerEndIf
EndMacro 

Macro AddTimes(Optimize = #False) ; Pseudo-procedure.
If Optimize <> #True
Emit(#iDupI)
Emit(#iFalseI)
Emit(#iLoEI)
EndIf
AddStruct(#cTimes)
InternalJump(0, 'IF')
ValWriter(NewCounter())
EndMacro

Macro AddIf(Block = #False, Reversed = #False) ; Pseudo-procedure.
CompilerIf Block : AddStruct(#cBlockIF)
CompilerElse : AddStruct(#cIF)
CompilerEndIf
CompilerIf Reversed : InternalJump(0, 'IF')
CompilerElse : InternalJump(0, 'IFZ')
CompilerEndIf
EndMacro

Macro AddSkip() ; Pseudo-procedure.
AddStruct(#cSkip)
InternalJump(0)
EndMacro

Macro AddFor() ; Pseudo-procedure.
; -Saving step and destination variables form prev. loop-
Define SVar.s = Compiler\StepVar
Define LVar.s = Compiler\LimitVar
; -Storing step value-
Compiler\StepVar = TmpLabelName("For_STEP")
Compiler\StepVar = AddInternalVar(Compiler\StepVar)
ValWriter(Compiler\StepVar)
; -Storing destintaion value-
Compiler\LimitVar = TmpLabelName("For_DEST")
Compiler\LimitVar = AddInternalVar(Compiler\LimitVar)
ValWriter(Compiler\LimitVar)
; -Storing destintaion value-
AddStruct(#cFor, SVar, LVar)
InternalJump(0)
ValWriter(NewCounter())
EndMacro

Macro AddForEach(Collection) ; Pseudo-procedure.
FindWord(*Word, Collection)
ElementsCount(Collection, *Word)
AddStruct(#cForEach, Compiler\ITemplate, Collection)
; -Create template variable-
Compiler\ITemplate = TmpLabelName("ITemplate:")
Compiler\ITemplate = AddInternalVar(Compiler\ITemplate, GetDataType(*Word))
; -Some primary checking-
InternalJump(0, 'IFZ') ; Jump if collection is empty.
Emit(#iFalseI)
; -Filling template-
Emit(#iDupI)
RequestElement(Collection, *Word.WordData)
ValWriter(Compiler\ITemplate)
ValWriter(NewCounter())
Else : ThrowError(#eFakeVal, "Request to undefined collection '" + __OldName + "'")
EndIf
EndMacro

Macro AddSwitch(Selector) ; Pseudo-procedure.
FindWord(*Word, Selector)
Define SVar.s = TmpLabelName(#STemplate)
Define DCVar.s = TmpLabelName("[Depth]:")
Define TI = GetDataType(*Word)
RequestVal(Selector, *Word, __OldName)
SVar = AddInternalVar(SVar, TI)
DCVar = AddInternalVar(DCVar)
ValWriter(SVar)
NewCase(Compiler\STemplate, DCVar)
Compiler\STemplate = SVar
Else : UndefinedVal(__OldName)
EndIf
EndMacro

Macro AddSequencer() ; Pseudo-procedure.
Define SVar.s = AddInternalVar(TmpLabelName("SeqStart:"))
Emit(#iDepth)
ValWriter(SVar)
AddStruct(#cSequence, SVar)
EndMacro

Procedure.S CompileAllBreakerz(Edge.i, PassesLabel.s)
Define *Struct.ConstructionData
Define FixLabel.s = TmpLabelName("Breaker:")
Repeat : *Struct = LastElement(Compiler\Breakerz())
If *Struct = #Null : Break 
ElseIf *Struct\Base <= Edge : Break
EndIf
If *Struct\Type = #cPass Or *Struct\Type = #cIfPass
ToBeFixed(PassesLabel, *Struct\Base)
Else : ToBeFixed(FixLabel, *Struct\Base)
EndIf
DeleteElement(Compiler\Breakerz())
ForEver
ProcedureReturn FixLabel
EndProcedure

Macro CompileFirstPart(SType, Type2 = -1) ; Partializer
Define *Struct.ConstructionData
*Struct = CheckStructType(SType, Type2)
If *Struct 
Define PassesLbl.s = TmpLabelName("PASS:")
CompilerIf Type2 = -2 : AddWord(PassesLbl, *Struct\Base, #wLabel)
CompilerElse : AddLabel(PassesLbl)
CompilerEndIf 
EndMacro

Macro FixStruct(LPrefix, FixOffset) ; Partializer
Define FixLabel.S = TmpLabelName(LPrefix)
LateBound(FixLabel, *Struct\Base + FixOffset)
EndMacro

Macro CaseBreaker() ; Partializer.
" CaseBreak:" + Compiler\STemplate
EndMacro

Macro FinishCase() ; Partializer.
ToBeFixed(CaseBreaker())
InternalJump(0)
EndMacro

Macro CompileLastPart(Error, FOffset = #PB_Ignore, FLabel = "") ; Partializer
CompilerIf FOffset <> #PB_Ignore
FixStruct(FLabel, FOffset)
CompilerEndIf
Select *Struct\Type
Case #cFor, #cTimes, #cBegin, #cForEach
Define BreaksLabel.S = CompileAllBreakerz(*Struct\Base, PassesLbl)
AddLabel(BreaksLabel)
EndSelect : If *Struct\Type <> #cBlockIf : DeleteElement(Compiler\Constructions()) : EndIf
Else 
CompilerIf Error = #eEndIfOut
If CheckStructType(#cBlockIf) : DeleteElement(Compiler\Constructions()) : Break 
Else : ThrowError(Error)
EndIf
CompilerElse : ThrowError(Error)
CompilerEndIf
EndIf
EndMacro

Macro CompileBegin(CondExit = #False) ; Pseudo-procedure.
CompileFirstPart(#cBegin, -2)
Compiler\LoopsLvl - 1
CompilerSelect CondExit
CompilerCase #True : InternalJump(*Struct\Base, 'IFZ')
CompileLastPart(#eUntilOut)
CompilerCase #False : InternalJump(*Struct\Base)
CompileLastPart(#eNoBegin)
CompilerCase '~end' : CompileLastPart(#eEndOut)
CompilerEndSelect
EndMacro

Macro CompileTimes(Optimize = #True) ; Pseudo-procedure.
CompileFirstPart(#cTimes)
ValReader(Compiler\Counters())
Emit(#iDecI)
Emit(#iDupI)
If Optimize = #False
Emit(#iFalseI)
Emit(#iGreaterI)
EndIf
InternalJump(*Struct\Base + Compiler\JumpSize, 'IF')
Emit(#iDropI)
RemoveCounter()
CompileLastPart(#eNoTimes, #OpCodeSize, "times[")
EndMacro

Macro InsertElse() ; Pseudo-procedure.
CompileFirstPart(#cIf, #cBlockIf)
AddStruct(#cElse)
PreviousElement(Compiler\Constructions())
InternalJump(0)
CompileLastPart(#eElseNoIF, #OpCodeSize, "else:")
LastElement(Compiler\Constructions())
EndMacro

Macro CompileIf(CloseAll = #False) ; Pseudo-procedure.
CompilerIf CloseAll
Repeat : CompileFirstPart(#cIf, #cElse)
CompileLastPart(#eEndIfOut, #OpCodeSize, "endif:")
ForEver
CompilerElse
CompileFirstPart(#cIf, #cElse)
CompileLastPart(#eThenNoIF, #OpCodeSize, "then:")
CompilerEndIf
EndMacro

Macro CompileSkip() ; Pseudo-procedure.
CompileFirstPart(#cSkip, #cElse)
CompileLastPart(#eNoSkip, #OpCodeSize, "skip:")
EndMacro

Macro CompileFor() ; Pseudo-procedure.
CompileFirstPart(#cFor)
ValReader(Compiler\Counters())
ValReader(Compiler\StepVar)
Emit(#iAddI)
LateBound(TmpLabelName("]rof"), *Struct\Base + #OpCodeSize)
Emit(#iDupI)
ValReader(Compiler\LimitVar)
Emit(#iSubI)
Emit(#iSgnI)
ValReader(Compiler\StepVar)
Emit(#iSgnI)
Emit(#iXorI)
InternalJump(*Struct\Base + Compiler\JumpSize, 'IF')
Emit(#iDropI)
Compiler\StepVar = *Struct\Var
Compiler\LimitVar = *Struct\Var2
RemoveCounter()
CompileLastPart(#eNoFor)
EndMacro

Macro CompileForEach()
CompileFirstPart(#cForEach)
ValReader(Compiler\Counters())
Emit(#iIncI)
Emit(#iDupI)
Define *Word.WordData = GetWord(*Struct\Var2)
ElementsCount(*Struct\Var2, *Word)
Emit(#iLessI)
InternalJump(*Struct\Base + Compiler\JumpSize + #OpCodeSize, 'IF')
Emit(#iDropI)
Compiler\ITemplate = *Struct\Var
RemoveCounter()
CompileLastPart(#eNoForEach, #OpCodeSize, "[ForEach:")
EndMacro

Macro InsertON() ; Pseudo-procedure.
CompileFirstPart(#cSwtDO)
If Compiler\FallThrough = #False : FinishCase() : EndIf
NewCase(*Struct\Var, *Struct\Var2)
PreviousElement(Compiler\Constructions())
CompileLastPart(#eONnoDO, #OpCodeSize, "[CASE_START]:")
LastElement(Compiler\Constructions())
EndMacro

Macro InsertDO() ; Pseudo-procedure.
CompileFirstPart(#cSwtON)
; Проверка условий.
Define TI = GetDataType(GetWord(Compiler\STemplate))
ValReader(*Struct\Var2)
ValReader(Compiler\STemplate, TI)
If TI = #vString 
EmitSysCall("Minimal.String.StackSearch")
Else : VarSelector(TI, #iSearch)
EndIf
; Компиляция перехода.
AddStruct(#cSwtDO, *Struct\Var, *Struct\Var2)
InternalJump(0, 'IFZ')
PreviousElement(Compiler\Constructions())
CompileLastPart(#eDOnoON)
LastElement(Compiler\Constructions())
EndMacro

Macro InsertDefault() ; Pseudo-procedure.
CompileFirstPart(#cSwtDO)
If Compiler\FallThrough = #False : FinishCase() : EndIf
AddStruct(#cDefault, *Struct\Var, *Struct\Var2)
PreviousElement(Compiler\Constructions())
CompileLastPart(#eDEFnoDO, #OpCodeSize, "[DEFAULT]:")
LastElement(Compiler\Constructions())
EndMacro

Macro CompileSwitch() ; Pseudo-procedure.
CompileFirstPart(#cSwtDO, #cDefault)
If *Struct\Type = #cSwtDO : FixStruct("[END_CASE]:", #OpCodeSize) : EndIf
AddLabel(CaseBreaker())
Compiler\STemplate = *Struct\Var
CompileLastPart(#eSwitchNoDO)
EndMacro

Macro CompileSequencer() ; Pseudo-procedure.
CompileFirstPart(#cSequence)
ValReader(*Struct\Var)
CompileLastPart(#eNoSeq)
EndMacro

Macro BoolOp(OpCode, Single = #False) ; Partializer.
CompilerIf Single : Emit(#iToBinI)
CompilerElse : Emit(#iTo2BinI)
CompilerEndIf
Emit(OpCode#I)
Emit(#iToBoolI)
EndMacro

Macro DataSizeCalc(DType, ActionOp = #iMulI) ; Partializer.
LiteralI(SizeOf(DType), ActionOp) : Emit(ActionOp)
EndMacro

Macro RewindWord(Part) ; Partializer.
If TPos + Len(Part) < WLen : ReparseWord(TPos + Len(Part)) : EndIf
EndMacro

Macro ThrowLocatedError(ErrNum, Location) ; Pseudo-procedure.
ThrowErrorEx(ErrNum, Location\LineNum, Location\SrcName)
EndMacro

Macro CheckUnfinishedStructs() ; Partializer.
Define ErrNum
LastElement(Compiler\Constructions())
Select Compiler\Constructions()\Type
Case #cSwtDo, #cSwtOn, #cDefault : ErrNum = #eSwitchOut
Case #cIf, #cElse : ErrNum = #eNoThen
Case #cBlockIf    : ErrNum = #eNoEndIf
Case #cTimes      : ErrNum = #eTimesOut
Case #cFor        : ErrNum = #eNoRof
Case #cForEach    : ErrNum = #eNoForEach
Case #cBegin      : ErrNum = #eNoAgain
Case #cSkip       : ErrNum = #eSkipOut
Case #cSequence   : ErrNum = #eSeqOut
EndSelect
If ErrNum : ThrowLocatedError(ErrNum, Compiler\Constructions()\Loc) : EndIf
EndMacro

Macro CheckStaticBlock() ; Pseudo-procedure.
If Compiler\DefStatic : ThrowLocatedError(#eNoStatic, Compiler\StaticLoc) : EndIf
EndMacro

Macro CheckUnfinished() ; Pseudo-procedure.
Define ErrNum
Select Compiler\State
Case #sCommented1, #sCommented2       : ErrNum = #eUnfinishedRem
Case #sVariableName , #sVariableWrite : ErrNum = #eNoVarName 
Case #sResLabelName , #sResLabelInfo  : ErrNum = #eNoResLabelName
Case #sArrayName    , #sArrayRequest  : ErrNum = #eNoArrName
Case #sProcedureName, #sIsoProcInfo   : ErrNum = #eNoProcName
Case #sConstantName  : ErrNum = #eNoConstName
Case #sMacroName     : ErrNum = #eNoMacroName
Case #sSysCallerName : ErrNum = #eNoSysCaller
Case #sStrRequest    : ErrNum = #eNoStrVarName
Case #sPtrRequest    : ErrNum = #eNoWordPtr
Case #sSizeRequest   : ErrNum = #eNoWordSize
Case #sLabelName     : ErrNum = #eNoLabelName
Case #sCollectionReq : ErrNum = #eNoWordForEach
Case #sSwitcherName  : ErrNum = #eNoSwitcher
Case #sContextName   : ErrNum = #eNoContextName
Case #sValueGroup    : ErrNum = #eNoDef
Default  ; Now let's check constructions and related stuff...
If ListSize(Compiler\Constructions()) : CheckUnfinishedStructs()
ElseIf ListSize(Compiler\NSExportStack()) > 1 : ThrowError(Compiler\NSExportStack()\ErrNum)
ElseIf Compiler\VoidProcs    : ThrowLocatedError(#eNoVoid, Compiler\VoidLoc)
ElseIf Compiler\Interpreting : ThrowLocatedError(#eNoInter, Compiler\InterLoc)
Else#CheckStaticBlock()
EndSelect
If ErrNum : ThrowLocatedError(ErrNum, Compiler\StateLoc) : EndIf
EndMacro
;}
;{ --Procs & scopes management--
Procedure StartExport(Name_Space.s, Err = 0, DP = #False) 
AddElement(Compiler\NSExportStack())
Define *ET.NameSpaceData = Compiler\NSExportStack()
With *ET
\Name = Name_Space
\ErrNum = Err
\PDef = Compiler\DefPublic
\ITable = Compiler\ImportTable
StorePosition(\Loc)
EndWith
Compiler\NameSpace = Name_Space
Compiler\DefPublic = DP
StartImport(Name_Space)
EndProcedure

Macro ContextIsProc(ContextData = Compiler\NSExportStack()) ; Pseudo-procedure.
ContextData\ErrNum = #eColonOut
EndMacro

Procedure StopExport()
Define *ET.NameSpaceData = Compiler\NSExportStack()
If ContextIsProc(*ET) 
Compiler\DefPublic = Compiler\ProcPDef
Else : Compiler\DefPublic = *ET\PDef
EndIf
Compiler\ImportTable = *ET\ITable
DeleteElement(Compiler\NSExportStack())
Compiler\NameSpace = Compiler\NSExportStack()\Name
EndProcedure

Macro AddProcedure(Name, ColonDef) ; Pseudo-procedure.
AddWord(Name, Compiler\Offset, #wProcedure, ColonDef)
If ColonDef : Compiler\ProcName = Name
Compiler\ProcLevel = ListSize(Compiler\Constructions()) + 1
Compiler\ProcParent = Compiler\NameSpace
Compiler\ProcPDef = Compiler\DefPublic
StartExport(#PoolPrefix + Name, #eColonOut)
If Compiler\VoidProcs = #False
ToBeFixed(#PoolPrefix + Str(Compiler\ProcNum))
LiteralI(0, #False)
Emit(#iAllot)
Emit(#iPopDS)
EndIf
EndIf
EndMacro

Macro StartColon() ; Pseudo-procedure.
If CompilingColon() = #False
Compiler\ProcCounters = ListSize(Compiler\Counters())
Compiler\ProcBase     = Compiler\ProcOffset
Compiler\ProcNum + 1
Swap Compiler\LoopsLvl  , Compiler\ProcLoopsLvl
Swap Compiler\OutputFile, Compiler\OverlayFile
Swap Compiler\Offset    , Compiler\ProcOffset
ParseProcName(#Isolated)
Else : ThrowError(#eNestColon)
EndIf
EndMacro

Macro AddReturn() ; Pseudo-procedure.
If CompilingColon()
ToBeFixed(#ReturnPrefix + Str(Compiler\ProcNum))
CompileJump(0, 'Jmp', 'Rel')
Else : ThrowError(#eReturnOut)
EndIf
EndMacro

Macro FlushVar(WordName, WordID)
VarBound(WordName, WordID)
Emit(#iReadI) : Emit(#iFreeMem)
EndMacro

Macro CollectGarbage()
ForEach Compiler\ProcGCList()
Define *Word.WordData = Compiler\ProcGCList()
Define WName.S = MapKey(Compiler\ProcGCList())
Select *Word\Type
Case #wArray : If *Word\ChainLink ; Если массив статический...
If IsDisposable(*Word\IValue) ; Если требуется сборка мусора...
ArrStartRetriever(WName, *Word\ChainLink) : ArrCellsRetriever(WName, *Word\ChainLink)
EmitSysCall("Minimal.RunTime.StaticArrayGC") ; Сборка мусора.
EndIf
Else : Emit(#iFalseI) : ArrResizer(WName, *Word\IValue, #Paranoic)
EndIf
Case #wVariable : If *Word\IValue = #vString : FlushVar(WName, *Word) : EndIf
EndSelect
Next
EndMacro

Macro FinishColon() ; Pseudo-procedure.
If CompilingColon()
If Compiler\ProcLevel <= ListSize(Compiler\Constructions())
CheckUnfinishedStructs()
CheckStaticBlock()
EndIf
AddLabel(#ReturnPrefix + Str(Compiler\ProcNum))
If Compiler\VoidProcs = #False
If Compiler\ColonGC : CollectGarbage() : EndIf
Emit(#iPushDS)
Emit(#iFreeMem)
EndIf
Emit(#iRet)
ClearMap(Compiler\ProcGCList())
Compiler\ProcName = FormatWord(Compiler\ProcName)
AddWord(#IsoEndPrefix  + Compiler\ProcName, Compiler\Offset)
AddWord(#IsoSizePrefix + Compiler\ProcName, Compiler\Offset - Compiler\ProcBase)
StopExport()
Swap Compiler\LoopsLvl  , Compiler\ProcLoopsLvl
Swap Compiler\OutputFile, Compiler\OverlayFile
Swap Compiler\Offset    , Compiler\ProcOffset
Compiler\ProcCounters = 0
Compiler\ProcLevel = 0
Compiler\ProcName = ""
Else : ThrowError(#eNoColon)
EndIf
EndMacro

Macro LinkPools(Capacitor) ; Pseudo-procedure.
Define I
For I = 1 To Compiler\ProcNum
AddWord(#PoolPrefix + Str(I), Compiler\LocalPools(I))
Capacitor + Compiler\LocalPools(I)
Next I
EndMacro

Macro ModeChangerOut(NewState, TrueErr, FlaseErr) ; Partializer.
CompilerIf NewState = #True : ThrowError(#eStaticCln1)
CompilerElse : ThrowError(#eStaticCln2)
CompilerEndIf
EndMacro

Macro SetModeCounter(IncFlag, CounterVar, LocVar, UnderflowErr) ; Partializer.
CompilerIf IncFlag = #True : CounterVar + 1
If CounterVar = 1 : StorePosition(LocVar) : EndIf
CompilerElse : If CounterVar = 0 : ThrowError(UnderflowErr) : EndIf
CounterVar - 1
CompilerEndIf
EndMacro

Macro DisablePools(NewState = #False) ; Pseudo-procedure.
If CompilingColon() : ModeChangerOut(NewState, #eColonVoid1, #eColonVoid2)
Else : SetModeCounter(NewState, Compiler\VoidProcs, Compiler\VoidLoc, #eVoidOut)
EndIf
EndMacro

Macro SetSDstate(NewState = #False) ; Pseudo-procedure.
If CompilingColon() 
SetModeCounter(NewState, Compiler\DefStatic, Compiler\StaticLoc, #eStaticOut)
Else : ModeChangerOut(NewState, #eStaticCln1, #eStaticCln2)
EndIf
EndMacro

Macro TmpNameSpace() ; Pseudo-procedure.
TmpLabelName("NameSpace:")
EndMacro

Macro AddContext(CName, Err = #eContextOut) ; Pseudo-procedure.
Define Context.s = CName
FindWord(*Context, Context)
If Context = #RootSpace : ThrowError(#eRootDef) : EndIf
StartExport(Context, Err, #True)
Else : StartExport(AddWord(CName, 0, #wContext), Err, #True)
EndIf
EndMacro

Macro FinishContext() ; Pseudo-procedure.
If ListSize(Compiler\NSExportStack()) > 1 : StopExport()
Else : ThrowError(#eNoContext)
EndIf
EndMacro

Macro StartBlock() ; Pseudo-procedure.
StartExport(FormatWord(TmpNameSpace()), #eBlockOut)
EndMacro

Macro FinishBlock() ; Pseudo-procedure.
If ListSize(Compiler\NSExportStack()) > 1 : StopExport()
Else : ThrowError(#eNoBlock)
EndIf
EndMacro

Macro SetDPState(NewState = #False) ; Pseudo-procedure.
If ContextIsProc() : Compiler\ProcPDef = NewState
ElseIf Compiler\NSExportStack()\ErrNum = #eContextOut 
Compiler\DefPublic = NewState
Else
CompilerIf NewState : ThrowError(#ePublicOut)
CompilerElse        : ThrowError(#ePrivateOut)
CompilerEndIf
EndIf
EndMacro

Macro SetFirstSpace(Name_Space) ; Pseudo-procedure.
Compiler\ImportTable = #ITDelimiter$ + Name_Space + #ITDelimiter$
EndMacro

Macro ImportOnly(Name_Space) ; Pseudo-procedure.
SetFirstSpace(Name_Space)
StartImport(Compiler\NameSpace)
EndMacro

Procedure.s FindNSParent(NS.s, GetChild.i = #False) ; Pseudo-procedure.
Define *Char.Character = @NS + StringByteLength(NS) - SizeOf(Character)
For *Char = *Char To @NS Step -SizeOf(Character)
If *Char\C = #NSDelimiter : Break : EndIf
Next 
If GetChild : ProcedureReturn PeekS(*Char + SizeOf(Character))
Else : ProcedureReturn PeekS(@NS, (*Char - @NS) >> (SizeOf(Character) >> 1))
EndIf
EndProcedure

Macro StopImportEx(NS) ; Pseudo-procedure.
If NS = Compiler\NameSpace 
#ForgetingErr = "Unable to forget about context on it's definition level"
If Compiler\NSExportStack()\ErrNum = #eContextOut Or Compiler\NSExportStack()\ErrNum = 0
ThrowError(#eForgetIt, #ForgetingErr + ": '" + FindNSParent(NS, #True) + "'")
Else : ThrowError(#eForgetIt, #ForgetingErr)
EndIf
EndIf
StopImport(NS)
EndMacro

Macro UpdateImport(Operation) ; Pseudo-procedure.
Define Arg.s = StringField(Compiler\ImportTable, 2, #ITDelimiter$)
Select Operation
Case 'frgt' : StopImportEx(Arg)
Case 'only' : ImportOnly(Arg)
Case 'prnt' : If Arg = #RootSpace
Warn("Cancelled attempt to retrieve parent of 'ROOT' context")
Else : StartImport(FindNSParent(Arg))
EndIf
EndSelect
EndMacro
;}
;{ --Interpertation mode--
Procedure UniPopper(*Accum.UniType)
With Compiler
If StackDepth(\Interstack) ; Если стек не пуст...
*Accum\TypeIdent = PopI(\Interstack)
Select *Accum\TypeIdent
Case 'd' : *Accum\DValue = PopD(\InterStack)
Case 's' : *Accum\SValue = PopS(\InterStack)
Default  : *Accum\QValue = PopQ(\InterStack)
EndSelect
Else : ThrowError(#eISUnderflow)
EndIf
EndWith
EndProcedure

Procedure UniPusher(TypeIdent, QValue.q, DValue.d = 0, SValue.s = "")
With Compiler
Select TypeIdent
Case 'f', 'd' : PushD(\InterStack, DValue) : TypeIdent = 'd'
Case 's'      : PushS(\InterStack, SValue) : TypeIdent = 's'
Default       : PushQ(\InterStack, QValue) : TypeIdent = 'q'
EndSelect     : PushI(\InterStack, TypeIdent)
EndWith
EndProcedure

Procedure Text2UniNum(*Accum.UniType)
With *Accum
\SValue = RefactoreString(\SValue)
If Literalize(\SValue)
EndWith
With Compiler\GenLiteral
*Accum\TypeIdent = \TypeID
*Accum\QValue    = \QValue
EndWith
Else : ThrowError(#eInvTypeCast, "Unable to numerize '" + *Accum\SValue + "'")
EndIf
EndProcedure

Procedure UniConvertor(*Accum.UniType, NewType)
With *Accum
Select \TypeIdent
Case 'f', 'd' ; Float types.
If NewType = 's' : \SValue = RTrim(RTrim(StrD(\DValue), "0"), ".")
ElseIf NewType <> 'f' And NewType <> 'd' : \QValue = \DValue
EndIf
Case 's' ; String type.
If Newtype <> 's' ; If it isn't s->s type cast.
If Text2UniNum(*Accum) : UniConvertor(*Accum, NewType) : EndIf
EndIf
Default ; Integer types.
If NewType = 's' : \SValue = Str(\QValue)
ElseIf NewType = 'f' Or NewType = 'd' : \DValue = \QValue
EndIf
EndSelect
\TypeIdent = NewType
EndWith
EndProcedure

Macro Unable2Interpret(WordName) ; Pseudo-procedure.
ThrowError(#eUnableInter, "Unable to interpret '" + WordName + "'")
EndMacro

Macro RecallUni(UniVal, NewType = '') ; Pseudo-procedure.
UniPopper(UniVal)
If NewType : UniConvertor(UniVal, NewType) : EndIf
EndMacro

Macro StoreUni(UniVal, NewType = '') ; Pseudo-procedure.
If NewType : UniConvertor(UniVal, NewType) : EndIf
UniPusher(UniVal\TypeIdent, UniVal\QValue, UniVal\DValue, UniVal\SValue)
EndMacro

Macro StoreLit(LitData) ; Pseudo-procedure.
UniPusher(LitData\TypeID, LitData\QValue, LitData\DValue, LitData\SValue)
EndMacro

Procedure RecallLit(*LitData.WordData)
With *LitData
Define Accum.UniType : RecallUni(Accum, \TypeID)
If \TypeID = 's' : \SValue = Accum\SValue
Else : \QValue = Accum\QValue
EndIf : \TypeID = Accum\TypeIdent
EndWith
EndProcedure

Procedure.s RecallStr()
Define Accum.UniType : RecallUni(Accum, 's')
ProcedureReturn Accum\SValue
EndProcedure

Procedure RecallInt()
Define Accum.UniType : RecallUni(Accum, 'q')
ProcedureReturn Accum\QValue
EndProcedure

Procedure StoreStr(Text.s)
Define Accum.UniType : Accum\TypeIdent = 's'
Accum\SValue = Text : StoreUni(Accum)
EndProcedure

Procedure InterpretDictionary(Word.S)
FindWord(*Word, Word)
With *Word
Select \Type
Case #wContext : StartImport(Word)
Case #wLiteral : StoreLit(*Word)
Case #wDefiner : UseDefiner(\Extra, \IValue, \ChainLink)
Case #wMacro   : TryInlining(Word, __OldName, *Word)
Case #wSysCall : ProcedureReturn #False ; Hack.
Default : Unable2Interpret(Word)
EndSelect
EndWith
ProcedureReturn #True
EndIf
EndProcedure

Macro Calculate(Val, Val2, Operation) ; Partializer.
Select Val\TypeIdent
Case 'd' : Val\DValue operation Val2\DValue
Default  : Val\QValue operation Val2\QValue
EndSelect
EndMacro

Macro Compare(Val, Val2, Operation) ; Partializer.
Select Val\TypeIdent
Case 'd' : If Val\DValue operation Val2\DValue : Val\QValue = #True : Else : Val\QValue = #False : EndIf
Case 's' ; Спасибо Фреду за наше счастливое детство.
CompilerIf Quotes#Operation#Quotes = "XOr"
If (Val\SValue <> "") operation (Val2\SValue <> "") : Val\QValue = #True : Else : Val\QValue = #False : EndIf
CompilerElse ; Такой-то XOr ! Такая-то забота.
If Val\SValue operation Val2\SValue : Val\QValue = #True : Else : Val\QValue = #False : EndIf
CompilerEndIf
Default  : If Val\QValue operation Val2\QValue : Val\QValue = #True : Else : Val\QValue = #False : EndIf
EndSelect
Val\TypeIdent = 'q'
EndMacro

Macro StringOP(StrAccum, Proc) ; Partializer.
StrAccum\SValue = Proc(StrAccum\SValue)
EndMacro

Macro FloatOP(FloatAccum, Proc) ; Partializer.
FloatAccum\DValue = Proc(FloatAccum\DValue)
EndMacro

Macro TypeCast(Val, Val2, ForceType = 0, TypeII = 0) ; Pseudo-procedure.
If ForceType ; Если необходимо силовое приведение к типу...
UniConvertor(Val, ForceType) 
If TypeII <> '-' ; Если предполагется второй аргумент...
If TypeII : UniConvertor(Val2, TypeII) : Else : UniConvertor(Val2, ForceType) : EndIf
EndIf
Else ; Приводим слагаемые к числам:
If Val\TypeIdent = 's'  : Text2UniNum(Val)  : EndIf
If Val2\TypeIdent = 's' : Text2UniNum(Val2) : EndIf
Select Val\TypeIdent
Case 'd' : If Val2\TypeIdent <> 'd' : UniConvertor(Val2, 'd') : EndIf
Default  : If Val2\TypeIdent = 'd'  : UniConvertor(Val, 'd')  : EndIf
EndSelect
EndIf
EndMacro

Macro ToBool(Val) ; Service proc.
Abs(Sign(Val))
EndMacro

Procedure Operate(Operation.q, ParseSecond = 0, TypeII = 0)
Define Val.UniType, Val2.UniType
If ParseSecond And ParseSecond < 10 : UniPopper(@Val)
Val2\QValue = ParseSecond : ParseSecond = 0
Else : If TypeII <> '-' : UniPopper(@Val2) : EndIf : UniPopper(@Val)
EndIf 
If ParseSecond <> '~' : TypeCast(Val, Val2, ParseSecond, TypeII) : EndIf
With Val
Select Operation
; -Prime math-
Case '+'  : Calculate(Val, Val2, +)
Case '-'  : Calculate(Val, Val2, -)
Case '*'  : Calculate(Val, Val2, *)
Case '/'  : Calculate(Val, Val2, /)
; -Complex arithmetics-
Case '**' : If \TypeIdent = 'd' : \DValue = Pow(\DValue, Val2\DValue) 
Else : \QValue = Pow(\QValue, Val2\QValue) : EndIf
Case 'Abs'  : If \TypeIdent = 'd' : \DValue = Abs(\DValue)  : Else : \QValue = Abs(\QValue)  : EndIf
Case 'Sign' : If \TypeIdent = 'd' : \DValue = Sign(\DValue) : Else : \QValue = Sign(\QValue) : EndIf
Case 'Lnat' : FloatOP(Val, Log)
Case 'Lg10' : FloatOP(Val, Log10)
Case 'Sqrt' : FloatOP(Val, Sqr)
Case 'DCut' : \DValue = Round(\DValue, Val2\QValue)
; -Trigonometry-
Case 'ACos' : FloatOP(Val, ACos)
Case 'ASin' : FloatOP(Val, ASin)
Case 'ATan' : FloatOP(Val, ATan)
Case 'Sin'  : FloatOP(Val, Sin)
Case 'Cos'  : FloatOP(Val, Cos)
Case 'Tan'  : FloatOP(Val, Tan)
; -String operations-
Case 's+'   : \SValue + Val2\SValue
Case 'Left' : \SValue = Left(\SValue, Val2\QValue)
Case 'Rite' : \SValue = Right(\SValue, Val2\QValue)
Case '    ' : \SValue = Space(\QValue) : \TypeIdent = 's'
Case 'Ltrm' : StringOP(Val, LTrim)
Case 'Rtrm' : StringOP(Val, RTrim)
Case 'Trim' : StringOP(Val, Trim)
Case 'Lcas' : StringOP(Val, LCase)
Case 'Ucas' : StringOP(Val, UCase)
Case 'Splt' :  Val2\SValue = Left(\SValue, Val2\QValue) : Val2\TypeIDent = 's'
StoreUni(Val2) : \SValue = Mid(\SValue, Val2\QValue + 1)
; -Comparsion-
Case '='  : Compare(Val, Val2, =)
Case '<'  : Compare(Val, Val2, <)
Case '>'  : Compare(Val, Val2, >)
Case '<=' : Compare(Val, Val2, <=)
Case '>=' : Compare(Val, Val2, >=)
Case '<>' : Compare(Val, Val2, <>)
; -Boolean logic-
Case 'Or'  : Compare(Val, Val2, Or)
Case 'And' : Compare(Val, Val2, And)
Case 'XOr' : Compare(Val, Val2, XOr)
Case 'Bool' : If \TypeIdent = 'd' : \DValue = ToBool(\DValue) : Else : \QValue = ToBool(\QValue) : EndIf
; -Stack operations-
Case 'Drop' : ProcedureReturn
Case 'Dup'  : StoreUni(Val)
Case 'Swap' : StoreUni(Val2)
Case 'Nip'  : StoreUni(Val2) : ProcedureReturn
; -Conversion-
Case 'c->s' : \SValue = Chr(\QValue) : \TypeIDent = 's'
Case 's->c' : \QValue = Asc(\SValue) : \TypeIDent = 'q'
; -Postfix variations-
Case 'ExpT' : ExportSysCall(\SValue)                         : ProcedureReturn
Case 'Dir!' : TryChangeInclusionDir(\SValue)                 : ProcedureReturn
Case 'Incl' : TryIncludeSource(\SValue, Compiler\XInclusion) : ProcedureReturn
Case '&Str' : MergeString(\SValue)                           : ProcedureReturn
Case '&Fil' : TryMergeFile(\SValue, Compiler\XInfusion)      : ProcedureReturn
Case '&Hex' : TryMergeSequence(\SValue)                      : ProcedureReturn
Case '&B64' : TryMergeSequence(\SValue, 1)                   : ProcedureReturn
Case 'PDel' : ParsePathDelim(\SValue)                        : ProcedureReturn
; -Extra-
Case 'IRnd' : \QValue = Random(\QValue)
Case '!Err' : ThrowError(#eCustomErr, \SValue)
Case '?Err' : If Val2\QValue : ThrowError(#eCustomErr, \SValue) : EndIf
Case 'Type' : ReportMessage(Val\SValue)                     : ProcedureReturn
Case '?exe' : If Val2\QValue : Paste2Code(\SValue) : EndIf  : ProcedureReturn
Case 'Eval' : Paste2Code(\SValue)                           : ProcedureReturn
EndSelect
EndWith
StoreUni(Val)
EndProcedure

Procedure Operate3(Operation.q, TypeI = 0, TypeII = 0, TypeIII = 0)
Define Val.UniType, Val2.UniType, Val3.UniType
RecallUni(@Val3, TypeIII) : RecallUni(@Val2, TypeII)
RecallUni(@Val, TypeI)
With Val
Select Operation
Case 'Mid'  : \SValue =  Mid(\SValue, Val2\QValue, Val3\QValue)
Case 'LSet' : \SValue = LSet(\SValue, Val2\QValue, Chr(Val3\QValue))
Case 'RSet' : \SValue = RSet(\SValue, Val2\QValue, Chr(Val3\QValue))
Case 'Rot'  : StoreUni(Val2) : StoreUni(Val3)
Case '-Rot' : StoreUni(Val3) : StoreUni(Val) : StoreUni(Val2) : ProcedureReturn
EndSelect
EndWith
StoreUni(Val)
EndProcedure

Macro AskCoder() ; Pseudo-procedure
ConsoleColor(15,0) : Print("?> ") : StoreStr(Input())
EndMacro

Macro OperateStr(OP, SecondType = '-') ; Partializer
Operate(OP, 's', SecondType)
EndMacro

Macro OperateFloat(OP, SecondType = '-') ; Partializer
Operate(OP, 'd', SecondType)
EndMacro

Macro OperateStr3(OP) ; Partializer
Operate3(OP, 's', 'q', 'q')
EndMacro

Macro Interpret(Word) ; Pseudo-procedure.
If InterpretDictionary(Word) = #False
Select Word
; -Prime math-
Case "+" : Operate('+')
Case "-" : Operate('-')
Case "*" : Operate('*')
Case "/" : Operate('/')
; -Compatibility math-
Case "1+", "++" : Operate('+', 1)
Case "1-", "--" : Operate('-', 1)
Case "2+"  : Operate('+', 2)
Case "2-"  : Operate('-', 2)
Case "neg" : Operate('*', -1)
; -Complex arithmetics-
Case "**"    : Operate('**')
Case "abs"   : Operate('Abs',  0, '-')
Case "sgn"   : Operate('Sign', 0, '-')
Case "log"   : OperateFloat('Lnat')
Case "log10" : OperateFloat('Lg10')
Case "sqr"   : OperateFloat('Sqrt')
Case "round" : OperateFloat('DCut', 'q')
; -Trigonometry-
Case "acos" : OperateFloat('ACos')
Case "asin" : OperateFloat('ASin')
Case "atan" : OperateFloat('ATan')
Case "sin"  : OperateFloat('Sin')
Case "cos"  : OperateFloat('Cos')
Case "tan"  : OperateFloat('Tan')
; -String operations-
Case "s+"     : OperateStr('s+', 0)
Case "s="     : OperateStr('=', 0)
Case "left"   : OperateStr('Left', 'q')
Case "right"  : OperateStr('Rite', 'q')
Case "mid"    : OperateStr3('Mid')
Case "lset"   : OperateStr3('LSet')
Case "rset"   : OperateStr3('RSet')
Case "spaces" : Operate('    ', 'q', '-')
Case "ltrim"  : OperateStr('Ltrm')
Case "rtrim"  : OperateStr('Rtrm')
Case "trim"   : OperateStr('Trim')
Case "lcase"  : OperateStr('Lcas')
Case "ucase"  : OperateStr('Ucas')
Case "ssplit" : OperateStr('Splt', 'q')
; -Comparsion-
Case "=" : Operate('=')
Case ">" : Operate('>')
Case "<" : Operate('<')
Case ">=", "=>" : Operate('>=')
Case "<=", "=<" : Operate('<=')
Case "<>", "><" : Operate('<>')
; -Boolean logic-
Case "or"  : Operate('Or')
Case "and" : Operate('And')
Case "xor" : Operate('XOr')
Case "not" : Operate('XOr', #True)
Case "->bool" : Operate('Bool', 0, '-')
; -Stack operations-
Case "dup"  : Operate('Dup' , '~', '-')
Case "drop" : Operate('Drop', '~', '-')
Case "swap" : Operate('Swap', '~')
Case "nip"  : Operate('Nip' , '~')
Case "rot"  : Operate3('Rot')
Case "-rot" : Operate3('-Rot')
Case "clear" : SeekStack(Compiler\InterStack)
; -Conversion-
Case "c->s" : Operate('c->s', 'q', '-')
Case "s->c" : OperateStr('s->c')
; -Postfix variations-
Case "export"       : OperateStr('ExpT')
Case "usedir"       : OperateStr('Dir!')
Case "include"      : OperateStr('Incl')
Case "merge"        : OperateStr('&Str')
Case "merge:file"   : OperateStr('&Fil')
Case "merge:hex"    : OperateStr('&Hex')
Case "merge:base64" : OperateStr('&B64')
Case "pathdelim"    : OperateStr('PDel')
; -Extra-
Case "request"   : AskCoder()
Case "report"    : OperateStr('Type')
Case "eval"      : OperateStr('Eval')
Case "throwerr"  : OperateStr('!Err')
Case "?throwerr" : OperateStr('?Err', 'q')
Case "?eval"     : OperateStr('?exe', 'q')
Case "random"    : Operate('IRnd', 'q', '-')
; -Partializers sections-
SharedWords()
Default : Unable2Interpret(Word)
EndSelect
EndIf
EndMacro

Macro InterpetationMode(NewState = #False) ; Pseudo-procedure.
SetModeCounter(NewState, Compiler\Interpreting, Compiler\InterLoc, #eInterOut)
EndMacro
;}
;}

;{ [Main macros]
Macro InitCompiler() ; Pseudo-procedure.
OpenConsole()
TypeOut("StasisForth assembler v0.095 (developed in 2009 by Guevara-chan)", 10)
Compiler\InterStack    = AllocateStack()
Compiler\Header\Signature = #HeaderSig
Compiler\Header\EntryPoint = #Uninitialized
Compiler\DefDataType = 'i'
Compiler\NumOpt       = #True
Compiler\StrOpt       = #True
Compiler\TimesTailOpt = #True
Compiler\ShowWarnings = #True
Compiler\XInclusion   = #True
Compiler\Informing    = #True
Compiler\ArraysGC     = #True
Compiler\ColonGC      = #True
Compiler\AddBye       = #True
Compiler\BigEndianStr = #True
Compiler\PathDelimiter = "\\"
Compiler\GenDataReq = GetWord(AddWord(#DataReq))
Compiler\GenLiteral = GetWord(AddLiteral(#GenLiteral))
Compiler\GenDefiner = GetWord(AddWord(#GenDefiner, 0, #wDefiner))
Compiler\GenCaller  = GetWord(AddSysCaller(#GenCaller, ""))
Compiler\VoidWord   = GetWord(AddMacro(#VoidWord))
Compiler\GenLiteral\Public = #True
Compiler\GenDataReq\Public = #True
Compiler\GenDefiner\Public = #True
Compiler\GenCaller\Public  = #True
Compiler\VoidWord\Public   = #True
Compiler\ThisMacro = @Compiler\VoidMacro
CreateRegularExpression(#rExtension, "(?<!\A)\.[\w]+\Z")
CreateRegularExpression(#rIsNumber,  "(^[-+]?\d+(,?\d*)*\.?\d*([Ee][-+]\d*)?$)|(^[-+]?\d?(,?\d*)*\.\d+([Ee][-+]\d*)?$)")
Define FixDir.s = GetPathPart(ProgramFilename()) ; На случай cmd и тому подобного.
If FixDir <> GetTemporaryDirectory() : SetCurrentDirectory(FixDir) : EndIf
SetupCompiler() : FindInput() : PrepareOutput()
SetCurrentDirectory(GetPathPart(Compiler\Source\FullName))
SetFirstSpace(#RootSpace)
AddContext(#RootSpace, 0)
Compiler\RootContext = GetWord(#RootSpace)
EndMacro

Macro AfterMath() ; Pseudo-procedure.
If Compiler\AddBye : Emit(#iBye) : EndIf
TypeOut("-----------------------------") : CheckFixez()
TypeOut(FormatCount(Compiler\WordsTotal,"word")+" in "+FormatCount(Compiler\FilesTotal, "file")+" got parsed.",10)
; Overlay linking.
Define OverlayBase = Compiler\Offset
TypeOut("Relocating isolated code layer...")
CloseFile(Compiler\OverlayFile)
Compiler\Offset + AppendFile(Compiler\OutputFile, Compiler\OverlayName)
Define CodeSize = Compiler\Offset
; Data space reconstruction.
TypeOut("Reconstructing data space...")
ReDim Compiler\LocalPools(Compiler\ProcNum)
FinalizeDict() ; Финальная обработка словаря.
CloseFile(Compiler\ResourcesFile)
Compiler\ResSection = Compiler\Offset
Compiler\Offset + AppendFile(Compiler\OutputFile, Compiler\ResFileName)
AddWord(ResInfoLabel(#ResSizePrefix), 0)
AddWord(ResInfoLabel(#ResEndPrefix), 0)
Define PoolsTotal.i
LinkPools(PoolsTotal)
; Pseudo-constants definition.
If Compiler\Header\EntryPoint = #Uninitialized 
Compiler\Header\EntryPoint = 0 : EndIf
Compiler\Header\CodeSize = Compiler\Offset
AddPConst("#ENTRYPOINT", Compiler\Header\EntryPoint)
AddPConst("#CODESIZE"  , CodeSize)
AddPConst("#RDATASIZE" , Compiler\Offset - CodeSize)
AddPConst("#VDATASIZE" , Compiler\Header\DataSize)
; Pointers fixing.
Define MainSegment.i = Compiler\Offset
TypeOut("Linking pointers...")
AddPConst("#FILESIZE"  , Compiler\Offset + SizeOf(HeaderData)) ; Should be there
FixThings(OverlayBase)
Inform("Entry offset         = " + FormatSize(Compiler\Header\EntryPoint), #False)
Inform("Code section         = " + FormatSize(CodeSize)                  , #False)
Inform("Real data section    = " + FormatSize(MainSegment - CodeSize)    , #False)
Inform("Local pools summary  = " + FormatSize(PoolsTotal)                , #False)
Inform("Virtual data section = " + FormatSize(Compiler\Header\DataSize)  , #False)
Inform("Import table section = " + FormatSize(Compiler\ImportSize) + EntryFormat(Compiler\Header\ImportTable), #False)
Inform("Export table section = " + FormatSize(Compiler\ExportSize) + EntryFormat(Compiler\Header\ExportTable), #False)
If Compiler\Warnings : WarningsInfo() : EndIf ; Информация о предупреждениях.
TypeOut("'" + GetFilePart(Compiler\OutputName) + "' successfuly created, work complete !", 10)
FinishWork()
EndMacro
;} {End/Macros}

;{ ==Main code==
InitCompiler()
Repeat ; Ассемблирование всех исходных текстов...
While ParseNextWord()
ReParse: :DrawPropeller()
Select Compiler\State
Case #sLabelName      : AddLabel(Compiler\ThisWord)                              : ResetState()
Case #sSysCallerName  : InitNewCaller(Compiler\ThisWord)                         : ResetState()
Case #sResLabelName   : InitNewResLabel(Compiler\ThisWord)                       : ResetState()
Case #sVariableName   : InitNewVariable(Compiler\ThisWord)                       : ResetState()
Case #sArrayName      : InitNewArray(Compiler\ThisWord)                          : ResetState()
Case #sConstantName   : InitNewLiteral(Compiler\ThisWord)                        : ResetState()
Case #sMacroName      : InitNewMacro(Compiler\ThisWord)                          : ResetState()
Case #sStrRequest     : If ProcessString(Compiler\ThisWord, Compiler\SubState)   : ResetState() : EndIf
Case #sPtrRequest     : If ExtractPointer(Compiler\ThisWord)                     : ResetState() : EndIf
Case #sSizeRequest    : If ExtractSize(Compiler\ThisWord)                        : ResetState() : EndIf
Case #sBodyRequest    : If ExtractBody(Compiler\ThisWord)                        : ResetState() : EndIf
Case #sWordRequest    : CheckDefinition(Compiler\ThisWord)                       : ResetState()
Case #sResLabelInfo   : If GetResourceInfo(Compiler\ThisWord, Compiler\SubState) : ResetState() : EndIf
Case #sIsoProcInfo    : GetProcInfo(Compiler\ThisWord, Compiler\SubState)        : ResetState()
Case #sProcedureName  : AddProcedure(Compiler\ThisWord, Compiler\SubState)       : ResetState()
Case #sContextName    : AddContext(Compiler\ThisWord)                            : ResetState()
Case #sArrayRequest   : If ProcessArray(Compiler\ThisWord, Compiler\SubState)    : ResetState() : EndIf
Case #sVariableWrite  : TryValWriting(Compiler\ThisWord, Compiler\SubState)
Case #sCommented1     : TryCloseComment("("   , ")")
Case #sCommented2     : TryCloseComment("/*"  , "*/")
Case #sCollectionReq  : AddForEach(Compiler\ThisWord)  : ResetState()
Case #sSwitcherName   : AddSwitch(Compiler\ThisWord)   : ResetState()
Case #sValueGroup     : ParseGroup(Compiler\ThisWord) 
Default ; Normal modes.
If Compiler\Interpreting = #False ; Compilation mode.
If TryPriming(Compiler\ThisWord) = #False
;{ -<Встроенные слова компиляции>-
Select Compiler\ThisWord
; -Infinite loops-
Case "begin" : AddBegin()
Case "again" : CompileBegin()
Case "until" : CompileBegin(#True)
Case "end"   : CompileBegin('~end')
; -Finite loops-
Case "times[" : AddTimes(Compiler\TimesHeadOpt)
Case "]times" : CompileTimes(Compiler\TimesTailOpt)
Case "for["   : AddFor()
Case "]rof"   : CompileFor()
; -Processing loops-
Case "[foreach:" : ChangeState(#sCollectionReq)
Case "next]"     : CompileForEach()
; -Conditions-
Case "if"     : AddIf()
Case "ifz"    : AddIf(#False, #True)
Case "[if"    : AddIf(#True)
Case "[ifz"   : AddIf(#True, #True)
Case "else"   : InsertElse()
Case "then"   : CompileIf()
Case "endif]" : CompileIf(#True)
; -Selectors-
Case "{switch:"  : ChangeState(#sSwitcherName)
Case ";on{"      : InsertON()
Case "}do:"      : InsertDO()
Case ";default:" : InsertDefault()
Case ";switch"   : CompileSwitch()
; -Universal breakers-
Case "break"  : AddBreak()
Case "?break" : AddBreak(#True)
Case "pass"   : AddPass()
Case "?pass"  : AddPass(#True)
; Code skipping-
Case "skip["  : AddSkip()
Case "]skip"  : CompileSkip()
; -Boolean logic-
Case "or"  : BoolOp(#iOr)
Case "and" : BoolOp(#iAnd)
Case "xor" : BoolOp(#iXOr)
Case "not" : BoolOp(#iNot, #True)
; -Aligning opertions-
Case "words"  : DataSizeCalc(Word)
Case "+word"  : DataSizeCalc(Word, #iAddI)
Case "~words" : DataSizeCalc(Word, #iDivI)
Case "chars"  : Emit(#iCharSize) : Emit(#iMulI)
Case "+char"  : Emit(#iCharSize) : Emit(#iAddI)
Case "~chars" : Emit(#iCharSize) : Emit(#iDivI)
Case "longs"  : DataSizeCalc(Long)
Case "+long"  : DataSizeCalc(Long, #iAddI)
Case "~longs" : DataSizeCalc(Long, #iDivI)
Case "quads"  : DataSizeCalc(Quad)
Case "+quad"  : DataSizeCalc(Quad, #iAddI)
Case "~quads" : DataSizeCalc(Quad, #iDivI)
Case "cells"  : Emit(#iPtrSize)  : Emit(#iMulI)
Case "+cell"  : Emit(#iPtrSize)  : Emit(#iAddI)
Case "~cells" : Emit(#iPtrSize)  : Emit(#iDivI)
; -Debugging macros-
Case "cr"  : EmitCharPrinter(10)
Case "bl"  : EmitCharPrinter(32)
Case "tab" : EmitCharPrinter(09)
Case "i."  : EmitValueWriter("i")
Case "b."  : EmitValueWriter("b")
Case "w."  : EmitValueWriter("w")
Case "l."  : EmitValueWriter("l")
Case "q."  : EmitValueWriter("q")
Case "f."  : EmitValueWriter("f")
Case "d."  : EmitValueWriter("d")
Case "c.", "emit" : EmitValueWriter("c")
Case "."   : EmitValueWriter(Chr(TypePriming(Compiler\DefDataType)))
; -String variables management-
Case "@strstart:" : PrepareStrRequest('@1st')
Case "@strend:"   : PrepareStrRequest('@end')
Case "@strsize:"  : PrepareStrRequest('size')
Case "@len:"      : PrepareStrRequest('@len')
Case "->len:"     : PrepareStrRequest('!len')
Case "void->"     : PrepareStrRequest('void')
Case "->char:"    : PrepareStrRequest('!chr')
Case "@char:"     : PrepareStrRequest('@chr')
Case "?char:"     : PrepareStrRequest('?chr')
Case "+char:"     : PrepareStrRequest('+chr')
; -Variable & arrays operations-
Case "->" : PrepareVarWriting()
Case "+=" : PrepareVarWriting('+')
Case "-=" : PrepareVarWriting('-')
Case "*=" : PrepareVarWriting('*')
Case "/=" : PrepareVarWriting('/')
Case "1+=" : PrepareVarWriting('+1')
Case "1-=" : PrepareVarWriting('-1')
Case "2+=" : PrepareVarWriting('+2')
Case "2-=" : PrepareVarWriting('-2')
Case "@sizeof:" : ChangeState(#sSizeRequest)
; -Array operations-
Case "->cells:"   : PrepareArrRequest('!bnd')
Case "->newarr:"  : PrepareArrRequest('!arr')
Case "@cells:"    : PrepareArrRequest('@bnd')
Case "+cells:"    : PrepareArrRequest('+bnd')
Case "?cell:"     : PrepareArrRequest('?ele')
Case "@arrstart:" : PrepareArrRequest('@1st')
Case "@arrend:"   : PrepareArrRequest('@end')
Case "@arrsize:"  : PrepareArrRequest('size')
Case "cleararr:"  : PrepareArrRequest('0000')
; -Coommon storage operations-
Case "->>"       : PrepareArrRequest('->>*')
; -Sequences management-
Case "@seq:"     : PrepareArrRequest('@seq')
Case "->seq:"    : PrepareArrRequest('!seq')
Case "->>seq:"   : PrepareArrRequest('{>>}')
Case "seq->arr:" : PrepareArrRequest('s->a')
; -Sequence management-
Case "{"  : AddSequencer()
Case "}"  : CompileSequencer()
Case "{}" : Emit(#iDepth)
Case "~"  : Emit(#iPopRS) : Emit(#iDepth) : Emit(#iPushRS) : Emit(#iSubI)
Case "{if}"  : EmitSeqIF()
Case "{ifz}" : EmitSeqIF(#True)
; -Procedure definitions-
Case ":" : StartColon()
Case ";" : FinishColon()
Case "proc:"      : ParseProcName()
Case "return"     : AddReturn()
Case "@procsize:" : PrepareProcAnalysis('size')
Case "@procend:"  : PrepareProcAnalysis('@end')
Case "void["      : DisablePools(#True)
Case "]void"      : DisablePools()
Case "static["    : SetSDState(#True)
Case "]static"    : SetSDState()
; -Resources management-
Case "@resend:"  : PrepareResAnalysis('@end')
; -------------------------------
Macro SharedWords() ; Partializer
; -Namespaces management-
Case "[context:"  : ChangeState(#sContextName)
Case "context]"   : FinishContext()
Case "[["         : StartBlock()
Case "]]"         : FinishBlock()
Case "]public["   : SetDPState(#True)
Case "]private["  : SetDPState()
Case "forget"     : UpdateImport('frgt')
Case "only"       : UpdateImport('only')
Case "parent"     : UpdateImport('prnt')
;Case "@findword:" : ChangeState(#sWordRequest)
;Case "@defined:"  : ChangeState(#sWordRequest, 'Here')
; -Compilation directives-
Case "<merge:me>"        : MergeFile(Compiler\Source\Loc\SrcName, Compiler\XInfusion)
Case "<entrypoint>"      : SetEntryPoint(Compiler\Offset)
Case "<xinclusion:on>"   : SetXInclusionState(#True)
Case "<xinclusion:off>"  : SetXInclusionState()
Case "<xinfusion:on>"    : SetXInfusionState(#True)
Case "<xinfusion:off>"   : SetXInfusionState()
Case "<warnings:on>"     : SetWarningsState(#True)
Case "<warnings:off>"    : SetWarningsState()
Case "<informing:on>"    : Compiler\Informing = #True
Case "<informing:off>"   : Compiler\Informing = #False
Case "<stropt:on>"       : SetSOState(#True)
Case "<stropt:off>"      : SetSOState()
Case "<numopt:on>"       : SetNOState(#True)
Case "<numopt:off>"      : SetNOState()
Case "<times[opt:on>"    : SetTHOState(#True)
Case "<times[opt:off>"   : SetTHOState()
Case "<]timesopt:on>"    : SetTTOState(#True)
Case "<]timesopt:off>"   : SetTTOState()
Case "<arraysgc:on>"     : SetAGCState(#True)
Case "<arraysgc:off>"    : SetAGCState()
Case "<colongc:on>"      : SetCGCState(#True)
Case "<colongc:off>"     : SetCGCState()
Case "<fallthrough:on>"  : SetFTState(#True)
Case "<fallthrough:off>" : SetFTState()
;Case "<directmerge:on>"  : SetDMState(#True)
;Case "<directmerge:off>" : SetDMState()
Case "<addbye:on>"       : SetABstate(#True)
Case "<addbye:off>"      : SetABstate()
Case "<bigendian:on>"    : SetBEstate(#True)
Case "<bigendian:off>"   : SetBEstate()
Case "<forceimport:on>"  : SetFIstate(#True)
Case "<forceimport:off>" : SetFIstate()
Case "<eof>"             : AbortParsing()
; -Interpretation mode-
Case "<[" : InterpetationMode(#True)
Case "]>" : InterpetationMode()
; -Macro management-
Case "@body:"    : ChangeState(#sBodyRequest)
; -Illegal words-
Case ")"    : ThrowError(#eRem1Out)
Case "*/"   : ThrowError(#eRem2Out)
Case "]def" : ThrowError(#eDefOut)
; -Partializers sections-
CommentsBlock()
EndMacro : SharedWords()
DefTypeModsBlock()
; -------------------------------
PsuedoConstantsBlock()
; -Misc constructions-
Case "clear"     : LiteralL($010100) : Emit(#iSearchB)
Case "?"         : ChangeState(#sPtrRequest)
Case "label:"    : ChangeState(#sLabelName)
Default : ToBeFixed(Compiler\ThisWord, -1, #eUndefined, #PostPonedCall) : CompileJump(0)
EndSelect
;} {End/словарная таблица}
EndIf
Else : Interpret(Compiler\ThisWord) ; Интерпретируем слово
EndIf
EndSelect
If Compiler\Reparse : Compiler\Reparse = #False : Goto Reparse : EndIf
Wend : If ListSize(Compiler\Sources()) = 1 Or #True : CheckUnfinished() : EndIf
Until RollSourceBack() = #Null
AfterMath()
;} {End/Main}
; IDE Options = PureBasic 5.30 (Windows - x86)
; ExecutableFormat = Console
; Folding = iCE64f+-----8---f-u-P+
; EnableUnicode
; EnableXP
; Executable = ..\StasisForth.exe
; CurrentDirectory = ..\