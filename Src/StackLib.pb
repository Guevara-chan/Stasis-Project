; *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*
; Untyped stack library v0.71
; Developed in 2009 by Guevara-chan.
; *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*

Structure StackData
*Base
*TOS
Size.i
Growth.i
EndStructure

; AllocateStack(Size.i, Growth) - creates stack of specifed size with specifed growth rate.
; Set 'Growth' to -1 for unlimited stack expansion. 
; Returns point to newborn stack's data if successful.
ProcedureDLL AllocateStack(Size.i = 1, Growth.i = -1)
If Growth < -1 : ProcedureReturn #Null : EndIf
If Size < 0 : ProcedureReturn #Null : EndIf
Define *NewStack.StackData = AllocateMemory(SizeOf(StackData))
With *Newstack
\Base = AllocateMemory(Size)
\TOS = \Base
\Size = Size 
\Growth = Growth
EndWith
ProcedureReturn *NewStack
EndProcedure

; FreeStack(*Stack.StackData) - completely disposes all stack's resources.
ProcedureDLL FreeStack(*Stack.StackData)
FreeMemory(*Stack\Base)
FreeMemory(*Stack)
EndProcedure

Macro __StackDepth(Stack) ; Psedo-procedure
Stack\TOS - Stack\Base
EndMacro

; StackDepth(*Stack.StackData) - returns current depth of stack (in bytes).
ProcedureDLL StackDepth(*Stack.StackData)
ProcedureReturn __StackDepth(*Stack)
EndProcedure

Macro __ResizeStack(Stack, NewSize, NewDepth) ; Psedo-procedure
Stack\Size = NewSize
Stack\Base = ReAllocateMemory(Stack\Base, Stack\Size)
Stack\TOS = Stack\Base + NewDepth
EndMacro

; ResizeStack(*Stack.StackData, NewSize.i) - reallocates stack to specifed size (in bytes).
; Content is fully preserved, returns new size of stack if successful.
ProcedureDLL ResizeStack(*Stack.StackData, NewSize.i)
If NewSize > 0
Define SDepth = __StackDepth(*Stack)
If SDepth > NewSize : SDepth = NewSize : EndIf
__ResizeStack(*Stack, NewSize, SDepth)
ProcedureReturn NewSize
EndIf
EndProcedure

; SeekStack(*Stack.StackData, NewPos.i = 0) - moves stack's top to specified position (in bytes).
; Returns #True if successful.
ProcedureDLL SeekStack(*Stack.StackData, NewPos.i = 0)
If NewPos => 0
If NewPos <= __StackDepth(*Stack)
*Stack\TOS = *Stack\Base + NewPos
ProcedureReturn #True
EndIf
EndIf
EndProcedure

Macro __CheckSize(Stack, RequestedSize) ; Partializer
Define SGrowth, SDepth = __StackDepth(Stack), OverHead = SDepth + RequestedSize - Stack\Size
If OverHead > 0 ; If you need more space...
If Stack\Growth = -1 : SGrowth = OverHead : Else : SGrowth = (OverHead / Stack\Growth + 1) * Stack\Growth : EndIf
If OverHead <= SGrowth ; If it's possible to just resize stack...
__ResizeStack(Stack, Stack\Size + SGrowth, SDepth)
Else : ProcedureReturn #False
EndIf
EndIf
EndMacro

Macro __Def_StackProcs(TypeName, TypeLetter)
; PushX(*Stack.StackData, Value.X) - sends value of type X over top of the stack.
; Returns #True if successful.
ProcedureDLL Push#TypeLetter(*Stack.StackData, TypeName.TypeLetter)
__CheckSize(*Stack, SizeOf(TypeName))
Define *NewTOS.TypeName = *Stack\TOS
*NewTOS\TypeLetter = TypeName
*Stack\TOS + SizeOf(TypeName)
ProcedureReturn #True
EndProcedure
; PopX(*Stack.StackData) - receives value of type X from top of the stack.
ProcedureDLL.TypeLetter Pop#TypeLetter(*Stack.StackData)
If __StackDepth(*Stack) >= SizeOf(TypeName)
Define *Value.TypeName = *Stack\TOS - SizeOf(TypeName)
*Stack\TOS = *Value
ProcedureReturn *Value\TypeLetter
EndIf
EndProcedure
EndMacro

; PushData(*Stack.StackData, *Buffer, Length.i) - sends given number of bytes from buffer to stack.
; Returns #True if successful.
ProcedureDLL PushData(*Stack.StackData, *Buffer, Length.i)
If Length > 0 And *Buffer
__CheckSize(*Stack, Length)
With *Stack
CopyMemory(*Buffer, \TOS, Length)
\TOS + Length
EndWith
ProcedureReturn #True
EndIf
EndProcedure

; PopData(*Stack.StackData, *Buffer, Length.i) - receives given number of bytes from top of stack.
; Set '*Buffer' to #Null for restriction of memory copying into it.
; Returns address of result-containing buffer.
ProcedureDLL PopData(*Stack.StackData, *Buffer, Length.i)
If Length > 0
With *Stack
If __StackDepth(*Stack) >= Length : \TOS - Length
If *Buffer : CopyMemory(\TOS, *Buffer, Length)
Else : *Buffer = \TOS
EndIf
ProcedureReturn *Buffer
EndIf
EndWith
EndIf
EndProcedure

; PushS(*Stack.StackData, Text.s) - sends string, followed by integer size counter to top of stack.
; String size + SizeOf(Integer) bytes used, returns #True if successful.
ProcedureDLL PushS(*Stack.StackData, Text.s)
Define *SizeMark.Integer, TSize.i = StringByteLength(Text)
__CheckSize(*Stack, TSize + SizeOf(Integer))
With *Stack
PokeS(\TOS, Text)
*SizeMark = \TOS + TSize
*SizeMark\I = TSize
\TOS = *SizeMark + SizeOf(Integer)
ProcedureReturn #True
EndWith
EndProcedure

; PopS(*Stack.StackData) - receives string from top of stack.
ProcedureDLL.S PopS(*Stack.StackData)
Define SDepth = __StackDepth(*Stack)
If SDepth >= SizeOf(Integer)
With *Stack
Define *TSize.Integer = \TOS - SizeOf(Integer)
If SDepth - SizeOf(Integer) >= *TSize\I : \TOS = *TSize - *TSize\I
CompilerIf SizeOf(Character) = SizeOf(Unicode) : ProcedureReturn PeekS(\TOS, *TSize\I >> 1)
CompilerElse                                   : ProcedureReturn PeekS(\TOS, *TSize\I)
CompilerEndIf
EndIf
EndWith
EndIf
EndProcedure

__Def_StackProcs(Byte, B)
__Def_StackProcs(Word, W)
__Def_StackProcs(Long, L)
__Def_StackProcs(Float, F)
__Def_StackProcs(Quad, Q)
__Def_StackProcs(Double, D)
__Def_StackProcs(Character, C)
__Def_StackProcs(Integer, I)
__Def_StackProcs(Ascii, A)
__Def_StackProcs(Unicode, U)
; IDE Options = PureBasic 5.30 Beta 9 (Windows - x86)
; Folding = --
; EnableUnicode
; EnableXP