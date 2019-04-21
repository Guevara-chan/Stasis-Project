# /Sum/:
__Stasis__ is a highly experimental virtual machine project, inspired by _CIL_ and _Forth_ languages.  
It was designed embeddable and easily extensible, though not really meant for scripting atm.  
❗ There are no documentation provided yet, so some reverse-engineering skills is crucial ❗ 

# /Featuræ/:
* 2bytecode-based extensible instruction set with most x86 operations (including FPU) covered.
* Lightweight (~100Kb) interpeter with basic string and OS microlibraries included.
* Forth-esque assembly language compiler with some ideas burrowed from [Rebol](http://www.rebol.com/).
* Common structured programming toolset (loops, if/else/then, switch) provided.
* Minimalistic snapshot binary format with import/export tables support.
* Extensive binary data embedding (including external files) directives.
* Direct memory management and IO access.
* Basic compile-time calculations support.
* Forward subroutine referencing support.
* Classical case-insensetive syntax.
* Extensive namespace support.

# /Reassembling/:
__Stasis__ was deveoped as OS-independent  [PureBasic v4.40](http://purebasic.com) project, though compatible with *5.7x (LTS)*.  
Compile `StasisVM.pb` or `StasisForth.pb` to assemble snapshot intepreter and compiler respectively.

# /Brief samples of interfacing/:
![image](https://user-images.githubusercontent.com/8768470/46802627-88c88100-cd66-11e8-9a8d-96669f399293.png)
![image](https://user-images.githubusercontent.com/8768470/46802748-eb218180-cd66-11e8-91a7-8c290100a891.png)
