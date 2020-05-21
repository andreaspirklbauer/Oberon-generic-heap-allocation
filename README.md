# Oberon-generic-heap-allocation
Generic heap allocation procedure *Memory.New(p, size)* for the Project Oberon 2013 operating system, which allocates a new dynamic variable of the specified *size* in the dynamic space (heap).

Note: In this repository, the term "Project Oberon 2013" refers to a re-implementation of the original "Project Oberon" on an FPGA development board around 2013, as published at www.projectoberon.com.

The plain vanilla Project Oberon 2013 operating system does *not* support allocating dynamic arrays.

There are several possible ways to allocate arrays in the heap anyway, including:

1. Place the array inside a record and allocate the record using NEW (see the appendix for variations of this method).

2. Continue to use the Project Oberon 2013 system, but use the generic heap allocation procedure *Memory.New(p, size)* of *this* repository (which however is *not* type-safe, as explained below).

3. Upgrade the Project Oberon 2013 system to use the *enhanced Oberon runtime system and Oberon-07 compiler* available at http://github.com/andreaspirklbauer/Oberon-enhanced-Oberon07-compiler, which implements a dynamic heap allocation procedure for fixed-length and open arrays.

4. Upgrade the Project Oberon 2013 system to *Extended Oberon* available at http://github.com/andreaspirklbauer/Oberon-extended, which also implements a dynamic heap allocation procedure for fixed-length and open arrays.

------------------------------------------------------

**PREREQUISITES**: A current version of Project Oberon 2013 (see http://www.projectoberon.com).

------------------------------------------------------
**Description of procedure Memory.New(p, size)**

If *p* is a variable of type *Memory.Pointer*, then a call of the procedure

     Memory.New(p, size)

allocates a dynamic variable of the specified *size* in bytes in the dynamic space (heap), and a pointer to it is assigned to *p*.

In addition, a "hidden" type descriptor containing the specified *size* is dynamically created on the fly. Multiple objects of the same size share the same descriptor. Note that no *pointer offsets* (for the garbage collector) or *procedure variable offsets* (for safe module unloading in Extended Oberon) are configured in the descriptor, i.e the object referenced by *p* is treated as an ARRAY OF BYTE with no descendant pointers or procedure variable fields (see "Restrictions" below).

Heap objects allocated by *Memory.New* are collected by the Oberon garbage collector in the normal way, i.e. if the object is reachable from a named global pointer variable (such as the variable *ptr* in the test program below), it is not collected.

The hidden type descriptors created by *Memory.New* are however not collected by the regular Oberon garbage collector (implementing this would require rebuilding the runtime system). But a command to collect them is provided:

     Memory.Collect

This command can be manually activated at any time or be called from within client modules (recommended). It could also be added or removed as an Oberon background task, using the commands

     Memory.Start
     Memory.Stop

**Warning**

The solution presented in this repository is **not** type-safe in the following sense: It is the responsibility of the programmer to ensure that any reference to the data referenced by *p* actually lies *within* the allocated memory area, and the programmer must be aware that this represents a potential security risk! For example, in the test program below, the compiler only performs array bound checks on *maxsize* (the size of the array as statically declared in the program's source text), but not on *size* (the size of the array as dynamically allocated using *Memory.New*).

------------------------------------------------------
**Test program**

     MODULE MemoryTest;  (*Test of Memory.New(ptr, size)*)
       IMPORT SYSTEM, Texts, Oberon, Memory;
       CONST size = 56;  (*ideally 8 less than 32, 64, 128 or n*256 bytes*)
         maxsize = 65536;

       TYPE Pointer = POINTER TO Record;
         Record = RECORD x: ARRAY maxsize OF BYTE END ;

       VAR ptr: Pointer;  (*named global pointer variable*)
         W: Texts.Writer;

       PROCEDURE New(VAR ptr: Pointer; size: LONGINT);
         VAR p: Memory.Pointer;
       BEGIN Memory.New(p, size); ptr := SYSTEM.VAL(Pointer, p)
       END New;

       PROCEDURE Free(VAR ptr: Pointer);
       BEGIN ptr := NIL
       END Free;

       PROCEDURE NewPtr*;
       BEGIN New(ptr, size) (*assigning the result to ptr prevents the object from being collected by the Oberon garbage collector*)
       END NewPtr;

       PROCEDURE FreePtr*;
       BEGIN Free(ptr)
       END FreePtr;

       PROCEDURE InitPtr*;
       BEGIN ptr.x[2] := 22;          (*compiles and valid*)
         (*ptr.x[100] := 33;*)        (*would compile, but write to a random memory location*)
         (*ptr.x[100000] := 44*)      (*would lead to a *bad index" compile-time error*)
       END InitPtr;

       PROCEDURE ShowPtr*;
       BEGIN Texts.WriteInt(W, ptr.x[2], 4); Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)
       END ShowPtr;

       PROCEDURE Collect*;
       BEGIN Memory.Collect
       END Collect;

     BEGIN Texts.OpenWriter(W); ptr := NIL
     END MemoryTest.

     ORP.Compile Memory.Mod MemoryTest.Mod ~

     MemoryTest.NewPtr
     MemoryTest.InitPtr
     MemoryTest.ShowPtr
     MemoryTest.FreePtr
     MemoryTest.Collect    # call Memory.Collect in client module

     Memory.Collect        # call Memory.Collect manually
     Memory.Start          # start Memory.Collect as a background task
     Memory.Stop           # stop Memory.Collect as a background task

------------------------------------------------------
**Restrictions**

**1.** If a variable *p* of type *Memory.Pointer* is converted ("type cast") to a variable of type *MyPointer = POINTER TO MyRecord*, then the type *MyRecord* must *not* contain descendant pointers or procedure variable fields, as in:

     CONST maxsize = 65536;                  (*any large number*)
     TYPE MyPointer = POINTER TO MyRecord;
       SubRecord = RECORD
         ptr: MyPointer;                     (*descendant pointer field -> NOT ALLOWED*)
         pvr: PROCEDURE (i: INTEGER)         (*procedure variable field -> NOT ALLOWED*)
       END ;
       MyRecord = RECORD x: ARRAY maxsize OF SubRecord END ;

This avoids the need to traverse the heap structure of objects allocated by *Memory.New* during **garbage collection** (where descendant *pointers* are traversed) or **safe module unloading** in *Extended Oberon* (where pointer *and* procedure variable fields are traversed).

**2.** (Extended Oberon only) If a variable *p* of type *Memory.Pointer* is converted ("type cast") to a variable of type *MyPointer = POINTER TO MyArray*, then the type *MyArray* must *not* be an open array, as in:

     TYPE MyPointer = POINTER TO MyArray;
       MyRecord = RECORD x: INTEGER END ;
       MyArray = ARRAY OF MyRecord;          (*open array -> NOT ALLOWED*)

     VAR p: MyPointer;

Such a declaration would (intentionally) lead to an "array index out of bound" trap - not when *Memory.New(p, size)* is called, but when accessing *p* afterwards. In *Extended Oberon*, dynamic arrays should always be allocated using the predefined procedure *NEW(p, len)* and not using *Memory.New*.

------------------------------------------------------
*Lifting these restrictions*

A *complete* implementation without these restrictions requires a modified version of module *Kernel* that can also handle *array* blocks (in addition to *record* blocks) and a compiler that implements a predefined procedure *NEW(p, size)* such that:

* If *p* is of type POINTER TO T, the type T can be a named record *or* array type
* Pointer and procedure variable fields are *allowed* in the definition of type T
* Bounds checks on *fixed-length* arrays are performed at *compile* time
* Bounds checks on *open* arrays are performed at *run* time

If you use Extended Oberon, this is already implemented. For Project Oberon 2013, a possible implementation is provided at: http://github.com/andreaspirklbauer/Oberon-enhanced-Oberon07-compiler.

------------------------------------------------------
**Alternatives considered**

An alternative to creating *hidden* type descriptors to hold the *size* of the allocated heap block would be to place the *size* of the heap block in the block's prefix itself (in place of the *tag*). Such a solution would then need to distinguish between two kinds of heap blocks, i.e. whether a) the *type tag* or b) the *size* of the record is stored in the block's prefix, and it must do so *each time* a pointer in the record is accessed.

This approach was used in *Ceres-Oberon* to implement a special low-level procedure *SYSTEM.NEW(p, n)* - an alternative to the predefined procedure NEW - that allocated a heap block *without* a fixed type identified by a type descriptor, i.e. effectively an ARRAY OF BYTE. In Ceres-Oberon, bit 0 in the block's prefix indicated whether bits 0-23 of the prefix represent a *tag* or a *size*. This implied that the garbage collector *also* needed to distinguish between these two kinds of heap blocks and traverse their structure accordingly. To avoid the latter, an *additional* implementation restriction was initially in effect: heap blocks created by *SYSTEM.NEW(p, n)* were *not* allowed to contain descendant pointers, while the Oberon compiler disallowed such constructs (this implementation restriction was lifted in later versions of Ceres-Oberon - at the expense of increasing the complexity of the garbage collector).

We have refrained from implementing this alternative. Instead, we recommend to use either:

1. The modified runtime system and compiler at http://github.com/andreaspirklbauer/Oberon-enhanced-Oberon07-compiler, which implements dynamic heap allocation procedure for fixed-length and open arrays, or:

2. Extended Oberon at http://github.com/andreaspirklbauer/Oberon-extended, which also implements it.

In both cases, SYSTEM.NEW(p, n) is equivalent to NEW(p, n), where p is a POINTER TO ARRAY OF BYTE.

------------------------------------------------------
**APPENDIX: Other possible ways to allocate arrays in the heap:**

Method 1: Use *records* to allocate *fixed-length* arrays:

     TYPE Rec = RECORD a: ARRAY 1000 OF INTEGER END;        (*fixed-length*)
       Ptr = POINTER TO Rec;

     VAR p: Ptr;

     NEW(p); p.a[10] := 1

Method 2: Use record *extensions* to allocate *dynamic-length* arrays:

     MODULE M;
       IMPORT Texts, Oberon;

       TYPE  (*size of arrays ideally 8 less than 32, 64, 128 or n*256 bytes*)
         Rec  = RECORD a: ARRAY 1022 OF INTEGER END;  (*size = 1022*4 = 4096-8 bytes*)
         Rec1 = RECORD (Rec) a1: ARRAY 1024 OF INTEGER END;
         Rec2 = RECORD (Rec1) a2: ARRAY 1024 OF INTEGER END;
         Ptr  = POINTER TO Rec;
         Ptr1 = POINTER TO Rec1;
         Ptr2 = POINTER TO Rec2;

       VAR p: Ptr; p1: Ptr1; p2: Ptr2; W: Texts.Writer;

       PROCEDURE New(VAR ptr: Ptr; len: INTEGER);
         VAR p: Ptr; p1: Ptr1; p2: Ptr2;
       BEGIN
         IF len <= 1016 THEN NEW(p); ptr := p
         ELSIF len <= 2040 THEN NEW(p1); ptr := p1
         ELSIF len <= 3064 THEN NEW(p2); ptr := p2
         ELSE ptr := NIL
         END
       END New;

       PROCEDURE P(ptr: Ptr);  (*can pass p, p1, p2 as parameter, since all are assignment compatible with Ptr*)
       BEGIN
         CASE ptr OF
             Ptr2: Texts.WriteString(W, "ptr IS Ptr2")
           | Ptr1: Texts.WriteString(W, "ptr IS Ptr1")
           | Ptr: Texts.WriteString(W, "ptr IS Ptr")
          END ;
          Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)
       END P;

       PROCEDURE Go*;
         VAR p: Ptr; p1: Ptr1; p2: Ptr2;
           S: Texts.Scanner;
       BEGIN Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
         IF S.class = Texts.Int THEN New(p, S.i);
           CASE p OF
              Ptr2: p2 := p(Ptr2); P(p2)
            | Ptr1: p1 := p(Ptr1); P(p1)
            | Ptr: P(p)
           END
         END
       END Go;

     BEGIN Texts.OpenWriter(W)
     END M.

     ORP.Compile M.Mod/s ~
     M.Go 1000 ~
     M.Go 2000 ~
     M.Go 3000 ~

  **Example:** Module *Fonts* in the Project Oberon 2013 system:

     TYPE Font* = POINTER TO FontDesc;
       FontDesc* = RECORD
         name*: ARRAY 32 OF CHAR;
         height*, minX*, maxX*, minY*, maxY*: INTEGER;
         next*: Font;
         T: ARRAY 128 OF INTEGER;
         raster: ARRAY 2360 OF BYTE
       END ;

       LargeFontDesc = RECORD (FontDesc) ext: ARRAY 2560 OF BYTE END ;
       LargeFont = POINTER TO LargeFontDesc;

     PROCEDURE Load*(name: ARRAY OF CHAR);
       ..
       IF NofBytes < 2300 THEN NEW(F) ELSE NEW(LF); F := LF END ;

Method 3: Use *arrays of pointers* to allow for even more granularity:

     TYPE
       DataBlock = RECORD data: ARRAY 256 OF INTEGER END;
       Data = POINTER TO DataBlock;

       Rec = RECORD a: ARRAY 16 OF Data END;  (*allocate data block a[i] "on demand" using NEW*)
       Ptr = POINTER TO Rec;

     VAR p: Ptr;

     NEW(p); NEW(p.a[0]); p.a[0].data[10] := 1
