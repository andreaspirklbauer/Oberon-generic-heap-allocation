MODULE MemoryTest;
  IMPORT SYSTEM, Texts, Oberon, Memory;
  CONST size = 56;  (*ideally 8 less than 32, 64, 128 or n*256*)
    maxsize = 65536;

  TYPE Pointer = POINTER TO Record;
    Record = RECORD x: ARRAY maxsize OF BYTE END ;

  VAR ptr: Pointer;
    W: Texts.Writer;

  PROCEDURE New(VAR ptr: Pointer; size: LONGINT);
    VAR p: Memory.Pointer;
  BEGIN Memory.New(p, size); ptr := SYSTEM.VAL(Pointer, p)
  END New;

  PROCEDURE Free(VAR ptr: Pointer);
  BEGIN ptr := NIL
  END Free;

  PROCEDURE NewPtr*;
  BEGIN New(ptr, size)
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
