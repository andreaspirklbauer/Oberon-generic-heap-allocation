MODULE Memory;  (*Generic heap allocation procedure New(ptr, size) / AP 7.2.20 Extended Oberon*)
  IMPORT SYSTEM, Kernel, Modules, Oberon;

  TYPE Pointer* = POINTER TO Record;
    Record* = RECORD END ;
    Tag = POINTER TO TypeDesc;
    TypeDesc = RECORD
      size: LONGINT;  (*size, in bytes, of the heap record including hidden prefix fields tag and mk*)
      ext: ARRAY 3 OF LONGINT;  (*extension table of tags (pointers to the descriptors) of the base types*)
      off: ARRAY 8 OF LONGINT;  (*pointer offsets followed by procedure variable offsets*)
      mark: LONGINT;  (*used to collect no longer referenced hidden type descriptors*)
      next: Tag
    END ;

  VAR root: Tag;  (*list of hidden type descriptors created by New(ptr, size)*)
    T: Oberon.Task;  (*background task that automatically collects hidden type descriptors*)

  PROCEDURE New*(VAR ptr: Pointer; size: LONGINT);
    VAR t: Tag;
  BEGIN (*convert size for heap allocation*)
    IF size <= 24 THEN size := 32 ELSIF size <= 56 THEN size := 64 ELSIF size <= 120 THEN size := 128
    ELSE size := (size+263) DIV 256 * 256
    END ;
    t := root;
    WHILE (t # NIL) & (t.size # size) DO t := t.next END ;
    IF t = NIL THEN NEW(t); (*allocate hidden type descriptor*)
      IF t # NIL THEN t.next := root; root := t; t.size := size; t.mark := 0;
        t.ext[0] := -1; t.ext[1] := -1; t.ext[2] := -1; t.off[0] := -1; (*ptr*) t.off[1] := -1 (*pvr*)
      END
    END ;
    IF t = NIL THEN ptr := NIL ELSE Kernel.New(SYSTEM.VAL(LONGINT, ptr), SYSTEM.ADR(t^), -1, 0) END
  END New;

  PROCEDURE Collect*; (*no longer referenced hidden type descriptors*)
    VAR mod: Modules.Module;
      t, prev: Tag;
      p, mark, tag, size, blktyp: LONGINT;
      continue: BOOLEAN;
  BEGIN
    IF root # NIL THEN mod := Modules.root;
      WHILE mod # NIL DO
        IF mod.name[0] # 0X THEN Kernel.Mark(mod.ptr) END ;
        mod := mod.next
      END ;
      p := Kernel.heapOrg;
      REPEAT SYSTEM.GET(p+4, mark);
        IF mark < 0 THEN (*free*) SYSTEM.GET(p, size)
        ELSE (*allocated*) SYSTEM.GET(p, tag); blktyp := tag MOD 4;
          IF blktyp = 0 THEN (*record*) SYSTEM.GET(tag, size) ELSE (*array*) SYSTEM.GET(p+16, size) END ;
          IF mark > 0 THEN (*marked*) SYSTEM.PUT(p+4, 0);
            IF blktyp = 0 THEN (*record*) t := root; continue := TRUE;
              WHILE continue & (t # NIL) DO (*mark referenced hidden type descriptors*)
                IF SYSTEM.VAL(LONGINT, t) = tag THEN INC(t.mark); continue := FALSE END ;
                t := t.next
              END
            END
          END
        END ;
        INC(p, size)
      UNTIL p >= Kernel.heapLim;
      t := root;
      WHILE t # NIL DO (*remove no longer referenced descriptors from list for later collection by GC*)
        IF t.mark > 0 THEN t.mark := 0; prev := t
        ELSIF t = root THEN root := root.next
        ELSE prev.next := t.next
        END ;
        t := t.next
      END
    END
  END Collect;

  PROCEDURE Start*;
  BEGIN IF T = NIL THEN T := Oberon.NewTask(Collect, 1000); Oberon.Install(T) END
  END Start;

  PROCEDURE Stop*;
  BEGIN IF T # NIL THEN Oberon.Remove(T); T := NIL END
  END Stop;

BEGIN root := NIL; T := NIL
END Memory.
