MODULE TestNew;
  IMPORT SYSTEM, Out;

  TYPE PtrArr = POINTER TO Arr;
    Arr = ARRAY OF BYTE;

    PtrRec = POINTER TO Rec;
    Rec = RECORD i, j, k: INTEGER;
      ptrarr: PtrArr
    END ;

  VAR ptrrec: PtrRec; i: INTEGER;

  PROCEDURE P*;
  BEGIN i := 33;
    NEW(ptrrec); NEW(ptrrec.ptrarr, 100); ptrrec.ptrarr[i] := 111
  END P;

  PROCEDURE Q*;
  BEGIN Out.Int(ptrrec.ptrarr[i], 6); Out.Ln
  END Q;

  PROCEDURE R*;  (*if R is called before S, then S will (intentionally) generate to a trap*)
  BEGIN ptrrec := NIL
  END R;

  PROCEDURE S*;  (*this intentionally generates a trap if R has been called before*)
    VAR a0: INTEGER; b: BYTE;
  BEGIN a0 := SYSTEM.ADR(ptrrec.ptrarr^[i]); SYSTEM.GET(a0, b); Out.Int(b, 6); Out.Ln
  END S;

  PROCEDURE T*;
    VAR a0: INTEGER; b: BYTE; ptrrec: PtrRec; i: INTEGER;
  BEGIN i := 99;
    NEW(ptrrec); NEW(ptrrec.ptrarr, 100); ptrrec.ptrarr[i] := 222;
    a0 := SYSTEM.ADR(ptrrec.ptrarr^[i]); SYSTEM.GET(a0, b); Out.Int(b, 6); Out.Ln
  END T;

END TestNew.

ORP.Compile TestNew.Mod/s ~
System.Free TestNew ~
TestNew.P
TestNew.Q
TestNew.R   # attention: if R is called before S, then S will (intentionally) generate to a trap
TestNew.S
TestNew.T



