unit ISLUtils;
interface
uses ISLTypes;
type
 TVarArray=array of TVariable;
 TFunc=record
        fName:shortString;
        fVariables:TVarArray;
        fParams:TVarArray;
        fScript:array of String;
        fIf:array of byte;
        fWhile:array of record
                line:integer;
                condition:string;
        end;
        last:Array of byte;
        fResult:TVariable;
        fLevel:integer;
 end;

 eFindType = (NonOperator, Operator, NonNumber);

function IsOperator(nVal: String): Boolean;
function IsNumber(nVal: String): Boolean;
function CodeFind(Start: Integer; const Text: String; Find: eFindType): Integer;
function CodeInStr(Start: Integer; const Text: String; const Find: String): Integer;
function NormalizeFloat(str:Pchar):PChar; overload;
function NormalizeFloat(str:string):string; overload;
function VarType(V:Variant):TVarType;
implementation
uses SysUtils;

function VarType(V:Variant):TVarType;
begin
 if (TVarData(V).VType>=2)and(TVarData(V).VType<=5) then result:=num
 else if (TVarData(V).VType=$0B) then result:=bool
 else if (TVarData(V).VType=8)or(TVarData(V).VType=$100) then result:=str
 else if (TVarData(V).VType>=2+$02000)and(TVarData(V).VType<=5+$02000) then result:=nVect
 else if (TVarData(V).VType=$0B+$02000) then result:=bVect
 else result:=sVect;
end;

function NormalizeFloat(str:Pchar):PChar; overload;
var
 p:integer;
 s:string;
begin
 s:=str;
 Result:=str;
 p:=pos('.',s);
 if p=0 then exit;
 s[p]:='.';
 Result:=pchar(s);
end;

function NormalizeFloat(str:string):string; overload;
var
 p:integer;
 s:string;
begin
 s:=str;
 Result:=str;
 p:=pos('.',s);
 if p=0 then exit;
 s[p]:='.';
 Result:=s;
end;

function CodeInStr;
var
 i, p: Integer;
 sTemp: String;
begin
 Result:=0;
 i:=Start;
 p:=0;
 while i<=Length(Text) do begin

        if (Copy(Text, i, Length(Find))=Find) and (p=0) then begin
                Result := i;
                Exit;
        end;
        sTemp := Text[i];
        if sTemp = '(' then inc(p)
                       else if sTemp =')'then dec(p);
                       
        if sTemp=#34 then i:=i+pos(#34, copy(Text, i+1, length(Text)));

        if i = 0 then Exit;
        inc(i);
 end;
end;

function CodeFind;
var
 p,p1, i: Integer;
 sTemp: String;
begin
 Result := 0;
 i := Start;
 p := 0;
 p1:=0;
 while i >= 1 do begin
        sTemp := Text[i];

        if (p = 0)and(p1 mod 2=0) then
                if Find=NonOperator then begin
                        if IsOperator(sTemp) = False then begin
                                Result := i;
                                Exit;
                        end;
                end
                else begin
                        if (IsOperator(sTemp) = True) then begin
                                Result := i;
                                Exit;
                        end;
                end;

         if sTemp = '"' then inc(p1)
                        else if sTemp = '(' then inc(p)
                                            else if sTemp = ')' then dec(p);

         dec(i);
 end;
end;

function IsNumber(nVal: String): Boolean;
begin
 Result := False;
 if (nVal = '0')or
    (nVal = '1')or
    (nVal = '2')or
    (nVal = '3')or
    (nVal = '4')or
    (nVal = '5')or
    (nVal = '6')or
    (nVal = '7')or
    (nVal = '8')or
    (nVal = '9')or
    (nVal = '.')or
    (nVal = 'e') then Result := True;
end;

function IsOperator(nVal: String): Boolean;
begin
 Result := False;
 if(nVal = '+') or
   (nVal = '-') or
   (nVal = '*') or
   (nVal = '^') or
   (nVal = '%') or
   (nVal = '/') or
   (nVal = '\') or
   (nVal = '&') or
   (nVal = '>') or
   (nVal = '<') or
   (nVal = '=') or
   (nVal = '!') or
   (nVal = '|') then
    Result := True;
end;

end.

