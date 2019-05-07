unit ISLFunctions;
interface
uses Variants, ISLTypes, ISLUtils, UVectors;
type
 TSysFuncs=class
        function Sin(Args:TArray; var error:pchar):Variant;
        function Cos(Args:TArray; var error:pchar):Variant;
        function Pow(Args:TArray; var error:pchar):Variant;
        function Sqr(Args:TArray; var error:pchar):Variant;
        function Sqrt(Args:TArray; var error:pchar):Variant;
        function _SetLength(Args:TArray; var error:pchar):Variant;
        function SetElement(Args:TArray; var error:pchar):Variant;
        function GetElement(Args:TArray; var error:pchar):Variant;
        function Length_(Args:TArray; var error:pchar):Variant;
        function Inc_(Args:Tarray; var error:pchar):Variant;
        function Dec_(Args:Tarray; var error:pchar):Variant;
        function numtostr(Args:Tarray; var error:pchar):Variant;
        function uppercase(Args:Tarray; var error:pchar):Variant;
        function trim(Args:Tarray; var error:pchar):Variant;
        function lowercase(Args:Tarray; var error:pchar):Variant;
        function booltostr(Args:Tarray; var error:pchar):Variant;
        function strtobool(Args:Tarray; var error:pchar):Variant;
        function booltonum(Args:Tarray; var error:pchar):Variant;
        function numtobool(Args:Tarray; var error:pchar):Variant;
        function strcopy(Args:Tarray; var error:pchar):Variant;
        function year(Args:Tarray; var error:pchar):Variant;
        function month(Args:Tarray; var error:pchar):Variant;
        function day(Args:Tarray; var error:pchar):Variant;
        function hour(Args:Tarray; var error:pchar):Variant;
        function min(Args:Tarray; var error:pchar):Variant;
        function sec(Args:Tarray; var error:pchar):Variant;
        function abs(Args:Tarray; var error:pchar):Variant;
        function randseed(Args:Tarray; var error:pchar):Variant;
        function rand(Args:Tarray; var error:pchar):Variant;
        function LogN(Args:Tarray; var error:pchar):Variant;
        function Ln(Args:Tarray; var error:pchar):Variant;
        function Floor(Args:Tarray; var error:pchar):Variant;
        function Ceil(Args:Tarray; var error:pchar):Variant;
        function _Lerp(Args:Tarray; var error:pchar):Variant;
 end;
  TSystemFunc=record
        Name:string;
        NumParams:integer;
//        ResType:TVarType;
//        ParamTypes:array of TVarType;
        F:TMethod;
 end;

 //compile time methods
 function cIsFunction(Rut:TFunc; const name: String): boolean;
 function cCheckFunction(var Rut:TFunc; SName:string; pCount:integer; var str:string): boolean;

 //runtime methods
 function rCallFunction(var Rut:TFunc; const DaFunc: String; const Args: TArray; var error:string): Variant;

 //miscelious functions
 procedure AddFunction(Name:string; NumParams:integer; F:TMethod);
 procedure Clear;

var
 Funcs:TSysFuncs=nil;
 fFunctions:array of TSystemFunc;
implementation
uses SysUtils, ISLMain, DateUtils,math;

function cIsFunction;
var
  sName, s: String;
  nTemp1, nTemp2: Integer;
  i:integer;
begin
 s:=name;
 nTemp1 := Pos('(',s);
 nTemp2 := CodeInStr(nTemp1+1, s, ')');
 s := Copy(s, 1, nTemp2-1);
 if s='' then s:=name;
 sName  := lowercase(Copy(s, 1, nTemp1-1));
 if sName='' then sName:=lowercase(name);
 Result:= false;

 for i:=0 to high(fFunctions) do
        if sName=fFunctions[i].Name then result:=true;
end;

function rCallFunction;
var
 i:integer;
 e:pchar;
begin
 error:='';
 Result:=0;
 for i:=0 to high(fFunctions) do
        if (fFunctions[i].Name=lowercase(DaFunc))and(length(Args)=fFunctions[i].NumParams)then
                if assigned(fFunctions[i].F) then Result:=fFunctions[i].F(Args, e);
 error:=e;
end;


function cCheckFunction;
var
  i:integer;
  ok:boolean;
begin
 ok:=true;
 for i:=0 to high(fFunctions) do begin
        if (sName=fFunctions[i].Name)then
                if pCount=fFunctions[i].NumParams then ok:=true
                                                  else ok:=false;
 end;
 if not ok then begin
        result:=false;
        str:='Different parameter count!';
        exit;
 end;
 result:=true;
end;

procedure Clear;
begin
 fFunctions:=nil;

 //math functions

 if funcs=nil then Funcs:=TSysFuncs.create;
 AddFunction('sin', 1, Funcs.sin);
 AddFunction('cos', 1, Funcs.cos);
 AddFunction('pow', 2, Funcs.pow);
 AddFunction('sqr', 1, Funcs.sqr);
 AddFunction('sqrt', 1, Funcs.sqrt);
 AddFunction('inc', 1, Funcs.inc_);
 AddFunction('dec', 1, Funcs.dec_);

 //vector Functions
 AddFunction('setlength', 2, Funcs._setlength);

 //string and vector functions
 AddFunction('setelement', 3, Funcs.setelement);
 AddFunction('getelement', 2, Funcs.getelement);
 AddFunction('length', 1, Funcs.length_);

 AddFunction('numtostr', 2,Funcs.numtostr);
 AddFunction('uppercase', 1, Funcs.uppercase);
 AddFunction('trim', 1, Funcs.trim);
 AddFunction('lowercase', 1, Funcs.lowercase);
 AddFunction('booltostr', 1, Funcs.booltostr);
 AddFunction('strtobool', 1, Funcs.strtobool);
 AddFunction('booltonum', 1, Funcs.booltonum);
 AddFunction('numtobool', 1, Funcs.numtobool);
 AddFunction('strcopy', 3, Funcs.strcopy);
 AddFunction('year', 0, Funcs.year);
 AddFunction('month', 0, Funcs.month);
 AddFunction('day', 0, Funcs.day);
 AddFunction('hour', 0, Funcs.hour);
 AddFunction('min', 0, Funcs.min);
 AddFunction('sec', 0, Funcs.sec);
 AddFunction('abs', 1, Funcs.abs);
 AddFunction('random', 0, Funcs.rand);
 AddFunction('randseed', 1, Funcs.randseed);
 AddFunction('ceil', 1, Funcs.Ceil);
 AddFunction('floor', 1, Funcs.Floor);
 AddFunction('ln', 1, Funcs.Ln);
 AddFunction('logn', 2, Funcs.LogN);
 AddFunction('lerp', 3, Funcs._Lerp);
end;

procedure AddFunction;
var
 i:integer;
begin
 for i:=0 to high(fFunctions) do
        if (fFunctions[i].Name=lowercase(Name))and(fFunctions[i].NumParams=NumParams) then exit;
 setlength(fFunctions, length(fFunctions)+1);
 fFunctions[high(fFunctions)].Name:=lowercase(Name);
 fFunctions[high(fFunctions)].NumParams:=NumParams;
 fFunctions[high(fFunctions)].F:=F;

{ fFunctions[high(fFunctions)].ResType:=ResType;
 setlength(fFunctions[high(fFunctions)].ParamTypes, length(ParamTypes));
 for i:=0 to high(ParamTypes) do
        fFunctions[high(fFunctions)].ParamTypes[i]:=ParamTypes[i];}
end;

function TSysFuncs.Sin;
begin
 error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid argument';
        exit;
 end;
 result:=System.sin(Args[0]);
end;

function TSysFuncs.Cos;
begin
 error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 result:=System.cos(Args[0]);
end;

function TSysFuncs.Pow;
begin
 error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 if (varType(Args[1])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 result:=Power(Args[0], Args[1]);
end;

function TSysFuncs.Sqr;
begin
 error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 result:=System.sqr(Args[0]);
end;

function TSysFuncs.Sqrt;
begin
 error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 result:=System.sqrt(Args[0]);
end;

function TSysFuncs.GetElement;
begin
 error:='';
 if (varType(Args[1])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 if not((VarType(Args[0])=str)or(VarType(Args[0])=nVect)or(VarType(Args[0])=sVect)or(VarType(Args[0])=bVect)) then begin
        Error:='Invalid Argument';
        exit;
 end;
 if (Args[1]<0)or(Args[1]>VarArrayHighBound(Args[0],1)) then begin
        Error:='Range check error';
        exit;
 end;
 result:=Args[0][integer(Args[1])];
end;

function TSysFuncs.SetElement;
begin
 error:='';
 if (varType(Args[1])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;

 if VarType(Args[0])=str then begin
        if (VarType(Args[2])<>str)or(length(Args[2])>1) then begin
                error:='Invalid Argument';
                exit;
        end;
        Args[0][Args[1]]:=Args[2];
        Result:=Args[0];
        exit;
 end;
 if not(((VarType(Args[0])=nVect)and(VarType(Args[2])=num))or((VarType(Args[0])=sVect)and(VarType(Args[2])=str))or((VarType(Args[0])=bVect)and(VarType(Args[2])=bool))) then begin
        Error:='Invalid Argument';
        exit;
 end;
 if (Args[1]<0)or(Args[1]>VarArrayHighBound(Args[0],1)) then begin
        Error:='Range check error';
        exit;
 end;
 Args[0][Args[1]]:=Args[2];
 Result:=Args[0];
end;

function TSysFuncs.Inc_;
begin
 error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 inc(Args[0]);
 result:=Args[0];
end;

function TSysFuncs.Dec_;
begin
  error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 dec(Args[0]);
 result:=Args[0];
end;

function TSysFuncs.Length_;
begin
 error:='';
 if varType(Args[0])=str then begin
        result:=length(Args[0]);
        exit;
 end;
 if not((VarType(Args[0])=nVect)or(VarType(Args[0])=sVect)or(VarType(Args[0])=bVect)) then begin
        Error:='Invalid Argument';
        exit;
 end;
 result:=VarArrayHighBound(Args[0],1)+1;
end;

function TSysFuncs._SetLength(Args:TArray; var error:pchar):Variant;
begin
 error:='';
 if not((VarType(Args[0])=nVect)or(VarType(Args[0])=sVect)or(VarType(Args[0])=bVect)) then begin
        Error:='Invalid Argument';
        exit;
 end;
 if not (VarType(Args[1])=Num) then begin
        Error:='Invalid Argument';
        exit;
 end;
 if Args[1]<1 then begin
        Error:='Range check error';
        exit;
 end;
 VarArrayRedim(Args[0],integer(Args[1])-1);
 Result:=Args[0];
end;

function TSysFuncs.numtostr(Args: Tarray; var error: pchar): Variant;
var
 a:longint;
 b:double;
begin
 //returneaza un string dintr-un numar. numtostr(string,zecimale);

 error:='';
 if (varType(Args[0])<>num)or(varType(Args[1])<>num)or(Args[1]<0)or(Args[1]>8) then begin
        error:='Invalid Argument';
        exit;
 end;
 if Args[1]=0 then begin
    a:=Args[0];
    result:=inttostr(a);
 end
 else begin
    b:=Args[0];
    result:=floattostrf(b, ffNumber , 15, Args[1]) ;
 end;
end;

function TSysFuncs.booltonum(Args: Tarray; var error: pchar): Variant;
var
 a:boolean;
begin
 //returneaza 1 pentru true si 0 pentru false. booltonum(var_bool);
 error:='';
 if (varType(Args[0])<>bool) then begin
        error:='Invalid Argument';
        exit;
 end;
 a:=Args[0];
 if a then result:=1
      else result:=0;
end;

function TSysFuncs.booltostr(Args: Tarray; var error: pchar): Variant;
var
 a:boolean;
begin
 //returneaza 'true' pentru true si 'false' pentru false. booltostr(var_bool);
 error:='';
 if (varType(Args[0])<>bool) then begin
        error:='Invalid Argument';
        exit;
 end;
 a:=Args[0];
 if a then result:='true'
      else result:='false';
end;

function TSysFuncs.day(Args: Tarray; var error: pchar): Variant;
var
 i:integer;
begin
 error:='';
 //get current day
 i:=dayof(now);
 result:=i;
end;

function TSysFuncs.hour(Args: Tarray; var error: pchar): Variant;
var
 i:integer;
begin
 error:='';
 //get current hour
 i:=hourof(now);
 result:=i;
end;

function TSysFuncs.lowercase(Args: Tarray; var error: pchar): Variant;
var
 a:string;
begin
 //returneaza textul in care toate literele sunt mici. lowercase(str);
 error:='';
 if (varType(Args[0])<>str) then begin
        error:='Invalid Argument';
        exit;
 end;
 a:=Args[0];
 result:=sysutils.LowerCase(a);
end;

function TSysFuncs.min(Args: Tarray; var error: pchar): Variant;
var
 i:integer;
begin
 error:='';
 //get current minute
 i:=Minuteof(now);
 result:=i;
end;

function TSysFuncs.month(Args: Tarray; var error: pchar): Variant;
var
 i:integer;
begin
 error:='';
 //get current minute
 i:=MonthOf(now);
 result:=i;
end;

function TSysFuncs.numtobool(Args: Tarray; var error: pchar): Variant;
begin
 //returneaza true pentru orice numar diferit de 0 si false pentru 0. numtobool(var_num);
 error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 if Args[0]=0 then result:=false
              else result:=true;
end;

function TSysFuncs.sec(Args: Tarray; var error: pchar): Variant;
var
 i:integer;
begin
 error:='';
 //get current second
 i:=SecondOf(now);
 result:=i;
end;

function TSysFuncs.strcopy(Args: Tarray; var error: pchar): Variant;
var
 a,b:integer;
 s:string;
begin
 //returneaza un substring dintr-un string. strcopy(string,start,length);
 error:='';
 if (varType(Args[0])<>str)or(varType(Args[1])<>num)or(varType(Args[2])<>num)or(Args[2]<1)or(Args[1]<1) then begin
        error:='Invalid Argument';
        exit;
 end;
 s:=Args[0];
 a:=Args[1];
 b:=Args[2];
 result:=copy(s,a,b);
end;

function TSysFuncs.strtobool(Args: Tarray; var error: pchar): Variant;
var
 s:string;
begin
 //returneaza true pentru string-ul 'true' si false pentru orice altceva. strtobool(var_str);
 error:='';
 if (varType(Args[0])<>str) then begin
        error:='Invalid Argument';
        exit;
 end;
 s:=Args[0];
 if sysutils.trim(sysutils.UpperCase(s))='TRUE' then result:=true
                                                else result:=true;
end;

function TSysFuncs.trim(Args: Tarray; var error: pchar): Variant;
var
 s:string;
begin
 //elimina spatiile de la inceputul si sfarsitul unui string. trim(var_str);
 error:='';
 if (varType(Args[0])<>str) then begin
        error:='Invalid Argument';
        exit;
 end;
 s:=Args[0];
 result:=sysutils.Trim(s);
end;

function TSysFuncs.uppercase(Args: Tarray; var error: pchar): Variant;
var
 s:string;
begin
 //transforma toate caracterele dintr-un string in litere mari. uppercase(var_str);
 error:='';
 if (varType(Args[0])<>str) then begin
        error:='Invalid Argument';
        exit;
 end;
 s:=Args[0];
 result:=sysutils.UpperCase(s);
end;

function TSysFuncs.year(Args: Tarray; var error: pchar): Variant;
var
 i:integer;
begin
 error:='';
  //get current year
 i:=YearOf(now);
 result:=i;
end;

function TSysFuncs.abs(Args: Tarray; var error: pchar): Variant;
begin
 error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 result:=system.Abs(Args[0]);
end;

function TSysFuncs.rand(Args: Tarray; var error: pchar): Variant;
begin
 error:='';
 result:=random;
end;

function TSysFuncs.randseed(Args: Tarray; var error: pchar): Variant;
begin
 error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 System.RandSeed:=trunc(Args[0]);
 result:=trunc(Args[0]);
end;

function TSysFuncs.Ceil(Args: Tarray; var error: pchar): Variant;
var
  d:extended;
begin
 error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 d:=Args[0];
 result:=math.Ceil(d);
end;

function TSysFuncs.Floor(Args: Tarray; var error: pchar): Variant;
var
  d:extended;
begin
 error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 d:=Args[0];
 result:=math.Floor(d);
end;

function TSysFuncs.Ln(Args: Tarray; var error: pchar): Variant;
var
  d:extended;
begin
 error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 d:=Args[0];
 result:=math.LnXP1(d-1);
end;

function TSysFuncs.LogN(Args: Tarray; var error: pchar): Variant;
var
  d:extended;
begin
 error:='';
 if (varType(Args[0])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 d:=Args[1];
 result:=math.LogN(Args[0], d);
end;

function TSysFuncs._Lerp(Args: Tarray; var error: pchar): Variant;
begin
 error:='';
 if (varType(Args[0])<>num)or(varType(Args[1])<>num)or(varType(Args[2])<>num) then begin
        error:='Invalid Argument';
        exit;
 end;
 result:=Lerp(Args[0], Args[1], Args[2]);
end;

initialization
 funcs:=nil;
finalization
 if funcs<>nil then funcs.Destroy;
end.
