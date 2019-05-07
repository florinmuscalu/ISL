unit ISLMain;
interface
uses Variants, ISLUtils, ISLTypes, ISLFunctions, Classes;

 //compile-time rutines
 procedure cCreateFunctions;
 function  cCompileFunction(var Rut:TFunc):boolean;
 function  cAddParams(p:string; var Rut:TFunc):boolean;
 function  cAddVariables(p:string; var Rut:TFunc):boolean;
 function  cCheck(var Rut:TFunc; sEquation: String; var str:string): boolean;
 function  cIsDefined(var Rut:TFunc; const name: String): boolean;
 function  cIsOperator(sOp: String): boolean;
 function  cIsVar(var Rut:TFunc; const name: String): boolean;
 function  cIsFunction(var Rut:TFunc; const name: String): boolean;
 function  cCheckFunction(var Rut:TFunc; DaCall: String; var str:string): boolean;

 //other rutines
 procedure RaiseError(s:string; RutName:String; Line:String; compilation:boolean; linenr:integer=-1);
 procedure setGlobalVars;
 function getGlobalVar(index:integer):TVariable;
 procedure setGlobalVar(index:integer; value:Variant);

 //runtime rutines
 function  rCallFunc(Rut:TFunc):boolean;
 function  rDoOperator(const LeftVal, RightVal: Variant; sOp: String): Variant;
 function  rDoFunction(var Rut:TFunc; DaCall: String): Variant;
 function  rGetVar(var Rut:TFunc; const sVar: String): Variant;
 procedure rChangeVar(var Rut:TFunc; const sVar: String; sNewVal: Variant);
 function  rCallFunction(var Rut:TFunc; const DaFunc: String; const Args: TArray): Variant;
 function  rSolve(var Rut:TFunc; sEquation: String): Variant;
 function  rCopyValues(Src:TArray; var dst:TVarArray):boolean;
 function  rSetParams(p:string; var Rut:TFunc):boolean;
 procedure rExecute;

 function Load(filename:string):boolean;
 function LoadFromPointer(p:PChar; size:cardinal):boolean; stdcall;
 function LoadCompiled(filename:string):boolean;
 function SaveCompiled(filename:string):boolean;

 procedure Create;
 procedure destroy;

 procedure Compile;
 procedure ExecuteScript(fFunction:string); overload;
 procedure ExecuteScript(fname:string; Args:TArray); overload;
 function Result:variant;

 procedure AddGlobalVar(Name:string; _Type:TVarType; value:Variant);
 procedure ClearGlobalVars;
 function GlobalVarCount:integer;

 function CompileError:string;
 function RunError:string;

 procedure setCompileError(v:string);
 procedure setRunError(v:string);


 function NewScript:cardinal; stdcall;
 procedure BindScript(const Handle:cardinal); stdcall;
 procedure DeleteScript(const Handle:cardinal); stdcall;

var
 fGlobalVars:Array of TVariable;
 fScripts:array of record
        Nr:cardinal;
        fFunc:array of TFunc;
        fScript: array of string;
        fCompileError:string;
        fResult:variant;
        fToRunFunction: String;
        fRunError:string;
 end;
 fBindedScript:cardinal=0;
implementation
uses sysutils;

function NewScript:cardinal;
var
 i:integer;
 res:cardinal;
begin
 res:=0;
 for i:=0 to high(fScripts) do if fScripts[i].Nr>res then res:=fScripts[i].Nr;
 inc(res);
 setlength(fScripts, length(fScripts)+1);
 fScripts[high(fScripts)].Nr:=res;
 fScripts[high(fScripts)].fFunc:=nil;
 fScripts[high(fScripts)].fScript:=nil;
 fScripts[high(fScripts)].fCompileError:='No Error';
 fScripts[high(fScripts)].fResult:=0;
 fScripts[high(fScripts)].fToRunFunction:='';
 fScripts[high(fScripts)].fRunError:='No Error';
 result:=res;
end;

procedure BindScript(const Handle:cardinal);
var
 i:integer;
begin
 if fBindedScript=Handle then exit;
 fBindedScript:=0;
 for i:=0 to high(fScripts) do
        if fScripts[i].Nr=Handle then begin
                fBindedScript:=Handle;
                exit;
        end;
end;

procedure DeleteScript(const Handle:cardinal);
var
 i,j:integer;
begin
 if fBindedScript=Handle then fBindedScript:=0;
 for i:=0 to high(fScripts) do
        if fScripts[i].Nr=Handle then begin
                for j:=i+1 to high(fScripts) do
                        fScripts[j-1]:=fScripts[j];
                setlength(fScripts, high(fScripts));
                exit;
        end;
end;

procedure setCompileError(v:string);
begin
 if fBindedScript=0 then exit;
 fScripts[fBindedScript-1].fCompileError:=v;
end;

procedure setRunError(v:string);
begin
 if fBindedScript=0 then exit;
 fScripts[fBindedScript-1].fRunError:=v;
end;

function CompileError:string;
begin
 result:='No Error';
 if fBindedScript=0 then exit;
 result:=fScripts[fBindedScript-1].fCompileError;
end;

function RunError:string;
begin
 result:='No Error';
 if fBindedScript=0 then exit;
 result:=fScripts[fBindedScript-1].fRunError;
end;

function Result:variant;
begin
 result:=0;
 if fBindedScript=0 then exit;
 result:=fScripts[fBindedScript-1].fResult;
end;

////////////////////Compilation Rutines/////////////////////////////////////////
////////////////////Compilation Rutines/////////////////////////////////////////
////////////////////Compilation Rutines/////////////////////////////////////////
function cIsOperator;
begin
 result:=false;
 if (sOp = '+')or(sOp = '-')or(sOp = '*')or(sOp = '%')or(sOp = '/')or
    (sOp = '\')or(sOp = '|')or(sOp = '&')or(sOp = '^')or(sOp = '>>')or
    (sOp = '<<')or(sOp = '>')or(sOp = '<')or(sOp = '>=')or(sOp = '<=')or
    (sOp = '<>')or(sOp = '=')or(sOp = '==')or(sOp = '!') then result:=true;
end;

procedure Compile;
var
 i:integer;
begin
 if fBindedScript=0 then exit;
 cCreateFunctions;
 for i:=0 to high(fScripts[fBindedScript-1].fFunc) do if not cCompileFunction(fScripts[fBindedScript-1].fFunc[i]) then exit;
end;

procedure cCreateFunctions;
var
 i:integer;
 k,j:integer;
 inFunc, inDefine:boolean;
 s, res, params, name, value:string;
 d:double;
 f:TFunc;

 vard:double;
begin
 if fBindedScript=0 then exit;
 d:=0;
 Finalize(fScripts[fBindedScript-1].fFunc);
 fScripts[fBindedScript-1].fFunc:=nil;
 setGlobalVars;
 
 inFunc:=false;
 inDefine:=false;
 for i:=0 to high(fScripts[fBindedScript-1].fScript) do begin

        if LowerCase(fScripts[fBindedScript-1].fScript[i])='define' then
                if inFunc then begin
                        RaiseError('Can''t define a global variable inside a function!',lowercase(s),fScripts[fBindedScript-1].fScript[i], true, i);
                        exit;
                end
                else begin
                        inDefine:=true;
                        continue;
                end;

        if LowerCase(fScripts[fBindedScript-1].fScript[i])='end define' then
                if inFunc then begin
                        RaiseError('Invalid declaration!',lowercase(s),fScripts[fBindedScript-1].fScript[i], true, i);
                        exit;
                end
                else begin
                        inDefine:=false;
                        continue;
                end;

        if LowerCase(fScripts[fBindedScript-1].fScript[i])='end func' then
                if inDefine then begin
                        RaiseError('Invalid declaration!',lowercase(s),fScripts[fBindedScript-1].fScript[i], true, i);
                        exit;
                end
                else begin
                        inFunc:=false;
                        continue;
                end;
                
        if Pos('func ',LowerCase(fScripts[fBindedScript-1].fScript[i]))<>0 then begin
                if inFunc then begin
                        RaiseError('Illegal declaration!',lowercase(s),fScripts[fBindedScript-1].fScript[i], true, i);
                        exit;
                end;
                
                k:=Pos(')',fScripts[fBindedScript-1].fScript[i]);
                if pos(':', copy(fScripts[fBindedScript-1].fScript[i], k+1, length(fScripts[fBindedScript-1].fScript[i])))=0 then begin
                        RaiseError('Function needs a result type',lowercase(s),fScripts[fBindedScript-1].fScript[i], true, i);
                        exit;
                end;
                s:=Trim(Copy(fScripts[fBindedScript-1].fScript[i],5,length(fScripts[fBindedScript-1].fScript[i])));
                if Pos('(',s)=0 then begin
                        if cIsDefined(f, lowercase(copy(s,1,Pos(':',s)-1))) then begin
                                RaiseError('Value "'+s+'" already in use!','', fScripts[fBindedScript-1].fScript[i], true, i);
                                exit;
                        end;
                        setlength(fScripts[fBindedScript-1].fFunc, length(fScripts[fBindedScript-1].fFunc)+1);
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fScript:=nil;
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fIf:=nil;
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fWhile:=nil;
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].last:=nil;
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fName:=lowercase(copy(s,1,Pos(':',s)-1));
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fParams:=nil;
                end
                else begin
                        if cIsDefined(f, lowercase(Trim(copy(s,1,Pos('(',s)-1)))) then begin
                                RaiseError('Value "'+lowercase(Trim(copy(s,1,Pos('(',s)-1)))+'" already in use!','', fScripts[fBindedScript-1].fScript[i], true, i);
                                exit;
                        end;
                        setlength(fScripts[fBindedScript-1].fFunc, length(fScripts[fBindedScript-1].fFunc)+1);
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fScript:=nil;
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fName:=lowercase(Trim(copy(s,1,Pos('(',s)-1)));
                        params:=Copy(s,Pos('(',s)+1,Pos(')',s)-Pos('(',s)-1);
                        if not cAddParams(params,fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)]) then begin
                                RaiseError('Invalid paramaters',lowercase(s),fScripts[fBindedScript-1].fScript[i], true, i);
                                exit;
                        end;
                end;
                k:=Pos(')',s);
                res:=trim(copy(s,k+pos(':', copy(s, k+1, length(s)))+1, length(s)));
                if lowercase(res)='num' then begin
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fType:=num;
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fName:='result';
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fValue:=d;
                end
                else if lowercase(res)='str' then begin
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fType:=str;
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fName:='result';
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fValue:=string('');
                end
                else if lowercase(res)='bool' then begin
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fType:=bool;
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fName:='result';
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fValue:=boolean(false);
                end
                else if lowercase(res)='nvect' then begin
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fType:=nVect;
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fName:='result';
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fValue:=varArrayCreate([0,0],varDouble);
                end
                else if lowercase(res)='svect' then begin
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fType:=sVect;
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fName:='result';
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fValue:=varArrayCreate([0,0],varOleStr);
                end
                else if lowercase(res)='bvect' then begin
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fType:=bVect;
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fName:='result';
                        fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fResult.fValue:=varArrayCreate([0,0],varBoolean);
                end
                else begin
                        RaiseError('Invalid result type',fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fName,fScripts[fBindedScript-1].fScript[i], true, i);
                        exit;
                end;
                inFunc:=true;
                continue;
        end;
        
        if InFunc then
                if (Pos('num ',lowercase(fScripts[fBindedScript-1].fscript[i]))=1)or
                   (Pos('str ',lowercase(fScripts[fBindedScript-1].fscript[i]))=1)or
                   (Pos('bool ',lowercase(fScripts[fBindedScript-1].fscript[i]))=1)or
                   (Pos('bvect ',lowercase(fScripts[fBindedScript-1].fscript[i]))=1)or
                   (Pos('svect ',lowercase(fScripts[fBindedScript-1].fscript[i]))=1)or
                   (Pos('nvect ',lowercase(fScripts[fBindedScript-1].fscript[i]))=1) then begin
                        if not cAddVariables(fScripts[fBindedScript-1].fScript[i],fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)]) then begin
                                RaiseError('Invalid variable declaration',fScripts[fBindedScript-1].fFunc[high(fScripts[fBindedScript-1].fFunc)].fName,fScripts[fBindedScript-1].fScript[i], true, i);
                                exit;
                        end;
                end
                else begin
                        setlength(fScripts[fBindedScript-1].ffunc[high(fScripts[fBindedScript-1].ffunc)].fScript,length(fScripts[fBindedScript-1].ffunc[high(fScripts[fBindedScript-1].ffunc)].fScript)+1);
                        fScripts[fBindedScript-1].ffunc[high(fScripts[fBindedScript-1].ffunc)].fScript[high(fScripts[fBindedScript-1].ffunc[high(fScripts[fBindedScript-1].ffunc)].fScript)]:=fScripts[fBindedScript-1].fScript[i];
                end;

        if InDefine then
                if ((Pos('num ',lowercase(fScripts[fBindedScript-1].fscript[i]))=1)or
                   (Pos('str ',lowercase(fScripts[fBindedScript-1].fscript[i]))=1)or
                   (Pos('bool ',lowercase(fScripts[fBindedScript-1].fscript[i]))=1))and
                   (Pos('=', fScripts[fBindedScript-1].fScript[i])<>0) then begin
                        if (Pos('num ',lowercase(fScripts[fBindedScript-1].fscript[i]))=1) then begin
                                name:=copy(lowercase(fScripts[fBindedScript-1].fscript[i]), 5, length(fScripts[fBindedScript-1].fScript[i]));
                                j:=Pos('=',name);
                                value:=trim(copy(name, j+1, length(name)));
                                name:=trim(copy(name, 1, j-1));
                                vard:=strtofloat(NormalizeFloat(value));
                                AddGlobalVar(name, num, vard);
                        end;

                        if (Pos('str ',lowercase(fScripts[fBindedScript-1].fscript[i]))=1) then begin
                                name:=copy(fScripts[fBindedScript-1].fscript[i], 5, length(fScripts[fBindedScript-1].fScript[i]));
                                j:=Pos('=',name);
                                value:=trim(copy(name, j+1, length(name)));
                                name:=lowercase(trim(copy(name, 1, j-1)));
                                AddGlobalVar(name, str, copy(value, 2, length(value)-2));
                        end;

                        if (Pos('bool ',lowercase(fScripts[fBindedScript-1].fscript[i]))=1) then begin
                                name:=copy(fScripts[fBindedScript-1].fscript[i], 6, length(fScripts[fBindedScript-1].fScript[i]));
                                j:=Pos('=',name);
                                value:=lowercase(trim(copy(name, j+1, length(name))));
                                name:=lowercase(trim(copy(name, 1, j-1)));
                                if value='true' then AddGlobalVar(name, bool, true)
                                                else AddGlobalVar(name, bool, false);
                        end;
                end
                else begin
                        RaiseError('Invalid declaration!',lowercase(s),fScripts[fBindedScript-1].fScript[i], true, i);
                        exit;
                end;
 end;

 if inFunc then begin
        RaiseError('Unterminated Function!',lowercase(s),'', true, -1);
        exit;
 end;
 if inDefine then begin
        RaiseError('Unterminated Define block!',lowercase(s),'', true, -1);
        exit;
 end;
end;

function cCompileFunction;
var
 fCurrentLine:integer;
 Line:string;
 i:integer;
 s, EMessage:string;
 error:boolean;
label EndLine, EndError;
begin
 result:=true;
 Rut.fIf:=nil;
 Rut.fWhile:=nil;
 Rut.last:=nil;
 fCurrentLine:=0;

 while (fCurrentLine<=high(Rut.fScript)) do begin
        Line:=trim(Rut.fScript[fCurrentLine]);

        if LowerCase(Line) = 'end' then begin
                if length(Rut.last)=0 then begin
                        EMessage:='Ilegal end!';
                        goto EndError;
                end
                else begin
                        if Rut.last[high(Rut.last)]=1 then begin
                                setlength(Rut.last, high(Rut.last));
                                goto EndLine;
                        end
                        else begin
                                setlength(Rut.last, high(Rut.last));
                                goto EndLine;
                        end;
                end;
        end;
        if LowerCase(Line) = 'else' then begin
                if (high(Rut.last)>=0)and(Rut.last[high(Rut.last)]=1) then goto EndLine
                        else begin
                                EMessage:='Ilegal else!';
                                goto EndError;
                        end;
        end;
        if LowerCase(Line) = 'exit' then goto EndLine;

        i := Pos(' ',Line);
        if i <> 0 then begin
                s := LowerCase(Copy(Line, 1, i - 1));
                if s = 'if' then begin
                        setlength(Rut.last, length(Rut.last)+1);
                        Rut.last[high(Rut.last)]:=1;
                        error:=cCheck(Rut, Copy(Line, i + 1, length(Line)), EMessage);
                        if error then goto endError
                        else goto EndLine;
                end
                else if s='while' then begin
                        setlength(Rut.last, length(Rut.last)+1);
                        Rut.last[high(Rut.last)]:=2;
                        error:=cCheck(Rut, Copy(Line, i + 1, length(Line)), EMessage);
                        if error then goto endError
                        else goto EndLine;
                end;
        end;

        for i := 1 to Length(Line) do begin
                s := Line[i];
                if Copy(Line,i,2)=':=' then begin
                        if (not cIsVar(Rut,Trim(Copy(Line, 1, i - 1))))and (not cIsFunction(Rut,Trim(Copy(Line, 1, i - 1)))) then begin
                                EMessage:='Udefined Variable: "'+Trim(Copy(Line, 1, i - 1))+'"!';
                                goto EndError;
                        end;
                        error:=cCheck(Rut, Copy(Line, i + 2, length(line)), EMessage);
                        if error then goto EndError
                        else goto EndLine;
                end
                else if s = '(' then begin
                        if not cIsFunction(Rut, Line) then begin
                                EMessage:='Undefined Function: "'+Line+'"!';
                                goto EndError;
                        end;
                        if not cCheckFunction(Rut, Line, EMessage) then goto EndError;
                        goto EndLine;
                end;
        end;

        if not cIsFunction(Rut, Line) then begin
                EMessage:='Undefined Function: "'+Line+'"!';
                goto EndError;
        end;
        if not cCheckFunction(Rut, Line, EMessage) then goto EndError;
        goto EndLine;
        EndError: begin
                result:=false;
                RaiseError(eMessage, Rut.fName, Line, true, fCurrentLine);
                exit;
        end;

        EndLine: inc(fCurrentLine);
 end;
 if length(Rut.Last)<>0 then begin
        result:=false;
        RaiseError('End expected!', Rut.fName, '', true, fCurrentLine);
        exit;
 end;
 fScripts[fBindedScript-1].fResult:=Rut.fResult.fValue;
 Rut.fIf:=nil;
 Rut.fWhile:=nil;
end;

function cAddParams;
var
 s,v, s1:string;
 t:TVarType;
 k:double;
 l:string;
 b:boolean;
begin
 k:=0;
 l:='';
 b:=false;
 result:=true;
 if Trim(p)='' then begin
        Rut.fParams:=nil;
        exit;
 end;
 p:=Trim(p);
 t:=num;
 while Pos(';',p)<>0 do begin
        s:=Trim(copy(p,1,Pos(';',p)-1));
        p:=Trim(copy(p,Pos(';',p)+1, length(p)));
        if Pos('num ',lowercase(s))=1 then begin
                v:=copy(s,5,length(s));
                t:=num;
        end
        else if Pos('str ',lowercase(s))=1 then begin
                v:=copy(s,5,length(s));
                t:=str;
        end
        else if Pos('bool ',lowercase(s))=1 then begin
                v:=copy(s,6,length(s));
                t:=bool;
        end
        else if Pos('nvect ',lowercase(s))=1 then begin
                v:=copy(s,7,length(s));
                t:=nVect;
        end
        else if Pos('svect ',lowercase(s))=1 then begin
                v:=copy(s,7,length(s));
                t:=sVect;
        end
        else if Pos('bvect ',lowercase(s))=1 then begin
                v:=copy(s,7,length(s));
                t:=bVect;
        end
        else begin
                result:=false;
                exit;
        end;
        while Pos(',',v)<>0 do begin
                s1:=Trim(copy(v,1,Pos(',',v)-1));
                v:=Trim(copy(v,Pos(',',v)+1, length(v)));
                if cIsDefined(Rut, lowercase(s1)) then begin
                        RaiseError('Value "'+lowercase(s1)+'" already in use!', Rut.fName, p, true, -1);
                        result:=false;
                        exit;
                end;
                setlength(Rut.fParams, length(Rut.fParams)+1);
                Rut.fParams[high(Rut.fParams)].fName:=lowercase(s1);
                Rut.fParams[high(Rut.fParams)].fType:=t;
                if t=str then Rut.fParams[high(Rut.fParams)].fValue:=l
                else if t=num then Rut.fParams[high(Rut.fParams)].fValue:=k
                else if t=bool then Rut.fParams[high(Rut.fParams)].fValue:=b
                else if t=nVect then Rut.fParams[high(Rut.fParams)].fValue:=VarArrayCreate([0,0],varDouble)
                else if t=sVect then Rut.fParams[high(Rut.fParams)].fValue:=VarArrayCreate([0,0],varOleStr)
                else if t=bVect then Rut.fParams[high(Rut.fParams)].fValue:=VarArrayCreate([0,0],varBoolean);
        end;
        if cIsDefined(Rut, lowercase(v)) then begin
                        RaiseError('Value "'+lowercase(v)+'" already in use!', Rut.fName, p, true, -1);
                        result:=false;
                        exit;
                end;
        setlength(Rut.fParams, length(Rut.fParams)+1);
        Rut.fParams[high(Rut.fParams)].fName:=lowercase(v);
        Rut.fParams[high(Rut.fParams)].fType:=t;
        if t=str then Rut.fParams[high(Rut.fParams)].fValue:=l
                else if t=num then Rut.fParams[high(Rut.fParams)].fValue:=k
                else if t=bool then Rut.fParams[high(Rut.fParams)].fValue:=b
                else if t=nVect then Rut.fParams[high(Rut.fParams)].fValue:=VarArrayCreate([0,0],varDouble)
                else if t=sVect then Rut.fParams[high(Rut.fParams)].fValue:=VarArrayCreate([0,0],varOleStr)
                else if t=bVect then Rut.fParams[high(Rut.fParams)].fValue:=VarArrayCreate([0,0],varBoolean);
 end;
 s:=p;
 if Pos('num ',lowercase(s))=1 then begin
        v:=copy(s,5,length(s));
        t:=num;
 end
 else if Pos('str ',lowercase(s))=1 then begin
        v:=copy(s,5,length(s));
        t:=str;
 end
 else if Pos('bool ',lowercase(s))=1 then begin
        v:=copy(s,6,length(s));
        t:=bool;
 end
 else if Pos('nvect ',lowercase(s))=1 then begin
        v:=copy(s,7,length(s));
        t:=nVect;
 end
 else if Pos('svect ',lowercase(s))=1 then begin
        v:=copy(s,7,length(s));
        t:=sVect;
 end
 else if Pos('bvect ',lowercase(s))=1 then begin
        v:=copy(s,7,length(s));
        t:=bVect;
 end
 else begin
        result:=false;
        exit;
 end;
 while Pos(',',v)<>0 do begin
        s1:=Trim(copy(v,1,Pos(',',v)-1));
        v:=Trim(copy(v,Pos(',',v)+1, length(v)));
        if cIsDefined(Rut, lowercase(s1)) then begin
                        RaiseError('Value "'+lowercase(s1)+'" already in use!', Rut.fName, p, true);
                        result:=false;
                        exit;
                end;
        setlength(Rut.fParams, length(Rut.fParams)+1);
        Rut.fParams[high(Rut.fParams)].fName:=lowercase(s1);
        Rut.fParams[high(Rut.fParams)].fType:=t;
        if t=str then Rut.fParams[high(Rut.fParams)].fValue:=l
                else if t=num then Rut.fParams[high(Rut.fParams)].fValue:=k
                else if t=bool then Rut.fParams[high(Rut.fParams)].fValue:=b
                else if t=nVect then Rut.fParams[high(Rut.fParams)].fValue:=VarArrayCreate([0,0],varDouble)
                else if t=sVect then Rut.fParams[high(Rut.fParams)].fValue:=VarArrayCreate([0,0],varOleStr)
                else if t=bVect then Rut.fParams[high(Rut.fParams)].fValue:=VarArrayCreate([0,0],varBoolean);
 end;
 if cIsDefined(Rut, lowercase(v)) then begin
        RaiseError('Value "'+lowercase(v)+'" already in use!', Rut.fName, p, true);
        result:=false;
        exit;
 end;
 setlength(Rut.fParams, length(Rut.fParams)+1);
 Rut.fParams[high(Rut.fParams)].fName:=lowercase(v);
 Rut.fParams[high(Rut.fParams)].fType:=t;
 if t=str then Rut.fParams[high(Rut.fParams)].fValue:=l
                else if t=num then Rut.fParams[high(Rut.fParams)].fValue:=k
                else if t=bool then Rut.fParams[high(Rut.fParams)].fValue:=b
                else if t=nVect then Rut.fParams[high(Rut.fParams)].fValue:=VarArrayCreate([0,0],varDouble)
                else if t=sVect then Rut.fParams[high(Rut.fParams)].fValue:=VarArrayCreate([0,0],varOleStr)
                else if t=bVect then Rut.fParams[high(Rut.fParams)].fValue:=VarArrayCreate([0,0],varBoolean);
end;

function cAddVariables;
var
 v, s1:string;
 t:TVarType;
 k:double;
 s:string;
 b:boolean;
begin
 k:=0;
 s:='';
 b:=false;
 result:=true;
 if Trim(p)='' then begin
        Rut.fVariables:=nil;
        exit;
 end;
 p:=Trim(p);
 t:=num;
 if Pos('num ',lowercase(p))=1 then begin
        v:=copy(p,5,length(p));
        t:=num;
 end
 else if Pos('str ',lowercase(p))=1 then begin
        v:=copy(p,5,length(p));
        t:=str;
 end
 else if Pos('bool ',lowercase(p))=1 then begin
        v:=copy(p,6,length(p));
        t:=bool;
 end
 else if Pos('nvect ',lowercase(p))=1 then begin
        v:=copy(p,7,length(p));
        t:=nVect;
 end
 else if Pos('svect ',lowercase(p))=1 then begin
        v:=copy(p,7,length(p));
        t:=sVect;
 end
 else if Pos('bvect ',lowercase(p))=1 then begin
        v:=copy(p,7,length(p));
        t:=bVect;
 end;
 while Pos(',',v)<>0 do begin
        s1:=Trim(copy(v,1,Pos(',',v)-1));
        v:=Trim(copy(v,Pos(',',v)+1, length(v)));
        if cIsDefined(Rut, lowercase(s1)) then begin
                RaiseError('Value "'+lowercase(s1)+'" already in use!', Rut.fName, p, true);
                result:=false;
                exit;
        end;
        setlength(Rut.fVariables, length(Rut.fVariables)+1);
        Rut.fVariables[high(Rut.fVariables)].fName:=lowercase(s1);
        Rut.fVariables[high(Rut.fVariables)].fType:=t;
        if t=str then Rut.fVariables[high(Rut.fVariables)].fValue:=s
        else if t=num then Rut.fVariables[high(Rut.fVariables)].fValue:=k
        else if t=bool then Rut.fVariables[high(Rut.fVariables)].fValue:=b
        else if t=nVect then Rut.fVariables[high(Rut.fVariables)].fValue:=VarArrayCreate([0,0],varDouble)
        else if t=bVect then Rut.fVariables[high(Rut.fVariables)].fValue:=VarArrayCreate([0,0],varBoolean)
        else if t=sVect then Rut.fVariables[high(Rut.fVariables)].fValue:=VarArrayCreate([0,0],varOleStr);
 end;
 if cIsDefined(Rut, lowercase(v)) then begin
        RaiseError('Value "'+lowercase(v)+'" already in use!', Rut.fName, p, true);
        result:=false;
        exit;
 end;
 setlength(Rut.fVariables, length(Rut.fVariables)+1);
 Rut.fVariables[high(Rut.fVariables)].fName:=lowercase(v);
 Rut.fVariables[high(Rut.fVariables)].fType:=t;
 if t=str then Rut.fVariables[high(Rut.fVariables)].fValue:=s
        else if t=num then Rut.fVariables[high(Rut.fVariables)].fValue:=k
        else if t=bool then Rut.fVariables[high(Rut.fVariables)].fValue:=b
        else if t=nVect then Rut.fVariables[high(Rut.fVariables)].fValue:=VarArrayCreate([0,0],varDouble)
        else if t=bVect then Rut.fVariables[high(Rut.fVariables)].fValue:=VarArrayCreate([0,0],varBoolean)
        else if t=sVect then Rut.fVariables[high(Rut.fVariables)].fValue:=VarArrayCreate([0,0],varOleStr);
end;

function cCheck(var Rut:TFunc; sEquation: String; var str:string):boolean;
var
 nTemp, nTemp2: Integer;
 sTemp, OpTemp: String;
 vTemp: Variant;
 q,p,i:integer;
 a:integer;
begin
 result:=false;
 str:='';
 vTemp := 0;
 p:=0;
 a:=0;
 sEquation := Trim(sEquation);
 for i:=1 to length(sEquation) do begin
        if sEquation[i]='"' then inc(a);
        if a mod 2=0 then begin
                if sEquation[i]='(' then inc(p);
                if sEquation[i]=')' then dec(p);
                if p<0 then begin
                        result:=true;
                        str:='Invalid parenthesis';
                        exit;
                end;
        end;
 end;
 if p<>0 then begin
        result:=true;
        str:='Invalid parenthesis';
        exit;
 end;
 p:=0;
 for i:=1 to length(sEquation) do if sEquation[i]='"' then inc(p);
 if p mod 2<>0 then begin
        result:=true;
        str:='Unterminated string';
        exit;
 end;
 if sEquation = '' then Exit;

 nTemp := length(sEquation);
 p:=0;
 q:=0;
 repeat
        sTemp := Copy(sEquation, nTemp, 1);
        if sTemp='(' then inc(p);
        if sTemp=')' then dec(p);
        if sTemp='"' then inc(q);

        if (q mod 2=0)and(p=0)and isOperator(sTemp) then begin
                nTemp2 := CodeFind(nTemp-1, sEquation, NonOperator);
                OpTemp := Copy(sEquation, nTemp2+1, nTemp - nTemp2);
                if not cIsOperator(OpTemp) then begin
                        result:=true;
                        str:='Invalid operator: "'+opTemp+'"';
                        exit;
                end;
                if (opTemp='<')or(opTemp='>')or(opTemp='>=')or(opTemp='<=')or(opTemp='=')or(opTemp='==') then begin
                        result:=cCheck(Rut,Copy(sEquation,1,nTemp2),str);
                        if result then exit;
                        result:=cCheck(Rut, Copy(sEquation, nTemp+1, length(sEquation)), str);
                        exit;
                end;
                nTemp:=nTemp2+1;
        end;

        dec(nTemp);
 until nTemp<1;

 nTemp := length(sEquation);
 p:=0;
 q:=0;
 repeat
        sTemp := Copy(sEquation, nTemp, 1);
        if sTemp='(' then inc(p);
        if sTemp=')' then dec(p);
        if sTemp='"' then inc(q);
        if (q mod 2=0)and(p=0)and isOperator(sTemp) then begin
                nTemp2 := CodeFind(nTemp-1, sEquation, NonOperator);
                OpTemp := Copy(sEquation, nTemp2+1, nTemp - nTemp2);
                if not cIsOperator(OpTemp) then begin
                        result:=true;
                        str:='Invalid operator: "'+opTemp+'"';
                        exit;
                end;
                if (opTemp='+')or(opTemp='-')or(opTemp='^')or(opTemp='|') then begin
                        result:=cCheck(Rut,Copy(sEquation,1,nTemp2),str);
                        if result then exit;
                        result:=cCheck(Rut, Copy(sEquation, nTemp+1, length(sEquation)), str);
                        exit;
                end;
                nTemp:=nTemp2+1;
        end;
        dec(nTemp);
 until nTemp<1;

 nTemp:=length(sEquation);
 p:=0;
 q:=0;
 repeat
        sTemp := Copy(sEquation, nTemp, 1);
        if sTemp='(' then inc(p);
        if sTemp=')' then dec(p);
        if sTemp='"' then inc(q);
        if (q mod 2=0)and(p=0)and isOperator(sTemp) then begin
                nTemp2 := CodeFind(nTemp-1, sEquation, NonOperator);
                OpTemp := Copy(sEquation, nTemp2+1, nTemp - nTemp2);
                if not cIsOperator(OpTemp) then begin
                        result:=true;
                        str:='Invalid operator: "'+opTemp+'"';
                        exit;
                end;
                if (opTemp='*')or(opTemp='/')or(opTemp='\')or(opTemp='%')or(opTemp='>>')or(opTemp='<<')or(opTemp='&') then begin
                        result:=cCheck(Rut,Copy(sEquation,1,nTemp2),str);
                        if result then exit;
                        result:=cCheck(Rut, Copy(sEquation, nTemp+1, length(sEquation)), str);
                        exit;
                end;
                nTemp:=nTemp2+1;
        end;

        dec(nTemp);
 until nTemp<1;

 nTemp := length(sEquation);
 p:=0;
 q:=0;
 repeat
        sTemp := Copy(sEquation, nTemp, 1);
        if sTemp='(' then inc(p);
        if sTemp=')' then dec(p);
        if sTemp='"' then inc(q);
        if (q mod 2=0)and(p=0)and isOperator(sTemp) then begin
                nTemp2 := CodeFind(nTemp-1, sEquation, NonOperator);
                OpTemp := Copy(sEquation, nTemp2+1, nTemp - nTemp2);
                if not cIsOperator(OpTemp) then begin
                        result:=true;
                        str:='Invalid operator: "'+opTemp+'"';
                        exit;
                end;
                if (opTemp='!') then begin
                        result:=cCheck(Rut,Copy(sEquation,1,nTemp2),str);
                        if result then exit;
                        result:=cCheck(Rut, Copy(sEquation, nTemp+1, length(sEquation)), str);
                        exit;
                end;
                nTemp:=nTemp2+1;
        end;

        dec(nTemp);
 until nTemp<1;

 if sEquation[1]='(' then begin
        result:=cCheck(Rut,Copy(sEquation,2,length(sEquation)-2),str);
        exit;
 end;


 if isNumber(sEquation[1])and(lowercase(sEquation[1])<>'e') then begin
        nTemp := length(sEquation);
        repeat
                sTemp := Copy(sEquation, nTemp, 1);
                dec(nTemp);
                if not isNumber(sTemp) then begin
                        result:=true;
                        str:='Invalid number in expresion: "'+sEquation+'"';
                        exit;
                end;
        until nTemp<1;
        exit;
 end;

 if sEquation[1]=#34 then begin
        if sEquation[length(sEquation)]<>#34 then begin
                result:=true;
                str:='Unterminated string: "'+sEquation+'"';
                exit;
        end;
        exit;
 end;

 nTemp:=Pos('(',sEquation);
 if nTemp<>0 then begin
        result:=not cIsFunction(Rut,sEquation);
        if result then str:='Unknown function: "'+sEquation+'"';
        exit;
 end
 else begin
        result:=not cIsVar(Rut,sEquation);
        if result=true then result:=not cIsFunction(Rut,sEquation);
        if result then str:='Unknown variable: "'+sEquation+'"';
        exit;
 end;
end;

function cIsDefined;
begin
 result:=cIsFunction(Rut, name);
 if result then exit;
 result:=cIsVar(Rut, name);
 if result then exit;
end;

function cIsVar;
var
 i:integer;
begin
 Result:= false;
 for i:=0 to high(fGlobalVars) do
        if lowercase(fGlobalVars[i].fName)=lowercase(name) then begin
                result:=true;
                exit;
        end;

 for i:=0 to high(rut.fParams) do
        if lowercase(rut.fParams[i].fName)=lowercase(name) then begin
                result:=true;
                exit;
        end;
 for i:=0 to high(rut.fVariables) do
        if lowercase(rut.fVariables[i].fName)=lowercase(name) then begin
                result:=true;
                exit;
        end;

 if lowercase(name)='result' then begin
        result:=true;
        exit;
 end;
end;

function cIsFunction;
var
  sName, s: String;
  nTemp1, nTemp2: Integer;
  i:integer;
begin
 result:=false;
 if fBindedScript=0 then exit;
 s:=name;
 nTemp1 := Pos('(',s);
 nTemp2 := CodeInStr(nTemp1+1, s, ')');
 s := Copy(s, 1, nTemp2-1);
 if s='' then s:=name;
 sName  := lowercase(Copy(s, 1, nTemp1-1));
 if sName='' then sName:=name;
 Result:= false;
 sname:=lowercase(sName);
 if ISLFunctions.cIsFunction(Rut, Sname) then begin
        result:=true;
        exit;
 end;

 for i:=0 to high(fScripts[fBindedScript-1].fFunc) do
        if lowercase(fScripts[fBindedScript-1].fFunc[i].fName)=sName then result:=true;
end;

function cCheckFunction;
var
  sName, sTemp, sTemp2, s, DaCalli: String;
  nTemp1, nTemp2, i, li: Integer;
  data: TArray;
  pCount:integer;
  p:integer;
begin
 result:=cIsFunction(Rut, DaCall);
 if result=false then exit;
 DaCalli:=DaCall;
 nTemp1 := Pos('(',DaCall);
 nTemp2 := CodeInStr(nTemp1+1, DaCall, ')');
 DaCall := Copy(DaCall, 1, nTemp2-1);
 sName  := Copy(DaCall, 1, nTemp1-1);
 sTemp  := Copy(DaCall, nTemp1+1, length(DaCall));
 if sName='' then begin
        DaCall:=DaCalli;
        sName:=DaCalli;
 end;

 data := nil;
 pCount:=0;
 p:=0;
 sTemp2:=sTemp;
 li:=1;
 for i:=1 to length(sTemp) do begin
        if sTemp[i]='(' then inc(p);
        if sTemp[i]=')' then dec(p);
        if (sTemp[i]=',')and(p=0) then begin
                s:=copy(sTemp,li,i-li);
                li:=i+1;
                sTemp2:=copy(sTemp,i+1, length(sTemp));
                setlength(data, length(data)+1);
                inc(pCount);
                if result then result:=not cCheck(Rut, s, str);
                if not result then exit;
        end;
 end;
 if sTemp2<>'' then begin
        setlength(data, length(data)+1);
        inc(PCount);
        if result then result:=not cCheck(Rut, sTemp2, str);
        if not result then exit;
 end;
 data:=nil;

 for i:=0 to high(fScripts[fBindedScript-1].fFunc) do
        if (fScripts[fBindedScript-1].fFunc[i].fName=sName) then begin
                if pCount<>length(fScripts[fBindedScript-1].fFunc[i].fParams) then begin
                        str:='Different parameter count!';
                        result:=false;
                        exit;
                end;
                result:=true;
                exit;
        end;
 Result:=ISLFunctions.cCheckFunction(Rut, sName, pCount, str);
end;


///////////////////////////Other Rutines////////////////////////////////////////
///////////////////////////Other Rutines////////////////////////////////////////
///////////////////////////Other Rutines////////////////////////////////////////
function LoadCompiled(filename:string):boolean;
var
 f:file;
 i,s,j,k:integer;
 c:char;
 int:double;
 boo:boolean;
 str:string;
begin
 result:=false;
 if fBindedScript=0 then exit;
 int:=0;
 boo:=false;
 str:='';
 result:=true;
 if not fileexists(filename) then begin
        result:=false;
        exit;
 end;
 try
        assign(f,filename);
        reset(f,1);
 except
        result:=false;
        exit;
 end;
 Finalize(fScripts[fBindedScript-1].fFunc);
 fScripts[fBindedScript-1].fFunc:=nil;
 fScripts[fBindedScript-1].fScript:=nil;
  //read the number of functions
 blockread(f,i,sizeof(i));
 setlength(fScripts[fBindedScript-1].fFunc,i);

 for i:=0 to high(fScripts[fBindedScript-1].fFunc) do begin
        //read the name
        blockread(f,s,sizeof(s));
        blockread(f,fScripts[fBindedScript-1].fFunc[i].fName,s);

        //read the result
        blockread(f,s,sizeof(s));
        blockread(f,fScripts[fBindedScript-1].fFunc[i].fResult.fName,s);
        blockread(f,fScripts[fBindedScript-1].fFunc[i].fResult.fType,1);
        if (fScripts[fBindedScript-1].fFunc[i].fResult.fType=num) then fScripts[fBindedScript-1].fFunc[i].fResult.fValue:=int;
        if (fScripts[fBindedScript-1].fFunc[i].fResult.fType=bool) then fScripts[fBindedScript-1].fFunc[i].fResult.fValue:=boo;
        if (fScripts[fBindedScript-1].fFunc[i].fResult.fType=ISLTypes.str) then fScripts[fBindedScript-1].fFunc[i].fResult.fValue:=str;
        if (fScripts[fBindedScript-1].fFunc[i].fResult.fType=nVect) then fScripts[fBindedScript-1].fFunc[i].fResult.fValue:=varArrayCreate([0,0],varDouble);
        if (fScripts[fBindedScript-1].fFunc[i].fResult.fType=bVect) then fScripts[fBindedScript-1].fFunc[i].fResult.fValue:=varArrayCreate([0,0],varBoolean);
        if (fScripts[fBindedScript-1].fFunc[i].fResult.fType=sVect) then fScripts[fBindedScript-1].fFunc[i].fResult.fValue:=varArrayCreate([0,0],varOleStr);

        //read the number of parameters
        blockread(f,s,sizeof(s));
        setlength(fScripts[fBindedScript-1].fFunc[i].fParams,s);
        //read the parameters
        for j:=0 to s-1 do begin
                blockRead(f,s,sizeof(s));
                blockRead(f,fScripts[fBindedScript-1].fFunc[i].fParams[j].fName,s);
                blockRead(f,fScripts[fBindedScript-1].fFunc[i].fParams[j].fType,1);
                if (fScripts[fBindedScript-1].fFunc[i].fParams[j].fType=num) then fScripts[fBindedScript-1].fFunc[i].fParams[j].fValue:=int;
                if (fScripts[fBindedScript-1].fFunc[i].fParams[j].fType=bool) then fScripts[fBindedScript-1].fFunc[i].fParams[j].fValue:=boo;
                if (fScripts[fBindedScript-1].fFunc[i].fParams[j].fType=ISLTypes.str) then fScripts[fBindedScript-1].fFunc[i].fParams[j].fValue:=str;
                if (fScripts[fBindedScript-1].fFunc[i].fParams[j].fType=nVect) then fScripts[fBindedScript-1].fFunc[i].fParams[j].fValue:=varArrayCreate([0,0],varDouble);
                if (fScripts[fBindedScript-1].fFunc[i].fParams[j].fType=sVect) then fScripts[fBindedScript-1].fFunc[i].fParams[j].fValue:=varArrayCreate([0,0],varOleStr);
                if (fScripts[fBindedScript-1].fFunc[i].fParams[j].fType=bVect) then fScripts[fBindedScript-1].fFunc[i].fParams[j].fValue:=varArrayCreate([0,0],varBoolean);
        end;

        //read the number of variables
        blockread(f,s,sizeof(s));
        setlength(fScripts[fBindedScript-1].fFunc[i].fVariables,s);

        //read the variables
        for j:=0 to s-1 do begin
                blockRead(f,s,sizeof(s));
                blockRead(f,fScripts[fBindedScript-1].fFunc[i].fVariables[j].fName,s);
                blockRead(f,fScripts[fBindedScript-1].fFunc[i].fVariables[j].fType,1);
                if (fScripts[fBindedScript-1].fFunc[i].fVariables[j].fType=num) then fScripts[fBindedScript-1].fFunc[i].fVariables[j].fValue:=int;
                if (fScripts[fBindedScript-1].fFunc[i].fVariables[j].fType=bool) then fScripts[fBindedScript-1].fFunc[i].fVariables[j].fValue:=boo;
                if (fScripts[fBindedScript-1].fFunc[i].fVariables[j].fType=ISLTypes.str) then fScripts[fBindedScript-1].fFunc[i].fVariables[j].fValue:=str;
                if (fScripts[fBindedScript-1].fFunc[i].fVariables[j].fType=nVect) then fScripts[fBindedScript-1].fFunc[i].fVariables[j].fValue:=varArrayCreate([0,0],varDouble);
                if (fScripts[fBindedScript-1].fFunc[i].fVariables[j].fType=sVect) then fScripts[fBindedScript-1].fFunc[i].fVariables[j].fValue:=varArrayCreate([0,0],varOleStr);
                if (fScripts[fBindedScript-1].fFunc[i].fVariables[j].fType=bVect) then fScripts[fBindedScript-1].fFunc[i].fVariables[j].fValue:=varArrayCreate([0,0],varBoolean);
        end;
        //read the number of script Lines
        blockRead(f,s,sizeof(s));
        setlength(fScripts[fBindedScript-1].fFunc[i].fScript,s);
        //read the script lines
        for j:=0 to s-1 do begin
                blockRead(f,s,sizeof(s));
                fScripts[fBindedScript-1].fFunc[i].fScript[j]:='';
                for k:=1 to s do begin
                        blockread(f,c,sizeof(c));
                        fScripts[fBindedScript-1].fFunc[i].fScript[j]:=fScripts[fBindedScript-1].fFunc[i].fScript[j]+c;
                end;
        end;
 end;

 closefile(f);
end;

function SaveCompiled(filename:string):boolean;
var
 f:file;
 i,j,k:integer;
 s:integer;
 c:char;
begin
 result:=false;
 if fBindedScript=0 then exit;
 result:=true;
 fScripts[fBindedScript-1].fCompileError:='No Error';
 Compile;
 if fScripts[fBindedScript-1].fCompileError<>'No Error' then begin
        result:=false;
        exit;
 end;
 try
        assign(f,filename);
        rewrite(f,1);
 except
        result:=false;
        exit;
 end;
 //write the number of functions
 i:=length(fScripts[fBindedScript-1].fFunc);
 blockwrite(f,i,sizeof(i));
 for i:=0 to high(fScripts[fBindedScript-1].fFunc) do begin

        //write the name
        s:=length(fScripts[fBindedScript-1].fFunc[i].fName)+1;
        blockWrite(f,s,sizeof(s));
        blockWrite(f,fScripts[fBindedScript-1].fFunc[i].fName,s);

        //write the result
        s:=length(fScripts[fBindedScript-1].fFunc[i].fResult.fName)+1;
        blockWrite(f,s,sizeof(s));
        blockWrite(f,fScripts[fBindedScript-1].fFunc[i].fResult.fName,s);
        blockWrite(f,fScripts[fBindedScript-1].fFunc[i].fResult.fType,1);

        //write the number of parameters
        s:=length(fScripts[fBindedScript-1].fFunc[i].fParams);
        blockWrite(f,s,sizeof(s));
        //write the parameters
        for j:=0 to high(fScripts[fBindedScript-1].fFunc[i].fParams) do begin
                s:=length(fScripts[fBindedScript-1].fFunc[i].fParams[j].fName)+1;
                blockWrite(f,s,sizeof(s));
                blockWrite(f,fScripts[fBindedScript-1].fFunc[i].fParams[j].fName,s);
                s:=sizeof(fScripts[fBindedScript-1].fFunc[i].fParams[j].fType);
                blockWrite(f,fScripts[fBindedScript-1].fFunc[i].fParams[j].fType,s);
        end;
        
        //write the number of variables
        s:=length(fScripts[fBindedScript-1].fFunc[i].fVariables);
        blockWrite(f,s,sizeof(s));
        //write the variables
        for j:=0 to s-1 do begin
                s:=length(fScripts[fBindedScript-1].fFunc[i].fVariables[j].fName)+1;
                blockWrite(f,s,sizeof(s));
                blockWrite(f,fScripts[fBindedScript-1].fFunc[i].fVariables[j].fName,s);
                s:=sizeof(fScripts[fBindedScript-1].fFunc[i].fVariables[j].fType);
                blockWrite(f,fScripts[fBindedScript-1].fFunc[i].fVariables[j].fType,s);
        end;
        //write the number of script Lines
        s:=length(fScripts[fBindedScript-1].fFunc[i].fScript);
        blockWrite(f,s,sizeof(s));
        //write the script lines
        for j:=0 to s-1 do begin
                s:=length(fScripts[fBindedScript-1].fFunc[i].fScript[j]);
                blockWrite(f,s,sizeof(s));
                for k:=1 to s do begin
                        c:=fScripts[fBindedScript-1].fFunc[i].fScript[j][k];
                        blockWrite(f,c,sizeof(c));
                end;
        end;
 end;
 closefile(f);
end;

procedure setGlobalVars;
begin
 ClearGlobalVars;
 AddGlobalVar('euler', num, exp(1));
 AddGlobalVar('pi', num, pi);
 AddGlobalVar('true', bool, true);
 AddGlobalVar('false', bool, false);
end;

procedure RaiseError;
var
 str:string;
begin
 if fBindedScript=0 then exit;
 if compilation then begin
        Finalize(fScripts[fBindedScript-1].fFunc);
        fScripts[fBindedScript-1].fFunc:=nil;
        fScripts[fBindedScript-1].fScript:=nil;
 end;
 if RutName='' then str:='Error: '+s+' in script'
               else str:='Error: '+s+' in function "'+RutName+'"';
 if (LineNr<>-1) and (Line<>'') then Line:='('+inttostr(LineNr+1)+') "'+Line+'"';
 if Line<>'' then str:=str+' on line '+Line;
 str:=str+'.';
 fScripts[fBindedScript-1].fCompileError:=str;
 fScripts[fBindedScript-1].fResult:=0;
end;

function GlobalVarCount:integer;
begin
 result:=length(fGlobalVars);
end;

procedure ClearGlobalVars;
begin
 fGlobalVars:=nil;
end;

procedure AddGlobalVar(Name:string; _Type:TVarType; value:Variant);
var
 d:double;
 f:TFunc;
begin
 if cIsDefined(f, Name) then begin
        RaiseError('Value "'+Name+'" already in use!','','',false);
        exit;
 end;
 setlength(fGlobalVars,length(fGlobalVars)+1);
 fGlobalVars[high(fGlobalVars)].fName:=lowercase(Name);
 fGlobalVars[high(fGlobalVars)].fType:=_Type;

 if _Type=num then begin
        d:=value;
        fGlobalVars[high(fGlobalVars)].fValue:=d;
 end
 else if _Type=str then fGlobalVars[high(fGlobalVars)].fValue:=string(value)
 else if _Type=bool then fGlobalVars[high(fGlobalVars)].fValue:=boolean(value)
 else fGlobalVars[high(fGlobalVars)].fValue:=value;
end;

function getGlobalVar(index:integer):TVariable;
begin
 result.fName:='';
 result.fValue:=0;
 if index<0 then exit;
 if index>high(fGlobalVars) then exit;
 result:=fGlobalVars[index];
end;

procedure setGlobalVar(index:integer; value:Variant);
var
 f:TFunc;
begin
 if index<0 then exit;
 if index>high(fGlobalVars) then exit;
 if cIsDefined(f, value.fName) then begin
        RaiseError('Value "'+value.fName+'" already in use!','','', false);
        exit;
 end;
 if VarType(Value)<>fGlobalVars[index].fType then begin
        RaiseError('Invalid types!','','', false);
        exit;
 end;
 fGlobalVars[index].fValue:=value;
end;

procedure Create;
begin
 DecimalSeparator:='.';
 ISLFunctions.clear;
 SetGlobalVars;
 fBindedScript:=0;
 fScripts:=nil;
end;

 procedure destroy;
begin
 ISLFunctions.Clear;
 fScripts:=nil;
end;

function LoadFromPointer;
var
 l:TStringList;
 s:string;
 k,p2,p1,i:integer;
 str:TStream;
 o:PChar;
begin
 result:=false;
 if fBindedScript=0 then exit;

 str:=TMemoryStream.Create;
 o:=p;
 for i:=0 to size-1 do begin
        str.Write(o^, 1);
        inc(o);
 end;
 str.Seek(0,soFromBeginning);
 l:=TStringList.Create;
 l.LoadFromStream(str);
 fScripts[fBindedScript-1].fScript:=nil;
 result:=true;
 for k:=0 to l.Count-1 do begin
        s:=l.Strings[k];
        //elimina comentariile si liniile goale
        s:=Trim(s);
        p2:=0;
        for i:=1 to length(s) do begin
                if s[i]='"' then inc(p2);
                if (p2 mod 2=0)and(s[i]='#') then begin
                        s:=Trim(Copy(s,1,i-1));
                        break;
                end;
        end;
        if s<>'' then begin
                i:=1;
                p2:=0;
                p1:=0;
                while i<=length(s) do begin
                        if s[i]='"' then inc(p2);
                        if p2 mod 2=0 then begin
                                if s[i]='(' then inc(p1);
                                if s[i]=')' then dec(p1);
                        end;
                        if (p1=0)and(p2 mod 2=0)and(s[i]=';') then begin
                                setlength(fScripts[fBindedScript-1].fScript, length(fScripts[fBindedScript-1].fScript)+1);
                                if Trim(Copy(s,1,i-1))<>'' then
                                        fScripts[fBindedScript-1].fScript[high(fScripts[fBindedScript-1].fScript)]:=trim(Copy(s,1,i-1));
                                s:=trim(Copy(s,i+1,length(s)));
                                i:=0;
                        end;
                        inc(i);
                end;
                if s<>'' then begin
                        setlength(fScripts[fBindedScript-1].fScript, length(fScripts[fBindedScript-1].fScript)+1);
                        fScripts[fBindedScript-1].fScript[high(fScripts[fBindedScript-1].fScript)]:=s;
                end;
        end;
 end;
 l.Free;
end;

function Load;
var
 str:TStream;
 s:TMemoryStream;
begin
 if not fileexists(filename) then begin
        result:=false;
        exit;
 end;
 str:=TFileStream.Create(filename, fmOpenRead);
 s:=TMemoryStream.Create;
 s.LoadFromStream(str);
 str.Free;
 result:=LoadFromPointer(s.Memory, s.Size);
 s.Free;
end;

///////////////////////////////Runtime Rutines//////////////////////////////////
///////////////////////////////Runtime Rutines//////////////////////////////////
///////////////////////////////Runtime Rutines//////////////////////////////////
procedure ExecuteScript(fFunction:string);
begin
 if fBindedScript=0 then exit;
 fScripts[fBindedScript-1].fResult:=0;
 fScripts[fBindedScript-1].fCompileError:='No Error';
 fScripts[fBindedScript-1].fToRunFunction:=fFunction;
 rExecute;
end;

procedure ExecuteScript(fname:string; Args:TArray);
var
 i:integer;
begin
 if fBindedScript=0 then exit;
 fScripts[fBindedScript-1].fRunError:='No Error';
 for i:=0 to high(fScripts[fBindedScript-1].fFunc) do begin
        fScripts[fBindedScript-1].fFunc[i].fIf:=nil;
        fScripts[fBindedScript-1].fFunc[i].fWhile:=nil;
        fScripts[fBindedScript-1].fFunc[i].last:=nil;
        fScripts[fBindedScript-1].fFunc[i].fLevel:=0;
 end;
 for i:=0 to high(fScripts[fBindedScript-1].fFunc) do
        if lowercase(fScripts[fBindedScript-1].fFunc[i].fName)=lowercase(fname) then begin
                if not rCopyValues(Args,fScripts[fBindedScript-1].fFunc[i].fParams) then begin
                        fScripts[fBindedScript-1].fCompileError:=pchar('Incorrect parameters');
                        exit;
                end;
                fScripts[fBindedScript-1].fFunc[i].fLevel:=1;
                rCallFunc(fScripts[fBindedScript-1].fFunc[i]);
                exit;
        end;
 fScripts[fBindedScript-1].fCompileError:=pchar('Function '+fName+' is undefined');
end;

procedure rExecute;
var
 i:integer;
 f, p:string;
begin
 if fBindedScript=0 then exit;
 if Pos('(',fScripts[fBindedScript-1].fToRunFunction)=0 then begin
        f:=Trim(lowercase(fScripts[fBindedScript-1].fToRunFunction));
        p:='';
 end
 else begin
        f:=Trim(lowercase(copy(fScripts[fBindedScript-1].fToRunFunction,1,Pos('(',fScripts[fBindedScript-1].fToRunFunction)-1)));
        p:=Trim(lowercase(copy(fScripts[fBindedScript-1].fToRunFunction,Pos('(',fScripts[fBindedScript-1].fToRunFunction)+1, Pos(')',fScripts[fBindedScript-1].fToRunFunction)-1-Pos('(',fScripts[fBindedScript-1].fToRunFunction))));
 end;
 for i:=0 to high(fScripts[fBindedScript-1].fFunc) do begin
        fScripts[fBindedScript-1].fFunc[i].fIf:=nil;
        fScripts[fBindedScript-1].fFunc[i].fWhile:=nil;
        fScripts[fBindedScript-1].fFunc[i].last:=nil;
        fScripts[fBindedScript-1].fFunc[i].fLevel:=0;
 end;
 for i:=0 to high(fScripts[fBindedScript-1].fFunc) do
        if lowercase(fScripts[fBindedScript-1].fFunc[i].fName)=f then begin
                if not rSetParams(p,fScripts[fBindedScript-1].fFunc[i]) then begin
                        exit;
                end;
                fScripts[fBindedScript-1].fFunc[i].fLevel:=1;
                rCallFunc(fScripts[fBindedScript-1].fFunc[i]);
                exit;
        end;
 fScripts[fBindedScript-1].fCompileError:=pchar('Function '+f+' is undefined');
end;

function rCallFunc;
var
 fCurrentLine:integer;
 Line:string;
 i,j:integer;
 s:string;
 skipWhile, skipAll:boolean;
 ok:boolean;
 b:Variant;
 a:Variant;
 maxLine:integer;
 lastLast, lastIf, lastWhile:integer;
 skipWhileIndex:integer;
label EndLine, ExitProc;
begin
 result:=false;
 if fBindedScript=0 then exit;
 fScripts[fBindedScript-1].fRunError:='';
 result:=true;
 skipWhile:=false;
 sKipAll:=false;
 fCurrentLine:=0;
 LastLast:=Length(Rut.Last);
 LastIf:=Length(Rut.fIf);
 LastWhile:=Length(Rut.fWhile);
 SkipWhileIndex:=-1;
 
 if Rut.fLevel=256 then begin
        fScripts[fBindedScript-1].fRunError:='Stack Overflow';
        exit;
 end;

 maxLine:=high(Rut.fScript);
 while (fCurrentLine<=maxLine) do begin
        Line:=Rut.fScript[fCurrentLine];

        if lowercase(Line)='end' then begin
                if (Rut.Last[high(Rut.Last)]=2) then begin
                        if skipWhile or SkipAll then begin
                                if skipWhile and (high(Rut.Last)=SkipWhileIndex) then skipWhile:=false;
                                setlength(Rut.Last, high(Rut.Last));
                        end
                        else begin
                                b:=rSolve(Rut, Rut.fWhile[high(Rut.fWhile)].condition);
                                if fScripts[fBindedScript-1].fRunError<>'' then goto ExitProc;
                                if b then fCurrentLine:=Rut.fWhile[high(Rut.fWhile)].line
                                     else begin
                                        setlength(Rut.fWhile, high(Rut.fWhile));
                                        setlength(Rut.Last,high(Rut.Last));
                                     end;
                        end;
                        goto endLine;
                end
                else if Rut.last[high(Rut.Last)]=1 then begin
                        setlength(Rut.Last, high(Rut.Last));
                        setlength(Rut.fIf, high(Rut.fIf));
                        goto EndLine;
                end;
        end;

        if skipWhile or SkipAll then begin
                i := Pos(' ',Line);
                if i <> 0 then begin
                        s := LowerCase(Copy(Line, 1, i - 1));
                        if s = 'if' then begin
                                setlength(Rut.Last, length(Rut.Last)+1);
                                Rut.Last[high(Rut.Last)]:=1;
                                setlength(Rut.fIf, length(Rut.fIf)+1);
                                Rut.fIf[high(Rut.fIf)]:=1;
                        end;
                        if s='while' then begin
                                setlength(Rut.Last, length(Rut.Last)+1);
                                Rut.Last[high(Rut.Last)]:=2;
                        end;
                end;
                goto EndLine;
        end;

        if length(Rut.fIf) > 0 then begin
                if Rut.fIf[high(Rut.fIf)] = 1 then
                        if copy(Line, 1, 2) = 'if' then begin
                                setlength(Rut.fIf, length(Rut.fIf)+1);
                                Rut.fIf[high(Rut.fIf)]:=2;
								 setlength(Rut.Last, length(Rut.Last)+1);
								Rut.Last[high(Rut.Last)]:=1;
                                goto EndLine;
                        end;

                if LowerCase(Line) = 'else' then
                        if Rut.fIf[high(Rut.fIf)] <> 2 then
                                if Rut.fIf[high(Rut.fIf)] = 1 then begin
                                        Rut.fIf[high(Rut.fIf)]:=3;
                                        goto EndLine;
                                end
                                else begin
                                        Rut.fIf[high(Rut.fIf)]:=1;
                                end;

                if (Rut.fIf[high(Rut.fIf)] = 1) or (Rut.fIf[high(Rut.fIf)] = 2) then goto EndLine;
        end;

        if lowercase(Line)='exit' then begin
                ok:=false;
                for i:=high(Rut.last) downto Lastlast do
                        if Rut.last[i]=2 then begin
                                ok:=true;
                                SkipWhileIndex:=i;
                                break;
                        end;
                if ok then begin
                        skipwhile:=true;
                        goto EndLine;
                end
                else begin
                        skipAll:=true;
                        goto EndLine;
                end;
        end;

        i := Pos(' ',Line);
        if i <> 0 then begin
                s := LowerCase(Copy(Line, 1, i - 1));
                if s = 'if' then begin
                        setlength(Rut.Last, length(Rut.Last)+1);
                        Rut.Last[high(Rut.Last)]:=1;
                        b:=rSolve(Rut, Copy(Line, i + 1, length(Line)));
                        if fScripts[fBindedScript-1].fRunError<>'' then goto ExitProc;
                        if not b then begin
                                setlength(Rut.fIf, length(Rut.fIf)+1);
                                Rut.fIf[high(Rut.fIf)]:=1;
                        end
                        else begin
                                setlength(Rut.fIf, length(Rut.fIf)+1);
                                Rut.fIf[high(Rut.fIf)]:=3;
                        end;
                        GoTo EndLine;
                end
                else if s='while' then begin
                        setlength(Rut.Last, length(Rut.Last)+1);
                        Rut.Last[high(Rut.Last)]:=2;
                        b:=rSolve(Rut, Copy(Line, i + 1, length(Line)));
                        if fScripts[fBindedScript-1].fRunError<>'' then goto ExitProc;
                        if b then begin
                                setlength(Rut.fWhile,length(Rut.fWhile)+1);
                                Rut.fWhile[high(Rut.fWhile)].line:=fCurrentLine;
                                Rut.fWhile[high(Rut.fWhile)].condition:=Copy(Line, i + 1, length(Line));
                                skipWhile:=false;
                        end
                        else skipWhile:=true;
                        goto EndLine;
                end;
        end;

        i:=CodeInStr(1,Line,':=');
        j:=CodeInStr(1,Line,'(');
        if (i<>0)and((j=0)or(i<j)) then begin
                a:=rSolve(Rut, Copy(Line, i + 2, length(line)));
                if fScripts[fBindedScript-1].fRunError<>'' then goto ExitProc;
                rChangeVar(Rut, Trim(Copy(Line, 1, i - 1)), a);
                if fScripts[fBindedScript-1].fRunError<>'' then goto ExitProc;
                goto EndLine;
        end;
        if (j<>0)and((i=0)or(j<i)) then begin
                rDoFunction(Rut, Line);
                if fScripts[fBindedScript-1].fRunError<>'' then goto ExitProc;
                goto EndLine;
        end;
        rDoFunction(Rut, Line);
        if fScripts[fBindedScript-1].fRunError<>'' then goto ExitProc;
        EndLine: inc(fCurrentLine);
 end;
 fScripts[fBindedScript-1].fResult:=Rut.fResult.fValue;
 setlength(Rut.Last,lastlast);
 setlength(Rut.fIf,lastIf);
 setlength(Rut.fWhile,lastWhile);
 ExitProc:
  if fScripts[fBindedScript-1].fRunError<>'' then begin
        Rut.fIf:=nil;
        Rut.fWhile:=nil;
        Rut.last:=nil;
        RaiseError(fScripts[fBindedScript-1].fRunError,Rut.fName,Line, false);
        exit;
  end;
end;

function rSetParams;
var
 s:string;
 i:integer;
 V:array of Variant;
begin
 result:=false;
 if fBindedScript=0 then exit;
 while Pos(',',p)<>0 do begin
        s:=Trim(copy(p,1,Pos(',',p)-1));
        p:=Trim(copy(p,Pos(',',p)+1, length(p)));
        setlength(V, length(V)+1);
        V[high(V)]:=s;
 end;
 if p<>'' then begin
        setlength(V, length(V)+1);
        V[high(V)]:=p;
 end;
 if length(V)=length(Rut.fParams) then begin
        result:=true;
        for i:=0 to high(v) do
                try
                        if Rut.fParams[i].fType=num then Rut.fParams[i].fValue:=strtofloat(NormalizeFloat((V[i])))
                        else if Rut.fParams[i].fType=str then Rut.fParams[i].fValue:=V[i]
                        else if Rut.fParams[i].fType=bool then Rut.fParams[i].fValue:=boolean(V[i])
                        else Rut.fParams[i].fValue:=V[i];
                except
                        fScripts[fBindedScript-1].fCompileError:=pchar('Incorrect parameters!');
                        result:=false;
                end;
 end
 else begin
        fScripts[fBindedScript-1].fCompileError:=pchar('Incorrect parameters!');
        result:=false;
 end;
end;

function rSolve;
var
 nTemp, nTemp2: Integer;
 OpTemp, sTemp: String;
 vTemp: Variant;
 p:integer;
 q:integer;
begin
 if fBindedScript=0 then exit;
 fScripts[fBindedScript-1].fRunError:='';
 Result := 0;
 vTemp := 0;
 sEquation := Trim(sEquation);
 if sEquation = '' then Exit;

 nTemp := CodeFind(length(sEquation),sEquation,Operator);
 if nTemp<>0 then begin
        //Priority 3 Operands
        p:=0;
        q:=0;
        repeat
                sTemp := Copy(sEquation, nTemp, 1);
                if sTemp='(' then inc(p);
                if sTemp=')' then dec(p);
                if sTemp='"' then inc(q);
                if (q mod 2=0)and(p=0)and isOperator(sTemp) then begin
                        nTemp2 := CodeFind(nTemp-1, sEquation, NonOperator);
                        OpTemp := Copy(sEquation, nTemp2+1, nTemp - nTemp2);

                        if (opTemp='<')or(opTemp='<>')or(opTemp='>')or(opTemp='>=')or(opTemp='<=')or(opTemp='=')or(opTemp='==') then begin
                                vTemp:=rSolve(Rut, Copy(sEquation,1,nTemp2));
                                if fScripts[fBindedScript-1].fRunError<>'' then exit;
                                result:=rSolve(Rut, Copy(sEquation, nTemp+1, length(sEquation)));
                                if fScripts[fBindedScript-1].fRunError<>'' then exit;
                                Result := rDoOperator(vTemp, Result, OpTemp);
                                exit;
                        end;
                        nTemp:=nTemp2+1;
                end;
                dec(nTemp);
        until nTemp<1;

        nTemp := CodeFind(length(sEquation),sEquation,Operator);
        //Priority 2 Operands
        p:=0;
        q:=0;
        repeat
                sTemp := Copy(sEquation, nTemp, 1);
                if sTemp='(' then inc(p);
                if sTemp=')' then dec(p);
                if sTemp='"' then inc(q);
                if (q mod 2=0)and(p=0)and isOperator(sTemp) then begin
                        nTemp2 := CodeFind(nTemp-1, sEquation, NonOperator);
                        OpTemp := Copy(sEquation, nTemp2+1, nTemp - nTemp2);
                        if (opTemp='+')or(opTemp='-')or(opTemp='^')or(opTemp='|') then begin
                                vTemp:=rSolve(Rut, Copy(sEquation,1,nTemp2));
                                if fScripts[fBindedScript-1].fRunError<>'' then exit;
                                result:=rSolve(Rut, Copy(sEquation, nTemp+1, length(sEquation)));
                                if fScripts[fBindedScript-1].fRunError<>'' then exit;
                                Result := rDoOperator(vTemp, Result, OpTemp);
                                exit;
                        end;
                        nTemp:=nTemp2+1;
                end;
                dec(nTemp);
        until nTemp<1;

        //Priority 1 Operands
        nTemp := CodeFind(length(sEquation),sEquation,Operator);
        p:=0;
        q:=0;
        repeat
                sTemp := Copy(sEquation, nTemp, 1);
                if sTemp='(' then inc(p);
                if sTemp=')' then dec(p);
                if sTemp='"' then inc(q);
                if (q mod 2=0)and(p=0)and isOperator(sTemp) then begin
                        nTemp2 := CodeFind(nTemp-1, sEquation, NonOperator);
                        OpTemp := Copy(sEquation, nTemp2+1, nTemp - nTemp2);
                        if (opTemp='*')or(opTemp='/')or(opTemp='\')or(opTemp='%')or(opTemp='>>')or(opTemp='<<')or(opTemp='&') then begin
                                vTemp:=rSolve(Rut, Copy(sEquation,1,nTemp2));
                                if fScripts[fBindedScript-1].fRunError<>'' then exit;
                                result:=rSolve(Rut, Copy(sEquation, nTemp+1, length(sEquation)));
                                if fScripts[fBindedScript-1].fRunError<>'' then exit;
                                Result := rDoOperator(vTemp, Result, OpTemp);
                                exit;
                        end;
                        nTemp:=nTemp2+1;
                end;
                dec(nTemp);
        until nTemp<1;

        //Priority 0 Operands
        nTemp := CodeFind(length(sEquation),sEquation,Operator);
        p:=0;
        q:=0;
        repeat
                sTemp := Copy(sEquation, nTemp, 1);
                if sTemp='(' then inc(p);
                if sTemp=')' then dec(p);
                if sTemp='"' then inc(q);
                if (q mod 2=0)and(p=0)and isOperator(sTemp) then begin
                        nTemp2 := CodeFind(nTemp-1, sEquation, NonOperator);
                        OpTemp := Copy(sEquation, nTemp2+1, nTemp - nTemp2);
                        if (opTemp='!') then begin
                                vTemp:=rSolve(Rut, Copy(sEquation,1,nTemp2));
                                if fScripts[fBindedScript-1].fRunError<>'' then exit;
                                result:=rSolve(Rut, Copy(sEquation, nTemp+1, length(sEquation)));
                                if fScripts[fBindedScript-1].fRunError<>'' then exit;
                                Result := rDoOperator(vTemp, Result, OpTemp);
                                exit;
                        end;
                        nTemp:=nTemp2+1;
                end;
                dec(nTemp);
        until nTemp<1;
 end;
 
 if sEquation[1]='(' then begin
        result:=rSolve(Rut, Copy(sEquation,2,length(sEquation)-2));
        exit;
 end;

 if (sEquation[1]<>'e')and isNumber(sEquation[1]) then begin
        sEquation:=NormalizeFloat(sEquation);
        result:=strtofloat(sEquation);
        exit;
 end;
 if sEquation[1]=#34 then begin
        result:=Copy(sEquation,2,length(sEquation)-2);
        exit;
 end;
 nTemp:=Pos('(',sEquation);
 if nTemp<>0 then begin
        result:=rDoFunction(Rut, sEquation);
        exit;
 end
 else begin
        result:=rGetVar(Rut, sEquation);
        exit;
 end;
end;


function rDoOperator;
begin
 if fBindedScript=0 then exit;
 fScripts[fBindedScript-1].fRunError:='';
 Result := '';
 sOp := trim(sOp);
 if (varIsArray(LeftVal))or(VarIsArray(RightVal))then begin
        fScripts[fBindedScript-1].fRunError:='Incompatibile types';
        exit;
 end;
 if (not((VarType(LeftVal)<>num) and (sOp='-')))and((varType(LeftVal)<>varType(RightVal))or((sOp='!')and(varType(RightVal)=str)))then begin
        fScripts[fBindedScript-1].fRunError:='Incompatibile types';
        exit;
 end;
 try

  if sOp = '>' then begin
        Result := LeftVal > RightVal;
        exit;
  end
  else if sOp = '<' then begin
        Result := LeftVal < RightVal;
        exit;
  end
  else if sOp = '>=' then begin
        Result := LeftVal >= RightVal;
        exit;
  end
  else if sOp = '<=' then begin
        Result := LeftVal <= RightVal;
        exit;
  end
  else if sOp = '<>' then begin
        Result := LeftVal <> RightVal;
        exit;
  end
  else if sOp = '=' then  begin
        Result := LeftVal = RightVal;
        exit;
  end
  else if sOp = '==' then begin
        Result := lowercase(LeftVal) = lowercase(RightVal);
        exit;
  end;

  if (sOp = '+') then begin
        if varType(LeftVal)=bool then begin
                fScripts[fBindedScript-1].fRunError:='Incompatibile types';
                exit;
        end;
        Result := LeftVal + RightVal;
        exit;
  end;

  if varType(LeftVal)=str then begin
        fScripts[fBindedScript-1].fRunError:='Incompatibile types';
        exit;
  end;

  if sOp = '|' then begin
        if VarType(LeftVal)=bool then Result := LeftVal or RightVal
                                 else Result := trunc(LeftVal) or trunc(RightVal);
        exit;
  end
  else if sOp = '&' then begin
        if VarType(LeftVal)=bool then Result := LeftVal and RightVal
                                 else Result := trunc(LeftVal) and trunc(RightVal);
        exit;
  end
  else if sOp = '^' then begin
        if varType(LeftVal)=bool then Result := LeftVal xor RightVal
                                 else Result := trunc(LeftVal) xor trunc(RightVal);
        exit;
  end
  else if sOp = '!' then begin
        if varType(RightVal)=bool then Result := not RightVal
                                  else Result := not trunc(RightVal);
        exit;
  end;

  if varType(LeftVal)=bool then begin
        fScripts[fBindedScript-1].fRunError:='Incompatibile types';
        exit;
  end;

  if sOp = '>>' then begin
        Result := trunc(LeftVal) shr trunc(RightVal);
        exit;
  end
  else if sOp = '<<' then begin
        Result := trunc(LeftVal) shl trunc(RightVal);
        exit;
  end
  else if sOp = '-' then begin
        if varType(LeftVal)<>num then begin Result:=-RightVal; exit; end;
        Result := LeftVal - RightVal;
        exit;
  end
  else if sOp = '*' then begin
        Result := LeftVal * RightVal;
        exit;
  end
  else if sOp = '%' then begin
        if RightVal=0 then fScripts[fBindedScript-1].fRunError:='Division by zero'
                      else Result := (LeftVal) mod trunc(RightVal);
        exit;
  end
  else if sOp = '/' then begin
        if RightVal=0 then fScripts[fBindedScript-1].fRunError:='Division by zero'
                      else Result := LeftVal / RightVal;
        exit;
  end
  else if sOp = '\' then begin
        if RightVal=0 then fScripts[fBindedScript-1].fRunError:='Division by zero'
                      else Result := trunc(LeftVal) div trunc(RightVal);
        exit;
  end
 except
        on E: Exception do begin
                fScripts[fBindedScript-1].fRunError:=E.Message;
        end;
        on e:EExternal do begin
                fScripts[fBindedScript-1].fRunError:=E.Message;
        end;
 end;
end;

function rDoFunction;
var

 sName, sTemp,sTemp2, s: String;
 nTemp1, nTemp2: Integer;
 data: TArray;
 p,i, li:integer;
begin
 if fBindedScript=0 then exit;
 fScripts[fBindedScript-1].fRunError:='';
 // Trim off the ')' at the end
 nTemp1 := Pos('(',DaCall);
 if nTemp1<>0 then begin
        nTemp2 := CodeInStr(nTemp1+1, DaCall, ')');
        DaCall := Copy(DaCall, 1, nTemp2-1);
        sName  := Copy(DaCall, 1, nTemp1-1);
        sTemp  := Copy(DaCall, nTemp1+1, length(DaCall));
 end
 else begin
        sName:=DaCall;
        sTemp:='';
 end;
 data := nil;
 p:=0;
 sTemp2:=sTemp;
 li:=1;
 for i:=1 to length(sTemp) do begin
        if sTemp[i]='(' then inc(p);
        if sTemp[i]=')' then dec(p);
        if (p=0)and(sTemp[i]=',') then begin
                s:=copy(sTemp,li,i-li);
                li:=i+1;
                sTemp2:=copy(sTemp,i+1, length(sTemp));
                setlength(data, length(data)+1);
                data[high(data)]:=rSolve(Rut, s);
                if fScripts[fBindedScript-1].fRunError<>'' then begin
                        data:=nil;
                        exit;
                end;
        end;
 end;
 if sTemp2<>'' then begin
        setlength(data, length(data)+1);
        data[high(data)]:=rSolve(Rut, sTemp2);
        if fScripts[fBindedScript-1].fRunError<>'' then begin
                data:=nil;
                exit;
        end;
 end;

 Result := rCallFunction(Rut, lowercase(sName), data);
 data:=nil;
end;

function rGetVar;
var
  n: Integer;
begin
 if fBindedScript=0 then exit;
 fScripts[fBindedScript-1].fRunError:='';
  for n := 0 to high(Rut.fVariables) do
        if Rut.fVariables[n].fName = lowercase(sVar) then begin
                Result := Rut.fVariables[n].fValue;
                Exit;
        end;
   for n := 0 to high(Rut.fParams) do
        if Rut.fParams[n].fName = lowercase(sVar) then begin
                Result := Rut.fParams[n].fValue;
                Exit;
        end;
   if lowercase(sVar)='result' then begin
        Result := Rut.fResult.fValue;
        Exit;
   end;

   for n := 0 to high(fGlobalVars) do
        if fGlobalVars[n].fName = lowercase(sVar) then begin
                Result := fGlobalVars[n].fValue;
                Exit;
        end;
   result:=rCallFunction(Rut, lowercase(sVar), nil);
end;

procedure rChangeVar;
var
  n: Integer;
begin
  fScripts[fBindedScript-1].fRunError:='';
  for n := 0 to high(Rut.fVariables) do
        if Rut.fVariables[n].fName = lowercase(sVar) then begin
                if (varType(Rut.fVariables[n].fValue)<>varType(sNewVal)) then begin
                        fScripts[fBindedScript-1].fRunError:='Invalid types';
                        exit;
                end;
                Rut.fVariables[n].fValue := sNewVal;
                Exit;
        end;
   for n := 0 to high(Rut.fParams) do
        if Rut.fParams[n].fName = lowercase(sVar) then begin
                if (varType(Rut.fParams[n].fValue)<>varType(sNewVal)) then begin
                        fScripts[fBindedScript-1].fRunError:='Invalid types';
                        exit;
                end;
                Rut.fParams[n].fValue:=sNewVal;
                Exit;
        end;

   if lowercase(sVar)='result' then begin
        if (varType(Rut.fResult.fValue)<>varType(sNewVal))  then begin
                fScripts[fBindedScript-1].fRunError:='Invalid types';
                exit;
        end;
        Rut.fResult.fValue:=sNewVal;
        Exit;
   end;
   for n := 0 to high(fGlobalVars) do
        if fGlobalVars[n].fName = lowercase(sVar) then begin
                if (varType(fGlobalVars[n].fValue)<>varType(sNewVal)) then begin
                        fScripts[fBindedScript-1].fRunError:='Invalid types';
                        exit;
                end;
                fGlobalVars[n].fValue:=sNewVal;
                Exit;
        end;
end;

function rCopyValues(Src:TArray; var dst:TVarArray):boolean;
var
 i:integer;
begin
 result:=true;
 if length(Src)<>length(Dst) then begin
        result:=false;
        exit;
 end;
 for i:=0 to high(src) do begin
        if varType(Src[i])<>varType(Dst[i].fValue) then begin
                result:=false;
                exit;
        end;

        Dst[i].fValue:=Src[i];
 end;
end;


function rCallFunction;
var
 i:integer;
 f:TFunc;
begin
 if fBindedScript=0 then exit;
 fScripts[fBindedScript-1].fRunError:='';
 Result := '';
 for i:=0 to high(fScripts[fBindedScript-1].fFunc) do
        if lowercase(fScripts[fBindedScript-1].fFunc[i].fName)=DaFunc then begin
                if Rut.fName=fScripts[fBindedScript-1].fFunc[i].fName then begin
                        f:=Rut;
                        f.fVariables:=Copy(Rut.fVariables, 0, length(Rut.fVariables));
                        f.fParams:=Copy(Rut.fParams, 0, length(Rut.fParams));
                end
                else f:=fScripts[fBindedScript-1].fFunc[i];
                if not rCopyValues(Args,f.fParams) then begin
                        fScripts[fBindedScript-1].fRunError:='Invalid parameters';
                        exit;
                end;
                try
                        inc(f.fLevel);
                        rCallFunc(f);
                        Finalize(f.fVariables);
                        Finalize(f.fParams);
                        Finalize(f.fScript);
                        Finalize(f.fIf);
                        Finalize(f.fWhile);
                        Finalize(f.last);
                except
                        on E:EExternal do fScripts[fBindedScript-1].fRunError:=E.Message;
                end;
                if fScripts[fBindedScript-1].fRunError<>'' then begin
                        exit;
                end;
                Rut.fResult:=f.fResult;
                Result:=fScripts[fBindedScript-1].fResult;
                exit;
        end;
 Result:=ISLFunctions.rCallFunction(Rut, DaFunc, Args, fScripts[fBindedScript-1].fRunError);
end;

end.

