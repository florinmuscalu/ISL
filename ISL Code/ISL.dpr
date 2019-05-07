library ISL;
uses ISLMain, ISLTypes, ISLUtils, ISLFunctions;
{$R *.res}

var
 SaveExit: Pointer;

procedure LibExit;
begin
 ISLMain.Destroy;
 ExitProc:=SaveExit;
end;

function LoadScript(filename:pchar):boolean; stdcall
begin
 result:=ISLMain.Load(filename);
end;

function CompileScript:pchar; stdcall
begin
 setCompileError('No Error');
 ISLMain.Compile;
 Result:=pchar(CompileError);
end;

function LastError:pchar; stdcall
begin
 result:=pchar(CompileError);
end;

function getGlobalVar(index:integer):TVariable; stdcall
begin
 result:=ISLMain.getGlobalVar(index);
end;

procedure setGlobalVar(index:integer; value:Variant); stdcall
begin
 ISLMain.setGlobalVar(index,value);
end;

function GlobalVarCount:integer; stdcall
begin
 result:=ISLMain.GlobalVarCount;
end;

function ScriptResult:Variant; stdcall
begin
 result:=ISLMain.Result;
end;

procedure ExecuteScript1(fFunction:pchar); stdcall
begin
 ISLMain.ExecuteScript(fFunction);
end;

procedure ExecuteScript2(fFunction:pchar; Args:TArray); stdcall
begin
 ISLMain.ExecuteScript(fFunction, Args);
end;

procedure AddFunction(Name:pchar; NumParams:integer; F:TMethod); stdcall
begin
 ISLFunctions.AddFunction(Name, NumParams, F);
end;

procedure CreateEngine; stdcall
begin
 ISLMain.Create;
 setCompileError('No Error');
end;

procedure ClearFunctions; stdcall
begin
 ISLFunctions.Clear;
end;

function LoadCompiledScript(filename:pchar):boolean; stdcall
begin
 result:=ISLMain.LoadCompiled(filename);
end;

function SaveCompiledScript(filename:pchar):boolean; stdcall
begin
 result:=ISLMain.SaveCompiled(filename);
end;

exports LoadScript,
        CompileScript,
        LastError,
        getGlobalVar,
        setGlobalVar,
        GlobalVarCount,
        ScriptResult,
        ExecuteScript1,
        ExecuteScript2,
        AddFunction,
        ClearFunctions,
        LoadCompiledScript,
        SaveCompiledScript,
        NewScript,
        BindScript,
        DeleteScript,
        LoadFromPointer;

begin
 SaveExit:=ExitProc;
 ExitProc:=@LibExit;
 CreateEngine;
end.
