unit XSL;
interface
uses ISLTypes, ISLMain, ISLUtils, ISLFunctions;
type
 TVarType=ISLTypes.TVarType;
 TVariable = ISLTypes.TVariable;

 TArray=ISLTypes.TArray;
 TMethod=ISLTypes.TMethod;

 PScript=^TScript;
 TScript=class
        private
         Procedure ExecuteScript1(fFunction:pchar); overload;
         Procedure ExecuteScript2(fFunction:pchar; Args:TArray); overload;
        public
         function LoadScript(filename:pchar):boolean;
         function CompileScript:pchar;
         function LastError:pchar;
         function getGlobalVar(index:integer):TVariable;
         procedure setGlobalVar(index:integer; value:Variant);
         function GlobalVarCount:integer;
         Function ScriptResult:Variant;
         procedure AddFunction(Name:pchar; NumParams:integer; F:TMethod);
         procedure ClearFunctions;
         function LoadCompiledScript(filename:pchar):boolean;
         function SaveCompiledScript(filename:pchar):boolean;
         function NewScript:cardinal;
         procedure BindScript(const Handle:cardinal);
         procedure DeleteScript(const Handle:cardinal);

         procedure Run (fFunction:pchar); overload;
         procedure Run (fFunction:pchar; Args:TArray); overload;

         constructor create;
         destructor destroy; override;

         function ISL_VarType(V:Variant):TVarType;
 end;
implementation
uses windows, UFormManager;

constructor TScript.create;
begin
 inherited Create;
 ISLMain.Create;
 setCompileError('No Error');
 glwAppendLog('ISL initialised.');
end;

destructor TScript.destroy;
begin
 ISLMain.Destroy;
 inherited;
 glwAppendLog('ISL destroyed.');
end;

procedure TScript.Run(fFunction: pchar);
begin
 ExecuteScript1(fFunction);
end;

procedure TScript.Run(fFunction: pchar; Args: TArray);
begin
 ExecuteScript2(fFunction, Args);
end;

function TScript.ISL_VarType(V:Variant):TVarType;
begin
 result:=ISLUtils.VarType(V);
end;

procedure TScript.AddFunction(Name: pchar; NumParams: integer; F: TMethod);
begin
 ISLFunctions.AddFunction(Name, NumParams, F);
end;

procedure TScript.BindScript(const Handle: cardinal);
begin
 ISLMain.BindScript(handle);
end;

procedure TScript.ClearFunctions;
begin
 ISLFunctions.Clear;
end;

function TScript.CompileScript: pchar;
begin
 setCompileError('No Error');
 ISLMain.Compile;
 Result:=pchar(CompileError);
end;

procedure TScript.DeleteScript(const Handle: cardinal);
begin
 ISLMain.DeleteScript(handle);
end;

procedure TScript.ExecuteScript1(fFunction: pchar);
begin
 ISLMain.ExecuteScript(fFunction);
end;

procedure TScript.ExecuteScript2(fFunction: pchar; Args: TArray);
begin
 ISLMain.ExecuteScript(fFunction, Args);
end;

function TScript.getGlobalVar(index: integer): TVariable;
begin
 result:=ISLMain.getGlobalVar(index);
end;

function TScript.GlobalVarCount: integer;
begin
 result:=ISLMain.GlobalVarCount;
end;

function TScript.LastError: pchar;
begin
 result:=pchar(CompileError);
end;

function TScript.LoadCompiledScript(filename: pchar): boolean;
begin
 result:=ISLMain.LoadCompiled(filename);
end;

function TScript.LoadScript(filename: pchar): boolean;
begin
 result:=ISLMain.Load(filename);
end;

function TScript.NewScript: cardinal;
begin
 result:=ISLMain.NewScript;
end;

function TScript.SaveCompiledScript(filename: pchar): boolean;
begin
 result:=ISLMain.SaveCompiled(filename);
end;

function TScript.ScriptResult: Variant;
begin
 result:=ISLMain.Result;
end;

procedure TScript.setGlobalVar(index: integer; value: Variant);
begin
 ISLMain.setGlobalVar(index,value);
end;

end.
