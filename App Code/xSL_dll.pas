unit XSL;
interface
uses ISL_Dynamic;
type
 TVarType=ISL_Dynamic.TVarType;
 TVariable = ISL_Dynamic.TVariable;

 TArray=ISL_Dynamic.TArray;
 TMethod=ISL_Dynamic.TMethod;

 PScript=^TScript;
 TScript=class
        private
         ExecuteScript1:TExecuteScript1;
         ExecuteScript2:TExecuteScript2;
        public
         LoadScript:TLoadScript;
         CompileScript:TCompileScript;
         LastError:TLastError;
         getGlobalVar:TGetGlobalVar;
         setGlobalVar:TsetGlobalVar;
         GlobalVarCount:TGlobalVarCount;
         ScriptResult:TScriptResult;
         AddFunction:TAddFunction;
         ClearFunctions:TClearFunctions;
         LoadCompiledScript:TLoadCompiledScript;
         SaveCompiledScript:TSaveCompiledScript;
         NewScript:TNewScript;
         BindScript:TBindScript;
         DeleteScript:TDeleteScript;

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
 if not LoadISL then begin
        glwAppendLog('Fatal error: ISL.dll not found!');
        MessageBox(0,'ISL.dll not Found!','Error',MB_ICONERROR or MB_SYSTEMMODAL);
        halt;
 end;
 LoadScript:=ISL_Dynamic.LoadScript;
 CompileScript:=ISL_Dynamic.CompileScript;
 LastError:=ISL_Dynamic.LastError;
 getGlobalVar:=ISL_Dynamic.getGlobalVar;
 setGlobalVar:=ISL_Dynamic.setGlobalVar;
 GlobalVarCount:=ISL_Dynamic.GlobalVarCount;
 ScriptResult:=ISL_Dynamic.ScriptResult;
 ExecuteScript1:=ISL_Dynamic.ExecuteScript1;
 ExecuteScript2:=ISL_Dynamic.ExecuteScript2;
 AddFunction:=ISL_Dynamic.AddFunction;
 ClearFunctions:=ISL_Dynamic.ClearFunctions;
 LoadCompiledScript:=ISL_Dynamic.LoadCompiledScript;
 SaveCompiledScript:=ISL_Dynamic.SaveCompiledScript;
 NewScript:=ISL_Dynamic.NewScript;
 BindScript:=ISL_Dynamic.BindScript;
 DeleteScript:=ISL_Dynamic.DeleteScript;

 glwAppendLog('ISL initialised.');
end;

destructor TScript.destroy;
begin
 FreeISL;
 LoadScript:=nil;
 CompileScript:=nil;
 LastError:=nil;
 getGlobalVar:=nil;
 setGlobalVar:=nil;
 GlobalVarCount:=nil;
 ScriptResult:=nil;
 ExecuteScript1:=nil;
 ExecuteScript2:=nil;
 AddFunction:=nil;
 ClearFunctions:=nil;
 LoadCompiledScript:=nil;
 SaveCompiledScript:=nil;
 NewScript:=nil;
 BindScript:=nil;
 DeleteScript:=nil;

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
 result:=ISL_Dynamic.ISL_VarType(V);
end;

end.
