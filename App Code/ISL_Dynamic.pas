unit ISL_Dynamic;
interface
const
 ISLDLL='ISL.dll';
type
 TVarType=(Num, Str, Bool, nVect, sVect, bVect); //tipuri de variabile acceptate de ISL
 TVariable = record								 //reprezentarea interna a unei variabile ISL
        fName: string[30];
        fType: TVarType;
        fValue: Variant;
 end;
 TArray=Array of Variant;
 TMethod=Function(Args:TArray; var error:pchar):Variant of object;	//prototipul unei functii externe ISL
 
 TLoadScript=function        (filename:pchar):boolean; stdcall;			//prototipul functiei LoadScript
 TCompileScript=function     :pchar; stdcall;							//prototipul functiei CompileScript
 TLastError=function         :pchar; stdcall;							//Prototipul functiei LastError
 TgetGlobalVar=function      (index:integer):TVariable; stdcall;		//Prototipul functiei getGlobalVar
 TsetGlobalVar=procedure     (index:integer; value:Variant); stdcall;	//prototipul functiei setGlobalVar
 TGlobalVarCount=function    :integer; stdcall;							//prototipul functiei GlobalVarCount
 TScriptResult=Function      :Variant; stdcall;							//prototipul functiei ScriptResult
 TExecuteScript1=Procedure   (fFunction:pchar); stdcall;				//Prototipul functiei ExecuteScript1 (executie functie fara parametrii)
 TExecuteScript2=Procedure   (fFunction:pchar; Args:TArray); stdcall; 	//Prototipul functiei ExecuteScript2 (executie functie cu parametrii)
 TAddFunction=procedure      (Name:pchar; NumParams:integer; F:TMethod); stdcall;	//Prototipul functiei AddFunction. Are ca parametru o functie cu prototipul TMethod.
 TClearFunctions=procedure   ; stdcall;									//prototipul functie ClearFunctions
 TLoadCompiledScript=function(filename:pchar):boolean; stdcall;			//Prototipul functiei LoadCompiledScript
 TSaveCompiledScript=function(filename:pchar):boolean; stdcall;			//Prototipul functiei SaveCompiledScript
 TNewScript=function :cardinal; stdcall;								//Prototipul functiei NewScript
 TBindScript=procedure (const Handle:cardinal); stdcall;				//Prototipul functiei BindScript
 TDeleteScript=procedure (const Handle:cardinal); stdcall;				//Prototipul functiei DeleteScript
var
 LoadScript:TLoadScript;
 CompileScript:TCompileScript;
 LastError:TLastError;
 getGlobalVar:TGetGlobalVar;
 setGlobalVar:TsetGlobalVar;
 GlobalVarCount:TGlobalVarCount;
 ScriptResult:TScriptResult;
 ExecuteScript1:TExecuteScript1;
 ExecuteScript2:TExecuteScript2;
 AddFunction:TAddFunction;
 ClearFunctions:TClearFunctions;
 LoadCompiledScript:TLoadCompiledScript;
 SaveCompiledScript:TSaveCompiledScript;
 NewScript:TNewScript;
 BindScript:TBindScript;
 DeleteScript:TDeleteScript;

function LoadISL:boolean;						//se apeleaza la inceput si are ca scop legarea DLL-ului.
procedure FreeISL;								//se apeleaza la sfarsit. Elibereaza resursele.
function ISL_VarType(V:Variant):TVarType;		//returneaza tipul unei variabile

implementation
uses windows;
var
 libHandle:THandle;
function LoadISL;
begin
 result:=true;
 libHandle:=LoadLibrary(ISLDLL);
 if libHandle=0 then begin
        result:=false;
        exit;
 end;
 LoadScript:=getProcAddress(libHandle,'LoadScript');
 CompileScript:=getProcAddress(libHandle,'CompileScript');
 LastError:=getProcAddress(libHandle,'LastError');
 getGlobalVar:=getProcAddress(libHandle,'getGlobalVar');
 setGlobalVar:=getProcAddress(libHandle,'setGlobalVar');
 GlobalVarCount:=getProcAddress(libHandle,'GlobalVarCount');
 ScriptResult:=getProcAddress(libHandle,'ScriptResult');
 ExecuteScript1:=getProcAddress(libHandle,'ExecuteScript1');
 ExecuteScript2:=getProcAddress(libHandle,'ExecuteScript2');
 AddFunction:=getProcAddress(libHandle,'AddFunction');
 ClearFunctions:=getProcAddress(libHandle,'ClearFunctions');
 LoadCompiledScript:=getProcAddress(libHandle,'LoadCompiledScript');
 SaveCompiledScript:=getProcAddress(libHandle,'SaveCompiledScript');
 NewScript:=getProcAddress(libHandle,'NewScript');
 BindScript:=getProcAddress(libHandle,'BindScript');
 DeleteScript:=getProcAddress(libHandle,'DeleteScript');
end;

procedure FreeISL;
begin
 if LibHandle<>0 then FreeLibrary(libHandle);
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
end;

function ISL_VarType(V:Variant):TVarType;
begin
 if (TVarData(V).VType>=2)and(TVarData(V).VType<=5) then result:=num
 else if (TVarData(V).VType=$0B) then result:=bool
 else if (TVarData(V).VType=8)or(TVarData(V).VType=$100) then result:=str
 else if (TVarData(V).VType>=2+$02000)and(TVarData(V).VType<=5+$02000) then result:=nVect
 else if (TVarData(V).VType=$0B+$02000) then result:=bVect
 else result:=sVect;
end;

initialization
 libHandle:=0;
 FreeISL;
end.
