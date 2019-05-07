unit UMaterials;
interface
uses dglOpenGL, UTypes;

procedure InitMaterials;
procedure ClearMaterial(var mat:PMaterial);
function NewMaterial:PMaterial;
implementation
uses URenderer;
var
 fInited:boolean=false;
 Materials:array of PMaterial;

procedure InitMaterials;
begin
 if fInited then exit;
 fInited:=true;
 Materials:=nil;
end;

procedure ClearMaterial(var mat:PMaterial);
var
 i:integer;
begin
 i:=Mat.index;
 Renderer.RC.fTexManager.DeleteTexture(Mat.TexStg.texture0);
 Renderer.RC.fTexManager.DeleteTexture(Mat.TexStg.texture1);
 dispose(Mat);
 Mat:=nil;
 materials[i]:=nil;
end;

function NewMaterial:PMaterial;
begin
 setlength(Materials, length(Materials)+1);
 new(Materials[high(Materials)]);
 fillchar(Materials[high(Materials)]^, sizeof(Materials[high(Materials)]^),0);
 Materials[high(Materials)].index:=high(Materials);
 result:=Materials[high(Materials)];
end;
procedure Final;
var
 i:integer;
begin
 for i:=0 to high(Materials) do dispose(Materials[i]);
 Materials:=nil;
end;

initialization
finalization
 Final;
end.
