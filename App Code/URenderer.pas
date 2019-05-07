unit URenderer;
interface
uses dglOpenGL, URC, USkyBox, UCamera, xsl,
     UTypes, UParticleManager, UVectors, UMaterials;
type
 TRenderer=class
        private
         fDeltaTime:single;
         fRendering:boolean;
         fRC:TRC;
         fCamera:TCamera;
         fScriptEngine:TScript;
         fParticleManager:TParticleManager;
         fSceneObjects:TBasicSceneObjectList;
         fSkyBox:TSkyBox;

         procedure SetSkyBox(const Value: TSkyBox);
         procedure OnAfterGLInit;
         procedure OnMove;
        public
         constructor Create;
         procedure StartRenderer;
         destructor destroy; override;
         procedure Clear;
         property SkyBox:TSkyBox read FSkyBox write SetSkyBox;

         property RC:TRC read fRC;
         property Camera:TCamera read fCamera;
         property ScriptEngine:TScript read fScriptEngine;
         property ParticleManager:TParticleManager read fParticleManager;

         property Rendering:boolean read fRendering;
         property DeltaTime:single read fDeltaTime;

         procedure BeginRendering(const rendering:boolean; const DeltaTime:single);
         procedure Render;
         procedure EndRendering;

         procedure AddSceneObject(obj:TBasicSceneObject);
         procedure ClearSceneObjects;
 end;

var
 Renderer:TRenderer=nil;
 switch:integer=0;
implementation

{ TRenderer }

procedure TRenderer.Clear;
begin
 if fSkyBox<>nil then begin
        fSkyBox.Free;
        fSkyBox:=nil;
 end;
 fCamera.SetPosition(MakeVector3(0,0,0));
 fParticleManager.Clear;
 fSceneObjects:=nil;
end;

constructor TRenderer.Create;
begin
 inherited Create;
 fSceneObjects:=nil;
end;

destructor TRenderer.destroy;
begin
 Clear;
 fParticleManager.Free;
 fCamera.Free;
 fRC.Free;
 fScriptEngine.Free;
 inherited;
end;

procedure TRenderer.OnAfterGLInit;
begin

end;

procedure TRenderer.OnMove;
begin
end;

procedure TRenderer.Render;
var
 i:integer;
begin
 SortSceneObjects(fSceneObjects);
 for i:=0 to high(fSceneObjects) do fSceneObjects[i].render;
end;

procedure TRenderer.SetSkyBox(const Value: TSkyBox);
begin
 FSkyBox := Value;
end;

procedure TRenderer.BeginRendering;
begin
 fSceneObjects:=nil;
 fDeltaTime:=DeltaTime;
 fRendering:=rendering;
 GL.glDepthMask(true);
 GL.glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
 fCamera.Update(fDeltaTime);
 fCamera.Look;
 fParticleManager.Update;
 if SkyBox<>nil then SkyBox.Render;
end;

procedure TRenderer.StartRenderer;
begin
 fSkyBox:=nil;
 fScriptEngine:=TScript.create;
 fRC:=TRC.Create(OnAfterGLInit, 32, 32, 8);
 fRC.AddScriptFunctions(@fScriptEngine);
 InitMaterials;
 fCamera:=TCamera.create;
 fCamera.AddScriptFunctions(@fScriptEngine);
 FCamera.LookProc:=OnMove;
 fParticleManager:=TParticleManager.create;
 fParticleManager.AddScriptFunctions(@fScriptEngine);
 SkyBox:=TSkyBox.Create;
 SkyBox.AddScriptFunctions(@fScriptEngine);
end;

procedure TRenderer.EndRendering;
begin
 fRC.GLFlip(true);
end;

procedure TRenderer.AddSceneObject(obj: TBasicSceneObject);
begin
 setlength(fSceneObjects, length(fSceneObjects)+1);
 fSceneObjects[high(fSceneObjects)]:=obj;
end;

procedure TRenderer.ClearSceneObjects;
begin
 fSceneObjects:=nil;
end;

end.
