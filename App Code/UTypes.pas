{Done}
unit UTypes;
interface
uses windows, dglOpenGL, UVectors;
const
 {used by SetGLPolygonMode:}
        {aFace}
        GL_FRONT = $0404;
        GL_BACK  = $0405;
        GL_FRONT_AND_BACK = $0408;
        {mode}
        GL_POINT = $1B00;
        GL_LINE  = $1B01;
        GL_FILL  = $1B02;
        
 {used by SetGLCurrentTexture}
        {texture unit}
        GL_TEXTURE0 = $84C0;
        GL_TEXTURE1 = $84C1;
        GL_TEXTURE2 = $84C2;
        GL_TEXTURE3 = $84C3;
        GL_TEXTURE4 = $84C4;
        GL_TEXTURE5 = $84C5;
        GL_TEXTURE6 = $84C6;
        GL_TEXTURE7 = $84C7;

 {used by SetBlendFunc}
        {both factors}
        GL_ZERO = 0;
        GL_ONE  = 1;
        GL_SRC_ALPHA = $0302;
        GL_ONE_MINUS_SRC_ALPHA = $0303;
        GL_DST_ALPHA = $0304;
        GL_ONE_MINUS_DST_ALPHA = $0305;
        {src}
        GL_DST_COLOR = $0306;
        GL_ONE_MINUS_DST_COLOR = $0307;
        GL_SRC_ALPHA_SATURATE = $0308;
        {dst}
        GL_SRC_COLOR = $0300;
        GL_ONE_MINUS_SRC_COLOR = $0301;

type
 TVertexProgram=record
        fVP:cardinal;
 end;
 
 {used for SetGLState, UnSetGLState}
 TGLState = (stLight,
             stBlend,           // GL_BLEND
             stCullFace,        //GL_CULL_FACE
             stDepthTest,       //GL_DEPTH_TEST
             stFog,             //GL_FOG
             stScissorTest,     //GL_SCISSOR_TEST
             stStencilTest,     //GL_STENCIL_TEST
             stRegisterCombiners//GL_REGISTER_COMBINERS_NV
             );

 {used for SetGLClientState, UnSetGLClientState}
 TGLClientState=(stVertexArray,         //GL_VERTEX_ARRAY
                 stColorArray,          //GL_COLOR_ARRAY
                 stTexCoordArray0,      //GL_TEXTURE_COORD_ARRAY pe GL_TEXTURE0
                 stTexCoordArray1,      //GL_TEXTURE_COORD_ARRAY pe GL_TEXTURE1
                 stNormalArray
                );

 {Used by ClientStates, SetClientStates}
 TGLClientStates=set of TGLClientState;

 {Used by States, SetStates}
 TGLStates = set of TGLState;

 PMaterial=^TMaterial;
 PBasicTexture=^TBasicTexture;

 TBasicTexture=class
        fFileName:shortstring;          //name of the texture file
        fTexHandle:cardinal;            //the texture object
        fHasMipMaps:boolean;
        fCompressed:boolean;
        Index:cardinal;
 end;

  TSubTex=record
        name:shortstring;
        index:cardinal;
        OffsetX:cardinal;
        OffsetY:cardinal;
        Width:cardinal;
        Height:cardinal;
        filename:shortstring;
 end;

 TTileSet=class(TBasicTexture)
        fWidth,fHeight:cardinal;
        fSubTex:array of TSubTex;
        constructor Create;
        destructor destroy; override;
 end;

 TBasicSceneObject=class(TObject)
       private
        fEyeDistance:single;
       public
        fTransformMatrix:TMatrix;
        fRenderingMaterial:PMaterial;
        fSelectionMaterial:PMaterial;
        procedure render; virtual; abstract;
        constructor create;
        destructor destroy; override;
 end;
 TBasicSceneObjectList=array of TBasicSceneObject;

 TBlendingRec=record
        Use:boolean;
        fSrc, fDst:glEnum;
 end;
 TDepthTest=record
        DepthTest:boolean;
        DepthMask:boolean;
 end;

 TVertexArrayUsage=record
        VertexArray, ColorArray, TexCoord0Array, TexCoord1Array, NormalArray:boolean;
 end;

 TTexStg=record
        UseTex0, UseTex1:boolean;
        texture0:PBasicTexture;
        texture1:PBasicTexture;
 end;

 TVPStg=record
        UseVP:boolean;
        VP:cardinal;
 end;

 TMaterial=record
        Blend:TBlendingRec;
        DepthTest:TDepthTest;
        VertexArray:TVertexArrayUsage;
        TexStg:TTexStg;
        VPSettings:TVPStg;
        
        UseCulling,
        UseFog, UseScissorTest, UseStencilTest, UseRegisterCombiners:boolean;
        Color:array[0..3]of single;
        DynamicalyIluminated:boolean;
        CastShadow:boolean;
        index:integer;
 end;

procedure SortSceneObjects(var List:TBasicSceneObjectList);
implementation
uses URenderer, UMaterials, SysUtils, UFormManager;

procedure SortSceneObjects(var List:TBasicSceneObjectList);
var
 i:integer;
 v:TVector3;

 procedure QSort(min, max:integer);
 var
  Lo, Hi: Integer;
  Mid:single;
  P:TBasicSceneObject;
 begin
  Lo := min;
  Hi := max;
  Mid := List[(Lo+Hi) div 2].fEyeDistance;
  repeat
        while List[Lo].fEyeDistance > Mid do Inc(Lo);
        while List[Hi].fEyeDistance < Mid do Dec(Hi);
        if Lo <= Hi then begin
                P:=List[Lo];
                List[Lo]:=List[Hi];
                List[Hi]:=P;
                Inc(Lo);
                Dec(Hi);
        end;
  until Lo > Hi;
  if Hi > min then QSort(min, Hi);
  if Lo < max then QSort(Lo, max);
 end;
begin
 if List=nil then exit;
 for i:=0 to high(List) do begin
        v[0]:=List[i].fTransformMatrix[3,0];
        v[1]:=List[i].fTransformMatrix[3,1];
        v[2]:=List[i].fTransformMatrix[3,2];
        List[i].fEyeDistance:=Renderer.Camera.EyeDistance(v);
 end;
 QSort(0, high(List));
end;

{ TBasicSceneClass }

constructor TBasicSceneObject.create;
begin
 inherited;
 fRenderingMaterial:=NewMaterial;
 fSelectionMaterial:=NewMaterial;

 fRenderingMaterial.Blend.Use:=false;
 fRenderingMaterial.Blend.fSrc:=GL_SRC_ALPHA;
 fRenderingMaterial.Blend.fDst:=GL_ONE_MINUS_SRC_ALPHA;

 fSelectionMaterial.Blend.Use:=false;
 fSelectionMaterial.Blend.fSrc:=GL_SRC_ALPHA;
 fSelectionMaterial.Blend.fDst:=GL_ONE_MINUS_SRC_ALPHA;

 fRenderingMaterial.DepthTest.DepthTest:=true;
 fRenderingMaterial.DepthTest.DepthMask:=true;

 fSelectionMaterial.DepthTest.DepthTest:=true;
 fSelectionMaterial.DepthTest.DepthMask:=true;

 fRenderingMaterial.VertexArray.VertexArray:=false;
 fRenderingMaterial.VertexArray.ColorArray:=false;
 fRenderingMaterial.VertexArray.TexCoord0Array:=false;
 fRenderingMaterial.VertexArray.TexCoord1Array:=false;
 fRenderingMaterial.VertexArray.NormalArray:=false;

 fSelectionMaterial.VertexArray.VertexArray:=false;
 fSelectionMaterial.VertexArray.ColorArray:=false;
 fSelectionMaterial.VertexArray.TexCoord0Array:=false;
 fSelectionMaterial.VertexArray.TexCoord1Array:=false;
 fSelectionMaterial.VertexArray.NormalArray:=false;

 fRenderingMaterial.TexStg.UseTex0:=false;
 fRenderingMaterial.TexStg.UseTex1:=false;
 fRenderingMaterial.TexStg.texture0:=nil;
 fRenderingMaterial.TexStg.texture1:=nil;

 fSelectionMaterial.TexStg.UseTex0:=false;
 fSelectionMaterial.TexStg.UseTex1:=false;
 fSelectionMaterial.TexStg.texture0:=nil;
 fSelectionMaterial.TexStg.texture1:=nil;

 fRenderingMaterial.VPSettings.UseVP:=false;
 fRenderingMaterial.VPSettings.VP:=0;

 fSelectionMaterial.VPSettings.UseVP:=false;
 fSelectionMaterial.VPSettings.VP:=0;

 fRenderingMaterial.Color[0]:=1;
 fRenderingMaterial.Color[1]:=1;
 fRenderingMaterial.Color[2]:=1;
 fRenderingMaterial.Color[3]:=1;

 fSelectionMaterial.Color[0]:=1;
 fSelectionMaterial.Color[1]:=1;
 fSelectionMaterial.Color[2]:=1;
 fSelectionMaterial.Color[3]:=1;
end;

destructor TBasicSceneObject.destroy;
begin
 if fRenderingMaterial<>nil then ClearMaterial(fRenderingMaterial);
 if fSelectionMaterial<>nil then ClearMaterial(fSelectionMaterial);
 inherited;
end;

{ TTexture }

{ TTileset }

constructor TTileset.Create;
begin
 inherited Create;
 fSubTex:=nil;
end;

destructor TTileset.destroy;
begin
 fSubTex:=nil;
 inherited;
end;

end.
