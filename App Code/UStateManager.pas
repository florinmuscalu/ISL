{In progress}
////////////////////////////////////////////////////////////////////////////////
//              Unit ce se ocupa de:
//                      - managementul starilor binare GL
//                      - managementul starilor binare Client
//                      - Orientarea Fetelor (sensul acelor de ceasornic sau nu)
//                      - Setarea modului de randare ( Polygon Mode)
//                      - Setarea texturii curente
//                      - setarea Vertex Progam-ului curent
//                      - Setarea culorii curente
//                      - setarea formulei pentru blend
//                      - seteaza culoare de stergere a ecranului
//                      - seteaza valoare cu care se sterge depth bufferul
//                      - Push si Pop pentru diverse grupuri de atribute
//
//              O parte din Cod este luat din GLScene
////////////////////////////////////////////////////////////////////////////////

unit UStateManager;
interface
uses SysUtils, dglOpenGL, UTypes, UMaterials;
const
 cGLStateToGLEnum : array [stBlend..stRegisterCombiners] of TGLEnum =(GL_BLEND,
        GL_CULL_FACE, GL_DEPTH_TEST, GL_FOG, GL_SCISSOR_TEST, GL_STENCIL_TEST,
        GL_REGISTER_COMBINERS_NV);

 cGLClientStateToGLEnum:array[stVertexArray..stNormalArray]of TGLEnum=(GL_VERTEX_ARRAY,
        GL_COLOR_ARRAY,
        GL_TEXTURE_COORD_ARRAY,
        GL_TEXTURE_COORD_ARRAY,
        GL_NORMAL_ARRAY);

type
 TStateManager=class
        private
         fParent:TObject;

         FStates : TGLStates;
         FClientStates : TGLClientStates;
         FLastFrontMode, FLastBackMode : TGLEnum;
         FFrontFaceCCW : Boolean;
         fDepthMask:boolean;

         //Managemetul starilor binare
//         procedure SetGLState(const aState : TGLState); stdcall;
         procedure UnSetGLState(const aState : TGLState); stdcall;
         procedure ResetGLStates; stdcall;

         procedure UnSetGLClientState(const aState : TGLClientState); stdcall;
         procedure ResetGLClientStates; stdcall;

         //resetaza toate setarile

        public
         procedure ResetAll; stdcall;
         procedure SetGLState(const aState : TGLState); stdcall;
         //modul de randare ( Polygon Mode)
         procedure SetGLPolygonMode(const aFace, mode : cardinal); stdcall;
         procedure ResetGLPolygonMode; stdcall;

         //orientarea fetelor
         procedure InvertGLFrontFace; stdcall;
         procedure ResetGLFrontFace; stdcall;  //resetaza la default (CCW)
         procedure SetGLFrontFaceCW; stdcall;
         procedure SetGLFrontFaceCCW; stdcall;

         //setaza culoarea curenta
         procedure SetColor(const r,g,b:single; const a:single=1); stdcall;

         //seteaza culoare cu care se sterge ecranul
         procedure SetClearColor(const r,g,b:single; const a:single=0); stdcall;

         //seteaza valoare cu care se sterge depth bufferul
         procedure SetClearDepth(const d:single=1); stdcall;

         //setarea formulei pentru blend
         constructor Create(parent:TObject);
         destructor Destroy; override;

         procedure ApplyMaterial(Mat:PMaterial);
 end;

implementation
uses Urc, UFormManager;

constructor TStateManager.Create;
begin
 inherited create;
 fParent:=Parent;
 FFrontFaceCCW:=True;
 GL.glDepthFunc(GL_LEQUAL);
 SetClearDepth;
 SetClearColor(0,0,0);
 ResetAll;
 GL.glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
 fDepthMask:=true;
 GL.glDepthMask(true);
end;

destructor TStateManager.Destroy;
begin

end;

procedure TStateManager.SetGLState(const aState : TGLState);
begin
 if not (aState in FStates) then begin
        Include(FStates, aState);
        GL.glEnable(cGLStateToGLEnum[aState]);
 end;
end;

procedure TStateManager.UnSetGLState(const aState : TGLState);
begin
 if (aState in FStates) then begin
        Exclude(FStates, aState);
        GL.glDisable(cGLStateToGLEnum[aState]);
 end;
end;

procedure TStateManager.SetGLPolygonMode(const aFace, mode : TGLEnum);
begin
 case aFace of
        GL_FRONT : if mode<>FLastFrontMode then begin
                        FLastFrontMode:=mode;
                        GL.glPolygonMode(aFace, mode);
        end;
        GL_BACK : if mode<>FLastBackMode then begin
                        FLastBackMode:=mode;
                        GL.glPolygonMode(aFace, mode);
        end;
        GL_FRONT_AND_BACK : if (mode<>FLastFrontMode) or (mode<>FLastBackMode) then begin
                        FLastFrontMode:=mode;
                        FLastBackMode:=mode;
                        GL.glPolygonMode(aFace, mode);
        end;
 end;
end;

procedure TStateManager.ResetGLPolygonMode;
begin
 SetGLPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
end;

procedure TStateManager.InvertGLFrontFace;
begin
 FFrontFaceCCW:=not FFrontFaceCCW;
 if FFrontFaceCCW then GL.glFrontFace(GL_CCW)
                  else GL.glFrontFace(GL_CW);
end;

procedure TStateManager.ResetGLFrontFace;
begin
 GL.glFrontFace(GL_CCW);
 FFrontFaceCCW:=True;
end;

procedure TStateManager.SetGLFrontFaceCW;
begin
 if FFrontFaceCCW then begin
        GL.glFrontFace(GL_CW);
        FFrontFaceCCW:=False;
 end;
end;

procedure TStateManager.SetGLFrontFaceCCW;
begin
 if not FFrontFaceCCW then begin
        GL.glFrontFace(GL_CCW);
        FFrontFaceCCW:=True;
 end;
end;

procedure TStateManager.ResetAll;
begin
 ResetGLPolygonMode;
 ResetGLFrontFace;
 ResetGLStates;
 ResetGLClientStates;
end;

procedure TStateManager.SetColor(const r, g, b, a: single);
begin
 GL.glColor4f(r,g,b,a);
end;

procedure TStateManager.ResetGLStates;
begin
 FStates:=[stBlend, stFog, stScissorTest, stStencilTest, stRegisterCombiners];
 unSetGLState(stRegisterCombiners);
 unSetGLState(stBlend);
 unSetGLState(stFog);
 unSetGLState(stScissorTest);
 unSetGLState(stStencilTest);

 SetGLState(stCullFace);
 SetGLState(stDepthTest);
end;

procedure TStateManager.SetClearColor(const r, g, b, a: single);
begin
 GL.glClearColor(r,g,b,a);
end;

procedure TStateManager.SetClearDepth(const d: single);
begin
 GL.glClearDepth(d);
end;

procedure TStateManager.ResetGLClientStates;
begin
 FClientStates:=[stVertexArray, stColorArray, stTexCoordArray0, stTexCoordArray1];
 unSetGLClientState(stVertexArray   );
 unSetGLClientState(stColorArray    );
 unSetGLClientState(stTexCoordArray0);
 unSetGLClientState(stTexCoordArray1);
end;

procedure TStateManager.UnSetGLClientState(const aState: TGLClientState);
begin
 if (aState in FClientStates) then begin
        Exclude(FClientStates, aState);
        if aState=stTexCoordArray0 then GL.glClientActiveTexture(GL_TEXTURE0)
                                   else if aState=stTexCoordArray1 then GL.glClientActiveTexture(GL_TEXTURE1);
        GL.glDisableClientState(cGLClientStateToGLEnum[aState]);
 end;
end;

procedure TStateManager.ApplyMaterial(Mat: PMaterial);
begin
 //blending
 if Mat.Blend.Use then begin
        if not(stBlend in FStates) then begin
                Include(FStates, stBlend);
                GL.glEnable(GL_BLEND);
        end;
        GL.glBlendFunc(Mat.Blend.fSrc, Mat.Blend.fDst);
 end
 else begin
        if (stBlend in FStates) then begin
                Exclude(FStates, stBlend);
                GL.glDisable(GL_BLEND);
        end;
 end;
 //culling
 if Mat.UseCulling then begin
        if not(stCullFace in FStates) then begin
                Include(FStates, stCullFace);
                GL.glEnable(GL_CULL_FACE);
        end;
 end
 else begin
        if (stCullFace in FStates) then begin
                Exclude(FStates, stCullFace);
                GL.glDisable(GL_CULL_FACE);
        end;
 end;
 //depthTest
 if Mat.DepthTest.DepthTest then begin
        if not(stDepthTest in FStates) then begin
                Include(FStates, stDepthTest);
                GL.glEnable(GL_DEPTH_TEST);
        end;
 end
 else begin
        if (stDepthTest in FStates) then begin
                Exclude(FStates, stDepthTest);
                GL.glDisable(GL_DEPTH_TEST);
        end;
 end;
 //Fog
 if Mat.UseFog then begin
        if not(stFog in FStates) then begin
                Include(FStates, stFog);
                GL.glEnable(GL_FOG);
        end;
 end
 else begin
        if (stFog in FStates) then begin
                Exclude(FStates, stFog);
                GL.glDisable(GL_FOG);
        end;
 end;
 //ScissorTest
 if Mat.UseScissorTest then begin
        if not(stScissorTest in FStates) then begin
                Include(FStates, stScissorTest);
                GL.glEnable(GL_SCISSOR_TEST);
        end;
 end
 else begin
        if (stScissorTest in FStates) then begin
                Exclude(FStates, stScissorTest);
                GL.glDisable(GL_SCISSOR_TEST);
        end;
 end;
 //StencilTest
 if Mat.UseStencilTest then begin
        if not(stStencilTest in FStates) then begin
                Include(FStates, stStencilTest);
                GL.glEnable(GL_STENCIL_TEST);
        end;
 end
 else begin
        if (stStencilTest in FStates) then begin
                Exclude(FStates, stStencilTest);
                GL.glDisable(GL_STENCIL_TEST);
        end;
 end;
 //RegisterCombiners
 if Mat.UseRegisterCombiners then begin
        if not(stRegisterCombiners in FStates) then begin
                Include(FStates, stRegisterCombiners);
                GL.glEnable(GL_REGISTER_COMBINERS_NV);
        end;
 end
 else begin
        if (stRegisterCombiners in FStates) then begin
                Exclude(FStates, stRegisterCombiners);
                GL.glDisable(GL_REGISTER_COMBINERS_NV);
        end;
 end;
 //VertexArray
 if Mat.VertexArray.VertexArray then begin
        if not(stVertexArray in FClientStates) then begin
                Include(FClientStates, stVertexArray);
                GL.glEnable(GL_VERTEX_ARRAY);
        end;
 end
 else begin
        if (stVertexArray in FClientStates) then begin
                Exclude(FClientStates, stVertexArray);
                GL.glDisable(GL_VERTEX_ARRAY);
        end;
 end;
 //ColorArray
 if Mat.VertexArray.ColorArray then begin
        if not(stColorArray in FClientStates) then begin
                Include(FClientStates, stColorArray);
                GL.glEnable(GL_COLOR_ARRAY);
        end;
 end
 else begin
        if (stColorArray in FClientStates) then begin
                Exclude(FClientStates, stColorArray);
                GL.glDisable(GL_COLOR_ARRAY);
        end;
 end;
 //TexCoords0
 if Mat.VertexArray.TexCoord0Array then begin
        if not(stTexCoordArray0 in FClientStates) then begin
                Include(FClientStates, stTexCoordArray0);
                GL.glClientActiveTexture(GL_TEXTURE0);
                GL.glEnable(GL_TEXTURE_COORD_ARRAY);
        end;
 end
 else begin
        if (stTexCoordArray0 in FClientStates) then begin
                Exclude(FClientStates, stTexCoordArray0);
                GL.glClientActiveTexture(GL_TEXTURE0);
                GL.glDisable(GL_TEXTURE_COORD_ARRAY);
        end;
 end;
 //TexCoords1
 if Mat.VertexArray.TexCoord1Array then begin
        if not(stTexCoordArray1 in FClientStates) then begin
                Include(FClientStates, stTexCoordArray1);
                GL.glClientActiveTexture(GL_TEXTURE1);
                GL.glEnable(GL_TEXTURE_COORD_ARRAY);
        end;
 end
 else begin
        if (stTexCoordArray1 in FClientStates) then begin
                Exclude(FClientStates, stTexCoordArray1);
                GL.glClientActiveTexture(GL_TEXTURE1);
                GL.glDisable(GL_TEXTURE_COORD_ARRAY);
        end;
 end;
 //NormalArray
 if Mat.VertexArray.NormalArray then begin
        if not(stNormalArray in FClientStates) then begin
                Include(FClientStates, stNormalArray);
                GL.glEnable(GL_NORMAL_ARRAY);
        end;
 end
 else begin
        if (stNormalArray in FClientStates) then begin
                Exclude(FClientStates, stNormalArray);
                GL.glDisable(GL_NORMAL_ARRAY);
        end;
 end;
 //DepthMask
 if Mat.DepthTest.DepthMask then begin
        if not fDepthMask then begin
                fDepthMask:=true;
                GL.glDepthMask(true);
        end;
 end
 else begin
        if fDepthMask then begin
                fDepthMask:=false;
                GL.glDepthMask(false);
        end;
 end;
 GL.glColor3fv(@Mat.Color);
end;

end.
