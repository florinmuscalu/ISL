{In progress... add more xSL functions}
{de elimitat glGet!!!!!!!!!!}
unit UCamera;
interface
uses UVectors, xSL, URC, UFormManager;
type
 TCamera=class;
 TFrustum=array[0..5]of TVector4;
 TValidateProc=procedure(const OldPos, NewPos:TVector3; var Pos:TVector3) of object;
 TResetProc=procedure(var sender:TCamera) of object;
 TMoveProc=procedure of Object;

 TCamera=class
        private
         f2D:boolean;

         fYSpeed:single;
         fCanJump:boolean;
         fY0:single;
         fTime:single;
         fGravity:single;
         fDeltaTime:single;
         fResetY:single;
         
         fOldPos:TVector3;
         fPos:TVector3;
         fView:TVector3;
         fUp:TVector3;
         fUpVector:TVector3;
         fFront:TVector3;
         fRight:TVector3;
         fZNear, fZFar, fFovY:single;
         fWidth, fHeight:integer;
         fCanMove:boolean;
         fMoved:boolean;

         fFwdKey:cardinal;
         fBackKey:cardinal;
         fLeftKey:cardinal;
         fRightKey:cardinal;
         fRunKey:cardinal;
         fJumpKey:cardinal;

         fWalkSpeed:single;
         fRunSpeed:single;
         fJumpSpeed:single;

         fFrustum:TFrustum;
         fHeading:single;
         fTilt:single;
         fMouseSpeed:single;
         fEnableCollision:boolean;
         procedure setZNear(value:single);
         procedure setZFar(value:single);
         procedure setFovY(value:single);
         procedure SetCanMove(value:boolean);
         procedure setPos(value:TVector3);
         procedure setView(value:TVector3);
         procedure UpdateVectors;

         function _CameraBindFwd(Args: TArray; var error: pchar): Variant;
         function _CameraBindBack(Args: TArray; var error: pchar): Variant;
         function _CameraBindLeft(Args: TArray; var error: pchar): Variant;
         function _CameraBindRight(Args: TArray; var error: pchar): Variant;
         function _CameraBindRun(Args: TArray; var error: pchar): Variant;
         function _CameraBindJump(Args: TArray; var error: pchar): Variant;
         function _CameraWalkSpeed(Args: TArray; var error: pchar): Variant;
         function _CameraRunSpeed(Args: TArray; var error: pchar): Variant;
         function _CameraJumpSpeed(Args: TArray; var error: pchar): Variant;
         function _CameraGravity(Args: TArray; var error: pchar): Variant;
         Function _CameraSetPos(Args:TArray; var error:pchar):Variant;
         Function _CameraSetLens (Args:TArray; var error:pchar):Variant;
         Function _CameraSetHeading(Args:TArray; var error:pchar):Variant;
         Function _CameraSetTilt(Args:TArray; var error:pchar):Variant;
        public
          //procedura de validare a miscarii
         ValidateProc:TValidateProc;

         //procedura de resetare a pozitiei
         ResetProc:TResetProc;

         //procedura apelata la fiecare apel al procedurii Look
         LookProc:TMoveProc;

         destructor destroy; override;
         constructor create;

         //pozitioneaza camera. Apelata la fiecare frame.
         procedure Look;

         //Setarea camerei.
         procedure SetupLens(Width, Height:integer; zNear:single=0.05; zFar:single=50; fovY:single=45);

         //Updatare (Fiecare Frame);
         procedure Update(const deltatime:single);

         //seteaza sau citeste zNear
         property ZNear:single read fZNear write setZNear;

         //seteaza sau citeste zFar
         property ZFar :single read fZFar  write setZFar;

         //seteaza sau citeste fovY
         property fovY :single read ffovY  write setFovY;

         //CanMove-true=> miscarea permisa. Alfel miscarea nu este posibila
         property CanMove:boolean read fCanMove write setCanMove;

         //citeste sau seteaza Pozitia
         property Position:TVector3 read fPos write setPos;

         //citeste sau seteaza Directia in care se priveste
         property ViewVector:TVector3 read fView write setView;

         //citeste Vectorul 'Inainte'
         property Front:TVector3 read fFront;

         //citeste Vectorul 'Sus'
         property Up:TVector3 read fUpVector;

         //citeste Vectorul 'Dreapta'
         property Right:TVector3 read fRight;

         //citeste sau scrie directia orizontala in care se priveste, in radiani
         property Heading:single read fHeading write fHeading;

         //citeste sau scrie directia verticala in care se priveste, in radiani
         property Tilt:single read fTilt write fTilt;

         //citeste sau scrie tasta de deplasare inainte
         property ForwardKey:cardinal read fFwdKey write fFwdKey;

        //citeste sau scrie tasta de deplasare inapoi
         property BackKey:cardinal read fBackKey write fBackKey;

         //citeste sau scrie tasta de deplasare la stanga
         property LeftKey:cardinal read fLeftKey write fLeftKey;

         //citeste sau scrie tasta de deplasare la dreapta
         property RightKey:cardinal read fRightKey write fRightKey;

         //citeste sau scrie tasta de transformare a mersului in alergare
         property RunKey:cardinal read fRunKey write fRunKey;

         property Width:integer read fWidth;
         property Height:integer read fHeight;

         //citeste sau scrie tasta de sarire
         property JumpKey:cardinal read fJumpKey write fJumpKey;

         //citeste sau scrie viteaza de mers (m/s)
         property WalkSpeed:single read fWalkSpeed write fWalkSpeed;

         //citeste sau scrie viteaza de alergare (m/s)
         property RunSpeed:single read fRunSpeed write fRunSpeed;

         //citeste sau scrie viteaza de deplasare a mouse-lui (senzitivitatea)
         property MouseSpeed:single read fMouseSpeed write fMouseSpeed;

         //citeste sau scrie viteaza sariturii (m/s)
         property JumpSpeed:single read fJumpSpeed write fJumpSpeed;

         //citeste sau scrie daca sunt permise coliziunile cu lumea
         property EnableCollision:boolean read fEnableCollision write fEnableCollision;

         //verifica daca un punct e in Frustum
         function PointInFrustum(const V:TVector3):boolean;

         //verifica daca o sfera e in Frustum
         function SphereInFrustum(const V:TVector3; const radius:single):boolean;

         //verifica daca un cub e in Frustum
         function CubeInFrustum(const V:TVector3; const size:single):boolean;

         //verifica daca un paralelipiped e in Frustum
         function BoxInFrustum(const Box:T3DBox):boolean;

         //verifica daca un triunghi e in Frustum
         function TriangleInFrustum(const V1,V2,V3:TVector3):boolean;

         //verifica daca un elipsoid e in Frustum
         function ElipsoidInFrustum(const C,R:TVector3):boolean;

         //seteaza pozitia si orientarea camerei
         procedure SetPosition(const Pos:TVector3; const Heading:single=0; const Tilt:single=0);

         //adauga functiile pentru xSL
         procedure AddScriptFunctions(Script:PScript);

         //eye-space distance to a point
         Function EyeDistance(const P:TVector3):single;

         procedure set2D;
         procedure unSet2D;
 end;
implementation
uses Windows, dglOpenGL, URenderer;
const
 frustum_RIGHT=0;        // The RIGHT side of the frustum
 frustum_LEFT=1;         // The LEFT  side of the frustum
 frustum_BOTTOM=2;       // The BOTTOM side of the frustum
 frustum_TOP=3;          // The TOP side of the frustum
 frustum_BACK=4;         // The BACK side of the frustum
 frustum_FRONT=5;        // The FRONT side of the frustum
 plane_A=0;              // The X value of the plane's normal
 plane_B=1;              // The Y value of the plane's normal
 plane_C=2;              // The Z value of the plane's normal
 plane_D=3;              // The distance the plane is from the origin
 
{ TCamera }

constructor TCamera.create;
begin
 inherited Create;
 f2D:=false;
 fPos:=MakeVector3(9.1388597488, 2, 4.3762993813);
 fView:=MakeVector3(9.1133823395, 1.9061384201, 5.3715586662);
 fUp:=MakeVector3(0,1,0);
 fZNear:=0.03;
 fZFar:=50;
 ffovY:=45;
 fCanMove:=true;
 fFwdKey:=VK_UP;
 fBackKey:=VK_DOWN;
 fLeftKey:=VK_LEFT;
 fRightKey:=VK_RIGHT;
 fRunKey:=VK_SHIFT;
 fJumpKey:=VK_SPACE;
 fWalkSpeed:=1.39;  // 5    Km/s
 fRunSpeed:=6;      // 21.6 Km/s   
 fJumpSpeed:=20;    // 75   Km/h    
 fHeading:=3.1159999371;
 fTilt:=0;
 ValidateProc:=nil;
 LookProc:=nil;
 fEnableCollision:=false;
 fTime:=0;
 fGravity:=9.8;
 ResetProc:=nil;
 fCanJump:=true;
 fResetY:=-30;
 fMouseSpeed:=1;
 SetupLens(glwWidth,glwHeight, zNear, ZFar, fovY);
 GLWAppendLog('Camera created.');
 fMoved:=true;
end;

procedure TCamera.setPos;
var
 V:TVector3;
begin
 Vector3Subtract(Value, fPos, V);
 fPos:=Value;
 Vector3Add(fView, V, fView);
end;

destructor TCamera.destroy;
begin
 inherited;
 GLWAppendLog('Camera destroyed.');
end;

procedure TCamera.SetPosition;
begin
 fPos:=Pos;
 fHeading:=Heading;
 fTilt:=Tilt;
end;

procedure TCamera.SetCanMove;
var
 MouseCoor : TPoint;
begin
 MouseCoor.x := fWidth div 2;
 MouseCoor.y := fHeight div 2;
 ClientToScreen(glwWinHandle, MouseCoor);
 fCanMove:=value;
 SetCursorPos(MouseCoor.X, MouseCoor.Y);
end;

function TCamera.PointInFrustum;
var
 i:integer;
begin
 for i:= 0 to 5 do
        if planeEvaluatePoint(fFrustum[i],V)<=0 then begin
            result:=false;
            exit;
        end;
 result:=true;
end;

function TCamera.SphereInFrustum;
var
 i:integer;
begin
 result:=true;
 for i:= 0 to 5 do
        if planeEvaluatePoint(fFrustum[i],V)<=-radius then begin
                result:=false;
                exit;
        end;
end;

function TCamera.CubeInFrustum;
var
 Box:T3DBox;
begin
 Box.center:=V;
 Box.Width:=size;
 Box.Height:=size;
 Box.Depth:=size;
 result:=BoxInFrustum(Box);
end;

function TCamera.TriangleInFrustum;
var
 i:integer;
begin
 for i:=0 to 5 do begin
        if planeEvaluatePoint(fFrustum[i],V1)>=0 then continue;
        if planeEvaluatePoint(fFrustum[i],V2)>=0 then continue;
        if planeEvaluatePoint(fFrustum[i],V3)>=0 then continue;
        result:=false;
        exit;
 end;
 result:=true;
end;

function TCamera.BoxInFrustum;
var
 i:integer;
begin
 if PointInFrustum(Box.Center) then begin
        result:=true;
        exit;
 end;
 for i:= 0 to 5 do begin
       if planeEvaluatePoint(fFrustum[i],MakeVector3(Box.Center[0]-Box.Width/2,Box.Center[1]-Box.Height/2,Box.Center[2]-Box.Depth/2))>0 then continue;
       if planeEvaluatePoint(fFrustum[i],MakeVector3(Box.Center[0]+Box.Width/2,Box.Center[1]+Box.Height/2,Box.Center[2]+Box.Depth/2))>0 then continue;

       if planeEvaluatePoint(fFrustum[i],MakeVector3(Box.Center[0]+Box.Width/2,Box.Center[1]-Box.Height/2,Box.Center[2]-Box.Depth/2))>0 then continue;
       if planeEvaluatePoint(fFrustum[i],MakeVector3(Box.Center[0]-Box.Width/2,Box.Center[1]+Box.Height/2,Box.Center[2]+Box.Depth/2))>0 then continue;

       if planeEvaluatePoint(fFrustum[i],MakeVector3(Box.Center[0]-Box.Width/2,Box.Center[1]+Box.Height/2,Box.Center[2]-Box.Depth/2))>0 then continue;
       if planeEvaluatePoint(fFrustum[i],MakeVector3(Box.Center[0]+Box.Width/2,Box.Center[1]-Box.Height/2,Box.Center[2]+Box.Depth/2))>0 then continue;

       if planeEvaluatePoint(fFrustum[i],MakeVector3(Box.Center[0]-Box.Width/2,Box.Center[1]-Box.Height/2,Box.Center[2]+Box.Depth/2))>0 then continue;
       if planeEvaluatePoint(fFrustum[i],MakeVector3(Box.Center[0]+Box.Width/2,Box.Center[1]+Box.Height/2,Box.Center[2]-Box.Depth/2))>0 then continue;
       result:=false;
       exit;
 end;
 result:=true;
end;

procedure TCamera.UpdateVectors;
var
 modl, proj, res:TMatrix;
begin
 if not fMoved then exit;
 fMoved:=false;
 fUp[0]:=0;
 fUp[1]:=1;
 fUp[2]:=0;

 Vector3Subtract(fView, fPos, fFront);
 Vector3Normalize(fFront);
 GL.glGetFloatv(GL_PROJECTION_MATRIX,@proj);
 GL.glGetFloatv(GL_MODELVIEW_MATRIX,@modl);

 fUpVector[0]:=modl[0,1];
 fUpVector[1]:=modl[1,1];
 fUpVector[2]:=modl[2,1];
 Vector3Normalize(fUpVector);
 fRight[0]:=modl[0,0];
 fRight[1]:=modl[1,0];
 fRight[2]:=modl[2,0];
 NormalizeVector3(fRight);

 res:=MatrixMultiply(modl, proj);
 fFrustum[frustum_RIGHT,plane_A]:=res[0][3]-res[0][0];
 fFrustum[frustum_RIGHT,plane_B]:=res[1][3]-res[1][0];
 fFrustum[frustum_RIGHT,plane_C]:=res[2][3]-res[2][0];
 fFrustum[frustum_RIGHT,plane_D]:=res[3][3]-res[3][0];
 UVectors.NormalizePlane(fFrustum[frustum_RIGHT]);

 fFrustum[frustum_LEFT,plane_A]:=res[0][3]+res[0][0];
 fFrustum[frustum_LEFT,plane_B]:=res[1][3]+res[1][0];
 fFrustum[frustum_LEFT,plane_C]:=res[2][3]+res[2][0];
 fFrustum[frustum_LEFT,plane_D]:=res[3][3]+res[3][0];
 UVectors.NormalizePlane(fFrustum[frustum_LEFT]);

 fFrustum[frustum_TOP,plane_A]:=res[0][3]-res[0][1];
 fFrustum[frustum_TOP,plane_B]:=res[1][3]-res[1][1];
 fFrustum[frustum_TOP,plane_C]:=res[2][3]-res[2][1];
 fFrustum[frustum_TOP,plane_D]:=res[3][3]-res[3][1];
 UVectors.NormalizePlane(fFrustum[frustum_TOP]);

 fFrustum[frustum_BOTTOM,plane_A]:=res[0][3]+res[0][1];
 fFrustum[frustum_BOTTOM,plane_B]:=res[1][3]+res[1][1];
 fFrustum[frustum_BOTTOM,plane_C]:=res[2][3]+res[2][1];
 fFrustum[frustum_BOTTOM,plane_D]:=res[3][3]+res[3][1];
 UVectors.NormalizePlane(fFrustum[frustum_BOTTOM]);

 fFrustum[frustum_BACK,plane_A]:=res[0][3]+res[0][2];
 fFrustum[frustum_BACK,plane_B]:=res[1][3]+res[1][2];
 fFrustum[frustum_BACK,plane_C]:=res[2][3]+res[2][2];
 fFrustum[frustum_BACK,plane_D]:=res[3][3]+res[3][2];
 UVectors.NormalizePlane(fFrustum[frustum_BACK]);

 fFrustum[frustum_FRONT,plane_A]:=res[0][3]-res[0][2];
 fFrustum[frustum_FRONT,plane_B]:=res[1][3]-res[1][2];
 fFrustum[frustum_FRONT,plane_C]:=res[2][3]-res[2][2];
 fFrustum[frustum_FRONT,plane_D]:=res[3][3]-res[3][2];
 UVectors.NormalizePlane(fFrustum[frustum_FRONT]);
end;

procedure TCamera.Look;
begin
 GL.glLoadIdentity;
 GL.gluLookAt(fPos[0],fPos[1],fPos[2],fView[0],fView[1],fView[2],0,1,0);
 UpdateVectors;
 if assigned(LookProc) then LookProc;
end;

procedure TCamera.SetupLens(Width, Height:integer; zNear:single=0.05; zFar:single=50; fovY:single=45);
var
  MouseCoor : TPoint;
begin
 if ZNear<0 then ZNear:=fZNear
            else fZNear:=zNear;
 if ZFar<0 then ZFar:=fZFar
           else fZFar:=zFar;
 if (Height = 0) then Height := 1;
 GL.glViewport(0, 0, Width, Height);                        // Set the viewport for the OpenGL window
 fWidth:=Width;
 fHeight:=Height;
 fFovY:=fovY;
 GL.glMatrixMode(GL_PROJECTION);                   // Change Matrix Mode to Projection
 GL.glLoadIdentity();
 GL.gluPerspective(fovy, Width/Height, zNear, zFar);
 GL.glMatrixMode(GL_MODELVIEW);                    // Return to the modelview matrix
 GL.glLoadIdentity();

 MouseCoor.x := fWidth div 2;
 MouseCoor.y := fHeight div 2;
 ClientToScreen(glwWinHandle, MouseCoor);
 SetCursorPos(MouseCoor.X, MouseCoor.Y);
end;

procedure TCamera.setZNear(value:single);
begin
 if value<>fZNear then SetupLens(fWidth, fHeight, value, fZFar, fFovY);
end;

procedure TCamera.setZFar(value:single);
begin
 if value<>fZFar then SetupLens(fWidth, fHeight, fZNear, value, fFovY);
end;

procedure TCamera.setFovY(value:single);
begin
 if value<>fFovY then SetupLens(fWidth, fHeight, fZNear, fZFar, value);
end;

procedure TCamera.Update;
var
 v:TVector3;
 speed:single;
 time:single;
 P:TPoint;
 aX, aY:single;
 y:single;
 MouseCoor:TPoint;
begin
 fDeltaTime:=deltatime;
 if not fCanMove then exit;

 if glwIsKeyDown(fRunKey) then speed:=fRunSpeed
                           else speed:=fWalkSpeed;
 time:=deltatime/1000;

 fOldPos:=fPos;
 if glwIsKeyDown(fFwdKey) then begin
        Vector3Subtract(fView, fPos, V);
        Vector3Normalize(V);
        Vector3Scale(V, speed,V);
        Vector3Scale(V,time,V);
        Vector3Add(fPos, V, fPos);
        Vector3Add(fView, V, fView);
        fMOved:=true;
 end;

 if glwIsKeyDown(fBackKey) then begin
        Vector3Subtract(fPos, fView, V);
        Vector3Normalize(V);
        Vector3Scale(V, speed,V);
        Vector3Scale(V,time,V);
        Vector3Add(fPos, V, fPos);
        Vector3Add(fView, V, fView);
        fMOved:=true;
 end;

 if glwIsKeyDown(fLeftKey) then begin
        speed:=speed/2;
        Vector3Subtract(fView, fPos, V);
        V:=Vector3CrossProduct(fUp,V);
        Vector3Normalize(V);
        Vector3Scale(V, speed,V);
        Vector3Scale(V,time,V);
        Vector3Add(fPos, V, fPos);
        Vector3Add(fView, V, fView);
        fMOved:=true;
 end;

 if glwIsKeyDown(fRightKey) then begin
        speed:=speed/2;
        Vector3Subtract(fPos, fView, V);
        V:=Vector3CrossProduct(fUp,V);
        Vector3Normalize(V);
        Vector3Scale(V, speed,V);
        Vector3Scale(V,time,V);
        Vector3Add(fPos, V, fPos);
        Vector3Add(fView, V, fView);
        fMOved:=true;
 end;

 fPos[1]:=fOldPos[1];
 fView[1]:=fOldPos[1];

 if fEnableCollision and assigned(ValidateProc) then begin
        ValidateProc(fOldPos, fPos, fPos);
        if fYSpeed=0 then begin
                if glwisKeyDown(fJumpKey) then begin
                        if fCanJump then begin
                                fYSpeed:=fJumpSpeed;
                                fY0:=fPos[1];
                                fTime:=0;
                                fCanJump:=false;
                                fMOved:=true;
                        end
                end
                else fCanJump:=true;
        end;
        fTime:=fTime+deltatime/1000;
        fOldPos:=fPos;
        fPos[1]:=fY0-fYSpeed*fTime+fGravity*fTime*fTime;
        y:=fPos[1];
        ValidateProc(fOldPos, fPos, fPos);

        if abs(fpos[1]-Y)>0.001 then begin
                fYSpeed:=0;
                fTime:=deltaTime/1000;
                fY0:=fpos[1];
        end;

        if fPos[1]<=fResetY then begin
                if assigned(ResetProc) then ResetProc(self);
                exit;
        end;
 end;

 V:=MakeVector3(0,0,-1);
 if switch=0 then begin
    GetCursorPos(p);
    MouseCoor.x := fWidth div 2;
    MouseCoor.y := fHeight div 2;
    ClientToScreen(glwWinHandle, MouseCoor);
    SetCursorPos(MouseCoor.X, MouseCoor.Y);
 end
 else begin
    MouseCoor.x := fWidth div 2;
    MouseCoor.y := fHeight div 2;
    p.Y:=MouseCoor.Y;
    p.X:=MouseCoor.X;
 end;
 ax:=0; ay:=0;
 if (MouseCoor.Y<>P.Y) then begin
        fMoved:=true;
        aY:=((MouseCoor.Y-p.Y)/500)*fMouseSpeed;
 end;
 if (MouseCoor.X<>P.X) then begin
        fMoved:=true;
        aX:=((MouseCoor.X-p.X)/500)*fMouseSpeed;
 end;


 fTilt:=fTilt+ay;
 if fTilt>1.04719755 then fTilt:=1.04719755;
 if fTilt<-1.04719755 then fTilt:=-1.04719755;
 V:=VectorRotateAroundX(V, -fTilt);
 fHeading:=fHeading+ax;
 V:=VectorRotateAroundY(V, fHeading);

 Vector3Add(FPos, V, fView);
end;

procedure TCamera.setView(value: TVector3);
begin
 fView:=value;
end;

//script functions
procedure TCamera.AddScriptFunctions;
begin
 Script.AddFunction('CameraBindBack',1,_CameraBindBack);
 Script.AddFunction('CameraBindFwd',1,_CameraBindFwd);
 Script.AddFunction('CameraBindLeft',1,_CameraBindLeft);
 Script.AddFunction('CameraBindRight',1,_CameraBindRight);
 Script.AddFunction('CameraBindJump',1,_CameraBindJump);
 Script.AddFunction('CameraBindRun',1,_CameraBindRun);
 Script.AddFunction('CameraWalkSpeed',1,_CameraWalkSpeed);
 Script.AddFunction('CameraRunSpeed',1,_CameraRunSpeed);
 Script.AddFunction('CameraJumpSpeed',1,_CameraJumpSpeed);
 Script.AddFunction('CameraGravity',1,_CameraGravity);
 Script.AddFunction ('CameraSetPos',3,_CameraSetPos);
 Script.AddFunction ('CameraSetHeading',1,_CameraSetHeading);
 Script.AddFunction ('CameraSetTilt',1,_CameraSetTilt);
 Script.AddFunction ('CameraSetLens',3,_CameraSetLens);
end;

function TCamera._CameraBindBack(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 self.fBackKey:=Args[0];
end;

function TCamera._CameraBindFwd(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 self.fFwdKey:=Args[0];
end;

function TCamera._CameraBindJump(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 self.fJumpKey:=Args[0];
end;

function TCamera._CameraBindLeft(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 self.fLeftKey:=Args[0];
end;

function TCamera._CameraBindRight(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 self.fRightKey:=Args[0];
end;

function TCamera._CameraBindRun(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 self.fRunKey:=Args[0];
end;

function TCamera._CameraGravity(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 self.fGravity:=Args[0];
end;

function TCamera._CameraJumpSpeed(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 self.fJumpSpeed:=Args[0];
end;

function TCamera._CameraRunSpeed(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 self.fRunSpeed:=Args[0];
end;

function TCamera._CameraWalkSpeed(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 self.fWalkSpeed:=Args[0];
end;

function TCamera._CameraSetPos(Args: TArray; var error: pchar): Variant;
 var
 p:TVector3;
begin
 error:='';
 p[0]:=Args[0];
 p[1]:=Args[1];
 p[2]:=Args[2];
 Position:=P;
 result:=0;
end;

function TCamera._CameraSetLens(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 ZNear:=Args[0];
 ZFar :=Args[1];
 fovY :=Args[2];
 result:=0;
end;

function TCamera._CameraSetHeading(Args: TArray;
  var error: pchar): Variant;
begin
 error:='';
 self.Heading:=Args[0];
end;

function TCamera._CameraSetTilt(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 self.Tilt:=Args[0];
end;

function TCamera.ElipsoidInFrustum(const C, R: TVector3): boolean;
var
 i:integer;
begin
 if PointInFrustum(C) then begin
        result:=true;
        exit;
 end;
 for i:= 0 to 5 do begin
       if planeEvaluatePoint(fFrustum[i],MakeVector3(C[0]-R[0],C[1]-R[1],C[2]-R[2]))>0 then continue;
       if planeEvaluatePoint(fFrustum[i],MakeVector3(C[0]+R[0],C[1]+R[1],C[2]+R[2]))>0 then continue;

       if planeEvaluatePoint(fFrustum[i],MakeVector3(C[0]+R[0],C[1]-R[1],C[2]-R[2]))>0 then continue;
       if planeEvaluatePoint(fFrustum[i],MakeVector3(C[0]-R[0],C[1]+R[1],C[2]+R[2]))>0 then continue;

       if planeEvaluatePoint(fFrustum[i],MakeVector3(C[0]-R[0],C[1]+R[1],C[2]-R[2]))>0 then continue;
       if planeEvaluatePoint(fFrustum[i],MakeVector3(C[0]+R[0],C[1]-R[1],C[2]+R[2]))>0 then continue;

       if planeEvaluatePoint(fFrustum[i],MakeVector3(C[0]-R[0],C[1]-R[1],C[2]+R[2]))>0 then continue;
       if planeEvaluatePoint(fFrustum[i],MakeVector3(C[0]+R[0],C[1]+R[1],C[2]-R[2]))>0 then continue;
       result:=false;
       exit;
 end;
 result:=true;
end;

function TCamera.EyeDistance(const P: TVector3): single;
begin
 result:=planeEvaluatePoint(fFrustum[4], P);
end;

procedure TCamera.set2D;
begin
 if not Renderer.RC.IsGLActive then exit;
 if f2D then exit;
 f2D:=true;
 GL.glMatrixMode(GL_PROJECTION);  // Change Matrix Mode to Projection
 GL.glPushMatrix;
 GL.glLoadIdentity();             // Reset View
 GL.gluOrtho2D(0, glwWidth, glwHeight, 0);
 GL.glMatrixMode(GL_MODELVIEW);   // Change Projection to Matrix Mode
 GL.glPushMatrix;
 GL.glLoadIdentity;
end;

procedure TCamera.unSet2D;
begin
 if not Renderer.RC.IsGLActive then exit;
 if not f2D then exit;
 f2D:=false;
 GL.glMatrixMode(GL_PROJECTION);  // Change Matrix Mode to Projection
 GL.glPopMatrix;
 GL.glMatrixMode(GL_MODELVIEW);   // Change Projection to Matrix Mode
 GL.glPopMatrix;
end;

end.
