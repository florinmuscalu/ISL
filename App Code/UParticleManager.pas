unit UParticleManager;
interface
uses UVectors, dglOpenGL, URC, UCamera, UTypes, UVertex, xSL;
const
 TSelectionNameBase=100000;
type
 TShape=(Point, Sphere, SphereSurface, Cube, CilinderY, CilinderX, CilinderZ);
 TShapeInfo=Class
 end;

 TSphere=class(TShapeInfo)
        radius:glFloat;
 end;
 TCube=class(TShapeInfo)
        MinX, MaxX, MinY, MaxY, MinZ, MaxZ:glFloat;
 end;
 TCilinderY=class(TShapeInfo)
        minY, MaxY:glFloat;
        radius:glFloat;
 end;
 TCilinderZ=class(TShapeInfo)
        minZ, MaxZ:glFloat;
        radius:glFloat;
 end;
 TCilinderX=class(TShapeInfo)
        minX, MaxX:glFloat;
        radius:glFloat;
 end;
 TPrimitiveType=(PT_Points, PT_Lines, PT_BillBoard);

 TPoint=record
        Color:array[0..3]of gluByte;
        Pos:TVector3;
 end;
 TLine=record
        StartPos, EndPos:TPoint;
 end;
 
 TInitInfo=class
        Gx:glFloat;     //gravity on x
        Gy:glFloat;     //gravity on y
        Gz:glFloat;     //gravity on z

        PMaxSpeedX, PMinSpeedX:glFLoat;
        PMaxSpeedY, PMinSpeedY:glFLoat;
        PMaxSpeedZ, PMinSpeedZ:glFLoat;

        PMinXAcc, PMaxXAcc:glFLoat;
        PMinYAcc, PMaxYAcc:glFLoat;
        PMinZAcc, PMaxZAcc:glFLoat;

        PMinSize, PMaxSize:glFloat;

        PMinEnergy, PMaxEnergy:glFloat;
        PInitShape:TShape;
        PShapeInfo:TShapeInfo;

        PrimitiveType:TPrimitiveType;
        Color:array[0..3]of glFloat;
        UseTexture:boolean;
        Texture:shortstring;
        NrOfParticles:cardinal; 
        SRCBlendMode, DSTBlendMode:glUint;
        SizeDecrease:boolean;
        SizeIncrease:boolean;

        DepthSort:Boolean;

        DepthTest:boolean;
 end;

 //base class for a particle system
 TBasicParticleSystem=class(TBasicSceneObject)
        private
         fBSphere:TBSphere;
         fDead:boolean;
         fName:cardinal;
         fID:cardinal;
         fNrAlive:cardinal;
        public
         procedure Update; virtual; abstract;
         property Dead:boolean read fDead;
         function IsVisible:boolean;
 end;

 TParticleManager=class(TObject)
        private
         fSystems:array of TBasicParticleSystem;
         fSystemCount:cardinal;
    function GetSystem(index: integer): TBasicParticleSystem;
    procedure SetSystem(index: integer; const Value: TBasicParticleSystem);
        public
         constructor create;
         procedure Clear;
         destructor destroy; override;
         function AddSystem(const System:TBasicParticleSystem):integer;
         procedure RemoveSystem(const ID:cardinal);
         procedure Update;
         function DoesExists(const ID:Cardinal):boolean;
         property System[index:integer]:TBasicParticleSystem read GetSystem write SetSystem;
         procedure AddScriptFunctions(Script: PScript);
         function _AddSystem(Args: TArray; var error: pchar): Variant;
         function _LoadFromFile(Args: TArray; var error: pchar): Variant;
         function _SetPosition(Args: TArray; var error: pchar): Variant;
 end;

 TParticle=object
         fPos:TVector3;
         fOldPos:TVector3;
         fVelocity:TVector3;
         fColor:array[0..2]of glFloat;
         fEnergy, fInitialEnergy:glFloat;
         fSize, fInitialSize:glFloat;
         fAccX, fAccY, fAccZ:glFloat;
 end;

 TParticleSystem=class(TBasicParticleSystem)
        private
         fVBOBuffer:glUint;
         fPosition:TVector3;
         fParticles:array of TParticle;
         fEyeDist:array of single;
         fLine:array of TLine;
         fPoint:array of TPoint;
         fRect:array of TParticleQuad;
         fInitInfo:TInitInfo;

         procedure QSort(const min, max:integer);
         procedure SetParticleDefaults(const i:cardinal);
         procedure UpdateParticle(const i:integer; const delta:single); virtual;
         procedure setInitInfo(const value:TinitInfo);

         procedure CalculateBillBoard(const Position:TVector3; const W,H:glFLoat; out s:TParticleQuad);
        public
         constructor create(Position:TVector3; ID:cardinal);
         procedure Update; override;
         procedure LoadFromFile(name:string);
         procedure SaveToFile(name:string);
         procedure Render; override;
         destructor destroy; override;
         property InitInfo:TInitInfo read fInitInfo write setInitInfo;
 end;

function MakeFire:TInitInfo;
function MakeNiceFire:TInitInfo;
function MakeStars:TInitInfo;

implementation
uses UFormManager, Classes, URenderer, sysutils, Windows, IniFiles;

function MakeStars:TInitInfo;
begin
 result:=TInitInfo.Create;
 result.Gx:=0;
 result.Gy:=0;
 result.Gz:=0;
 result.PMaxSpeedX:=0;
 result.PMinSpeedX:=0;
 result.PMaxSpeedZ:=0;
 result.PMinSpeedZ:=0;
 result.PMaxSpeedY:=0;
 result.PMinSpeedY:=0;
 result.PMinXAcc:=0;
 result.PMaxXAcc:=0;
 result.PMinYAcc:=0;
 result.PMaxYAcc:=0;
 result.PMinZAcc:=0;
 result.PMaxZAcc:=0;
 result.Texture:='';
 result.PMinSize:=0;
 result.PMaxSize:=0;
 result.PMinEnergy:=10000;
 result.PMaxEnergy:=1000000;
 result.PInitShape:=SphereSurface;
 result.PrimitiveType:=PT_POINTS;
 result.PShapeInfo:=TSphere.Create;
 (result.PShapeInfo as TSphere).radius:=20;
 result.Color[0]:=1;
 result.Color[1]:=0.5;
 result.Color[2]:=1;
 result.Color[3]:=1;
 result.UseTexture:=false;
 result.NrOfParticles:=1000;
 result.SRCBlendMode:=GL_SRC_ALPHA;
 result.DSTBlendMode:=GL_ONE;
 result.SizeDecrease:=false;
 result.SizeIncrease:=false;

 result.DepthSort:=false;
 result.DepthTest:=false;
end;

function MakeFire:TInitInfo;
begin
 result:=TInitInfo.Create;
 result.Gx:=0;
 result.Gy:=-1.7;
 result.Gz:=0;
 result.PMaxSpeedX:=0.15;
 result.PMinSpeedX:=-0.15;
 result.PMaxSpeedZ:=0.15;
 result.PMinSpeedZ:=-0.15;
 result.PMaxSpeedY:=1.3;
 result.PMinSpeedY:=0.8;
 result.PMinXAcc:=-0.1;
 result.PMaxXAcc:=0.1;
 result.PMinYAcc:=0;
 result.PMaxYAcc:=0;
 result.PMinZAcc:=-0.1;
 result.PMaxZAcc:=0.1;
 result.Texture:='particle.tga';
 result.PMinSize:=0.2;
 result.PMaxSize:=0.5;
 result.PMinEnergy:=0;
 result.PMaxEnergy:=0.5;
 result.PInitShape:=Point;
 result.PrimitiveType:=PT_Billboard;
 result.PShapeInfo:=nil;
 result.Color[0]:=1;
 result.Color[1]:=0.5;
 result.Color[2]:=0;
 result.Color[3]:=1;
 result.UseTexture:=true;
 result.NrOfParticles:=50;
 result.SRCBlendMode:=GL_SRC_ALPHA;
 result.DSTBlendMode:=GL_ONE;
 result.SizeDecrease:=false;
 result.SizeIncrease:=false;

 result.DepthSort:=true;
 result.DepthTest:=false;
end;

function MakeNiceFire:TInitInfo;
begin
 result:=TInitInfo.Create;
 result.Gx:=0;
 result.Gy:=-1.7;
 result.Gz:=0;
 result.PMaxSpeedX:=0.15;
 result.PMinSpeedX:=-0.15;
 result.PMaxSpeedZ:=0.15;
 result.PMinSpeedZ:=-0.15;
 result.PMaxSpeedY:=1.5;
 result.PMinSpeedY:=0.8;
 result.PMinXAcc:=-0.1;
 result.PMaxXAcc:=0.1;
 result.PMinYAcc:=0;
 result.PMaxYAcc:=0;
 result.PMinZAcc:=-0.1;
 result.PMaxZAcc:=0.1;
 result.Texture:='particle.tga';
 result.PMinSize:=0.2;
 result.PMaxSize:=0.5;
 result.PMinEnergy:=0;
 result.PMaxEnergy:=0.5;
 result.PInitShape:=Point;
 result.PrimitiveType:=PT_Billboard;
 result.PShapeInfo:=nil;
 result.Color[0]:=1;
 result.Color[1]:=0.5;
 result.Color[2]:=1;
 result.Color[3]:=1;
 result.UseTexture:=true;
 result.NrOfParticles:=100;
 result.SRCBlendMode:=GL_SRC_ALPHA;
 result.DSTBlendMode:=GL_ONE;
 result.SizeDecrease:=false;
 result.SizeIncrease:=false;

 result.DepthSort:=true;
 result.DepthTest:=false;
end;

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////TParticleSystem////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
procedure TParticleSystem.setInitInfo;
var
 i:integer;
begin
 fInitInfo:=value;


 fRenderingMaterial.Color[0]:=value.Color[0];
 fRenderingMaterial.Color[1]:=value.Color[1];
 fRenderingMaterial.Color[2]:=value.Color[2];
 fRenderingMaterial.Color[3]:=value.Color[3];
 fSelectionMaterial.Color[0]:=value.Color[0];
 fSelectionMaterial.Color[1]:=value.Color[1];
 fSelectionMaterial.Color[2]:=value.Color[2];
 fSelectionMaterial.Color[3]:=value.Color[3];
 fRenderingMaterial.DepthTest.DepthMask:=value.DepthTest;
 fSelectionMaterial.DepthTest.DepthMask:=value.DepthTest;
 if value.UseTexture then begin
        Renderer.RC.fTexManager.DeleteTexture(fRenderingMaterial.texStg.Texture0);
        fRenderingMaterial.TexStg.texture0:=Renderer.RC.fTexManager.LoadTexture(pchar('PSystems\'+value.Texture), 0.9, false, -1, true);
        fRenderingMaterial.TexStg.UseTex0:=true;
 end;
 setlength(fParticles, value.NrOfParticles);
 fPoint:=nil;
 fLine:=nil;
 fRect:=nil;
 fEyeDist:=nil;
 if value.PrimitiveType=PT_Points then begin
        setlength(fPoint, value.NrOfParticles);
        if value.DepthSort then setlength(fEyeDist, value.NrOfParticles);
 end;
 if value.PrimitiveType=PT_Lines then begin
        setlength(fLine, value.NrOfParticles);
        if value.DepthSort then setlength(fEyeDist, value.NrOfParticles);
 end;
 if value.PrimitiveType=PT_BillBoard then begin
        setlength(fRect, value.NrOfParticles);
        if value.DepthSort then setlength(fEyeDist, value.NrOfParticles);
        for i:=0 to high(fRect) do begin
                fRect[i].V1.TexCoord[0]:=0; fRect[i].V1.TexCoord[1]:=0;
                fRect[i].V2.TexCoord[0]:=1; fRect[i].V2.TexCoord[1]:=0;
                fRect[i].V3.TexCoord[0]:=1; fRect[i].V3.TexCoord[1]:=1;
                fRect[i].V4.TexCoord[0]:=0; fRect[i].V4.TexCoord[1]:=1;
        end;
 end;
 for i:=0 to value.NrOfParticles-1 do SetParticleDefaults(i);
 fNrAlive:=value.NrOfParticles;
end;

procedure TParticleSystem.SaveToFile(name:string);
var
 ini:TIniFile;
 f:string;
begin
 if fInitInfo=nil then exit;
 try
    f:=glwAppDir+'Data\PSystems\'+name;
    deletefile(pchar(f));
    ini:=TIniFile.Create(f);
    with fInitInfo do begin
        ini.WriteString('Main', 'Version', 'Particle Sistem version 1.0');
        Ini.WriteFloat('Main', 'Gx', Gx);
        Ini.WriteFloat('Main', 'Gy', Gy);
        Ini.WriteFloat('Main', 'Gz', Gz);

        Ini.WriteFloat('Main', 'PMaxSpeedX', PMaxSpeedX);
        Ini.WriteFloat('Main', 'PMinSpeedX', PMinSpeedX);
        Ini.WriteFloat('Main', 'PMaxSpeedY', PMaxSpeedY);
        Ini.WriteFloat('Main', 'PMinSpeedY', PMinSpeedY);
        Ini.WriteFloat('Main', 'PMaxSpeedZ', PMaxSpeedZ);
        Ini.WriteFloat('Main', 'PMinSpeedZ', PMinSpeedZ);

        Ini.WriteFloat('Main', 'PMinXAcc', PMinXAcc);
        Ini.WriteFloat('Main', 'PMaxXAcc', PMaxXAcc);
        Ini.WriteFloat('Main', 'PMinYAcc', PMinYAcc);
        Ini.WriteFloat('Main', 'PMaxYAcc', PMaxYAcc);
        Ini.WriteFloat('Main', 'PMinZAcc', PMinZAcc);
        Ini.WriteFloat('Main', 'PMaxZAcc', PMaxZAcc);

        Ini.WriteFloat('Main', 'PMinSize', PMinSize);
        Ini.WriteFloat('Main', 'PMaxSize', PMaxSize);

        Ini.WriteFloat('Main', 'PMinEnergy', PMinEnergy);
        Ini.WriteFloat('Main', 'PMaxEnergy', PMaxEnergy);

        if PInitShape=Point then Ini.WriteString('Main', 'PInitShape', 'Point');
        if PInitShape=Sphere then Ini.WriteString('Main', 'PInitShape', 'Sphere');
        if PInitShape=SphereSurface then Ini.WriteString('Main', 'PInitShape', 'SphereSurface');
        if PInitShape=Cube then Ini.WriteString('Main', 'PInitShape', 'Cube');
        if PInitShape=CilinderY then Ini.WriteString('Main', 'PInitShape', 'CilinderY');
        if PInitShape=CilinderX then Ini.WriteString('Main', 'PInitShape', 'CilinderX');
        if PInitShape=CilinderZ then Ini.WriteString('Main', 'PInitShape', 'CilinderZ');

        if PrimitiveType=PT_Points then Ini.WriteString('Main', 'PrimitiveType', 'PT_Points');
        if PrimitiveType=PT_Lines then Ini.WriteString('Main', 'PrimitiveType', 'PT_Lines');
        if PrimitiveType=PT_BillBoard then Ini.WriteString('Main', 'PrimitiveType', 'PT_BillBoard');

        Ini.WriteFloat('Main', 'ColorR', Color[0]);
        Ini.WriteFloat('Main', 'ColorG', Color[1]);
        Ini.WriteFloat('Main', 'ColorB', Color[2]);
        Ini.WriteFloat('Main', 'ColorA', Color[3]);

        Ini.WriteBool('Main', 'UseTexture', UseTexture);
        Ini.WriteString('Main', 'Texture', Texture);
        Ini.WriteInteger('Main', 'NrOfParticles', NrOfParticles);

        Ini.WriteInteger('Main', 'SRCBlendMode', SRCBlendMode);
        Ini.WriteInteger('Main', 'DSTBlendMode', DSTBlendMode);

        Ini.WriteBool('Main', 'SizeIncrease', SizeIncrease);
        Ini.WriteBool('Main', 'SizeDecrease', SizeDecrease);

        if PInitShape=Sphere then ini.WriteFloat('Main', 'Radius',(PShapeInfo as TSphere).radius);
        if PInitShape=SphereSurface then ini.WriteFloat('Main', 'Radius',(PShapeInfo as TSphere).radius);

        if PInitShape=Cube then begin
                ini.WriteFloat('Main', 'MinX',(PShapeInfo as TCube).MinX);
                ini.WriteFloat('Main', 'MaxX',(PShapeInfo as TCube).MaxX);
                ini.WriteFloat('Main', 'MinY',(PShapeInfo as TCube).MinY);
                ini.WriteFloat('Main', 'MaxY',(PShapeInfo as TCube).MaxY);
                ini.WriteFloat('Main', 'MinZ',(PShapeInfo as TCube).MinZ);
                ini.WriteFloat('Main', 'MaxZ',(PShapeInfo as TCube).MaxZ);
        end;
        if PInitShape=CilinderX then begin
                ini.WriteFloat('Main', 'MinX',(PShapeInfo as TCilinderX).minX);
                ini.WriteFloat('Main', 'MaxX',(PShapeInfo as TCilinderX).MaxX);
                ini.WriteFloat('Main', 'Radius',(PShapeInfo as TCilinderX).radius);
        end;
        if PInitShape=CilinderY then begin
                ini.WriteFloat('Main', 'MinY',(PShapeInfo as TCilinderY).MinY);
                ini.WriteFloat('Main', 'MaxY',(PShapeInfo as TCilinderY).MaxY);
                ini.WriteFloat('Main', 'Radius',(PShapeInfo as TCilinderY).radius);
        end;
        if PInitShape=CilinderZ then begin
                ini.WriteFloat('Main', 'MinZ',(PShapeInfo as TCilinderZ).MinZ);
                ini.WriteFloat('Main', 'MaxZ',(PShapeInfo as TCilinderZ).MaxZ);
                ini.WriteFloat('Main', 'Radius',(PShapeInfo as TCilinderZ).radius);
        end;
        Ini.WriteBool('Main', 'DepthSort', DepthSort);
        Ini.WriteBool('Main', 'DepthTest', DepthTest);
    end;
    ini.Free;
 except end;
end;

procedure TParticleSystem.LoadFromFile(name:string);
var
 ini:TIniFile;
 f:string;
 shape, ptype:string;
 i:integer;
begin
 if fInitInfo=nil then fInitInfo:=TInitInfo.Create;

 try
    f:=glwAppDir+'Data\PSystems\'+name;
    ini:=TIniFile.Create(f);
    with fInitInfo do begin
        Gx:=Ini.ReadFloat('Main', 'Gx', 0);
        Gy:=Ini.ReadFloat('Main', 'Gy', 0);
        Gz:=Ini.ReadFloat('Main', 'Gz', 0);

        PMaxSpeedX:=Ini.ReadFloat('Main', 'PMaxSpeedX', 0);
        PMinSpeedX:=Ini.ReadFloat('Main', 'PMinSpeedX', 0);
        PMaxSpeedY:=Ini.ReadFloat('Main', 'PMaxSpeedY', 0);
        PMinSpeedY:=Ini.ReadFloat('Main', 'PMinSpeedY', 0);
        PMaxSpeedZ:=Ini.ReadFloat('Main', 'PMaxSpeedZ', 0);
        PMinSpeedZ:=Ini.ReadFloat('Main', 'PMinSpeedZ', 0);

        PMinXAcc:=Ini.ReadFloat('Main', 'PMinXAcc', 0);
        PMaxXAcc:=Ini.ReadFloat('Main', 'PMaxXAcc', 0);
        PMinYAcc:=Ini.ReadFloat('Main', 'PMinYAcc', 0);
        PMaxYAcc:=Ini.ReadFloat('Main', 'PMaxYAcc', 0);
        PMinZAcc:=Ini.ReadFloat('Main', 'PMinZAcc', 0);
        PMaxZAcc:=Ini.ReadFloat('Main', 'PMaxZAcc', 0);

        PMinSize:=Ini.ReadFloat('Main', 'PMinSize', 0);
        PMaxSize:=Ini.ReadFloat('Main', 'PMaxSize', 0);

        PMinEnergy:=Ini.ReadFloat('Main', 'PMinEnergy', 0);
        PMaxEnergy:=Ini.ReadFloat('Main', 'PMaxEnergy', 0);

        shape:=Ini.ReadString('Main', 'PInitShape', 'Point');
        if shape='Point' then PInitShape:=Point;
        if shape='Sphere' then PInitShape:=Sphere;
        if shape='SphereSurface' then PInitShape:=SphereSurface;
        if shape='Cube' then PInitShape:=Cube;
        if shape='CilinderY' then PInitShape:=CilinderY;
        if shape='CilinderX' then PInitShape:=CilinderX;
        if shape='CilinderZ' then PInitShape:=CilinderZ;

        ptype:=Ini.ReadString('Main', 'PrimitiveType', 'PT_Points');
        if ptype='PT_Points' then PrimitiveType:=PT_Points;
        if ptype='PT_Lines' then PrimitiveType:=PT_Lines;
        if ptype='PT_BillBoard' then PrimitiveType:=PT_BillBoard;

        Color[0]:=Ini.ReadFloat('Main', 'ColorR', 0);
        Color[1]:=Ini.ReadFloat('Main', 'ColorG', 0);
        Color[2]:=Ini.ReadFloat('Main', 'ColorB', 0);
        Color[3]:=Ini.ReadFloat('Main', 'ColorA', 0);

        UseTexture:=Ini.ReadBool('Main', 'UseTexture', false);
        Texture:=Ini.ReadString('Main', 'Texture', '');
        if UseTexture then begin
                Renderer.Rc.fTexManager.DeleteTexture(fRenderingMaterial.texStg.texture0);
                fRenderingMaterial.TexStg.texture0:=Renderer.Rc.fTexManager.LoadTexture(pchar('PSystems\'+Texture), 0.9, false, -1, true);
                fRenderingMaterial.TexStg.UseTex0:=true;
        end;
        NrOfParticles:=Ini.ReadInteger('Main', 'NrOfParticles', 0);

        if PrimitiveType=PT_Points then setlength(fPoint, NrOfParticles);
        if PrimitiveType=PT_Lines then setlength(fLine, NrOfParticles);
        if PrimitiveType=PT_BillBoard then begin
                setlength(fRect, NrOfParticles);
                for i:=0 to high(fRect) do begin
                        fRect[i].V1.TexCoord[0]:=0; fRect[i].V1.TexCoord[1]:=0;
                        fRect[i].V2.TexCoord[0]:=1; fRect[i].V2.TexCoord[1]:=0;
                        fRect[i].V3.TexCoord[0]:=1; fRect[i].V3.TexCoord[1]:=1;
                        fRect[i].V4.TexCoord[0]:=0; fRect[i].V4.TexCoord[1]:=1;
                end;
        end;

        SRCBlendMode:=Ini.ReadInteger('Main', 'SRCBlendMode', 0);
        DSTBlendMode:=Ini.ReadInteger('Main', 'DSTBlendMode', 0);

        SizeIncrease:=Ini.ReadBool('Main', 'SizeIncrease', false);
        SizeDecrease:=Ini.ReadBool('Main', 'SizeDecrease', false);
        fRenderingMaterial.Blend.fSrc:=SRCBlendMode;
        fRenderingMaterial.Blend.fDst:=DSTBlendMode;

        PShapeInfo:=nil;
        if PInitShape=Sphere then begin
                PShapeInfo:=TSphere.Create;
                (PShapeInfo as TSphere).radius:=ini.ReadFloat('Main', 'Radius',0);
        end;
        if PInitShape=SphereSurface then begin
                PShapeInfo:=TSphere.Create;
                (PShapeInfo as TSphere).radius:=ini.ReadFloat('Main', 'Radius',0);
        end;
        if PInitShape=Cube then begin
                PShapeInfo:=TCube.Create;

                (PShapeInfo as TCube).MinX:=ini.ReadFloat('Main', 'MinX',0);
                (PShapeInfo as TCube).MaxX:=ini.ReadFloat('Main', 'MaxX',0);
                (PShapeInfo as TCube).MinY:=ini.ReadFloat('Main', 'MinY',0);
                (PShapeInfo as TCube).MaxY:=ini.ReadFloat('Main', 'MaxY',0);
                (PShapeInfo as TCube).MinZ:=ini.ReadFloat('Main', 'MinZ',0);
                (PShapeInfo as TCube).MaxZ:=ini.ReadFloat('Main', 'MaxZ',0);
        end;
        if PInitShape=CilinderX then begin
                PShapeInfo:=TCilinderX.Create;
                (PShapeInfo as TCilinderX).minX:=ini.ReadFloat('Main', 'MinX',0);
                (PShapeInfo as TCilinderX).MaxX:=ini.ReadFloat('Main', 'MaxX',0);
                (PShapeInfo as TCilinderX).radius:=ini.ReadFloat('Main', 'Radius',0);
        end;
        if PInitShape=CilinderY then begin
                PShapeInfo:=TCilinderY.Create;
                (PShapeInfo as TCilinderY).MinY:=ini.ReadFloat('Main', 'MinY',0);
                (PShapeInfo as TCilinderY).MaxY:=ini.ReadFloat('Main', 'MaxY',0);
                (PShapeInfo as TCilinderY).radius:=ini.ReadFloat('Main', 'Radius',0);
        end;
        if PInitShape=CilinderZ then begin
                PShapeInfo:=TCilinderZ.Create;
                (PShapeInfo as TCilinderZ).MinZ:=ini.ReadFloat('Main', 'MinZ',0);
                (PShapeInfo as TCilinderZ).MaxZ:=ini.ReadFloat('Main', 'MaxZ',0);
                (PShapeInfo as TCilinderZ).radius:=ini.ReadFloat('Main', 'Radius',0);
        end;
        DepthSort:=Ini.ReadBool('Main', 'DepthSort', false);
        DepthTest:=Ini.ReadBool('Main', 'DepthTest', false);
        fRenderingMaterial.DepthTest.DepthTest:=True;
        fRenderingMaterial.DepthTest.DepthMask:=DepthTest;

        fNrAlive:=NrOfParticles;
        setlength(fParticles, NrOfParticles);
        if depthSort then setlength(fEyeDist, NrOfParticles)
                     else setlength(fEyeDist, 0);

        for i:=0 to NrOfParticles-1 do SetParticleDefaults(i);
    end;
    ini.Free;
 except end;
end;

destructor TParticleSystem.destroy;
begin
 fParticles:=nil;
 if fInitInfo.PShapeInfo<>nil then fInitInfo.PShapeInfo.Free;
 fInitInfo.Free;
 fRect:=nil;
 fLine:=nil;
 fPoint:=nil;
 fEyeDist:=nil;
 glwAppendLog('Particle System '+inttostr(fID)+' destroyed.');
 inherited;
end;

constructor TParticleSystem.create;
begin
 inherited create;
 fDead:=false;
 fPosition:=Position;
 fTransformMatrix:=MatrixTranslate4(Position[0], Position[1], Position[2]);

 fParticles:=nil;
 fNrAlive:=0;
 fBSphere.C:=MakeVector3(0,0,0);
 fBSphere.R:=0;
 fID:=ID;
 fRect:=nil;
 fLine:=nil;
 fPoint:=nil;
 fEyeDist:=nil;
 fInitInfo:=nil;
 GL.glGenBuffersARB(1, @fVBOBuffer);
 glwAddErrCode(400,'Error reading from file!');
 glwAddErrCode(401,'Different Particle System File Version!');

 fRenderingMaterial.Blend.Use:=true;

 fRenderingMaterial.DynamicalyIluminated:=false;
 fSelectionMaterial.DynamicalyIluminated:=false;

 fRenderingMaterial.UseFog:=false;
 fSelectionMaterial.UseFog:=false;
 fRenderingMaterial.UseCulling:=true;
 fSelectionMaterial.UseCulling:=true;
 fRenderingMaterial.UseScissorTest:=false;
 fSelectionMaterial.UseScissorTest:=false;
 fRenderingMaterial.UseStencilTest:=false;
 fSelectionMaterial.UseStencilTest:=false;
 fRenderingMaterial.UseRegisterCombiners:=false;
 fSelectionMaterial.UseRegisterCombiners:=false;
 glwAppendLog('Particle System '+inttostr(fID)+' created.');
end;

procedure TParticleSystem.Update;
var
 delta:single;
 i:integer;
 minx, maxx, miny, maxy, minz, maxz:glFLoat;
 mx, my, mz:single;
begin
 delta:=Renderer.DeltaTime/1000;   //transform miliseconds in seconds
 fNrAlive:=length(fparticles);

 fBSphere.C:=MakeVector3(0,0,0);
 fBSphere.R:=0; minx:=10000000000; maxx:=-10000000000;
 miny:=10000000000; maxy:=-10000000000;
 minz:=10000000000; maxz:=-10000000000;
 
 for i:=0 to high(fParticles) do begin
        UpdateParticle(i, delta);
        if fParticles[i].fEnergy>0 then begin
                if fParticles[i].fPos[0]-fParticles[i].fSize/2<minx then minx:=fParticles[i].fPos[0]-fParticles[i].fSize/2;
                if fParticles[i].fPos[0]+fParticles[i].fSize/2>maxx then maxx:=fParticles[i].fPos[0]+fParticles[i].fSize/2;

                if fParticles[i].fPos[1]-fParticles[i].fSize/2<miny then miny:=fParticles[i].fPos[1]-fParticles[i].fSize/2;
                if fParticles[i].fPos[1]+fParticles[i].fSize/2>maxy then maxy:=fParticles[i].fPos[1]+fParticles[i].fSize/2;

                if fParticles[i].fPos[2]-fParticles[i].fSize/2<minz then minz:=fParticles[i].fPos[2]-fParticles[i].fSize/2;
                if fParticles[i].fPos[2]+fParticles[i].fSize/2>maxz then maxz:=fParticles[i].fPos[2]+fParticles[i].fSize/2;
        end;
 end;

 if fNrAlive<>0 then begin
        mx:= abs(maxx-minx);
        my:=abs(maxy-miny);
        mz:= abs(maxz-minz);
        fBSphere.C[0]:=minx+mx/2;
        fBSphere.C[1]:=miny+my/2;
        fBSphere.C[2]:=minz+mz/2;
//        AddVector3(fBSphere.C,fPosition);
        if (mx>my)and(mx>mz) then
                fBSphere.R:=mx/2
        else
        if (my>mx)and(my>mz) then
                fBSphere.R:=my/2
        else fBSphere.R:=mz/2;
 end
 else begin
        fBSphere.C:=MakeVector3(0,0,0);
        fBSphere.R:=0;
 end;
 if fNrAlive=0 then fDead:=true;
end;

procedure TParticleSystem.SetParticleDefaults;
begin
 case fInitInfo.PInitShape of
        Point: begin
                fParticles[i].fPos[0]:=fPosition[0];
                fParticles[i].fPos[1]:=fPosition[1];
                fParticles[i].fPos[2]:=fPosition[2];
        end;
        Cube: begin
                fParticles[i].fPos[0]:=random(round((fInitInfo.PShapeInfo as TCube).MaxX-(fInitInfo.PShapeInfo as TCube).minX)*1000)/1000+(fInitInfo.PShapeInfo as TCube).minX;
                fParticles[i].fPos[1]:=random(round((fInitInfo.PShapeInfo as TCube).maxY-(fInitInfo.PShapeInfo as TCube).minY)*1000)/1000+(fInitInfo.PShapeInfo as TCube).minY;
                fParticles[i].fPos[2]:=random(round((fInitInfo.PShapeInfo as TCube).maxZ-(fInitInfo.PShapeInfo as TCube).minZ)*1000)/1000+(fInitInfo.PShapeInfo as TCube).minZ;
        end;
        SphereSurface:begin
                RandomPointOnSphere(fParticles[i].fPos);
                ScaleVector3(fParticles[i].fPos,(fInitInfo.PShapeInfo as TSphere).radius);
        end;
        Sphere:begin
                RandomPointOnSphere(fParticles[i].fPos);
                ScaleVector3(fParticles[i].fPos,random(trunc((fInitInfo.PShapeInfo as TSphere).radius*1000)*1000)/1000000);
        end;
        CilinderX:begin
                RandomPointOnSphere(fParticles[i].fPos);
                ScaleVector3(fParticles[i].fPos,random(trunc((fInitInfo.PShapeInfo as TCilinderX).radius*1000)*1000)/1000000);
                fParticles[i].fPos[0]:=random(round((fInitInfo.PShapeInfo as TCilinderX).MaxX-(fInitInfo.PShapeInfo as TCilinderX).minX)*1000)/1000+(fInitInfo.PShapeInfo as TCilinderX).minX;
        end;
        CilinderY:begin
                RandomPointOnSphere(fParticles[i].fPos);
                ScaleVector3(fParticles[i].fPos,random(trunc((fInitInfo.PShapeInfo as TCilinderY).radius*1000)*1000)/1000000);
                fParticles[i].fPos[0]:=random(round((fInitInfo.PShapeInfo as TCilinderY).MaxY-(fInitInfo.PShapeInfo as TCilinderY).minY)*1000)/1000+(fInitInfo.PShapeInfo as TCilinderY).minY;
        end;
        CilinderZ:begin
                RandomPointOnSphere(fParticles[i].fPos);
                ScaleVector3(fParticles[i].fPos,random(trunc((fInitInfo.PShapeInfo as TCilinderZ).radius*1000)*1000)/1000000);
                fParticles[i].fPos[0]:=random(round((fInitInfo.PShapeInfo as TCilinderZ).MaxZ-(fInitInfo.PShapeInfo as TCilinderZ).minZ)*1000)/1000+(fInitInfo.PShapeInfo as TCilinderZ).minZ;
        end;
 end;
 fParticles[i].fOldPos:=fParticles[i].fPos;
 fParticles[i].fVelocity[0]:=random(round((fInitInfo.PMaxSpeedX-fInitInfo.PMinSpeedX)*1000))/1000+fInitInfo.PMinSpeedX;
 fParticles[i].fVelocity[1]:=random(round((fInitInfo.PMaxSpeedY-fInitInfo.PMinSpeedY)*1000))/1000+fInitInfo.PMinSpeedY;
 fParticles[i].fVelocity[2]:=random(round((fInitInfo.PMaxSpeedZ-fInitInfo.PMinSpeedZ)*1000))/1000+fInitInfo.PMinSpeedZ;

 fParticles[i].fEnergy:=random(round((fInitInfo.PMaxEnergy-fInitInfo.PMinEnergy)*1000))/1000+fInitInfo.PMinEnergy;
 while fParticles[i].fEnergy=0 do fParticles[i].fEnergy:=random(round((fInitInfo.PMaxEnergy-fInitInfo.PMinEnergy)*1000))/1000+fInitInfo.PMinEnergy;
 fParticles[i].fInitialEnergy:=fParticles[i].fEnergy;
 
 fParticles[i].fSize:=random(round((fInitInfo.PMaxSize-fInitInfo.PMinSize)*1000))/1000+fInitInfo.PMinSize;
 fParticles[i].fInitialSize:=fParticles[i].fSize;
 fParticles[i].fAccX:=random(round((fInitInfo.PMaxXAcc-fInitInfo.PMinXAcc)*1000))/1000+fInitInfo.PMinXAcc;
 fParticles[i].fAccY:=random(round((fInitInfo.PMaxYAcc-fInitInfo.PMinYAcc)*1000))/1000+fInitInfo.PMinYAcc;
 fParticles[i].fAccZ:=random(round((fInitInfo.PMaxZAcc-fInitInfo.PMinZAcc)*1000))/1000+fInitInfo.PMinZAcc;

 fParticles[i].fColor[0]:=fInitInfo.color[0];
 fParticles[i].fColor[1]:=fInitInfo.color[1];
 fParticles[i].fColor[2]:=fInitInfo.color[2];
end;

procedure TParticleSystem.UpdateParticle;
var
 v:TVector3;
begin
 with fParticles[i] do begin
        if fparticles[i].fEnergy<=0 then SetParticleDefaults(i);
        fParticles[i].fOldPos:=fParticles[i].fPos;
        
        fparticles[i].fPos[0]:=fparticles[i].fPos[0]+fparticles[i].fVelocity[0]*delta+fparticles[i].fAccX*delta*delta/2;
        fparticles[i].fPos[1]:=fparticles[i].fPos[1]+fparticles[i].fVelocity[1]*delta+fparticles[i].fAccY*delta*delta/2;
        fparticles[i].fPos[2]:=fparticles[i].fPos[2]+fparticles[i].fVelocity[2]*delta+fparticles[i].fAccZ*delta*delta/2;

        fparticles[i].fAccX:=fparticles[i].fAccX+fInitInfo.Gx;
        fparticles[i].fAccY:=fparticles[i].fAccY+fInitInfo.Gy;
        fparticles[i].fAccZ:=fparticles[i].fAccZ+fInitInfo.Gx;

        fparticles[i].fEnergy:=fparticles[i].fEnergy-delta;
        if fparticles[i].fEnergy<0 then fparticles[i].fEnergy:=0;
        if fInitInfo.SizeDecrease then fparticles[i].fSize:=(fParticles[i].fEnergy*fParticles[i].fInitialSize)/fParticles[i].fInitialEnergy;
        if fInitInfo.SizeIncrease then fparticles[i].fSize:=(fParticles[i].fInitialEnergy*fParticles[i].fInitialSize)/fParticles[i].fEnergy;

        case fInitInfo.PrimitiveType of
                PT_Points:begin
                        fPoint[i].Color[0]:=trunc(fInitInfo.Color[0]*255);
                        fPoint[i].Color[1]:=trunc(fInitInfo.Color[1]*255);
                        fPoint[i].Color[2]:=trunc(fInitInfo.Color[2]*255);
                        fPoint[i].Color[3]:=trunc((fEnergy/10+0.3)*255);
                        fPoint[i].Pos:=fParticles[i].fPos;
                        if fInitInfo.DepthSort then fEyeDist[i]:=Renderer.Camera.EyeDistance(fPoint[i].Pos);
                end;
                PT_Lines:begin
                        fLine[i].StartPos.Color[0]:=trunc(fInitInfo.Color[0]*255);
                        fLine[i].EndPos.Color[0]:=trunc(fInitInfo.Color[0]*255);
                        fLine[i].StartPos.Color[1]:=trunc(fInitInfo.Color[1]*255);
                        fLine[i].EndPos.Color[1]:=trunc(fInitInfo.Color[1]*255);
                        fLine[i].StartPos.Color[2]:=trunc(fInitInfo.Color[2]*255);
                        fLine[i].EndPos.Color[2]:=trunc(fInitInfo.Color[2]*255);
                        fLine[i].StartPos.Color[3]:=trunc((fEnergy/10+0.3)*255);
                        fLine[i].EndPos.Color[3]:=trunc((fEnergy/10+0.3)*255);
                        fLine[i].StartPos.Pos:=fParticles[i].fOldPos;
                        fLine[i].EndPos.Pos:=fParticles[i].fPos;
                        if fInitInfo.DepthSort then fEyeDist[i]:=Renderer.Camera.EyeDistance(fLine[i].StartPos.Pos);
                end;
                PT_BillBoard: begin
                        fRect[i].V1.Color[0]:=trunc(fInitInfo.Color[0]*255); fRect[i].V2.Color[0]:=trunc(fInitInfo.Color[0]*255); fRect[i].V3.Color[0]:=trunc(fInitInfo.Color[0]*255); fRect[i].V4.Color[0]:=trunc(fInitInfo.Color[0]*255);
                        fRect[i].V1.Color[1]:=trunc(fInitInfo.Color[1]*255); fRect[i].V2.Color[1]:=trunc(fInitInfo.Color[1]*255); fRect[i].V3.Color[1]:=trunc(fInitInfo.Color[1]*255); fRect[i].V4.Color[1]:=trunc(fInitInfo.Color[1]*255);
                        fRect[i].V1.Color[2]:=trunc(fInitInfo.Color[2]*255); fRect[i].V2.Color[2]:=trunc(fInitInfo.Color[2]*255); fRect[i].V3.Color[2]:=trunc(fInitInfo.Color[2]*255); fRect[i].V4.Color[2]:=trunc(fInitInfo.Color[2]*255);
                        fRect[i].V1.Color[3]:=trunc(fEnergy*255);             fRect[i].V2.Color[3]:=trunc(fEnergy*255);            fRect[i].V3.Color[3]:=trunc(fEnergy*255);            fRect[i].V4.Color[3]:=trunc(fEnergy*255);
                        //Vector3Add(fPosition, fParticles[i].fPos, V);
                        V:=fParticles[i].fPos;
                        CalculateBillBoard(v,fParticles[i].fSize,fParticles[i].fSize,fRect[i]);
                        if fInitInfo.DepthSort then fEyeDist[i]:=Renderer.Camera.EyeDistance(fRect[i].V1.Pos);
                end;
        end;
 end;
end;

procedure TParticleSystem.Render;
begin
 if not Renderer.Rendering then GL.glLoadName(fName);
 if Renderer.Rendering then begin
        Renderer.RC.fTexManager.ApplyMaterial(fRenderingMaterial);

        fRenderingMaterial.DepthTest.DepthMask:=true;
        fRenderingMaterial.VertexArray.TexCoord0Array:=true;
        Renderer.RC.fStateManager.ApplyMaterial(fRenderingMaterial);
        fRenderingMaterial.DepthTest.DepthMask:=false;
        Renderer.RC.fStateManager.ApplyMaterial(fRenderingMaterial);
 end
 else begin
        Renderer.RC.fTexManager.ApplyMaterial(fSelectionMaterial);
        Renderer.RC.fStateManager.ApplyMaterial(fSelectionMaterial);
 end;

 Renderer.RC.fVPs.DisableVP;

 if fInitInfo.PrimitiveType=PT_Points then begin
        GL.glBindBufferARB(GL_ARRAY_BUFFER_ARB, fVBOBuffer);
        GL.glBufferDataARB(GL_ARRAY_BUFFER_ARB,sizeof(TPoint)*length(fPoint),@fPoint[0],GL_STREAM_DRAW_ARB);
        if Renderer.Rendering then GL.glInterleavedArrays(GL_C4UB_V3F, sizeof(TPoint),nil)
                              else GL.glInterleavedArrays(GL_V3F, sizeof(TPoint),pointer(4));
        GL.glDrawArrays(GL_POINTS,0,length(fParticles)-1);
        GL.glDisableClientState(GL_VERTEX_ARRAY);
        if Renderer.Rendering then GL.glDisableClientState(GL_COLOR_ARRAY);
 end
 else if fInitInfo.PrimitiveType=PT_LINES then begin
        GL.glBindBufferARB(GL_ARRAY_BUFFER_ARB, fVBOBuffer);
        GL.glBufferDataARB(GL_ARRAY_BUFFER_ARB,sizeof(TLine)*length(fLine),@fLine[0],GL_STREAM_DRAW_ARB);
        if Renderer.Rendering then GL.glInterleavedArrays(GL_C4UB_V3F, sizeof(TPoint),nil)
                              else GL.glInterleavedArrays(GL_V3F, sizeof(TPoint),pointer(4));
        GL.glDrawArrays(GL_Lines,0,length(fParticles)*2-1);
        GL.glDisableClientState(GL_VERTEX_ARRAY);
        if Renderer.Rendering then GL.glDisableClientState(GL_COLOR_ARRAY);
 end
 else begin

        if fInitInfo.DepthSort then QSort(0, high(fEyeDist));
        
        GL.glBindBufferARB(GL_ARRAY_BUFFER_ARB, fVBOBuffer);
        GL.glBufferDataARB(GL_ARRAY_BUFFER_ARB,sizeof(TParticleQuad)*length(fRect),@fRect[0],GL_STREAM_DRAW_ARB);
        if Renderer.Rendering then GL.glInterleavedArrays(GL_T2F_C4UB_V3F, sizeof(TParticleVertex),nil)
                              else GL.glInterleavedArrays(GL_V3F, sizeof(TParticleVertex),pointer(12));
        GL.glDrawArrays(GL_QUADS,4,high(fRect)*4-1);
        GL.glDisableClientState(GL_VERTEX_ARRAY);
        if Renderer.Rendering then begin
                GL.glDisableClientState(GL_COLOR_ARRAY);
                GL.glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
 end;
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////TParticleManager///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
constructor TParticleManager.create;
begin
 inherited create;
 fSystems:=nil;
 fSystemCount:=0;
 glwAppendLog('Particle Manager created.');
end;

destructor TParticleManager.destroy;
begin
 Clear;
 glwAppendLog('Particle Manager destroyed.');
 inherited;
end;

function TParticleManager.AddSystem;
begin
 setlength(fSystems, length(fSystems)+1);
 fSystems[high(fSystems)]:=System;
 fSystemCount:=length(fSystems);
 System.fName:=TSelectionNameBase+fSystemCount-1;
 result:=high(fSystems);
end;

procedure TParticleManager.RemoveSystem;
var
 i,j:integer;
begin
 for i:=0 to fSystemCount-1 do
        if fSystems[i].fID=ID then begin
                fSystems[i].Free;
                for j:=i+1 to fSystemCount-1 do
                        fSystems[j-1]:=fSystems[j];
                setlength(fSystems,length(fSystems)-1);
                dec(fSystemCount);
        end;
end;

procedure TParticleManager.Update;
var
 i, j:integer;
begin
 if Renderer.Camera.CanMove=false then exit;
 i:=0;
 while i<=integer(fSystemCount-1) do begin
        with fSystems[i] do begin
                if fNrAlive>0 then begin
                        Update;
                end;
                if Dead then begin
                        RemoveSystem(fID);
                        dec(i);
                end;
        end;
        inc(i);
 end;
 j:=fSystemCount;
 for i:=0 to j-1 do
        if (fSystems[i].fDead=false)and(fSystems[i].IsVisible) then begin
                Renderer.AddSceneObject(fSystems[i]);
        end;
end;

function TParticleManager.DoesExists;
var
 i:integer;
begin
 result:=false;
 for i:=0 to fSystemCount-1 do
        if fSystems[i].fID=ID then begin
                result:=true;
                exit;
        end;
end;

procedure TParticleSystem.QSort;
var
 Lo, Hi: Integer;
 Mid:single;
 Q:TParticleQuad;
 P:TPoint;
 L:TLine;
 t:single;
begin
 Lo := min;
 Hi := max;
 Mid := fEyeDist[(Lo+Hi) div 2];
 repeat
        while fEyeDist[Lo] > Mid do Inc(Lo);
        while fEyeDist[Hi] < Mid do Dec(Hi);
        if Lo <= Hi then begin
                t:=fEyeDist[Lo];
                fEyeDist[Lo]:=fEyeDist[Hi];
                fEyeDist[Hi]:=t;
                if fInitInfo.PrimitiveType=PT_Points then begin
                        P:=fPoint[Lo];
                        fPoint[Lo]:=fPoint[Hi];
                        fPoint[Hi]:=P;
                end
                else if fInitInfo.PrimitiveType=PT_Lines then begin
                        L:=fLine[Lo];
                        fLine[Lo]:=fLine[Hi];
                        fLine[Hi]:=L;
                end
                else begin
                        Q:=fRect[Lo];
                        fRect[Lo]:=fRect[Hi];
                        fRect[Hi]:=Q;
                end;
                Inc(Lo);
                Dec(Hi);
        end;
 until Lo > Hi;
 if Hi > min then QSort(min, Hi);
 if Lo < max then QSort(Lo, max);
end;

procedure TParticleSystem.CalculateBillBoard(const Position: TVector3;
  const W, H: glFLoat; out s: TParticleQuad);
var
 x,y:glFloat;
begin
 x:=W/2;
 y:=H/2;
 s.V1.Pos[0]:=Position[0]+(-Renderer.Camera.right[0]*x-Renderer.Camera.up[0]*y);
 s.V1.Pos[1]:=Position[1]+(-Renderer.Camera.right[1]*x-Renderer.Camera.up[1]*y);
 s.V1.Pos[2]:=Position[2]+(-Renderer.Camera.right[2]*x-Renderer.Camera.up[2]*y);

 s.V2.Pos[0]:=Position[0]+( Renderer.Camera.right[0]*x-Renderer.Camera.up[0]*y);
 s.V2.Pos[1]:=Position[1]+( Renderer.Camera.right[1]*x-Renderer.Camera.up[1]*y);
 s.V2.Pos[2]:=Position[2]+( Renderer.Camera.right[2]*x-Renderer.Camera.up[2]*y);

 s.V3.Pos[0]:=Position[0]+( Renderer.Camera.right[0]*x+Renderer.Camera.up[0]*y);
 s.V3.Pos[1]:=Position[1]+( Renderer.Camera.right[1]*x+Renderer.Camera.up[1]*y);
 s.V3.Pos[2]:=Position[2]+( Renderer.Camera.right[2]*x+Renderer.Camera.up[2]*y);

 s.V4.Pos[0]:=Position[0]+(+Renderer.Camera.up[0]*y-Renderer.Camera.right[0]*x);
 s.V4.Pos[1]:=Position[1]+(+Renderer.Camera.up[1]*y-Renderer.Camera.right[1]*x);
 s.V4.Pos[2]:=Position[2]+(+Renderer.Camera.up[2]*y-Renderer.Camera.right[2]*x);
end;

procedure TParticleManager.Clear;
var
 i:integer;
begin
 for i:=0 to high(fSystems) do fSystems[i].Free;
 fSystems:=nil;
 fSystemCount:=0;
end;

function TParticleManager.GetSystem(index: integer): TBasicParticleSystem;
begin
 result:=nil;
 if index<0 then exit;
 if index>high(fSystems) then exit;
 result:=fSystems[index];
end;

procedure TParticleManager.SetSystem(index: integer;
  const Value: TBasicParticleSystem);
begin
 if index<0 then exit;
 if index>high(fSystems) then exit;
 fSystems[index]:=Value;
end;

{ TBasicParticleSystem }

function TBasicParticleSystem.IsVisible: boolean;
begin
 result:=Renderer.Camera.SphereInFrustum(fBSphere.C, fBSphere.R);
end;

procedure TParticleManager.AddScriptFunctions(Script: PScript);
begin
 Script.AddFunction('PSAdd',4,_AddSystem);;
 Script.AddFunction('PSLoad',2,_LoadFromFile);;
 Script.AddFunction('PSSetPos',4,_SetPosition);;
end;

function TParticleManager._AddSystem(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 self.AddSystem(TParticleSystem.create(MakeVector3(Args[1], Args[2], Args[3]),Args[0]));
end;

function TParticleManager._LoadFromFile(Args: TArray; var error: pchar): Variant;
var
 index:integer;
 name:string;
begin
 error:='';
 index:=Args[0];
 name:=Args[1];
 (self.fSystems[index] as TParticleSystem).LoadFromFile(name);
end;

function TParticleManager._SetPosition(Args: TArray; var error: pchar): Variant;
var
 index:integer;
 x,y,z:single;
begin
 error:='';
 index:=Args[0];
 x:=Args[1];
 y:=Args[2];
 z:=Args[3];
 (self.fSystems[index] as TParticleSystem).fPosition[0]:=x;
 (self.fSystems[index] as TParticleSystem).fPosition[1]:=y;
 (self.fSystems[index] as TParticleSystem).fPosition[2]:=z;
end;

end.

