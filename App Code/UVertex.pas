unit UVertex;
interface
uses dglOpenGL, UVectors;
type
 TVertex = record
        Normal: TVector3;                   // Vertex normal.
        Pos: TVector3;                      // Vertex position.
        Color: TVector4;
        TexCoord:TVector2;
        UVCoords:TVector2;
        FogCoord:single;
 end;

 TBSphere = record
        C: TVector3;                        // BSphere center.
        R: Single;                          // BSphere radius.
 end;

 TVertexList=class
       private
        fVerts:array of TVertex;
        function EqualsF (v1,v2:single):boolean;
        function Equals2F(v1,v2:TVector2):boolean;
        function Equals3F(v1,v2:TVector3):boolean;
        function Equals4F(v1,v2:TVector4):boolean;
        function GetVertex(index: integer): TVertex;
       public
        procedure clear;
        constructor create;
        destructor destroy; override;
        function Add(value:TVertex):integer;
        property Vertex[index:integer]:TVertex read GetVertex;
        procedure Render(index:integer);
        procedure Remove(index:integer);
 end;

 TVertexArray = record
        Normal: array of TVector3;                   // Vertex normal.
        Pos: array of TVector3;                      // Vertex position.
        Color: array of  TVector4;
        TexCoord: array of TVector2;
        UVCoords: array of TVector2;
        FogCoord: array of single;
        I: array of Cardinal;
 end;

implementation
const
 Epsilon=0.0000000001;
{ TVertexList }

function TVertexList.Add(value: TVertex):integer;
var
 i:integer;
begin
 for i:=0 to high(fVerts) do
        if EqualsF (value.FogCoord, fVerts[i].FogCoord) and
           Equals2F(value.TexCoord, fVerts[i].TexCoord) and
           Equals2F(value.UVCoords, fVerts[i].UVCoords) and
           Equals3F(value.Pos, fVerts[i].Pos) and
           Equals3F(value.Normal, fVerts[i].Normal) and
           Equals4F(value.Color, fVerts[i].Color) then begin
                result:=i;
                exit;
           end;
 setlength(fVerts, length(fVerts)+1);
 result:=high(fVerts);
 fVerts[result]:=value;
end;

procedure TVertexList.clear;
begin
 fVerts:=nil;
end;

constructor TVertexList.create;
begin
 inherited create;
 fVerts:=nil;
end;

destructor TVertexList.destroy;
begin
 Clear;
 inherited;
end;

function TVertexList.Equals2F(v1, v2: TVector2): boolean;
begin
 result:=(EqualsF(v1[0], v2[0]) and EqualsF(V1[1], V2[1]));
end;

function TVertexList.Equals3F(v1, v2: TVector3): boolean;
begin
 result:=(EqualsF(v1[0], v2[0]) and EqualsF(V1[1], V2[1]) and EqualsF(V1[2], V2[2]));
end;

function TVertexList.Equals4F(v1, v2: TVector4): boolean;
begin
 result:=(EqualsF(v1[0], v2[0]) and EqualsF(V1[1], V2[1]) and EqualsF(V1[2], V2[2]) and EqualsF(V1[3], V2[3]));
end;

function TVertexList.EqualsF(v1, v2: single): boolean;
begin
 result:=(abs(v1-v2)<Epsilon);
end;

function TVertexList.GetVertex(index: integer): TVertex;
begin
 if index<0 then exit;
 if index>high(fVerts) then exit;
 result:=fVerts[index];
end;

procedure TVertexList.Render(index: integer);
begin
 if index<0 then exit;
 if index>high(fVerts) then exit;

 GL.glColor4fv(@fVerts[index].Color);
 GL.glMultiTexCoord2fv(GL_TEXTURE0, @fVerts[index].TexCoord);
 GL.glMultiTexCoord2fv(GL_TEXTURE1, @fVerts[index].UVCoords);
 GL.glFogCoordf(fVerts[index].FogCoord);
 GL.glNormal3fv(@fVerts[index].Normal);
 GL.glVertex3fv(@fVerts[index].Pos);
end;

procedure TVertexList.Remove(index: integer);
var
 i:integer;
begin
 if index<0 then exit;
 if index>high(fVerts) then exit;
 for i:=index+1 to high(fVerts) do fVerts[i-1]:=fVerts[i];
 setlength(fVerts, high(fVerts));
end;

end.
