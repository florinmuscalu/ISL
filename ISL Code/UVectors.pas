unit UVectors;
interface
const
 Error=0.0001;
type
 PVector3=^TVector3;
 TVector3=array[0..2]of single;
 TVector4=array[0..3]of single;
 TVector2=array[0..1]of single;
 TMatrix=array[0..3,0..3]of single;
 TGLMatrixf4 = TMatrix;
 TMatrix4f = TGLMatrixf4;
 TRect3D=record
        x1,y1,z1:single;
        x2,y2,z2:single;
        x3,y3,z3:single;
        x4,y4,z4:single;
 end;
 T3DBox=record
        center:TVector3;
        Width, Height, Depth:single;
 end;

  TParticleVertex=record
        TexCoord:TVector2;
        Color:array[0..3]of byte;
        Pos:TVector3;
 end;

 TParticleQuad=record
        V1, V2, V3, V4:TParticleVertex;
 end;

 TUIntArray   = array of cardinal;
 TVector2Array= array of TVector2;
 TVector3Array= array of TVector3;
 TVector4Array= array of TVector4;

 TTransType = (ttScaleX, ttScaleY, ttScaleZ,
               ttShearXY, ttShearXZ, ttShearYZ,
               ttRotateX, ttRotateY, ttRotateZ,
               ttTranslateX, ttTranslateY, ttTranslateZ,
               ttPerspectiveX, ttPerspectiveY, ttPerspectiveZ, ttPerspectiveW);

 // used to describe a sequence of transformations in following order:
 // [Sx][Sy][Sz][ShearXY][ShearXZ][ShearZY][Rx][Ry][Rz][Tx][Ty][Tz][P(x,y,z,w)]
 // constants are declared for easier access (see MatrixDecompose below)
 TTransformations  = array [TTransType] of Single;


const
 MATRIX_IDENTITY4: TMatrix = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));
 MATRIX_ZERO4: TMatrix = ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0));
 UNIT_VECTOR3:TVector3=(1,1,1);
 ZERO_VECTOR3:TVector3=(0,0,0);

function MatrixDecompose(const M: TMatrix; var Tran: TTransformations): Boolean;
function MatrixDeterminant(const M: TMatrix): Single;
function MatrixDetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
procedure InvertMatrix(var M : TMatrix);
procedure AdjointMatrix(var M : TMatrix);
procedure ScaleMatrix(var M : TMatrix; const factor : Single);
procedure TransposeMatrix(var M: TMatrix);
procedure SetVector(var v : TVector4; const vSrc : TVector4);
function VectorNorm(const v : TVector4) : Single;
function Vector4Scale(const v : TVector4; factor : Single) : TVector4;
function Vector4DotProduct(const V1, V2 : TVector4) : Single;
procedure CombineVector4(var vr : TVector4; const v : TVector4; var f : Single);
function Vector4CrossProduct(const v1, v2 : TVector4) : TVector4;
procedure NegateVector4(var v : TVector4);
function ArcSin(const x : Single) : Single;


function VectorNormalize(const v : TVector3):TVector3;
procedure Vector3Normalize(var v : TVector3);
function MakeVector3(x,y,z:single):TVector3;
function MakeVector4(x,y,z,w:single):TVector4;
function MatrixMultiply(const M1, M2: TMatrix): TMatrix;
procedure NormalizePlane(var plane : TVector4);
function RSqrt(v : Single) : Single;                    // 1/sqrt(v)
procedure ScaleVector4(var v : TVector4; factor: Single);
procedure ScaleVector3(var v : TVector3; factor: Single);
function PlaneEvaluatePoint(const plane : TVector4; const point : TVector3) : Single;

function Vector3Add(const v1, v2 : TVector3) : TVector3; overload;
procedure Vector3Add(const v1, v2 : TVector3; var vr : TVector3); overload;
procedure AddVector3(var v1 : TVector3; const v2 : TVector3);

function GetBoundingBox(var a:array of TVector3):T3DBox;
function Trunc64(v : Extended) : Int64;
function Trunc(v : Single) : Integer;
function Round(v : Single) : Integer;
function ScaleAndRound(i : Integer; var s : Single) : Integer; //Result:=Round(i*s);
procedure RandomPointOnSphere(var p : TVector3);

function  Lerp(const start, stop, t : Single) : Single;
function  Vector2Lerp(const V1, V2: TVector2; t: Single): TVector2;
function  Vector3Lerp(const V1, V2: TVector3; t: Single): TVector3; overload;
procedure Vector3Lerp(const v1, v2: TVector3; t: Single; var vr : TVector3); overload;
function  Vector4Lerp(const V1, V2: TVector4; t: Single): TVector4;

function PointSegmentDistance(const point, segmentStart, segmentStop : TVector3) : Single;
function PointSegmentClosestPoint(const point, segmentStart, segmentStop : TVector3) : TVector3;
function Vector3Distance(const v1, v2 : TVector3) : Single;
procedure Vector3Subtract(const v1, v2 : TVector3; var result : TVector3); overload;
function Vector3Subtract(const v1, v2 : TVector3): TVector3; overload;
function Vector3DotProduct(const V1, V2 : TVector3): Single;
function ClampValue(const aValue, aMin, aMax : Single) : Single;
function Vector3Scale(const v : TVector3; factor : Single) : TVector3; overload;
procedure Vector3Scale(const v : TVector3; factor : Single; var vr : TVector3); overload;

function Vector3Equals(const V1, V2: TVector3) : Boolean;

function IsNan(const AValue: Double): Boolean;

//Aduna A cu B si daca rezultatul e mai mare de 255 atunci fa-l 255
function Add(A,B:smallInt):SmallInt; overload;
function Add(const v1, v2 : single) : single; overload;

//Aduna A cu B*s si daca rezultatul e mai mare de 255 atunci fa-l 255
function AddMul(B : smallInt; var s : Single; A:Integer) : Integer;

//scaleaza un vector de single!!!!!, values, cu nb componente, cu factorul factor, si el tot single!!!!
procedure ScaleFloatArray(values : pointer; nb : Integer; var factor : Single); 


//make a plane out of 3 points
function PlaneMake(const p1, p2, p3 : TVector3) : TVector4; overload;
function PlaneMake(const point, normal : TVector3) : TVector4; overload;
//calculate the normal of a plane defined by 3 points
procedure CalcPlaneNormal(const p1, p2, p3 : TVector3; var vr : TVector3);
//normalize the vector
procedure NormalizeVector3(var v : TVector3);
//Cross Product
procedure Vector3CrossProduct(const v1, v2 : TVector3; var vr : TVector3); overload;
function Vector3CrossProduct(const v1, v2 : TVector3) : TVector3; overload;

function Vector3Negate(const v : TVector3) : TVector3;

function Vector3Length(const v : TVector3) : Single;

function RayCastSphereIntersect(const rayStart, rayVector : TVector3;
                                const sphereCenter : TVector3;
                                const sphereRadius : Single;
                                var i1, i2 : TVector3) : Integer;
function PointProject(const p, origin, direction : TVector3) : Single;
procedure Vector3Combine(const V1, V2: TVector3; const F2: Single; var vr : TVector3);
function Vector3Distance2(const v1, v2 : TVector3) : Single;
function Vector3Norm(const v : TVector3) : Single;
function RayCastPlaneIntersect(const rayStart, rayVector : TVector3;
                               const planePoint, planeNormal : TVector3;
                               intersectPoint : PVector3 = nil) : Boolean;
function RayCastTriangleIntersect(const rayStart, rayVector : TVector3;
                                  const p1, p2, p3 : TVector3;
                                  intersectPoint : PVector3 = nil;
                                  intersectNormal : PVector3 = nil) : Boolean;
function CheckPointInTriangle(const point:TVector3; var a, b, c: TVector3):boolean;
function ClosestPointOnTriangleEdge(var a, b, c, p: TVector3): TVector3;
function ClosestPointOnLine(var a, b, p : TVector3): TVector3;
function CheckPointInRect(const point:TVector3; var a, b, c, d: TVector3):boolean;
function VectorPerpendicular(const V, N : TVector3) : TVector3;

function SegmentSegmentDistance(const S0Start, S0Stop, S1Start, S1Stop : TVector3) : single;
procedure SegmentSegmentClosestPoint(const S0Start, S0Stop, S1Start, S1Stop : TVector3; var Segment0Closest, Segment1Closest : TVector3);
function VectorRotateAroundX(const v : TVector3; alpha : Single) : TVector3;
function VectorRotateAroundY(const v : TVector3; alpha : Single) : TVector3;
procedure SinCos(const Theta: Single; var Sin, Cos: Single); overload;
procedure RotateVector(var vector : TVector3; const axis : TVector3; angle: Single);
function VectorAngleCosine(const V1, V2: TVector3): Single;
procedure VectorLerp(const v1, v2 : TVector3; const t : Single; var vr : TVector3);
procedure Vector3ArrayLerp(const src1, src2 : TVector3Array; const t : Single; n : Integer; dest : TVector3Array); stdcall;

function VectorTransform(const V: TVector4; const M: TMatrix) : TVector4; overload;
function VectorTransform(const V: TVector3; const M: TGLMatrixf4): TVector3; overload;

function MatrixTranslate4(x, y, z: Single): TMatrix;
function CreateRotationMatrixX(const sine, cosine: Single) : TMatrix; overload;
function CreateRotationMatrixX(const angle: Single) : TMatrix; overload;
function CreateRotationMatrixY(const sine, cosine: Single) : TMatrix; overload;
function CreateRotationMatrixY(const angle: Single) : TMatrix; overload;
function CreateRotationMatrixZ(const sine, cosine: Single) : TMatrix; overload;
function CreateRotationMatrixZ(const angle: Single) : TMatrix; overload;
implementation
uses math;
const
 X=0;
 Y=1;
 Z=2;
 W=3;
 cwChop : Word = $1F3F;
 cOne:single = 1;
 IdentityHmgMatrix: TMatrix = ((1, 0, 0, 0),
                                (0, 1, 0, 0),
                                (0, 0, 1, 0),
                                (0, 0, 0, 1));
 EmptyHmgMatrix: TMatrix = ((0, 0, 0, 0),
                             (0, 0, 0, 0),
                             (0, 0, 0, 0),
                             (0, 0, 0, 0));
var
 // this var is adjusted during "initialization", current values are
 // + 0 : use standard optimized FPU code
 // + 1 : use 3DNow! optimized code (requires K6-2/3 CPU)
 vSIMD : Byte = 0;

function CreateRotationMatrixZ(const sine, cosine: Single): TMatrix;
begin
   Result:=EmptyHmgMatrix;
   Result[X, X]:=cosine;
   Result[X, Y]:=sine;
   Result[Y, X]:=-sine;
   Result[Y, Y]:=cosine;
   Result[Z, Z]:=1;
   Result[W, W]:=1;
end;

function CreateRotationMatrixZ(const angle : Single) : TMatrix;
var
   s, c : Single;
begin
   SinCos(angle, s, c);
   Result:=CreateRotationMatrixZ(s, c);
end;

function CreateRotationMatrixY(const sine, cosine: Single): TMatrix;
begin
   Result:=EmptyHmgMatrix;
   Result[X, X]:=cosine;
   Result[X, Z]:=-sine;
   Result[Y, Y]:=1;
   Result[Z, X]:=sine;
   Result[Z, Z]:=cosine;
   Result[W, W]:=1;
end;

function CreateRotationMatrixY(const angle : Single) : TMatrix;
var
   s, c : Single;
begin
   SinCos(angle, s, c);
   Result:=CreateRotationMatrixY(s, c);
end;

function CreateRotationMatrixX(const sine, cosine: Single) : TMatrix;
begin
   Result:=EmptyHmgMatrix;
   Result[X, X]:=1;
   Result[Y, Y]:=cosine;
   Result[Y, Z]:=sine;
   Result[Z, Y]:=-sine;
   Result[Z, Z]:=cosine;
   Result[W, W]:=1;
end;

function CreateRotationMatrixX(const angle : Single) : TMatrix;
var
   s, c : Single;
begin
   SinCos(angle, s, c);
   Result:=CreateRotationMatrixX(s, c);
end;

function MatrixTranslate4(x, y, z: Single): TMatrix;
begin
 Result := MATRIX_IDENTITY4;
 Result[3,0] := x;
 Result[3,1] := y;
 Result[3,2] := z;
end;

function MatrixDetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
// internal version for the determinant of a 3x3 matrix
begin
  Result:=  a1 * (b2 * c3 - b3 * c2)
          - b1 * (a2 * c3 - a3 * c2)
          + c1 * (a2 * b3 - a3 * b2);
end;

function MatrixDeterminant(const M: TMatrix): Single;
begin
  Result:= M[X, X]*MatrixDetInternal(M[Y, Y], M[Z, Y], M[W, Y], M[Y, Z], M[Z, Z], M[W, Z], M[Y, W], M[Z, W], M[W, W])
          -M[X, Y]*MatrixDetInternal(M[Y, X], M[Z, X], M[W, X], M[Y, Z], M[Z, Z], M[W, Z], M[Y, W], M[Z, W], M[W, W])
          +M[X, Z]*MatrixDetInternal(M[Y, X], M[Z, X], M[W, X], M[Y, Y], M[Z, Y], M[W, Y], M[Y, W], M[Z, W], M[W, W])
          -M[X, W]*MatrixDetInternal(M[Y, X], M[Z, X], M[W, X], M[Y, Y], M[Z, Y], M[W, Y], M[Y, Z], M[Z, Z], M[W, Z]);
end;

procedure InvertMatrix(var M : TMatrix);
var
   det : Single;
begin
   det:=MatrixDeterminant(M);
   if Abs(Det)<Error then
      M:=IdentityHmgMatrix
   else begin
      AdjointMatrix(M);
      ScaleMatrix(M, 1/det);
   end;
end;

procedure ScaleMatrix(var M : TMatrix; const factor : Single);
var
   i : Integer;
begin
   for i:=0 to 3 do begin
      M[I, 0]:=M[I, 0] * Factor;
      M[I, 1]:=M[I, 1] * Factor;
      M[I, 2]:=M[I, 2] * Factor;
      M[I, 3]:=M[I, 3] * Factor;
   end;
end;

procedure AdjointMatrix(var M : TMatrix);
var
   a1, a2, a3, a4,
   b1, b2, b3, b4,
   c1, c2, c3, c4,
   d1, d2, d3, d4: Single;
begin
    a1:= M[X, X]; b1:= M[X, Y];
    c1:= M[X, Z]; d1:= M[X, W];
    a2:= M[Y, X]; b2:= M[Y, Y];
    c2:= M[Y, Z]; d2:= M[Y, W];
    a3:= M[Z, X]; b3:= M[Z, Y];
    c3:= M[Z, Z]; d3:= M[Z, W];
    a4:= M[W, X]; b4:= M[W, Y];
    c4:= M[W, Z]; d4:= M[W, W];

    // row column labeling reversed since we transpose rows & columns
    M[X, X]:= MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
    M[Y, X]:=-MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
    M[Z, X]:= MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
    M[W, X]:=-MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);

    M[X, Y]:=-MatrixDetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
    M[Y, Y]:= MatrixDetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
    M[Z, Y]:=-MatrixDetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
    M[W, Y]:= MatrixDetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);

    M[X, Z]:= MatrixDetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
    M[Y, Z]:=-MatrixDetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
    M[Z, Z]:= MatrixDetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
    M[W, Z]:=-MatrixDetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);

    M[X, W]:=-MatrixDetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
    M[Y, W]:= MatrixDetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
    M[Z, W]:=-MatrixDetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
    M[W, W]:= MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

procedure TransposeMatrix(var M: TMatrix);
var
   f : Single;
begin
   f:=M[0, 1]; M[0, 1]:=M[1, 0]; M[1, 0]:=f;
   f:=M[0, 2]; M[0, 2]:=M[2, 0]; M[2, 0]:=f;
   f:=M[0, 3]; M[0, 3]:=M[3, 0]; M[3, 0]:=f;
   f:=M[1, 2]; M[1, 2]:=M[2, 1]; M[2, 1]:=f;
   f:=M[1, 3]; M[1, 3]:=M[3, 1]; M[3, 1]:=f;
   f:=M[2, 3]; M[2, 3]:=M[3, 2]; M[3, 2]:=f;
end;

function VectorTransform(const V: TVector4; const M: TMatrix) : TVector4;
begin
 if vSIMD=1 then begin
        asm
         db $0F,$6F,$00           /// movq        mm0,[eax]
         db $0F,$6F,$48,$08       /// movq        mm1,[eax+8]
         db $0F,$6F,$22           /// movq        mm4,[edx]
         db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
         db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
         db $0F,$62,$C0           /// punpckldq   mm0,mm0
         db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
         db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
         db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
         db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
         db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
         db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
         db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
         db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
         db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
         db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
         db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
         db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
         db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
         db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2

         db $0F,$7F,$39           /// movq        [ecx],mm7
         db $0F,$7F,$59,$08       /// movq        [ecx+8],mm3
         db $0F,$0E               /// femms
        end
 end;
end;

procedure SetVector(var v : TVector4; const vSrc : TVector4);
begin
   // faster than memcpy, move or ':=' on the TVector...
	v[0]:=vSrc[0];
	v[1]:=vSrc[1];
	v[2]:=vSrc[2];
	v[3]:=vSrc[3];
end;

function VectorNorm(const v : TVector4) : Single;
asm
 FLD DWORD PTR [EAX];
 FMUL ST, ST
 FLD DWORD PTR [EAX+4];
 FMUL ST, ST
 FADD
 FLD DWORD PTR [EAX+8];
 FMUL ST, ST
 FADD
end;

function Vector4Scale(const v : TVector4; factor : Single) : TVector4;
asm
 FLD  DWORD PTR [EAX]
 FMUL DWORD PTR [EBP+8]
 FSTP DWORD PTR [EDX]
 FLD  DWORD PTR [EAX+4]
 FMUL DWORD PTR [EBP+8]
 FSTP DWORD PTR [EDX+4]
 FLD  DWORD PTR [EAX+8]
 FMUL DWORD PTR [EBP+8]
 FSTP DWORD PTR [EDX+8]
 FLD  DWORD PTR [EAX+12]
 FMUL DWORD PTR [EBP+8]
 FSTP DWORD PTR [EDX+12]
end;

function Vector4DotProduct(const V1, V2 : TVector4) : Single;
asm
 FLD DWORD PTR [EAX]
 FMUL DWORD PTR [EDX]
 FLD DWORD PTR [EAX + 4]
 FMUL DWORD PTR [EDX + 4]
 FADDP
 FLD DWORD PTR [EAX + 8]
 FMUL DWORD PTR [EDX + 8]
 FADDP
 FLD DWORD PTR [EAX + 12]
 FMUL DWORD PTR [EDX + 12]
 FADDP
end;

procedure CombineVector4(var vr : TVector4; const v : TVector4; var f : Single);
asm
 test vSIMD, 1
 jz @@FPU
@@3DNow:
 db $0F,$6E,$11           /// MOVD  MM2, [ECX]
 db $0F,$62,$D2           /// PUNPCKLDQ MM2, MM2
 db $0F,$6F,$02           /// MOVQ  MM0, [EDX]
 db $0F,$0F,$C2,$B4       /// PFMUL MM0, MM2
 db $0F,$0F,$00,$9E       /// PFADD MM0, [EAX]
 db $0F,$7F,$00           /// MOVQ  [EAX], MM0
 db $0F,$6F,$4A,$08       /// MOVQ  MM1, [EDX+8]
 db $0F,$0F,$CA,$B4       /// PFMUL MM1, MM2
 db $0F,$0F,$48,$08,$9E   /// PFADD MM1, [EAX+8]
 db $0F,$7F,$48,$08       /// MOVQ  [EAX+8], MM1
 db $0F,$0E               /// FEMMS
 ret
@@FPU:
 FLD  DWORD PTR [EDX]
 FMUL DWORD PTR [ECX]
 FADD DWORD PTR [EAX]
 FSTP DWORD PTR [EAX]
 FLD  DWORD PTR [EDX+4]
 FMUL DWORD PTR [ECX]
 FADD DWORD PTR [EAX+4]
 FSTP DWORD PTR [EAX+4]
 FLD  DWORD PTR [EDX+8]
 FMUL DWORD PTR [ECX]
 FADD DWORD PTR [EAX+8]
 FSTP DWORD PTR [EAX+8]
 FLD  DWORD PTR [EDX+12]
 FMUL DWORD PTR [ECX]
 FADD DWORD PTR [EAX+12]
 FSTP DWORD PTR [EAX+12]
end;

function Vector4CrossProduct(const v1, v2 : TVector4) : TVector4;
begin
   Result[X]:=v1[Y]*v2[Z]-v1[Z]*v2[Y];
   Result[Y]:=v1[Z]*v2[X]-v1[X]*v2[Z];
   Result[Z]:=v1[X]*v2[Y]-v1[Y]*v2[X];
   Result[W]:=0;
end;

procedure NegateVector4(var v : TVector4);
asm
 FLD DWORD PTR [EAX]
 FCHS
 FSTP DWORD PTR [EAX]
 FLD DWORD PTR [EAX+4]
 FCHS
 FSTP DWORD PTR [EAX+4]
 FLD DWORD PTR [EAX+8]
 FCHS
 FSTP DWORD PTR [EAX+8]
 FLD DWORD PTR [EAX+12]
 FCHS
 FSTP DWORD PTR [EAX+12]
end;

function ArcSin(const x : Single) : Single;
asm
 FLD   X
 FLD   ST
 FMUL  ST, ST
 FSUBR cOne
 FSQRT
 FPATAN
end;

function MatrixDecompose(const M: TMatrix; var Tran: TTransformations): Boolean;
var
   I, J: Integer;
   LocMat, pmat, invpmat : TMatrix;
   prhs, psol: TVector4;
   row0, row1, row2 : TVector4;
   f : Single;
begin
  Result:=False;
  locmat:=M;
  // normalize the matrix
  if locmat[W, W] = 0 then Exit;
  for I:=0 to 3 do
    for J:=0 to 3 do
      locmat[I, J]:=locmat[I, J] / locmat[W, W];

  // pmat is used to solve for perspective, but it also provides
  // an easy way to test for singularity of the upper 3x3 component.

  pmat:=locmat;
  for I:=0 to 2 do pmat[I, W]:=0;
  pmat[W, W]:=1;

  if MatrixDeterminant(pmat) = 0 then Exit;

  // First, isolate perspective.  This is the messiest.
  if (locmat[X, W] <> 0) or (locmat[Y, W] <> 0) or (locmat[Z, W] <> 0) then begin
    // prhs is the right hand side of the equation.
    prhs[X]:=locmat[X, W];
    prhs[Y]:=locmat[Y, W];
    prhs[Z]:=locmat[Z, W];
    prhs[W]:=locmat[W, W];

    // Solve the equation by inverting pmat and multiplying
    // prhs by the inverse.  (This is the easiest way, not
    // necessarily the best.)

    invpmat:=pmat;
    InvertMatrix(invpmat);
    TransposeMatrix(invpmat);
    psol:=VectorTransform(prhs, invpmat);

    // stuff the answer away
    Tran[ttPerspectiveX]:=psol[X];
    Tran[ttPerspectiveY]:=psol[Y];
    Tran[ttPerspectiveZ]:=psol[Z];
    Tran[ttPerspectiveW]:=psol[W];

    // clear the perspective partition
    locmat[X, W]:=0;
    locmat[Y, W]:=0;
    locmat[Z, W]:=0;
    locmat[W, W]:=1;
  end else begin
    // no perspective
    Tran[ttPerspectiveX]:=0;
    Tran[ttPerspectiveY]:=0;
    Tran[ttPerspectiveZ]:=0;
    Tran[ttPerspectiveW]:=0;
  end;

  // next take care of translation (easy)
  for I:=0 to 2 do begin
    Tran[TTransType(Ord(ttTranslateX) + I)]:=locmat[W, I];
    locmat[W, I]:=0;
  end;

  // now get scale and shear
  row0[0]:=locmat[0][0]; row0[1]:=locmat[0][1]; row0[2]:=locmat[0][2]; row0[3]:=locmat[0][3];
  row1[0]:=locmat[1][0]; row1[1]:=locmat[1][1]; row1[2]:=locmat[1][2]; row1[3]:=locmat[1][3];
  row2[0]:=locmat[2][0]; row2[1]:=locmat[2][1]; row2[2]:=locmat[2][2]; row2[3]:=locmat[2][3];

  // compute X scale factor and normalize first row
  Tran[ttScaleX]:=VectorNorm(row0);
  Vector4Scale(row0, RSqrt(Tran[ttScaleX]));

  // compute XY shear factor and make 2nd row orthogonal to 1st
  Tran[ttShearXY]:=Vector4DotProduct(row0, row1);
  f:=-Tran[ttShearXY];
  CombineVector4(row1, row0, f);

  // now, compute Y scale and normalize 2nd row
  Tran[ttScaleY]:=VectorNorm(row1);
  Vector4Scale(row1, RSqrt(Tran[ttScaleY]));
  Tran[ttShearXY]:=Tran[ttShearXY]/Tran[ttScaleY];

  // compute XZ and YZ shears, orthogonalize 3rd row
  Tran[ttShearXZ]:=Vector4DotProduct(row0, row2);
  f:=-Tran[ttShearXZ];
  CombineVector4(row2, row0, f);
  Tran[ttShearYZ]:=Vector4DotProduct(row1, row2);
  f:=-Tran[ttShearYZ];
  CombineVector4(row2, row1, f);

  // next, get Z scale and normalize 3rd row
  Tran[ttScaleZ]:=VectorNorm(row2);
  Vector4Scale(row2, RSqrt(Tran[ttScaleZ]));
  Tran[ttShearXZ]:=Tran[ttShearXZ] / tran[ttScaleZ];
  Tran[ttShearYZ]:=Tran[ttShearYZ] / Tran[ttScaleZ];

  // At this point, the matrix (in rows[]) is orthonormal.
  // Check for a coordinate system flip.  If the determinant
  // is -1, then negate the matrix and the scaling factors.
  if Vector4DotProduct(row0, Vector4CrossProduct(row1, row2)) < 0 then begin
    for I:=0 to 2 do
      Tran[TTransType(Ord(ttScaleX) + I)]:=-Tran[TTransType(Ord(ttScaleX) + I)];
    NegateVector4(row0);
    NegateVector4(row1);
    NegateVector4(row2);
  end;

  // now, get the rotations out, as described in the gem
  Tran[ttRotateY]:=ArcSin(-row0[Z]);
  if cos(Tran[ttRotateY]) <> 0 then begin
    Tran[ttRotateX]:=ArcTan2(row1[Z], row2[Z]);
    Tran[ttRotateZ]:=ArcTan2(row0[Y], row0[X]);
  end else begin
    tran[ttRotateX]:=ArcTan2(row1[X], row1[Y]);
    tran[ttRotateZ]:=0;
  end;
  // All done!
  Result:=True;
end;


procedure Vector3ArrayLerp;
var
 pt : ^Single;
begin
 pt:=@t;
 exit;
 asm
        push ebx
        push edi

        mov   eax, src1
        mov   edx, src2
        mov   ecx, n
        shr   ecx, 1
        mov   ebx, dest
        mov   edi, pt

        db $0F,$0E               /// femms

        db $0F,$6E,$3F           /// movd     mm7, [edi]
        db $0F,$62,$FF           /// punpckldq mm7, mm7

 @@Loop:
        db $0F,$6F,$00           /// movq     mm0, [eax]
        db $0F,$6F,$50,$08       /// movq     mm2, [eax+8]
        db $0F,$6F,$60,$10       /// movq     mm4, [eax+16]
        db $0F,$6F,$C8           /// movq     mm1, mm0
        db $0F,$6F,$DA           /// movq     mm3, mm2
        db $0F,$6F,$EC           /// movq     mm5, mm4
        db $0F,$0F,$02,$AA       /// pfsubr   mm0, [edx]
        db $0F,$0F,$52,$08,$AA   /// pfsubr   mm2, [edx+8]
        db $0F,$0F,$62,$10,$AA   /// pfsubr   mm4, [edx+16]
        db $0F,$0D,$4B,$40       /// prefetchw [ebx+64]
        db $0F,$0F,$C7,$B4       /// pfmul    mm0, mm7
        db $0F,$0F,$D7,$B4       /// pfmul    mm2, mm7
        db $0F,$0F,$E7,$B4       /// pfmul    mm4, mm7
        db $0F,$0D,$40,$40       /// prefetch [eax+64]
        add   eax, 24
        add   edx, 24
        db $0F,$0F,$C1,$9E       /// pfadd    mm0, mm1
        db $0F,$0F,$D3,$9E       /// pfadd    mm2, mm3
        db $0F,$0F,$E5,$9E       /// pfadd    mm4, mm5
        db $0F,$0D,$42,$40       /// prefetch [edx+64]
        db $0F,$7F,$03           /// movq     [ebx], mm0
        db $0F,$7F,$53,$08       /// movq     [ebx+8], mm2
        db $0F,$7F,$63,$10       /// movq     [ebx+16], mm4

        add   ebx, 24

        dec   ecx
        jnz @@Loop

        db $0F,$0E               /// femms

        pop edi
        pop ebx
 end;
 if (n and 1)=1 then VectorLerp(src1[n-1], src2[n-1], t, dest[n-1]);
end;

procedure VectorLerp(const v1, v2 : TVector3; const t : Single; var vr : TVector3);
// EAX contains address of v1
// EDX contains address of v2
// EBX contains address of t
// ECX contains address of vr
asm
      fld   t

      fld   dword ptr [eax+0]
      fld   dword ptr [edx+0]
      fsub  st(0), st(1)
      fmul  st(0), st(2)
      faddp
      fstp  dword ptr [ecx+0]

      fld   dword ptr [eax+4]
      fld   dword ptr [edx+4]
      fsub  st(0), st(1)
      fmul  st(0), st(2)
      faddp
      fstp  dword ptr [ecx+4]

      fld   dword ptr [eax+8]
      fld   dword ptr [edx+8]
      fsub  st(0), st(1)
      fmul  st(0), st(2)
      faddp
      fstp  dword ptr [ecx+8]

      ffree st(0)
end;

function VectorAngleCosine(const V1, V2: TVector3): Single;
// EAX contains address of Vector1
// EDX contains address of Vector2
asm
      FLD DWORD PTR [EAX]           // V1[0]
      FLD ST                        // double V1[0]
      FMUL ST, ST                   // V1[0]^2 (prep. for divisor)
      FLD DWORD PTR [EDX]           // V2[0]
      FMUL ST(2), ST                // ST(2):=V1[0] * V2[0]
      FMUL ST, ST                   // V2[0]^2 (prep. for divisor)
      FLD DWORD PTR [EAX + 4]       // V1[1]
      FLD ST                        // double V1[1]
      FMUL ST, ST                   // ST(0):=V1[1]^2
      FADDP ST(3), ST               // ST(2):=V1[0]^2 + V1[1] *  * 2
      FLD DWORD PTR [EDX + 4]       // V2[1]
      FMUL ST(1), ST                // ST(1):=V1[1] * V2[1]
      FMUL ST, ST                   // ST(0):=V2[1]^2
      FADDP ST(2), ST               // ST(1):=V2[0]^2 + V2[1]^2
      FADDP ST(3), ST               // ST(2):=V1[0] * V2[0] + V1[1] * V2[1]
      FLD DWORD PTR [EAX + 8]       // load V2[1]
      FLD ST                        // same calcs go here
      FMUL ST, ST                   // (compare above)
      FADDP ST(3), ST
      FLD DWORD PTR [EDX + 8]
      FMUL ST(1), ST
      FMUL ST, ST
      FADDP ST(2), ST
      FADDP ST(3), ST
      FMULP                         // ST(0):=(V1[0]^2 + V1[1]^2 + V1[2]) *
                                    //          (V2[0]^2 + V2[1]^2 + V2[2])
      FSQRT                         // sqrt(ST(0))
      FDIVP                         // ST(0):=Result:=ST(1) / ST(0)
  // the result is expected in ST(0), if it's invalid, an error is raised
end;

function CreateRotationMatrix(const anAxis : TVector3; angle : Single) : TGLMatrixf4;
var
   axis : TVector3;
   cosine, sine, one_minus_cosine : Single;
begin
   SinCos(angle, sine, cosine);
   one_minus_cosine:=1-cosine;
   axis:=VectorNormalize(anAxis);

   Result[X, X]:=(one_minus_cosine * axis[0] * axis[0]) + cosine;
   Result[X, Y]:=(one_minus_cosine * axis[0] * axis[1]) - (axis[2] * sine);
   Result[X, Z]:=(one_minus_cosine * axis[2] * axis[0]) + (axis[1] * sine);
   Result[X, W]:=0;

   Result[Y, X]:=(one_minus_cosine * axis[0] * axis[1]) + (axis[2] * sine);
   Result[Y, Y]:=(one_minus_cosine * axis[1] * axis[1]) + cosine;
   Result[Y, Z]:=(one_minus_cosine * axis[1] * axis[2]) - (axis[0] * sine);
   Result[Y, W]:=0;

   Result[Z, X]:=(one_minus_cosine * axis[2] * axis[0]) - (axis[1] * sine);
   Result[Z, Y]:=(one_minus_cosine * axis[1] * axis[2]) + (axis[0] * sine);
   Result[Z, Z]:=(one_minus_cosine * axis[2] * axis[2]) + cosine;
   Result[Z, W]:=0;

   Result[W, X]:=0;
   Result[W, Y]:=0;
   Result[W, Z]:=0;
   Result[W, W]:=1;
end;

function VectorTransform(const V: TVector3; const M: TGLMatrixf4): TVector3;
begin
   Result[X]:=V[X] * M[X, X] + V[Y] * M[Y, X] + V[Z] * M[Z, X] + M[W, X];
   Result[Y]:=V[X] * M[X, Y] + V[Y] * M[Y, Y] + V[Z] * M[Z, Y] + M[W, Y];
   Result[Z]:=V[X] * M[X, Z] + V[Y] * M[Y, Z] + V[Z] * M[Z, Z] + M[W, Z];
end;

procedure RotateVector(var vector : TVector3; const axis : TVector3; angle: Single);
var
 rotMatrix : TMatrix4f;
begin
 rotMatrix:=CreateRotationMatrix(axis, Angle);
 vector:=VectorTransform(vector, rotMatrix);
end;

procedure SinCos(const Theta: Single; var Sin, Cos: Single); overload;
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
asm
   FLD  Theta
   FSINCOS
   FSTP DWORD PTR [EDX]    // cosine
   FSTP DWORD PTR [EAX]    // sine
end;

function VectorRotateAroundX(const v : TVector3; alpha : Single) : TVector3;
var
   c, s : Single;
begin
   SinCos(alpha, s, c);
   Result[0]:=v[0];
   Result[1]:=c*v[1]+s*v[2];
   Result[2]:=c*v[2]-s*v[1];
end;

function VectorRotateAroundY(const v : TVector3; alpha : Single) : TVector3;
var
   c, s : Single;
begin
   SinCos(alpha, s, c);
   Result[1]:=v[1];
   Result[0]:=c*v[0]+s*v[2];
   Result[2]:=c*v[2]-s*v[0];
end;



procedure SegmentSegmentClosestPoint(const S0Start, S0Stop, S1Start, S1Stop : TVector3; var Segment0Closest, Segment1Closest : TVector3);
const
  cSMALL_NUM = 0.000000001;
var
  u, v,w : TVector3;
  a,b,c,smalld,e, largeD, sc, sn, sD, tc, tN, tD : single;
begin
  Vector3Subtract(S0Stop, S0Start, u);
  Vector3Subtract(S1Stop, S1Start, v);
  Vector3Subtract(S0Start, S1Start, w);

  a := Vector3DotProduct(u,u);
  b := Vector3DotProduct(u,v);
  c := Vector3DotProduct(v,v);
  smalld := Vector3DotProduct(u,w);
  e := Vector3DotProduct(v,w);
  largeD := a*c - b*b;

  sD := largeD;
  tD := largeD;

  if LargeD<cSMALL_NUM then
  begin
    sN := 0.0;
    sD := 1.0;
    tN := e;
    tD := c;
  end else
  begin
    sN := (b*e - c*smallD);
    tN := (a*e - b*smallD);
    if (sN < 0.0) then
    begin
      sN := 0.0;
      tN := e;
      tD := c;
    end
    else if (sN > sD) then
    begin
      sN := sD;
      tN := e + b;
      tD := c;
    end;
  end;

  if (tN < 0.0) then
  begin
      tN := 0.0;
      // recompute sc for this edge
      if (-smalld < 0.0) then
          sN := 0.0
      else if (-smalld > a) then
          sN := sD
      else
      begin
          sN := -smalld;
          sD := a;
      end;
  end
  else if (tN > tD) then
  begin
      tN := tD;
      // recompute sc for this edge
      if ((-smallD + b) < 0.0) then
          sN := 0
      else if ((-smallD + b) > a) then
          sN := sD
      else
      begin
          sN := (-smallD + b);
          sD := a;
      end;
   end;

  // finally do the division to get sc and tc
  //sc := (abs(sN) < SMALL_NUM ? 0.0 : sN / sD);
  if abs(sN) < cSMALL_NUM then
    sc := 0
  else
    sc := sN/sD;

  //tc := (abs(tN) < SMALL_NUM ? 0.0 : tN / tD);
  if abs(tN) < cSMALL_NUM then
    tc := 0
  else
    tc := tN/tD;

  // get the difference of the two closest points
  //Vector   dP = w + (sc * u) - (tc * v);  // = S0(sc) - S1(tc)

  Segment0Closest := Vector3Add(S0Start, Vector3Scale(u, sc));
  Segment1Closest := Vector3Add(S1Start, Vector3Scale(v, tc));
end;

function SegmentSegmentDistance(const S0Start, S0Stop, S1Start, S1Stop : TVector3) : single;
var
  Pb0, PB1 : TVector3;
begin
  SegmentSegmentClosestPoint(S0Start, S0Stop, S1Start, S1Stop, PB0, PB1);
  result := Vector3Distance(PB0, PB1);
end;

function VectorPerpendicular(const V, N : TVector3) : TVector3;
var
   dot : Single;
begin
   dot:=Vector3DotProduct(V, N);
   Result[X]:=V[X]-Dot * N[X];
   Result[Y]:=V[Y]-Dot * N[Y];
   Result[Z]:=V[Z]-Dot * N[Z];
end;

function PlaneMake(const point, normal : TVector3) : TVector4; overload;
begin
   PVector3(@Result)^:=normal;
   Result[3]:=-Vector3DotProduct(point, normal);
end;

function PlaneMake(const p1, p2, p3 : TVector3) : TVector4;
begin
   CalcPlaneNormal(p1, p2, p3, PVector3(@Result)^);
//   NormalizeVector3(PVector3(@Result)^);
   Result[3]:=-Vector3DotProduct(p1, PVector3(@Result)^);
end;

function ClosestPointOnLine(var a, b, p : TVector3): TVector3;
var d, t: double;
    c, v: TVector3;
begin
    Vector3Subtract(p, a, c);
    Vector3Subtract(b, a, v);

    d:=Vector3Length(v);
    NormalizeVector3(v);
    t:=Vector3DotProduct(v,c);

    //Check to see if t is beyond the extents of the line segment
    if (t < 0.0) then result:=a
    else if (t > d) then result:=b
    else begin
      v[0]:=v[0]*t;
      v[1]:=v[1]*t;
      v[2]:=v[2]*t;
      result:=Vector3Add(a, v);
    end;
end;

function ClosestPointOnTriangleEdge(var a, b, c, p: TVector3): TVector3;
var
   dAB, dBC, dCA : Single;
   Rab, Rbc, Rca : TVector3;
begin
    Rab:=ClosestPointOnLine(a, b, p);
    Rbc:=ClosestPointOnLine(b, c, p);
    Rca:=ClosestPointOnLine(c, a, p);

    dAB:=Vector3Distance2(p, Rab);
    dBC:=Vector3Distance2(p, Rbc);
    dCA:=Vector3Distance2(p, Rca);

    if dBC<dAB then
      if dCA<dBC then
         Result:=Rca
      else Result:=Rbc
    else if dCA<dAB then
      Result:=Rca
    else Result:=Rab;
end;

function CheckPointInTriangle(const point:TVector3; var a, b, c: TVector3):boolean;
var
  total_angles:Single;
  v1,v2,v3:TVector3;
begin
  total_angles := 0;

  // make the 3 vectors
  v1 := Vector3Subtract(point,a);
  v2 := Vector3Subtract(point,b);
  v3 := Vector3Subtract(point,c);

  normalizeVector3(v1);
  normalizeVector3(v2);
  normalizeVector3(v3);

  total_angles := total_angles + arccos(Vector3DotProduct(v1,v2));
  total_angles := total_angles + arccos(Vector3DotProduct(v2,v3));
  total_angles := total_angles + arccos(Vector3DotProduct(v3,v1));

  if (abs(total_angles-2*PI) <= 0.0005) then
    result:= TRUE
  else
    result:=FALSE;
end;

function CheckPointInRect(const point:TVector3; var a, b, c, d: TVector3):boolean;
var
  total_angles:Single;
  v1,v2,v3,v4:TVector3;
begin
  total_angles := 0;

  // make the 3 vectors
  v1 := Vector3Subtract(point,a);
  v2 := Vector3Subtract(point,b);
  v3 := Vector3Subtract(point,c);
  v4 := Vector3Subtract(point,d);
  normalizeVector3(v1);
  normalizeVector3(v2);
  normalizeVector3(v3);
  normalizeVector3(v4);

  total_angles := total_angles + arccos(Vector3DotProduct(v1,v2));
  total_angles := total_angles + arccos(Vector3DotProduct(v2,v3));
  total_angles := total_angles + arccos(Vector3DotProduct(v3,v4));
  total_angles := total_angles + arccos(Vector3DotProduct(v4,v1));

  if (abs(total_angles-2*PI) <= 0.0005) then
    result:= TRUE
  else
    result:=FALSE;
end;

function RayCastTriangleIntersect;
var
   pvec : TVector3;
   v1, v2, qvec, tvec : TVector3;
   t, u, v, det, invDet : Single;
begin
   Vector3Subtract(p2, p1, v1);
   Vector3Subtract(p3, p1, v2);
   Vector3CrossProduct(rayVector, v2, pvec);
   det:=Vector3DotProduct(v1, pvec);
   if ((det<error) and (det>-error)) then begin // vector is parallel to triangle's plane
      Result:=False;
      Exit;
   end;
   invDet:=1/det;
   Vector3Subtract(rayStart, p1, tvec);
   u:=Vector3DotProduct(tvec, pvec)*invDet;
   if (u<0) or (u>1) then
      Result:=False
   else begin
      qvec:=Vector3CrossProduct(tvec, v1);
      v:=Vector3DotProduct(rayVector, qvec)*invDet;
      Result:=(v>=0) and (u+v<=1);
      if Result then begin
         t:=Vector3DotProduct(v2, qvec)*invDet;
         if (t>0) then begin
            if intersectPoint<>nil then
               Vector3Combine(rayStart, rayVector, t, intersectPoint^);
            if intersectNormal<>nil then
               Vector3CrossProduct(v1, v2, intersectNormal^);
         end else Result:=False;
      end;
   end;
end;

function RayCastPlaneIntersect;
var
   sp : TVector3;
   t, d : Single;
begin
   d:=Vector3DotProduct(rayVector, planeNormal);
   Result:=((d>error) or (d<-error));
   if Result and Assigned(intersectPoint) then begin
      Vector3Subtract(planePoint, rayStart, sp);
      d:=1/d; // will keep one FPU unit busy during dot product calculation
      t:=Vector3DotProduct(sp, planeNormal)*d;
      if t>0 then
         Vector3Combine(rayStart, rayVector, t, intersectPoint^)
      else Result:=False;
   end;
end;

function Vector3Norm(const v : TVector3) : Single;
// EAX contains address of V
// result is passed in ST(0)
asm
 FLD DWORD PTR [EAX];
 FMUL ST, ST
 FLD DWORD PTR [EAX+4];
 FMUL ST, ST
 FADD
 FLD DWORD PTR [EAX+8];
 FMUL ST, ST
 FADD
end;

function VectorNormalize(const v : TVector3) : TVector3;
asm
 test vSIMD, 1
 jz @@FPU
@@3DNow:
 db $0F,$6F,$00           /// movq        mm0,[eax]
 db $0F,$6E,$48,$08       /// movd        mm1,[eax+8]
 db $0F,$6F,$E0           /// movq        mm4,mm0
 db $0F,$6F,$D9           /// movq        mm3,mm1
 db $0F,$0F,$C0,$B4       /// pfmul       mm0,mm0
 db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
 db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
 db $0F,$0F,$C1,$9E       /// pfadd       mm0,mm1
 db $0F,$0F,$C8,$97       /// pfrsqrt     mm1,mm0
 db $0F,$6F,$D1           /// movq        mm2,mm1

 db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
 db $0F,$0F,$C8,$A7       /// pfrsqit1    mm1,mm0
 db $0F,$0F,$CA,$B6       /// pfrcpit2    mm1,mm2
 db $0F,$62,$C9           /// punpckldq   mm1,mm1
 db $0F,$0F,$D9,$B4       /// pfmul       mm3,mm1
 db $0F,$0F,$E1,$B4       /// pfmul       mm4,mm1
 db $0F,$7E,$5A,$08       /// movd        [edx+8],mm3
 db $0F,$7F,$22           /// movq        [edx],mm4
@@norm_end:
 db $0F,$0E               /// femms
 ret

@@FPU:
 FLD  DWORD PTR [EAX]
 FMUL ST, ST
 FLD  DWORD PTR [EAX+4]
 FMUL ST, ST
 FADD
 FLD  DWORD PTR [EAX+8]
 FMUL ST, ST
 FADD
 FSQRT
 FLD1
 FDIVR
 FLD  ST
 FMUL DWORD PTR [EAX]
 FSTP DWORD PTR [EDX]
 FLD  ST
 FMUL DWORD PTR [EAX+4]
 FSTP DWORD PTR [EDX+4]
 FMUL DWORD PTR [EAX+8]
 FSTP DWORD PTR [EDX+8]
end;

procedure Vector3Combine(const V1, V2: TVector3; const F2: Single; var vr : TVector3);
begin      // 201283
   vr[0]:=V1[0] + (F2 * V2[0]);
   vr[1]:=V1[1] + (F2 * V2[1]);
   vr[2]:=V1[2] + (F2 * V2[2]);
end;

function Vector3Distance2(const v1, v2 : TVector3) : Single;
// EAX contains address of v1
// EDX contains highest of v2
// Result is passed on the stack
asm
 FLD  DWORD PTR [EAX]
 FSUB DWORD PTR [EDX]
 FMUL ST, ST
 FLD  DWORD PTR [EAX+4]
 FSUB DWORD PTR [EDX+4]
 FMUL ST, ST
 FADD
 FLD  DWORD PTR [EAX+8]
 FSUB DWORD PTR [EDX+8]
 FMUL ST, ST
 FADD
end;

function RayCastSphereIntersect;
var
   proj, d2 : Single;
   id2 : Integer;
   projPoint : TVector3;
begin
   proj:=PointProject(sphereCenter, rayStart, rayVector);
   Vector3Combine(rayStart, rayVector, proj, projPoint);
   d2:=sphereRadius*sphereRadius-Vector3Distance2(sphereCenter, projPoint);
   id2:=PInteger(@d2)^;
   if id2>=0 then begin
      if id2=0 then begin
         if PInteger(@proj)^>0 then begin
            Vector3Combine(rayStart, rayVector, proj, i1);
            Result:=1;
            Exit;
         end;
      end else if id2>0 then begin
         d2:=Sqrt(d2);
         if proj>=d2 then begin
            Vector3Combine(rayStart, rayVector, proj-d2, i1);
            Vector3Combine(rayStart, rayVector, proj+d2, i2);
            Result:=2;
            Exit;
         end else if proj+d2>=0 then begin
            Vector3Combine(rayStart, rayVector, proj+d2, i1);
            Result:=1;
            Exit;
         end;
      end;
   end;
   Result:=0;
end;

function PointProject(const p, origin, direction : TVector3) : Single;
// EAX -> p, EDX -> origin, ECX -> direction
asm
 fld   dword ptr [eax]
 fsub  dword ptr [edx]
 fmul  dword ptr [ecx]
 fld   dword ptr [eax+4]
 fsub  dword ptr [edx+4]
 fmul  dword ptr [ecx+4]
 fadd
 fld   dword ptr [eax+8]
 fsub  dword ptr [edx+8]
 fmul  dword ptr [ecx+8]
 fadd
end;


procedure Vector3Scale(const v : TVector3; factor : Single; var vr : TVector3);
asm
 FLD  DWORD PTR [EAX]
 FMUL DWORD PTR [EBP+8]
 FSTP DWORD PTR [EDX]
 FLD  DWORD PTR [EAX+4]
 FMUL DWORD PTR [EBP+8]
 FSTP DWORD PTR [EDX+4]
 FLD  DWORD PTR [EAX+8]
 FMUL DWORD PTR [EBP+8]
 FSTP DWORD PTR [EDX+8]
end;

function Vector3Subtract(const v1, v2 : TVector3): TVector3;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
asm
 FLD  DWORD PTR [EAX]
 FSUB DWORD PTR [EDX]
 FSTP DWORD PTR [ECX]
 FLD  DWORD PTR [EAX+4]
 FSUB DWORD PTR [EDX+4]
 FSTP DWORD PTR [ECX+4]
 FLD  DWORD PTR [EAX+8]
 FSUB DWORD PTR [EDX+8]
 FSTP DWORD PTR [ECX+8]
end;

function Vector3Length(const v : TVector3) : Single;
// EAX contains address of V
// result is passed in ST(0)
asm
 FLD  DWORD PTR [EAX]
 FMUL ST, ST
 FLD  DWORD PTR [EAX+4]
 FMUL ST, ST
 FADDP
 FLD  DWORD PTR [EAX+8]
 FMUL ST, ST
 FADDP
 FSQRT
end;

function Vector3Negate(const v : TVector3) : TVector3;
// EAX contains address of v
// EDX contains address of Result
asm
 FLD DWORD PTR [EAX]
 FCHS
 FSTP DWORD PTR [EDX]
 FLD DWORD PTR [EAX+4]
 FCHS
 FSTP DWORD PTR [EDX+4]
 FLD DWORD PTR [EAX+8]
 FCHS
 FSTP DWORD PTR [EDX+8]
end;

procedure CalcPlaneNormal(const p1, p2, p3 : TVector3; var vr : TVector3);
var
   v1, v2 : TVector3;
begin
   Vector3Subtract(p2, p1, v1);
   Vector3Subtract(p3, p1, v2);
   Vector3CrossProduct(v1, v2, vr);
   NormalizeVector3(vr);
end;

procedure Vector3CrossProduct(const v1, v2 : TVector3; var vr : TVector3);
begin
 vr[X]:=v1[Y]*v2[Z]-v1[Z]*v2[Y];
 vr[Y]:=v1[Z]*v2[X]-v1[X]*v2[Z];
 vr[Z]:=v1[X]*v2[Y]-v1[Y]*v2[X];
end;

function Vector3CrossProduct(const v1, v2 : TVector3) : TVector3;
begin
   Result[X]:=v1[Y]*v2[Z]-v1[Z]*v2[Y];
   Result[Y]:=v1[Z]*v2[X]-v1[X]*v2[Z];
   Result[Z]:=v1[X]*v2[Y]-v1[Y]*v2[X];
end;

procedure NormalizeVector3(var v : TVector3);
asm
 test vSIMD, 1
 jz @@FPU
@@3DNow:
 db $0F,$6F,$00           /// movq        mm0,[eax]
 db $0F,$6E,$48,$08       /// movd        mm1,[eax+8]
 db $0F,$6F,$E0           /// movq        mm4,mm0
 db $0F,$6F,$D9           /// movq        mm3,mm1
 db $0F,$0F,$C0,$B4       /// pfmul       mm0,mm0
 db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
 db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
 db $0F,$0F,$C1,$9E       /// pfadd       mm0,mm1
 db $0F,$0F,$C8,$97       /// pfrsqrt     mm1,mm0
 db $0F,$6F,$D1           /// movq        mm2,mm1

 db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
 db $0F,$0F,$C8,$A7       /// pfrsqit1    mm1,mm0
 db $0F,$0F,$CA,$B6       /// pfrcpit2    mm1,mm2
 db $0F,$62,$C9           /// punpckldq   mm1,mm1
 db $0F,$0F,$D9,$B4       /// pfmul       mm3,mm1
 db $0F,$0F,$E1,$B4       /// pfmul       mm4,mm1
 db $0F,$7E,$58,$08       /// movd        [eax+8],mm3
 db $0F,$7F,$20           /// movq        [eax],mm4
@@norm_end:
 db $0F,$0E               /// femms
 ret

@@FPU:
 FLD  DWORD PTR [EAX]
 FMUL ST, ST
 FLD  DWORD PTR [EAX+4]
 FMUL ST, ST
 FADD
 FLD  DWORD PTR [EAX+8]
 FMUL ST, ST
 FADD
 FSQRT
 FLD1
 FDIVR
 FLD  ST
 FMUL DWORD PTR [EAX]
 FSTP DWORD PTR [EAX]
 FLD  ST
 FMUL DWORD PTR [EAX+4]
 FSTP DWORD PTR [EAX+4]
 FMUL DWORD PTR [EAX+8]
 FSTP DWORD PTR [EAX+8]
end;

procedure ScaleFloatArray(values : pointer; nb : Integer; var factor : Single);
asm
 test vSIMD, 1
 jz @@FPU

 push  edx
 shr   edx, 2
 or    edx, edx
 jz    @@FPU

 db $0F,$6E,$39           /// movd        mm7, [ecx]
 db $0F,$62,$FF           /// punpckldq   mm7, mm7

@@3DNowLoop:
 db $0F,$0D,$48,$40       /// prefetchw [eax+64]
 db $0F,$6F,$00           /// movq  mm0, [eax]
 db $0F,$6F,$48,$08       /// movq  mm1, [eax+8]
 db $0F,$0F,$C7,$B4       /// pfmul mm0, mm7
 db $0F,$0F,$CF,$B4       /// pfmul mm1, mm7
 db $0F,$7F,$00           /// movq  [eax], mm0
 db $0F,$7F,$48,$08       /// movq  [eax+8], mm1

 add   eax, 16
 dec   edx
 jnz   @@3DNowLoop

 pop   edx
 and   edx, 3
 db $0F,$0E               /// femms

@@FPU:
 push  edx
 shr   edx, 1
 or    edx, edx
 jz    @@FPULone

@@FPULoop:
 fld   dword ptr [eax]
 fmul  dword ptr [ecx]
 fstp  dword ptr [eax]
 fld   dword ptr [eax+4]
 fmul  dword ptr [ecx]
 fstp  dword ptr [eax+4]

 add   eax, 8
 dec   edx
 jnz   @@FPULoop

@@FPULone:
 pop   edx
 test  edx, 1
 jz    @@End

 fld   dword ptr [eax]
 fmul  dword ptr [ecx]
 fstp  dword ptr [eax]

@@End:
end;

function Add(A,B:smallInt):SmallInt; overload;
asm
 add AX,B;
 cmp AX,255;
 jle @end;
 mov AX,255;
 @end:
end;

function Add(const v1, v2 : single) : single; overload;
begin
asm
 FLD  v1
 FADD v2
 FSTP result
end;
 result:=ClampValue(result, 0, 1);
end;

function AddMul(B : smallInt; var s : Single; A:Integer) : Integer;
asm
   push  EAX
   fild  dword ptr [esp]
   fmul  dword ptr [edx]
   fistp dword ptr [esp]
   pop EAX;

   Add EAX,A;
   cmp EAX,255;
   jle @end;
   mov EAX,255;
   @end:
end;
 
function IsNan(const AValue: Double): Boolean;
begin
  Result := ((PInt64(@AValue)^ and $7FF0000000000000)  = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) <> $0000000000000000)
end;

function Vector3Equals(const V1, V2: TVector3) : Boolean;
// EAX contains address of v1
// EDX contains highest of v2
begin
 result:=false;
 if (abs(V1[0]-v2[0])<error)and(abs(V1[1]-v2[1])<error)and(abs(V1[2]-v2[2])<error)then result:=true;
end;
{
asm
 mov ecx, [edx]
 cmp ecx, [eax]
 jne @@Diff
 mov ecx, [edx+$4]
 cmp ecx, [eax+$4]
 jne @@Diff
 mov ecx, [edx+$8]
 cmp ecx, [eax+$8]
 jne @@Diff
@@Equal:
 mov al, 1
 ret
@@Diff:
 xor eax, eax
@@End:
end;     }

procedure Vector3Add(const v1, v2 : TVector3; var vr : TVector3); overload;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
asm
 FLD  DWORD PTR [EAX]
 FADD DWORD PTR [EDX]
 FSTP DWORD PTR [ECX]
 FLD  DWORD PTR [EAX+4]
 FADD DWORD PTR [EDX+4]
 FSTP DWORD PTR [ECX+4]
 FLD  DWORD PTR [EAX+8]
 FADD DWORD PTR [EDX+8]
 FSTP DWORD PTR [ECX+8]
end;

function Vector3Scale(const v : TVector3; factor : Single) : TVector3;
asm
 FLD  DWORD PTR [EAX]
 FMUL DWORD PTR [EBP+8]
 FSTP DWORD PTR [EDX]
 FLD  DWORD PTR [EAX+4]
 FMUL DWORD PTR [EBP+8]
 FSTP DWORD PTR [EDX+4]
 FLD  DWORD PTR [EAX+8]
 FMUL DWORD PTR [EBP+8]
 FSTP DWORD PTR [EDX+8]
end;

function ClampValue(const aValue, aMin, aMax : Single) : Single;
asm   // 118
 fld   aValue
 fcom  aMin
 fstsw ax
 sahf
 jb    @@ReturnMin
@@CompMax:
 fcom  aMax
 fstsw ax
 sahf
 jnbe  @@ReturnMax
 pop   ebp
 ret   $0C
@@ReturnMax:
 fld   aMax
 jmp @@End
@@ReturnMin:
 fld   aMin
@@End:
 ffree st(1)
end;

function Vector3DotProduct(const V1, V2 : TVector3): Single;
// EAX contains address of V1
// EDX contains address of V2
// result is stored in ST(0)
asm
 FLD DWORD PTR [eax]
 FMUL DWORD PTR [edx]
 FLD DWORD PTR [eax+4]
 FMUL DWORD PTR [edx+4]
 faddp
 FLD DWORD PTR [eax+8]
 FMUL DWORD PTR [edx+8]
 faddp
end;

procedure Vector3Subtract(const v1, v2 : TVector3; var result : TVector3);
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
asm
 FLD  DWORD PTR [EAX]
 FSUB DWORD PTR [EDX]
 FSTP DWORD PTR [ECX]
 FLD  DWORD PTR [EAX+4]
 FSUB DWORD PTR [EDX+4]
 FSTP DWORD PTR [ECX+4]
 FLD  DWORD PTR [EAX+8]
 FSUB DWORD PTR [EDX+8]
 FSTP DWORD PTR [ECX+8]
end;

function Vector3Distance(const v1, v2 : TVector3) : Single;
// EAX contains address of v1
// EDX contains highest of v2
// Result  is passed on the stack
asm
 FLD  DWORD PTR [EAX]
 FSUB DWORD PTR [EDX]
 FMUL ST, ST
 FLD  DWORD PTR [EAX+4]
 FSUB DWORD PTR [EDX+4]
 FMUL ST, ST
 FADD
 FLD  DWORD PTR [EAX+8]
 FSUB DWORD PTR [EDX+8]
 FMUL ST, ST
 FADD
 FSQRT
end;

function PointSegmentDistance(const point, segmentStart, segmentStop : TVector3) : Single;
var
   pb : TVector3;
begin
   pb:=PointSegmentClosestPoint(point, segmentStart, segmentStop);
   Result:=Vector3Distance(point, pb);
end;

function PointSegmentClosestPoint(const point, segmentStart, segmentStop : TVector3) : TVector3;
var
   w, lineDirection : TVector3;
   c1, c2, b : Single;
begin
   Vector3Subtract(segmentStop, segmentStart, lineDirection);
   Vector3Subtract(point, segmentStart, w);

   c1:=Vector3DotProduct(w, lineDirection);
   c2:=Vector3DotProduct(lineDirection, lineDirection);
   b:=ClampValue(c1/c2, 0, 1);

   Vector3Add(segmentStart, Vector3Scale(lineDirection, b), Result);
end;

function Vector4Lerp(const V1, V2: TVector4; t: Single): TVector4;
begin
   Result[X]:=V1[X]+(V2[X]-V1[X])*t;
   Result[Y]:=V1[Y]+(V2[Y]-V1[Y])*t;
   Result[Z]:=V1[Z]+(V2[Z]-V1[Z])*t;
   Result[W]:=V1[W]+(V2[W]-V1[W])*t;
end;

function Vector2Lerp(const V1, V2 : TVector2; t : Single) : TVector2;
begin
   Result[0]:=V1[0]+(V2[0]-V1[0])*t;
   Result[1]:=V1[1]+(V2[1]-V1[1])*t;
end;

function Vector3Lerp(const V1, V2: TVector3; t: Single): TVector3; overload;
asm
 fld   t

 fld   dword ptr [eax+0]
 fld   dword ptr [edx+0]
 fsub  st(0), st(1)
 fmul  st(0), st(2)
 faddp
 fstp  dword ptr [ecx+0]

 fld   dword ptr [eax+4]
 fld   dword ptr [edx+4]
 fsub  st(0), st(1)
 fmul  st(0), st(2)
 faddp
 fstp  dword ptr [ecx+4]

 fld   dword ptr [eax+8]
 fld   dword ptr [edx+8]
 fsub  st(0), st(1)
 fmul  st(0), st(2)
 faddp
 fstp  dword ptr [ecx+8]

 ffree st(0)
end;

procedure Vector3Lerp(const v1, v2 : TVector3; t : Single; var vr : TVector3); overload;
// EAX contains address of v1
// EDX contains address of v2
// EBX contains address of t
// ECX contains address of vr
asm
 fld   t

 fld   dword ptr [eax+0]
 fld   dword ptr [edx+0]
 fsub  st(0), st(1)
 fmul  st(0), st(2)
 faddp
 fstp  dword ptr [ecx+0]

 fld   dword ptr [eax+4]
 fld   dword ptr [edx+4]
 fsub  st(0), st(1)
 fmul  st(0), st(2)
 faddp
 fstp  dword ptr [ecx+4]

 fld   dword ptr [eax+8]
 fld   dword ptr [edx+8]
 fsub  st(0), st(1)
 fmul  st(0), st(2)
 faddp
 fstp  dword ptr [ecx+8]

 ffree st(0)
end;

function Lerp(const start, stop, t : Single) : Single;
begin
   Result:=start+(stop-start)*t;
end;

procedure AddVector3(var v1 : TVector3; const v2 : TVector3);
// EAX contains address of V1
// EDX contains address of V2
asm
 FLD  DWORD PTR [EAX]
 FADD DWORD PTR [EDX]
 FSTP DWORD PTR [EAX]
 FLD  DWORD PTR [EAX+4]
 FADD DWORD PTR [EDX+4]
 FSTP DWORD PTR [EAX+4]
 FLD  DWORD PTR [EAX+8]
 FADD DWORD PTR [EDX+8]
 FSTP DWORD PTR [EAX+8]
end;

procedure SinCos(const theta, radius : Single; var Sin, Cos: Single); overload;
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
asm
 FLD  theta
 FSINCOS
 FMUL radius
 FSTP DWORD PTR [EDX]    // cosine
 FMUL radius
 FSTP DWORD PTR [EAX]    // sine
end;


procedure RandomPointOnSphere(var p : TVector3);
var
   t, w : Single;
begin
   p[2]:=2*Random-1;
   t:=2*PI*Random;
   w:=Sqrt(1-p[2]*p[2]);
   SinCos(t, w, p[1], p[0]);
end;

function ScaleAndRound(i : Integer; var s : Single) : Integer;
asm
 push  eax
 fild  dword ptr [esp]
 fmul  dword ptr [edx]
 fistp dword ptr [esp]
 pop   eax
end;
 
function Round(v : Single) : Integer;
asm
      FLD     v
      FISTP   DWORD PTR [v]     // use v as storage to place the result
      MOV     EAX, [v]
end;

function Trunc64(v : Extended) : Int64;
asm
      SUB     ESP,12
      FSTCW   [ESP]
      FLDCW   cwChop
      FLD     v
      FISTP   qword ptr [ESP+4]
      FLDCW   [ESP]
      POP     ECX
      POP     EAX
      POP     EDX
end;

function Trunc(v : Single) : Integer;
asm
      SUB     ESP,8
      FSTCW   [ESP]
      FLDCW   cwChop
      FLD     v
      FISTP   dword ptr [ESP+4]
      FLDCW   [ESP]
      POP     ECX
      POP     EAX
end;

function GetBoundingBox(var a:array of TVector3):T3DBox;
var
 i:integer;
 minx, maxx, miny, maxy, minz, maxz:single;
begin
 minx:=10000000000000;
 miny:=10000000000000;
 minz:=10000000000000;

 maxx:=-10000000000000;
 maxy:=-10000000000000;
 maxz:=-10000000000000;
 for i:=0 to high(a) do begin
        if a[i][0]<minx then minx:=a[i][0];
        if a[i][0]>maxx then maxx:=a[i][0];
                
        if a[i][1]<miny then miny:=a[i][1];
        if a[i][1]>maxy then maxy:=a[i][1];

        if a[i][2]<minz then minz:=a[i][2];
        if a[i][2]>maxz then maxz:=a[i][2];
 end;

 result.Width:= abs(maxx-minx);
 result.Height:=abs(maxy-miny);
 result.Depth:= abs(maxz-minz);
 result.center[0]:=minx+result.Width/2;
 result.center[1]:=miny+result.Height/2;
 result.center[2]:=minz+result.Depth/2;
end;

procedure ScaleVector3(var v : TVector3; factor: Single);
asm
 FLD  DWORD PTR [EAX]
 FMUL DWORD PTR [EBP+8]
 FSTP DWORD PTR [EAX]
 FLD  DWORD PTR [EAX+4]
 FMUL DWORD PTR [EBP+8]
 FSTP DWORD PTR [EAX+4]
 FLD  DWORD PTR [EAX+8]
 FMUL DWORD PTR [EBP+8]
 FSTP DWORD PTR [EAX+8]
end;

function Vector3Add(const v1, v2 : TVector3) : TVector3;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
asm
 FLD  DWORD PTR [EAX]
 FADD DWORD PTR [EDX]
 FSTP DWORD PTR [ECX]
 FLD  DWORD PTR [EAX+4]
 FADD DWORD PTR [EDX+4]
 FSTP DWORD PTR [ECX+4]
 FLD  DWORD PTR [EAX+8]
 FADD DWORD PTR [EDX+8]
 FSTP DWORD PTR [ECX+8]
end;

function MakeVector3(x,y,z:single):TVector3;
begin
 result[0]:=x;
 result[1]:=y;
 result[2]:=z;
end;

function MakeVector4;
begin
 result[0]:=x;
 result[1]:=y;
 result[2]:=z;
 result[3]:=w;
end;

procedure Vector3Normalize(var v : TVector3);
asm
 test vSIMD, 1
 jz @@FPU
 @@3DNow:
        db $0F,$6F,$00           /// movq        mm0,[eax]
        db $0F,$6E,$48,$08       /// movd        mm1,[eax+8]
        db $0F,$6F,$E0           /// movq        mm4,mm0
        db $0F,$6F,$D9           /// movq        mm3,mm1
        db $0F,$0F,$C0,$B4       /// pfmul       mm0,mm0
        db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
        db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
        db $0F,$0F,$C1,$9E       /// pfadd       mm0,mm1
        db $0F,$0F,$C8,$97       /// pfrsqrt     mm1,mm0
        db $0F,$6F,$D1           /// movq        mm2,mm1

        db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
        db $0F,$0F,$C8,$A7       /// pfrsqit1    mm1,mm0
        db $0F,$0F,$CA,$B6       /// pfrcpit2    mm1,mm2
        db $0F,$62,$C9           /// punpckldq   mm1,mm1
        db $0F,$0F,$D9,$B4       /// pfmul       mm3,mm1
        db $0F,$0F,$E1,$B4       /// pfmul       mm4,mm1
        db $0F,$7E,$58,$08       /// movd        [eax+8],mm3
        db $0F,$7F,$20           /// movq        [eax],mm4
 @@norm_end:
        db $0F,$0E               /// femms
        ret

 @@FPU:
        FLD  DWORD PTR [EAX]
        FMUL ST, ST
        FLD  DWORD PTR [EAX+4]
        FMUL ST, ST
        FADD
        FLD  DWORD PTR [EAX+8]
        FMUL ST, ST
        FADD
        FSQRT
        FLD1
        FDIVR
        FLD  ST
        FMUL DWORD PTR [EAX]
        FSTP DWORD PTR [EAX]
        FLD  ST
        FMUL DWORD PTR [EAX+4]
        FSTP DWORD PTR [EAX+4]
        FMUL DWORD PTR [EAX+8]
        FSTP DWORD PTR [EAX+8]
end;

function MatrixMultiply(const M1, M2: TMatrix): TMatrix;
begin
 if vSIMD=1 then begin
        asm
         xchg eax, ecx
         db $0F,$6F,$01           /// movq        mm0,[ecx]
         db $0F,$6F,$49,$08       /// movq        mm1,[ecx+8]
         db $0F,$6F,$22           /// movq        mm4,[edx]
         db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
         db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
         db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
         db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
         db $0F,$62,$C0           /// punpckldq   mm0,mm0
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$42,$08,$B4   /// pfmul       mm0, [edx+8]
         db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
         db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
         db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
         db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
         db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
         db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
         db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
         db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
         db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
         db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
         db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
         db $0F,$6F,$41,$10       /// movq        mm0,[ecx+16]
         db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
         db $0F,$6F,$49,$18       /// movq        mm1,[ecx+24]
         db $0F,$7F,$38           /// movq        [eax],mm7
         db $0F,$6F,$22           /// movq        mm4,[edx]
         db $0F,$7F,$58,$08       /// movq        [eax+8],mm3

         db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
         db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
         db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
         db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
         db $0F,$62,$C0           /// punpckldq   mm0,mm0
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
         db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
         db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
         db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
         db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
         db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
         db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
         db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
         db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
         db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
         db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
         db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
         db $0F,$6F,$41,$20       /// movq        mm0,[ecx+32]
         db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
         db $0F,$6F,$49,$28       /// movq        mm1,[ecx+40]
         db $0F,$7F,$78,$10       /// movq        [eax+16],mm7
         db $0F,$6F,$22           /// movq        mm4,[edx]
         db $0F,$7F,$58,$18       /// movq        [eax+24],mm3

         db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
         db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
         db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
         db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
         db $0F,$62,$C0           /// punpckldq   mm0,mm0
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
         db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
         db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
         db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
         db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
         db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
         db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
         db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
         db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
         db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
         db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
         db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
         db $0F,$6F,$41,$30       /// movq        mm0,[ecx+48]
         db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
         db $0F,$6F,$49,$38       /// movq        mm1,[ecx+56]
         db $0F,$7F,$78,$20       /// movq        [eax+32],mm7
         db $0F,$6F,$22           /// movq        mm4,[edx]
         db $0F,$7F,$58,$28       /// movq        [eax+40],mm3

         db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
         db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
         db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
         db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
         db $0F,$62,$C0           /// punpckldq   mm0,mm0
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
         db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
         db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
         db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
         db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
         db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
         db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
         db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
         db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
         db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
         db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
         db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
         db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
         db $0F,$7F,$78,$30       /// movq        [eax+48],mm7
         db $0F,$7F,$58,$38       /// movq        [eax+56],mm3
         db $0F,$0E               /// femms
        end;
 end 
 else begin
        Result[X,X]:=M1[X,X]*M2[X,X]+M1[X,Y]*M2[Y,X]+M1[X,Z]*M2[Z,X]+M1[X,W]*M2[W,X];
        Result[X,Y]:=M1[X,X]*M2[X,Y]+M1[X,Y]*M2[Y,Y]+M1[X,Z]*M2[Z,Y]+M1[X,W]*M2[W,Y];
        Result[X,Z]:=M1[X,X]*M2[X,Z]+M1[X,Y]*M2[Y,Z]+M1[X,Z]*M2[Z,Z]+M1[X,W]*M2[W,Z];
        Result[X,W]:=M1[X,X]*M2[X,W]+M1[X,Y]*M2[Y,W]+M1[X,Z]*M2[Z,W]+M1[X,W]*M2[W,W];
        Result[Y,X]:=M1[Y,X]*M2[X,X]+M1[Y,Y]*M2[Y,X]+M1[Y,Z]*M2[Z,X]+M1[Y,W]*M2[W,X];
        Result[Y,Y]:=M1[Y,X]*M2[X,Y]+M1[Y,Y]*M2[Y,Y]+M1[Y,Z]*M2[Z,Y]+M1[Y,W]*M2[W,Y];
        Result[Y,Z]:=M1[Y,X]*M2[X,Z]+M1[Y,Y]*M2[Y,Z]+M1[Y,Z]*M2[Z,Z]+M1[Y,W]*M2[W,Z];
        Result[Y,W]:=M1[Y,X]*M2[X,W]+M1[Y,Y]*M2[Y,W]+M1[Y,Z]*M2[Z,W]+M1[Y,W]*M2[W,W];
        Result[Z,X]:=M1[Z,X]*M2[X,X]+M1[Z,Y]*M2[Y,X]+M1[Z,Z]*M2[Z,X]+M1[Z,W]*M2[W,X];
        Result[Z,Y]:=M1[Z,X]*M2[X,Y]+M1[Z,Y]*M2[Y,Y]+M1[Z,Z]*M2[Z,Y]+M1[Z,W]*M2[W,Y];
        Result[Z,Z]:=M1[Z,X]*M2[X,Z]+M1[Z,Y]*M2[Y,Z]+M1[Z,Z]*M2[Z,Z]+M1[Z,W]*M2[W,Z];
        Result[Z,W]:=M1[Z,X]*M2[X,W]+M1[Z,Y]*M2[Y,W]+M1[Z,Z]*M2[Z,W]+M1[Z,W]*M2[W,W];
        Result[W,X]:=M1[W,X]*M2[X,X]+M1[W,Y]*M2[Y,X]+M1[W,Z]*M2[Z,X]+M1[W,W]*M2[W,X];
        Result[W,Y]:=M1[W,X]*M2[X,Y]+M1[W,Y]*M2[Y,Y]+M1[W,Z]*M2[Z,Y]+M1[W,W]*M2[W,Y];
        Result[W,Z]:=M1[W,X]*M2[X,Z]+M1[W,Y]*M2[Y,Z]+M1[W,Z]*M2[Z,Z]+M1[W,W]*M2[W,Z];
        Result[W,W]:=M1[W,X]*M2[X,W]+M1[W,Y]*M2[Y,W]+M1[W,Z]*M2[Z,W]+M1[W,W]*M2[W,W];
 end;
end;

procedure NormalizePlane(var plane : TVector4);
var
 n : Single;
begin
 n:=RSqrt(plane[0]*plane[0]+plane[1]*plane[1]+plane[2]*plane[2]);
 ScaleVector4(plane, n);
end;

function RSqrt(v : Single) : Single;
asm
 test vSIMD, 1
 jz @@FPU
 @@3DNow:
        lea eax, [ebp+8]
        db $0F,$6E,$00           /// movd mm0, [eax]
        db $0F,$0F,$C8,$97       /// pfrsqrt  mm1, mm0

        db $0F,$6F,$D1           /// movq     mm2, mm1
        db $0F,$0F,$C9,$B4       /// pfmul    mm1, mm1
        db $0F,$0F,$C8,$A7       /// pfrsqit1 mm1, mm0
        db $0F,$0F,$CA,$B6       /// pfrcpit2 mm1, mm2

        db $0F,$7E,$08           /// movd [eax], mm1
        db $0F,$0E               /// femms
        fld dword ptr [eax]
        jmp @@End
 @@FPU:
        fld v
        fsqrt
        fld1
        fdivr
 @@End:
end;

procedure ScaleVector4(var v : TVector4; factor: Single);
asm
 test vSIMD, 1
 jz @@FPU
 @@3DNow:      // 121824
        db $0F,$6E,$4D,$08       /// movd        mm1, [ebp+8]
        db $0F,$62,$C9           /// punpckldq   mm1, mm1

        db $0F,$6F,$00           /// movq        mm0, [eax]
        db $0F,$6F,$50,$08       /// movq        mm2, [eax+8]
        db $0F,$0F,$C1,$B4       /// pfmul       mm0, mm1
        db $0F,$0F,$D1,$B4       /// pfmul       mm2, mm1
        db $0F,$7F,$00           /// movq        [eax], mm0
        db $0F,$7F,$50,$08       /// movq        [eax+8], mm2

        db $0F,$0E               /// femms
      
        pop   ebp
        ret   $04
 @@FPU:        // 155843
        FLD  DWORD PTR [EBP+8]
     
        FLD  DWORD PTR [EAX]
        FMUL ST, ST(1)
        FSTP DWORD PTR [EAX]
        FLD  DWORD PTR [EAX+4]
        FMUL ST, ST(1)
        FSTP DWORD PTR [EAX+4]
        FLD  DWORD PTR [EAX+8]
        FMUL ST, ST(1)
        FSTP DWORD PTR [EAX+8]
        FLD  DWORD PTR [EAX+12]
        FMULP
        FSTP DWORD PTR [EAX+12]
end;

function PlaneEvaluatePoint(const plane : TVector4; const point : TVector3) : Single;
// EAX contains address of plane
// EDX contains address of point
// result is stored in ST(0)
asm
 FLD DWORD PTR [EAX]
 FMUL DWORD PTR [EDX]
 FLD DWORD PTR [EAX + 4]
 FMUL DWORD PTR [EDX + 4]
 FADDP
 FLD DWORD PTR [EAX + 8]
 FMUL DWORD PTR [EDX + 8]
 FADDP
 FLD DWORD PTR [EAX + 12]
 FADDP
end;

initialization
 try
 //detect 3DNow! capable CPU (adapted from AMD's "3DNow! Porting Guide")
        asm
         pusha
         mov  eax, $80000000
         db $0F,$A2               /// cpuid
         cmp  eax, $80000000
         jbe @@No3DNow
         mov  eax, $80000001
         db $0F,$A2               /// cpuid
         test edx, $80000000
         jz @@No3DNow
         mov vSIMD, 1
         @@No3DNow:
         popa
        end;
 except
        vSIMD:=0;
 end;
end.
