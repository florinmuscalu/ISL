{Done}
unit GLImg;
interface
uses classes, windows;
type
 TTextureImage = record
        ok:boolean;     							// Structure Name
        imageData : PByte;							// Image Data (Up To 32 Bits)
	bpp : integer;								// Image Color Depth In Bits Per Pixel.
	width : integer;							// Image Width
	height : integer;							// Image Height
 end;
 TImgGetError=function:pchar;
 TImgRelease=procedure(var Img:TTextureImage);
 TBMP_LoadTextureFromStream=function(Stream: TMemoryStream): TTextureImage;
 TTGA_LoadTextureFromStream=function(Stream: TMemoryStream): TTextureImage;
 TJPG_LoadTextureFromStream=function(Stream: TMemoryStream): TTextureImage;

var
 ImgGetError:TImgGetError=nil;
 ImgRelease:TImgRelease=nil;
 BMP_LoadTextureFromStream:TBMP_LoadTextureFromStream=nil;
 TGA_LoadTextureFromStream:TTGA_LoadTextureFromStream=nil;
 JPG_LoadTextureFromStream:TJPG_LoadTextureFromStream=nil;

procedure LoadGLImg(name:string='GLImg.dll');
procedure UnLoadGLImg;
implementation
var
 libhandle:thandle;

procedure LoadGLImg;
begin
 libhandle:=loadlibrary(pchar(name));
 if libHandle=0 then begin
        MessageBox(0,'GLImg.dll not Found!','Error',MB_ICONERROR or MB_SYSTEMMODAL);
        halt;
 end;

 ImgGetError:=getprocaddress(libhandle, 'ImgGetError');
 BMP_LoadTextureFromStream:=getprocaddress(libhandle,'BMP_LoadTextureFromStream');
 TGA_LoadTextureFromStream:=getprocaddress(libhandle,'TGA_LoadTextureFromStream');
 JPG_LoadTextureFromStream:=getprocaddress(libhandle,'JPG_LoadTextureFromStream');
 ImgRelease:=getprocaddress(libhandle,'ImgRelease');
end;

procedure UnLoadGLImg;
begin
 freelibrary(libhandle);
 ImgGetError:=nil;
 BMP_LoadTextureFromStream:=nil;
 TGA_LoadTextureFromStream:=nil;
 JPG_LoadTextureFromStream:=nil;
 ImgRelease:=nil;
end;


end.
