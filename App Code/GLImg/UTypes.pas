unit UTypes;
interface
type
 TTextureImage = record
        ok:boolean;     							// Structure Name
        imageData : PByte;							// Image Data (Up To 32 Bits)
	bpp : integer;								// Image Color Depth In Bits Per Pixel.
	width : integer;							// Image Width
	height : integer;							// Image Height
 end;

procedure SetError(s:string);
function ImgGetError:pchar;
procedure ImgRelease(var Img:TTextureImage);
exports ImgGetError,ImgRelease;
implementation
var
 err:string='';

procedure SetError(s:string);
begin
 err:=s;
end;

function ImgGetError:pchar;
begin
 result:=pchar(err);
 err:='';
end;

procedure ImgRelease(var Img:TTextureImage);
begin
 FreeMem(Img.imageData);
 Img.imageData:=nil;
 Img.ok:=false;
 Img.bpp:=0;
 Img.width:=0;
 Img.height:=0;
end;

end.
