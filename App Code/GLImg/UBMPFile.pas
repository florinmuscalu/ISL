unit UBMPFile;
interface
uses Windows, Classes, UTypes;

{------------------------------------------------------------------}
{  Load BMP textures                                               }
{------------------------------------------------------------------}

function BMP_LoadTextureFromStream(Stream: TMemoryStream): TTextureImage;
exports  BMP_LoadTextureFromStream;
implementation

function BMP_LoadTextureFromStream;
var
 FileHeader: BITMAPFILEHEADER;
 InfoHeader: BITMAPINFOHEADER;
 Palette: array of RGBQUAD;
 BitmapLength: LongWord;
 PaletteLength: LongWord;
 Width, Height : Integer;
 p1, p2: PByte;
 a,b:byte;
 i:integer;
begin
 SetError('');
 result.height:=0;
 result.width:=0;
 result.bpp:=0;
 result.imageData:=nil;
 result.ok:=false;

 try
        // Get header information
        Stream.Read(FileHeader, SizeOf(FileHeader));
        Stream.Read(InfoHeader, SizeOf(InfoHeader));

        // Get palette
        PaletteLength := InfoHeader.biClrUsed;
        SetLength(Palette, PaletteLength);
        Stream.Read(Palette, PaletteLength);
 except
        SetError('Invalid stream, or not a BMP file!');
        exit;
 end;
 
 Width  := InfoHeader.biWidth;
 Height := InfoHeader.biHeight;
 BitmapLength := InfoHeader.biSizeImage;
 if BitmapLength = 0 then BitmapLength := Width * Height * InfoHeader.biBitCount Div 8;
 result.bpp:=InfoHeader.biBitCount;
 result.width:=Width;
 result.height:=Height;
 // Get the actual pixel data
 try
        GetMem(result.imageData, BitmapLength);
 except
        SetError('Unable to allocate memory!');
        exit;
 end;
 try
        Stream.Read(result.imagedata^, BitmapLength);
 except
        SetError('Invalid BMP!');
        exit;
 end;

 // Bitmaps are stored BGR and not RGB, so swap the R and B bytes

 p1:=result.imageData;
 for i:=0 to Width*Height-1 do begin
        p2:=result.imageData;
        a:=byte(result.imageData^);
        inc(result.imageData, 2);
        b:=byte(result.imageData^);
        p2^:=b;
        result.imageData^:=a;
        inc(result.imageData);
 end;
 result.imageData:=p1;
 result.ok:=true;
end;

end.
