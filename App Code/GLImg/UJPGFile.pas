unit UJPGFile;
interface
uses UTypes, Windows, JPEG, CLasses, Graphics;

{------------------------------------------------------------------}
{  Load JPEG textures                                              }
{------------------------------------------------------------------}

function JPG_LoadTextureFromStream(Stream: TMemoryStream): TTextureImage;
exports  JPG_LoadTextureFromStream;
implementation

function JPG_LoadTextureFromStream(Stream: TMemoryStream): TTextureImage;
var
  Data : Array of LongWord;
  W, Width : Integer;
  H, Height : Integer;
  BMP : TBitmap;
  JPG : TJPEGImage;
  C : LongWord;
  Line : ^LongWord;
begin
 SetError('');
 result.height:=0;
 result.width:=0;
 result.bpp:=0;
 result.imageData:=nil;
 result.ok:=false;
 
 JPG:=TJPEGImage.Create;

 try
        JPG.LoadFromStream(stream);
 except
        SetError('Invalid stream, or not a JPG file!');
        Exit;
 end;

 // Create Bitmap
 BMP:=TBitmap.Create;
 BMP.pixelformat:=pf32bit;
 BMP.width:=JPG.width;
 BMP.height:=JPG.height;
 BMP.canvas.draw(0,0,JPG);        // Copy the JPEG onto the Bitmap

 result.width:=BMP.Width;
 result.height:=BMP.Height;
 result.bpp:=32;

 Width :=BMP.Width;
 Height :=BMP.Height;
 SetLength(Data, Width*Height);

 For H:=0 to Height-1 do Begin
        Line :=BMP.scanline[Height-H-1];   // flip JPEG
        For W:=0 to Width-1 do Begin
                c:=Line^ and $FFFFFF; // Need to do a color swap
                Data[W+(H*Width)] :=(((c and $FF) shl 16)+(c shr 16)+(c and $FF00)) or $FF000000;  // 4 channel.
                inc(Line);
        End;
 End;

 BMP.free;
 JPG.free;
 try
        getmem(result.imageData, length(Data)*sizeof(LongWord));
 except
        SetError('Unable to allocate memory!');
        exit;
 end;
 CopyMemory(result.imageData, addr(Data[0]), length(Data)*sizeof(LongWord));
 finalize(data);
 result.ok:=true;
end;

end.
