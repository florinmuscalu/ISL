unit UTGAFile;
interface
uses Sysutils, Classes, UTypes;

{------------------------------------------------------------------}
{  Loads 24 and 32bpp (alpha channel) TGA textures                 }
{------------------------------------------------------------------}

function TGA_LoadTextureFromStream(Stream: TMemoryStream): TTextureImage;
exports  TGA_LoadTextureFromStream;
implementation
const
 TGAheader    : array [0..11] of byte = (0,0,2 ,0,0,0,0,0,0,0,0,0);	// Uncompressed TGA Header
 TGAComheader : array [0..11] of byte = (0,0,10,0,0,0,0,0,0,0,0,0);	// Compressed TGA Header


function TGA_LoadTextureFromStream;
var
 TGAcompare : array [0..11] of byte;								// Used To Compare TGA Header
 header : array [0..5] of byte;									// First 6 Useful Bytes From The Header
 bytesPerPixel : integer;					// Holds Number Of Bytes Per Pixel Used In The TGA File
 imageSize : integer;									// Used To Store The Image Size When Setting Aside Ram
 i : integer;										// Temporary Variable
 Compressed:boolean;

 Tm: char;
 PixelCount, CurrentPixel, CurrentByte: integer;
 ColorBuffer: Pchar;
 ChunkHeader: byte;
 Counter, Ret: integer;
 Img:PChar;
begin
 SetError('');
 result.height:=0;
 result.width:=0;
 result.bpp:=0;
 result.imageData:=nil;
 result.ok:=false;
 try
        Ret := Stream.Read(TGAcompare, sizeof(TGAcompare));
        if Ret <> sizeof(TGAcompare) then exit;
 except
        SetError('Invalid stream, or not a BMP file!');
        exit;
 end;
 if (CompareMem(@TGAheader, @TGAcompare, sizeof(TGAheader)) = false) Then begin //File is not uncompressed...
        if (CompareMem(@TGAComheader, @TGAcompare, sizeof(TGAComheader)) = True) Then Compressed := True
                                                                                 else exit;
 end
 else Compressed := False;

 if (stream.Read(header, sizeof(header)) <> sizeof(header)) then begin
        SetError('Invalid TGA file!');
        exit;
 end;

 result.width  := header[1] * 256 + header[0];			// Determine The TGA Width	(highbyte*256+lowbyte)
 result.height := header[3] * 256 + header[2];			// Determine The TGA Height	(highbyte*256+lowbyte)

 if (result.width <= 0)	or (result.height <= 0)	or ((header[4] <> 24) and (header[4] <> 32)) then begin
        SetError('Invalid TGA file!');
        exit;
 end;

 result.bpp	:= header[4];							// Grab The TGA's Bits Per Pixel (24 or 32)
 bytesPerPixel	:= result.bpp div 8;						// Divide By 8 To Get The Bytes Per Pixel
 imageSize	:= result.width * result.height * bytesPerPixel;	// Calculate The Memory Required For The TGA Data

 try
        GetMem(img, imageSize);		// Reserve Memory To Hold The TGA Data
 except
        SetError('Unable to allocate memory!');
        exit;
 end;
 if Not Compressed then begin
        if (img = nil) or (stream.Read(img^, integer(imageSize)) <> imageSize)then begin
                        if (img <> nil) then freemem(img); 	// Was Image Data Loaded
                        SetError('Invalid TGA file!');
                        exit;
        end;
        i := 0;
        while i < imageSize do
                with result do begin
                        Tm := Img[I+2];
                        img[i+2] := img[i];					// Set The 3rd Byte To The Value In 'temp' (1st Byte Value)
                        img[i] := Tm;                          // Set The 1st Byte To The Value Of The 3rd Byte

                        i := i + bytesPerPixel;
                end;
 end
 else begin //COMPRESSED TGA'S
        PixelCount := result.width * result.Height;
        CurrentPixel := 0;
        CurrentByte := 0;
        GetMem(ColorBuffer, BytesPerPixel);
        Repeat
                ChunkHeader := 0;
                if stream.Read(ChunkHeader, sizeof(byte)) = 0 then begin
                        //ERROR reading Chunk!
                        SetError('Invalid TGA file!');
                        exit;
                end;
                if ChunkHeader < 128 then begin
                        ChunkHeader := ChunkHeader + 1;
                        For Counter := 0 to ChunkHeader-1 do begin
                                if stream.Read(ColorBuffer^, BytesPerPixel) <> BytesPerPixel then begin
                                        SetError('Invalid TGA file!');
                                        exit;
                                end;
                                img[CurrentByte] := (ColorBuffer[2]);
                                img[CurrentByte+1] := (ColorBuffer[1]);
                                img[CurrentByte+2] := (ColorBuffer[0]);
                                if BytesPerPixel = 4 then img[CurrentByte+3] := (ColorBuffer[3]);

                                CurrentByte := CurrentByte + bytesPerPixel;
                                inc(CurrentPixel);
                                if CurrentPixel > PixelCount then begin
                                        SetError('Invalid TGA file!');
                                        exit;
                                end;
                        end;
                end
                else begin //Chunkheader > 128
                        ChunkHeader := ChunkHeader - 128;
                        if stream.Read(ColorBuffer^, BytesPerPixel) <> BytesPerPixel then begin
                                SetError('Invalid TGA file!');
                                exit;
                        end;
                        For Counter := 0 to ChunkHeader do begin
                                img[CurrentByte] := ColorBuffer[2];
                                img[CurrentByte+1] := ColorBuffer[1];
                                img[CurrentByte+2] := ColorBuffer[0];
                                if BytesPerPixel = 4 then img[CurrentByte+3] := ColorBuffer[3];
                                CurrentByte := CurrentByte + bytesPerPixel;
                                inc(CurrentPixel);
                        end;
                end;
        Until CurrentPixel >= PixelCount;
 end;
 result.imageData:=@img[0];
 result.ok:=true;
end;

end.
