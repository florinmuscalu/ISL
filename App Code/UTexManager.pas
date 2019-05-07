{Done}
unit UTexManager;
interface
uses dglOpenGL, UTypes, GLImg, UMaterials, UVectors;

type
 TTexManager=class
       private

        FTextureHandle : array [GL_TEXTURE0..GL_TEXTURE1] of glUint;
        fUnits: array [GL_TEXTURE0..GL_TEXTURE1] of boolean;

        function TEX_Create2D(Img:TTextureImage; MipMaps, Compressed: boolean; MaxAnisotropy, Priority: glFLoat; MipMapMaxLevel: glInt): Integer;
       public
        fTextures:array[0..512] of TBasicTexture;
        fTopTex:Integer;
        
        function  LoadTexture(Name:pchar;                    //Numele texturii de forma Cale\nume.exetensie
                              Priority:single=0.5;              //prioritate
                              MipMaps:boolean=false;
                              MipMapmaxLevel:integer=-1;
                              Compressed:boolean=false;
                              maxAnisotropy:single=1):PBasicTexture; //returneaza texture handle-ul

        function  GetTextureCount:cardinal;                     //number of textures
        procedure ClearTextures;                                //remove all textures

        constructor create;
        destructor Destroy; override;

        procedure DeleteTexture(var tex:PBasicTexture);

        procedure Reset;                          //bind texture 0 to all texture units
        procedure ApplyMaterial(mat:PMaterial);
        procedure BindTexture(const textureUnit: cardinal; const handle: glUint);

        //tileset Function
        function AdjustCoordinate(Texture:PBasicTexture; const Coord:TVector2; const texname:shortstring):TVector2;
        procedure TexCoord(Texture:PBasicTexture;  const texUnit:glUint; const Coord:TVector2; const texname:shortstring);
 end;

implementation
uses sysutils, UFormManager, Classes, Urc;

procedure TTexManager.ClearTextures;
var
 i:integer;
begin
 for i:=0 to fTopTex do begin
        if fTextures[i]<>nil then GL.glDeleteTextures(1,@fTextures[i].fTexHandle);
        fTextures[i].Free;
        fTextures[i]:=nil;
 end;
 for i:=GL_TEXTURE0 to GL_TEXTURE1 do begin
        fTextureHandle[i]:=0;
 end;
 fTopTex:=-1;
end;

function TTexManager.GetTextureCount: cardinal;
begin
 result:=fTopTex+1;
end;

constructor TTexManager.Create;
var
 i:cardinal;
begin
 LoadGLImg;
 for i:=GL_TEXTURE0 to GL_TEXTURE1 do begin
        fTextureHandle[i]:=0;
        fUnits[i]:=false;
 end;

 GL.glActiveTexture(GL_TEXTURE0);
 fUnits[GL_TEXTURE0]:=true;
 GL.glEnable(GL_TEXTURE_2D);
 fTopTex:=-1;
 glwAppendLog('Texture Manager created.');
end;

destructor TTexManager.Destroy;
begin
 ClearTextures;
 UnLoadGLImg;
 inherited;
 glwAppendLog('Texture Manager destroyed.');
end;

function TTexManager.TEX_Create2D(Img: TTextureImage;
  MipMaps, Compressed: boolean; MaxAnisotropy, Priority: glFLoat;
  MipMapMaxLevel: glInt): Integer;
var
  Texture : TGLuint;
begin
 result:=0;
 if LibHandle=0 then exit;

 GL.glGenTextures(1, @Texture);

 GL.glActiveTexture(GL_TEXTURE0);
 GL.glBindTexture(GL_TEXTURE_2D, Texture);
 FTextureHandle[GL_TEXTURE0]:=Texture;

 GL.glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
 if (MaxAnisotropy>1)and(MaxAnisotropy<=2) then GL.glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, MaxAnisotropy);

 if mipMaps then begin
        if MipMapMaxLevel>=0 then GL.glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAX_LEVEL,MipMapMaxLevel);
        GL.glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
        GL.glTexParameteri(GL_TEXTURE_2D,GL_GENERATE_MIPMAP,GL_TRUE);
 end
 else GL.glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

 if Compressed then begin
        if mipmaps then begin
                if Img.bpp = 32 then GL.gluBuild2DMipmaps(GL_TEXTURE_2D, GL_COMPRESSED_RGBA, Img.width, Img.Height, GL_RGBA, GL_UNSIGNED_BYTE, Img.imageData)
                                else GL.gluBuild2DMipmaps(GL_TEXTURE_2D, GL_COMPRESSED_RGB, Img.Width, Img.Height, GL_RGB, GL_UNSIGNED_BYTE, Img.imageData);
        end
        else begin
                if Img.bpp = 32 then GL.glTexImage2D(GL_TEXTURE_2D,0, GL_COMPRESSED_RGBA, Img.Width, Img.Height,0, GL_RGBA, GL_UNSIGNED_BYTE, Img.imageData)
                                else GL.glTexImage2D(GL_TEXTURE_2D,0, GL_COMPRESSED_RGB, Img.Width, Img.Height,0, GL_RGB, GL_UNSIGNED_BYTE, Img.imageData);
        end
 end
 else begin
        if mipmaps then begin
                if Img.bpp = 32 then GL.gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA8, Img.Width, Img.Height, GL_RGBA, GL_UNSIGNED_BYTE, Img.imageData)
                                else GL.gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGB8, Img.Width, Img.Height, GL_RGB, GL_UNSIGNED_BYTE, Img.imageData);
        end
        else begin
                if Img.bpp = 32 then GL.glTexImage2D(GL_TEXTURE_2D,0, GL_RGBA8, Img.Width, Img.Height,0, GL_RGBA, GL_UNSIGNED_BYTE, Img.imageData)
                                else GL.glTexImage2D(GL_TEXTURE_2D,0, GL_RGB8, Img.Width, Img.Height,0, GL_RGB, GL_UNSIGNED_BYTE, Img.imageData);
        end
 end;

 GL.glPrioritizeTextures(1,@Texture,@Priority);
 result :=Texture;
end;

procedure TTexManager.DeleteTexture;
var
 i:integer;
 index:integer;
begin
 if tex=nil then exit;
 if tex^=nil then exit;
 if fTextures[tex^.Index]=nil then begin
        tex^:=fTextures[tex^.Index];
        exit;
 end;
 glwAppendLog('Texture "'+tex^.fFileName+'" unloaded.');

 GL.glDeleteTextures(1,@tex^.fTexHandle);
 for i:=GL_TEXTURE0 to GL_TEXTURE1 do if fTextureHandle[i]=tex^.fTexHandle then BindTexture(i, 0);
 index:=tex^.Index;
 if tex^ is TTileSet then (tex^ as TTileSet).fSubTex:=nil;
 tex.Free;
 tex^:=nil;
 tex:=nil;
 fTextures[index]:=nil;
end;

procedure TTexManager.Reset;
var
 i : Integer;
begin
 for i:=GL_TEXTURE0 to GL_TEXTURE1 do begin
        GL.glActiveTexture(i);
        GL.glBindTexture(GL_TEXTURE_2D, 0);
        FTextureHandle[i]:=0;
 end;
end;

procedure TTexManager.BindTexture(const textureUnit: cardinal; const handle: glUint);
begin
 GL.glActiveTexture(textureUnit);
 if handle<>FTextureHandle[textureUnit] then begin
        GL.glBindTexture(GL_TEXTURE_2D, handle);
        FTextureHandle[textureUnit]:=handle;
 end;
end;

procedure TTexManager.ApplyMaterial(mat: PMaterial);
begin
 if mat.TexStg.UseTex0 then begin
        GL.glActiveTexture(GL_TEXTURE0);
        if not fUnits[GL_TEXTURE0] then begin
                fUnits[GL_TEXTURE0]:=true;
                GL.glEnable(GL_TEXTURE_2D);
        end;
        if mat.TexStg.texture0.fTexHandle<>FTextureHandle[GL_TEXTURE0] then begin
                GL.glBindTexture(GL_TEXTURE_2D, mat.TexStg.texture0.fTexHandle);
                FTextureHandle[GL_TEXTURE0]:=mat.TexStg.texture0.fTexHandle;
        end;
 end
 else begin
        GL.glActiveTexture(GL_TEXTURE0);
        if fUnits[GL_TEXTURE0] then begin
                fUnits[GL_TEXTURE0]:=false;
                GL.glDisable(GL_TEXTURE_2D);
        end;
 end;
 if mat.TexStg.UseTex1 then begin
        GL.glActiveTexture(GL_TEXTURE1);
        if not fUnits[GL_TEXTURE1] then begin
                fUnits[GL_TEXTURE1]:=true;
                GL.glEnable(GL_TEXTURE_2D);
        end;
        if mat.TexStg.texture1.fTexHandle<>FTextureHandle[GL_TEXTURE1] then begin
                GL.glBindTexture(GL_TEXTURE_2D, mat.TexStg.texture1.fTexHandle);
                FTextureHandle[GL_TEXTURE1]:=mat.TexStg.texture1.fTexHandle;
        end;
 end
 else begin
        GL.glActiveTexture(GL_TEXTURE1);
        if fUnits[GL_TEXTURE1] then begin
                fUnits[GL_TEXTURE1]:=false;
                GL.glDisable(GL_TEXTURE_2D);
        end;
 end;
end;

function TTexManager.LoadTexture(Name: pchar; Priority: single;
  MipMaps: boolean; MipMapmaxLevel: integer; Compressed: boolean;
  maxAnisotropy: single): PBasicTexture;
var
 s, texName:string;
 size:cardinal;
 i:integer;
 stream:TStream;
 TexHandle:cardinal;
 err:string;
 Img:TTextureImage;
 textureExt:string;
begin
 for i:=0 to fTopTex do
        if (fTextures[i]<>nil)and(sametext(texName,fTextures[i].fFileName)) then begin
                result:=@fTextures[i];
                exit;
        end;
 s:=ChangeFileExt(Name,'.tset');
 texName:=Name;
 textureExt:=uppercase(extractfileext(name));

 stream:=glwGetFileAsStream(pchar(texName), size);
 err:=glwGetLastErrorFs;
 if err<>'' then begin
        glwPostError(100);
        result:=nil;
        exit;
 end;
 if textureExt='.TGA' then Img:=TGA_LoadTextureFromStream(TMemoryStream(stream));
 if textureExt='.BMP' then Img:=BMP_LoadTextureFromStream(TMemoryStream(stream));
 if textureExt='.JPG' then Img:=JPG_LoadTextureFromStream(TMemoryStream(stream));
 if not Img.ok then begin
        glwPostError(101);
        result:=nil;
        exit;
 end;
 TexHandle:=Tex_Create2D(Img, MipMaps, Compressed, MaxAnisotropy, Priority, MipMapMaxLevel);
 ImgRelease(Img);
 stream.Free;

 glwAppendLog('Texture "'+texName+'" loaded.');

 stream:=glwGetFileAsStream(pchar(s), size);
 s:=glwGetLastErrorFs;
 if s<>'' then begin
        inc(fTopTex);
        fTextures[fTopTex]:=TBasicTexture.Create;
        fTextures[fTopTex].fHasMipMaps:=MipMaps;
        fTextures[fTopTex].fCompressed:=Compressed;
        fTextures[fTopTex].fFileName:=Name;
        fTextures[fTopTex].fTexHandle:=TexHandle;
        fTextures[fTopTex].Index:=fTopTex;
        result:=@fTextures[fTopTex];
        exit;
 end;

 inc(fTopTex);
 fTextures[fTopTex]:=TTileSet.Create;
 fTextures[fTopTex].fHasMipMaps:=MipMaps;
 fTextures[fTopTex].fCompressed:=Compressed;
 fTextures[fTopTex].fFileName:=Name;
 fTextures[fTopTex].fTexHandle:=TexHandle;
 fTextures[fTopTex].Index:=fTopTex;
 result:=@fTextures[fTopTex];

 size:=stream.Size div sizeof(TSubTex);
 setlength((fTextures[fTopTex] as TTileSet).fSubTex, size);
 stream.Seek(0, soFromBeginning);
 stream.Read((fTextures[fTopTex] as TTileSet).fWidth, sizeof((fTextures[fTopTex] as TTileSet).fWidth));
 stream.Read((fTextures[fTopTex] as TTileSet).fHeight, sizeof((fTextures[fTopTex] as TTileSet).fHeight));
 for i:=1 to size do begin
        stream.Read((fTextures[fTopTex] as TTileSet).fSubTex[i-1], sizeof(TSubTex));
        (fTextures[fTopTex] as TTileSet).fSubTex[i-1].index:=i-1;
 end;
 stream.Free;
end;

function TTexManager.AdjustCoordinate;
var
 i:integer;
 t:TTileSet;
begin
 if Texture^ is TTileSet then begin
        t:=Texture^ as TTileSet;
                for i:=0 to high(t.fSubTex) do
                        if SameText(texName, t.fSubTex[i].name) then begin
                                result[0]:=abs(frac(Coord[0]));
                                result[1]:=abs(frac(Coord[1]));
                                result[0]:=t.fSubTex[i].OffsetX/t.fWidth +result[0]*(t.fSubTex[i].Width /t.fWidth );
                                result[1]:=1-(t.fSubTex[i].OffsetY/t.fHeight+result[1]*(t.fSubTex[i].Height/t.fHeight));
                                exit;
                        end;
 end
 else result:=Coord;
end;

procedure TTexManager.TexCoord;
var
 V:TVector2;
begin
 V:=Self.AdjustCoordinate(Texture, Coord, texname);
 GL.glMultiTexCoord2fv(texUnit, @V);
end;

end.
