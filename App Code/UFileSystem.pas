unit UFileSystem;
interface
uses Classes, UGLWinTypes;
type
 TFileSystem=class
       private
        fDirName:string;
        flasterror:string;
        function getLastError: string;
       public
        constructor create(dir:string);
        destructor destroy; override;

        procedure DeleteFile(filename:string);
        procedure RenameFile(OldName, Newname:string);

        function  GetFile(filename:string; out size:cardinal):Pointer;  {get a file's Data form a PAK as a Pointer}
        function  GetFileAsStream(filename:string; out size:cardinal):TMemoryStream;  {get a file's Data form a PAK as a Memory Stream}

        procedure SaveFileFromStream(var S:TStream; filename:string);
        property  LastError:string read getLastError;
 end;

implementation
uses windows, sysutils;

constructor TFileSystem.create;
begin
 inherited Create;
 fDirName:=dir;
 fLastError:='';
end;

procedure TFileSystem.DeleteFile(filename: string);
begin
 fLastError:='';
 sysutils.DeleteFile(fDirName+filename);
end;

destructor TFileSystem.destroy;
begin
 fLastError:='';
 inherited;
end;

function TFileSystem.GetFile;
var
 f:file;
 fn:string;
begin
 result:=nil;
 fLastError:='';
 fn:=fDirName+filename;
 if not(FileExists(fn)) then begin
    fLastError:='Unable to read from file: '+filename;
    exit;
 end;
 assignfile(f, fn);
 filemode:=0;
 reset(f,1);
 filemode:=2;
 size:=filesize(f);
 getmem(result,size);
 Seek(f, 0);
 blockread(f, result^, size);
 closefile(f);
end;

function TFileSystem.GetFileAsStream(filename: string; out size: cardinal): TMemoryStream;
var
 p:pointer;
begin
 fLastError:='';
 result:=TMemoryStream.Create;
 p:=GetFile(filename, size);
 if fLastError<>'' then exit;
 result.WriteBuffer(p^, size);
 freemem(p, size);
 result.Seek(0, soBeginning);
end;

function TFileSystem.getLastError: string;
begin
 result:=fLastError;
 fLastError:='';
end;

procedure TFileSystem.RenameFile(OldName, Newname: string);
begin
 fLastError:='';
 sysutils.RenameFile(fDirName+OldName, fDirName+NewName);
end;

procedure TFileSystem.SaveFileFromStream(var S: TStream; filename: string);
begin
{}
end;

end.
