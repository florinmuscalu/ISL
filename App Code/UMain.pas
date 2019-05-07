unit UMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UFormManager, ExtCtrls, xSL, USkyBox, UTypes, UVectors, UParticleManager, URenderer,
     UMaterials, Menus, StdCtrls, XPMan;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Memo1: TMemo;
    Edit1: TMenuItem;
    EditScript1: TMenuItem;
    XPManifest1: TXPManifest;
    ComboBox1: TComboBox;
    Label4: TLabel;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditScript1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    xSLHandle:cardinal;
    procedure OnTick(Sender: TObject; var Done: Boolean);
  public
    procedure ReloadScript;
  end;

var
  Form1: TForm1;
  time:double;
implementation

uses dglOpenGL, UEditScript;
{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
 glwInit(cmdLine, true);
 glwCreateWindow(panel1.Handle);
 glwWidth:=Panel1.width;
 glwHeight:=Panel1.height;

 Renderer:=TRenderer.Create;
 Renderer.StartRenderer;

// Renderer.SkyBox.Load('SkyBox5');

 ReloadScript;
 time:=0;
 GL.glDisable(GL_DITHER);

 Application.OnIdle:=OnTick;
end;

procedure TForm1.OnTick(Sender: TObject; var Done: Boolean);
var
 deltatime:single;
 fps:cardinal;
 call:string;
begin
 Done:=true;
 glwTick(deltatime, fps);

 caption:='3D Engine. FPS: '+inttostr(glwFPS);

 Renderer.BeginRendering(true, DeltaTime);

 time:=time+Deltatime/10;
 Renderer.ScriptEngine.BindScript(xSLHandle);
 call:='OnIdle('+inttostr(trunc(time))+')';
 Renderer.ScriptEngine.Run(pchar(call));
 call:=Renderer.ScriptEngine.LastError;
 Renderer.Render;

 Renderer.EndRendering;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
 application.Terminate;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
 MouseCoor : windows.TPoint;
begin
 UpdateKey(Key, true);
 if key=27 then begin
    switch:=1-switch;
    if switch=1 then begin
        ShowCursor(true);
        Timer1.Enabled:=true;
        comboBox1.Enabled:=true;
    end
    else begin
        Timer1.Enabled:=false;
        comboBox1.Enabled:=false;
        MouseCoor.x := Renderer.Camera.Width div 2;
        MouseCoor.y := Renderer.Camera.Height div 2;
        windows.ClientToScreen(glwWinHandle, MouseCoor);
        SetCursorPos(MouseCoor.X, MouseCoor.Y);
        ShowCursor(false);
        ReloadScript;
    end;
 end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
 done:boolean;
begin
 OnTick(self, done);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 UpdateKey(Key, false);
end;

procedure TForm1.EditScript1Click(Sender: TObject);
var
 key:word;
begin
 key:=27;
 form2.Init;
 if form2.ShowModal=mrOk then FormKeyDown(self, key, []);
end;

procedure TForm1.ReloadScript;
var
 e:string;
begin
 Renderer.ScriptEngine.DeleteScript(xSLHandle);
 xSLHandle:=Renderer.ScriptEngine.NewScript;
 Renderer.ScriptEngine.BindScript(xSLHandle);
 Renderer.ScriptEngine.LoadScript(pchar(glwAppDir+comboBox1.Items[comboBox1.ItemIndex]));
 Renderer.ScriptEngine.CompileScript;
 e:=Renderer.ScriptEngine.LastError;
 Memo1.Lines.Clear;
 Memo1.Lines.Add(e);
 Renderer.ParticleManager.Clear;
 Renderer.ScriptEngine.Run('OnCreate()');
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
 ReloadScript;
end;

end.
