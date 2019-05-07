unit UEditScript;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FireColorSynth, Menus, ExtCtrls, StdCtrls, ISLFUnctions, ISLMain;

type
  TForm2 = class(TForm)
    MainMenu1: TMainMenu;
    Panel1: TPanel;
    File1: TMenuItem;
    Save1: TMenuItem;
    SaveClose1: TMenuItem;
    Close1: TMenuItem;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Label3: TLabel;
    ComboBox3: TComboBox;
    Label4: TLabel;
    ComboBox4: TComboBox;
    procedure Close1Click(Sender: TObject);
    procedure SaveClose1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Save;
    procedure Init;
    { Public declarations }
  end;

var
  Form2: TForm2;
  FCS : TFireColorSynth;
implementation

uses UMain;

{$R *.dfm}

procedure TForm2.Close1Click(Sender: TObject);
begin
 ModalResult:=mrCancel;
end;

procedure TForm2.Save;
var
 i:integer;
 f:Textfile;
begin
 try
    assignfile(f, form1.ComboBox1.Items[form1.ComboBox1.itemIndex]);
    rewrite(f);
    for i:=0 to FCS.Lines.Count-1 do writeln(f,FCS.Lines[i]);
    closefile(f);
 except end;
end;

procedure TForm2.SaveClose1Click(Sender: TObject);
begin
 Save;
 modalResult:=mrOk;
end;

procedure TForm2.Save1Click(Sender: TObject);
begin
 Save;
 form1.ReloadScript;
end;

procedure TForm2.Init;
var
      i,j : Integer;
begin
      if FCS<>nil then FCS.Destroy;
      FCS := TFireColorSynth.Create(Form2);
      FCS.Parent := Form2;
      FCS.Align:=alClient;
      FCS.items.Add;
      FCS.items[0].Name := 'KEYWORDS';
      FCS.items[0].ColorList.ListType := TYPE_WORD;
      FCS.items[0].ColorList.List.Add('if');
      FCS.items[0].ColorList.List.Add('while');
      FCS.items[0].ColorList.List.Add('else');
      FCS.items[0].ColorList.List.Add('func');
      FCS.items[0].ColorList.List.Add('end func');
      FCS.items[0].ColorList.List.Add('define');
      FCS.items[0].ColorList.List.Add('end define');
      FCS.items[0].ColorList.List.Add('end');
      FCS.items[0].ColorList.List.Add('exit');
      combobox1.Items:=FCS.Items[0].ColorList.List; combobox1.ItemIndex:=0;
      FCS.items[0].ColorList.Font.Color := clBlack;
      FCS.items[0].ColorList.Font.Style := [fsBold];

      FCS.items.Add;
      FCS.items[1].Name := 'NUMBER';
      FCS.items[1].ColorList.ListType := TYPE_WORD;
      FCS.items[1].ColorList.LoadWordListFromText('0 1 2 3 4 5 6 7 8 9 .');
      FCS.items[1].ColorList.Font.Color := clRed;

      FCS.items.Add;
      FCS.items[2].Name := 'FUNCTIONS';
      FCS.items[2].ColorList.ListType := TYPE_WORD;
      for i:=0 to high(fFunctions) do FCS.Items[2].ColorList.List.Add(fFunctions[i].Name);
      for i:=0 to high(fScripts) do
          if fScripts[i].Nr=fBindedScript then
              for j:=0 to high(fScripts[i].fFunc) do FCS.Items[2].ColorList.List.Add(fScripts[i].fFunc[j].fName);
      combobox2.Items:=FCS.Items[2].ColorList.List; combobox2.ItemIndex:=0;
      FCS.items[2].ColorList.Font.Color := clBlue;

      FCS.items.Add;
      FCS.items[3].ColorList.ListType := TYPE_WORD;
      FCS.items[3].Name := 'GLOBAL VARIABLES';
      for i:=0 to high(fGlobalVars) do FCS.Items[3].ColorList.List.Add(fGlobalVars[i].fName);
      combobox3.Items:=FCS.Items[3].ColorList.List; combobox3.ItemIndex:=0;
      FCS.items[3].ColorList.Font.Style := [fsBold];
      FCS.items[3].ColorList.Font.Color := clBlue;

      FCS.items.Add;
      FCS.items[4].ColorList.ListType := TYPE_COMMENT;
      FCS.items[4].Name := 'COMMENTS';
      FCS.items[4].ColorList.List.Add('#');
      FCS.items[4].ColorList.Font.Name := 'Courier New';
      FCS.items[4].ColorList.Font.Color := clGray;
      FCS.items[4].ColorList.Font.Style  := [fsItalic];

      FCS.items.Add;
      FCS.items[5].ColorList.ListType := TYPE_WORD;
      FCS.items[5].Name := 'TYPES';
      FCS.items[5].ColorList.List.Add('num');
      FCS.items[5].ColorList.List.Add('str');
      FCS.items[5].ColorList.List.Add('bool');
      FCS.items[5].ColorList.List.Add('nVect');
      FCS.items[5].ColorList.List.Add('sVect');
      FCS.items[5].ColorList.List.Add('bVect');
      FCS.items[5].ColorList.Font.Color := clMaroon;
      combobox4.Items:=FCS.Items[5].ColorList.List; combobox4.ItemIndex:=0;

      FCS.InitialiseList;
      FCS.ExplodedWord:=true;
      FCS.Lines.LoadFromFile(form1.ComboBox1.Items[form1.ComboBox1.itemIndex]);
      FCS.ScrollBars:=ssVertical;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
 FCS:=nil;
end;

end.
