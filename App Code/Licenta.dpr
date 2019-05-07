program Licenta;

uses
  Forms,
  UMain in 'UMain.pas' {Form1},
  ISLFunctions in '..\ISL Code\ISLFunctions.pas',
  ISLMain in '..\ISL Code\ISLMain.pas',
  ISLTypes in '..\ISL Code\ISLTypes.pas',
  ISLUtils in '..\ISL Code\ISLUtils.pas',
  UEditScript in 'UEditScript.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
