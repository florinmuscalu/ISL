{***************************************************************************

    Auteur : Jean-Baptiste DEMONTE alias FireJocker ( jbdemonte@gmail.com - www.firejocker.com)
    =*=*=*=*

    Date : 14 Septembre 2005
    =*=*=*

    Description
    =*=*=*=*=*=
    Ce composant est un TRichEdit qui possède une fonction de coloration syntaxique.
    Son principal intéret est que la liste des mots à coloriser est modifiable à chaud.

    Mise en garde
    =*=*=*=*=*=*=
    Ce composant est destiné à des applications nécessitant une "petite" coloration
    syntaxique, son but n'est absolument pas de prétendre à concurrencer des composants
    plus puissants tels que SynEdit.
    Aucun développement suplémentaire de ma part n'ira dans ce sens.
    Ce composant est fournit tel quel, son auteur ne pourra nullement être tenu responsable
    de tout dysfonctionnement du directement ou non à l'utilisation de ce composant.

    Remerciements
    =*=*=*=*=*=*=
    Je tiens à remercier tout personne ayant pris part de prés ou de loin au développement,
    aux tests... de ce composant.
    En particulier : Jihn, Sizious et toute la communauté phidels (www.phidels.com)

***************************************************************************}

unit FireColorSynth;

interface

  uses Windows, Classes, Forms, Graphics, Messages, SysUtils, ComCtrls, IniFiles, Controls, Dialogs, StdCtrls, Math;

  type

        TFireColorSynth = class;
        TColorSyntItem = class;
        TChangeItemEvent = procedure(Sender : TObject; ColorSyntItem : TColorSyntItem) of object;
        TListType = (TYPE_WORD,TYPE_COMMENT);

        TColorList = class(TPersistent)
        private
          FFont : TFont;
          FList : TStringList;
          FListType : TListType;

          FOnChangeFont : TNotifyEvent;
          FOnChangeList : TNotifyEvent;
          FOnChangeType : TNotifyEvent;

          procedure FontChanged(Sender: TObject);
          procedure ListChanged(Sender: TObject);
          procedure SetFont(Value : TFont);
          procedure SetList(S : TStringList);
          procedure SetListType(LT : TListType);
        public
          constructor Create;
          destructor destroy; override;
          procedure LoadWordListFromText(pS_Text : String);
          property OnChangeFont : TNotifyEvent read FOnChangeFont write FOnChangeFont;
          property OnChangeList : TNotifyEvent read FOnChangeList write FOnChangeList;
          property OnChangeType : TNotifyEvent read FOnChangeType write FOnChangeType;
        published
          property Font : TFont read FFont write SetFont;
          property List : TStringList read FList write SetList;
          property ListType : TListType read FListType write SetListType;
        end;



        TColorSyntItem = class(TCollectionItem)
        private
          FName: string;
          FColorList : TColorList;
          FOnChangeFont : TChangeItemEvent;
          FOnChangeList : TChangeItemEvent;
          FOnChangeType : TChangeItemEvent;
          procedure SetName(const Value: string);
          procedure FontChanged(Sender: TObject);
          procedure ListChanged(Sender: TObject);
          procedure TypeChanged(Sender: TObject);

          function GetDisplayName: string; override;
        public
          constructor Create(Collection: TCollection); override;
          destructor Destroy; override;
          property OnChangeFont : TChangeItemEvent read FOnChangeFont write FOnChangeFont;
          property OnChangeList : TChangeItemEvent read FOnChangeList write FOnChangeList;
          property OnChangeType : TChangeItemEvent read FOnChangeType write FOnChangeType;
        published
          property Name: string read FName write SetName;
          property ColorList : TColorList read FColorList write FColorList;
        end;

        TColorSyntCollection = class(TCollection)
        private
          FMyComponent: TFireColorSynth;
          FOnChangeFont : TChangeItemEvent;
          FOnChangeList : TChangeItemEvent;
          FOnChangeType : TChangeItemEvent;
          function GetItem(Index: Integer): TColorSyntItem;
          procedure SetItem(Index: Integer; Value: TColorSyntItem);
          procedure FontChanged(Sender: TObject; ColorSyntItem : TColorSyntItem);
          procedure ListChanged(Sender: TObject; ColorSyntItem : TColorSyntItem);
          procedure TypeChanged(Sender: TObject; ColorSyntItem : TColorSyntItem);

        protected
          function GetOwner: TPersistent; override;

        public
          constructor Create(MyComponent: TFireColorSynth);
          function Add: TColorSyntItem;
          property ColorSynt[Index: Integer]: TColorSyntItem read GetItem write SetItem; default;
          property OnChangeFont : TChangeItemEvent read FOnChangeFont write FOnChangeFont;
          property OnChangeList : TChangeItemEvent read FOnChangeList write FOnChangeList;
          property OnChangeType : TChangeItemEvent read FOnChangeType write FOnChangeType;
        end;


        TFireColorSynth = class(TRichEdit)
        private
          FExplodedWord : Boolean;
          OkToWork : Boolean;
          FColorSyntBlocked : Boolean;
          FormAowner : TForm;
          FormOldOnshow : TNotifyEvent;
          OnDefaultFontChanged : TNotifyEvent;

          FColorListColl: TColorSyntCollection;
          FlistColorSynt : TStringList;
          FOnChangeFont : TChangeItemEvent;
          FOnChangeList : TChangeItemEvent;
          FOnChangeType : TChangeItemEvent;
          FMemoLine : String;

          procedure DefaultFontChanged(Sender: TObject);
          procedure FontChanged(Sender: TObject; ColorSyntItem : TColorSyntItem);
          procedure ListChanged(Sender: TObject; ColorSyntItem : TColorSyntItem);
          procedure TypeChanged(Sender: TObject; ColorSyntItem : TColorSyntItem);
          procedure RefreshLiskKnownWord;
          function GetFontOfWord(Mot : string) : Tfont;
          procedure SetItems(Value: TColorSyntCollection);
          procedure FCSyntShow(sender: TObject);
          procedure Coloration_Syntaxique(TexteEntier : Boolean);
          procedure Coloriser(deb, longueur : Integer; couleur : Tfont);
          function CheckList(InString: string; var deb : Integer; var LongMot : integer): boolean;
          function CheckListForComment(InString: string; var deb : Integer; var Couleur : TFont): boolean;
          function SearchDebMot(Text : String; SelStart : Integer) : Integer;
          function SearchFinMot(Text : String; SelStart : Integer) : Integer;
          Procedure WriteExplodedWord(value : Boolean);

        public
          constructor Create(AOwner:TComponent); override;
          destructor Destroy; override;
          procedure InitialiseList;
          Function WordExist(Value : String) : Boolean;
          Function SaveColorToIniFile(FileName : String) : Boolean;
          Function LoadColorFromIniFile(FileName : String) : Boolean;

          Function GetItemByName(pS_Name : String) : TColorSyntItem;
          Function GetIndexItemNamed(pS_Name : String) : Integer;

        published
          property ExplodedWord : Boolean read FExplodedWord write WriteExplodedWord;
          property Items: TColorSyntCollection read FColorListColl write SetItems;

          property OnChangeFont : TChangeItemEvent read FOnChangeFont write FOnChangeFont;
          property OnChangeList : TChangeItemEvent read FOnChangeList write FOnChangeList;
          property OnChangeType : TChangeItemEvent read FOnChangeType write FOnChangeType;

        protected
            procedure Change; override;
            procedure Loaded; override;
        End;

const
        FILEMARK = 'FireColorSynth File - 1.0';

procedure Register;

implementation


procedure Register;
begin
      RegisterComponents('FJComp', [TFireColorSynth]);
end;


{ TColorList}

constructor TColorList.Create;
begin
  inherited Create;

  FFont := TFont.Create;
  FFont.OnChange := FontChanged;

  FList := TStringList.create;
  FList.OnChange := ListChanged;
end;

destructor TColorList.Destroy;
begin
  FList.clear;
  FList.free;
  FreeAndNil(FFont);
  inherited;
end;

procedure TColorList.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TColorList.FontChanged(Sender: TObject);
begin
  if Assigned(FOnChangeFont) then FOnChangeFont(Self);
end;

procedure TColorList.SetList(S : TStringList);
begin
      FList.Assign(S);
end;

procedure TColorList.SetListType(LT : TListType);
begin
      FListType := LT;
      if Assigned(FOnChangeType) then FOnChangeType(Self);
end;


procedure TColorList.ListChanged(Sender: TObject);
begin
  if Assigned(FOnChangeList) then OnChangeList(Self);
end;


procedure TColorList.LoadWordListFromText(pS_Text : String);
var
      MemoOnChangeList : TNotifyEvent;
      P : Integer;
      m : string;
begin
      if pS_Text <> '' then
      begin
          MemoOnChangeList := FOnChangeList;
          FOnChangeList := nil;

          pS_Text := StringReplace(pS_Text, #9, ' ', [rfReplaceAll]);
          pS_Text := StringReplace(pS_Text, #10, ' ', [rfReplaceAll]);
          pS_Text := StringReplace(pS_Text, #13, ' ', [rfReplaceAll]);

          While pS_Text <> '' do
          begin
              P := Pos(' ', pS_Text);
              If P = 0 then P := length(pS_Text)+1;
              m := copy(pS_Text, 1, P-1);
              pS_Text := copy(pS_Text, P+1, length(pS_Text));
              if m <> '' then
              begin
                    P := List.IndexOf(m);
                    if P = -1 then
                    begin
                        List.Add(m);
                    end;
              end;
          end;

          FOnChangeList := MemoOnChangeList;
          if Assigned(FOnChangeList) then OnChangeList(Self);
      end;
end;


{ TMyCollectionItem }


constructor TColorSyntItem.Create(Collection: TCollection);
begin
      SetCollection(Collection);
      FColorList := TColorList.Create;
      FColorList.OnChangeFont := FontChanged;
      FColorList.OnChangeList := ListChanged;
      FColorList.OnChangeType := TypeChanged;
end;


destructor TColorSyntItem.Destroy;
var
      MemoOnChangeList : TNotifyEvent;
begin
      MemoOnChangeList := FColorList.FOnChangeList;
      FColorList.FOnChangeList := nil;
      FreeAndNil(FColorList);
      inherited Destroy;
      if  Assigned(MemoOnChangeList) then MemoOnChangeList(self);
end;

function TColorSyntItem.GetDisplayName: string;
begin
  Result := FName;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TColorSyntItem.SetName(const Value: string);
begin
  if FName <> Value then
    FName := Value;
end;

procedure TColorSyntItem.FontChanged(Sender: TObject);
begin
  if Assigned(Collection) then
  begin
    if Collection is TColorSyntCollection then
      TColorSyntCollection(Collection).FontChanged(Self,Self);
  end;
  if Assigned(FOnChangeFont) then OnChangeFont(Self,Self);
end;

procedure TColorSyntItem.ListChanged(Sender: TObject);
begin
  if Assigned(Collection) then
  begin
    if Collection is TColorSyntCollection then
      TColorSyntCollection(Collection).ListChanged(Self,Self);
  end;
  if Assigned(FOnChangeList) then FOnChangeList(Self,Self);
end;

procedure TColorSyntItem.TypeChanged(Sender: TObject);
begin
  if Assigned(Collection) then
  begin
    if Collection is TColorSyntCollection then
      TColorSyntCollection(Collection).TypeChanged(Self,Self);
  end;
  if Assigned(FOnChangeType) then FOnChangeType(Self,Self);
end;

{ TColorSyntCollection }

constructor TColorSyntCollection.Create(MyComponent: TFireColorSynth);
begin
  inherited Create(TColorSyntItem);
  FMyComponent := MyComponent;
end;

function TColorSyntCollection.Add: TColorSyntItem;
begin
  Result := TColorSyntItem(inherited Add);
  Result.OnChangeFont := OnChangeFont;
  Result.OnChangeList := OnChangeList;
end;

function TColorSyntCollection.GetItem(Index: Integer): TColorSyntItem;
begin
  Result := TColorSyntItem(inherited GetItem(Index));
end;

procedure TColorSyntCollection.SetItem(Index: Integer; Value: TColorSyntItem);
begin
  inherited SetItem(Index, Value);
end;

function TColorSyntCollection.GetOwner: TPersistent;
begin
  Result := FMyComponent;
end;

procedure TColorSyntCollection.FontChanged(Sender: TObject;
  ColorSyntItem : TColorSyntItem);
begin
  if Assigned(FOnChangeFont) then OnChangeFont(Self,ColorSyntItem);
end;

procedure TColorSyntCollection.ListChanged(Sender: TObject;
  ColorSyntItem : TColorSyntItem);
begin
  if Assigned(FOnChangeList) then OnChangeList(Self,ColorSyntItem);
end;

procedure TColorSyntCollection.typeChanged(Sender: TObject;
  ColorSyntItem : TColorSyntItem);
begin
  if Assigned(FOnChangeType) then OnChangeType(Self,ColorSyntItem);
end;


{ TFireColorSynth }

procedure TFireColorSynth.SetItems(Value: TColorSyntCollection);
begin
  FColorListColl.Assign(Value);
end;

constructor TFireColorSynth.Create(AOwner: TComponent);
begin
      FMemoLine := '';

      FlistColorSynt := TStringList.Create;

      inherited Create(AOwner);
      FormAowner := TForm(AOwner);

      FColorSyntBlocked := true;

      // HideSelection permet de ne pas afficher la selection lors du
      // traitement de la coloration syntaxique
      // le focus est donnée à la fiche parent puis recupéré
      HideSelection := true;

      // permet d'attendre que la fiche qoit apparu avant de lancer la color synth
      FormOldOnshow := FormAowner.OnShow;
      FormAowner.OnShow := FCSyntShow;


      FColorListColl := TColorSyntCollection.Create(Self);
      FColorListColl.OnChangeFont := FontChanged;
      FColorListColl.OnChangeList := ListChanged;
      FColorListColl.OnChangeType := TypeChanged;

      OnDefaultFontChanged := Font.OnChange;
      Font.OnChange := DefaultFontChanged;

      // mode conception : initialisation
      if (csDesigning in ComponentState) then
      begin
            FExplodedWord := False;
      end;

      FColorSyntBlocked := False;
end;
procedure TFireColorSynth.DefaultFontChanged(Sender: TObject);
begin
    if Assigned(OnDefaultFontChanged) then OnDefaultFontChanged(self);
    If OkToWork Then Coloration_Syntaxique(True);
end;

Function TFireColorSynth.GetItemByName(pS_Name : String) : TColorSyntItem;
var
    i : Integer;
begin
    i := 0;
    Result := nil;

    While (i < FColorListColl.Count) And (Result = Nil) Do
    begin
          if UpperCase(FColorListColl[i].Name) = UpperCase(pS_Name) then Result := FColorListColl[i];
          Inc(i);
    end;
end;

Function TFireColorSynth.GetIndexItemNamed(pS_Name : String) : Integer;
var
    i : Integer;
begin
    i := 0;
    Result := -1;

    While (i < FColorListColl.Count) And (Result = -1) Do
    begin
          if UpperCase(FColorListColl[i].Name) = UpperCase(pS_Name) then Result := i;
          Inc(i);
    end;
end;

procedure TFireColorSynth.ListChanged(Sender: TObject; ColorSyntItem : TColorSyntItem);
begin
  If OkToWork Then
  begin
        RefreshLiskKnownWord;
        Coloration_Syntaxique(True);
  end;
  if Assigned(FOnChangeList) then FOnChangeList(Self,ColorSyntItem);
end;

procedure TFireColorSynth.TypeChanged(Sender: TObject; ColorSyntItem : TColorSyntItem);
begin
  If OkToWork Then
  begin
        RefreshLiskKnownWord;
        Coloration_Syntaxique(True);
  end;
  if Assigned(FOnChangeType) then FOnChangeType(Self,ColorSyntItem);
end;


procedure TFireColorSynth.FontChanged(Sender: TObject; ColorSyntItem : TColorSyntItem);
begin
  If OkToWork Then Coloration_Syntaxique(True);
  if Assigned(FOnChangeFont) then OnChangeFont(Self,ColorSyntItem);
end;

// Cette fonction retourne la couleur du mot present dans une des listes
function TFireColorSynth.GetFontOfWord(Mot : string) : Tfont;
var
    i,c, i2, c2 : Integer;
    Found : Boolean;
begin
      Result := Nil;
      Found := false;
      Mot := UpperCase(Mot);

      i := 0;
      c := FColorListColl.Count;

      repeat
            if FColorListColl.GetItem(i).ColorList.ListType = TYPE_WORD then
            begin
                i2 := 0;
                c2 := FColorListColl.GetItem(i).FColorList.List.Count;
                repeat
                      if Mot = UpperCase(FColorListColl.GetItem(i).FColorList.List.Strings[i2]) then found := true;
                      Inc(i2);
                until i2 = c2;
            end;
            if not Found then Inc(i);
      until Found or (i = c);
      if Found then result := FColorListColl.GetItem(i).FColorList.Font;
end;

procedure TFireColorSynth.InitialiseList;
begin
      OkToWork := True;
      RefreshLiskKnownWord;
      Coloration_Syntaxique(True);
end;

procedure TFireColorSynth.RefreshLiskKnownWord;
var
    i, i2, c, c2 : Integer;
    lstsorted : TStringList;
    j, lg, lgmax, jmax : Integer;
begin
      OkToWork := False;
      jmax := 0;

      FlistColorSynt.Clear;

      c := FColorListColl.Count;
      for i := 0 to c-1 do
      begin
            if FColorListColl.GetItem(i).ColorList.ListType = TYPE_WORD then
            begin
                c2 := FColorListColl.GetItem(i).FColorList.List.Count;
                for i2 := 0 to c2-1 do FlistColorSynt.Add(UpperCase(FColorListColl.GetItem(i).FColorList.List.Strings[i2]));
            end;
      end;

      lstsorted := TStringList.Create;
      lstsorted.AddStrings(FlistColorSynt);
      lstsorted.Sorted := True;

      FlistColorSynt.Clear;
      lg := lstsorted.Count-1;

      // boucle d'insertion par ordre decroissant des mots
      for i := 0 to lg do
      begin
            lgmax := -1;
            for j := 0 to lstsorted.Count -1 do
            begin
                  if (length(lstsorted[j])>lgmax) Then
                  begin
                        jmax := j;
                        lgmax := length(lstsorted[j]);
                  end;
            end;
            FlistColorSynt.Add(lstsorted[jmax]);
            lstsorted.Delete(jmax);
      end;

      OkToWork := True;
end;


procedure TFireColorSynth.FCSyntShow(sender: TObject);
begin
      If assigned(FormOldOnshow) then FormOldOnshow(sender);
      FormAowner.OnShow := FormOldOnshow;
      OkToWork := True;
      if FlistColorSynt.Count = 0 then RefreshLiskKnownWord;
      Coloration_Syntaxique(True);
end;


destructor TFireColorSynth.Destroy;
begin
      OkToWork := False;
      FColorListColl.Free;
      FlistColorSynt.Clear;
      FlistColorSynt.Free;
      inherited Destroy;
end;

procedure TFireColorSynth.Loaded;
begin
    Inherited Loaded;
    FColorSyntBlocked := False;
    FColorSyntBlocked := False;
end;

procedure TFireColorSynth.Change;
var
    BTotal : Boolean;
    ln1, ln2, i : integer;
begin
      inherited Change;
      if (not (csDesigning in ComponentState)) then
      begin
            If OkToWork Then
            begin
                ln1 := length(FMemoLine);
                ln2 := length(lines.Text);
                i :=  ln1 - ln2 ;
                BTotal := abs(i) > 1;
                if not BTotal then
                begin
                    if i > 0 then
                      BTotal := copy(FMemoLine, 1, ln1 - 3) <> copy(Lines.Text, 1, ln2 - 2)
                    else
                      BTotal := copy(FMemoLine, 1, ln1 - 2) <> copy(Lines.Text, 1, ln2 - 3)
                end;
                FMemoLine := Lines.Text;
                Coloration_Syntaxique(BTotal);
            end;
      end;

end;

Function TFireColorSynth.WordExist(Value : String) : Boolean;
begin
    Result :=FlistColorSynt.IndexOf(Value) <> -1;
end;

Procedure TFireColorSynth.WriteExplodedWord(value : Boolean);
begin
      FExplodedWord := value;
      if ((not (csDesigning in ComponentState)) And Visible And OkToWork) then Coloration_Syntaxique(True);
end;



Function TFireColorSynth.SaveColorToIniFile(FileName : String) : Boolean;
var
  Ini : TIniFile;
  i, i2, c, c2 : Integer;
  PCL : TColorList;
  Container : String;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteString('FILE', 'IDENT', FILEMARK);
    c := FColorListColl.Count;
    Ini.WriteInteger('FILE', 'N', c);

    // Sauvegarde de la police defaut
    Ini.WriteString('DEFAULT_FONT','NAME',Font.Name);
    Ini.WriteString('DEFAULT_FONT','COLOR',ColorToString(Font.Color));
    Ini.WriteInteger('DEFAULT_FONT','SIZE',Font.Size);
    Ini.WriteInteger('DEFAULT_FONT','CHARSET', Font.Charset);
    Ini.WriteInteger('DEFAULT_FONT','HEIGHT',Font.Height);
    Ini.WriteInteger('DEFAULT_FONT','PITCH',Integer(Font.Pitch));
    Ini.WriteBool('DEFAULT_FONT','BOLD', fsBold in Font.Style);
    Ini.WriteBool('DEFAULT_FONT','ITALIC',fsItalic in Font.Style);
    Ini.WriteBool('DEFAULT_FONT','UNDERLINE',fsUnderline in Font.Style);
    Ini.WriteBool('DEFAULT_FONT','STRIKEOUT',fsStrikeOut in Font.Style);


    // Boucle sur chaque liste de colorisation
    for i := 1 to c do
    begin
          Container := 'LIST' + InttoStr(i);

          // Ecriture du nom de la liste
          Ini.WriteString(Container,'NAME',FColorListColl.GetItem(i-1).Name);

          case FColorListColl.GetItem(i-1).ColorList.ListType of
            TYPE_WORD            : Ini.WriteString(Container,'TYPE','WORD');
            TYPE_COMMENT         : Ini.WriteString(Container,'TYPE','COMMENT');
          end;

          PCL := FColorListColl.GetItem(i-1).ColorList;

          // Ecriture de la police
          Ini.WriteString(Container,'FONT_NAME',PCL.Font.Name);
          Ini.WriteString(Container,'FONT_COLOR',ColorToString(PCL.Font.Color));
          Ini.WriteInteger(Container,'FONT_SIZE',PCL.Font.Size);
          Ini.WriteInteger(Container,'FONT_CHARSET', PCL.Font.Charset);
          Ini.WriteInteger(Container,'FONT_HEIGHT',PCL.Font.Height);
          Ini.WriteInteger(Container,'FONT_PITCH',Integer(PCL.Font.Pitch));
          Ini.WriteBool(Container,'FONT_BOLD', fsBold in PCL.Font.Style);
          Ini.WriteBool(Container,'FONT_ITALIC',fsItalic in PCL.Font.Style);
          Ini.WriteBool(Container,'FONT_UNDERLINE',fsUnderline in PCL.Font.Style);
          Ini.WriteBool(Container,'FONT_STRIKEOUT',fsStrikeOut in PCL.Font.Style);

          // Ecriture de la liste de mot
          c2 := PCL.List.Count;
          Ini.WriteInteger(Container, 'N', c2);
          for i2 := 1 to c2 do
            Ini.WriteString(Container, 'W'+InttoStr(i2),PCL.List.Strings[i2-1]);
      end;

      Result := true;

  finally
    Ini.Free;
  end;
end;


Function TFireColorSynth.LoadColorFromIniFile(FileName : String) : Boolean;
var
  Ini : TIniFile;
  Error : Boolean;
  c, c2, i , i2 : Integer;
  PCL : TColorList;
  s : string;
  Container : String;
begin
  Result := False;
  OkToWork := False;
  Items.Clear;

  if not FileExists(FileName) then Exit;

  Ini := TIniFile.Create(FileName);

  Error := Ini.ReadString('FILE', 'IDENT', '') <> FILEMARK;

  If not Error Then
  begin
      c := Ini.ReadInteger('FILE','N',-1);

      // Lecture de la police
      Font.Name := Ini.ReadString('DEFAULT_FONT', 'NAME','Tahoma');
      Font.Color := StringToColor(Ini.ReadString('DEFAULT_FONT', 'COLOR','clWindowText'));
      Font.Size := Ini.ReadInteger('DEFAULT_FONT', 'SIZE',8);
      Font.Charset := Ini.ReadInteger('DEFAULT_FONT', 'CHARSET', ANSI_CHARSET);
      Font.Height := Ini.ReadInteger('DEFAULT_FONT', 'HEIGHT',-11);
      Font.Pitch := TFontPitch(Ini.ReadInteger('DEFAULT_FONT', 'PITCH',Integer(fpDefault)));
      Font.Style := [];
      If Ini.ReadBool('DEFAULT_FONT', 'BOLD', false) then Font.Style := Font.Style + [fsBold];
      If Ini.ReadBool('DEFAULT_FONT', 'ITALIC',false)then Font.Style := Font.Style + [fsItalic];
      If Ini.ReadBool('DEFAULT_FONT', 'UNDERLINE',false)then Font.Style := Font.Style + [fsUnderline];
      If Ini.ReadBool('DEFAULT_FONT', 'STRIKEOUT',false)then Font.Style := Font.Style + [fsStrikeOut];

      if c > 0 then
      begin
          for i := 1 to c do
          begin
              Items.Add;

              Container := 'LIST' + InttoStr(i);

              // Lecture du nom de la liste
              FColorListColl.GetItem(i-1).Name := Ini.ReadString(Container,'NAME','');

              s := UpperCase(Ini.ReadString(Container,'TYPE','WORD'));
              if S = 'WORD'           then FColorListColl.GetItem(i-1).ColorList.ListType := TYPE_WORD;
              if S = 'COMMENT'        then FColorListColl.GetItem(i-1).ColorList.ListType := TYPE_COMMENT;

              PCL := FColorListColl.GetItem(i-1).ColorList;

              // Lecture de la police
              PCL.Font.Name := Ini.ReadString(Container, 'FONT_NAME','Tahoma');
              PCL.Font.Color := StringToColor(Ini.ReadString(Container, 'FONT_COLOR','clWindowText'));
              PCL.Font.Size := Ini.ReadInteger(Container, 'FONT_SIZE',8);
              PCL.Font.Charset := Ini.ReadInteger(Container, 'FONT_CHARSET', ANSI_CHARSET);
              PCL.Font.Height := Ini.ReadInteger(Container, 'FONT_HEIGHT',-11);
              PCL.Font.Pitch := TFontPitch(Ini.ReadInteger(Container, 'FONT_PITCH',Integer(fpDefault)));
              PCL.Font.Style := [];
              If Ini.ReadBool(Container, 'FONT_BOLD', false) then PCL.Font.Style := PCL.Font.Style + [fsBold];
              If Ini.ReadBool(Container, 'FONT_ITALIC',false)then PCL.Font.Style := PCL.Font.Style + [fsItalic];
              If Ini.ReadBool(Container, 'FONT_UNDERLINE',false)then PCL.Font.Style := PCL.Font.Style + [fsUnderline];
              If Ini.ReadBool(Container, 'FONT_STRIKEOUT',false)then PCL.Font.Style := PCL.Font.Style + [fsStrikeOut];

              // Lecture de la liste de mot
              c2 := Ini.ReadInteger(Container, 'N', -1);
              Error := c2 < 0;
              i2 := 1;
              While Not Error And (i2 <= c2) do
              begin
                s := Ini.ReadString(Container, 'W'+InttoStr(i2),'');
                PCL.List.Add(s);
                Error := s = '';
                Inc(i2);
              end;

          end;
      end;
  end;

  Result := Not Error;
  OkToWork := True;
  Ini.Free;

  If Error Then
    Items.Clear
  else
    ListChanged(Self,FColorListColl.GetItem(0));
end;


procedure TFireColorSynth.Coloration_Syntaxique(TexteEntier : Boolean);
var
    deb, fin, iStartColor : integer;
    debtxt, debcomment, longueur : Integer;
    PosDeb : Integer;
    TexteATraiter, tmp : string;
    CommentNeedColor, TxtNeedColor : Boolean;
    Coul : TFont;
begin

    OkToWork := False;
    iStartColor := 0;

    if Not FColorSyntBlocked Then
    Begin
          // Bloc les prochain changement
          FColorSyntBlocked := true;

          if Text <> '' then
          begin
                // Desactivation de l'inversion video de la selection
                Perform(Messages.WM_USER + 63, WParam(True), LParam(False));

                // Mémorisation de la position de depart pour le replacement
                PosDeb := Selstart;

                // Rajout d'un saut de ligne a la fin pour se reperer
                TexteATraiter := Text + #$D#$A;

                Coul := TFont.Create;

                if TexteEntier Then
                begin
                    deb := 1;
                end
                else
                Begin
                    // Recherche du debut de l'analyse
                    deb := SearchDebMot(TexteATraiter, SelStart);

                    // Recherche de la fin de l'analyse
                    fin := SearchFinMot(TexteATraiter, SelStart);

                    TexteATraiter := copy(TexteATraiter,deb,fin+1-deb);

                    while (TexteATraiter <> '') And (TexteATraiter[1]=#$D) Do
                    begin
                          TexteATraiter := copy(TexteATraiter,3,length(TexteATraiter));
                          deb := deb + 2;
                    end;
                End;

                While (TexteATraiter <> '') do
                begin
                      CommentNeedColor := CheckListForComment(TexteATraiter, debcomment, Coul);
                      if CommentNeedColor then
                        tmp := copy(TexteATraiter, 1, debcomment-1)
                      else
                        tmp := TexteATraiter;
                      TxtNeedColor := CheckList(tmp,debtxt, longueur);

                      // Texte & Commentaire
                      if TxtNeedColor And CommentNeedColor then
                      begin
                            // Un commentaire en plein milieu d'un mot à coloriser prime sur le mot
                            if not ((debtxt + longueur - 1) < debcomment) then
                            begin
                                  TxtNeedColor := False;
                            end
                            else
                            begin
                                  TxtNeedColor := debtxt < debcomment;
                                  CommentNeedColor := not TxtNeedColor;
                            end;
                      end;

                      if CommentNeedColor Or TxtNeedColor then
                      begin

                          if CommentNeedColor then
                          begin
                                iStartColor := debcomment;
                                longueur := pos(#$D, TexteATraiter) - iStartColor;
                          end;

                          if TxtNeedColor then
                          begin
                                iStartColor := debtxt;
                                Coul.Assign(GetFontOfWord(copy(TexteATraiter,debtxt, longueur))); // mot au lieu de copy... ????
                          end;

                          // Colorisation du debut en defaut
                          if (iStartColor > 1) Then Coloriser(deb, iStartColor,Font);

                          // colorisation du bloc reconnu
                          Coloriser(deb + iStartColor - 1, longueur,Coul);

                          // suppression du bloc reconnu du reste a analyser
                          TexteATraiter := copy(TexteATraiter, iStartColor + longueur, length(TexteATraiter));
                          while (TexteATraiter <> '') And (TexteATraiter[1]=#$D) Do
                          begin
                                TexteATraiter := copy(TexteATraiter,3,length(TexteATraiter));
                                deb := deb + 2;
                          end;

                          // decallage du drapeau de debut pour le prochaine passage
                          deb := deb + iStartColor + longueur - 1;
                      end
                      else
                      begin
                          Coloriser(deb, longueur,Font);
                          TexteATraiter := '';
                      end;
                end;

                Coul.Destroy;

                // Repositionnement initial du curseur
                Selstart := PosDeb;

                SelAttributes.Assign(Font);

                // Re-activation de l'inversion video de la selection
                Perform(Messages.WM_USER + 63, WParam(False), LParam(False));

          End
          Else // texte vide
          Begin
                SelAttributes.Assign(Font);
          End;

          // prochain changement autorisés
          FColorSyntBlocked := false;
    End;

    OkToWork := True;

End;

procedure TFireColorSynth.Coloriser(deb, longueur : Integer; couleur : Tfont);
begin
      if deb > 0 Then SelStart := deb -1;
      SelLength := longueur;
      SelAttributes.Assign(couleur);
end;



// Cette fonction verifie si le mot est a coloriser
function TFireColorSynth.CheckList(InString: string; var deb : Integer; var LongMot : integer): boolean;
var
    i, memo_i, p, nbr_mot_color : integer;
    pos_deb, pos_fin, fin : integer;
    mot, mot_a_tester : string;
begin
    Result := False;

    InString := UpperCase(InString);

    nbr_mot_color := FlistColorSynt.Count;


    if FExplodedWord Then
    Begin
          // positionnement du drapeau sur le premier mot a gauche a l'extremité droite
          deb := length(InString)+1;

          i := 0;

          memo_i := -1; // drapeau sur le mot selectionné pour la colorisation

          While i < nbr_mot_color Do
          Begin
                // recup d'un mot a verifier
                mot := FlistColorSynt[i];

                // recup de sa place dans la chaine
                p := Pos(mot, InString);

                // verif si le mot est plus a gauche que le precedent selectionné
                if ( (p > 0) And (deb > p) ) Then
                begin
                      deb := p;
                      memo_i := i;

                      // Si le mot commence la chaine, inutile de chercher + loin
                      if p = 1 Then i := nbr_mot_color;
                end;

                Inc(i)
          End;

          if memo_i <> -1 Then
          begin
                LongMot := Length(FlistColorSynt[memo_i]);
                result := true;
          end;
    End
    Else
    Begin
          //  --  Analyse de chaque mot  --  //

          pos_deb := 1;

          fin := length(InString)+1;

          While not Result And (pos_deb < fin) Do
          begin
                // recherche du debut du mot (1er caractere non espace ou fin de chaine
                while (pos_deb < fin) And (InString[pos_deb] in [' ', #$D, #$A]) Do Inc(pos_deb);

                // si la fin n'a pas ete atteinte, on peut continuer
                if (pos_deb < fin) then
                begin

                      // recherche de la fin du mot
                      pos_fin := pos_deb + 1;
                      while (pos_fin < fin) And not (InString[pos_fin] in [' ', #$D, #$A]) Do Inc(pos_fin);

                      mot_a_tester := copy(InString,pos_deb,pos_fin - pos_deb);

                      // analyse du mot
                      i := 0;
                      While not Result And (i < nbr_mot_color) Do
                      Begin
                            mot := FlistColorSynt[i];
                            if mot = mot_a_tester Then
                            Begin
                                  deb := pos_deb;
                                  LongMot := length(mot);
                                  Result := True;
                            End;
                            Inc(i);
                      End;

                      pos_deb := pos_fin + 1;
                end;
          end;
    End;

    if not Result then LongMot := length(InString);

end;



// Cette fonction verifie si le mot est a coloriser
function TFireColorSynth.CheckListForComment(InString: string; var deb : Integer; var Couleur : TFont): boolean;
var
    i, j, p : integer;
    mot : string;
begin
    Result := False;

    InString := UpperCase(InString);

    deb := length(InString) + 1;

    for i := 0 to Items.Count - 1 Do
    Begin
          if Items[i].ColorList.ListType = TYPE_COMMENT then
          begin
                for j := 0 to Items[i].ColorList.List.Count - 1 do
                begin
                      // recup d'un mot a verifier
                      mot := UpperCase(Items[i].ColorList.List.Strings[j]);

                      // recup de sa place dans la chaine
                      p := Pos(mot, InString);

                      // verif si le mot est plus a gauche que le precedent selectionné
                      if ( (p > 0) And (deb > p) ) Then
                      begin
                            deb := p;
                            Result := True;
                            Couleur.Assign(Items[i].ColorList.Font);
                      end;
                end;
          end;
    end;

end;

// fonction qui retourne l'indice du debut du mot pu se trouve le curseur
function TFireColorSynth.SearchDebMot(Text : String; SelStart : Integer) : Integer;
var
      iline : Integer;
begin
      Result := SelStart;
      iline := 0;
      While (Result>1) And (iline <> 2) do
      begin
            if Text[Result-1] = #$A then Inc(iline);
            if iline <> 2 then Dec(Result);
      end;
end;

// function qui retourne l'indice de la fin du mot ou se trouve le curseur
function TFireColorSynth.SearchFinMot(Text : String; SelStart : Integer) : Integer;
begin
      Result := SelStart;
      While not (Text[Result+1] = #$D) Do Inc(Result);
      Result := Result + 2; // On rajoute dans le texte le $D$A de fin de ligne
end;

end.

