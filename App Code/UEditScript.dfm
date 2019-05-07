object Form2: TForm2
  Left = 193
  Top = 124
  BorderStyle = bsToolWindow
  Caption = 'Edit Script'
  ClientHeight = 424
  ClientWidth = 814
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 814
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 2
      Width = 47
      Height = 13
      Caption = 'Keywords'
    end
    object Label2: TLabel
      Left = 128
      Top = 2
      Width = 46
      Height = 13
      Caption = 'Functions'
    end
    object Label3: TLabel
      Left = 248
      Top = 2
      Width = 75
      Height = 13
      Caption = 'Global Variables'
    end
    object Label4: TLabel
      Left = 368
      Top = 2
      Width = 29
      Height = 13
      Caption = 'Types'
    end
    object ComboBox1: TComboBox
      Left = 8
      Top = 16
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      TabStop = False
    end
    object ComboBox2: TComboBox
      Left = 128
      Top = 16
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
      TabStop = False
    end
    object ComboBox3: TComboBox
      Left = 248
      Top = 16
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 2
      TabStop = False
    end
    object ComboBox4: TComboBox
      Left = 368
      Top = 16
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 3
      TabStop = False
    end
  end
  object MainMenu1: TMainMenu
    Left = 448
    Top = 224
    object File1: TMenuItem
      Caption = 'File'
      object Save1: TMenuItem
        Caption = 'Save'
        OnClick = Save1Click
      end
      object SaveClose1: TMenuItem
        Caption = 'Save && Close'
        OnClick = SaveClose1Click
      end
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = Close1Click
      end
    end
  end
end
