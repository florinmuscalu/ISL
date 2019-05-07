object Form1: TForm1
  Left = 332
  Top = 30
  Width = 816
  Height = 694
  Caption = '3D Engine'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 303
    Top = 0
    Width = 452
    Height = 13
    Caption = 
      'You can look around with the mouse and the arrow keys. By pressi' +
      'ng ESC you disable  looking '
  end
  object Label2: TLabel
    Left = 303
    Top = 16
    Width = 452
    Height = 13
    Caption = 
      'around mode, and you show the mouse cursor. Now you can modify t' +
      'he script file, save it and'
  end
  object Label3: TLabel
    Left = 303
    Top = 32
    Width = 456
    Height = 13
    Caption = 
      'then press ESC again and the script is reloaded. This way you ca' +
      'n test different script settings '
  end
  object Label5: TLabel
    Left = 0
    Top = 48
    Width = 59
    Height = 13
    Caption = 'Using script:'
  end
  object Label4: TLabel
    Left = 0
    Top = 0
    Width = 89
    Height = 13
    Caption = 'Compilation result:'
  end
  object Label6: TLabel
    Left = 303
    Top = 48
    Width = 210
    Height = 13
    Caption = 'without opening and closing the application.'
  end
  object Panel1: TPanel
    Left = 0
    Top = 88
    Width = 800
    Height = 548
    Align = alBottom
    Caption = 'Panel1'
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 0
    Top = 16
    Width = 300
    Height = 33
    TabStop = False
    Enabled = False
    TabOrder = 1
  end
  object ComboBox1: TComboBox
    Left = 0
    Top = 64
    Width = 145
    Height = 21
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 2
    TabStop = False
    Text = 'Data\blank.isl'
    OnChange = ComboBox1Change
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    Items.Strings = (
      'Data\blank.isl'
      'Data\script1.isl'
      'Data\script2.isl'
      'Data\script3.isl'
      'Data\solar.isl')
  end
  object MainMenu1: TMainMenu
    Left = 448
    Top = 304
    object File1: TMenuItem
      Caption = 'File'
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object EditScript1: TMenuItem
        Caption = 'Edit Script'
        OnClick = EditScript1Click
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 1
    OnTimer = Timer1Timer
    Left = 888
    Top = 296
  end
  object XPManifest1: TXPManifest
    Left = 760
  end
end
