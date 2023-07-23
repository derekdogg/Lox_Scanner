object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 749
  ClientWidth = 1104
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BtnScan: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Scan'
    TabOrder = 0
    OnClick = BtnScanClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 1088
    Height = 307
    Color = clMenuText
    Font.Charset = ANSI_CHARSET
    Font.Color = clMoneyGreen
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'var global = "global";'
      '{'
      '  var a = "scope 1a";'
      '  {'
      '     var b = "scope 2";'
      '     print b ;'
      '   }'
      '   var c = "scope 1b";'
      '   print a ;  '
      '   print c ;'
      '}'
      'print global ;')
    ParentFont = False
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 8
    Top = 352
    Width = 401
    Height = 279
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMoneyGreen
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'Memo2')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Memo3: TMemo
    Left = 415
    Top = 352
    Width = 681
    Height = 279
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMoneyGreen
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'Memo3')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object BtnHash: TButton
    Left = 440
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Hash testing'
    TabOrder = 4
    OnClick = BtnHashClick
  end
  object Button1: TButton
    Left = 286
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 5
    OnClick = Button1Click
  end
  object chkRun: TCheckBox
    Left = 183
    Top = 16
    Width = 97
    Height = 17
    Caption = 'run'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object chkEmit: TCheckBox
    Left = 89
    Top = 16
    Width = 97
    Height = 17
    Caption = 'emit'
    TabOrder = 7
  end
end
