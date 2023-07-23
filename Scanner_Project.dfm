object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 749
  ClientWidth = 753
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
    Width = 728
    Height = 226
    Lines.Strings = (
      '{'
      ''
      '  var a = "scope 1a";'
      '    '
      '  {'
      '     var b = "scope 2";'
      ''
      '   }'
      ''
      '  var c = "scope 1b";'
      ''
      '  print c ;'
      '}')
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 8
    Top = 271
    Width = 353
    Height = 360
    Lines.Strings = (
      'Memo2')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Memo3: TMemo
    Left = 367
    Top = 271
    Width = 369
    Height = 360
    Lines.Strings = (
      'Memo3')
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
