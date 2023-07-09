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
      '(5 - (3 - 1)) + -1')
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
  object Button1: TButton
    Left = 344
    Top = 384
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 4
  end
  object BtnHash: TButton
    Left = 272
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Hash testing'
    TabOrder = 5
    OnClick = BtnHashClick
  end
end
