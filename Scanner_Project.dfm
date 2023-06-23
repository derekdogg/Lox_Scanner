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
      'Memo1')
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 8
    Top = 271
    Width = 353
    Height = 360
    Lines.Strings = (
      'Memo2')
    TabOrder = 2
  end
  object Button1: TButton
    Left = 544
    Top = 8
    Width = 192
    Height = 25
    Caption = 'Dynamic Array Testing'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 432
    Top = 8
    Width = 106
    Height = 25
    Caption = 'Stack Testing'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 336
    Top = 8
    Width = 90
    Height = 25
    Caption = 'Chunk Testing'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 255
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Number val'
    TabOrder = 6
    OnClick = Button5Click
  end
  object Memo3: TMemo
    Left = 367
    Top = 271
    Width = 369
    Height = 360
    Lines.Strings = (
      'Memo3')
    TabOrder = 7
  end
end
