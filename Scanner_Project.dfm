object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 749
  ClientWidth = 1498
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
  object memEdit: TMemo
    Left = 8
    Top = 39
    Width = 1097
    Height = 363
    Color = clMenuText
    Font.Charset = ANSI_CHARSET
    Font.Color = clMoneyGreen
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'var a = "fred";'
      '{'
      '   var a = "bob";'#9#9
      '    '
      ' '
      '}'
      ''
      ''
      'print a; ')
    ParentFont = False
    TabOrder = 1
    WantTabs = True
  end
  object memRun: TMemo
    Left = 8
    Top = 408
    Width = 1097
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
    TabOrder = 2
  end
  object chkRun: TCheckBox
    Left = 135
    Top = 8
    Width = 42
    Height = 17
    Caption = 'run'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object chkEmit: TCheckBox
    Left = 89
    Top = 8
    Width = 40
    Height = 17
    Caption = 'emit'
    TabOrder = 4
  end
  object btnClear: TButton
    Left = 183
    Top = 8
    Width = 75
    Height = 17
    Caption = 'Clear'
    TabOrder = 5
    OnClick = btnClearClick
  end
end
