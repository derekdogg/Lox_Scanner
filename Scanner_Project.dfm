object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 638
  ClientWidth = 1101
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
    Width = 137
    Height = 25
    Caption = 'Compile and run'
    TabOrder = 0
    OnClick = BtnScanClick
  end
  object memEdit: TMemo
    Left = 8
    Top = 39
    Width = 1071
    Height = 387
    Color = clMenuText
    Font.Charset = ANSI_CHARSET
    Font.Color = clMoneyGreen
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      ''
      'fun fib(n) {'
      '  if (n < 2) return n;'
      '  return fib(n - 2) + fib(n - 1);'
      '}'
      ''
      ' '
      'print fib(10);')
    ParentFont = False
    TabOrder = 1
    WantTabs = True
  end
  object chkRun: TCheckBox
    Left = 223
    Top = 8
    Width = 42
    Height = 17
    Caption = 'run'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object chkEmit: TCheckBox
    Left = 177
    Top = 8
    Width = 40
    Height = 17
    Caption = 'emit'
    TabOrder = 3
  end
  object btnClear: TButton
    Left = 271
    Top = 8
    Width = 75
    Height = 17
    Caption = 'Clear'
    TabOrder = 4
    OnClick = btnClearClick
  end
  object MemRun: TMemo
    Left = 383
    Top = 432
    Width = 369
    Height = 387
    Color = clMenuText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    TabOrder = 5
  end
  object MemCodes: TMemo
    Left = 758
    Top = 432
    Width = 321
    Height = 387
    Color = clMenuText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    TabOrder = 6
  end
  object MemLocals: TMemo
    Left = 8
    Top = 432
    Width = 369
    Height = 209
    Color = clMenuText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    TabOrder = 7
  end
end
