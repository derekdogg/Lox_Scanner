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
    Width = 641
    Height = 307
    Color = clMenuText
    Font.Charset = ANSI_CHARSET
    Font.Color = clMoneyGreen
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      '{'
      '    var s = "Scope 1";'
      '    print s ;'
      '    var a = "1a";'
      '    {'
      '        var s = "Scope 2";'
      '        print s ;'
      '        {'
      #9'        var s = "Scope 3";'
      #9'        print s ;'
      ''
      '            {'
      #9'            var s = "Scope 4";'
      #9'            print s ;'
      ''
      ''
      '            }'
      '        }'
      '    }'
      ''
      '}')
    ParentFont = False
    TabOrder = 1
    WantTabs = True
  end
  object MemTokens: TMemo
    Left = 8
    Top = 352
    Width = 417
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
  object memRun: TMemo
    Left = 431
    Top = 352
    Width = 442
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
  object MemLogging: TMemo
    Left = 655
    Top = 39
    Width = 827
    Height = 307
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
    TabOrder = 8
  end
  object MemVmLog: TMemo
    Left = 879
    Top = 352
    Width = 603
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
    TabOrder = 9
  end
end
