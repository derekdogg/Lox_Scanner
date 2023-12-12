object fmScript: TfmScript
  Left = 0
  Top = 0
  Caption = 'Script'
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
  object Splitter1: TSplitter
    Left = 0
    Top = 468
    Width = 1101
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsLine
    ExplicitLeft = -2
    ExplicitTop = 477
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1101
    Height = 41
    Align = alTop
    TabOrder = 0
    object BtnScan: TButton
      Left = 8
      Top = 10
      Width = 137
      Height = 25
      Caption = 'Compile and run'
      TabOrder = 0
      OnClick = BtnScanClick
    end
    object btnClear: TButton
      Left = 151
      Top = 8
      Width = 82
      Height = 25
      Caption = 'Clear'
      TabOrder = 1
      OnClick = btnClearClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 472
    Width = 1101
    Height = 166
    Align = alBottom
    Caption = 'Panel2'
    TabOrder = 1
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 411
      Height = 164
      Align = alLeft
      Caption = 'Panel3'
      TabOrder = 0
      object MemLocals: TMemo
        Left = 1
        Top = 1
        Width = 409
        Height = 162
        Align = alClient
        Color = clMenuText
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          '')
        ParentFont = False
        TabOrder = 0
      end
    end
    object Panel4: TPanel
      Left = 412
      Top = 1
      Width = 368
      Height = 164
      Align = alClient
      Caption = 'Panel3'
      TabOrder = 1
      object MemRun: TMemo
        Left = 1
        Top = 1
        Width = 366
        Height = 162
        Align = alClient
        Color = clMenuText
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          '')
        ParentFont = False
        TabOrder = 0
      end
    end
    object Panel5: TPanel
      Left = 780
      Top = 1
      Width = 320
      Height = 164
      Align = alRight
      Caption = 'Panel3'
      TabOrder = 2
      object MemCodes: TMemo
        Left = 1
        Top = 1
        Width = 318
        Height = 162
        Align = alClient
        Color = clMenuText
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          '')
        ParentFont = False
        TabOrder = 0
      end
    end
  end
  object Panel6: TPanel
    Left = 0
    Top = 41
    Width = 1101
    Height = 427
    Align = alClient
    Caption = 'Panel6'
    TabOrder = 2
    object memEdit: TRichEdit
      Left = 1
      Top = 1
      Width = 1099
      Height = 425
      Align = alClient
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
        'print fib(10);'
        ''
        'fun factorial(n)'
        '{'
        '// Base case'
        'if (n == 0) {'
        'return 1;'
        '}'
        '// Recursive case'
        'return n * factorial(n - 1);'
        '}'
        ''
        ''
        ''
        'print factorial(10);')
      ParentFont = False
      TabOrder = 0
      WantTabs = True
    end
  end
end
