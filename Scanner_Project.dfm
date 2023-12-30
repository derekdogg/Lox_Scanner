object fmScript: TfmScript
  Left = 0
  Top = 0
  Caption = 'Script'
  ClientHeight = 638
  ClientWidth = 1112
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
    Top = 352
    Width = 1112
    Height = 0
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsLine
    ExplicitTop = 301
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1112
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 520
      Top = 16
      Width = 54
      Height = 13
      Caption = 'Stack Top :'
    end
    object Label2: TLabel
      Left = 780
      Top = 22
      Width = 71
      Height = 13
      Caption = 'Stack Capacity'
    end
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
    object Edit1: TEdit
      Left = 580
      Top = 14
      Width = 121
      Height = 21
      ReadOnly = True
      TabOrder = 2
    end
    object Edit2: TEdit
      Left = 857
      Top = 15
      Width = 121
      Height = 21
      ReadOnly = True
      TabOrder = 3
    end
    object Button1: TButton
      Left = 352
      Top = 11
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 4
      OnClick = Button1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 352
    Width = 1112
    Height = 286
    Align = alBottom
    Caption = 'Panel2'
    TabOrder = 1
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 232
      Height = 284
      Align = alLeft
      Caption = 'Panel3'
      TabOrder = 0
      object MemLocals: TMemo
        Left = 1
        Top = 1
        Width = 230
        Height = 282
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
      Left = 233
      Top = 1
      Width = 571
      Height = 284
      Align = alClient
      Caption = 'Panel3'
      TabOrder = 1
      object MemRun: TMemo
        Left = 1
        Top = 1
        Width = 569
        Height = 282
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
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object Panel5: TPanel
      Left = 804
      Top = 1
      Width = 307
      Height = 284
      Align = alRight
      Caption = 'Panel3'
      TabOrder = 2
      object MemStack: TMemo
        Left = -7
        Top = 1
        Width = 313
        Height = 282
        Align = alRight
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
    Width = 1112
    Height = 311
    Align = alClient
    Caption = 'Panel6'
    TabOrder = 2
    object memEdit: TRichEdit
      Left = 1
      Top = 1
      Width = 1110
      Height = 309
      Align = alClient
      Color = clMenuText
      Font.Charset = ANSI_CHARSET
      Font.Color = clMoneyGreen
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      Lines.Strings = (
        'var i = 0;'
        ''
        ''
        'while (i < 10) { print i; i = i + 1;}')
      ParentFont = False
      TabOrder = 0
      WantTabs = True
    end
  end
end
