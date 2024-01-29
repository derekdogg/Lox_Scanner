object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 669
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
  object lblTokenCount: TLabel
    Left = 392
    Top = 48
    Width = 71
    Height = 13
    Caption = 'Token Count : '
  end
  object btnTokenizer: TButton
    Left = 24
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Tokenizer'
    TabOrder = 0
    OnClick = btnTokenizerClick
  end
  object MemoEdit: TMemo
    Left = 24
    Top = 72
    Width = 345
    Height = 553
    Lines.Strings = (
      'Memo2')
    TabOrder = 1
  end
  object lvWords: TListView
    Left = 384
    Top = 72
    Width = 1049
    Height = 561
    Columns = <
      item
        Caption = 'Text'
        Width = 200
      end
      item
        Caption = 'Line'
      end
      item
        Caption = 'Pos'
      end
      item
        Caption = 'Kind'
      end>
    TabOrder = 2
    ViewStyle = vsReport
  end
  object Button2: TButton
    Left = 105
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Clear Text'
    TabOrder = 3
    OnClick = Button2Click
  end
  object EdtTokenCount: TEdit
    Left = 469
    Top = 45
    Width = 121
    Height = 21
    ReadOnly = True
    TabOrder = 4
    Text = '0'
  end
end
