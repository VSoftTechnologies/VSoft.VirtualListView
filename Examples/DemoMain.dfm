object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 411
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 484
    Top = 0
    Width = 4
    Height = 411
    Align = alRight
    ResizeStyle = rsUpdate
    ExplicitLeft = 485
  end
  object Panel1: TPanel
    Left = 488
    Top = 0
    Width = 364
    Height = 411
    Align = alRight
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    object Label1: TLabel
      Left = 29
      Top = 32
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 29
      Top = 64
      Width = 31
      Height = 13
      Caption = 'Label2'
    end
    object Button1: TButton
      Left = 17
      Top = 112
      Width = 75
      Height = 25
      Caption = '0 Rows'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 137
      Top = 112
      Width = 75
      Height = 25
      Caption = '200 Rows'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 16
      Top = 168
      Width = 129
      Height = 25
      Caption = 'ChangeRow 4'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 16
      Top = 216
      Width = 129
      Height = 25
      Caption = 'ScrollInView'
      TabOrder = 3
      OnClick = Button4Click
    end
  end
  object VListView: TVSoftVirtualListView
    Left = 0
    Top = 0
    Width = 484
    Height = 411
    Align = alClient
    BevelInner = bvNone
    BevelKind = bkFlat
    BorderStyle = bsNone
    ParentBackground = True
    TabOrder = 1
    RowCount = 0
    RowHeight = 80
    OnPaintRow = VListViewPaintRow
    OnPaintNoRows = VListViewPaintNoRows
    OnRowChange = VListViewRowChange
  end
end
