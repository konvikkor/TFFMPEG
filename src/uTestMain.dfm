object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 393
  ClientWidth = 590
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  PopupMenu = PopupMenu1
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 590
    Height = 374
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 586
    ExplicitHeight = 373
    object TabSheet1: TTabSheet
      Caption = 'LOG'
      object Splitter1: TSplitter
        Left = 394
        Top = 0
        Height = 346
        Align = alRight
        ExplicitLeft = 240
        ExplicitTop = 128
        ExplicitHeight = 100
      end
      object Splitter2: TSplitter
        Left = 185
        Top = 0
        Height = 346
        ExplicitLeft = 256
        ExplicitTop = 112
        ExplicitHeight = 100
      end
      object Memo1: TMemo
        Left = 188
        Top = 0
        Width = 206
        Height = 346
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Memo2: TMemo
        Left = 397
        Top = 0
        Width = 185
        Height = 346
        Align = alRight
        Lines.Strings = (
          'Memo2')
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object Memo3: TMemo
        Left = 0
        Top = 0
        Width = 185
        Height = 346
        Align = alLeft
        Lines.Strings = (
          'Memo3')
        ScrollBars = ssVertical
        TabOrder = 2
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'VideoCaplion'
      ImageIndex = 1
      object TrackBar1: TTrackBar
        Left = 0
        Top = 320
        Width = 582
        Height = 26
        Hint = 'TEST'
        Align = alBottom
        DoubleBuffered = True
        ParentDoubleBuffered = False
        ParentShowHint = False
        PageSize = 3
        Position = 10
        SelEnd = 5
        ShowHint = True
        TabOrder = 0
        TickMarks = tmBoth
        TickStyle = tsNone
        ExplicitTop = 319
        ExplicitWidth = 578
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'TabSheet3'
      ImageIndex = 2
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 374
    Width = 590
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 50
      end>
    ExplicitTop = 373
    ExplicitWidth = 586
  end
  object PopupMenu1: TPopupMenu
    Left = 88
    Top = 64
    object Open1: TMenuItem
      Caption = 'Open'
      OnClick = Open1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object DecodePak1: TMenuItem
      Caption = 'Decode Pak'
    end
    object Play1: TMenuItem
      Caption = 'Play'
      OnClick = Play1Click
    end
    object Stop1: TMenuItem
      Caption = 'Stop'
      OnClick = Stop1Click
    end
    object EST1: TMenuItem
      Caption = 'TEST'
      OnClick = EST1Click
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 176
    Top = 48
  end
  object TrayIcon1: TTrayIcon
    PopupMenu = PopupMenu1
    Visible = True
    Left = 228
    Top = 64
  end
  object TaskDialog1: TTaskDialog
    Buttons = <>
    RadioButtons = <>
    Left = 252
    Top = 152
  end
  object OpenDialog2: TOpenDialog
    Left = 156
    Top = 160
  end
end
