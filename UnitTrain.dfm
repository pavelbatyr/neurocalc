object FormTrain: TFormTrain
  Left = 0
  Top = 0
  Caption = #1053#1077#1081#1088#1086#1082#1072#1083#1100#1082#1091#1083#1103#1090#1086#1088': '#1086#1073#1091#1095#1077#1085#1080#1077
  ClientHeight = 555
  ClientWidth = 779
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object btnToTrain: TButton
    Left = 355
    Top = 72
    Width = 75
    Height = 25
    Caption = #1054#1073#1091#1095#1077#1085#1080#1077
    Enabled = False
    TabOrder = 0
  end
  object btnToUse: TButton
    Left = 449
    Top = 72
    Width = 75
    Height = 25
    Caption = #1047#1072#1087#1091#1089#1082
    TabOrder = 1
    OnClick = btnToUseClick
  end
  object btnToModel: TButton
    Left = 258
    Top = 72
    Width = 75
    Height = 25
    Caption = #1052#1086#1076#1077#1083#1100
    TabOrder = 2
    OnClick = btnToModelClick
  end
end
