object LocoDialogueWindow: TLocoDialogueWindow
  Left = 794
  Top = 567
  Anchors = [akLeft, akBottom]
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Select Loco'
  ClientHeight = 507
  ClientWidth = 244
  Color = clBtnFace
  DockSite = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnClose = LocoDialogueWindowClose
  OnHide = LocoDialogueWindowHide
  OnKeyDown = LocoDialogueWindowKeyDown
  OnKeyUp = LocoDialogueWindowKeyUp
  OnMouseDown = LocoDialogueWindowMouseDown
  OnMouseMove = LocoDialogueWindowMouseMove
  OnMouseWheel = LocoDialogueWindowMouseWheel
  OnShow = LocoDialogueWindowShow
  PixelsPerInch = 96
  TextHeight = 13
  object LocoDialogueMaskEditLabel: TLabel
    Left = 12
    Top = 8
    Width = 63
    Height = 13
    Caption = 'Item Number:'
  end
  object LocoDialogueSpeedDisplay: TLabel
    Left = 55
    Top = 116
    Width = 102
    Height = 77
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBtnShadow
    Font.Height = -64
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    OnMouseDown = LocoDialogueSpeedDisplayMouseDown
  end
  object LocoDialogueFunctionsLabel: TLabel
    Left = 16
    Top = 284
    Width = 49
    Height = 13
    Caption = 'Functions:'
    Enabled = False
    OnMouseDown = LocoDialogueFunctionsLabelMouseDown
  end
  object LocoDialogueFunction1Label: TLabel
    Left = 88
    Top = 276
    Width = 12
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '1'
    Enabled = False
  end
  object LocoDialogueFunction12Label: TLabel
    Left = 136
    Top = 308
    Width = 12
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '12'
    Enabled = False
  end
  object LocoDialogueFunction9Label: TLabel
    Left = 87
    Top = 308
    Width = 12
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '9'
    Enabled = False
  end
  object LocoDialogueFunction4Label: TLabel
    Left = 136
    Top = 276
    Width = 12
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '4'
    Enabled = False
  end
  object LocoDialogueFunction10Label: TLabel
    Left = 102
    Top = 308
    Width = 12
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '10'
    Enabled = False
  end
  object LocoDialogueFunction2Label: TLabel
    Left = 104
    Top = 276
    Width = 12
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '2'
    Enabled = False
  end
  object LocoDialogueFunction3Label: TLabel
    Left = 120
    Top = 276
    Width = 12
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '3'
    Enabled = False
  end
  object LocoDialogueFunction11Label: TLabel
    Left = 122
    Top = 308
    Width = 12
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '11'
    Enabled = False
  end
  object LocoDialogueFunction5Label: TLabel
    Left = 16
    Top = 308
    Width = 9
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = '5'
    Enabled = False
  end
  object LocoDialogueFunction8Label: TLabel
    Left = 64
    Top = 308
    Width = 12
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '8'
    Enabled = False
  end
  object LocoDialogueFunction6Label: TLabel
    Left = 32
    Top = 308
    Width = 12
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '6'
    Enabled = False
  end
  object LocoDialogueFunction7Label: TLabel
    Left = 48
    Top = 308
    Width = 12
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '7'
    Enabled = False
  end
  object LocoDialogueDHLocoMaskEditLabel: TLabel
    Left = 12
    Top = 356
    Width = 86
    Height = 13
    Caption = 'DH Loco Number:'
    OnMouseDown = LocoDialogueDHLocoMaskEditLabelMouseDown
  end
  object LocoDialogueFunction0Label: TLabel
    Left = 72
    Top = 276
    Width = 12
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
    Enabled = False
  end
  object LocoDialogueMPHLabel: TLabel
    Left = 120
    Top = 180
    Width = 24
    Height = 13
    Caption = 'MPH'
    Enabled = False
  end
  object LocoDialogueLeftButton: TSpeedButton
    Left = 16
    Top = 199
    Width = 65
    Height = 31
    GroupIndex = 1
    Caption = #231
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Wingdings'
    Font.Style = []
    ParentFont = False
    OnMouseDown = LocoDialogueLeftButtonMouseDown
  end
  object LocoDialogueRightButton: TSpeedButton
    Left = 87
    Top = 199
    Width = 65
    Height = 31
    GroupIndex = 1
    Caption = #232
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Wingdings'
    Font.Style = []
    ParentFont = False
    OnMouseDown = LocoDialogueRightButtonMouseDown
  end
  object LocoDialogueMaxLabel: TLabel
    Left = 55
    Top = 180
    Width = 20
    Height = 13
    Caption = 'Max'
    Enabled = False
    Visible = False
  end
  object LocoDialogueChangeOrSelectButton: TButton
    Left = 4
    Top = 36
    Width = 85
    Height = 25
    Caption = 'Change/Select'
    Default = True
    Enabled = False
    TabOrder = 2
    OnMouseDown = LocoDialogueChangeOrSelectButtonMouseDown
  end
  object LocoDialogueCancelButton: TButton
    Left = 100
    Top = 36
    Width = 57
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnMouseDown = LocoDialogueCancelButtonMouseDown
  end
  object LocoDialogueLocoQueryButton: TButton
    Left = 136
    Top = 4
    Width = 17
    Height = 25
    Caption = '?'
    TabOrder = 4
    OnMouseDown = LocoDialogueLocoQueryButtonMouseDown
  end
  object LocoDialogueEmergencyStopButton: TButton
    Left = 4
    Top = 76
    Width = 153
    Height = 25
    Caption = 'Emergency &Stop'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnMouseDown = LocoDialogueEmergencyStopButtonMouseDown
  end
  object LocoDialogueTurnLightsOnOrOffButton: TButton
    Left = 8
    Top = 245
    Width = 153
    Height = 25
    Caption = 'F0: Turn Lights On Or Off'
    Enabled = False
    TabOrder = 6
    OnMouseDown = LocoDialogueTurnLightsOnOrOffButtonMouseDown
  end
  object LocoDialogueLocoMaskEdit: TMaskEdit
    Left = 81
    Top = 5
    Width = 23
    Height = 21
    TabOrder = 0
    Text = ''
    OnChange = LocoDialogueLocoMaskEditChange
    OnKeyPress = LocoDialogueLocoMaskEditKeyPress
  end
  object LocoDialogueLocoTimerStartStopButton: TButton
    Left = 4
    Top = 424
    Width = 153
    Height = 25
    Caption = 'Start Loco Timer'
    Enabled = False
    TabOrder = 7
    OnMouseDown = LocoDialogueLocoTimerStartStopButtonMouseDown
  end
  object LocoDialogueFunction1CheckBox: TCheckBox
    Left = 88
    Top = 288
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 8
    OnClick = LocoDialogueFunction1CheckBoxClick
  end
  object LocoDialogueFunction2CheckBox: TCheckBox
    Left = 104
    Top = 288
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 9
    OnClick = LocoDialogueFunction2CheckBoxClick
  end
  object LocoDialogueFunction3CheckBox: TCheckBox
    Left = 120
    Top = 288
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 10
    OnClick = LocoDialogueFunction3CheckBoxClick
  end
  object LocoDialogueFunction4CheckBox: TCheckBox
    Left = 136
    Top = 288
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 11
    OnClick = LocoDialogueFunction4CheckBoxClick
  end
  object LocoDialogueFunction9CheckBox: TCheckBox
    Left = 88
    Top = 320
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 12
    OnClick = LocoDialogueFunction9CheckBoxClick
  end
  object LocoDialogueFunction10CheckBox: TCheckBox
    Left = 104
    Top = 320
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 13
    OnClick = LocoDialogueFunction10CheckBoxClick
  end
  object LocoDialogueFunction11CheckBox: TCheckBox
    Left = 120
    Top = 320
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 14
    OnClick = LocoDialogueFunction11CheckBoxClick
  end
  object LocoDialogueFunction12CheckBox: TCheckBox
    Left = 136
    Top = 320
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 15
    OnClick = LocoDialogueFunction12CheckBoxClick
  end
  object LocoDialogueFunction5CheckBox: TCheckBox
    Left = 16
    Top = 320
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 16
    OnClick = LocoDialogueFunction5CheckBoxClick
  end
  object LocoDialogueFunction6CheckBox: TCheckBox
    Left = 32
    Top = 320
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 17
    OnClick = LocoDialogueFunction6CheckBoxClick
  end
  object LocoDialogueFunction7CheckBox: TCheckBox
    Left = 48
    Top = 320
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 18
    OnClick = LocoDialogueFunction7CheckBoxClick
  end
  object LocoDialogueFunction8CheckBox: TCheckBox
    Left = 64
    Top = 320
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 19
    OnClick = LocoDialogueFunction8CheckBoxClick
  end
  object LocoDialogueDHLocoQueryButton: TButton
    Left = 136
    Top = 352
    Width = 17
    Height = 25
    Caption = '?'
    TabOrder = 20
    OnMouseDown = LocoDialogueDHLocoQueryButtonMouseDown
  end
  object LocoDialogueDHLocoMaskEdit: TMaskEdit
    Left = 104
    Top = 352
    Width = 23
    Height = 21
    TabOrder = 1
    Text = ''
    OnChange = LocoDialogueDHLocoMaskEditChange
    OnKeyPress = LocoDialogueDHLocoMaskEditKeyPress
  end
  object LocoDialogueDHLocoChangeOrSelectButton: TButton
    Left = 4
    Top = 384
    Width = 85
    Height = 25
    Caption = 'Change/Select'
    Default = True
    Enabled = False
    TabOrder = 21
    OnMouseDown = LocoDialogueDHLocoChangeOrSelectButtonMouseDown
  end
  object LocoDialogueDHLocoClearButton: TButton
    Left = 100
    Top = 384
    Width = 57
    Height = 25
    Cancel = True
    Caption = 'Clear DH'
    Enabled = False
    TabOrder = 22
    OnMouseDown = LocoDialogueDHLocoClearButtonMouseDown
  end
  object LocoDialogueFunction0CheckBox: TCheckBox
    Left = 72
    Top = 288
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 23
    OnClick = LocoDialogueFunction0CheckBoxClick
  end
  object LocoDialogueSpeedInMPHButton: TButton
    Left = 8
    Top = 466
    Width = 153
    Height = 25
    Caption = 'Switch To Speed In MPH'
    Enabled = False
    TabOrder = 24
    OnMouseDown = LocoDialogueSpeedInMPHButtonMouseDown
  end
  object LocoDialogueDownButton: TButton
    Left = 8
    Top = 154
    Width = 40
    Height = 37
    Caption = #234
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Wingdings'
    Font.Style = []
    ParentFont = False
    TabOrder = 25
    OnMouseDown = LocoDialogueDownButtonMouseDown
    OnMouseLeave = LocoDialogueDownButtonMouseLeave
    OnMouseUp = LocoDialogueDownButtonMouseUp
  end
  object LocoDialogueUpButton: TButton
    Left = 8
    Top = 107
    Width = 41
    Height = 41
    Caption = #233
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Wingdings'
    Font.Style = []
    ParentFont = False
    TabOrder = 26
    OnMouseDown = LocoDialogueUpButtonMouseDown
    OnMouseLeave = LocoDialogueUpButtonMouseLeave
    OnMouseUp = LocoDialogueUpButtonMouseUp
  end
  object LocoDialogueTimer: TTimer
    OnTimer = LocoDialogueTimerTick
    Left = 188
    Top = 300
  end
  object LocoDialogueMouseDownTimer: TTimer
    Enabled = False
    OnTimer = LocoDialogueMouseDownTimerTick
    Left = 192
    Top = 344
  end
end
