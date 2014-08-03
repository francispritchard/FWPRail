UNIT LocoDialogue;
{ Implements an LH100 simulator

  v.2.0  02/03/13 major rewrite
}
INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Mask, ComCtrls, ExtCtrls, Buttons, System.Types;

TYPE
  TLocoDialogueWindow = CLASS(TForm)
    LocoDialogueCancelButton: TButton;
    LocoDialogueChangeOrSelectButton: TButton;
    LocoDialogueDHLocoChangeOrSelectButton: TButton;
    LocoDialogueDHLocoClearButton: TButton;
    LocoDialogueDHLocoMaskEdit: TMaskEdit;
    LocoDialogueDHLocoMaskEditLabel: TLabel;
    LocoDialogueDHLocoQueryButton: TButton;
    LocoDialogueDownButton: TButton;
    LocoDialogueEmergencyStopButton: TButton;
    LocoDialogueFunction0CheckBox: TCheckBox;
    LocoDialogueFunction0Label: TLabel;
    LocoDialogueFunction10CheckBox: TCheckBox;
    LocoDialogueFunction10Label: TLabel;
    LocoDialogueFunction11CheckBox: TCheckBox;
    LocoDialogueFunction11Label: TLabel;
    LocoDialogueFunction12CheckBox: TCheckBox;
    LocoDialogueFunction12Label: TLabel;
    LocoDialogueFunction1CheckBox: TCheckBox;
    LocoDialogueFunction1Label: TLabel;
    LocoDialogueFunction2CheckBox: TCheckBox;
    LocoDialogueFunction2Label: TLabel;
    LocoDialogueFunction3CheckBox: TCheckBox;
    LocoDialogueFunction3Label: TLabel;
    LocoDialogueFunction4CheckBox: TCheckBox;
    LocoDialogueFunction4Label: TLabel;
    LocoDialogueFunction5CheckBox: TCheckBox;
    LocoDialogueFunction5Label: TLabel;
    LocoDialogueFunction6CheckBox: TCheckBox;
    LocoDialogueFunction6Label: TLabel;
    LocoDialogueFunction7CheckBox: TCheckBox;
    LocoDialogueFunction7Label: TLabel;
    LocoDialogueFunction8CheckBox: TCheckBox;
    LocoDialogueFunction8Label: TLabel;
    LocoDialogueFunction9CheckBox: TCheckBox;
    LocoDialogueFunction9Label: TLabel;
    LocoDialogueFunctionsLabel: TLabel;
    LocoDialogueLeftButton: TSpeedButton;
    LocoDialogueLocoQueryButton: TButton;
    LocoDialogueLocoTimerStartStopButton: TButton;
    LocoDialogueLocoMaskEdit: TMaskEdit;
    LocoDialogueMaskEditLabel: TLabel;
    LocoDialogueMaxLabel: TLabel;
    LocoDialogueMouseDownTimer: TTimer;
    LocoDialogueMPHLabel: TLabel;
    LocoDialogueRightButton: TSpeedButton;
    LocoDialogueSpeedDisplay: TLabel;
    LocoDialogueSpeedInMPHButton: TButton;
    LocoDialogueTimer: TTimer;
    LocoDialogueTurnLightsOnOrOffButton: TButton;
    LocoDialogueUpButton: TButton;
    PROCEDURE LocoDialogueCancelButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueChangeOrSelectButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueDHLocoChangeOrSelectButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueDHLocoClearButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueDHLocoMaskEditChange(Sender: TObject);
    PROCEDURE LocoDialogueDHLocoMaskEditKeyPress(Sender: TObject; VAR Key: Char);
    PROCEDURE LocoDialogueDHLocoMaskEditLabelMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueDHLocoQueryButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueDownButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState : TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueDownButtonMouseLeave(Sender: TObject);
    PROCEDURE LocoDialogueDownButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueEmergencyStopButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueFunction0CheckBoxClick(Sender: TObject);
    PROCEDURE LocoDialogueFunction10CheckBoxClick(Sender: TObject);
    PROCEDURE LocoDialogueFunction11CheckBoxClick(Sender: TObject);
    PROCEDURE LocoDialogueFunction12CheckBoxClick(Sender: TObject);
    PROCEDURE LocoDialogueFunction1CheckBoxClick(Sender: TObject);
    PROCEDURE LocoDialogueFunction2CheckBoxClick(Sender: TObject);
    PROCEDURE LocoDialogueFunction3CheckBoxClick(Sender: TObject);
    PROCEDURE LocoDialogueFunction4CheckBoxClick(Sender: TObject);
    PROCEDURE LocoDialogueFunction5CheckBoxClick(Sender: TObject);
    PROCEDURE LocoDialogueFunction6CheckBoxClick(Sender: TObject);
    PROCEDURE LocoDialogueFunction7CheckBoxClick(Sender: TObject);
    PROCEDURE LocoDialogueFunction8CheckBoxClick(Sender: TObject);
    PROCEDURE LocoDialogueFunction9CheckBoxClick(Sender: TObject);
    PROCEDURE LocoDialogueFunctionsLabelMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueLeftButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState : TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueLocoQueryButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueLocoTimerStartStopButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueLocoMaskEditChange(Sender: TObject);
    PROCEDURE LocoDialogueLocoMaskEditKeyPress(Sender: TObject; VAR Key: Char);
    PROCEDURE LocoDialogueWindowMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueMouseDownTimerTick(Sender: TObject);
    PROCEDURE LocoDialogueWindowMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueWindowMouseWheel(Sender: TObject; ShiftState: TShiftState; WheelDelta: Integer; MousePos: TPoint; VAR Handled: Boolean);
    PROCEDURE LocoDialogueRightButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState : TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueSpeedDisplayMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueSpeedInMPHButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState : TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueTimerTick(Sender: TObject);
    PROCEDURE LocoDialogueTurnLightsOnOrOffButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueUpButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState : TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueUpButtonMouseLeave(Sender: TObject);
    PROCEDURE LocoDialogueUpButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    PROCEDURE LocoDialogueWindowClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE LocoDialogueWindowHide(Sender: TObject);
    PROCEDURE LocoDialogueWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE LocoDialogueWindowKeyUp(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE LocoDialogueWindowShow(Sender: TObject);
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE CheckEmergencyStop(Button: TMouseButton; ShiftState: TShiftState);
{ Sees if there's an emergency stop occasioned by the right mouse button being pressed }

PROCEDURE ControlSpeedByMouseWheel(WheelDelta: Integer; MousePos: TPoint);
{ Adjust speed by means of the mouse wheel - slow down the reading, though }

PROCEDURE EmergencyStopInLocoDialogue;
{ Causes an emergency stop and alters the display }

PROCEDURE EnableLocoDialogueLocoButtonsAndBoxes;
{ Enables the LocoDialogue's buttons and boxes }

FUNCTION GetLocoDialogueLocoChip : Integer;
{ Return the loco chip number that the dialogue has selected }

FUNCTION GetLocoDialogueLocoSpeed : Integer;
{ Return the speed from the dialogue window }

PROCEDURE InitialiseLocoDialogueUnit;
{ Initialises the unit }

PROCEDURE LocoDialogueChangeOrSelectLoco;
{ Select a loco }

PROCEDURE LocoDialogueDecreaseSpeed;
{ Decrease a loco's speed by means of the Loco Dialogue Box. This routine is called by a mouse down event and also by MainWindowShortCut }

PROCEDURE LocoDialogueIncreaseSpeed;
{ Increase a loco's speed by means of the Loco Dialogue Box. This routine is called by a mouse down event and also by MainWindowShortCut }

VAR
  LocoDialogueWindow : TLocoDialogueWindow;

IMPLEMENTATION

{$R *.dfm}

USES InitVars, LocoUtils, MiscUtils, Lenz, Feedback, DateUtils, Input, Options;

CONST
  UnitRef = 'LocoDialogue';

VAR
  AltPressed : Boolean = False;
  CtrlPressed : Boolean = False;
  LocoDialogueCharValid : Boolean = False;
  LocoDialogueDoubleHeaderLocoChip : Integer = UnknownLocoChip;
  LocoDialogueDoubleHeaderLocoIndex : LocoIndex = UnknownLocoIndex;
  LocoDialogueDoubleHeaderLocoIndexFound : Boolean = False;
  LocoDialogueIncreaseSpeedFlag : Boolean = True;
  LocoDialogueLocoIndex : LocoIndex = UnknownLocoIndex;
  LocoDialogueLocoIndexFound : Boolean = False;
  LocoDialogueLocoChip : Integer = UnknownLocoChip;
  LocoDialogueLocoSpeed : Integer = 0;
  LocoDialogueLocoSpeedInMPH : MPHType = MPH0;
  LocoDialogueMouseDownTime : TDateTime = 0;
  LocoDialogueShiftState : TShiftState = [];
  LocoTakenOver : Boolean = False;
  SaveLocoTakenOverState : Boolean = False;
  SaveWheelTime : TDateTime = 0;
  ShiftPressed : Boolean = False;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION GetLocoDialogueLocoChip : Integer;
{ Return the loco chip number that the dialogue has selected }
BEGIN
  Result := LocoDialogueLocoChip;
END; { GetLocoDialogueLocoChip }

FUNCTION GetLocoDialogueLocoSpeed : Integer;
{ Return the speed from the dialogue window }
BEGIN
  Result := LocoDialogueLocoSpeed;
END; { GetLocoDialogueLocoSpeed }

PROCEDURE SwitchMPHSpeedsOn;
BEGIN
  WITH LocoDialogueWindow DO BEGIN
    LocoDialogueSpeedInMPH := True;
    LocoDialogueSpeedInMPHButton.Caption := 'Switch To Speed Steps';
    LocoDialogueMPHLabel.Visible := True;
  END; {WITH}
END; { SwitchMPHSpeedsOn }

PROCEDURE DisableFunctionBoxes;
{ Stops the function boxes being ticked }
BEGIN
  WITH LocoDialogueWindow DO BEGIN
    LocoDialogueFunction0CheckBox.Enabled := False;
    LocoDialogueFunction0Label.Enabled := False;
    LocoDialogueFunction1CheckBox.Enabled := False;
    LocoDialogueFunction1Label.Enabled := False;
    LocoDialogueFunction2CheckBox.Enabled := False;
    LocoDialogueFunction2Label.Enabled := False;
    LocoDialogueFunction3CheckBox.Enabled := False;
    LocoDialogueFunction3Label.Enabled := False;
    LocoDialogueFunction4CheckBox.Enabled := False;
    LocoDialogueFunction4Label.Enabled := False;
    LocoDialogueFunction5CheckBox.Enabled := False;
    LocoDialogueFunction5Label.Enabled := False;
    LocoDialogueFunction6CheckBox.Enabled := False;
    LocoDialogueFunction6Label.Enabled := False;
    LocoDialogueFunction7CheckBox.Enabled := False;
    LocoDialogueFunction7Label.Enabled := False;
    LocoDialogueFunction8CheckBox.Enabled := False;
    LocoDialogueFunction8Label.Enabled := False;
    LocoDialogueFunction9CheckBox.Enabled := False;
    LocoDialogueFunction9Label.Enabled := False;
    LocoDialogueFunctionsLabel.Enabled := False;
    LocoDialogueFunction10CheckBox.Enabled := False;
    LocoDialogueFunction10Label.Enabled := False;
    LocoDialogueFunction11CheckBox.Enabled := False;
    LocoDialogueFunction11Label.Enabled := False;
    LocoDialogueFunction12CheckBox.Enabled := False;
    LocoDialogueFunction12Label.Enabled := False;
  END; {WITH}
END; { DisableFunctionBoxes }

PROCEDURE EnableFunctionBoxes;
{ Allows the function boxes to be ticked }
BEGIN
  WITH LocoDialogueWindow DO BEGIN
    LocoDialogueFunction0CheckBox.Enabled := True;
    LocoDialogueFunction0Label.Enabled := True;
    LocoDialogueFunction1CheckBox.Enabled := True;
    LocoDialogueFunction1Label.Enabled := True;
    LocoDialogueFunction2CheckBox.Enabled := True;
    LocoDialogueFunction2Label.Enabled := True;
    LocoDialogueFunction3CheckBox.Enabled := True;
    LocoDialogueFunction3Label.Enabled := True;
    LocoDialogueFunction4CheckBox.Enabled := True;
    LocoDialogueFunction4Label.Enabled := True;
    LocoDialogueFunction5CheckBox.Enabled := True;
    LocoDialogueFunction5Label.Enabled := True;
    LocoDialogueFunction6CheckBox.Enabled := True;
    LocoDialogueFunction6Label.Enabled := True;
    LocoDialogueFunction7CheckBox.Enabled := True;
    LocoDialogueFunction7Label.Enabled := True;
    LocoDialogueFunction8CheckBox.Enabled := True;
    LocoDialogueFunction8Label.Enabled := True;
    LocoDialogueFunction9CheckBox.Enabled := True;
    LocoDialogueFunction9Label.Enabled := True;
    LocoDialogueFunctionsLabel.Enabled := True;
    LocoDialogueFunction10CheckBox.Enabled := True;
    LocoDialogueFunction10Label.Enabled := True;
    LocoDialogueFunction11CheckBox.Enabled := True;
    LocoDialogueFunction11Label.Enabled := True;
    LocoDialogueFunction12CheckBox.Enabled := True;
    LocoDialogueFunction12Label.Enabled := True;
  END; {WITH}
END; { EnableFunctionBoxes }

PROCEDURE TLocoDialogueWindow.LocoDialogueWindowMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
{ If the mouse moves into the dialogue window, move the focus there }
BEGIN
  IF NOT KeyboardAndMouseLocked AND (LocoDialogueWindow <> NIL) THEN BEGIN
    IF NOT LocoDialogueWindow.Active THEN
      LocoDialogueWindow.SetFocus;
  END;
END; { LocoDialogueWindowMouseMove }

PROCEDURE TLocoDialogueWindow.LocoDialogueLocoMaskEditChange(Sender: TObject);
CONST
  SetUp = True;
  StartButton = True;
  Surround = True;
  UndrawRequired = True;
  UndrawToBeAutomatic = True;

BEGIN
  TRY
    DisableFunctionBoxes;

    LocoDialogueChangeOrSelectButton.Caption := 'Select &Loco 1';
    LocoDialogueChangeOrSelectButton.Enabled := False;
    LocoDialogueEmergencyStopButton.Enabled := False;
    LocoDialogueLocoIndex := UnknownLocoIndex;
    LocoDialogueLocoIndexFound := False;
    LocoDialogueLocoTimerStartStopButton.Enabled := False;
    LocoDialogueSpeedInMPHButton.Enabled := False;
    LocoDialogueSpeedDisplay.Font.Color := clBtnShadow;
    LocoDialogueTurnLightsOnOrOffButton.Caption := '&F0: Turn Lights On or Off';
    LocoDialogueTurnLightsOnOrOffButton.Enabled := False;

    LocoDialogueDHLocoChangeOrSelectButton.Enabled := False;
    LocoDialogueDHLocoMaskEditLabel.Enabled := False;
    LocoDialogueDHLocoMaskEdit.Enabled := False;
    LocoDialogueDHLocoQueryButton.Enabled := False;
    LocoDialogueDHLocoClearButton.Enabled := False;

    IF LocoDialogueLocoMaskEdit.Text <> '' THEN BEGIN
      LocoDialogueLocoIndex := 0;
      WHILE (LocoDialogueLocoIndex <= High(Locos)) AND NOT (LocoDialogueLocoIndexFound) DO BEGIN
        IF Locos[LocoDialogueLocoIndex].Loco_LocoChip <> StrToInt(LocoDialogueLocoMaskEdit.Text) THEN BEGIN
          LocoDialogueChangeOrSelectButton.Enabled := False;
          Inc(LocoDialogueLocoIndex);
        END ELSE BEGIN
          { found a loco }
          LocoDialogueChangeOrSelectButton.Enabled := True;
          LocoDialogueLocoIndexFound := True;

          { and also get its speed in mph data and its lighting data, as that is not normally loaded unless it is in a diagram *** }

        END;
      END; {WHILE}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG LocoDialogueMaskEditChange:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { LocoDialogueMaskEditChange }

PROCEDURE TLocoDialogueWindow.LocoDialogueDHLocoMaskEditChange(Sender: TObject);
BEGIN
  TRY
    LocoDialogueDHLocoChangeOrSelectButton.Caption := 'Select Lo&co 2';
    LocoDialogueDHLocoChangeOrSelectButton.Enabled := False;
    LocoDialogueDoubleHeaderLocoIndexFound := False;
    LocoDialogueDHLocoClearButton.Enabled := True;

    IF LocoDialogueDHLocoMaskEdit.Text = '' THEN BEGIN
      LocoDialogueUpButton.Enabled := True;
      LocoDialogueDownButton.Enabled := True;
    END ELSE BEGIN
      LocoDialogueDoubleHeaderLocoIndex := 0;
      WHILE (LocoDialogueDoubleHeaderLocoIndex <= High(Locos)) AND NOT (LocoDialogueDoubleHeaderLocoIndexFound) DO BEGIN
        IF Locos[LocoDialogueDoubleHeaderLocoIndex].Loco_LocoChip <> StrToInt(LocoDialogueDHLocoMaskEdit.Text) THEN BEGIN
          LocoDialogueChangeOrSelectButton.Enabled := False;
          Inc(LocoDialogueDoubleHeaderLocoIndex);
        END ELSE BEGIN
          { found a loco }
          LocoDialogueDoubleHeaderLocoIndexFound := True;
          LocoDialogueDHLocoChangeOrSelectButton.Enabled := True;

          { now disable all the other functions until this loco is selected or the number cleared - this avoids the possibility of a number being entered but not selected }
          DisableFunctionBoxes;
          LocoDialogueChangeOrSelectButton.Enabled := False;
          LocoDialogueLocoTimerStartStopButton.Enabled := False;
          LocoDialogueSpeedInMPHButton.Enabled := False;
          LocoDialogueUpButton.Enabled := False;
          LocoDialogueDownButton.Enabled := False;
          LocoDialogueSpeedDisplay.Font.Color := clBtnShadow;
          LocoDialogueTurnLightsOnOrOffButton.Enabled := False;
          LocoDialogueLeftButton.Enabled := False;
          LocoDialogueRightButton.Enabled := False;
          LocoDialogueSpeedInMPHButton.Enabled := False;
        END;
      END; {WHILE}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG LocoDialogueDHLocoMaskEditChange:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { LocoDialogueDHLocoMaskEditChange }

PROCEDURE LocoDialogueReturnFunctionState;
{ Reads in the function states for the chip. Does not need ForceRead as a loco speed read has recently been done }
VAR
  FunctionsArray : ARRAY[0..12] OF Boolean;
  OK : Boolean;

BEGIN
  WITH LocoDialogueWindow DO BEGIN
    IF LocoDialogueLocoIndexFound THEN BEGIN
      { Function 0 }
      WITH Locos[LocoDialogueLocoIndex] DO BEGIN
        IF Loco_LightsType <> NoLights THEN
          LocoDialogueTurnLightsOnOrOffButton.Enabled := True;

        GetLocoFunctions(Locos[LocoDialogueLocoIndex], NOT ForceARead, FunctionsArray, OK);

        IF FunctionsArray[0] THEN BEGIN
          IF (Loco_LightsType = HeadlightsAndTailLightsConnected)
          OR (Loco_LightsType = LightsOperatedByTwoChips)
          THEN
            LocoDialogueTurnLightsOnOrOffButton.Caption := '&F0: Turn Lights Off';
          LocoDialogueFunction0CheckBox.Checked := True
        END ELSE BEGIN
          IF (Loco_LightsType= HeadlightsAndTailLightsConnected)
          OR (Loco_LightsType = LightsOperatedByTwoChips)
          THEN
            LocoDialogueTurnLightsOnOrOffButton.Caption := '&F0: Turn Lights On';
          LocoDialogueFunction0CheckBox.Checked := False;
        END;
      END; {WITH}

      { Function 1 }
      IF FunctionsArray[1] THEN
        LocoDialogueFunction1CheckBox.Checked := True
      ELSE
        LocoDialogueFunction1CheckBox.Checked := False;

      { Function 2 }
      IF FunctionsArray[2] THEN
        LocoDialogueFunction2CheckBox.Checked := True
      ELSE
        LocoDialogueFunction2CheckBox.Checked := False;

      { Function 3 }
      IF FunctionsArray[3] THEN
        LocoDialogueFunction3CheckBox.Checked := True
      ELSE
        LocoDialogueFunction3CheckBox.Checked := False;

      { Function 4 }
      IF FunctionsArray[4] THEN
        LocoDialogueFunction4CheckBox.Checked := True
      ELSE
        LocoDialogueFunction4CheckBox.Checked := False;

      { Function 5 }
      IF FunctionsArray[5] THEN
        LocoDialogueFunction5CheckBox.Checked := True
      ELSE
        LocoDialogueFunction5CheckBox.Checked := False;

      { Function 6 }
      IF FunctionsArray[6] THEN
        LocoDialogueFunction6CheckBox.Checked := True
      ELSE
        LocoDialogueFunction6CheckBox.Checked := False;

      { Function 7 }
      IF FunctionsArray[7] THEN
        LocoDialogueFunction7CheckBox.Checked := True
      ELSE
        LocoDialogueFunction7CheckBox.Checked := False;

      { Function 8 }
      IF FunctionsArray[8] THEN
        LocoDialogueFunction8CheckBox.Checked := True
      ELSE
        LocoDialogueFunction8CheckBox.Checked := False;

      { Function 9 }
      IF FunctionsArray[9] THEN
        LocoDialogueFunction9CheckBox.Checked := True
      ELSE
        LocoDialogueFunction9CheckBox.Checked := False;

      { Function 10 }
      IF FunctionsArray[10] THEN
        LocoDialogueFunction10CheckBox.Checked := True
      ELSE
        LocoDialogueFunction10CheckBox.Checked := False;

      { Function 11 }
      IF FunctionsArray[11] THEN
        LocoDialogueFunction11CheckBox.Checked := True
      ELSE
        LocoDialogueFunction11CheckBox.Checked := False;

      { Function 12 }
      IF FunctionsArray[12] THEN
        LocoDialogueFunction12CheckBox.Checked := True
      ELSE
        LocoDialogueFunction12CheckBox.Checked := False;
    END {WITH}
  END;
END; { LocoDialogueReturnFunctionState }

PROCEDURE EnableLocoDialogueLocoButtonsAndBoxes;
{ Enables the LocoDialogue's buttons and boxes }
BEGIN
  TRY
    WITH LocoDialogueWindow DO BEGIN
      EnableFunctionBoxes;

      LocoDialogueChangeOrSelectButton.Enabled := True;
      IF LocoDialogueLocoIndexFound THEN BEGIN
        WITH Locos[LocoDialogueLocoIndex] dO BEGIN
          LocoDialogueLocoChip := Loco_LocoChip;
          LocoDialogueLocoSpeed := GetLenzSpeed(Locos[LocoDialogueLocoIndex], ForceARead);
          IF (LocoDialogueLocoSpeed > 0) AND (LocoDialogueLocoSpeed < 29) THEN BEGIN
            LocoDialogueUpButton.Enabled := True;
            LocoDialogueDownButton.Enabled := True;
            LocoDialogueLeftButton.Enabled := False;
            LocoDialogueRightButton.Enabled := False;
          END ELSE
            IF LocoDialogueLocoSpeed = 0 THEN BEGIN
              LocoDialogueUpButton.Enabled := True;
              LocoDialogueDownButton.Enabled := False;
              LocoDialogueLeftButton.Enabled := True;
              LocoDialogueRightButton.Enabled := True;
            END ELSE
              IF LocoDialogueLocoSpeed = 28 THEN BEGIN
                LocoDialogueUpButton.Enabled := False;
                LocoDialogueDownButton.Enabled := True;
              END;

          IF Loco_CurrentDirection = Up THEN
            LocoDialogueLeftButton.Down := True
          ELSE
            LocoDialogueRightButton.Down := True;

          IF NOT Loco_SpeedSettingsMissing THEN
            LocoDialogueSpeedInMPHButton.Enabled := True
          ELSE BEGIN
            LocoDialogueSpeedInMPH := False;
            LocoDialogueSpeedInMPHButton.Caption := 'Switch to Speed In MPH';
            LocoDialogueSpeedInMPHButton.Enabled := False;
            LocoDialogueMaxLabel.Visible := False;
            LocoDialogueMPHLabel.Visible := False;
          END;
        END;

        LocoDialogueEmergencyStopButton.Enabled := True;
        LocoDialogueLocoTimerStartStopButton.Enabled := True;
        IF NOT LocoSpeedTimingMode THEN
          LocoDialogueLocoTimerStartStopButton.Caption := 'Start Loco Timer'
        ELSE
          LocoDialogueLocoTimerStartStopButton.Caption := 'Stop Loco Timer';
        LocoDialogueSpeedDisplay.Caption := IntToStr(LocoDialogueLocoSpeed);
        LocoDialogueSpeedDisplay.Font.Color := clBtnText;
        LocoDialogueReturnFunctionState;
        LocoDialogueChangeOrSelectButton.Caption := 'Deselect &Loco 1';

        LocoDialogueDHLocoMaskEditLabel.Enabled := True;
        LocoDialogueDHLocoMaskEdit.Enabled := True;
        LocoDialogueDHLocoQueryButton.Enabled := True;
        LocoDialogueDHLocoClearButton.Enabled := True;
        IF LocoDialogueDHLocoMaskEdit.Text <> '' THEN BEGIN
          LocoDialogueDHLocoChangeOrSelectButton.Enabled := True;
          LocoDialogueDHLocoClearButton.Enabled := True;
        END;

        LocoDialogueMaxLabel.Enabled := True;
        LocoDialogueMPHLabel.Enabled := True;

        LocoDialogueEmergencyStopButton.SetFocus;
      END; {WITH}
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG EnableLocoDialogueLocoButtonsAndBoxes:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { EnableLocoDialogueLocoButtonsAndBoxes }

PROCEDURE DisableLocoDialogueLocoButtonsAndBoxes;
{ Disables the LocoDialogue's buttons and boxes }
BEGIN
  WITH LocoDialogueWindow DO BEGIN
    LocoDialogueFunction0CheckBox.Enabled := False;
    LocoDialogueFunction0Label.Enabled := False;
    LocoDialogueFunction1CheckBox.Enabled := False;
    LocoDialogueFunction1Label.Enabled := False;
    LocoDialogueFunction2CheckBox.Enabled := False;
    LocoDialogueFunction2Label.Enabled := False;
    LocoDialogueFunction3CheckBox.Enabled := False;
    LocoDialogueFunction3Label.Enabled := False;
    LocoDialogueFunction4CheckBox.Enabled := False;
    LocoDialogueFunction4Label.Enabled := False;
    LocoDialogueFunction5CheckBox.Enabled := False;
    LocoDialogueFunction5Label.Enabled := False;
    LocoDialogueFunction6CheckBox.Enabled := False;
    LocoDialogueFunction6Label.Enabled := False;
    LocoDialogueFunction7CheckBox.Enabled := False;
    LocoDialogueFunction7Label.Enabled := False;
    LocoDialogueFunction8CheckBox.Enabled := False;
    LocoDialogueFunction8Label.Enabled := False;
    LocoDialogueFunction9CheckBox.Enabled := False;
    LocoDialogueFunction9Label.Enabled := False;
    LocoDialogueFunctionsLabel.Enabled := False;
    LocoDialogueFunction10CheckBox.Enabled := False;
    LocoDialogueFunction10Label.Enabled := False;
    LocoDialogueFunction11CheckBox.Enabled := False;
    LocoDialogueFunction11Label.Enabled := False;
    LocoDialogueFunction12CheckBox.Enabled := False;
    LocoDialogueFunction12Label.Enabled := False;
    LocoDialogueChangeOrSelectButton.Caption := 'Select &Loco 1';
    LocoDialogueEmergencyStopButton.Enabled := False;
    LocoDialogueSpeedDisplay.Font.Color := clBtnShadow;
    LocoDialogueTurnLightsOnOrOffButton.Enabled := False;
    LocoDialogueLocoTimerStartStopButton.Enabled := False;
    LocoDialogueSpeedInMPHButton.Enabled := False;

    LocoDialogueUpButton.Enabled := False;
    LocoDialogueDownButton.Enabled := False;
    LocoDialogueLeftButton.Enabled := False;
    LocoDialogueRightButton.Enabled := False;

    LocoDialogueDHLocoChangeOrSelectButton.Caption := 'Select Lo&co 2';
    LocoDialogueDHLocoChangeOrSelectButton.Enabled := False;
    LocoDialogueDHLocoMaskEditLabel.Enabled := False;
    LocoDialogueDHLocoMaskEdit.Enabled := False;
    LocoDialogueDHLocoQueryButton.Enabled := False;
    LocoDialogueDHLocoClearButton.Enabled := False;
    LocoDialogueMaxLabel.Enabled := False;
    LocoDialogueMPHLabel.Enabled := False;

    LocoDialogueLocoMaskEdit.SetFocus;
  END; {WITH}
END; { DisableLocoDialogueLocoButtonsAndBoxes }

PROCEDURE EmergencyStopInLocoDialogue;
{ Causes an emergency stop and alters the display }
VAR
  OK : Boolean;

BEGIN
  IF LocoDialogueLocoIndexFound THEN BEGIN
    WITH LocoDialogueWindow DO BEGIN
      LocoDialogueDownButton.Enabled := False;
      WITH Locos[LocoDialogueLocoIndex] DO
        SetLenzSpeedAndDirection(Locos[LocoDialogueLocoIndex], QuickStop, Loco_CurrentDirection, OK);
      IF LocoDialogueDoubleHeaderLocoIndexFound THEN
        WITH Locos[LocoDialogueDoubleHeaderLocoIndex] DO
          SetLenzSpeedAndDirection(Locos[LocoDialogueDoubleHeaderLocoIndex], QuickStop, Loco_CurrentDirection, OK);

      WITH Locos[LocoDialogueLocoIndex] DO
        LocoDialogueLocoSpeed := GetLenzSpeed(Locos[LocoDialogueLocoIndex], ForceARead);
      IF LocoDialogueLocoSpeed <> 0 THEN
        Log('XG Emergency stop - speed is still ' + IntToStr(LocoDialogueLocoSpeed));
      IF NOT LocoDialogueSpeedInMPH THEN
        LocoDialogueSpeedDisplay.Caption := IntToStr(LocoDialogueLocoSpeed)
      ELSE
        LocoDialogueLocoSpeedInMPH := MPH0;

      IF LocoDialogueLocoSpeed = 0 THEN BEGIN
        LocoDialogueSpeedDisplay.Caption := '0';
        LocoDialogueDownButton.Enabled := False;
        LocoDialogueUpButton.Enabled := True;
        LocoDialogueLeftButton.Enabled := True;
        LocoDialogueRightButton.Enabled := True;
      END;

      Log(LocoChipToStr(LocoDialogueLocoChip) + ' L Emergency stopped by Loco Dialogue');
    END; {WITH}
  END;
END; { EmergencyStop }

PROCEDURE TLocoDialogueWindow.LocoDialogueWindowHide(Sender: TObject);
CONST
  StartButton = True;
  SetUp = True;

BEGIN
  { Need to undraw the rectangle if any }
  LocoDialogueWindowLeft := Left;
  LocoDialogueWindowTop := Top;

  SaveLocoDialogueMaskEditText := LocoDialogueLocoMaskEdit.Text;

  { Restore the mouse cursor to its original position only if it's still within the Dialogue box when we hide the Dialogue box }
  IF PtInRect(Rect(LocoDialogueWindow.Left,
                   LocoDialogueWindow.Top,
                   LocoDialogueWindow.Left + Width,
                   LocoDialogueWindow.Top + Height),
              Mouse.CursorPos)
  THEN
    Mouse.CursorPos := SaveMouseCursorPos;
END; { LocoDialogueHide }

PROCEDURE TLocoDialogueWindow.LocoDialogueWindowShow(Sender: TObject);
BEGIN
  { Set up size of box contents first }
  WITH LocoDialogueLocoMaskEdit DO BEGIN
    Left := 100;
    Top := 4;
    Width := 33;
    Height := 21;
    TabOrder := 0;
    OnChange := LocoDialogueLocoMaskEditChange;
  END; {WITH}

  WITH LocoDialogueDHLocoMaskEdit DO BEGIN
    Left := 100;
//    Top := 316;
    Width := 33;
    Height := 21;
    TabOrder := 0;
    OnChange := LocoDialogueDHLocoMaskEditChange;
  END; {WITH}

  WITH LocoDialogueChangeOrSelectButton DO BEGIN
    Left := 4;
    Top := 36;
    Width := 85;
    Height := 25;
    Caption := 'Change Button';
    Default := True;
    Enabled := False;
    TabOrder := 1;
//    OnClick := LocoDialogueChangeOrSelectButtonClick;
  END; {WITH}

  WITH LocoDialogueCancelButton DO BEGIN
    Left := 100;
    Top := 36;
    Width := 53;
    Height := 25;
    Cancel := True;
    Caption := 'Cancel';
    TabOrder := 2;
//    OnClick := LocoDialogueCancelButtonClick;
  END; {WITH}

  Left := LocoDialogueWindowLeft;
  Top := LocoDialogueWindowTop;
  ClientWidth := 162;
//  ClientHeight := 425;

  IF LocoDialogueLocoIndexFound THEN BEGIN
    IF (SaveLocoDialogueMaskEditText <> '') AND (StrToInt(SaveLocoDialogueMaskEditText) <> 0) THEN BEGIN
      { display the same number in the edit box }
      LocoDialogueLocoMaskEdit.Text := SaveLocoDialogueMaskEditText;
      LocoDialogueChangeOrSelectButton.Caption := 'Deselect &Loco 1';
      EnableLocoDialogueLocoButtonsAndBoxes;
    END ELSE BEGIN
      LocoDialogueLocoMaskEdit.Text := '';
      LocoDialogueChangeOrSelectButton.Caption := 'Select &Loco 1';
      DisableLocoDialogueLocoButtonsAndBoxes;
    END;
  END;

  LocoDialogueWindow.Caption := 'Select Loco';
  LocoDialogueMaskEditLabel.Caption := 'Loco Number:';
  LocoDialogueLocoMaskEdit.MaxLength := 4;
  LocoDialogueLocoQueryButton.Show;
  LocoDialogueLocoMaskEdit.SetFocus;

  { Save where the mouse cursor is }
  SaveMouseCursorPos := Mouse.CursorPos;
  { And move it to the debug window }
  Mouse.CursorPos := Point(LocoDialogueWindow.Left + (LocoDialogueWindow.Width DIV 2),
                           LocoDialogueWindow.Top + (LocoDialogueWindow.Height DIV 2));
END; { LocoDialogueShow }

PROCEDURE TLocoDialogueWindow.LocoDialogueLocoMaskEditKeyPress(Sender: TObject; VAR Key: Char);
BEGIN
  LocoDialogueCharValid := False;
  CASE Key OF
    '0'..'9', Chr(vk_Back):
      LocoDialogueCharValid := True;
  ELSE
    Key := #0; { seems to work by turning the naughty non-numeric key into a null keystroke }
  END; {CASE}
END; { LocoDialogueMaskEditKeyPress }

PROCEDURE TLocoDialogueWindow.LocoDialogueDHLocoMaskEditKeyPress(Sender: TObject; VAR Key: Char);
BEGIN
  LocoDialogueCharValid := False;
  CASE Key OF
    '0'..'9', Chr(vk_Back):
      LocoDialogueCharValid := True;
  ELSE
    Key := #0; { seems to work by turning the naughty non-numeric key into a null keystroke }
  END; {CASE}
END; { LocoDialogueDHLocoMaskEditKeyPress }

PROCEDURE FunctionCheckBoxClick(Checked : Boolean; FunctionNum : Integer);
{ Deal with function check boxes being ticked }
CONST
  TurnOn = True;

VAR
  OK : Boolean;

BEGIN
  LocoTakenOver := False;

  IF LocoDialogueLocoIndexFound THEN BEGIN
    WITH Locos[LocoDialogueLocoIndex] DO BEGIN
      IF Checked THEN
        SetSingleLocoFunction(Locos[LocoDialogueLocoIndex], FunctionNum, TurnOn, OK)
      ELSE
        SetSingleLocoFunction(Locos[LocoDialogueLocoIndex], FunctionNum, NOT TurnOn, OK);
    END; {WITH}
  END;

  LocoDialogueWindow.LocoDialogueEmergencyStopButton.SetFocus;
END; { FunctionCheckBoxClick }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunction0CheckBoxClick(Sender: TObject);
BEGIN
  FunctionCheckBoxClick(LocoDialogueFunction0CheckBox.Checked, Function0);
END; { LocoDialogueFunction0CheckBoxClick }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunction1CheckBoxClick(Sender: TObject);
BEGIN
  FunctionCheckBoxClick(LocoDialogueFunction1CheckBox.Checked, Function1);
END; { LocoDialogueFunction1CheckBoxClick }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunction2CheckBoxClick(Sender: TObject);
BEGIN
  FunctionCheckBoxClick(LocoDialogueFunction2CheckBox.Checked, Function2);
END; { LocoDialogueFunction2CheckBoxClick }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunction3CheckBoxClick(Sender: TObject);
BEGIN
  FunctionCheckBoxClick(LocoDialogueFunction3CheckBox.Checked, Function3);
END; { LocoDialogueFunction3CheckBoxClick }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunction4CheckBoxClick(Sender: TObject);
BEGIN
  FunctionCheckBoxClick(LocoDialogueFunction4CheckBox.Checked, Function4);
END; { LocoDialogueFunction4CheckBoxClick }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunction5CheckBoxClick(Sender: TObject);
BEGIN
  FunctionCheckBoxClick(LocoDialogueFunction5CheckBox.Checked, Function5);
END; { LocoDialogueFunction5CheckBoxClick }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunction6CheckBoxClick(Sender: TObject);
BEGIN
  FunctionCheckBoxClick(LocoDialogueFunction6CheckBox.Checked, Function6);
END; { LocoDialogueFunction6CheckBoxClick }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunction7CheckBoxClick(Sender: TObject);
BEGIN
  FunctionCheckBoxClick(LocoDialogueFunction7CheckBox.Checked, Function7);
END; { LocoDialogueFunction7CheckBoxClick }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunction8CheckBoxClick(Sender: TObject);
BEGIN
  FunctionCheckBoxClick(LocoDialogueFunction8CheckBox.Checked, Function8);
END; { LocoDialogueFunction8CheckBoxClick }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunction9CheckBoxClick(Sender: TObject);
BEGIN
  FunctionCheckBoxClick(LocoDialogueFunction9CheckBox.Checked, Function9);
END; { LocoDialogueFunction9CheckBoxClick }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunction10CheckBoxClick(Sender: TObject);
BEGIN
  FunctionCheckBoxClick(LocoDialogueFunction10CheckBox.Checked, Function10);
END; { LocoDialogueFunction10CheckBoxClick }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunction11CheckBoxClick(Sender: TObject);
BEGIN
  FunctionCheckBoxClick(LocoDialogueFunction11CheckBox.Checked, Function11);
END; { LocoDialogueFunction11CheckBoxClick }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunction12CheckBoxClick(Sender: TObject);
BEGIN
  FunctionCheckBoxClick(LocoDialogueFunction12CheckBox.Checked, Function12);
END; { LocoDialogueFunction12CheckBoxClick }

PROCEDURE IncrementSpeedInMPH;
{ Increase the speed in MPH and also check whether the maximum speed has been reached }
BEGIN
  WITH LocoDialogueWindow DO BEGIN
    IF LocoDialogueLocoIndexFound THEN BEGIN
      WITH Locos[LocoDialogueLocoIndex] DO BEGIN
        CASE LocoDialogueLocoSpeedInMPH OF
          MPH0:
            BEGIN
              LocoDialogueLocoSpeedInMPH := MPH10;
              LocoDialogueMaxLabel.Visible := False;
            END;
          MPH10:
            BEGIN
              LocoDialogueLocoSpeedInMPH := MPH20;
              IF Loco_MaximumSpeedInMPH = MPH20 THEN BEGIN
                LocoDialogueUpButton.Enabled := False;
                LocoDialogueMaxLabel.Visible := True;
              END;
            END;
          MPH20:
            BEGIN
              LocoDialogueLocoSpeedInMPH := MPH30;
              IF Loco_MaximumSpeedInMPH = MPH30 THEN BEGIN
                LocoDialogueUpButton.Enabled := False;
                LocoDialogueMaxLabel.Visible := True;
              END;
            END;
          MPH30:
            BEGIN
              LocoDialogueLocoSpeedInMPH := MPH40;
              IF Loco_MaximumSpeedInMPH = MPH40 THEN BEGIN
                LocoDialogueUpButton.Enabled := False;
                LocoDialogueMaxLabel.Visible := True;
              END;
            END;
          MPH40:
            BEGIN
              LocoDialogueLocoSpeedInMPH := MPH50;
              IF Loco_MaximumSpeedInMPH = MPH50 THEN BEGIN
                LocoDialogueUpButton.Enabled := False;
                LocoDialogueMaxLabel.Visible := True;
              END;
            END;
          MPH50:
            BEGIN
              LocoDialogueLocoSpeedInMPH := MPH60;
              IF Loco_MaximumSpeedInMPH = MPH60 THEN BEGIN
                LocoDialogueUpButton.Enabled := False;
                LocoDialogueMaxLabel.Visible := True;
              END;
            END;
          MPH60:
            BEGIN
              LocoDialogueLocoSpeedInMPH := MPH70;
              IF Loco_MaximumSpeedInMPH = MPH70 THEN BEGIN
                LocoDialogueUpButton.Enabled := False;
                LocoDialogueMaxLabel.Visible := True;
              END;
            END;
          MPH70:
            BEGIN
              LocoDialogueLocoSpeedInMPH := MPH80;
              IF Loco_MaximumSpeedInMPH = MPH80 THEN BEGIN
                LocoDialogueUpButton.Enabled := False;
                LocoDialogueMaxLabel.Visible := True;
              END;
            END;
          MPH80:
            BEGIN
              LocoDialogueLocoSpeedInMPH := MPH90;
              IF Loco_MaximumSpeedInMPH = MPH90 THEN BEGIN
                LocoDialogueUpButton.Enabled := False;
                LocoDialogueMaxLabel.Visible := True;
              END;
            END;
          MPH90:
            BEGIN
              LocoDialogueLocoSpeedInMPH := MPH100;
              IF Loco_MaximumSpeedInMPH = MPH100 THEN BEGIN
                LocoDialogueUpButton.Enabled := False;
                LocoDialogueMaxLabel.Visible := True;
              END;
            END;
          MPH100:
            BEGIN
              LocoDialogueLocoSpeedInMPH := MPH110;
              IF Loco_MaximumSpeedInMPH = MPH110 THEN BEGIN
                LocoDialogueUpButton.Enabled := False;
                LocoDialogueMaxLabel.Visible := True;
              END;
            END;
          MPH110:
            BEGIN
              LocoDialogueLocoSpeedInMPH := MPH120;
              IF Loco_MaximumSpeedInMPH = MPH120 THEN BEGIN
                LocoDialogueUpButton.Enabled := False;
                LocoDialogueMaxLabel.Visible := True;
              END;
            END;
          MPH120:
            BEGIN
              LocoDialogueLocoSpeedInMPH := MPH120;
              LocoDialogueUpButton.Enabled := False;
              LocoDialogueMaxLabel.Visible := True;
            END;
        END; {CASE}

        IF LocoDialogueLocoSpeedInMPH <> MPH0 THEN BEGIN
          LocoDialogueDownButton.Enabled := True;
          IF NOT LocoDialogueLeftButton.Down THEN
            LocoDialogueLeftButton.Enabled := False;
          IF NOT LocoDialogueRightButton.Down THEN
            LocoDialogueRightButton.Enabled := False;
        END;

        IF LocoDialogueLocoSpeedInMPH = MPH120 THEN
          LocoDialogueUpButton.Enabled := False;

        LocoDialogueSpeedDisplay.Caption := MPHToStr(LocoDialogueLocoSpeedInMPH);
        LocoDialogueSpeedDisplay.Font.Color := clBtnText;
      END; {WITH}
    END;
  END; {WITH}
END; { IncrementSpeedInMPH }

PROCEDURE DecrementSpeedInMPH;
{ Increase the speed in MPH and also check whether the maximum speed has been reached }
BEGIN
  WITH LocoDialogueWindow DO BEGIN
    CASE LocoDialogueLocoSpeedInMPH OF
      MPH0:
        BEGIN
          LocoDialogueLocoSpeedInMPH := MPH0;
          LocoDialogueMaxLabel.Visible := False;
        END;
      MPH10:
        BEGIN
          LocoDialogueLocoSpeedInMPH := MPH0;
          LocoDialogueMaxLabel.Visible := False;
        END;
      MPH20:
        BEGIN
          LocoDialogueLocoSpeedInMPH := MPH10;
          LocoDialogueMaxLabel.Visible := False;
        END;
      MPH30:
        BEGIN
          LocoDialogueLocoSpeedInMPH := MPH20;
          LocoDialogueMaxLabel.Visible := False;
        END;
      MPH40:
        BEGIN
          LocoDialogueLocoSpeedInMPH := MPH30;
          LocoDialogueMaxLabel.Visible := False;
        END;
      MPH50:
        BEGIN
          LocoDialogueLocoSpeedInMPH := MPH40;
          LocoDialogueMaxLabel.Visible := False;
        END;
      MPH60:
        BEGIN
          LocoDialogueLocoSpeedInMPH := MPH50;
          LocoDialogueMaxLabel.Visible := False;
        END;
      MPH70:
        BEGIN
          LocoDialogueLocoSpeedInMPH := MPH60;
          LocoDialogueMaxLabel.Visible := False;
        END;
      MPH80:
        BEGIN
          LocoDialogueLocoSpeedInMPH := MPH70;
          LocoDialogueMaxLabel.Visible := False;
        END;
      MPH90:
        BEGIN
          LocoDialogueLocoSpeedInMPH := MPH80;
          LocoDialogueMaxLabel.Visible := False;
        END;
      MPH100:
        BEGIN
          LocoDialogueLocoSpeedInMPH := MPH90;
          LocoDialogueMaxLabel.Visible := False;
        END;
      MPH110:
        BEGIN
          LocoDialogueLocoSpeedInMPH := MPH100;
          LocoDialogueMaxLabel.Visible := False;
        END;
      MPH120:
        BEGIN
          LocoDialogueLocoSpeedInMPH := MPH110;
          LocoDialogueMaxLabel.Visible := False;
        END;
    END; {CASE}

    IF LocoDialogueLocoSpeedInMPH <> MPH0 THEN BEGIN
      LocoDialogueUpButton.Enabled := True;
      IF NOT LocoDialogueLeftButton.Down THEN
        LocoDialogueLeftButton.Enabled := False;
      IF NOT LocoDialogueRightButton.Down THEN
        LocoDialogueRightButton.Enabled := False;
    END;

    IF LocoDialogueLocoSpeedInMPH = MPH0 THEN BEGIN
      LocoDialogueDownButton.Enabled := False;
      LocoDialogueLeftButton.Enabled := True;
      LocoDialogueRightButton.Enabled := True;
    END;

    LocoDialogueSpeedDisplay.Caption := MPHToStr(LocoDialogueLocoSpeedInMPH);
    LocoDialogueSpeedDisplay.Font.Color := clBtnText;
  END; {WITH}
END; { DecrementSpeedInMPH }

PROCEDURE ControlSpeedByMouseWheel(WheelDelta: Integer; MousePos: TPoint);
{ Adjust speed by means of the mouse wheel - slow down the reading, though }
VAR
  OK : Boolean;
  SaveLocoSpeed : Integer;

BEGIN
  WITH LocoDialogueWindow DO BEGIN
    { Only allow mouse speed changes if the loco-speed buttons are enabled }
    IF LocoDialogueLocoIndexFound
    AND (LocoDialogueUpButton.Enabled OR LocoDialogueDownButton.Enabled)
    THEN BEGIN
      IF LocoDialogueSpeedInMPH THEN BEGIN
        SaveLocoSpeed := LocoDialogueLocoSpeed;

        { If there's a small movement of the wheel (usually WheelDelta = 120 or -120), reduce the amount of speed increase/decrease, or the loco speeds up or slows down
          too quickly.
        }
        IF (WheelDelta > -240) AND (WheelDelta < 240) THEN
          IF MilliSecondsBetween(Time, SaveWheelTime) < 200 THEN
            Exit;

        SaveWheelTime := Time;

        IF (WheelDelta > 0) AND (LocoDialogueUpButton.Enabled) THEN
          IncrementSpeedInMPH
        ELSE
          IF (WheelDelta < 0) AND (LocoDialogueDownButton.Enabled) THEN
            DecrementSpeedInMPH;

        IF SaveLocoSpeed <> LocoDialogueLocoSpeed THEN BEGIN
          WITH Locos[LocoDialogueLocoIndex] DO
            SetLenzSpeedAndDirection(Locos[LocoDialogueLocoIndex], LocoDialogueLocoSpeed, Loco_CurrentDirection, OK);
          IF LocoDialogueDoubleHeaderLocoIndexFound THEN
            WITH Locos[LocoDialogueDoubleHeaderLocoIndex] DO
              SetLenzSpeedAndDirection(Locos[LocoDialogueDoubleHeaderLocoIndex], LocoDialogueLocoSpeed, Loco_CurrentDirection, OK);
        END;
      END ELSE BEGIN
        SaveLocoSpeed := LocoDialogueLocoSpeed;

        { If there's a small movement of the wheel (usually WheelDelta = 120 or -120), reduce the amount of speed increase/decrease, or the loco speeds up or slows down
          too quickly.
        }
        IF (WheelDelta > -240) AND (WheelDelta < 240) THEN
          IF MilliSecondsBetween(Time, SaveWheelTime) < 200 THEN
            Exit;

        SaveWheelTime := Time;

        IF WheelDelta > 0 THEN
          LocoDialogueLocoSpeed := LocoDialogueLocoSpeed + 1
        ELSE
          IF WheelDelta < 0 THEN
            LocoDialogueLocoSpeed := LocoDialogueLocoSpeed - 1;

        IF LocoDialogueLocoSpeed < 0 THEN BEGIN
          LocoDialogueLocoSpeed := 0

        END ELSE
          IF LocoDialogueLocoSpeed > 28 THEN
            LocoDialogueLocoSpeed := 28;

        IF SaveLocoSpeed <> LocoDialogueLocoSpeed THEN BEGIN
          WITH Locos[LocoDialogueLocoIndex] DO
            SetLenzSpeedAndDirection(Locos[LocoDialogueLocoIndex], LocoDialogueLocoSpeed, Loco_CurrentDirection, OK);
          IF LocoDialogueDoubleHeaderLocoIndexFound THEN
            WITH Locos[LocoDialogueDoubleHeaderLocoIndex] DO
              SetLenzSpeedAndDirection(Locos[LocoDialogueDoubleHeaderLocoIndex], LocoDialogueLocoSpeed, Loco_CurrentDirection, OK);
        END;

        IF OK THEN BEGIN
          IF LocoDialogueLocoSpeed = 0 THEN BEGIN
            LocoDialogueWindow.LocoDialogueUpButton.Enabled := True;
            LocoDialogueWindow.LocoDialogueDownButton.Enabled := False;
            LocoDialogueWindow.LocoDialogueDownButton.Enabled := False;
            LocoDialogueWindow.LocoDialogueLeftButton.Enabled := True;
            LocoDialogueWindow.LocoDialogueRightButton.Enabled := True;
          END ELSE
            IF LocoDialogueLocoSpeed > 0 THEN BEGIN
              LocoDialogueDownButton.Enabled := True;
              IF NOT LocoDialogueLeftButton.Down THEN
                LocoDialogueLeftButton.Enabled := False;
              IF NOT LocoDialogueRightButton.Down THEN
                LocoDialogueRightButton.Enabled := False;

              IF LocoDialogueLocoSpeed = 28 THEN
                LocoDialogueUpButton.Enabled := False;
            END;

          LocoDialogueWindow.LocoDialogueSpeedDisplay.Caption := IntToStr(LocoDialogueLocoSpeed);
        END;
      END;
    END;
  END; { WITH}
END; { ControlSpeedByMouseWheel }

PROCEDURE TLocoDialogueWindow.LocoDialogueWindowMouseWheel(Sender: TObject; ShiftState: TShiftState; WheelDelta: Integer; MousePos: TPoint; VAR Handled: Boolean);
BEGIN
  ControlSpeedByMouseWheel(WheelDelta, MousePos);
END; { LocoDialogueMouseWheel }

PROCEDURE TLocoDialogueWindow.LocoDialogueWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  CASE Key OF
    vk_Escape:
      BEGIN
        EmergencyStopInLocoDialogue;
        LocoDialogueWindow.Hide;
      END;

    Ord('8'): { the star key, which inserts a line of stars in the log }
      IF ssShift IN ShiftState THEN
        KeyPressedDown(Key, ShiftState);
    Ord('V'):
      IF ssCtrl IN ShiftState THEN
        { this sent by the mouse wheel being pressed - the mouse wheel can only send certain characters }
        KeyPressedDown(Key, ShiftState);

    vk_Insert, vk_Multiply: { the star key, which inserts a line of stars in the log }
      KeyPressedDown(Key, ShiftState);
  END; {CASE}

  LocoDialogueShiftState := ShiftState;
END; { LocoDialogueKeyDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueWindowKeyUp(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  LocoDialogueShiftState := [];
END; { LocoDialogueKeyUp }

PROCEDURE TLocoDialogueWindow.LocoDialogueTimerTick(Sender: TObject);
{ If the loco is taken over, update the Dialogue box }
BEGIN
  IF SystemOnline THEN BEGIN { needs to do something if we're offline ****** }
    IF LocoDialogueLocoIndexFound THEN BEGIN
      WITH Locos[LocoDialogueLocoIndex] DO BEGIN
        IF (Loco_ControlState = ControlledByUser) AND (Loco_PreviousControlState <> ControlledByUser) THEN
          LocoDialogueSpeedDisplay.Font.Color := clBtnText;

        IF (Loco_ControlState = ControlledByUser) THEN BEGIN
          { update the speed info }
          LocoDialogueLocoSpeed := GetLenzSpeed(Locos[LocoDialogueLocoIndex], ForceARead);
          LocoDialogueSpeedDisplay.Caption := IntToStr(LocoDialogueLocoSpeed);
          LocoDialogueSpeedDisplay.Font.Color := clBtnShadow;

          { Update the direction info }
          IF LocoDialogueLocoSpeed = 28 THEN
            LocoDialogueUpButton.Enabled := False
          ELSE
            IF LocoDialogueLocoSpeed = 0 THEN BEGIN
              LocoDialogueDownButton.Enabled := False;

              LocoDialogueRightButton.Enabled := True;
              LocoDialogueLeftButton.Enabled := True;

              IF Locos[LocoDialogueLocoIndex].Loco_CurrentDirection = Up THEN BEGIN
                LocoDialogueLeftButton.Down := True;
                LocoDialogueRightButton.Down := False;
              END ELSE BEGIN
                LocoDialogueLeftButton.Down := False;
                LocoDialogueRightButton.Down := True;
              END;
            END ELSE BEGIN
              LocoDialogueUpButton.Enabled := True;
              LocoDialogueDownButton.Enabled := True;

              LocoDialogueRightButton.Enabled := False;
              LocoDialogueLeftButton.Enabled := False;

              IF Locos[LocoDialogueLocoIndex].Loco_CurrentDirection = Up THEN BEGIN
                LocoDialogueLeftButton.Down := True;
                LocoDialogueRightButton.Down := False;
              END ELSE BEGIN
                LocoDialogueLeftButton.Down := False;
                LocoDialogueRightButton.Down := True;
              END;
            END;

          { and the lights and function info }
          LocoDialogueReturnFunctionState;
        END;
      END; {WITH}
    END;
  END;
END; { LocoDialogueTimerTick }

PROCEDURE TLocoDialogueWindow.LocoDialogueWindowMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
{ May want to reset default position }
CONST
  StopTimer = True;

BEGIN
  IF Button = mbRight THEN BEGIN
    IF ssShift IN ShiftState THEN BEGIN
      IF MessageDialogueWithDefault('Reset dialogue box position to default?',
                                    NOT StopTimer, mtConfirmation, [mbOK, mbAbort], mbAbort) = mrOK
      THEN BEGIN
        LocoDialogueWindow.Hide;
        LocoDialogueWindowLeft := DefaultLocoDialogueWindowLeft;
        LocoDialogueWindowTop := DefaultLocoDialogueWindowTop;
        LocoDialogueWindow.Show;
      END;
    END ELSE
      { it's an emergency stop }
      EmergencyStopInLocoDialogue;
  END;
END; { LocoDialogueMouseDown }

PROCEDURE CheckEmergencyStop(Button: TMouseButton; ShiftState: TShiftState);
{ Sees if there's an emergency stop occasioned by the right mouse button being pressed }
BEGIN
  IF (Button = mbRight) AND (ShiftState = [ssRight]) THEN
    { it's an emergency stop }
    EmergencyStopInLocoDialogue;
END; { CheckEmergencyStop }

PROCEDURE TLocoDialogueWindow.LocoDialogueSpeedDisplayMouseDown(Sender: TObject; Button: TMouseButton; ShiftState : TShiftState; X, Y: Integer);
BEGIN
  CheckEmergencyStop(Button, ShiftState);
END; { LocoDialogueSpeedDisplayMouseDown }

PROCEDURE LocoDialogueIncreaseSpeed;
{ Increase a loco's speed by means of the Loco Dialogue Box. This routine is called by a mouse down event and also by MainWindowShortCut }
VAR
  OK : Boolean;
  TempDHLocoSpeed : Integer;

BEGIN
  WITH LocoDialogueWindow DO BEGIN
    IF LocoDialogueLocoIndexFound THEN BEGIN
      LocoTakenOver := False;
      LocoDialogueDownButton.Enabled := True;

      IF LocoDialogueSpeedInMPH THEN BEGIN
        IncrementSpeedInMPH;

        { Now convert the speed in MPH to the appropriate Lenz speed }
        LocoDialogueLocoSpeed := SpeedInMPHToLocoLenzSpeed(LocoDialogueLocoIndex, LocoDialogueLocoSpeedInMPH);

        { Now we have to separate out the two locos, as they may well have different Lenz speeds }
        WITH Locos[LocoDialogueLocoIndex] DO
          SetLenzSpeedAndDirection(Locos[LocoDialogueLocoIndex], LocoDialogueLocoSpeed, Loco_CurrentDirection, OK);
        IF LocoDialogueDoubleHeaderLocoIndexFound THEN BEGIN
          TempDHLocoSpeed := SpeedInMPHToLocoLenzSpeed(LocoDialogueDoubleHeaderLocoIndex, LocoDialogueLocoSpeedInMPH);
          WITH Locos[LocoDialogueDoubleHeaderLocoIndex] DO
            SetLenzSpeedAndDirection(Locos[LocoDialogueDoubleHeaderLocoIndex], TempDHLocoSpeed, Loco_CurrentDirection, OK);
        END;
      END ELSE BEGIN
        Inc(LocoDialogueLocoSpeed);
        WITH Locos[LocoDialogueLocoIndex] DO
        SetLenzSpeedAndDirection(Locos[LocoDialogueLocoIndex], LocoDialogueLocoSpeed, Loco_CurrentDirection, OK);
        IF LocoDialogueDoubleHeaderLocoIndexFound THEN
          WITH Locos[LocoDialogueDoubleHeaderLocoIndex] DO
            SetLenzSpeedAndDirection(Locos[LocoDialogueDoubleHeaderLocoIndex], LocoDialogueLocoSpeed, Loco_CurrentDirection, OK);
      END;

      IF LocoDialogueLocoSpeed > 0 THEN BEGIN
        LocoDialogueDownButton.Enabled := True;
        IF NOT LocoDialogueLeftButton.Down THEN
          LocoDialogueLeftButton.Enabled := False;
        IF NOT LocoDialogueRightButton.Down THEN
          LocoDialogueRightButton.Enabled := False;

        IF LocoDialogueLocoSpeed = 28 THEN BEGIN
          LocoDialogueUpButton.Enabled := False;
          LocoDialogueMouseDownTimer.Enabled := False;
        END;
      END;

      LocoDialogueSpeedDisplay.Caption := IntToStr(LocoDialogueLocoSpeed);
      LocoDialogueSpeedDisplay.Font.Color := clBtnText;
    END;
  END; {WITH}
END; { LocoDialogueIncreaseSpeed }

PROCEDURE TLocoDialogueWindow.LocoDialogueUpButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
BEGIN
  CheckEmergencyStop(Button, ShiftState);

  LocoDialogueMouseDownTimer.Enabled := True;
  LocoDialogueMouseDownTime := Time;
  LocoDialogueMouseDownTimer.Interval := 1000;
  LocoDialogueIncreaseSpeedFlag := True;
  LocoDialogueIncreaseSpeed;
END;{ LocoDialogueUpButtonMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueUpButtonMouseLeave(Sender: TObject);
BEGIN
  LocoDialogueMouseDownTimer.Enabled := False;
END; { LocoDialogueUpButtonMouseLeave }

PROCEDURE TLocoDialogueWindow.LocoDialogueUpButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
BEGIN
  LocoDialogueMouseDownTimer.Enabled := False;
END; { LocoDialogueUpButtonMouseUp }

PROCEDURE TLocoDialogueWindow.LocoDialogueMouseDownTimerTick(Sender: TObject);
BEGIN
  IF (MilliSecondsBetween(Time, LocoDialogueMouseDownTime) >= 2000) AND (MilliSecondsBetween(Time, LocoDialogueMouseDownTime) < 4000) THEN
    LocoDialogueMouseDownTimer.Interval := 500;

  IF MilliSecondsBetween(Time, LocoDialogueMouseDownTime) >= 4000 THEN
    LocoDialogueMouseDownTimer.Interval := 250;

  IF LocoDialogueIncreaseSpeedFlag THEN
    LocoDialogueIncreaseSpeed
  ELSE
    LocoDialogueDecreaseSpeed;
END; { MouseDownTimerTick }

PROCEDURE LocoDialogueDecreaseSpeed;
{ Decrease a loco's speed by means of the Loco Dialogue Box. This routine is called by a mouse down event and also by MainWindowShortCut }
VAR
  OK : Boolean;
  TempDHLocoSpeed : Integer;

BEGIN
  WITH LocoDialogueWindow DO BEGIN
  IF LocoDialogueLocoIndexFound THEN BEGIN
      LocoTakenOver := False;
      LocoDialogueUpButton.Enabled := True;

      IF LocoDialogueSpeedInMPH THEN BEGIN
        DecrementSpeedInMPH;

        { Now convert the speed in MPH to the appropriate Lenz speed }
        LocoDialogueLocoSpeed := SpeedInMPHToLocoLenzSpeed(LocoDialogueLocoIndex, LocoDialogueLocoSpeedInMPH);

        { Now we have to separate out the two locos, as they may well have different Lenz speeds }
        WITH Locos[LocoDialogueLocoIndex] DO
          SetLenzSpeedAndDirection(Locos[LocoDialogueLocoIndex], LocoDialogueLocoSpeed, Loco_CurrentDirection, OK);
        IF LocoDialogueDoubleHeaderLocoIndexFound THEN BEGIN
          TempDHLocoSpeed := SpeedInMPHToLocoLenzSpeed(LocoDialogueDoubleHeaderLocoIndex, LocoDialogueLocoSpeedInMPH);
          WITH Locos[LocoDialogueDoubleHeaderLocoIndex] DO
            SetLenzSpeedAndDirection(Locos[LocoDialogueDoubleHeaderLocoIndex], TempDHLocoSpeed, Loco_CurrentDirection, OK);
        END;
      END ELSE BEGIN
        IF LocoDialogueLocoSpeed > 0 THEN BEGIN
          Dec(LocoDialogueLocoSpeed);
          WITH Locos[LocoDialogueLocoIndex] DO
            SetLenzSpeedAndDirection(Locos[LocoDialogueLocoIndex], LocoDialogueLocoSpeed, Loco_CurrentDirection, OK);
          IF LocoDialogueDoubleHeaderLocoIndexFound THEN
            WITH Locos[LocoDialogueDoubleHeaderLocoIndex] DO
              SetLenzSpeedAndDirection(Locos[LocoDialogueDoubleHeaderLocoIndex], LocoDialogueLocoSpeed, Loco_CurrentDirection, OK);
        END;
      END;

      IF LocoDialogueLocoSpeed <= 0 THEN BEGIN
        LocoDialogueDownButton.Enabled := False;
        LocoDialogueLeftButton.Enabled := True;
        LocoDialogueRightButton.Enabled := True;
      END;

      IF LocoDialogueLocoSpeed > 0 THEN BEGIN
        LocoDialogueDownButton.Enabled := True;
        IF NOT LocoDialogueLeftButton.Down THEN
          LocoDialogueLeftButton.Enabled := False;
        IF NOT LocoDialogueRightButton.Down THEN
          LocoDialogueRightButton.Enabled := False;
      END;

      LocoDialogueSpeedDisplay.Caption := IntToStr(LocoDialogueLocoSpeed);
      LocoDialogueSpeedDisplay.Font.Color := clBtnText;
    END;
  END; {WITH}
END; { LocoDialogueDecreaseSpeed }

PROCEDURE TLocoDialogueWindow.LocoDialogueDownButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState : TShiftState; X, Y: Integer);
BEGIN
  CheckEmergencyStop(Button, ShiftState);

  LocoDialogueMouseDownTimer.Enabled := True;
  LocoDialogueMouseDownTime := Time;
  LocoDialogueMouseDownTimer.Interval := 1000;
  LocoDialogueIncreaseSpeedFlag := False;
  LocoDialogueDecreaseSpeed;
END; { LocoDialogueDownButtonMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueDownButtonMouseLeave(Sender: TObject);
BEGIN
  LocoDialogueMouseDownTimer.Enabled := False;
END; { LocoDialogueDownButtonMouseLeave }

PROCEDURE TLocoDialogueWindow.LocoDialogueDownButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
BEGIN
  LocoDialogueMouseDownTimer.Enabled := False;
END; { LocoDialogueDownButtonMouseUp }

PROCEDURE TLocoDialogueWindow.LocoDialogueLeftButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
{ Switch the direction to Up }
VAR
  OK : Boolean;

BEGIN
  CheckEmergencyStop(Button, ShiftState);

  LocoTakenOver := False;

  IF LocoDialogueLocoIndexFound THEN BEGIN
    WITH Locos[LocoDialogueLocoIndex] DO BEGIN
      IF Loco_CurrentDirection = Down THEN BEGIN
        LocoDialogueLeftButton.Down := True;

        SetLocoDirection(Locos[LocoDialogueLocoIndex], Up, OK);
        IF NOT OK THEN
          Log(Loco_LocoChipStr + ' LG Could not change direction to up')
        ELSE
          IF Loco_LightsType = LightsOperatedByTwoChips THEN
            SetTwoLightingChips(LocoDialogueLocoIndex, Up, Up, Loco_LightsOn);

        LocoDialogueEmergencyStopButton.SetFocus;
      END;
    END; {WITH}
  END;
END; { LocoDialogueLeftButtonMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueRightButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState : TShiftState; X, Y: Integer);
{ Switch the direction to Down }
VAR
  OK : Boolean;

BEGIN
  CheckEmergencyStop(Button, ShiftState);

  LocoTakenOver := False;

  IF LocoDialogueLocoIndexFound THEN BEGIN
    WITH Locos[LocoDialogueLocoIndex] DO BEGIN
      IF Loco_CurrentDirection = Up THEN BEGIN
        LocoDialogueRightButton.Down := True;

        SetLocoDirection(Locos[LocoDialogueLocoIndex], Down, OK);
        IF NOT OK THEN
          Log(Loco_LocoChipStr + ' LG Could not change direction to down')
        ELSE
          IF Loco_LightsType = LightsOperatedByTwoChips THEN
            SetTwoLightingChips(LocoDialogueLocoIndex, Down, Down, Loco_LightsOn);

        LocoDialogueEmergencyStopButton.SetFocus;
      END;
    END; {WITH}
  END;
END; { LocoDialogueRightButtonMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueTurnLightsOnOrOffButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
{ Turn lights on or off }
CONST
  TurnOn = True;
  LightsOn = True;

VAR
  OK : Boolean;
  UserMsg : String;

BEGIN
  CheckEmergencyStop(Button, ShiftState);

  LocoTakenOver := False;

  IF LocoDialogueLocoIndexFound THEN BEGIN
    WITH Locos[LocoDialogueLocoIndex] DO BEGIN
      IF Loco_LightsType = HeadlightsAndTailLightsConnected THEN BEGIN
        { Assume for the moment that all lights are operated by function 0 - will want to improve on this *** }
        IF LocoDialogueFunction0CheckBox.Checked OR Loco_LightsOn THEN BEGIN
          TurnLocoLightsOff(LocoDialogueLocoIndex, NOT UserMsgRequired, UserMsg, OK);
          LocoDialogueTurnLightsOnOrOffButton.Caption := 'Turn Lights On';
          LocoDialogueFunction0CheckBox.Checked := False;
        END ELSE BEGIN
          TurnLocoLightsOn(LocoDialogueLocoIndex, NOT NonMovingLoco, NOT LightLoco, NOT UserMsgRequired, UserMsg, OK);
          LocoDialogueTurnLightsOnOrOffButton.Caption := 'Turn Lights Off';
          LocoDialogueFunction0CheckBox.Checked := True;
        END;
      END ELSE
        IF Loco_LightsType = LightsOperatedByTwoChips THEN BEGIN
          { Assume for the moment that all lights are operated by function 0 - will want to improve on this *** }
          IF LocoDialogueFunction0CheckBox.Checked OR Loco_LightsOn THEN BEGIN
            SetTwoLightingChips(LocoDialogueLocoIndex, Up, Up, NOT LightsOn);
            LocoDialogueTurnLightsOnOrOffButton.Caption := 'Turn Lights On';
            LocoDialogueFunction0CheckBox.Checked := False;
          END ELSE BEGIN
            SetTwoLightingChips(LocoDialogueLocoIndex, Loco_CurrentDirection, Loco_CurrentDirection, LightsOn);
            LocoDialogueTurnLightsOnOrOffButton.Caption := 'Turn Lights Off';
            LocoDialogueFunction0CheckBox.Checked := True;
          END;
        END;
    END; {WITH}
  END;

  LocoDialogueEmergencyStopButton.SetFocus;
END; { LocoDialogueTurnLightsOnOrOffButtonMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueSpeedInMPHButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState : TShiftState; X, Y: Integer);

  PROCEDURE SwitchMPHSpeedsOff;
  BEGIN
    WITH LocoDialogueWindow DO BEGIN
      LocoDialogueSpeedInMPH := False;
      LocoDialogueSpeedInMPHButton.Caption := 'Switch to Speed In MPH';
      LocoDialogueMPHLabel.Visible := False;
    END; {WITH}
  END; { SwitchMPHSpeedsOff }

BEGIN
  CheckEmergencyStop(Button, ShiftState);

  LocoTakenOver := False;

  IF LocoDialogueLocoIndexFound THEN BEGIN
    WITH Locos[LocoDialogueLocoIndex] DO BEGIN
      IF GetLenzSpeed(Locos[LocoDialogueLocoIndex], ForceARead) = 0 THEN BEGIN
        IF LocoDialogueSpeedInMPH THEN
          SwitchMPHSpeedsOff
        ELSE
          SwitchMPHSpeedsOn;
      END;
    END; {WITH}
  END;
END; { LocoDialogueSpeedInMPHButtonMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueFunctionsLabelMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
BEGIN
  CheckEmergencyStop(Button, ShiftState);
END; { LocoDialogueFunctionsLabelMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueEmergencyStopButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
BEGIN
  CheckEmergencyStop(Button, ShiftState);

  LocoTakenOver := False;
  EmergencyStopInLocoDialogue;
END; { LocoDialogueEmergencyStopButtonMouseDown }

PROCEDURE LocoDialogueChangeOrSelectLoco;
{ Select a loco }
CONST
  ErrorMessageRequired = True;
  StartButton = True;
  Surround = True;
  UndrawRequired = True;
  UndrawToBeAutomatic = True;

BEGIN
  WITH LocoDialogueWindow DO BEGIN
    IF LocoDialogueLocoIndexFound THEN BEGIN
      IF LocoDialogueChangeOrSelectButton.Caption = 'Deselect &Loco 1' THEN BEGIN
        DisableLocoDialogueLocoButtonsAndBoxes;
        LocoDialogueWindow.Caption := 'Select Loco';
        LocoDialogueDHLocoMaskEdit.Clear;
        LocoDialogueDHLocoClearButton.Enabled := False;
      END ELSE BEGIN
        EnableLocoDialogueLocoButtonsAndBoxes;
        LocoDialogueWindow.Caption := 'Drive Loco';
      END;
    END;
  END; {WITH}
END; { LocoDialogueChangeOrSelectLoco }

PROCEDURE TLocoDialogueWindow.LocoDialogueChangeOrSelectButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
CONST
  ErrorMessageRequired = True;
  StartButton = True;
  Surround = True;
  UndrawRequired = True;
  UndrawToBeAutomatic = True;

BEGIN
  CheckEmergencyStop(Button, ShiftState);

  LocoDialogueChangeOrSelectLoco;
END; { LocoDialogueChangeOrSelectButtonMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueCancelButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
BEGIN
  CheckEmergencyStop(Button, ShiftState);

  EmergencyStopInLocoDialogue;
  LocoDialogueWindow.Hide;
END; { LocoDialogueCancelButtonMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueLocoQueryButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
BEGIN
  CheckEmergencyStop(Button, ShiftState);

  LocoUtilsWindow.Visible := True;
  LocoUtilsWindow.BringToFront;
  ListLocosByChip;
  Log('A List of locos made visible');

  { The tag is used to tell the LocoDialogue where the request is coming from, so it can write the loco number back to the correct place, i.e. the LocoNum or DHLLocoNum
    edit control.
  }
  LocoUtilsWindow.Tag := -1;
END; { LocoDialogueLocoQueryButtonMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueDHLocoChangeOrSelectButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
VAR
  I : Integer;
  DifferenceFound : Boolean;

BEGIN
  CheckEmergencyStop(Button, ShiftState);

  IF LocoDialogueLocoIndexFound AND LocoDialogueDoubleHeaderLocoIndexFound THEN BEGIN
    IF LocoDialogueDHLocoChangeOrSelectButton.Caption = 'Deselect Lo&co 2' THEN BEGIN
      { disable double heading items }
      LocoDialogueWindow.Caption := 'Select Loco';

      LocoDialogueDHLocoQueryButton.Enabled := False;
      IF LocoDialogueDHLocoMaskEdit.Text = '' THEN
        LocoDialogueDHLocoChangeOrSelectButton.Enabled := False
      ELSE
        LocoDialogueDHLocoClearButton.Enabled := True;
      LocoDialogueDHLocoChangeOrSelectButton.Caption := 'Select Lo&co 2';
      LocoDialogueDoubleHeaderLocoChip := UnknownLocoChip;

      LocoDialogueDHLocoMaskEdit.SetFocus;
    END ELSE BEGIN
      LocoDialogueDoubleHeaderLocoChip := Locos[LocoDialogueDoubleHeaderLocoIndex].Loco_LocoChip;
      LocoDialogueWindow.Caption := 'Drive Loco';

      { Enable double heading items }
      LocoDialogueDHLocoChangeOrSelectButton.Enabled := True;
      LocoDialogueDHLocoChangeOrSelectButton.Caption := 'Deselect Lo&co 2';

      { And the other items which were temporarily disabled }
      EnableFunctionBoxes;
      LocoDialogueChangeOrSelectButton.Enabled := True;
      LocoDialogueUpButton.Enabled := True;
      LocoDialogueDownButton.Enabled := True;
      LocoDialogueLeftButton.Enabled := True;
      LocoDialogueRightButton.Enabled := True;
      LocoDialogueSpeedDisplay.Font.Color := clBtnText;
      LocoDialogueTurnLightsOnOrOffButton.Enabled := True;
      LocoDialogueSpeedInMPHButton.Enabled := True;

      IF Locos[LocoDialogueDoubleHeaderLocoIndex].Loco_SpeedSettingsMissing THEN
        { can't allow double heading where we don't know that the two locos have compatible speed settings }
        LocoDialogueSpeedInMPHButton.Enabled := False
      ELSE BEGIN
        { but if the speed settings are different for the two locos, we have to use MPH speeds as running the locos with different Lenz speed settings will cause problems }
        I := 1;
        DifferenceFound := False;
        WHILE (I <= 12) AND NOT DifferenceFound DO BEGIN
          IF Locos[LocoDialogueDoubleHeaderLocoIndex].Loco_SpeedArray[I] <> Locos[LocoDialogueLocoIndex].Loco_SpeedArray[I] THEN
            DifferenceFound := True;
          Inc(I);
        END; {WHILE}

        IF NOT DifferenceFound THEN
          LocoDialogueSpeedInMPHButton.Enabled := True
        ELSE BEGIN
          SwitchMPHSpeedsOn;
          { but stop MPH speeds from being turned off }
          LocoDialogueSpeedInMPHButton.Enabled := False;
        END;
      END;

      LocoDialogueEmergencyStopButton.SetFocus;
    END;
  END;
END; { LocoDialogueDHLocoChangeOrSelectButtonMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueDHLocoQueryButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
BEGIN
  CheckEmergencyStop(Button, ShiftState);

  LocoUtilsWindow.Visible := True;
  LocoUtilsWindow.BringToFront;
  ListLocosByChip;
  Log('A List of locos made visible');

  { The tag is used to tell the LocoDialogue where the request is coming from, so it can write the loco number back to the correct place, i.e. the LocoNum or DHLLocoNum
    edit control.
  }
  LocoUtilsWindow.Tag := 1;
END; { LocoDialogueDHLocoQueryButtonMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueDHLocoClearButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
BEGIN
  CheckEmergencyStop(Button, ShiftState);

  LocoDialogueDHLocoMaskEdit.Clear;
  LocoDialogueDHLocoClearButton.Enabled := False;

  IF LocoDialogueLocoIndexFound THEN BEGIN
    IF NOT LocoDialogueSpeedInMPHButton.Enabled THEN
      { it may be disabled because the double-header loco doesn't have any speed settings }
      IF NOT Locos[LocoDialogueLocoIndex].Loco_SpeedSettingsMissing THEN
        LocoDialogueSpeedInMPHButton.Enabled := True;
  END;
END; { LocoDialogueDHLocoClearButtonMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueLocoTimerStartStopButtonMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
BEGIN
  CheckEmergencyStop(Button, ShiftState);

  LocoTakenOver := False;

  IF LocoDialogueLocoIndexFound THEN BEGIN
    IF LocoDialogueLocoTimerStartStopButton.Caption = 'Start Loco Timer' THEN BEGIN
      Log(LocoChipToStr(LocoDialogueLocoChip) + ' *G Loco speed test initiated');
      InitialiseLocoSpeedTiming(LocoDialogueLocoIndex);
      LocoDialogueLocoTimerStartStopButton.Caption := 'Stop Loco Timer';
      LocoDialogueSpeedDisplay.Color := clRed;
    END ELSE BEGIN
      LocoDialogueLocoTimerStartStopButton.Caption := 'Start Loco Timer';
      Log(LocoChipToStr(LocoDialogueLocoChip) + ' *G Loco speed test completed');
      LocoSpeedTimingMode := False;
      LocoDialogueSpeedDisplay.Color := clBtnFace;
    END;

    LocoDialogueEmergencyStopButton.SetFocus;
  END;
END; { LocoDialogueLocoTimerStartStopButtonMouseDown }

PROCEDURE TLocoDialogueWindow.LocoDialogueDHLocoMaskEditLabelMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
BEGIN
  CheckEmergencyStop(Button, ShiftState);
END; { LocoDialogueDHLocoMaskEditLabelMouseDown }

PROCEDURE InitialiseLocoDialogueUnit;
{ Initialises the unit }
BEGIN
  IF LocoDialogueSpeedInMPH THEN BEGIN
    LocoDialogueWindow.LocoDialogueSpeedInMPHButton.Caption := 'Switch To Speed Steps';
    LocoDialogueWindow.LocoDialogueMPHLabel.Visible := True;
  END ELSE BEGIN
    LocoDialogueWindow.LocoDialogueSpeedInMPHButton.Caption := 'Switch To Speed In MPH';
    LocoDialogueWindow.LocoDialogueMPHLabel.Visible := False;
  END;
END; { InitialiseLocoDialogueUnit }

PROCEDURE TLocoDialogueWindow.LocoDialogueWindowClose(Sender: TObject; VAR Action: TCloseAction);
BEGIN
  EmergencyStopInLocoDialogue;
END; { LocoDialogueClose }

END { LocoDialogue }.

