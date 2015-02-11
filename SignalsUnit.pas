UNIT SignalsUnit;
{ Controls the various signals and signal-related things

  Copyright © F.W. Pritchard 2015. All Rights Reserved.

  v0.1  02/02/15 Code mainly extracted from Main and Locks
}
INTERFACE

USES
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, InitVars, Train;

TYPE
  TSignalUnitForm = CLASS(TForm)
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

  AspectType = (DoubleYellowAspect, FlashingDoubleYellowAspect, GreenAspect, SingleYellowAspect, FlashingSingleYellowAspect, RedAspect, NoAspect, UnknownAspectType);
  TypeOfSignal = (CallingOn, TwoAspect, ThreeAspect, FourAspect, SemaphoreHome, SemaphoreDistant, UnknownSignalType);
  IndicatorType = (NoIndicator, JunctionIndicator, TheatreIndicator, QueryIndicator, UnknownIndicator);
  IndicatorStateType = (NoIndicatorLit, LeftIndicatorLit, RightIndicatorLit, UpperLeftIndicatorLit, MiddleLeftIndicatorLit, LowerLeftIndicatorLit, UpperRightIndicatorLit,
                        MiddleRightIndicatorLit, LowerRightIndicatorLit, TheatreIndicatorLit, QueryIndicatorLit);
  SignalStateType = (SignalOff, SignalOn, UnknownSignalState);

  JunctionIndicatorType = (UpperLeftIndicator, MiddleLeftIndicator, LowerLeftIndicator, UpperRightIndicator, MiddleRightIndicator, LowerRightIndicator,
                           UnknownJunctionIndicator);
  JunctionIndicatorRec = RECORD
    JunctionIndicator_Exists : Boolean;
    JunctionIndicator_TargetSignal : Integer;
    JunctionIndicator_TargetBufferStop : Integer;
    JunctionIndicator_MouseRect : TRect;
  END;

  QuadrantType = (UpperQuadrant, LowerQuadrant, NoQuadrant);

PROCEDURE AddNewRecordToSignalDatabase;
{ Append a record to the signal database }

PROCEDURE CalculateAllSignalPositions;
{ Work out where all the signals are on the screen }

PROCEDURE CalculateSignalPosition(S : Integer);
{ Work out where a signal is on the screen }

PROCEDURE CheckSemaphoreDistantBeforeSemaphoreHomeCleared(S : Integer);
{ Sees if semaphore distants need automatically to be reset to allow a semaphore home to be put on }

PROCEDURE ClearHiddenStationSignalAspectSignals(T : TrainIndex; HiddenStationSignalAspectSignal : Integer);
{ Clear the hidden station aspects of a given signal }

FUNCTION DeleteRecordFromSignalDatabase(SignalToDeleteNum : Integer) : Boolean;
{ Remove a record from the signal database }

FUNCTION GetLineAdjacentSignal(Line : Integer) : Integer;
{ Return the signal nearest the line }

FUNCTION GetSignalAdjacentLine(S : Integer) : Integer;
{ Return the line adjacent to the given signal }

FUNCTION GetSignalAspect(S : Integer) : AspectType;
{ Return the state of a signal }

PROCEDURE InitialiseSignalVariables(S : Integer);
{ Initialise all the variables where the data is not read in from the database or added during the edit process }

FUNCTION IsSignalInStringArray(StringArray : StringArrayType; Signal : Integer) : Boolean;
{ Returns whether the given signal is found in a string array }

FUNCTION JunctionIndicatorLit(S : Integer) : Boolean;
{ Return true if a junction indicator is lit }

PROCEDURE PullSignal{1}(LocoChipStr: String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        SettingString : String; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal }

PROCEDURE PullSignal{2}(LocoChipStr: String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        TrainTypeForRouteing : TypeOfTrainType; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal }

PROCEDURE PullSignal{3}(LocoChipStr: String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        ResetTC : Integer; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal. This version is only used by signals resetting track circuits, which can happen even if the signal is locked by a route,}

PROCEDURE PullSignal{4}(LocoChipStr: String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        SettingString : String; TrainTypeForRouteing : TypeOfTrainType; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal; includes the original setting string for saving if necessary }

PROCEDURE ReadInSignalDataFromDatabase(NewSignalData : Boolean);
{ Create entries for the signals }

PROCEDURE RestoreAllSignalsToPreviousState;
{ Sets all off signals to how they were before short circuit }

PROCEDURE SaveSignalsCurrentState;
{ Save all the previous state of all signals }

PROCEDURE SetAllSignalsToDanger;
{ Sets all off signals to on }

PROCEDURE SetIndicator(LocoChipStr : String; S : Integer; IndicatorState : IndicatorStateType; TheatreIndicatorString : String; Route : Integer; User : Boolean);
{ Turn the indicator of a particular signal on or off }

PROCEDURE SetHiddenStationSignalAspectSignals(T : TrainIndex; HiddenStationSignalAspectSignal, Journey, Route : Integer);
{ Find and set current and previous hidden station aspects }

PROCEDURE SetPreviousSignals(LocoChipStr : String; S : Integer);
{ Sees what previous signals are set to, and resets aspects accordingly. PreviousSignal1 is the nearer and PreviousSignal2 the further }

PROCEDURE SetSignal(LocoChipStr : String; S : Integer; NewAspect : AspectType; LogSignalData, ForceWriting : Boolean);
{ Set the state of a particular signal and draws it }

FUNCTION SignalAdjacentLineOK(Line : Integer) : Boolean;
{ Returns true if a signal can be created next to the current line }

FUNCTION SignalHasLeftJunctionIndicator(S : Integer; OUT Indicator : JunctionIndicatorType) : Boolean;
{ Returns true if the signal has a left junction indicator }

FUNCTION SignalHasRightJunctionIndicator(S : Integer; OUT Indicator : JunctionIndicatorType) : Boolean;
{ Returns true if the signal has a right junction indicator }

FUNCTION SignalIsLocked(S : Integer; OUT LockingMsg : String) : Boolean;
{ Returns true if the signal is locked }

FUNCTION SignalIsLockedByAnyRoute(S : Integer; OUT RouteLockingArray : IntegerArrayType) : Boolean;
{ Returns true if the signal is locked by any route (in which case the routes doing the locking are returned in RouteLockingArray) }

FUNCTION SignalIsLockedByOppositePassingLoopSignal(S : Integer; OUT OtherS : Integer) : Boolean;
{ Returns true if an opposite passing loop signal is off }

FUNCTION SignalIsLockedBySpecificRoute(S, Route : Integer) : Boolean;
{ Returns true if the signal is locked by the given route, unless Route is -1, when it returns true if the signal is locked by any route (in which case the routes doing the
  locking are returned in RouteLockingArray).
}
PROCEDURE TurnAllSignalsOff;
{ Turn off the LEDs in the signals }

PROCEDURE UnlockSignalLockedBySpecificRoute(S, Route : Integer);
{ Remove the locking }

PROCEDURE UnlockSignalLockedByUser(S : Integer);
{ Remove the locking }

FUNCTION ValidateNextSignalIfNoIndicator(Str : String; Init : Boolean; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns what the other signal is if no indicator is lit }

FUNCTION ValidateSignalAccessoryAddress(Str : String; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder address for a TrainTech S3 accessory decoder. This test must be done after Signal Type is validated. }

FUNCTION ValidateSignalAdjacentLine(SignalBeingValidated : Integer; Str : String; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the line next to a signal }

FUNCTION ValidateSignalAdjacentLineXOffset(Str : String; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns whether a signal's on screen adjustment if any }

FUNCTION ValidateSignalApproachControlAspect(Str : String; OUT ErrorMsg : String) : AspectType;
{ Validates and if ok returns the approach control signal aspect }

FUNCTION ValidateSignalAsTheatreDestination(Str : String; OUT ErrorMsg : String) : String;
{ Validates and if ok returns the one or two character display used in a theatre indicator }

FUNCTION ValidateSignalDecoderNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for a signal decoder. This check must be done after Accessory Address and Signal Type are validated. }

FUNCTION ValidateSignalDirection(Str : String; OUT ErrorMsg : String) : DirectionType;
{ Validates and if ok returns the signal direction }

FUNCTION ValidateSignalIndicator(Str : String; OUT ErrorMsg : String) : IndicatorType;
{ Validates and if ok returns the signal indicator }

FUNCTION ValidateSignalIndicatorDecoderFunctionNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for an indicator function decoder. This check must be done after Accessory Address and Signal Type are validated. }

FUNCTION ValidateSignalIndicatorDecoderNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for an indicator decoder. This check must be done after Accessory Address and Signal Type are validated. }

FUNCTION ValidateSignalDistantHomesArray(S : Integer; Signal_Type : TypeOfSignal; Str : String; OUT ErrorMsg : String) : IntegerArrayType;
{ Validates the home signal numbers supplied }

FUNCTION ValidateSignalIndicatorSpeedRestriction(Str : String; Indicator : IndicatorType; OUT ErrorMsg : String) : MPHType;
{ Validates and if ok returns what the tempoarary speed restriction is. This test must be carried outr after Indicator is validated. }

FUNCTION ValidateSignalJunctionIndicators1(Str, FieldName : String; Signal_Indicator : IndicatorType; OUT ErrorMsg : String) : JunctionIndicatorRec;
{ The first part of verifying whether junction indicators are correctly set up; this part also returns the values for each junction indicator. This test requires that
  the indicator has been validated first
}
PROCEDURE ValidateSignalJunctionIndicators2(StrArray : ARRAY OF String; SignalIndicator : IndicatorType; SignalJunctionIndicators : ARRAY OF JunctionIndicatorRec;
                                            OUT ErrorMsg : String);
{ The second part of verifying whether junction indictaors are correctly set up }

FUNCTION ValidateSignalLocationsToMonitorArray(Str : String; PossibleRouteHold : Boolean; OUT ErrorMsg : String) : IntegerArrayType;
{ Validates and if ok returns the signal locations monitored when a route is held. This test must be done after Possible Route Hold is validated. }

FUNCTION ValidateSignalNum(SignalToTest : Integer) : String;
{ Validates a signal number. This has to be carried out separately from other validation, as when creating signals a reference may be made to a signal not yet created }

FUNCTION ValidateSignalOppositePassingLoopSignal(Str : String; Init : Boolean; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the other signal involved in a passing loop }

FUNCTION ValidateSignalOutOfUseAndAddAdjacentTC(Flag : Boolean; AdjacentLine : Integer; OUT AdjacentTC : Integer; OUT ErrorMsg : String) : Boolean;
{ Validates and if ok returns true if a signal is marked as not in use. This test must be done after Adjacent Signal is validated. }

FUNCTION ValidateSignalPossibleStationStartRouteHold(Flag : Boolean; PossibleRouteHold : Boolean; OUT ErrorMsg : String) : Boolean;
{ Validates and if ok returns whether a station start route hold is in operation. This test must be done after Possible Route Hold is validated. }

FUNCTION ValidateSignalQuadrant(Str : String; OUT ErrorMsg : String) : QuadrantType;
{ Validates and if ok returns the quadrant type }

FUNCTION ValidateSignalType(Str : String; OUT Quadrant : QuadrantType; DistantHomesArray : IntegerArrayType; OUT ErrorMsg : String) : TypeOfSignal;
{ Validates and if ok returns the signal type }

PROCEDURE WriteOutSignalDataToDatabase;
{ If a Signal's data has been changed, record it in the database }

TYPE
  SignalRec = RECORD
    Signal_AccessoryAddress : Integer;
    Signal_Aspect : AspectType;
    Signal_AdjacentLine : Integer;
    Signal_AdjacentLineXOffset : Integer;
    Signal_AdjacentTC : Integer;
    Signal_ApproachControlAspect : AspectType;
    Signal_ApproachLocked : Boolean;
    Signal_AsTheatreDestination : String; { what a signal pointing at this signal might display }
    Signal_Automatic : Boolean; { not yet implemented }
    Signal_DataChanged : Boolean;
    Signal_DecoderNum : Integer;
    Signal_Direction : DirectionType;
    Signal_Energised : Boolean;
    Signal_EnergisedTime : TDateTime;
    Signal_FailedToResetFlag : Boolean;
    Signal_FailMsgWritten : Boolean;
    Signal_FindNextSignalBufferStopMsgWritten : Boolean;
    Signal_FromWhichUserMustDrive : Boolean;
    Signal_HiddenStationSignalAspect : AspectType; { used to force stopping at stations where signal is potentially off }
    Signal_Indicator : IndicatorType;
    Signal_IndicatorDecoderNum : Integer;
    Signal_IndicatorDecoderFunctionNum : Integer;
    Signal_IndicatorMouseRect : TRect; { mouse access rectangle for indicators }
    Signal_IndicatorSpeedRestriction : MPHType; { applicable only if the route indicator is set }
    Signal_IndicatorState : IndicatorStateType;
    Signal_JunctionIndicators : ARRAY [JunctionIndicatorType] OF JunctionIndicatorRec;
    Signal_LampIsOn : Boolean; { used for flashing aspects }
    Signal_LineX : Integer;
    Signal_LineY : Integer;
    Signal_LineWithVerticalSpacingY : Integer;
    Signal_LocationsToMonitorArray : IntegerArrayType;

    { Signal_LockedArray and Signal_RouteLockingNeededArray sound similar but serve different purposes - RouteLockingNeededArray covers the lines, track circuits, points,
      etc. ahead that must be locked before a signal can be pulled off; Signal_LockedArray shows whether a signal is locked either by a specific route or by a user.
    }
    Signal_LockedArray : StringArrayType;
    Signal_LockedBySemaphoreDistant : Boolean;

    Signal_LockFailureNotedInRouteUnit : Boolean;
    Signal_MouseRect : TRect; { mouse access rectangle for signal }
    Signal_NextSignalIfNoIndicator : Integer;
    Signal_NotUsedForRouteing : Boolean;
    Signal_Notes : String;
    Signal_Number : Integer;
    Signal_OppositePassingLoopSignal : Integer;
    Signal_OutOfUse : Boolean;
    Signal_OutOfUseMsgWritten : Boolean;
    Signal_PossibleRouteHold : Boolean;
    Signal_PossibleStationStartRouteHold : Boolean;
    Signal_PostColour : TColour;
    Signal_PostMouseRect : TRect; { mouse access rectangle for signal posts }
    Signal_PreviousAspect : AspectType;
    Signal_PreviousIndicatorState : IndicatorStateType;
    Signal_PreviousTheatreIndicatorString : String;
    Signal_PreviousSignal1 : Integer;
    Signal_PreviousSignal2 : Integer;
    Signal_PreviousHiddenStationSignalAspectSignal1 : Integer;
    Signal_PreviousHiddenStationSignalAspectSignal2 : Integer;
    Signal_PreviousLineX : Integer;
    Signal_PreviousLineY : Integer;
    Signal_PreviousLineWithVerticalSpacingY : Integer;
    Signal_Quadrant : QuadrantType;
    Signal_ResettingTC : Integer;

    { see note above for Signal_LockedArray }
    Signal_RouteLockingNeededArray : StringArrayType;

    Signal_SemaphoreDistantHomesArray : IntegerArrayType; { needed to tell a semaphore distant which semaphore homes lock it }
    Signal_SemaphoreDistantLocking : Integer;
    Signal_StateChanged : Boolean;
    Signal_TheatreIndicatorString : String; { what this signal might display }
    Signal_TRSHeld : Boolean;
    Signal_TRSHeldMsgWritten : Boolean;
    Signal_TRSReleased : Boolean;
    Signal_TRSReleasedMsgWritten : Boolean;
    Signal_Type : TypeOfSignal;
  END;

  WriteReadType = (ReadOnly, WriteOnly, WriteThenRead);

CONST
  Signal_AccessoryAddressFieldName : String = 'Signal Accessory Address';
  Signal_AdjacentLineFieldName : String = 'Signal Adjacent Line';
  Signal_AdjacentLineXOffsetFieldName : String = 'Signal AdjacentLine XOffset';
  Signal_ApproachControlAspectFieldName : String = 'Signal Approach Control Aspect';
  Signal_AsTheatreDestinationFieldName : String = 'Signal As Theatre Destination';
  Signal_AutomaticFieldName : String = 'Signal Automatic'; { not in use }
  Signal_DirectionFieldName : String = 'Signal Direction';
  Signal_DecoderNumFieldName : String = 'Signal Decoder Num';
  Signal_FromWhichUserMustDriveFieldName : String = 'Signal From Which User Must Drive';
  Signal_IndicatorDecoderFunctionNumFieldName : String = 'Signal Indicator Decoder Function Num';
  Signal_IndicatorDecoderNumFieldName : String = 'Signal Indicator Decoder Num';
  Signal_IndicatorSpeedRestrictionFieldName : String = 'Signal Indicator Speed Restriction';
  Signal_IndicatorFieldName : String = 'Signal Indicator';
  Signal_JunctionIndicatorsFieldName : String = 'Signal Junction Indicators';
  Signal_LocationsToMonitorFieldName : String = 'Signal Locations To Monitor';
  Signal_LowerLeftIndicatorTargetFieldName : String = 'Signal Lower Left Indicator Target';
  Signal_LowerRightIndicatorTargetFieldName : String = 'Signal Lower Right Indicator Target';
  Signal_MiddleLeftIndicatorTargetFieldName : String = 'Signal Middle Left Indicator Target';
  Signal_MiddleRightIndicatorTargetFieldName : String = 'Signal Middle Right Indicator Target';
  Signal_NextSignalIfNoIndicatorFieldName : String = 'Signal Next Signal If No Indicator';
  Signal_NotesFieldName : String = 'Signal Notes';
  Signal_NotUsedForRouteingFieldName : String = 'Signal Not Used For Routeing';
  Signal_NumberFieldName : String = 'Signal Number';
  Signal_OppositePassingLoopSignalFieldName : String = 'Signal Opposite Passing Loop Signal';
  Signal_OutOfUseFieldName : String = 'Signal Out Of Use';
  Signal_PossibleRouteHoldFieldName : String = 'Signal Possible Route Hold';
  Signal_PossibleStationStartRouteHoldFieldName : String = 'Signal Possible Station Start Route Hold';
  Signal_QuadrantFieldName : String = 'Signal Quadrant';
  Signal_SemaphoreDistantHomesArrayFieldName : String = 'Signal Distant Homes';
  Signal_TypeFieldName : String = 'Signal Type';
  Signal_UpDownFieldName : String = 'Signal Direction';
  Signal_UpperLeftIndicatorTargetFieldName : String = 'Signal Upper Left Indicator Target';
  Signal_UpperRightIndicatorTargetFieldName : String = 'Signal Upper Right Indicator Target';
  Signal_VerticalSpacingFieldName : String = 'Signal Vertical Spacing';

VAR
  SignalUnitForm: TSignalUnitForm;

  SignalHighlighted : Integer = UnknownSignal;
  Signals : ARRAY OF SignalRec;

IMPLEMENTATION

{$R *.dfm}

USES RailDraw, Route, MiscUtils, Lenz, StrUtils, Locks, CreateRoute, Options, PointsUnit, TrackCircuitsUnit, LinesUnit, LocationsUnit, Main, Logging, Edit;

CONST
  UnitRef = 'Signal';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION JunctionIndicatorLit(S : Integer) : Boolean;
{ Return true if a junction indicator is lit }
BEGIN
  Result := False;

  WITH Signals[S] DO BEGIN
    CASE Signals[S].Signal_IndicatorState OF
      UpperLeftIndicatorLit:
        Result := True;
      MiddleLeftIndicatorLit:
        Result := True;
      LowerLeftIndicatorLit:
        Result := True;
      UpperRightIndicatorLit:
        Result := True;
      MiddleRightIndicatorLit:
        Result := True;
      LowerRightIndicatorLit:
        Result := True;
    END; { CASE }
  END; {WITH}
END; { JunctionIndicatorLit }

FUNCTION IsSignalInStringArray(StringArray : StringArrayType; Signal : Integer) : Boolean;
{ Returns whether the given signal is found in a string array }
VAR
  I : Integer;

BEGIN
  I := 0;
  Result := False;
  WHILE (I <= High(StringArray)) AND (Result = False) DO BEGIN
    IF (Signal <> UnknownSignal) AND (Signal = ExtractSignalFromString(StringArray[I])) THEN
      Result := True
    ELSE
      Inc(I);
  END; {WHILE}
END; { IsSignalInStringArray }

PROCEDURE CheckSemaphoreDistantBeforeSemaphoreHomeCleared(S : Integer);
{ Sees if semaphore distants need automatically to be reset to allow a semaphore home to be put on }
VAR
  I : Integer;
  OK : Boolean;

BEGIN
  IF (Signals[S].Signal_Type = SemaphoreHome) AND (Signals[S].Signal_SemaphoreDistantLocking <> UnknownSignal) THEN BEGIN
    I := 0;
    WHILE I <= High(Signals[Signals[S].Signal_SemaphoreDistantLocking].Signal_SemaphoreDistantHomesArray) DO BEGIN
      { unlock the semaphore home signals }
      IF Signals[Signals[Signals[S].Signal_SemaphoreDistantLocking].Signal_SemaphoreDistantHomesArray[I]].Signal_Aspect = RedAspect THEN
        Signals[Signals[Signals[S].Signal_SemaphoreDistantLocking].Signal_SemaphoreDistantHomesArray[I]].Signal_LockedBySemaphoreDistant := False;
      Inc(I);
    END; {WHILE}

    { and set the distant on }
    Signals[Signals[S].Signal_SemaphoreDistantLocking].Signal_Aspect := RedAspect;
    IF SystemOnline THEN BEGIN
      MakeSemaphoreSignalChange(UnknownLocoChipStr, Signals[S].Signal_SemaphoreDistantLocking, Signals[Signals[S].Signal_SemaphoreDistantLocking].Signal_AccessoryAddress,
                                SignalOn, OK);
      IF OK THEN
        Log('S S=' + IntToStr(Signals[S].Signal_SemaphoreDistantLocking) + ' on')
      ELSE
        Log('S S=' + IntToStr(Signals[S].Signal_SemaphoreDistantLocking) + ' setting to on failed');
    END;
  END;
END; { CheckSemaphoreDistantBeforeSemaphoreHomeCleared }

FUNCTION SignalAdjacentLineOK(Line : Integer) : Boolean;
{ Returns true if a signal can be created next to the current line }
VAR
  S : Integer;
  SignalAdjacentLineFound : Boolean;

BEGIN
  Result := False;

  IF Line <> UnknownLine THEN BEGIN
    { only create a signal next to a line if there isn't one already attached to it }
    S := 0;
    SignalAdjacentLineFound := False;
    WHILE (S <= High(Signals)) AND NOT SignalAdjacentLineFound DO BEGIN
      IF Signals[S].Signal_AdjacentLine = Line THEN
        SignalAdjacentLineFound := True;
      Inc(S);
    END; {WHILE}

    IF NOT SignalAdjacentLineFound THEN
      { the line has to be horizontal }
      IF Lines[Line].Line_GridUpY = Lines[Line].Line_GridDownY THEN
        Result := True;
  END;
END; { SignalAdjacentLineOK }

FUNCTION GetSignalAdjacentLine(S : Integer) : Integer;
{ Return the line adjacent to the given signal }
BEGIN
  IF S <> UnknownSignal THEN
    Result := Signals[S].Signal_AdjacentLine
  ELSE
    Result := UnknownLine;
END; { GetSignalAdjacentLine }

FUNCTION GetSignalAspect(S : Integer) : AspectType;
{ Return the state of a signal }
BEGIN
  IF S <> UnknownSignal THEN
    Result := Signals[S].Signal_Aspect
  ELSE BEGIN
    ShowMessage('Signal is zero');
    Result := NoAspect;
  END;
END; { GetSignalAspect }

FUNCTION SignalHasLeftJunctionIndicator(S : Integer; OUT Indicator : JunctionIndicatorType) : Boolean;
{ Returns true if the signal has a left junction indicator }
BEGIN
  Result := False;

  WITH Signals[S] DO BEGIN
    IF Signal_Indicator = JunctionIndicator THEN BEGIN
      Indicator := UnknownJunctionIndicator;
      IF Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_Exists THEN
        Indicator := UpperLeftIndicator
      ELSE
        IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists THEN
          Indicator := MiddleLeftIndicator
        ELSE
          IF Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_Exists THEN
            Indicator := LowerLeftIndicator;

      IF Indicator <> UnknownJunctionIndicator THEN
        Result := True;
    END; {WITH}
  END;
END; { SignalHasLeftJunctionIndicator }

FUNCTION SignalHasRightJunctionIndicator(S : Integer; OUT Indicator : JunctionIndicatorType) : Boolean;
{ Returns true if the signal has a right junction indicator }
BEGIN
  Result := False;

  WITH Signals[S] DO BEGIN
    IF Signal_Indicator = JunctionIndicator THEN BEGIN
      Indicator := UnknownJunctionIndicator;
      IF Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_Exists THEN
        Indicator := UpperRightIndicator
      ELSE
        IF Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists THEN
          Indicator := MiddleRightIndicator
        ELSE
          IF Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_Exists THEN
            Indicator := LowerRightIndicator;

      IF Indicator <> UnknownJunctionIndicator THEN
        Result := True;
    END; {WITH}
  END;
END; { SignalHasRightJunctionIndicator }

PROCEDURE CalculateSignalPosition(S : Integer);
{ Work out where a signal is on the screen }
BEGIN
  TRY
    WITH Signals[S] DO BEGIN
      IF Signal_AdjacentLine <> UnknownLine THEN BEGIN
        IF Signal_Direction = Up THEN BEGIN
          Signal_LineX := MapGridXToScreenX(Lines[Signal_AdjacentLine].Line_GridUpX);
          Signal_LineX := Signal_LineX + SignalRadiusScaled;
          IF Signal_Indicator <> NoIndicator THEN
            Signal_LineX := Signal_LineX + SignalHorizontalSpacingScaled;
          IF Signal_Type = FourAspect THEN
            Signal_LineX := Signal_LineX + SignalHorizontalSpacingScaled;
        END ELSE BEGIN
          { Down }
          Signal_LineX := MapGridXToScreenX(Lines[Signal_AdjacentLine].Line_GridDownX);
          Signal_LineX := Signal_LineX - SignalRadiusScaled;
          IF Signal_Indicator <> NoIndicator THEN
            Signal_LineX := Signal_LineX - SignalHorizontalSpacingScaled;
          IF Signal_Type = FourAspect THEN
            Signal_LineX := Signal_LineX - SignalHorizontalSpacingScaled;
        END;

        Signal_LineY := MapGridYToScreenY(Lines[Signal_AdjacentLine].Line_GridUpY);

        { Adjust left or right if AdjacentLineXOffset greater than or less than zero respectively }
        IF Signal_AdjacentLineXOffset > 0 THEN
          Signal_LineX := Signal_LineX + MulDiv(FWPRailWindow.ClientWidth, Signal_AdjacentLineXOffset, ZoomScaleFactor)
        ELSE
          IF Signal_AdjacentLineXOffset < 0 THEN
            Signal_LineX := Signal_LineX - MulDiv(FWPRailWindow.ClientWidth, Abs(Signal_AdjacentLineXOffset), ZoomScaleFactor);

        SignalVerticalSpacingScaled := SignalVerticalSpacingScaled;

        IF Signal_Direction = Up THEN
          Signal_LineWithVerticalSpacingY := Signal_LineY + SignalVerticalSpacingScaled
        ELSE
          IF Signal_Direction = Down THEN
            Signal_LineWithVerticalSpacingY := Signal_LineY - SignalVerticalSpacingScaled;
      END; {WITH}

      { Set up mouse access rectangles }
      WITH Signal_MouseRect DO BEGIN
        RailWindowBitmap.Canvas.Pen.Width := WindowPenWidth;

        IF (Signal_Type <> SemaphoreHome) AND (Signal_Type <> SemaphoreDistant) THEN BEGIN
          { it covers the colour-light signal circles }
          Left := Signal_LineX - SignalRadiusScaled;
          Top := Signal_LineWithVerticalSpacingY - SignalRadiusScaled;
          Right := Signal_LineX + SignalRadiusScaled;
          Bottom := Signal_LineWithVerticalSpacingY + SignalRadiusScaled;
        END ELSE BEGIN
          { it covers the semaphore signal arms }
          IF Signal_Direction = Up THEN BEGIN
            Left := Signal_LineX - SignalSemaphoreWidthScaled;
            Top := Signal_LineWithVerticalSpacingY + RailWindowBitmap.Canvas.Pen.Width;
            Right := Signal_LineX + (SignalSemaphoreWidthScaled * 2);
            Bottom := Signal_LineWithVerticalSpacingY + (SignalSemaphoreWidthScaled * 2);
          END ELSE
            IF Signal_Direction = Down THEN BEGIN
              Left := Signal_LineX - (SignalSemaphoreWidthScaled * 2);
              Top := Signal_LineWithVerticalSpacingY - (SignalSemaphoreWidthScaled * 2);
              Right := Signal_LineX + SignalSemaphoreWidthScaled;
              Bottom := Signal_LineWithVerticalSpacingY - RailWindowBitmap.Canvas.Pen.Width;
            END;
        END;
      END; {WITH}

      { Initialise the route indicator mouse access rectangles }
      WITH Signal_IndicatorMouseRect DO BEGIN
        Left := 0;
        Top := 0;
        Right := 0;
        Bottom := 0;
      END;

      IF Signal_Direction = Up THEN BEGIN
        IF Signal_Type = FourAspect THEN
          Signal_MouseRect.Left := Signal_MouseRect.Left - SignalHorizontalSpacingScaled;
        IF Signal_Indicator <> NoIndicator THEN BEGIN
          WITH Signal_IndicatorMouseRect DO BEGIN
            Left := Signal_MouseRect.Left - (MulDiv(IndicatorHorizontalSpacingScaled, 150, 100));
            Top := Signal_MouseRect.Top;
            Right := Signal_MouseRect.Left;
            Bottom := Signal_MouseRect.Bottom;
          END; {WITH}
        END;
      END ELSE
        IF Signal_Direction = Down THEN BEGIN
          { Signal_Direction = Down }
          IF Signal_Type = FourAspect THEN
            Signal_MouseRect.Right := Signal_MouseRect.Right + SignalHorizontalSpacingScaled;
          IF Signal_Indicator <> NoIndicator THEN BEGIN
            IF Signal_Indicator <> NoIndicator THEN BEGIN
              WITH Signal_IndicatorMouseRect DO BEGIN
                Left := Signal_MouseRect.Right;
                Top := Signal_MouseRect.Top;
                Right := Signal_MouseRect.Right + (MulDiv(IndicatorHorizontalSpacingScaled, 150, 100));
                Bottom := Signal_MouseRect.Bottom;
              END; {WITH}
            END;
          END;
        END;

      { Now the signal posts (used for routeing) }
      WITH Signal_PostMouseRect DO BEGIN
        IF Signal_Direction = Up THEN BEGIN
          { pen.width is the width of the line outlining the signal }
          Left := Signal_LineX + SignalRadiusScaled;
          Top := Signal_LineWithVerticalSpacingY - SignalRadiusScaled;
          Right := Signal_LineX + SignalRadiusScaled + MulDiv(FWPRailWindow.ClientWidth, 10, ZoomScalefactor);
          Bottom := Signal_LineWithVerticalSpacingY + SignalRadiusScaled;
        END ELSE
          IF Signal_Direction = Down THEN BEGIN
            Left := Signal_LineX - SignalRadiusScaled - MulDiv(FWPRailWindow.ClientWidth, 10, ZoomScalefactor);
            Top := Signal_LineWithVerticalSpacingY - SignalRadiusScaled;
            Right := Signal_LineX - SignalRadiusScaled;
            Bottom := Signal_LineWithVerticalSpacingY + SignalVerticalSpacingScaled - RailWindowBitmapCanvasPenWidth;
          END;
      END; {WITH}
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CalculateSignalPosition: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CalculateSignalPosition }

PROCEDURE CalculateAllSignalPositions;
{ Work out where all the signals are on the screen }
VAR
  S : Integer;

BEGIN
  TRY
    S := 0;
    WHILE S <= High(Signals) DO BEGIN
      WITH Signals[S] DO BEGIN
        IF Signal_AdjacentLine <> UnknownLine THEN
          CalculateSignalPosition(S);
      END; {WITH}
      Inc(S);
    END; {WHILE}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CalculateAllSignalPositions: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CalculateAllSignalPositions }

PROCEDURE SaveSignalsCurrentState;
{ Save all the previous state of all signals }
VAR
  S : Integer;

BEGIN
  TRY
    FOR S := 0 TO High(Signals) DO BEGIN
      WITH Signals[S] DO BEGIN
        IF NOT Signal_OutOfUse THEN BEGIN
          Signal_PreviousAspect := Signals[S].Signal_Aspect;
          Signal_PreviousIndicatorState := Signals[S].Signal_IndicatorState;
          Signal_PreviousTheatreIndicatorString := Signal_TheatreIndicatorString;
        END;
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG SaveSignalsCurrentState:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SaveSignalsCurrentState }

PROCEDURE SetSignal(LocoChipStr : String; S : Integer; NewAspect : AspectType; LogSignalData, ForceWriting : Boolean);
{ Set the state of a particular signal and draws it }
VAR
  OK : Boolean;

BEGIN
  TRY
    WITH Signals[S] DO BEGIN
      IF (Signal_Aspect <> NewAspect) OR ForceWriting THEN BEGIN
        Signal_Aspect := NewAspect;
        IF NOT ProgramStarting AND LogSignalData THEN BEGIN
          IF (Signals[S].Signal_Type <> SemaphoreHome) AND (Signals[S].Signal_Type <> SemaphoreDistant) THEN
            Log('S S=' + IntToStr(S) + ' successfully set to ' + AspectToStr(Signals[S].Signal_Aspect))
          ELSE
            Log('S S=' + IntToStr(S) + ' successfully set to ' + SemaphoreAspectToStr(Signals[S].Signal_Aspect));
        END;

        IF SystemOnline AND NOT ResizeMap THEN BEGIN
          IF Signal_DecoderNum <> 0 THEN
            { uses LF100 decoders - bits usually set as follows:
              green is bit 1, red 2, single yellow 3, double yellow 3 + 4; the indicator is bit 4 (not necessarily on same decoder though)
            }
            SetSignalFunction(LocoChipStr, S)
          ELSE
            IF Signal_AccessoryAddress <> 0 THEN
              { uses TrainTech SC3 units for controlling Dapol semaphores }
              IF NewAspect = RedAspect THEN
                MakeSemaphoreSignalChange(LocoChipStr, S, Signal_AccessoryAddress, SignalOn, OK)
              ELSE
                MakeSemaphoreSignalChange(LocoChipStr, S, Signal_AccessoryAddress, SignalOff, OK);
        END;

        IF NOT ProgramStarting THEN
          { calling invalidate here didn't redraw the signal in time - repaint causes the screen to be redrawn instantly and not via the message queue }
          FWPRailWindow.Repaint;
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG SetSignal:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SetSignal }

PROCEDURE SetAllSignalsToDanger;
{ Sets all off signals to on }
VAR
  S : Integer;

BEGIN
  TRY
    FOR S := 0 TO High(Signals) DO BEGIN
      IF NOT Signals[S].Signal_OutOfUse THEN BEGIN
        IF (GetSignalAspect(S) <> RedAspect) THEN
          SetSignal(UnknownLocoChipStr, S, RedAspect, LogSignalData, NOT ForceAWrite);
        IF Signals[S].Signal_IndicatorState <> NoIndicatorLit THEN
          SetIndicator(UnknownLocoChipStr, S, NoIndicatorLit, '', NoRoute, NOT ByUser);
      END;
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG SetAllSignalsToDanger:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SetAllSignalsToDanger }

PROCEDURE SetPreviousSignals(LocoChipStr : String; S : Integer);
{ Sees what previous signals are set to, and resets aspects accordingly. PreviousSignal1 is the nearer and PreviousSignal2 the further }
BEGIN
  WITH Signals[S] DO BEGIN
    CASE Signal_Aspect OF
      RedAspect:
        IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
          SetSignal(LocoChipStr, Signal_PreviousSignal1, SingleYellowAspect, LogSignalData, NOT ForceAWrite);
          IF Signal_PreviousSignal2 <> UnknownSignal THEN BEGIN
            IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
              SetSignal(LocoChipStr, Signal_PreviousSignal2, DoubleYellowAspect, LogSignalData, NOT ForceAWrite);
          END;
        END;
      SingleYellowAspect:
        IF (Signal_IndicatorState <> NoIndicatorLit) AND (Signal_ApproachControlAspect = SingleYellowAspect) THEN BEGIN
          IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
            SetSignal(LocoChipStr, Signal_PreviousSignal1, FlashingSingleYellowAspect, LogSignalData, NOT ForceAWrite);
            IF Signal_PreviousSignal2 <> UnknownSignal THEN
              IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
                SetSignal(LocoChipStr, Signal_PreviousSignal2, FlashingDoubleYellowAspect, LogSignalData, NOT ForceAWrite);
          END;
        END ELSE BEGIN
          IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
            IF Signals[Signal_PreviousSignal1].Signal_Type = FourAspect THEN
              SetSignal(LocoChipStr, Signal_PreviousSignal1, DoubleYellowAspect, LogSignalData, NOT ForceAWrite)
            ELSE
              SetSignal(LocoChipStr, Signal_PreviousSignal1, GreenAspect, LogSignalData, NOT ForceAWrite);
            IF Signal_PreviousSignal2 <> UnknownSignal THEN
              IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
                SetSignal(LocoChipStr, Signal_PreviousSignal2, GreenAspect, LogSignalData, NOT ForceAWrite);
          END;
        END;
      DoubleYellowAspect:
        IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
          SetSignal(LocoChipStr, Signal_PreviousSignal1, GreenAspect, LogSignalData, NOT ForceAWrite);
          IF Signal_PreviousSignal2 <> UnknownSignal THEN
            IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
              SetSignal(LocoChipStr, Signal_PreviousSignal2, GreenAspect, LogSignalData, NOT ForceAWrite);
        END;
      GreenAspect:
        IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
          SetSignal(LocoChipStr, Signal_PreviousSignal1, GreenAspect, LogSignalData, NOT ForceAWrite);
          IF Signal_PreviousSignal2 <> UnknownSignal THEN
            IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
              SetSignal(LocoChipStr, Signal_PreviousSignal2, GreenAspect, LogSignalData, NOT ForceAWrite);
        END;
    END; {CASE}
  END; {WITH}
END; { SetPreviousSignals }

PROCEDURE SetIndicator(LocoChipStr : String; S : Integer; IndicatorState : IndicatorStateType; TheatreIndicatorString : String; Route : Integer; User : Boolean);
{ Turn the indicator of a particular signal on or off }
CONST
  ShowNames = True;

VAR
  DebugStr : String;
  I : Integer;
  OK : Boolean;
  RouteLockingArray : IntegerArrayType;

BEGIN
  OK := True;
  WITH Signals[S] DO BEGIN
    IF Signals[S].Signal_OutOfUse THEN BEGIN
      IF NOT Signals[S].Signal_OutOfUseMsgWritten THEN BEGIN
        IF NOT User THEN
          Log('S Cannot change an indicator for a signal (S=' + IntToStr(S) + ') which is out of use')
        ELSE
          Log('S+ User cannot change an indicator for a signal (S=' + IntToStr(S) + ') which is out of use');
        Signals[S].Signal_OutOfUseMsgWritten := True;
      END;
    END ELSE BEGIN
      IF (Route <> UnknownRoute) AND SignalIsLockedByAnyRoute(S, RouteLockingArray) THEN BEGIN
        IF (Length(RouteLockingArray) > 1) OR (RouteLockingArray[0] <> Route) THEN BEGIN
          OK := False;
          IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
            Signals[S].Signal_FailMsgWritten := True;
            DebugStr := 'Cannot set indicator for S=' + IntToStr(S) + ' as signal locked by';
            IF Length(RouteLockingArray) = 1 THEN
              DebugStr := DebugStr + ' R=' + IntToStr(RouteLockingArray[0])
            ELSE
              FOR I := 0 TO High(RouteLockingArray) DO
                DebugStr := DebugStr + ' R=' + IntToStr(RouteLockingArray[I]);

            Log(LocoChipStr + ' R ' + DebugStr);
          END;
        END;
      END;

      IF OK THEN BEGIN
        Signal_IndicatorState := IndicatorState;
        Signal_TheatreIndicatorString := TheatreIndicatorString;

        IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
          CASE Signal_IndicatorState OF
            NoIndicatorLit:
              DebugStr := 'No Indicator';
            UpperLeftIndicatorLit:
              DebugStr := 'Upper Left Indicator';
            MiddleLeftIndicatorLit:
              DebugStr := 'Middle Left Indicator';
            LowerLeftIndicatorLit:
              DebugStr := 'Lower Left Indicator';
            UpperRightIndicatorLit:
              DebugStr := 'Upper Right Indicator';
            MiddleRightIndicatorLit:
              DebugStr := 'Middle Right Indicator';
            LowerRightIndicatorLit:
              DebugStr := 'Lower Right Indicator';
            QueryIndicatorLit:
              DebugStr := 'Query';
            RightIndicatorLit:
              DebugStr := 'Right';
            LeftIndicatorLit:
              DebugStr := 'Left';
            TheatreIndicatorLit:
              DebugStr := 'Theatre (' + Signal_TheatreIndicatorString + ')';
          END; {CASE}

          IF Signal_IndicatorState <> NoIndicatorLit THEN
            DebugStr := 'Indicator for S=' + IntToStr(S) + ' set to ' + DebugStr
          ELSE
            DebugStr := 'Indicator for S=' + IntToStr(S) + ' cleared';
          IF Route <> UnknownRoute THEN
            DebugStr := DebugStr + ' by R=' + IntToStr(Route);
          Log(LocoChipStr + ' S ' + DebugStr);

          InvalidateScreen(UnitRef, 'SetIndicator');

          IF SystemOnline THEN
            { NB: Often route indicator will be operated by a different LF100 to the one operating the signal }
            SetSignalRouteFunction(LocoChipStr, S);
        END;
      END;
    END;
  END; {WITH}
END; { SetIndicator }

PROCEDURE RestoreAllSignalsToPreviousState;
{ Sets all off signals to how they were before short circuit }
VAR
  S : Integer;

BEGIN
  TRY
    Log('S Restoring all signals to their previous aspects');
    FOR S := 0 TO High(Signals) DO BEGIN
      WITH Signals[S] DO BEGIN
        IF NOT Signal_OutOfUse THEN BEGIN
          { have to set state to NoAspect, or SetSignal won't redraw the SignalAspect/indicator }
          Signal_Aspect := NoAspect;
          Signal_IndicatorState := NoIndicatorLit;
          SetSignal(UnknownLocoChipStr, S, Signal_PreviousAspect, LogSignalData, NOT ForceAWrite);
          IF Signal_PreviousIndicatorState <> NoIndicatorLit THEN
            SetIndicator(UnknownLocoChipStr, S, Signal_PreviousIndicatorState, Signal_PreviousTheatreIndicatorString, NoRoute, NOT ByUser);
        END;
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG RestoreAllSignalsToPreviousState:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { RestoreAllSignalsToPreviousState }

PROCEDURE TurnAllSignalsOff;
{ Turn off the LEDs in the signals }
VAR
  S : Integer;

BEGIN
  TRY
    FOR S := 0 TO High(Signals) DO BEGIN
      SetSignal(UnknownLocoChipStr, S, NoAspect, LogSignalData, NOT ForceAWrite);
      IF Signals[S].Signal_IndicatorState <> NoIndicatorLit THEN
        SetIndicator(UnknownLocoChipStr, S, NoIndicatorLit, '', NoRoute, NOT ByUser);
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG TurnAllSignalsOff:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TurnAllSignalsOff }

PROCEDURE SetHiddenStationSignalAspectSignals(T : TrainIndex; HiddenStationSignalAspectSignal, Journey, Route : Integer);
{ Find and set current and previous hidden station aspects }

  PROCEDURE FindPreviousHiddenStationSignalAspectSignals(RouteArray : StringArrayType; CurrentSignal : Integer; VAR PreviousSignal1, PreviousSignal2 : Integer);
  { Look through a journey to see if any previous aspects need to be set too }
  VAR
    Done : Boolean;
    RouteArrayPos : Integer;
    TempS : Integer;

  BEGIN
    RouteArrayPos := High(RouteArray);
    Done := False;
    WHILE (RouteArrayPos > -1) AND NOT Done DO BEGIN
      IF Pos('FS=', RouteArray[RouteArrayPos]) > 0 THEN BEGIN
        IF ExtractSignalFromString(RouteArray[RouteArrayPos]) <> CurrentSignal THEN
          IF (PreviousSignal1 = UnknownSignal) AND (ExtractSignalFromString(RouteArray[RouteArrayPos]) <> CurrentSignal) THEN
            PreviousSignal1 := ExtractSignalFromString(RouteArray[RouteArrayPos])
          ELSE
            IF PreviousSignal2 = UnknownSignal THEN BEGIN
              { assuming it's not the signal we've just found above }
              TempS := ExtractSignalFromString(RouteArray[RouteArrayPos]);
              IF (TempS <> UnknownSignal) AND (TempS <> PreviousSignal1) THEN BEGIN
                Done := True;
                { only use it if it's a four aspect signal }
                IF Signals[TempS].Signal_Type = FourAspect THEN
                  PreviousSignal2 := ExtractSignalFromString(RouteArray[RouteArrayPos]);
              END;
            END;
      END;
      Dec(RouteArrayPos)
    END; {WHILE}
  END; { FindPreviousHiddenStationSignalAspectSignals }

VAR
  PreviousHiddenStationSignalAspectSignal1 : Integer;
  PreviousHiddenStationSignalAspectSignal2 : Integer;
  TempJourneyCount : Integer;
  TempLocoChipStr : String;

BEGIN
  WITH RailWindowBitmap.Canvas DO BEGIN
    IF T = UnknownTrainIndex THEN
      UnknownTrainRecordFound('SetHiddenStationSignalAspectSignals')
    ELSE BEGIN
      WITH Trains[T] DO BEGIN
        IF T <= High(Trains) THEN
          TempLocoChipStr := LocoChipToStr(Train_LocoChip)
        ELSE
          TempLocoChipStr := '';

        IF Signals[HiddenStationSignalAspectSignal].Signal_Aspect <> RedAspect THEN
          Log(TempLocoChipStr + ' X+ S=' + IntToStr(HiddenStationSignalAspectSignal) + ': cannot set hidden station aspect as signal is off')
        ELSE BEGIN
          Signals[HiddenStationSignalAspectSignal].Signal_HiddenStationSignalAspect := RedAspect;
          Signals[HiddenStationSignalAspectSignal].Signal_PostColour := clRed;

          Log(TempLocoChipStr + ' R J=' + IntToStr(Journey) + ' R=' + IntToStr(Route)
                              + ' S=' + IntToStr(HiddenStationSignalAspectSignal) + ': hidden station aspect set to red');
          IF ShowSignalHiddenStationSignalAspects THEN
            DrawSignal(HiddenStationSignalAspectSignal);

          PreviousHiddenStationSignalAspectSignal1 := UnknownSignal;
          PreviousHiddenStationSignalAspectSignal2 := UnknownSignal;
          TempJourneyCount := Journey;
          IF TempLocoChipStr <> '' THEN BEGIN
            WHILE ((PreviousHiddenStationSignalAspectSignal1 = UnknownSignal) OR (PreviousHiddenStationSignalAspectSignal2 = UnknownSignal))
            AND (TempJourneyCount > -1) AND NOT Train_JourneysArray[TempJourneyCount].TrainJourney_Cleared
            DO BEGIN
              FindPreviousHiddenStationSignalAspectSignals(Train_JourneysArray[TempJourneyCount].TrainJourney_RouteArray,
                                                     HiddenStationSignalAspectSignal, PreviousHiddenStationSignalAspectSignal1,
                                                     PreviousHiddenStationSignalAspectSignal2);
              Dec(TempJourneyCount);
            END; {WHILE}
          END;

          Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1 := PreviousHiddenStationSignalAspectSignal1;
          Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2 := PreviousHiddenStationSignalAspectSignal2;

          { If this signal has a hidden station aspect that's on, make sure previous signals are included }
          IF (PreviousHiddenStationSignalAspectSignal1 <> UnknownSignal) AND (Signals[PreviousHiddenStationSignalAspectSignal1].Signal_Type = FourAspect) THEN BEGIN
            Signals[PreviousHiddenStationSignalAspectSignal1].Signal_HiddenStationSignalAspect := SingleYellowAspect;
            Signals[PreviousHiddenStationSignalAspectSignal1].Signal_PostColour := clYellow;
            Log(TempLocoChipStr + ' R J=' + IntToStr(Journey) + ' R=' + IntToStr(Route)
                                + ' S=' + IntToStr(HiddenStationSignalAspectSignal) + '''s previous signal S='
                                + IntToStr(PreviousHiddenStationSignalAspectSignal1) + ': hidden station aspect set to single yellow');
            IF ShowSignalHiddenStationSignalAspects THEN
              DrawSignal(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1);

            IF (PreviousHiddenStationSignalAspectSignal2 <> UnknownSignal)
            AND ((Signals[PreviousHiddenStationSignalAspectSignal2].Signal_Type = FourAspect)
                 OR (Signals[PreviousHiddenStationSignalAspectSignal2].Signal_Type = ThreeAspect))
            THEN BEGIN
              Signals[PreviousHiddenStationSignalAspectSignal2].Signal_HiddenStationSignalAspect := DoubleYellowAspect;
              Signals[PreviousHiddenStationSignalAspectSignal2].Signal_PostColour := clYellow;
              Log(TempLocoChipStr + ' R J=' + IntToStr(Journey) + ' R=' + IntToStr(Route)
                                  + ' S=' + IntToStr(HiddenStationSignalAspectSignal) + '''s previous signal but one S='
                                  + IntToStr(PreviousHiddenStationSignalAspectSignal2) + ': hidden station aspect set to double yellow');
              IF ShowSignalHiddenStationSignalAspects THEN
                DrawSignal(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2);
            END;
          END ELSE
            IF (PreviousHiddenStationSignalAspectSignal1 <> UnknownSignal) AND (Signals[PreviousHiddenStationSignalAspectSignal1].Signal_Type = ThreeAspect) THEN BEGIN
              Signals[PreviousHiddenStationSignalAspectSignal1].Signal_HiddenStationSignalAspect := SingleYellowAspect;
              Signals[PreviousHiddenStationSignalAspectSignal1].Signal_PostColour := clYellow;
              Log(TempLocoChipStr + ' R J=' + IntToStr(Journey) + ' R=' + IntToStr(Route)
                                  + ' S=' + IntToStr(HiddenStationSignalAspectSignal) + '''s previous signal S='
                                  + IntToStr(PreviousHiddenStationSignalAspectSignal1) + ' hidden station aspect set to single yellow');
              IF ShowSignalHiddenStationSignalAspects THEN
                DrawSignal(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1);
            END;
        END;
      END; {WITH}
    END;
  END; {WITH}
END; { SetHiddenStationSignalAspectSignals }

PROCEDURE ClearHiddenStationSignalAspectSignals(T : TrainIndex; HiddenStationSignalAspectSignal : Integer);
{ Clear the hidden station aspects of a given signal }
VAR
  TempLocoChipStr : String;

BEGIN
  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('ClearHiddenStationSignalAspectSignals')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      IF T <= High(Trains) THEN
        TempLocoChipStr := LocoChipToStr(Train_LocoChip)
      ELSE
        TempLocoChipStr := '';

      Signals[HiddenStationSignalAspectSignal].Signal_HiddenStationSignalAspect := NoAspect;
      Signals[HiddenStationSignalAspectSignal].Signal_PostColour := SignalPostColour;
      IF ShowSignalHiddenStationSignalAspects THEN
        DrawSignal(HiddenStationSignalAspectSignal);
      Log(TempLocoChipStr + ' R S=' + IntToStr(HiddenStationSignalAspectSignal) + ' hidden station aspect set to no aspect');

      { and also reset any previous hidden station signal aspect signals }
      IF (Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1 <> UnknownSignal)
      AND (Signals[Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1].Signal_HiddenStationSignalAspect <> NoAspect)
      THEN BEGIN
        Signals[Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1].Signal_HiddenStationSignalAspect := NoAspect;
        Signals[Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1].Signal_PostColour := SignalPostColour;
        Log(TempLocoChipStr + ' R S=' + IntToStr(HiddenStationSignalAspectSignal) + '''s previous signal S='
                            + IntToStr(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1)
                            + ' hidden station aspect set to no aspect');
        IF ShowSignalHiddenStationSignalAspects THEN
          DrawSignal(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1);
      END;

      IF (Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2 <> UnknownSignal)
      AND (Signals[Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2].Signal_HiddenStationSignalAspect <> NoAspect)
      THEN BEGIN
        Signals[Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2].Signal_HiddenStationSignalAspect := NoAspect;
        Signals[Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2].Signal_PostColour := SignalPostColour;
        Log(TempLocoChipStr + ' R S=' + IntToStr(HiddenStationSignalAspectSignal) + '''s previous signal but one S='
                            + IntToStr(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2)
                            + ' hidden station aspect set to no aspect');
        IF ShowSignalHiddenStationSignalAspects THEN
          DrawSignal(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2);
       END;
    END; {WITH}
  END;
END; { ClearHiddenStationSignalAspectSignals }

PROCEDURE PullSignalMainProcedure(LocoChipStr : String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                                  SettingString : String; ResetTC : Integer; TrainType : TypeOfTrainType; User : Boolean; OUT OK : Boolean);
{ Changes the state of a signal if legal }

  FUNCTION SignalIsLockedByUser(S : Integer) : Boolean;
  { Returns true if the signal is locked by the user }
  VAR
    LockCount : Integer;

  BEGIN
    Result := False;
    WITH Signals[S] DO BEGIN
      LockCount := 0;
      WHILE (LockCount < Length(Signal_LockedArray)) AND (Result = False) DO BEGIN
        IF Signal_LockedArray[LockCount] = 'USER' THEN
          Result := True;
        Inc(LockCount);
      END; {WHILE}
    END; {WITH}
  END; { SignalIsLockedByUser }

  PROCEDURE LockSignalByRoute(LocoChipStr : String; S, Route : Integer; DoNotWriteMessage : Boolean);
  { Mark the signal as locked by a specific route }
  VAR
    ElementPos : Integer;

  BEGIN
    IF NOT IsElementInStringArray(Signals[S].Signal_LockedArray, 'R=' + IntToStr(Route), ElementPos) THEN BEGIN
      AppendToStringArray(Signals[S].Signal_LockedArray, 'R=' + IntToStr(Route));
      IF NOT DoNotWriteMessage THEN
        Log(LocoChipStr + ' S S=' + IntToStr(S) + ' now locked by R=' + IntToStr(Route));
    END;
  END; { LockSignalByRoute }

  PROCEDURE LockSignalByUser(S : Integer);
  { Mark the signal as locked by a user }
  BEGIN
    AppendToStringArray(Signals[S].Signal_LockedArray, 'USER');
    RemoveDuplicateElementsFromStringArray(Signals[S].Signal_LockedArray);
    Log('S S=' + IntToStr(S) + ' now locked by user');
  END; { LockSignalByUser }

  PROCEDURE FindPreviousHiddenStationSignalAspectSignals(RouteArray : StringArrayType; CurrentSignal : Integer; VAR PreviousSignal1, PreviousSignal2 : Integer);
  { Look through a journey to see if any previous hidden station aspects need to be set too }
  VAR
    Done : Boolean;
    RouteArrayPos : Integer;
    TempS : Integer;

  BEGIN
    RouteArrayPos := High(RouteArray);
    Done := False;
    WHILE (RouteArrayPos > -1) AND NOT Done DO BEGIN
      IF Pos('FS=', RouteArray[RouteArrayPos]) > 0 THEN BEGIN
        IF ExtractSignalFromString(RouteArray[RouteArrayPos]) <> CurrentSignal THEN
          IF (PreviousSignal1 = UnknownSignal) AND (ExtractSignalFromString(RouteArray[RouteArrayPos]) <> CurrentSignal) THEN
            PreviousSignal1 := ExtractSignalFromString(RouteArray[RouteArrayPos])
          ELSE
            IF PreviousSignal2 = UnknownSignal THEN BEGIN
              { assuming it's not the signal we've just found above }
              TempS := ExtractSignalFromString(RouteArray[RouteArrayPos]);
              IF (TempS <> UnknownSignal) AND (TempS <> PreviousSignal1) THEN BEGIN
                Done := True;
                { only use it if it's a four aspect signal }
                IF Signals[TempS].Signal_Type = FourAspect THEN
                  PreviousSignal2 := ExtractSignalFromString(RouteArray[RouteArrayPos]);
              END;
            END;
      END;
      Dec(RouteArrayPos)
    END; {WHILE}
  END; { FindPreviousHiddenStationSignalAspectSignals }

  FUNCTION SignalLockingOK(LocoChipStr : String; S : Integer; LockList : StringArrayType; ShowError : Boolean) : Boolean;
  { Accepts a string containing a list of locking requirements, consisting of pairs of points or signal names then '/' or '-', or '\' or '=' respectively. Also checks
    routeing.
  }
    PROCEDURE LockPointBySignal(P, S : Integer);
    { Mark the point as locked by a specific signal }
    VAR
      ElementPos : Integer;

    BEGIN
      { First remove the point from the point resetting array if it's there }
      IF IsElementInIntegerArray(PointResettingToDefaultStateArray, P, ElementPos) THEN
        DeleteElementFromIntegerArray(PointResettingToDefaultStateArray, ElementPos);

      AppendToStringArray(Points[P].Point_LockingArray, 'S=' + IntToStr(S));
      RemoveDuplicateElementsFromStringArray(Points[P].Point_LockingArray);
      Points[P].Point_LockingState := Points[P].Point_PresentState;
    END; { LockPointBySignal }

    FUNCTION PointIsLockedByASpecificSignal(P, S : Integer) : Boolean;
    { Returns true of the point is locked by the given signal }
    VAR
      PointLockCount : Integer;

    BEGIN
      Result := False;
      WITH Points[P] DO BEGIN
        IF S <> UnknownSignal THEN BEGIN
          PointLockCount := 0;
          WHILE (PointLockCount < Length(Point_LockingArray)) AND (Result = False) DO BEGIN
            IF (Point_LockingArray[PointLockCount] = 'S=' + IntToStr(S)) THEN
              Result := True;
            Inc(PointLockCount);
          END; {WHILE}
        END;
      END; {WITH}
    END; { PointIsLockedByASpecificSignal }

  VAR
    ActionCh : Char;
    Device : Integer;
    IndicatorRequested : IndicatorStateType;
    LockListItem : String;
    LockListPos : Word;
    OK : Boolean;
    IndicatorStrPos : Integer;
    SignalPutativeStateStr : String;

  BEGIN
    OK := True;
    ActionCh := ' ';
    Device := 99999;
    SignalPutativeStateStr := 'on';

    IF Length(Signals[S].Signal_RouteLockingNeededArray) <> 0 THEN BEGIN
      { see whether we've pulling the signal off or not }
      IF Signals[S].Signal_Aspect = RedAspect THEN
        SignalPutativeStateStr := 'off';

      IF NOT Signals[S].Signal_FailMsgWritten THEN
        WriteStringArrayToLog(LocoChipStr, 'S', 'LA for S=' + IntToStr(S) + ': ', LockList, 2, 190);

      LockListPos := 0;
      WHILE OK AND (LockListPos <= High(LockList)) DO BEGIN
        { "L=" is not there for locking, but needed later for route drawing using LockList; HoldMarker is used by the routeing routine }
        IF (Pos('L=', LockList[LockListPos]) > 0)
        OR (LockList[LockListPos] = HoldMarker)
        THEN
          Inc(LockListPos)
        ELSE BEGIN
          { remove the prefix (which is there for FWP's benefit, not the program's) }
          IF (Copy(LockList[LockListPos], 1, 3) = 'FP=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'TP=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'XP=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'TS=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'SR=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'BS=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'TC=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'FS=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'FR=')
          THEN
            LockListItem := Copy(LockList[LockListPos], 4, 255)
          ELSE
            Log(LocoChipStr + ' S! Error in LockList at position ' + IntToStr(LockListPos) + ' : ' + LockList[LockListPos]);

          { Ignore buffer stops - info is there to help debug the list }
          IF (Copy(LockList[LockListPos], 1, 3) <> 'BS=') AND (Copy(LockList[LockListPos], 1, 3) <> 'SR=') THEN BEGIN
            IF (Copy(LockList[LockListPos], 1, 3) = 'FR=') AND (Pos('>', LockList[LockListPos]) > 0) THEN BEGIN
              { a theatre indicator }
              ActionCh := '>';
              Device := StrToInt(Copy(LockListItem, 1, Pos('>', LockList[LockListPos]) - 4));
            END ELSE BEGIN
              ActionCh := LockListItem[Length(LockListItem)];
              IndicatorStrPos := Pos('UL', LockListItem);
              IF IndicatorStrPos > 0 THEN
                Delete(LockListItem, IndicatorStrPos, 2)
              ELSE BEGIN
                IndicatorStrPos := Pos('ML', LockListItem);
                IF IndicatorStrPos > 0 THEN
                  Delete(LockListItem, IndicatorStrPos, 2)
                ELSE BEGIN
                  IndicatorStrPos := Pos('LL', LockListItem);
                  IF IndicatorStrPos > 0 THEN
                    Delete(LockListItem, IndicatorStrPos, 2)
                  ELSE BEGIN
                    IndicatorStrPos := Pos('UR', LockListItem);
                    IF IndicatorStrPos > 0 THEN
                      Delete(LockListItem, IndicatorStrPos, 2)
                    ELSE BEGIN
                      IndicatorStrPos := Pos('MR', LockListItem);
                      IF IndicatorStrPos > 0 THEN
                        Delete(LockListItem, IndicatorStrPos, 2)
                      ELSE BEGIN
                        IndicatorStrPos := Pos('LR', LockListItem);
                        IF IndicatorStrPos > 0 THEN
                          Delete(LockListItem, IndicatorStrPos, 2);
                      END;
                    END;
                  END;
                END;
              END;

              Device := StrToInt(Copy(LockListItem, 1, Length(LockListItem) - 1));
            END;

            IF (Device <> S)
            OR ((ActionCh <> '\') AND (ActionCh <> '=') AND (ActionCh <> '>') AND (ActionCh <> '|') AND (ActionCh <> '.'))
            THEN BEGIN
              { need to disregard the locking of the calling signal, or we find it can't be unlocked because it's already locked itself! }
              CASE ActionCh OF
                '\', '=', '>':
                  IF (ActionCh = '\') OR (ActionCh = '>') THEN BEGIN
                    IF GetSignalAspect(Device) = RedAspect THEN BEGIN
                      { the next signal - if it should be off, and it's actually on, it's a failure }
                      OK := False;
                      IF NOT Signals[S].Signal_FailMsgWritten THEN
                        Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' off because S=' + IntToStr(Device) + ' is on');
                    END;
                  END ELSE
                    { ActionCh = '=' }
                    IF NOT (GetSignalAspect(Device) = RedAspect) THEN BEGIN
                     { the next signal - if it should be on, and it's actually off, it's a failure }
                      OK := False;
                      IF NOT Signals[S].Signal_FailMsgWritten THEN
                        Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' on because S=' + IntToStr(Device) + ' is off');
                    END;
                '/', '-':
                  { a point }
                  { If the points are unlocked, or if they're locked by our signal, they're ok; they're also ok if they're locked by something else, and they're just an XP
                    [crossing point] as far as our signal is concerned
                  }
                  IF NOT PointIsLockedByASpecificSignal(Device, S) THEN BEGIN
                    IF Points[Device].Point_PresentState = PointStateUnknown THEN BEGIN
                      { the next point - if it should be known, and it's actually unknown, it's a failure }
                      OK := False;
                      IF NOT Signals[S].Signal_FailMsgWritten THEN
                        Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' because P=' + IntToStr(Device) + ' is in an unknown state');
                    END ELSE BEGIN
                      IF ActionCh = '/' THEN BEGIN
                        IF Points[Device].Point_PresentState = Straight THEN BEGIN
                          { the next point - if it should be diverging, and it's straight, it's a failure }
                          OK := False;
                          IF NOT Signals[S].Signal_FailMsgWritten THEN
                            Log(LocoChipStr +  ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' because P=' + IntToStr(Device) + ' is straight');
                        END;
                      END ELSE BEGIN
                        { ActionCh = '-' }
                        IF Points[Device].Point_PresentState = Diverging THEN BEGIN
                          { the next point - if it should be straight, and it's diverging, it's a failure }
                          OK := False;
                          IF NOT Signals[S].Signal_FailMsgWritten THEN
                            Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' because P=' + IntToStr(Device) + ' is diverging');
                        END;
                      END;
                    END;
                    { Now lock them, if ok }
                    IF OK THEN BEGIN
                      LockPointBySignal(Device, S);
                      IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
                        IF Points[Device].Point_PresentState = Straight THEN
                          Log(LocoChipStr + ' S P=' + IntToStr(Device) + ' locked straight by S=' + IntToStr(S))
                        ELSE
                          Log(LocoChipStr + ' S P=' + IntToStr(Device) + ' locked diverging by S=' + IntToStr(S));
                      END;
                    END;
                  END;
                '|', '.':
                  BEGIN
                    IndicatorRequested := Signals[Device].Signal_IndicatorState;
                    { an indicator }
                    IF ActionCh = '|' THEN BEGIN
                      IF IndicatorRequested = NoIndicatorLit THEN BEGIN
                        IF NOT Signals[S].Signal_FailMsgWritten THEN
                          Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr
                                          + ' because indicator requested not set up');
                        OK := False;
                      END;
                    END ELSE
                      IF ActionCh = '.' THEN BEGIN
                        IF IndicatorRequested <> NoIndicatorLit THEN BEGIN
                          IF NOT Signals[S].Signal_FailMsgWritten THEN
                            Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr
                                            + ' because a different indicator is set');
                          OK := False;
                        END;
                      END;
                  END;
                '*':
                  { track circuit unoccupied - unless it's the one next the signal }
                  IF (TrackCircuits[Device].TC_OccupationState <> TCUnoccupied) AND (Lines[Signals[S].Signal_AdjacentLine].Line_TC <> Device) THEN BEGIN
                    IF NOT Signals[S].Signal_FailMsgWritten THEN
                      Log(LocoChipStr +  ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr
                                      + ' because TC=' + IntToStr(Device) + ' is occupied');
                    OK := False;
                  END;
              END; {CASE}
            END;
          END;
          Inc(LockListPos);
        END;
      END; {WHILE}

      { write out the first lock failure, or clear it }
      IF NOT OK AND ShowError AND NOT InAutoMode THEN
        DrawFailure(Device, ActionCh);
    END;
    Result := OK;
  END; { SignalLockingOK }

CONST
  EmergencyRouteing = True;
  IncludeOutOfUseLines = True;
  ShowError = True;
  SignalNotPoint = True;
  SuppressMessage = True;
  UnknownTrainLen = 0;
  WriteMessage = True;

VAR
  DebugStr : String;
  DraftRouteArray : StringArrayType;
  ErrorMsg : String;
  I : Integer;
  IndicatorToBeSet : Boolean;
  L : Integer;
  LinesNotAvailableStr : String;
  NewAspect : AspectType;
  NextBufferStop : Integer;
  NextSignal : Integer;
  OtherS : Integer;
  RouteLockingArray : IntegerArrayType;
  SemaphoreDistantHomesArrayPos : Integer;
  SemaphoreDistantSignalOffFound : Boolean;
  SemaphoreHomeSignalOnFound : Boolean;
  SignalPutativeStateStr : String;
  TempDestination : String;
  TempTheatreIndicatorString : String;
  TempS : Integer;

BEGIN
  OK := True;
  NextBufferStop := UnknownBufferStop;
  NextSignal := UnknownSignal;
  IndicatorToBeSet := (NewIndicatorState <> NoIndicatorLit);
  SignalPutativeStateStr := 'on';

  { See whether we've pulling the signal off or not }
  IF Signals[S].Signal_Aspect = RedAspect THEN
    { this includes semaphore distants that are on }
    SignalPutativeStateStr := 'off';

  IF Signals[S].Signal_OutOfUse THEN BEGIN
    IF NOT Signals[S].Signal_OutOfUseMsgWritten THEN BEGIN
      IF NOT User THEN
        Log('S Cannot change a signal (S=' + IntToStr(S) + ') which is out of use')
      ELSE
        Log('S+ User cannot change a signal (S=' + IntToStr(S) + ') which is out of use');
      Signals[S].Signal_OutOfUseMsgWritten := True;
    END;
    OK := False;
  END ELSE BEGIN
    { Is signal being locked by the user, and it's already locked by a route, or is it locked by any route but the current one? }
    IF InLockingMode
    AND (ResetTC = UnknownTrackCircuit)
    AND (((Route = UnknownRoute) AND SignalIsLockedByAnyRoute(S, RouteLockingArray))
         OR (SignalIsLockedByAnyRoute(S, RouteLockingArray)
         AND NOT SignalIsLockedBySpecificRoute(S, Route)))
    THEN BEGIN
      IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
        IF Length(RouteLockingArray) <> 0 THEN BEGIN
          IF NOT User THEN
            DebugStr := 'Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' as it is locked by'
          ELSE
            DebugStr := 'User cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' as it is locked by';
          IF Length(RouteLockingArray) = 1 THEN
            DebugStr := DebugStr + ' R=' + IntToStr(RouteLockingArray[0])
          ELSE
            FOR I := 0 TO High(RouteLockingArray) DO
              DebugStr := DebugStr + ' R=' + IntToStr(RouteLockingArray[I]);

          Log(LocoChipStr + ' R ' + DebugStr);
        END ELSE BEGIN
          IF NOT User THEN
            Log(LocoChipStr + ' RG Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr
                            + ' as it is locked by R=' + IntToStr(Route) + IfThen(SubRoute <> NoSubRoute,
                                                                                  IntToStr(SubRoute)))
          ELSE
            Log(LocoChipStr + ' RG User cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr
                            + ' as it is locked by R=' + IntToStr(Route) + IfThen(SubRoute <> NoSubRoute,
                                                                                  IntToStr(SubRoute)));
        END;
        Signals[S].Signal_FailMsgWritten := True;
      END;
      OK := False;
    END ELSE BEGIN
      IF InLockingMode AND SignalIsLockedByOppositePassingLoopSignal(S, OtherS) THEN BEGIN
        OK := False;
        IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
          IF NOT User THEN
            Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' as it is locked by opposite passing loop signal S=' + IntToStr(OtherS))
          ELSE
            Log(LocoChipStr + ' S User cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' as it is locked by opposite passing loop signal S=' + IntToStr(OtherS));
          Signals[S].Signal_FailMsgWritten := True;
          Forbid;
        END;
      END ELSE BEGIN
        IF InLockingMode AND SignalIsLockedByUser(S) AND (Route <> UnknownRoute) THEN BEGIN
          OK := False;
          IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
            Log(LocoChipStr + ' R R=' + IntToStr(Route) +
              IfThen(SubRoute <> NoSubRoute, IntToStr(SubRoute)) + ' cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' as it is locked by user');
            Signals[S].Signal_FailMsgWritten := True;
            Forbid;
          END;
        END ELSE BEGIN
          { Check that both the signals and the track-circuit occupation resetting them, if locked, are locked by the same route }
          IF InLockingMode AND (ResetTC <> UnknownTrackCircuit) THEN BEGIN
            IF SignalIsLockedByAnyRoute(S, RouteLockingArray) THEN BEGIN
              IF NOT IsElementInIntegerArray(RouteLockingArray, TrackCircuits[ResetTC].TC_LockedForRoute) THEN BEGIN
                IF NOT User THEN
                  DebugStr := 'Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' by TC=' + IntToStr(ResetTC) + ' as S=' + IntToStr(S) + ' is locked by'
                ELSE
                  DebugStr := 'User cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' by TC=' + IntToStr(ResetTC) + ' as S=' + IntToStr(S) + ' is locked by';
                FOR I := 0 TO High(RouteLockingArray) DO
                  DebugStr := DebugStr + ' R=' + IntToStr(RouteLockingArray[I]);
                IF TrackCircuits[ResetTC].TC_LockedForRoute <> UnknownRoute THEN
                  DebugStr := DebugStr + ' and TC=' + IntToStr(ResetTC) + ' is locked by R=' + IntToStr(TrackCircuits[ResetTC].TC_LockedForRoute);
                Log(LocoChipStr + ' R ' + DebugStr);
              END;
            END;
          END;

          IF Signals[S].Signal_Aspect = RedAspect THEN BEGIN
            { Pull signal off }
            DebugStr := 'Putatively setting S=' + IntToStr(S) + ' ';

            IF NewIndicatorState <> NoIndicatorLit THEN BEGIN
              CASE NewIndicatorState OF
                LeftIndicatorLit:
                  DebugStr := DebugStr + 'left indicator ';
                RightIndicatorLit:
                  DebugStr := DebugStr + 'right indicator ';
                UpperLeftIndicatorLit:
                  DebugStr := DebugStr + 'upper left indicator ';
                MiddleLeftIndicatorLit:
                  DebugStr := DebugStr + 'middle left indicator ';
                LowerLeftIndicatorLit:
                  DebugStr := DebugStr + 'lower left indicator ';
                UpperRightIndicatorLit:
                  DebugStr := DebugStr + 'upper right indicator ';
                MiddleRightIndicatorLit:
                  DebugStr := DebugStr + 'middle right indicator ';
                LowerRightIndicatorLit:
                  DebugStr := DebugStr + 'lower right indicator ';
                TheatreIndicatorLit:
                  BEGIN
                    DebugStr := DebugStr + 'theatre indicator to ';
                    IF GetLineAdjacentSignal(PlatformOrFiddleyardLine) <> UnknownSignal THEN
                      DebugStr := DebugStr + 'S=' + IntToStr(GetLineAdjacentSignal(PlatformOrFiddleyardLine)) + ' '
                    ELSE
                      DebugStr := DebugStr + 'BS=' + IntToStr(Lines[PlatformOrFiddleyardLine].Line_AdjacentBufferStop) + ' ';
                  END;
                QueryIndicatorLit:
                  DebugStr := DebugStr + 'query indicator ';
              END; {CASE}
            END;

            DebugStr := DebugStr + 'off';
            IF Route <> NoRoute THEN
              DebugStr := DebugStr + ' for R=' + IntToStr(Route) + IfThen(SubRoute <> NoSubRoute,
                                                                          '/' + IntToStr(SubRoute));
            IF NOT Signals[S].Signal_FailMsgWritten THEN
              Log(LocoChipStr + ' S ' + DebugStr);
            DebugStr := '';

            IF (Signals[S].Signal_IndicatorState = NoIndicatorLit) AND NOT Signals[S].Signal_ApproachLocked THEN BEGIN
              { need to see if a signal is approach locked here - if it is, the indicator will not (yet) be set }
              CASE NewIndicatorState OF
                UpperLeftIndicatorLit:
                  IF Signals[S].Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                    NextSignal := Signals[S].Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetSignal
                  ELSE
                    NextBufferStop := Signals[S].Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetBufferStop;
                MiddleLeftIndicatorLit:
                  IF Signals[S].Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                    NextSignal := Signals[S].Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetSignal
                  ELSE
                    NextBufferStop := Signals[S].Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetBufferStop;
                LowerLeftIndicatorLit:
                  IF Signals[S].Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                    NextSignal := Signals[S].Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetSignal
                  ELSE
                    NextBufferStop := Signals[S].Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetBufferStop;
                UpperRightIndicatorLit:
                  IF Signals[S].Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                    NextSignal := Signals[S].Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetSignal
                  ELSE
                    NextBufferStop := Signals[S].Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetBufferStop;
                MiddleRightIndicatorLit:
                  IF Signals[S].Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                    NextSignal := Signals[S].Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetSignal
                  ELSE
                    NextBufferStop := Signals[S].Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetBufferStop;
                LowerRightIndicatorLit:
                  IF Signals[S].Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                    NextSignal := Signals[S].Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetSignal
                  ELSE
                    NextBufferStop := Signals[S].Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetBufferStop;
              ELSE {CASE}
                NextSignal := UnknownSignal;
              END; {CASE}

              { We need to deal with semaphore distants here, as the conventional locking for home signals doesn't apply }
              IF Signals[S].Signal_Type = SemaphoreDistant THEN BEGIN
                { See if the signals that control the distant are off - if not then it's locked }
                SemaphoreDistantHomesArrayPos := 0;
                SemaphoreHomeSignalOnFound := False;
                WHILE (SemaphoreDistantHomesArrayPos <= High(Signals[S].Signal_SemaphoreDistantHomesArray)) AND NOT SemaphoreHomeSignalOnFound DO BEGIN
                  IF Signals[Signals[S].Signal_SemaphoreDistantHomesArray[SemaphoreDistantHomesArrayPos]].Signal_Aspect = RedAspect THEN
                    SemaphoreHomeSignalOnFound := True
                  ELSE
                    Inc(SemaphoreDistantHomesArrayPos);
                END; {WHILE}

                IF SemaphoreHomeSignalOnFound THEN BEGIN
                  OK := False;
                  DrawFailure(Signals[S].Signal_SemaphoreDistantHomesArray[SemaphoreDistantHomesArrayPos], '=');
                  IF InLockingMode THEN
                    Log(LocoChipStr + ' S Cannot pull off semaphore distant S=' + IntToStr(S)
                                    + ' as semaphore home signal S=' + IntToStr(Signals[S].Signal_SemaphoreDistantHomesArray[SemaphoreDistantHomesArrayPos]) + ' is on');
                  Signals[S].Signal_FailMsgWritten := True;
                END ELSE BEGIN
                  { we now have to lock the home signals that are off, which are then locked by the semaphore distant }
                  SemaphoreDistantHomesArrayPos := 0;
                  WHILE (SemaphoreDistantHomesArrayPos <= High(Signals[S].Signal_SemaphoreDistantHomesArray)) DO BEGIN
                    Signals[Signals[S].Signal_SemaphoreDistantHomesArray[SemaphoreDistantHomesArrayPos]].Signal_LockedBySemaphoreDistant := True;
                    Inc(SemaphoreDistantHomesArrayPos);
                  END; {WHILE}
                END;
              END ELSE BEGIN
                { We need to find the next signal - DraftRouteArray will contain the data from which the signal locking array is created }
                IF FindNextSignalOrBufferStop(S, NextSignal, NextBufferStop, IndicatorToBeSet, LinesNotAvailableStr, DraftRouteArray) THEN BEGIN
                  CreateLockingArrayFromDraftRouteArray(LocoChipStr, DraftRouteArray, Signals[S].Signal_RouteLockingNeededArray);
                  Signals[S].Signal_ResettingTC := GetResettingTrackCircuit(LocoChipStr, S, NOT SuppressMessage);
                  FindPreviousSignals(S, Signals[S].Signal_PreviousSignal1, Signals[S].Signal_PreviousSignal2);
                  Signals[S].Signal_FindNextSignalBufferStopMsgWritten := False;
                END ELSE BEGIN
                  { No signal or buffer stop found - should only reach here if the line is out of use ahead }
                  IF NOT Signals[S].Signal_FindNextSignalBufferStopMsgWritten THEN BEGIN
                    Log(LocoChipStr + ' SG Find next signal/bufferstop to S=' + IntToStr(S) + ' failed:');
                    Log(LocoChipStr + ' S ' + LinesNotAvailableStr + ' {WRAP=SCREENWIDTH}');
                    Signals[S].Signal_FindNextSignalBufferStopMsgWritten := True;
                  END;
                  OK := False;
                END;
              END;
            END ELSE BEGIN
              IF Signals[S].Signal_IndicatorState = QueryIndicatorLit THEN BEGIN
                { we need to set a specific route up, not just look for the next signal }
                IF NOT Signals[S].Signal_FailMsgWritten THEN
                  Log(LocoChipStr + ' S Finding a route for theatre indicator for S=' + IntToStr(S));

                { DraftRouteArray will also contain the data from which the signal locking array is created }
                FindRouteFromLineAToLineB(LocoChipStr, UnknownJourney, S, Signals[S].Signal_AdjacentLine, PlatformOrFiddleyardLine, Signals[S].Signal_Direction, TrainType,
                                          UnknownTrainLength, NOT EmergencyRouteing, NOT IncludeOutOfUseLines, DraftRouteArray, LinesNotAvailableStr, ErrorMsg, OK);
                IF NOT OK THEN BEGIN
                  { try to find a route with emergency routeing }
                  FindRouteFromLineAToLineB(LocoChipStr, UnknownJourney, S, Signals[S].Signal_AdjacentLine, PlatformOrFiddleyardLine, Signals[S].Signal_Direction, TrainType,
                                            UnknownTrainLength, EmergencyRouteing, NOT IncludeOutOfUseLines, DraftRouteArray, LinesNotAvailableStr, ErrorMsg, OK);
                  IF NOT OK AND NOT FindARouteFailMsgWritten THEN BEGIN
                    FindARouteFailMsgWritten := True;
                    Log(LocoChipStr + ' SG Find next signal to S=' + IntToStr(S) + ' failed in FindARoute:');
                    Log(LocoChipStr + ' S ' + LinesNotAvailableStr + ' (' + ErrorMsg + ')');
                  END;
                END;

                IF OK THEN BEGIN
                  L := ExtractLineFromString(DraftRouteArray[High(DraftRouteArray)]);
                  IF L <> UnknownLine THEN
                    IF GetLineAdjacentSignal(L) <> UnknownSignal THEN
                      AppendToStringArray(DraftRouteArray, 'FS=' + IntToStr(GetLineAdjacentSignal(L)));

                  IF Length(Signals[S].Signal_RouteLockingNeededArray) = 0 THEN
                    CreateLockingArrayFromDraftRouteArray(LocoChipStr, DraftRouteArray, Signals[S].Signal_RouteLockingNeededArray);

                  Signals[S].Signal_ResettingTC := GetResettingTrackCircuit(LocoChipStr, S, NOT SuppressMessage);
                  FindPreviousSignals(S, Signals[S].Signal_PreviousSignal1, Signals[S].Signal_PreviousSignal2);

                  { and extract the theatre indicator text }
                  IF Signals[S].Signal_RouteLockingNeededArray[1] = HoldMarker THEN
                    { the 'HOLD' marker occupies the second position in the string }
                    TempDestination := Copy(Signals[S].Signal_RouteLockingNeededArray[2], Pos('>', Signals[S].Signal_RouteLockingNeededArray[2]) + 1, 255)
                  ELSE
                    TempDestination := Copy(Signals[S].Signal_RouteLockingNeededArray[1], Pos('>', Signals[S].Signal_RouteLockingNeededArray[1]) + 1, 255);

                  IF Pos('FS=', TempDestination) > 0 THEN BEGIN
                    TempTheatreIndicatorString := Signals[ExtractSignalFromString(TempDestination)].Signal_AsTheatreDestination;
                    Log(LocoChipStr + ' R S=' + IntToStr(S) + ' theatre indicator ''' + TempTheatreIndicatorString
                                    + ''' set from Signal_AsTheatreDestination field in signal record for S=' + IntToStr(ExtractSignalFromString(TempDestination)));
                  END ELSE BEGIN
                    IF ExtractBufferStopFromString(TempDestination) <> UnknownBufferStop THEN
                      TempTheatreIndicatorString := BufferStops[ExtractBufferStopFromString(TempDestination)].BufferStop_AsTheatreDestination;
                    Log(LocoChipStr + ' R S=' + IntToStr(S) + ' theatre indicator ''' + TempTheatreIndicatorString
                                    + ''' set from BufferStop_AsTheatreDestination field in buffer stop record for BS='
                                    + IntToStr(ExtractBufferStopFromString(TempDestination)));
                  END;
                END;
              END ELSE
                FindARouteFailMsgWritten := False;
            END;
          END ELSE BEGIN
            { Aspect <> RedAspect : push signal on }

            SemaphoreDistantSignalOffFound := False;
            IF Signals[S].Signal_LockedBySemaphoreDistant THEN BEGIN
              IF Signals[Signals[S].Signal_SemaphoreDistantLocking].Signal_Aspect <> RedAspect THEN
                SemaphoreDistantSignalOffFound := True
              ELSE
                Signals[S].Signal_LockedBySemaphoreDistant := False;
            END;

            IF SemaphoreDistantSignalOffFound THEN BEGIN
              OK := False;
              DrawFailure(Signals[S].Signal_SemaphoreDistantLocking, '\');
              Log(LocoChipStr + ' S Cannot put on semaphore home S=' + IntToStr(S)
                              + ' as distant signal S=' + IntToStr(Signals[S].Signal_SemaphoreDistantLocking) + ' is off');
              Signals[S].Signal_FailMsgWritten := True;
            END;

            IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
              IF NOT User THEN
                DebugStr := 'Setting S=' + IntToStr(S) + ' on'
              ELSE
                DebugStr := 'User setting S=' + IntToStr(S) + ' on';
              IF Route <> NoRoute THEN
                DebugStr := DebugStr + ' for R=' + IntToStr(Route);
              Log(LocoChipStr + ' S ' + DebugStr);
            END;

            IF OK THEN BEGIN
              { make sure the preceding signal is not off - if it is, we can't set the current one on }
              IF Signals[S].Signal_PreviousSignal1 <> UnknownSignal THEN BEGIN
                SetLength(Signals[S].Signal_RouteLockingNeededArray, 1);
                Signals[S].Signal_RouteLockingNeededArray[0] := 'FS=' + IntToStr(Signals[S].Signal_PreviousSignal1) + '=';
              END ELSE BEGIN
                { it's ok }
                SetLength(Signals[S].Signal_RouteLockingNeededArray, 0);
                Signals[S].Signal_TheatreIndicatorString := '';
              END;
            END;
          END;

          IF OK AND InLockingMode THEN BEGIN
            { If there's nothing in the locking array, it's ok, otherwise test the locking }
            OK := SignalLockingOK(LocoChipStr, S, Signals[S].Signal_RouteLockingNeededArray, ShowError);
            IF OK AND (Signals[S].Signal_Aspect = RedAspect) THEN BEGIN
              Signals[S].Signal_StateChanged := True;
              IF Route = UnknownRoute THEN
                LockSignalByUser(S)
              ELSE
                LockSignalByRoute(LocoChipStr, S, Route, NOT WriteMessage);
            END;
          END;
        END;

        { Check that the previous signal is not a theatre in the process of being set up - a bit esoteric, this check, but necessary, as the previous signal could not
          otherwise be completely pulled off
        }
        IF OK AND InLockingMode THEN BEGIN
          IF (Signals[S].Signal_PreviousSignal1 <> UnknownSignal)
          AND (Signals[Signals[S].Signal_PreviousSignal1].Signal_Aspect = RedAspect)
          AND (Signals[Signals[S].Signal_PreviousSignal1].Signal_Indicator = TheatreIndicator)
          AND (Signals[Signals[S].Signal_PreviousSignal1].Signal_IndicatorState = TheatreIndicatorLit)
          AND (Signals[Signals[S].Signal_PreviousSignal1].Signal_TheatreIndicatorString = Signals[S].Signal_AsTheatreDestination)
          THEN BEGIN
            IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
              IF NOT User THEN
                Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' as previous signal ' + IntToStr(Signals[S].Signal_PreviousSignal1)
                                + '''s theatre indicator is on but the signal is not yet off')
              ELSE
                Log(LocoChipStr + ' S User cannot set S=' + IntToStr(S) + ' as previous signal ' + IntToStr(Signals[S].Signal_PreviousSignal1)
                                + '''s theatre indicator is on but the signal is not yet off');
            END;
            OK := False;
            DrawFailure(Signals[S].Signal_PreviousSignal1, 'T');
          END;
        END;

        IF NOT OK OR (Signals[S].Signal_Aspect <> RedAspect) THEN BEGIN
          { if the theatre indicator was set to query, set it back on }
          IF (Signals[S].Signal_IndicatorState = QueryIndicatorLit) THEN BEGIN
            SetIndicator(LocoChipStr, S, NoIndicatorLit, '', Route, User);
            IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
              IF NOT User THEN
                Log(LocoChipStr + ' S Turning off theatre query indication for S=' + IntToStr(S))
              ELSE
                Log(LocoChipStr + ' S User turning off theatre query indication for S=' + IntToStr(S));
            END;
          END;

          { and unlock any points we previously locked, either because the signal setting has failed, or because we're unsetting it }
          IF NOT OK AND NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
            Signals[S].Signal_FailMsgWritten := True;
            IF Signals[S].Signal_Aspect <> RedASpect THEN BEGIN
              IF NOT User THEN
                DebugStr := 'Setting S=' + IntToStr(S) + ' on failed'
              ELSE
                DebugStr := 'User setting S=' + IntToStr(S) + ' on failed'
            END ELSE BEGIN
              IF NOT User THEN
                DebugStr := 'Setting S=' + IntToStr(S) + ' off failed'
              ELSE
                DebugStr := 'User setting S=' + IntToStr(S) + ' off failed';
            END;
            IF Route <> NoRoute THEN
              DebugStr := DebugStr + ' for R=' + IntToStr(Route);
            Log(LocoChipStr + ' S ' + DebugStr)
          END;

          UnlockPointsLockedBySignal(S);
          UnlockSignalLockedByUser(S);
        END;

        IF NOT OK AND InLockingMode THEN BEGIN
          SetLength(Signals[S].Signal_RouteLockingNeededArray, 0);
          IF (Route <> UnknownRoute) AND Routes_RouteSettingsInProgress[Route] THEN BEGIN
            Signals[S].Signal_StateChanged := False;
            Forbid;
          END;
        END ELSE BEGIN
          { If we're route-setting, and approach control mode has been set, store the signals and don't set them yet }
          IF (Route <> UnknownRoute) AND Routes_RouteSettingsInProgress[Route] AND Routes_ApproachControlsSet[Route] THEN BEGIN
            Log(LocoChipStr + ' R ' + SettingString + ' added to approach setting signals list for R=' + IntToStr(Route) + ' and not yet set off');
            AppendToStringArray(Routes_ApproachControlSignalsWaitingToBeSet[Route], SettingString);
            Signals[S].Signal_ApproachLocked := True;
            Signals[S].Signal_PostColour := clAqua;
            DrawSignalPost(S);
            WriteStringArrayToLog(LocoChipToStr(Routes_LocoChips[Route]), 'R', 'Signals held by approach control for R=' + IntToStr(Route) + ':',
                                                                Routes_ApproachControlSignalsWaitingToBeSet[Route]);
          END ELSE BEGIN
            { Either deal with route and theatre setting ... }
            IF NewIndicatorState <> NoIndicatorLit THEN BEGIN
              IF IndicatorToBeSet THEN
                SetIndicator(LocoChipStr, S, NewIndicatorState, TempTheatreIndicatorString, Route, user)
              ELSE
                SetIndicator(LocoChipStr, S, NoIndicatorLit, '', Route, User);
            { ... or with query theatre indicator setting ... }
            END ELSE BEGIN
              IF (Signals[S].Signal_IndicatorState = QueryIndicatorLit) THEN BEGIN
                SetIndicator(LocoChipStr, S, NewIndicatorState, TempTheatreIndicatorString, Route, user)
              END ELSE BEGIN
                { ... or with signal setting - if no particular aspect selected already, these are the defaults }
                IF Signals[S].Signal_Aspect = RedAspect THEN BEGIN
                  IF (Signals[S].Signal_Type = TwoAspect)
                  THEN
                    NewAspect := GreenAspect
                  ELSE
                    NewAspect := SingleYellowAspect;
                END ELSE
                  NewAspect := RedAspect;

                SetSignal(LocoChipStr, S, NewAspect, NOT LogSignalData, NOT ForceAWrite);
                IF Route <> NoRoute THEN
                  DebugStr := DebugStr + ' for R=' + IntToStr(Route);
                Log(LocoChipStr + ' S ' + DebugStr);

                { reset several things }
                Signals[S].Signal_FailMsgWritten := False;
                SetLength(Signals[S].Signal_RouteLockingNeededArray, 0);

                IF (Signals[S].Signal_PreviousSignal1 <> UnknownSignal) AND (Signals[S].Signal_Type <> SemaphoreDistant) THEN
                  SetPreviousSignals(LocoChipStr, S);

                { If the signal is being reset to on, reset the other data too }
                IF NewAspect = RedAspect THEN BEGIN
                  IF Signals[S].Signal_IndicatorState <> NoIndicatorLit THEN
                    SetIndicator(LocoChipStr, S, NoIndicatorLit, '', Route, user);
                  Signals[S].Signal_PreviousSignal1 := UnknownSignal;
                  Signals[S].Signal_PreviousSignal2 := UnknownSignal;

                  { and see if it affects any other signal's previous signals - otherwise, a previous signal may be reset, then be set to off for a different route, and our
                    signal would treat the previous signal as still being off, and wouldn't reset.
                  }
                  FOR TempS := 0 TO High(Signals) DO BEGIN
                    IF Signals[TempS].Signal_PreviousSignal1 = S THEN
                      Signals[TempS].Signal_PreviousSignal1 := UnknownSignal;
                    IF Signals[TempS].Signal_PreviousSignal2 = S THEN
                      Signals[TempS].Signal_PreviousSignal2 := UnknownSignal;
                  END; {FOR}
                END;
              END;
            END;
          END;
        END;
      END;
    END;
  END;
END; { PullSignalMainProcedure }

PROCEDURE PullSignal{1}(LocoChipStr : String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        SettingString : String; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal }
CONST
  ResetTC = UnknownTrackCircuit;

BEGIN
  PullSignalMainProcedure(LocoChipStr, S, NewIndicatorState, Route, SubRoute, PlatformOrFiddleyardLine, SettingString, ResetTC, UnknownTrainType, User, OK);
END; { PullSignal-1 }

PROCEDURE PullSignal{2}(LocoChipStr : String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        TrainTypeForRouteing : TypeOfTrainType; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal }
CONST
  NoSettingString = '';
  ResetTC = UnknownTrackCircuit;

BEGIN
  PullSignalMainProcedure(LocoChipStr, S, NewIndicatorState, Route, SubRoute, PlatformOrFiddleyardLine, NoSettingString, ResetTC, TrainTypeForRouteing, User, OK);
END; { PullSignal-2 }

PROCEDURE PullSignal{3}(LocoChipStr : String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer; ResetTC : Integer;
                        User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal. This version is only used by signals resetting track circuits, which can happen even if the signal is locked by a route }
CONST
  NoSettingString = '';

BEGIN
  PullSignalMainProcedure(LocoChipStr, S, NewIndicatorState, Route, SubRoute, PlatformOrFiddleyardLine, NoSettingString, ResetTC, UnknownTrainType, User, OK);
END; { PullSignal-3 }

PROCEDURE PullSignal{4}(LocoChipStr : String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        SettingString : String; TrainTypeForRouteing : TypeOfTrainType; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal; includes the original setting string for saving if necessary }
CONST
  ResetTC = UnknownTrackCircuit;

BEGIN
  PullSignalMainProcedure(LocoChipStr, S, NewIndicatorState, Route, SubRoute, PlatformOrFiddleyardLine, SettingString, ResetTC, TrainTypeForRouteing, User, OK);
END; { PullSignal-4 }

FUNCTION SignalIsLockedBySpecificRoute(S, Route : Integer) : Boolean;
{ Returns true if the signal is locked by the given route }
VAR
  LockCount : Integer;

BEGIN
  Result := False;
  WITH Signals[S] DO BEGIN
    IF Route <> UnknownRoute THEN BEGIN
      { see if the signal is locked by the given route }
      LockCount := 0;
      WHILE (LockCount < Length(Signal_LockedArray)) AND (Result = False) DO BEGIN
        IF (Signal_LockedArray[LockCount] = 'R=' + IntToStr(Route)) THEN
          Result := True;
        Inc(LockCount);
      END; {WHILE}
    END;
  END; {WITH}
END; { SignalIsLockedBySpecificRoute }

FUNCTION SignalIsLockedByAnyRoute(S : Integer; OUT RouteLockingArray : IntegerArrayType) : Boolean;
{ Returns true if the signal is locked by any route (in which case the routes doing the locking are returned in RouteLockingArray) }
VAR
  LockCount : Integer;

BEGIN
  SetLength(RouteLockingArray, 0);
  Result := False;
  WITH Signals[S] DO BEGIN
    { see if the signal is locked by any route }
    LockCount := 0;
    WHILE LockCount < Length(Signal_LockedArray) DO BEGIN
      IF Pos('R=', Signal_LockedArray[LockCount]) > 0 THEN BEGIN
        Result := True;
        AppendToIntegerArray(RouteLockingArray, ExtractRouteFromString(Signal_LockedArray[LockCount]));
      END;
      Inc(LockCount);
    END; {WHILE}
  END; {WITH}
END; { SignalIsLockedByAnyRoute }

FUNCTION SignalIsLockedByOppositePassingLoopSignal(S : Integer; OUT OtherS : Integer) : Boolean;
{ Returns true if an opposite passing loop signal is off }
BEGIN
  Result := False;
  IF Signals[S].Signal_OppositePassingLoopSignal <> UnknownSignal THEN BEGIN
    IF Signals[Signals[S].Signal_OppositePassingLoopSignal].Signal_Aspect <> RedAspect THEN BEGIN
      Result := True;
      OtherS := Signals[S].Signal_OppositePassingLoopSignal;
    END;
  END;
END; { SignalisLockedByOppositePassingLoopSignal }

FUNCTION SignalIsLocked(S : Integer; OUT LockingMsg : String) : Boolean;
{ Returns true if the signal is locked }
VAR
  I : Integer;
  OtherS : Integer;
  Route : Integer;
  RoutePos : Integer;
  SArrayCount : Integer;

BEGIN
  LockingMsg := '';

  IF Length(Signals[S].Signal_LockedArray) > 0 THEN BEGIN
    FOR SArrayCount := 0 TO High(Signals[S].Signal_LockedArray) DO BEGIN
      RoutePos := Pos('R', Signals[S].Signal_LockedArray[SArrayCount]);
      IF RoutePos > 0 THEN BEGIN
        Route := ExtractRouteFromString(Signals[S].Signal_LockedArray[SArrayCount]);
        IF (Route = UnknownRoute) OR (Length(Routes_LocoChips) = 0) THEN
          LockingMsg := ' ' + Signals[S].Signal_LockedArray[SArrayCount]
        ELSE
          LockingMsg := ' ' + Signals[S].Signal_LockedArray[SArrayCount] + ' (' + LocoChipToStr(Routes_LocoChips[Route]) + ')';
      END ELSE
        IF Pos('USER', Signals[S].Signal_LockedArray[SArrayCount]) > 0 THEN
          LockingMsg := ' ' + Signals[S].Signal_LockedArray[SArrayCount]
    END;
  END;

  IF Signals[S].Signal_ApproachLocked THEN
    LockingMsg := LockingMsg + ' AC';

  IF Signals[S].Signal_Type = SemaphoreDistant THEN BEGIN
    FOR I := 0 TO High(Signals[S].Signal_SemaphoreDistantHomesArray) DO BEGIN
      IF Signals[Signals[S].Signal_SemaphoreDistantHomesArray[I]].Signal_Aspect = RedAspect THEN
        LockingMsg := LockingMsg + ' S' + IntToStr(Signals[S].Signal_SemaphoreDistantHomesArray[I]);
    END; {FOR}
  END;

  IF Signals[S].Signal_LockedBySemaphoreDistant THEN
    LockingMsg := LockingMsg + ' S' + IntToStr(Signals[S].Signal_SemaphoreDistantLocking);

  IF SignalIsLockedByOppositePassingLoopSignal(S, OtherS) THEN
    LockingMsg := ' OPLS (S' + IntToStr(Others) + ')';

  IF LockingMsg = '' THEN BEGIN
    LockingMsg := 'not locked';
    Result := False;
  END ELSE BEGIN
    LockingMsg := 'locked by' + LockingMsg;
    Result := True;
  END;
END; { SignalIsLocked }

PROCEDURE UnlockSignalLockedBySpecificRoute(S, Route : Integer);
{ Remove the locking }
VAR
  ArrayCount : Integer;
  RouteFound : Boolean;

BEGIN
  { First find the array element to delete }
  ArrayCount := -1;
  RouteFound := False;
  WHILE (ArrayCount < High(Signals[S].Signal_LockedArray)) AND NOT RouteFound DO BEGIN
    Inc(ArrayCount);
    IF Pos('R=' + IntToStr(Route), Signals[S].Signal_LockedArray[ArrayCount]) > 0 THEN
      RouteFound := True;
  END; {WHILE}

  IF RouteFound THEN BEGIN
    DeleteElementFromStringArray(Signals[S].Signal_LockedArray, ArrayCount);
    Log('S S=' + IntToStr(S) + ' now unlocked by R=' + IntToStr(Route));
  END;
END; { UnlockSignalLockedBySpecificRoute }

PROCEDURE UnlockSignalLockedByUser(S : Integer);
{ Remove the locking }
VAR
  ArrayCount : Integer;
  UserLockingFound : Boolean;

BEGIN
  { First find the array element to delete }
  ArrayCount := -1;
  UserLockingFound := False;
  WHILE (ArrayCount < High(Signals[S].Signal_LockedArray)) AND NOT UserLockingFound DO BEGIN
    Inc(ArrayCount);
    IF Pos('USER', Signals[S].Signal_LockedArray[ArrayCount]) > 0 THEN
      UserLockingFound := True;
  END; {WHILE}

  IF UserLockingFound THEN BEGIN
    DeleteElementFromStringArray(Signals[S].Signal_LockedArray, ArrayCount);
    Log('S S=' + IntToStr(S) + ' now unlocked by user ');
  END;
END; { UnlockSignalLockedBySpecificRoute }

FUNCTION ValidateSignalAccessoryAddress(Str : String; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder address for a TrainTech S3 accessory decoder. This test must be done after Signal Type is validated. }
BEGIN
  ErrorMsg := '';
  Result := 0;

  IF Str <> '' THEN
    IF NOT TryStrToInt(Trim(Str), Result) THEN
      ErrorMsg := 'ValidateSignalAccessoryAddress: invalid signal accessory address integer String "' + Str + '"'
    ELSE
      IF (SignalType <> SemaphoreHome) AND (SignalType <> SemaphoreDistant) THEN
        ErrorMsg := 'ValidateSignalAccessoryAddress: cannot use TrainTech SC3 accessory addresses with non-semaphore signals';
END; { ValidateSignalAccessoryAddress }

FUNCTION ValidateSignalAdjacentLine(SignalBeingValidated : Integer; Str : String; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the line next to a signal }
VAR
  S : Integer;

BEGIN
  ErrorMsg := '';

  Result := StrToLine(Str);
  IF Result = UnknownLine THEN
    ErrorMsg := 'ValidateSignalAdjacentLine: unknown line';

  IF ErrorMsg = '' THEN BEGIN
    { Check that there isn't more than one signal on the same bit of line - it causes problems }
    S := 0;
    WHILE S < - High(Signals) DO BEGIN
      IF S <> SignalBeingValidated THEN BEGIN
        IF Signals[S].Signal_AdjacentLine = Result THEN
          ErrorMsg := 'ValidateSignalAdjacentLine: S=' + IntToStr(SignalBeingValidated) + ':'
                      + ' adjacent line ' +  LineToStr(Signals[S].Signal_AdjacentLine) + ' is already in use by another signal (S=' + IntToStr(S) + ')';
      END;
      Inc(S);
    END; {WHILE}
  END;
END; { ValidateSignalAdjacentLine }

FUNCTION ValidateSignalApproachControlAspect(Str : String; OUT ErrorMsg : String) : AspectType;
{ Validates and if ok returns the approach control signal aspect }
BEGIN
  ErrorMsg := '';
  Result := NoAspect;

  Str := UpperCase(Str);
  IF Str <> '' THEN BEGIN
    IF Str = 'NONE' THEN
      Result := NoAspect
    ELSE
      IF Str = 'RED' THEN
        Result := RedAspect
      ELSE
        IF Str = 'SINGLE YELLOW' THEN
          Result := SingleYellowAspect
        ELSE
          ErrorMsg := 'ValidateSignalApproachControlAspect: invalid approach control aspect';
  END;
END; { ValidateSignalApproachControlAspect }

FUNCTION ValidateSignalAsTheatreDestination(Str : String; OUT ErrorMsg : String) : String;
{ Validates and if ok returns the one or two character display used in a theatre indicator }
BEGIN
  ErrorMsg := '';
  Result := '';

  IF Length(Str) > 2 THEN
    ErrorMsg := 'ValidateSignalAsTheatreDestination: Theatre Destination "' + Str + '" is more than two characters long'
  ELSE
    Result := Str;
END; { ValidateSignalAsTheatreDestination }

FUNCTION ValidateSignalDecoderNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for a signal decoder. This check must be done after Accessory Address and Signal Type are validated. }
BEGIN
  ErrorMsg := '';
  Result := 0;

  IF Str <> '' THEN
    IF NOT TryStrToInt(Trim(Str), Result) THEN
      ErrorMsg := 'ValidateSignalDecoderNum: invalid signal decoder integer String "' + Str + '"'
    ELSE
      IF AccessoryAddress <> 0 THEN
        ErrorMsg := 'ValidateSignalDecoderNum: cannot combine signal decoders and TrainTech SC3 accessory addresses'
      ELSE
        IF (SignalType = SemaphoreHome) OR (SignalType = SemaphoreDistant) THEN
          ErrorMsg := 'ValidateSignalDecoderNum: cannot use function decoder addresses with semaphore signals';
END; { ValidateSignalDecoderNum }

FUNCTION ValidateSignalDirection(Str : String; OUT ErrorMsg : String) : DirectionType;
{ Validates and if ok returns the signal direction }
BEGIN
  ErrorMsg := '';

  Result := StrToDirectionType(Str);
  IF Result = UnknownDirection THEN
    ErrorMsg := 'ValidateSignalDirection: unknown signal direction';
END; { ValidateSignalDirection }

FUNCTION ValidateSignalIndicatorSpeedRestriction(Str : String; Indicator : IndicatorType; OUT ErrorMsg : String) : MPHType;
{ Validates and if ok returns what the tempoarary speed restriction is. This test must be carried outr after Indicator is validated. }
VAR
  TempInt : Integer;

BEGIN
  ErrorMsg := '';
  Result := NoSpecifiedSpeed;

  IF Str <> '' THEN BEGIN
    IF TryStrToInt(Trim(Str), TempInt) THEN
      Result := IntToMPH(TempInt)
    ELSE
      ErrorMsg := 'ValidateSignalIndicatorSpeedRestriction: invalid integer String "' + Str + '"';

    IF ErrorMsg = '' THEN
      IF (Result <> NoSpecifiedSpeed) AND (Indicator = NoIndicator) THEN
        ErrorMsg := 'ValidateSignalIndicatorSpeedRestriction: cannot have an indicator speed restriction if there is no indicator';
  END;
END; { ValidateSignalIndicatorSpeedRestriction }

FUNCTION ValidateSignalJunctionIndicators1(Str, FieldName : String; Signal_Indicator : IndicatorType; OUT ErrorMsg : String) : JunctionIndicatorRec;
{ The first part of verifying whether junction indicators are correctly set up; this part also returns the values for each junction indicator. This test requires that
  Indicator has been validated first
}
BEGIN
  ErrorMsg := '';
  Result.JunctionIndicator_Exists := False;
  Result.JunctionIndicator_TargetSignal := UnknownSignal;
  Result.JunctionIndicator_TargetBufferStop := UnknownBufferStop;

  Str := UpperCase(Str);
  IF Str <> '' THEN BEGIN
    IF (Copy(Str, 1, 1) <> 'S') AND (Copy(Str, 1, 2) <> 'BS') THEN
      ErrorMsg := 'ValidateSignalJunctionIndicators1: invalid target signal (S) or bufferstop (BS) for ' + FieldName + ': "' + Str + '"'
    ELSE
      IF Copy(Str, 1, 1) = 'S' THEN BEGIN
        Str := Copy(Str, 2);
        IF (Str = '') OR NOT TryStrToInt(Trim(Str), Result.JunctionIndicator_TargetSignal) THEN
          ErrorMsg := 'ValidateSignalJunctionIndicators1: invalid target signal (S) or bufferstop (BS) for ' + FieldName + ': "' + Str + '"';
      END
      ELSE
        IF Copy(Str, 1, 2) = 'BS' THEN BEGIN
          Str := Copy(Str, 3);
          IF (Str = '') OR NOT TryStrToInt(Trim(Str), Result.JunctionIndicator_TargetBufferStop) THEN
            ErrorMsg := 'ValidateSignalJunctionIndicators1: invalid target signal (S) or bufferstop (BS) for ' + FieldName + ': "' + Str + '"';
        END;

    IF ErrorMsg = '' THEN
      Result.JunctionIndicator_Exists := True;
  END;
END; { ValidateSignalJunctionIndicators1 }

PROCEDURE ValidateSignalJunctionIndicators2(StrArray : ARRAY OF String; SignalIndicator : IndicatorType; SignalJunctionIndicators : ARRAY OF JunctionIndicatorRec;
                                            OUT ErrorMsg : String);
{ The second part of verifying whether junction indicators are correctly set up }
VAR
  I, J : Integer;
  IndicatorsFound : Boolean;

BEGIN
  ErrorMsg := '';

  { See if any of the signal indicator data is duplicated }
  I := 0;
  WHILE (I <= High(StrArray)) AND (ErrorMsg = '') DO BEGIN
    J := 0;
    WHILE (J <= High(StrArray)) AND (ErrorMsg = '') DO BEGIN
      IF I <> J THEN BEGIN
        IF (StrArray[I] <> '') AND (StrArray[I] = StrArray[J]) THEN
          ErrorMsg := 'ValidateSignalJunctionIndicators2: identical signal/bufferstopdata (' + StrArray[I] + ') in indicator fields';
      END;
      Inc(J);
    END; {WHILE}
    Inc(I);
  END; {WHILE}

  IF ErrorMsg = '' THEN BEGIN
    IndicatorsFound := False;
    FOR I := 0 TO 5 DO BEGIN
      IF SignalJunctionIndicators[I].JunctionIndicator_Exists THEN
        IndicatorsFound := True;
    END; {FOR}

    IF (SignalIndicator = JunctionIndicator) AND NOT IndicatorsFound THEN
      ErrorMsg := 'ValidateSignalJunctionIndicators2: signal indicator type is set to Junction Indicator even though there is no indicator data'
    ELSE
      IF (SignalIndicator <> JunctionIndicator) AND IndicatorsFound THEN
        ErrorMsg := 'ValidateSignalJunctionIndicators2: signal indicator type is not set to Junction Indicator even though there is indicator data';
  END;
END; { ValidateSignalJunctionIndicators2 }

FUNCTION ValidateNextSignalIfNoIndicator(Str : String; Init : Boolean; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns what the other signal is if no indicator is lit }
BEGIN
  ErrorMsg := '';
  Result := UnknownSignal;

  Str := UpperCase(Str);
  IF Str <> '' THEN BEGIN
    IF Copy(Str, 1, 1) = 'S' THEN
      Str := Copy(Str, 2);

    IF NOT TryStrToInt(Trim(Str), Result) THEN
      ErrorMsg := 'ValidateNextSignalIfNoIndicator: invalid signal integer string "' + Str + '"';
  END;
END; { ValidateNextSignalIfNoIndicator }

FUNCTION ValidateSignalNum(SignalToTest : Integer) : String;
{ Validates a signal number. This has to be carried out separately from other validation, as when creating signals a reference may be made to a signal not yet created }
BEGIN
  Result := '';

  IF (SignalToTest <> UnknownSignal) AND (SignalToTest > High(Signals)) THEN
    Result := 'ValidateSignalNum: cross-reference to invalid signal number "' + IntToStr(SignalToTest) + '"';
END; { ValidateSignalNum }

FUNCTION ValidateSignalIndicator(Str : String; OUT ErrorMsg : String) : IndicatorType;
{ Validates and if ok returns the signal indicator }
BEGIN
  ErrorMsg := '';
  Result := NoIndicator;

  Str := UpperCase(Str);
  IF Str <> '' THEN BEGIN
    CASE Str[1] OF
      'J':
        Result := JunctionIndicator;
      'T':
        Result := TheatreIndicator;
      ELSE { CASE }
        ErrorMsg := 'ValidateSignalIndicator: invalid indicator';
    END; { CASE }
  END;
END; { ValidateSignalIndicator }

FUNCTION ValidateSignalIndicatorDecoderNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for an indicator decoder. This check must be done after Accessory Address and Signal Type are validated. }
BEGIN
  ErrorMsg := '';
  Result := 0;

  IF Str <> '' THEN
    IF NOT TryStrToInt(Trim(Str), Result) THEN
      ErrorMsg := 'ValidateSignalIndicatorDecoderNum: invalid signal indicator decoder integer String "' + Str + '"'
    ELSE
      IF AccessoryAddress <> 0 THEN
        ErrorMsg := 'ValidateSignalIndicatorDecoderNum: cannot combine signal decoders and TrainTech SC3 accessory addresses'
      ELSE
        IF (SignalType = SemaphoreHome) OR (SignalType = SemaphoreDistant) THEN
          ErrorMsg := 'ValidateSignalIndicatorDecoderNum: cannot use function decoder addresses with semaphore signals';
END; { ValidateSignalIndicatorDecoderNum }

FUNCTION ValidateSignalIndicatorDecoderFunctionNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for an indicator function decoder. This check must be done after Accessory Address and Signal Type are validated. }
BEGIN
  ErrorMsg := '';
  Result := 0;

  IF Str <> '' THEN
    IF NOT TryStrToInt(Trim(Str), Result) THEN
      ErrorMsg := 'ValidateSignalIndicatorDecoderFunctionNum: invalid signal indicator decoder function integer String "' + Str + '"'
    ELSE
      IF AccessoryAddress <> 0 THEN
        ErrorMsg := 'ValidateSignalIndicatorDecoderFunctionNum: cannot combine signal decoders and TrainTech SC3 accessory addresses'
      ELSE
        IF (SignalType = SemaphoreHome) OR (SignalType = SemaphoreDistant) THEN
          ErrorMsg := 'ValidateSignalIndicatorDecoderFunctionNum: cannot use function decoder addresses with semaphore signals';
END; { ValidateSignalIndicatorDecoderFunctionNum }

FUNCTION ValidateSignalLocationsToMonitorArray(Str : String; PossibleRouteHold : Boolean; OUT ErrorMsg : String) : IntegerArrayType;
{ Validates and if ok returns the signal locations monitored when a route is held. This test must be done after Possible Route Hold is validated. }
VAR
  I : Integer;
  PlatformDataOK : Boolean;
  TempLocation : Integer;
  TempLocationsToMonitorStrArray : StringArrayType;

BEGIN
  ErrorMsg := '';
  SetLength(Result, 0);

  IF Str <> '' THEN BEGIN
    IF NOT PossibleRouteHold THEN
      ErrorMsg := 'ValidateSignalLocationsToMonitorArray: cannot monitor locations without a route hold'
    ELSE BEGIN
      ExtractSubStringsFromString(Str, ',', TempLocationsToMonitorStrArray);
      I := 0;
      PlatformDataOK := True;
      WHILE (I <= High(TempLocationsToMonitorStrArray)) AND PlatformDataOK DO BEGIN
        TempLocation := StrToLocation(TempLocationsToMonitorStrArray[I]);
        IF TempLocation = UnknownLocation THEN BEGIN
          PlatformDataOK := False;
          ErrorMsg := 'ValidateSignalLocationsToMonitorArray: unknown location "' + TempLocationsToMonitorStrArray[I] + '" in Signal Locations To Monitor';
        END ELSE
          AppendToLocationArray(Result, TempLocation);
        Inc(I);
      END; {WHILE}
    END;
  END;
END; { ValidateSignalLocationsToMonitorArray }

FUNCTION ValidateSignalOppositePassingLoopSignal(Str : String; Init : Boolean; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the other signal involved in a passing loop }
BEGIN
  ErrorMsg := '';
  Result := UnknownSignal;

  Str := UpperCase(Str);

  IF Str <> '' THEN BEGIN
    IF Copy(Str, 1, 1) = 'S' THEN
      Str := Copy(Str, 2);

    IF NOT TryStrToInt(Trim(Str), Result) THEN
      ErrorMsg := 'ValidateSignalOppositePassingLoopSignal: invalid signal integer string "' + Str + '"';
  END;
END; { ValidateSignalOppositePassingLoopSignal }

FUNCTION ValidateSignalOutOfUseAndAddAdjacentTC(Flag : Boolean; AdjacentLine : Integer; OUT AdjacentTC : Integer; OUT ErrorMsg : String) : Boolean;
{ Validates and if ok returns true if a signal is marked as not in use. This test must be done after Adjacent Signal is validated. }
BEGIN

  ErrorMsg := '';
  Result := Flag;

  { Note which lines and track circuits are next the signal }
  IF NOT Flag THEN BEGIN
    AdjacentTC := Lines[AdjacentLine].Line_TC;
    IF AdjacentTC = UnknownTrackCircuit THEN
      ErrorMsg := 'ValidateSignalOutOfUse: adjacent to line' + ' ' + LineToStr(AdjacentLine) + ') has no adjacent track circuit';
  END;
END; { ValidateSignalOutOfUseAndAddAdjacentTC }

FUNCTION ValidateSignalPossibleStationStartRouteHold(Flag : Boolean; PossibleRouteHold : Boolean; OUT ErrorMsg : String) : Boolean;
{ Validates and if ok returns whether a station start route hold is in operation. This test must be done after Possible Route Hold is validated. }
BEGIN
  ErrorMsg := '';
  Result := Flag;

  IF PossibleRouteHold AND Flag THEN
    { both shouldn't be ticked }
    ErrorMsg := 'ValidateSignalPossibleStationStartRouteHold: route hold and station start route hold are both ticked';
END; { ValidateSignalPossibleStationStartRouteHold }

FUNCTION ValidateSignalQuadrant(Str : String; OUT ErrorMsg : String) : QuadrantType;
{ Validates and if ok returns the quadrant type }
BEGIN
  ErrorMsg := '';
  Result := NoQuadrant;

  Str := UpperCase(Str);

  IF Str = 'UPPER' THEN
    Result := UpperQuadrant
  ELSE
    IF Str = 'LOWER' THEN
      Result := LowerQuadrant
    ELSE
      IF Str <> '' THEN
        ErrorMsg := 'Invalid signal quadrant type';
END; { ValidateSignalQuadrant }

FUNCTION ValidateSignalType(Str : String; OUT Quadrant : QuadrantType; DistantHomesArray : IntegerArrayType; OUT ErrorMsg : String) : TypeOfSignal;
{ Validates and if ok returns the signal type }
VAR
  QuadrantStr : String;

BEGIN
  ErrorMsg := '';
  Result := StrToSignalType(Str);

  IF Result = UnknownSignalType THEN
    ErrorMsg := 'ValidateSignalType: invalid signal type';

  IF (Result = CallingOn) OR (Result = TwoAspect) OR (Result = ThreeAspect) OR (Result = FourAspect) THEN BEGIN
    IF Quadrant <> NoQuadrant THEN BEGIN
      IF MessageDialogueWithDefault('A non-semaphore signal cannot have a quadrant'
                                    + CRLF
                                    + 'Do you wish to remove the quadrant?',
                                    StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
      THEN
        ErrorMsg := 'ValidateSignalType: non-semaphore signals cannot have upper or lower quadrants'
      ELSE
        Quadrant := NoQuadrant;

      WriteSignalValuesToValueList;
    END;
  END ELSE
    IF (Result = SemaphoreDistant) OR (Result = SemaphoreHome) THEN BEGIN
      IF Quadrant = NoQuadrant THEN BEGIN
        QuadrantStr := InputBox('Quadrant Type', 'A semaphore signal must have a quadrant. Enter "U" for upper quadrant or "L" for lower quadrant', 'L');
        IF UpperCase(QuadrantStr) = 'U' THEN
          Quadrant := UpperQuadrant
        ELSE
          Quadrant := LowerQuadrant;

        WriteSignalValuesToValueList;
      END;
    END;

  IF Length(DistantHomesArray) <> 0 THEN
    IF Result <> SemaphoreDistant THEN
      ErrorMsg := 'ValidateSignalType: only semaphore distants can have "Distant Homes"';
END; { ValidateSignalType }

FUNCTION ValidateSignalAdjacentLineXOffset(Str : String; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns a signal's on-screen adjustment if any }
BEGIN
  ErrorMsg := '';

  IF Str = '' THEN
    Result := -1
  ELSE
    IF NOT TryStrToInt(Trim(Str), Result) THEN
      ErrorMsg := 'ValidateSignalAdjacentLineXOffset: Invalid integer String "' + Str + '"';
END; { ValidateSignalAdjacentLineXOffset }

FUNCTION ValidateIndicatorDestinations(S : Integer) : String;
{ Validates whether the indicator destinations are valid. This test can only be done once all the signal data has been read in. }
VAR
  TempJunctionIndicator : JunctionIndicatorType;

BEGIN
  Result := '';

  WITH Signals[S] DO BEGIN
    FOR TempJunctionIndicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
      IF Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_Exists THEN
        IF (Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetSignal <> UnknownSignal)
        AND ((Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetSignal < 0)
              OR
             (Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetSignal > High(Signals)))
        THEN
          Result := ' target for its ' + JunctionIndicatorTypeToStr(TempJunctionIndicator) + ' indicator'
                    + ' (S' + IntToStr(Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetSignal) + ')' + ' is not a valid signal number'
        ELSE
          IF (Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop)
          AND ((Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetBufferStop < 0)
               OR
               (Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetBufferStop > High(BufferStops)))
          THEN
            Result := ' target for its ' + JunctionIndicatorTypeToStr(TempJunctionIndicator) + ' indicator'
                      + ' (BS' + IntToStr(Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetBufferStop) + ')' + ' is not a valid buffer stop number';

      IF (Signal_NextSignalIfNoIndicator <> UnknownSignal) AND ((Signal_NextSignalIfNoIndicator < 0) OR (Signal_NextSignalIfNoIndicator > High(Signals))) THEN
        Result := ' target for next signal if no indicator (S' + IntToStr(Signal_NextSignalIfNoIndicator) + ') is not a valid signal number';
    END; {FOR}
  END; {WITH}
END; { ValidateIndicatorDestinations }

FUNCTION ValidateSignalDistantHomesArray(S : Integer; Signal_Type : TypeOfSignal; Str : String; OUT ErrorMsg : String) : IntegerArrayType;
{ Validates the home signal numbers supplied }
VAR
  I, J : Integer;
  SignalDataOK : Boolean;
  TempSignal : Integer;
  TempSignalsStrArray : StringArrayType;

BEGIN
  ErrorMsg := '';
  SetLength(Result, 0);

  IF (Signal_Type <> SemaphoreDistant) AND (Str <> '') THEN
    ErrorMsg := 'ValidateSignalDistantHomesArray: non-distant semaphore signal ' + IntToStr(S) + ' has home signals connected to it'
  ELSE BEGIN
    IF Signal_Type = SemaphoreDistant THEN BEGIN
      IF Str = '' THEN
        ErrorMsg := 'ValidateSignalDistantHomesArray: semaphore distant signal ' + IntToStr(S) + ' does not have any home signals connected to it'
      ELSE BEGIN
        IF Str <> '' THEN BEGIN
          ExtractSubStringsFromString(Str, ',', TempSignalsStrArray);
          I := 0;
          SignalDataOK := True;
          WHILE (I <= High(TempSignalsStrArray)) AND SignalDataOK DO BEGIN
            IF Copy(TempSignalsStrArray[I], 1, 1) <> 'S' THEN
              ErrorMsg := 'ValidateSignalDistantHomesArray: signal "' + TempSignalsStrArray[I] + '" not preceded by ''S'''
            ELSE BEGIN
              TempSignalsStrArray[I] := Copy(TempSignalsStrArray[I], 2);
              IF NOT TryStrToInt(Trim(TempSignalsStrArray[I]), TempSignal) THEN
                ErrorMsg := 'ValidateSignalDistantHomesArray: invalid signal integer string "' + TempSignalsStrArray[I] + '"'
              ELSE
                { we need to validate the signal number too, but can't do that until all the signals are read in }
                AppendToLocationArray(Result, TempSignal);
            END;
            Inc(I);
          END; {WHILE}

          { Now check that the supplied homes don't appear in any other DistantHomesArray as a home can't be attached to more than one distant }
          FOR TempSignal := 0 TO High(Signals) DO BEGIN
            IF TempSignal <> S THEN BEGIN
              IF Signals[TempSignal].Signal_Type = SemaphoreDistant THEN BEGIN
                I := 0;
                WHILE (I <= High(Signals[TempSignal].Signal_SemaphoreDistantHomesArray)) AND (ErrorMsg = '') DO BEGIN
                  J := 0;
                  WHILE (J <= High(Result)) AND (ErrorMsg = '') DO BEGIN
                    IF Signals[TempSignal].Signal_SemaphoreDistantHomesArray[I] = Result[J] THEN
                      ErrorMsg := 'ValidateSignalDistantHomesArray: signal ' + IntToStr(Result[J])
                                  + ' appears in both DistantHomesArray for S' + IntToStr(S) + ' and for S' +  IntToStr(TempSignal);
                    Inc(J);
                  END; {WHILE}
                  Inc(I);
                END; {WHILE}
              END;
            END;
          END; {FOR}
        END;
      END;
    END;
  END;
END; { ValidateSignalDistantHomesArray }

PROCEDURE InitialiseSignalVariables(S : Integer);
{ Initialise all the variables where the data is not read in from the database or added during the edit process }
VAR
  TempJunctionIndicator : JunctionIndicatorType;

BEGIN
  WITH Signals[S] DO BEGIN
    Signal_AccessoryAddress := 0;
    Signal_AdjacentLineXOffset := 0;
    Signal_AdjacentTC := UnknownTrackCircuit;
    Signal_ApproachControlAspect := NoAspect;
    Signal_ApproachLocked := False;
    Signal_AsTheatreDestination := ''; { what a signal pointing at this signal might display }
    Signal_Automatic := False; { not yet implemented }
    Signal_DecoderNum := 0;
    Signal_Energised := False;
    Signal_EnergisedTime := 0;
    Signal_FailedToResetFlag := False;
    Signal_FailMsgWritten := False;
    Signal_FindNextSignalBufferStopMsgWritten := False;
    Signal_FromWhichUserMustDrive := False;
    Signal_HiddenStationSignalAspect := NoAspect;
    Signal_Indicator := NoIndicator;
    Signal_IndicatorDecoderFunctionNum := 0;
    Signal_IndicatorDecoderNum := 0;
    Signal_IndicatorSpeedRestriction := MPH0; { applicable only if the route indicator is set }
    Signal_IndicatorState := NoIndicatorLit;

    FOR TempJunctionIndicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
      WITH Signal_JunctionIndicators[TempJunctionIndicator] DO BEGIN
        JunctionIndicator_Exists := False;
        JunctionIndicator_TargetSignal := UnknownSignal;
        JunctionIndicator_TargetBufferStop := UnknownBufferStop;
      END; {WITH}
    END; {FOR}

    Signal_LampIsOn := True;
    SetLength(Signal_LocationsToMonitorArray, 0);

    { Signal_LockedArray and Signal_RouteLockingNeededArray sound similar but serve different purposes - RouteLockingNeededArray covers the lines, track circuits,
      points, etc. ahead that must be locked before a signal can be pulled off; Signal_LockedArray shows whether a signal is locked either by a specific route or
      by a user.
    }
    SetLength(Signal_LockedArray, 0);
    Signal_LockedBySemaphoreDistant := False;
    Signal_LockFailureNotedInRouteUnit := False;
    Signal_NextSignalIfNoIndicator := UnknownSignal;
    Signal_NotUsedForRouteing := True;
    Signal_OppositePassingLoopSignal := UnknownSignal;
    Signal_OutOfUse := False;
    Signal_OutOfUseMsgWritten := False;
    Signal_PossibleRouteHold := False;
    Signal_PossibleStationStartRouteHold := False;
    Signal_PostColour := ForegroundColour;
    Signal_PreviousAspect := NoAspect;
    Signal_PreviousHiddenStationSignalAspectSignal1 := UnknownSignal;
    Signal_PreviousHiddenStationSignalAspectSignal2 := UnknownSignal;
    Signal_PreviousIndicatorState := NoIndicatorLit;
    Signal_PreviousSignal1 := UnknownSignal;
    Signal_PreviousSignal2 := UnknownSignal;
    Signal_PreviousTheatreIndicatorString := '';
    Signal_Quadrant := NoQuadrant;
    Signal_ResettingTC := UnknownTrackCircuit;

    { see note above for Signal_LockedArray }
    SetLength(Signal_RouteLockingNeededArray, 0);
    SetLength(Signal_SemaphoreDistantHomesArray, 0); { needed to tell a semaphore distant which semaphore homes lock it }
    Signal_SemaphoreDistantLocking := UnknownSignal;
    Signal_StateChanged := False;
    Signal_TheatreIndicatorString := '';
    Signal_TRSHeld := False;
    Signal_TRSHeldMsgWritten := False;
    Signal_TRSReleased := False;
    Signal_TRSReleasedMsgWritten := False;
    Signal_Type := TwoAspect;
  END; {WITH}
END; { InitialiseSignalVariables }

PROCEDURE ReadInSignalDataFromDatabase(NewSignalData : Boolean);
{ Create entries for the signals }
CONST
  ApproachControlled = True;
  InUse = True;
  Init = True;
  PermitRouteTo = True;
  StopTimer = True;

VAR
  DistantFound : Boolean;
  ErrorMsg : String;
  I : Integer;
  S : Integer;
  TempLineArray : IntegerArrayType;
  TempS : Integer;
  TempSignalNumber : Integer;
  TempStrArray : ARRAY [0..5] OF String;

BEGIN
  TRY
    Log('A READ IN SIGNAL DATA FROM DATABASE {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      SetLength(Signals, 0);
      SetLength(TempLineArray, 0);

      IF NOT FileExists(PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Signal database file "' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInSignalDataFromDatabase')
        ELSE
          Exit;
      END;

      SignalsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix
                                               + ';Persist Security Info=False';
      TRY
        SignalsADOConnection.Connected := True;
      EXCEPT
        ON E : Exception DO
          Log('EG ReadInSignalDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
      END; {TRY}

      SignalsADOTable.Open;

      { First see if the signal numbers in the MSAccess file are sequential and, if not, renumber it - we need this or deletions from the MSAccess file will causes
        problems
      }
      S := -1;
      SignalsADOTable.Sort := '[' + Signal_NumberFieldName + '] ASC';
      SignalsADOTable.First;
      WHILE NOT SignalsADOTable.EOF DO BEGIN
        Inc(S);
        IF SignalsADOTable.FieldByName(Signal_NumberFieldName).AsInteger <> S THEN BEGIN
          { we need to renumber from here on }
          SignalsADOTable.Edit;
          SignalsADOTable.FieldByName(Signal_NumberFieldName).AsInteger := S;
          SignalsADOTable.Post;
        END;
        SignalsADOTable.Next;
      END; {WHILE}

      Log('S Signal data table and connection opened to initialise the signal data');

      S := -1;
      SignalsADOTable.Sort := '[' + Signal_NumberFieldName + '] ASC';
      SignalsADOTable.First;
      WHILE NOT SignalsADOTable.EOF DO BEGIN
        ErrorMsg := '';
        Inc(S);

        TempSignalNumber := SignalsADOTable.FieldByName(Signal_NumberFieldName).AsInteger;
        IF TempSignalNumber <> S THEN
          ErrorMsg := 'it does not match the signal number in the database (' + IntToStr(TempSignalNumber) + ')'
        ELSE
          SetLength(Signals, Length(Signals) + 1);

        WITH Signals[S] DO BEGIN
          IF ErrorMsg = '' THEN BEGIN
            InitialiseSignalVariables(S);
            IF NewSignalData THEN
              { colour-light signals are set to RedAspect just before they're switched on, unless we're reloading the data; semaphore signals are always set to red aspect }
              Signal_Aspect := RedAspect
            ELSE
              Signal_Aspect := NoAspect;
            Signal_AdjacentLine := UnknownLine;
            Signal_DataChanged := False;
            Signal_DataChanged := False;
            Signal_Direction := UnknownDirection;
            Signal_LineWithVerticalSpacingY := UnknownLine;
            Signal_LineWithVerticalSpacingY := UnknownLine;
            Signal_LineX := 0;
            Signal_LineY := 0;
            Signal_Notes := '';
            Signal_Number := TempSignalNumber;
            Signal_PreviousLineWithVerticalSpacingY := 0;
            Signal_PreviousLineX := 0;
            Signal_PreviousLineY := 0;
          END;

          IF ErrorMsg = '' THEN
            Signal_Quadrant := ValidateSignalQuadrant(SignalsADOTable.FieldByName(Signal_QuadrantFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN BEGIN
            IF SignalsADOTable.FieldByName(Signal_TypeFieldName).AsString = '' THEN
              ErrorMsg := 'no signal type given'
            ELSE
              Signal_Type := ValidateSignalType(SignalsADOTable.FieldByName(Signal_TypeFieldName).AsString, Signal_Quadrant, Signal_SemaphoreDistantHomesArray, ErrorMsg);
          END;

          { NB the signal fields are in the following order for validation purposes }
          Signal_Indicator := NoIndicator;
          IF ErrorMsg = '' THEN
            Signal_Indicator := ValidateSignalIndicator(SignalsADOTable.FieldByName(Signal_IndicatorFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[0] := SignalsADOTable.FieldByName(Signal_UpperLeftIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[UpperLeftIndicator] :=
                                                   ValidateSignalJunctionIndicators1(TempStrArray[0], Signal_UpperLeftIndicatorTargetFieldName, Signal_Indicator, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[1] := SignalsADOTable.FieldByName(Signal_MiddleLeftIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[MiddleLeftIndicator] :=
                                                  ValidateSignalJunctionIndicators1(TempStrArray[1], Signal_MiddleLeftIndicatorTargetFieldName, Signal_Indicator, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[2] := SignalsADOTable.FieldByName(Signal_LowerLeftIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[LowerLeftIndicator] :=
                                                   ValidateSignalJunctionIndicators1(TempStrArray[2], Signal_LowerLeftIndicatorTargetFieldName, Signal_Indicator, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[3] := SignalsADOTable.FieldByName(Signal_UpperRightIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[UpperRightIndicator] :=
                                                  ValidateSignalJunctionIndicators1(TempStrArray[3], Signal_UpperRightIndicatorTargetFieldName, Signal_Indicator, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[4] := SignalsADOTable.FieldByName(Signal_MiddleRightIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[MiddleRightIndicator] :=
                                                 ValidateSignalJunctionIndicators1(TempStrArray[4], Signal_MiddleRightIndicatorTargetFieldName, Signal_Indicator, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[5] := SignalsADOTable.FieldByName(Signal_LowerRightIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[LowerRightIndicator] :=
                                                  ValidateSignalJunctionIndicators1(TempStrArray[5], Signal_LowerRightIndicatorTargetFieldName, Signal_Indicator, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN
            ValidateSignalJunctionIndicators2(TempStrArray, Signal_Indicator, Signal_JunctionIndicators, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_AdjacentLine := ValidateSignalAdjacentLine(S, SignalsADOTable.FieldByName(Signal_AdjacentLineFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_AdjacentLineXOffset := ValidateSignalAdjacentLineXOffset(SignalsADOTable.FieldByName(Signal_AdjacentLineXOffsetFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_Direction := ValidateSignalDirection(SignalsADOTable.FieldByName(Signal_DirectionFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_IndicatorSpeedRestriction :=
                       ValidateSignalIndicatorSpeedRestriction(SignalsADOTable.FieldByName(Signal_IndicatorSpeedRestrictionFieldName).AsString, Signal_Indicator, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_NextSignalIfNoIndicator :=
                                             ValidateNextSignalIfNoIndicator(SignalsADOTable.FieldByName(Signal_NextSignalIfNoIndicatorFieldName).AsString, Init, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_OppositePassingLoopSignal := ValidateSignalOppositePassingLoopSignal(SignalsADOTable.FieldByName(Signal_OppositePassingLoopSignalFieldName).AsString,
                                                                                        Init, ErrorMsg);
          IF ErrorMsg = '' THEN
            Signal_AsTheatreDestination := ValidateSignalAsTheatreDestination(SignalsADOTable.FieldByName(Signal_AsTheatreDestinationFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_OutOfUse :=
                  ValidateSignalOutOfUseAndAddAdjacentTC(SignalsADOTable.FieldByName(Signal_OutOfUseFieldName).AsBoolean, Signal_AdjacentLine, Signal_AdjacentTC, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_Notes := SignalsADOTable.FieldByName(Signal_NotesFieldName).AsString;

          IF ErrorMsg = '' THEN
            Signal_NotUsedForRouteing := SignalsADOTable.FieldByName(Signal_NotUsedForRouteingFieldName).AsBoolean;

          { This is the accessory address used by the TrainTech SC3s switching Dapol semaphores }
          IF ErrorMsg = '' THEN
            Signal_AccessoryAddress := ValidateSignalAccessoryAddress(SignalsADOTable.FieldByName(Signal_AccessoryAddressFieldName).AsString, Signal_Type, ErrorMsg);

          { This is for signals using function decoder addresses }
          IF ErrorMsg = '' THEN
            Signal_DecoderNum := ValidateSignalDecoderNum(SignalsADOTable.FieldByName(Signal_DecoderNumFieldName).AsString, Signal_AccessoryAddress, Signal_Type, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_IndicatorDecoderNum := ValidateSignalIndicatorDecoderNum
                                                (SignalsADOTable.FieldByName(Signal_IndicatorDecoderNumFieldName).AsString, Signal_AccessoryAddress, Signal_Type, ErrorMsg);
          IF ErrorMsg = '' THEN
            Signal_IndicatorDecoderFunctionNum := ValidateSignalIndicatorDecoderFunctionNum
                                        (SignalsADOTable.FieldByName(Signal_IndicatorDecoderFunctionNumFieldName).AsString, Signal_AccessoryAddress, Signal_Type, ErrorMsg);
          IF ErrorMsg = '' THEN
            Signal_ApproachControlAspect := ValidateSignalApproachControlAspect(SignalsADOTable.FieldByName(Signal_ApproachControlAspectFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_PossibleRouteHold := SignalsADOTable.FieldByName(Signal_PossibleRouteHoldFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Signal_PossibleStationStartRouteHold := ValidateSignalPossibleStationStartRouteHold
                                                 (SignalsADOTable.FieldByName(Signal_PossibleStationStartRouteHoldFieldName).AsBoolean, Signal_PossibleRouteHold, ErrorMsg);
          IF ErrorMsg = '' THEN
            Signal_LocationsToMonitorArray :=
                        ValidateSignalLocationsToMonitorArray(SignalsADOTable.FieldByName(Signal_LocationsToMonitorFieldName).AsString, Signal_PossibleRouteHold, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_Automatic := SignalsADOTable.FieldByName(Signal_AutomaticFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Signal_SemaphoreDistantHomesArray := ValidateSignalDistantHomesArray(S, Signal_Type,
                                                                                 SignalsADOTable.FieldByName(Signal_SemaphoreDistantHomesArrayFieldName).AsString,
                                                                                 ErrorMsg);
          IF ErrorMsg = '' THEN
            Signal_FromWhichUserMustDrive := SignalsADOTable.FieldByName(Signal_FromWhichUserMustDriveFieldName).AsBoolean;

          IF Signal_PossibleRouteHold AND Signal_PossibleStationStartRouteHold THEN
            { both shouldn't be ticked }
            ErrorMsg := 'route hold and station start route hold are both ticked';

          IF ErrorMsg = '' THEN BEGIN
            { The two following arrays sound similar but serve different purposes - RouteLockingNeededArray covers the lines, track circuits, points, etc. ahead that must
              be locked before a signal can be pulled off; Signal_LockedArray shows whether a signal is locked either by a specific route or by a user.
            }
            SetLength(Signal_RouteLockingNeededArray, 0);
            SetLength(Signal_LockedArray, 0);
          END ELSE BEGIN
            IF MessageDialogueWithDefault('Error in creating S=' + IntToStr(S) + ': '
                                          + ErrorMsg
                                          + CRLF
                                          + 'Do you wish to continue?',
                                          StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
            THEN
              ShutDownProgram(UnitRef, 'ReadInSignalDataFromDatabase 1')
            ELSE
              Signal_OutOfUse := True;
          END;
          SignalsADOTable.Next;
        END; {WITH}
      END; {WHILE}

      { Tidy up the database }
      SignalsADOTable.Close;
      SignalsADOConnection.Connected := False;
      Log('S Signal Data table and connection closed');
    END; {WITH}

    { Check that signal numbers are valid. (We can only do this once all the signal details are read in). }
    S := 0;
    WHILE (S <= High(Signals)) AND (ErrorMsg = '') DO BEGIN
      WITH Signals[S] DO BEGIN
        ErrorMsg := ValidateSignalNum(Signal_NextSignalIfNoIndicator);

        IF ErrorMsg = '' THEN
          ErrorMsg := ValidateSignalNum(Signal_OppositePassingLoopSignal);

        IF ErrorMsg = '' THEN BEGIN
          { we also need to validate the signals held in the DistantHomesArrays }
          I := 0;
          WHILE (I <= High(Signals[S].Signal_SemaphoreDistantHomesArray)) AND (ErrorMsg = '') DO BEGIN
            ErrorMsg := ValidateSignalNum(Signals[S].Signal_SemaphoreDistantHomesArray[I]);
            Inc(I);
          END; {WHILE}
        END;

        IF ErrorMsg = '' THEN
          Inc(S);
      END; {WITH}
    END; {WHILE}

    IF ErrorMsg = '' THEN BEGIN
      { And add locking semaphore distants to records for semaphore homes }
      S := 0;
      WHILE S <= High(Signals) DO BEGIN
        IF Signals[S].Signal_Type = SemaphoreHome THEN BEGIN
          TempS := 0;
          DistantFound := False;
          WHILE (TempS <= High(Signals)) AND NOT DistantFound DO BEGIN
            IF Signals[TempS].Signal_Type = SemaphoreDistant THEN BEGIN
              IF IsElementInIntegerArray(Signals[TempS].Signal_SemaphoreDistantHomesArray, S) THEN BEGIN
                Signals[S].Signal_SemaphoreDistantLocking := TempS;
                DistantFound := True;
              END;
            END;
            Inc(TempS);
          END; {WHILE}
        END;
        Inc(S);
      END; {WHILE}
    END;

    { Check that indicator destinations are valid. (We can only do this once all the signal details are read in, too). }
    IF ErrorMsg = '' THEN BEGIN
      S := 0;
      WHILE (S <= High(Signals)) AND (ErrorMsg = '') DO BEGIN
        ErrorMsg := ValidateIndicatorDestinations(S);
        IF ErrorMsg = '' THEN
          Inc(S);
      END; {WHILE}
    END;

    IF ErrorMsg <> '' THEN
      IF MessageDialogueWithDefault('Error in creating S=' + IntToStr(S) + ': '
                                    + ErrorMsg
                                    + CRLF
                                    + 'Do you wish to continue?',
                                    StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
      THEN
        ShutDownProgram(UnitRef, 'ReadInSignalDataFromDatabase 2');

    CalculateTCAdjacentSignals;
    CalculateAllSignalPositions;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInSignalDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadInSignalDataFromDatabase }

PROCEDURE AddNewRecordToSignalDatabase;
{ Append a record to the signal database }
BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Signal database file "' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'AddNewRecordToSignalDatabase')
        ELSE
          Exit;
      END;

      SignalsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix
                                               + ';Persist Security Info=False';
      SignalsADOConnection.Connected := True;
      SignalsADOTable.Open;
      SignalsADOTable.Append;
      SignalsADOTable.FieldByName(Signal_NumberFieldName).AsInteger := High(Signals);
      SignalsADOTable.Post;

      Log('S Signal data table and connection opened to write out signal data that has changed');
      { Tidy up the database }
      SignalsADOTable.Close;
      SignalsADOConnection.Connected := False;
      Log('S Signal Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToSignalDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { AddNewRecordToSignalDatabase }

FUNCTION DeleteRecordFromSignalDatabase(SignalToDeleteNum : Integer) : Boolean;
{ Remove a record from the signal database }
BEGIN
  Result := False;
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Signal database file "' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'DeleteRecordFromSignalDatabase')
        ELSE
          Exit;
      END;

      SignalsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix
                                               + ';Persist Security Info=False';
      SignalsADOConnection.Connected := True;
      SignalsADOTable.Open;
      IF NOT SignalsADOTable.Locate(Signal_NumberFieldName, IntToStr(SignalToDeleteNum), []) THEN BEGIN
        Log('S Signal data table and connection opened to delete S' + IntToStr(SignalToDeleteNum) + ' but it cannot be found');
      END ELSE BEGIN
        Log('S Signal data table and connection opened to delete S' + IntToStr(SignalToDeleteNum));

        { Now delete the signal - we have already checked, in the Edit unit, whether deleting it will cause knock-on problems with other signals }
        SignalsADOTable.Delete;
        Log('SG S' + IntToStr(SignalToDeleteNum) + ' has been deleted');
        Result := True;
      END;

      { Tidy up the database }
      SignalsADOTable.Close;
      SignalsADOConnection.Connected := False;
      Log('S Signal Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToSignalDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DeleteRecordFromSignalDatabase }

PROCEDURE WriteOutSignalDataToDatabase;
{ If a Signal's data has been changed, record it in the database }
VAR
  DebugStr : String;
  I : Integer;
  JunctionIndicatorStr : String;
  S : Integer;
  SignalDatabaseNeedsUpdating : Boolean;
  TempStr : String;

BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      SignalDatabaseNeedsUpdating := False;

      { First see if we need to open the database }
      S := 0;
      WHILE (S <= High(Signals)) AND NOT SignalDatabaseNeedsUpdating DO BEGIN
        IF Signals[S].Signal_DataChanged THEN
          SignalDatabaseNeedsUpdating := True;
        Inc(S);
      END; {WHILE}

      IF SignalDatabaseNeedsUpdating THEN BEGIN
        IF NOT FileExists(PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix) THEN BEGIN
          IF MessageDialogueWithDefault('Signal database file "' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix + '" cannot be located'
                                        + CRLF
                                        + 'Do you wish to continue?',
                                        StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
          THEN
            ShutDownProgram(UnitRef, 'DeleteRecordFromSignalDatabase')
          ELSE
            Exit;
        END;

        SignalsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                 + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix
                                                 + ';Persist Security Info=False';
        SignalsADOConnection.Connected := True;
        SignalsADOTable.Open;
        SignalsADOTable.Edit;
        Log('S Signal data table and connection opened to write out signal data that has changed');

        SignalsADOTable.First;
        WHILE NOT SignalsADOTable.EOF DO BEGIN
          S := SignalsADOTable.FieldByName(Signal_NumberFieldName).AsInteger;
          WITH Signals[S] DO BEGIN
            IF Signal_DataChanged THEN BEGIN
              Signal_DataChanged := False;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_NumberFieldName + ' is ''' + IntToStr(Signal_Number) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_NumberFieldName).AsString := IntToStr(Signal_Number);
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_AccessoryAddressFieldName
                     + ' is ''' + IntToStr(Signals[S].Signal_AccessoryAddress) + '''');
              TempStr := IntToStr(Signal_AccessoryAddress);
              IF TempStr = '0' THEN
                { the database records a zero as a space in this field }
                TempStr := '';
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AccessoryAddressFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_AdjacentLineFieldName + ' is ''' + LineToStr(Signal_AdjacentLine) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AdjacentLineFieldName).AsString := LineToStr(Signal_AdjacentLine);
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_AdjacentLineXOffsetFieldName
                     + ' is ''' + IntToStr(Signal_AdjacentLineXOffset) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AdjacentLineXOffsetFieldName).AsString := IntToStr(Signal_AdjacentLineXOffset);
              SignalsADOTable.Post;

              { The database records "no aspect" as a space in this field }
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_ApproachControlAspectFieldName
                     + ' is ''' + AspectToStr(Signal_ApproachControlAspect) + '''');
              TempStr := AspectToStr(Signal_ApproachControlAspect);
              IF UpperCase(TempStr) = 'NO ASPECT' THEN
                TempStr := '';
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_ApproachControlAspectFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              DebugStr := '';
              IF Length(Signals[S].Signal_SemaphoreDistantHomesArray) > 0 THEN BEGIN
                DebugStr := 'S' + IntToStr(Signals[S].Signal_SemaphoreDistantHomesArray[0]);
                IF Length(Signals[S].Signal_SemaphoreDistantHomesArray) > 1 THEN BEGIN
                  FOR I := 1 TO High(Signals[S].Signal_SemaphoreDistantHomesArray) DO
                    DebugStr := DebugStr + ', S' + IntToStr(Signals[S].Signal_SemaphoreDistantHomesArray[I]);
                END;
              END;
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_SemaphoreDistantHomesArrayFieldName + ' contains ''' + DebugStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_SemaphoreDistantHomesArrayFieldName).AsString := DebugStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_AsTheatreDestinationFieldName + ' is ''' + Signal_AsTheatreDestination + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AsTheatreDestinationFieldName).AsString := Signal_AsTheatreDestination;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_AutomaticFieldName
                     + '  is ''' + IfThen(Signals[S].Signal_Automatic, 'automatic', 'not automatic') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AutomaticFieldName).AsBoolean := Signal_Automatic;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_DecoderNumFieldName + ' is ''' + IntToStr(Signal_DecoderNum) + '''');
              TempStr := IntToStr(Signal_DecoderNum);
              IF TempStr = '0' THEN
                { the database records a zero as a space in this field }
                TempStr := '';
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_DecoderNumFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_DirectionFieldName + ' is ''' + DirectionToStr(Signal_Direction) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_DirectionFieldName).AsString := DirectionToStr(Signal_Direction, VeryShortStringType);
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_IndicatorFieldName
                     + ' is ''' + IndicatorToStr(Signal_Indicator, LongStringType) + '''');
              TempStr := IndicatorToStr(Signal_Indicator, ShortStringType);
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_IndicatorFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_IndicatorDecoderFunctionNumFieldName
                     + ' is ''' + IntToStr(Signal_IndicatorDecoderFunctionNum) + '''');
              TempStr := IntToStr(Signal_IndicatorDecoderFunctionNum);
              IF TempStr = '0' THEN
                { the database records a zero as a space in this field }
                TempStr := '';
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_IndicatorDecoderFunctionNumFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_IndicatorDecoderNumFieldName
                     + ' is ''' + IntToStr(Signal_IndicatorDecoderNum) + '''');
              TempStr := IntToStr(Signal_IndicatorDecoderNum);
              IF TempStr = '0' THEN
                { the database records a zero as a space in this field }
                TempStr := '';
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_IndicatorDecoderNumFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_IndicatorSpeedRestrictionFieldName
                     + ' is ''' + MPHToStr(Signal_IndicatorSpeedRestriction) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_IndicatorSpeedRestrictionFieldName).AsString := MPHToStr(Signal_IndicatorSpeedRestriction);
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_UpperLeftIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_UpperLeftIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_MiddleLeftIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_MiddleLeftIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_LowerLeftIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_LowerLeftIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_UpperRightIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_UpperRightIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_MiddleRightIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_MiddleRightIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_LowerRightIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_LowerRightIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              TempStr := '';
              { insert a comma between entries }
              IF Length(Signal_LocationsToMonitorArray) > 0 THEN
                FOR I := 0 TO High(Signal_LocationsToMonitorArray) DO
                  TempStr := TempStr + Locations[Signal_LocationsToMonitorArray[I]].Location_LongNameStr + ',';

              { and remove the comma at the end of the String, if any }
              IF Copy(TempStr, Length(TempStr), 1) = ',' THEN
                TempStr := Copy(TempStr, 1, Length(TempStr) - 1);

              { also remove any spaces in the text - this will help to get around the 250 character length restriction in MSAccess fields }
              TempStr := ReplaceText(TempStr, ' ', '');
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_LocationsToMonitorFieldName + ' is ''' + TempStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_LocationsToMonitorFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              TempStr := IntToStr(Signal_NextSignalIfNoIndicator);
              IF TempStr = IntToStr(UnknownSignal) THEN
                { the database records unknown signal as a space in this field }
                TempStr := ''
              ELSE
                TempStr := 'S' + TempStr;
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_NextSignalIfNoIndicatorFieldName + ' is ''' + TempStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_NextSignalIfNoIndicatorFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_NotUsedForRouteingFieldName
                     + ' is ''' + IfThen(Signals[S].Signal_NotUsedForRouteing, 'not used for routeing', 'used for routeing') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_NotUsedForRouteingFieldName).AsBoolean := Signal_NotUsedForRouteing;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_NotesFieldName + ' is ''' + Signal_Notes + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_NotesFieldName).AsString := Signal_Notes;
              SignalsADOTable.Post;

              TempStr := IntToStr(Signal_OppositePassingLoopSignal);
              IF TempStr = IntToStr(UnknownSignal) THEN
                { the database records unknown signal as a space in this field }
                TempStr := ''
              ELSE
                TempStr := 'S' + TempStr;
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_OppositePassingLoopSignalFieldName + ' is ''' + TempStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_OppositePassingLoopSignalFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_OutOfUseFieldName
                     + ' is ''' + IfThen(Signals[S].Signal_OutOfUse, 'out of use', 'back in use') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_OutOfUseFieldName).AsBoolean := Signal_OutOfUse;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_PossibleRouteHoldFieldName
                     + ' is ''' + IfThen(Signals[S].Signal_PossibleRouteHold, 'a possible route hold', 'not a possible route hold') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_PossibleRouteHoldFieldName).AsBoolean := Signal_PossibleRouteHold;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_PossibleStationStartRouteHoldFieldName
                     + ' is ''' + IfThen(Signals[S].Signal_PossibleStationStartRouteHold,
                                      'a possible station start route hold',
                                      'not a possible station start route hold') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_PossibleStationStartRouteHoldFieldName).AsBoolean := Signal_PossibleStationStartRouteHold;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_QuadrantFieldName + ' is ''' + SignalQuadrantToStr(Signal_Quadrant) + '''');
              SignalsADOTable.Edit;
              IF Signal_Quadrant = NoQuadrant THEN
                SignalsADOTable.FieldByName(Signal_QuadrantFieldName).AsString := ''
              ELSE
                SignalsADOTable.FieldByName(Signal_QuadrantFieldName).AsString := SignalQuadrantToStr(Signal_Quadrant);
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_TypeFieldName + ' is ''' + SignalTypeToStr(Signal_Type, LongStringType) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_TypeFieldName).AsString := SignalTypeToStr(Signal_Type, ShortStringType);
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_FromWhichUserMustDriveFieldName + ' is '''
                     + IfThen(Signal_FromWhichUserMustDrive, 'on', 'off') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_FromWhichUserMustDriveFieldName).AsBoolean := Signal_FromWhichUserMustDrive;
              SignalsADOTable.Post;
            END; {WITH}
          END;
          SignalsADOTable.Next;
        END; {WHILE}

        { Tidy up the database }
        SignalsADOTable.Close;
        SignalsADOConnection.Connected := False;
        Log('S Signal Data table and connection closed');
      END;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteOutSignalDataToDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteOutSignalDataToDatabase }

FUNCTION GetLineAdjacentSignal(Line : Integer) : Integer;
{ Return the signal nearest the line, except for semaphore distants as they are treated differently }
VAR
  Found : Boolean;
  S : Integer;

BEGIN
  Result := UnknownSignal;
  Found := False;

  S := 0;
  WHILE (S <= High(Signals)) AND NOT Found DO BEGIN
    IF (Signals[S].Signal_Type <> SemaphoreDistant) AND (Signals[S].Signal_AdjacentLine = Line) THEN BEGIN
      Found := True;
      Result := S;
    END ELSE
      Inc(S);
  END; {WHILE}
END; { GetLineAdjacentSignal }

END { SignalsUnit }.
