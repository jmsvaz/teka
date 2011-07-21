unit uUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, uStrings;

type

  { TOptions }

  TPathType = (ptCircle, ptLine);
  TColorPalleteType = (cptRandomRainbowColorPallete, cptRainbowColorPallete, cptRandomWebSafeColorPallete);

  TOptions = class
    private
      fBackgroundColor: TColor;
      fChanged: Boolean;
      fImagePaths: TStringList;
      fOuterRadius: Integer;
      fPathColorType: TColorPalleteType;
      fPathType: TPathType;
      fPathWidth: Integer;
      fStampColorType: TColorPalleteType;
      fStarReciprocalRadius: Integer;
      fStarSpikes: Integer;
      function GetImagePaths: TStringList;
      procedure SetBackgroundColor(const AValue: TColor);
      procedure SetOuterRadius(const AValue: Integer);
      procedure SetPathColorType(AValue: TColorPalleteType);
      procedure SetPathType(const AValue: TPathType);
      procedure SetPathWidth(const AValue: Integer);
      procedure SetStampColorType(AValue: TColorPalleteType);
      procedure SetStarReciprocalRadius(const AValue: Integer);
      procedure SetStarSpikes(const AValue: Integer);
    public
      constructor Create; overload;
      constructor Create(AFileName: string); overload;
      destructor Destroy; override;
      procedure LoadFromFile(AFileName: string);
      procedure SaveToFile(AFileName: string);
      procedure AddImagePath(AImagePath: string);
      property Changed: Boolean read fChanged write fChanged;
      property StarReciprocalRadius: Integer read fStarReciprocalRadius write SetStarReciprocalRadius;
      property OuterRadius: Integer read fOuterRadius write SetOuterRadius;
      property StarSpikes: Integer read fStarSpikes write SetStarSpikes;
      property BackgroundColor: TColor read fBackgroundColor write SetBackgroundColor;
      property PathWidth: Integer read fPathWidth write SetPathWidth;
      property PathType: TPathType read fPathType write SetPathType;
      property PathColorType: TColorPalleteType read fPathColorType write SetPathColorType;
      property StampColorType: TColorPalleteType read fStampColorType write SetStampColorType;
      property ImagePaths: TStringList read GetImagePaths;
  end;

const
  PathTypeNames: array[TPathType] of string =
    (s_Circle, s_Line);
  ColorPalleteTypeNames: array[TColorPalleteType] of string =
    (s_RandomRainbowColorPallete, s_RainbowColorPallete, s_RandomWebSafeColorPallete);

function GetPathTypeNames: TStringList;
function GetColorPalleteTypeNames: TStringList;

function GetPathTypeFromName(APathTypeName: string): TPathType;
function GetColorPalleteTypeFromName(AColorPalleteTypeName: string): TColorPalleteType;

implementation

uses IniFiles;

{ TOptions }

const
  c_StarReciprocalRadius = 3;
  c_OuterRadius = 25;
  c_PathWidth = 10;
  c_StarSpikes = 12;
  c_BackgroundColor = clWhite;
  c_PathType = ptCircle;
  c_PathColorType = cptRainbowColorPallete;
  c_StampColorType = cptRandomWebSafeColorPallete;

function GetPathTypeNames: TStringList;
var
  i: TPathType;
begin
  Result:= TStringList.Create;
  for i:= Low(TPathType) to High(TPathType) do
    Result.Add(PathTypeNames[i]);
end;

function GetColorPalleteTypeNames: TStringList;
var
  i: TColorPalleteType;
begin
  Result:= TStringList.Create;
  for i:= Low(TColorPalleteType) to High(TColorPalleteType) do
    Result.Add(ColorPalleteTypeNames[i]);
end;

function GetPathTypeFromName(APathTypeName: string): TPathType;
var
  i: TPathType;
begin
  for i:= Low(TPathType) to High(TPathType) do
    if APathTypeName = PathTypeNames[i] then
      begin
        Result:= i;
        Exit;
      end;
  raise Exception.Create(s_PathTypeNameNotFound);
end;

function GetColorPalleteTypeFromName(AColorPalleteTypeName: string): TColorPalleteType;
var
  i: TColorPalleteType;
begin
  for i:= Low(TColorPalleteType) to High(TColorPalleteType) do
    if AColorPalleteTypeName = ColorPalleteTypeNames[i] then
      begin
        Result:= i;
        Exit;
      end;
  raise Exception.Create('Color Pallete Type Name not found.');
end;

constructor TOptions.Create(AFileName: string);
begin
  Create;
  LoadFromFile(AFileName);
end;

constructor TOptions.Create;
begin
  inherited Create;
  fImagePaths:= TStringList.Create;
  fImagePaths.NameValueSeparator:= '=';
  StarReciprocalRadius:= c_StarReciprocalRadius;
  OuterRadius:= c_OuterRadius;
  PathWidth:= c_PathWidth;
  PathType:= c_PathType;
  PathColorType:= c_PathColorType;
  StarSpikes:= c_StarSpikes;
  StampColorType:= c_StampColorType;
  BackgroundColor:= c_BackgroundColor;
  Changed:= False;
end;

destructor TOptions.Destroy;
begin
  fImagePaths.Free;
  inherited Destroy;
end;

function TOptions.GetImagePaths: TStringList;
begin
  Result:= TStringList.Create;
  Result.Assign(fImagePaths);
end;

procedure TOptions.LoadFromFile(AFileName: string);
var
  IniFile: TIniFile;
  i: Integer;
begin
  IniFile:= TIniFile.Create(AFileName);
  try
    StarReciprocalRadius:= IniFile.ReadInteger('Stamp','StarReciprocalRadius',StarReciprocalRadius);
    OuterRadius:= IniFile.ReadInteger('Stamp','OuterRadius',OuterRadius);
    StarSpikes:= IniFile.ReadInteger('Stamp','StarSpikes',StarSpikes);
    StampColorType:= TColorPalleteType(IniFile.ReadInteger('Stamp','StampColorType',Integer(StampColorType)));
    PathWidth:= IniFile.ReadInteger('Path','Width',PathWidth);
    PathType:= TPathType(IniFile.ReadInteger('Path','Type',Integer(PathType)));
    PathColorType:= TColorPalleteType(IniFile.ReadInteger('Path','PathColorType',Integer(PathColorType)));
    BackgroundColor:= TColor(IniFile.ReadInteger('Main','BackgroundColor',Integer(BackgroundColor)));
    IniFile.ReadSectionValues('ImagePath',fImagePaths);
    for i:= 0 to (fImagePaths.Count-1) do
      fImagePaths[i]:= fImagePaths.ValueFromIndex[i];
  finally
    IniFile.Free;
  end;
end;

procedure TOptions.SaveToFile(AFileName: string);
var
  IniFile: TIniFile;
  i: Integer;
begin
  IniFile:= TIniFile.Create(AFileName);
  try
    IniFile.WriteInteger('Stamp','StarReciprocalRadius',StarReciprocalRadius);
    IniFile.WriteInteger('Stamp','OuterRadius',OuterRadius);
    IniFile.WriteInteger('Stamp','Spikes',StarSpikes);
    IniFile.WriteInteger('Stamp','StampColorType',Integer(StampColorType));
    IniFile.WriteInteger('Path','Width',PathWidth);
    IniFile.WriteInteger('Path','Type',Integer(PathType));
    IniFile.WriteInteger('Path','PathColorType',Integer(PathColorType));
    IniFile.WriteInteger('Main','BackgroundColor',Integer(BackgroundColor));
    for i:= 0 to (fImagePaths.Count-1) do
      IniFile.WriteString('ImagePath','ImagePath' + IntToStr(i+1),fImagePaths[i]);
  finally
    IniFile.Free;
  end;
end;


procedure TOptions.AddImagePath(AImagePath: string);
begin
  if fImagePaths.IndexOf(AImagePath) >= 0 then exit;
  if not DirectoryExists(AImagePath) then exit;

  fImagePaths.Add(AImagePath);
  fChanged:= True;
end;

procedure TOptions.SetBackgroundColor(const AValue: TColor);
begin
  if fBackgroundColor = AValue then exit;
  fBackgroundColor:= AValue;
  fChanged:= True;
end;

procedure TOptions.SetOuterRadius(const AValue: Integer);
begin
  if fOuterRadius = AValue then exit;
  fOuterRadius:= AValue;
  fChanged:= True;
end;

procedure TOptions.SetPathColorType(AValue: TColorPalleteType);
begin
  if fPathColorType = AValue then exit;
  fPathColorType:= AValue;
  fChanged:= True;
end;

procedure TOptions.SetPathType(const AValue: TPathType);
begin
  if fPathType = AValue then exit;
  fPathType:= AValue;
  fChanged:= True;
end;

procedure TOptions.SetPathWidth(const AValue: Integer);
begin
  if fPathWidth = AValue then exit;
  fPathWidth:= AValue;
  fChanged:= True;
end;

procedure TOptions.SetStampColorType(AValue: TColorPalleteType);
begin
  if fStampColorType = AValue then exit;
  fStampColorType:= AValue;
  fChanged:= True;
end;

procedure TOptions.SetStarReciprocalRadius(const AValue: Integer);
begin
  if fStarReciprocalRadius = AValue then exit;
  fStarReciprocalRadius:= AValue;
  fChanged:= True;
end;

procedure TOptions.SetStarSpikes(const AValue: Integer);
begin
  if fStarSpikes = AValue then exit;
  fStarSpikes:= AValue;
  fChanged:= True;
end;


end.

