unit uUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type

  { TOptions }

  TPathType = (ptCircle, ptLine);

  TOptions = class
    private
      fBackgroundColor: TColor;
      fChanged: Boolean;
      fImagePaths: TStringList;
      fOuterRadius: Integer;
      fPathType: TPathType;
      fPathWidth: Integer;
      fStarReciprocalRadius: Integer;
      fStarSpikes: Integer;
      function GetImagePaths: TStringList;
      procedure SetBackgroundColor(const AValue: TColor);
      procedure SetOuterRadius(const AValue: Integer);
      procedure SetPathType(const AValue: TPathType);
      procedure SetPathWidth(const AValue: Integer);
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
      property ImagePaths: TStringList read GetImagePaths;
  end;


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

procedure TOptions.AddImagePath(AImagePath: string);
begin
  if DirectoryExists(AImagePath) then
    fImagePaths.Add(AImagePath);
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
  StarReciprocalRadius:= c_StarReciprocalRadius;
  OuterRadius:= c_OuterRadius;
  PathWidth:= c_PathWidth;
  PathType:= c_PathType;
  StarSpikes:= c_StarSpikes;
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
begin
  IniFile:= TIniFile.Create(AFileName);
  try
    StarReciprocalRadius:= IniFile.ReadInteger('Stamp','StarReciprocalRadius',StarReciprocalRadius);
    OuterRadius:= IniFile.ReadInteger('Stamp','OuterRadius',OuterRadius);
    StarSpikes:= IniFile.ReadInteger('Stamp','StarSpikes',StarSpikes);
    PathWidth:= IniFile.ReadInteger('Path','Width',PathWidth);
    PathType:= TPathType(IniFile.ReadInteger('Path','Type',Integer(PathType)));
    BackgroundColor:= TColor(IniFile.ReadInteger('Main','BackgroundColor',Integer(BackgroundColor)));
  finally
    IniFile.Free;
  end;
end;

procedure TOptions.SaveToFile(AFileName: string);
var
  IniFile: TIniFile;
begin
  IniFile:= TIniFile.Create(AFileName);
  try
    IniFile.WriteInteger('Stamp','StarReciprocalRadius',StarReciprocalRadius);
    IniFile.WriteInteger('Stamp','OuterRadius',OuterRadius);
    IniFile.WriteInteger('Stamp','Spikes',StarSpikes);
    IniFile.WriteInteger('Path','Width',PathWidth);
    IniFile.WriteInteger('Path','Type',Integer(PathType));
    IniFile.WriteInteger('Main','BackgroundColor',Integer(BackgroundColor));
  finally
    IniFile.Free;
  end;

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

