unit uOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, uUtil;

type

  { TfmOptions }

  TfmOptions = class(TForm)
    btCancel: TButton;
    btOK: TButton;
    cbPathType: TComboBox;
    cbPathColorType: TComboBox;
    cbStampColorType: TComboBox;
    btBackgroundColor: TColorButton;
    lbStarReciprocalRadius: TLabel;
    lbOuterRadius: TLabel;
    lbStarSpikes: TLabel;
    lbPathWidth: TLabel;
    lbStampColorType: TLabel;
    lbPathColorType: TLabel;
    lbPathType: TLabel;
    lbBackgroundColor: TLabel;
    Panel: TPanel;
    edStarReciprocalRadius: TSpinEdit;
    edPathWidth: TSpinEdit;
    edOuterRadius: TSpinEdit;
    edStarSpikes: TSpinEdit;
    procedure btCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    function GetBackgroundColor: TColor;
    function GetImagePaths: TStringList;
    function GetOuterRadius: Integer;
    function GetPathColorType: TColorPalleteType;
    function GetPathType: TPathType;
    function GetPathWidth: Integer;
    function GetStampColorType: TColorPalleteType;
    function GetStarReciprocalRadius: Integer;
    function GetStarSpikes: Integer;
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetImagePaths(AValue: TStringList);
    procedure SetOuterRadius(AValue: Integer);
    procedure SetPathColorType(AValue: TColorPalleteType);
    procedure SetPathType(AValue: TPathType);
    procedure SetPathWidth(AValue: Integer);
    procedure SetStampColorType(AValue: TColorPalleteType);
    procedure SetStarReciprocalRadius(AValue: Integer);
    procedure SetStarSpikes(AValue: Integer);
  public
    { public declarations }
    function Execute: Boolean;
    property StarReciprocalRadius: Integer read GetStarReciprocalRadius write SetStarReciprocalRadius;
    property OuterRadius: Integer read GetOuterRadius write SetOuterRadius;
    property StarSpikes: Integer read GetStarSpikes write SetStarSpikes;
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property PathWidth: Integer read GetPathWidth write SetPathWidth;
    property PathType: TPathType read GetPathType write SetPathType;
    property PathColorType: TColorPalleteType read GetPathColorType write SetPathColorType;
    property StampColorType: TColorPalleteType read GetStampColorType write SetStampColorType;
    property ImagePaths: TStringList read GetImagePaths write SetImagePaths;
  end;


implementation

{$R *.lfm}

uses uStrings;

{ TfmOptions }

procedure TfmOptions.btCancelClick(Sender: TObject);
begin
  Close;
end;

function TfmOptions.Execute: Boolean;
begin
  Result:= (ShowModal = mrOK);
end;

procedure TfmOptions.FormCreate(Sender: TObject);
begin
  cbPathType.Items.Assign(GetPathTypeNames);
  cbPathColorType.Items.Assign(GetColorPalleteTypeNames);
  cbStampColorType.Items.Assign(GetColorPalleteTypeNames);
end;

procedure TfmOptions.FormShow(Sender: TObject);
begin
  Caption:= s_Options;
  btOK.Caption:= s_OK;
  btCancel.Caption:= s_Cancel;
  lbBackgroundColor.Caption:= s_BackgroundColor;
  lbStarReciprocalRadius.Caption:= s_StarReciprocalRadius;
  lbOuterRadius.Caption:= s_OuterRadius;
  lbStarSpikes.Caption:= s_StarSpikes;
  lbPathWidth.Caption:= s_PathWidth;
  lbPathType.Caption:= s_PathType;
  lbPathColorType.Caption:= s_PathColorType;
  lbStampColorType.Caption:= s_StampColorType;
//  lbImagePaths.Caption:= s_ImagePaths;
end;

function TfmOptions.GetBackgroundColor: TColor;
begin
  Result:= btBackgroundColor.ButtonColor;
end;

function TfmOptions.GetImagePaths: TStringList;
begin

end;

function TfmOptions.GetOuterRadius: Integer;
begin
  Result:= edOuterRadius.Value;
end;

function TfmOptions.GetPathColorType: TColorPalleteType;
begin
  Result:= GetColorPalleteTypeFromName(cbPathColorType.Items[cbPathColorType.ItemIndex]);
end;

function TfmOptions.GetPathType: TPathType;
begin
  Result:= GetPathTypeFromName(cbPathType.Items[cbPathType.ItemIndex]);
end;

function TfmOptions.GetPathWidth: Integer;
begin
  Result:= edPathWidth.Value;
end;

function TfmOptions.GetStampColorType: TColorPalleteType;
begin
  Result:= GetColorPalleteTypeFromName(cbStampColorType.Items[cbStampColorType.ItemIndex]);
end;

function TfmOptions.GetStarReciprocalRadius: Integer;
begin
  Result:= edStarReciprocalRadius.Value;
end;

function TfmOptions.GetStarSpikes: Integer;
begin
  Result:= edStarSpikes.Value;
end;

procedure TfmOptions.SetBackgroundColor(AValue: TColor);
begin
  btBackgroundColor.ButtonColor:= AValue;
end;

procedure TfmOptions.SetImagePaths(AValue: TStringList);
begin

end;

procedure TfmOptions.SetOuterRadius(AValue: Integer);
begin
  edOuterRadius.Value:= AValue;
end;

procedure TfmOptions.SetPathColorType(AValue: TColorPalleteType);
begin
  cbPathColorType.ItemIndex:= cbPathColorType.Items.IndexOf(ColorPalleteTypeNames[AValue]);
end;

procedure TfmOptions.SetPathType(AValue: TPathType);
begin
  cbPathType.ItemIndex:= cbPathType.Items.IndexOf(PathTypeNames[AValue]);
end;

procedure TfmOptions.SetPathWidth(AValue: Integer);
begin
  edPathWidth.Value:= AValue;
end;

procedure TfmOptions.SetStampColorType(AValue: TColorPalleteType);
begin
  cbStampColorType.ItemIndex:= cbStampColorType.Items.IndexOf(ColorPalleteTypeNames[AValue]);
end;

procedure TfmOptions.SetStarReciprocalRadius(AValue: Integer);
begin
  edStarReciprocalRadius.Value:= AValue;
end;

procedure TfmOptions.SetStarSpikes(AValue: Integer);
begin
  edStarSpikes.Value:= AValue;
end;

end.
