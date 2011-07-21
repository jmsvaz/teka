unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, ActnList, uUtil, uColors;

type

  { TfmMain }

  TfmMain = class(TForm)
    acAbout: TAction;
    acClear: TAction;
    acClose: TAction;
    ActionList: TActionList;
    PaintBox: TPaintBox;
    StatusBar: TStatusBar;
    procedure acAboutExecute(Sender: TObject);
    procedure acClearExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseEnter(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { private declarations }
    Images: TFPList;
    fLastPoint: TPoint;
    fOptions: TOptions;
    fPathColor: TColorPallete;
    fStarColor: TColorPallete;
    MyCanvas: TCanvas;
    procedure GetImages;
    procedure OnFileFound(FileIterator: TFileIterator);
  public
    { public declarations }
    procedure DrawStar(Center: TPoint; spike_count: Integer);
    procedure DrawImage(Center: TPoint; ImageNumber: Integer);
    procedure DrawPath(LastPoint,CurrentPoint: TPoint);
    procedure Test;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

uses uStrings;

function GetAColorPallete(APalleteType: TColorPalleteType): TColorPallete;
begin
  case APalleteType of
    cptRandomRainbowColorPallete:
      Result:= TRandomRainbowColorPallete.Create;
    cptRainbowColorPallete:
      Result:= TRainbowColorPallete.Create;
    cptRandomWebSafeColorPallete:
      Result:= TRandomWebSafeColorPallete.Create;
  end;
end;

{ TfmMain }

procedure TfmMain.acAboutExecute(Sender: TObject);
begin
  ShowMessage(Application.Title);
end;

procedure TfmMain.acClearExecute(Sender: TObject);
begin
  with PaintBox.Canvas do
    begin
      Brush.Color:= fOptions.BackgroundColor;
      Clear;
    end;
end;

procedure TfmMain.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.Test;
var
  i: Integer;
  Center: TPoint;
begin
  for i:= 1 to 20 do
    begin
      Center.X:= i*30;
      Center.Y:= Center.X;
      DrawStar(Center, i);
    end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Caption:= Application.Title;
  Randomize;
  Images:= TFPList.Create;
  fLastPoint:= Point(0,0);
  fOptions:= TOptions.Create(GetAppConfigFile(False));
  fOptions.AddImagePath(ExtractFilePath(Application.EXEName) + 'images');
  fOptions.AddImagePath(ExtractFilePath(Application.EXEName) + 'img');
  fPathColor:= GetAColorPallete(fOptions.PathColorType);
  fStarColor:= GetAColorPallete(fOptions.StampColorType);
  MyCanvas:= PaintBox.Canvas;

  fOptions.PathType:= ptLine;
  with MyCanvas do
    begin
      Color:= fOptions.BackgroundColor;
      Brush.Style:= bsSolid;
      Brush.Color:= fOptions.BackgroundColor;
      Pen.Color:= fOptions.BackgroundColor;
      Pen.Mode:= pmCopy;
      Pen.Style:= psSolid;
      Pen.Width:= 1;
    end;
  StatusBar.SimpleText:= Format('%s : ESC | %s : %s | %s : F1',[s_Quit,s_Clear,Upcase(s_SpaceKey),s_About]);
  GetImages;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  if fOptions.Changed then
    fOptions.SaveToFile(GetAppConfigFile(False));
  fPathColor.Free;
  fStarColor.Free;
  Images.Free;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
// WindowState:= wsFullScreen;
end;

procedure TfmMain.GetImages;
var
  fs: TFileSearcher;
  i: Integer;
begin
  fs:= TFileSearcher.Create;
  try
    fs.OnFileFound:= @OnFileFound;
    Images.Clear;
    for i:= 0 to (fOptions.ImagePaths.Count - 1) do
      fs.Search(fOptions.ImagePaths[i]);     //TODO: search only image files -> image extensions
  finally
    fs.Free;
  end;
end;

procedure TfmMain.OnFileFound(FileIterator: TFileIterator);
var
  APicture: TPicture;
begin
  APicture:= TPicture.Create;
  try
    APicture.LoadFromFile(FileIterator.FileName);
    Images.Add(APicture);
  except
    //ignore exception -> dont add this file to the list
  end;
end;

procedure TfmMain.PaintBoxMouseEnter(Sender: TObject);
begin
  fLastPoint:= PaintBox.ScreenToClient(Mouse.CursorPos);
end;

procedure TfmMain.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  DrawPath(fLastPoint, Point(X,Y));
  fLastPoint:= Point(X,Y);
end;


procedure TfmMain.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  nStarTypes, nImageTypes: Integer;
  DrawingType: Integer;
begin
  nStarTypes:= fOptions.StarSpikes - 2;
  nImageTypes:= Images.Count;
  DrawingType:= Random(nStarTypes + nImageTypes); //every star and image has the same probability
  if DrawingType < nStarTypes then
    DrawStar(Point(X,Y), Random(nStarTypes) + 3)
  else
    DrawImage(Point(X,Y), Random(nImageTypes));
end;

procedure TfmMain.DrawPath(LastPoint, CurrentPoint: TPoint);
var
  c: TColor;
  radius: Integer;
begin
  c:= fPathColor.GetColor;
  case fOptions.PathType of
    ptCircle:
      begin
        radius:= Trunc(fOptions.PathWidth/2);
        with MyCanvas do
          begin
            Brush.Color:= c;
            Pen.Color:= c;
            Pen.Width:= 1;
            Ellipse (CurrentPoint.X-radius,CurrentPoint.Y-radius,
                     CurrentPoint.X+radius,CurrentPoint.Y+radius);
          end;
      end;
    ptLine:
      begin
        with MyCanvas do
          begin
            Brush.Color:= c;
            Pen.Color:= c;
            Pen.Width:= fOptions.PathWidth;
            MoveTo(LastPoint);
            LineTo(CurrentPoint);
          end;
      end;
  end;
end;

procedure TfmMain.DrawStar(Center: TPoint; spike_count: Integer);
const
  RadConvert = PI/180;
var
  c: TColor;
  Points: array of TPoint;
  i: Integer;
  radius: Integer;
  angle, rotation: Extended;
begin
  SetLength(Points,2*spike_count);
  rotation:= 360/spike_count;
  for i:= 0 to (2*spike_count-1) do begin
      if (i mod 2) = 0 then
        radius:= Round(fOptions.OuterRadius/fOptions.StarReciprocalRadius)
      else
        radius:= fOptions.OuterRadius;
    angle:= ((i * rotation) + 90) * RadConvert;
    Points[i].X:= Center.X + Round(cos(angle) * radius);
    Points[i].Y:= Center.Y - Round(sin(angle) * radius);
  end;

  c:= fStarColor.GetColor;
  with MyCanvas do
    begin
      Brush.Color:= c;
      Pen.Color:= c;
      Pen.Width:= 9*Random(2) + 1;
      Polygon(Points);
    end;
end;


procedure TfmMain.DrawImage(Center: TPoint; ImageNumber: Integer);
var
  DestRect: TRect;
begin
  DestRect.Left:= Center.X - fOptions.OuterRadius;
  DestRect.Right:= Center.X + fOptions.OuterRadius;
  DestRect.Top:= Center.Y - fOptions.OuterRadius;
  DestRect.Bottom:= Center.Y + fOptions.OuterRadius;
  MyCanvas.StretchDraw(DestRect,TPicture(Images[ImageNumber]).Bitmap);
end;


end.

