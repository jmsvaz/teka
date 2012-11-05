{
    This file is part of Teka, a drawing game for very young children who
    will discover mouse moving.

    Copyright (C) 2011-2012 João Marcelo S. Vaz

    Teka is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Teka is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit uColors;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Math;

type

  { TColorPallete }

  TColorPallete = class
    protected
      function DoGetColor: TColor; virtual; abstract;
    public
      function GetColor: TColor;
  end;

  { TCustomRainbowColorPallete }

  TCustomRainbowColorPallete = class(TColorPallete)
    private
      fMaximumWavelength: Double;
      fMinimumWavelength: Double;
      function GetARainbowColor(AWavelength: Double): TColor;
    public
      property MaximumWavelength: Double read fMaximumWavelength;
      property MinimumWavelength: Double read fMinimumWavelength;
  end;

  { TRandomRainbowColorPallete }

  TRandomRainbowColorPallete = class(TCustomRainbowColorPallete)
    protected
      function DoGetColor: TColor; override;
  end;

  { TRainbowColorPallete }

  TRainbowColorPallete = class(TCustomRainbowColorPallete)
    private
      fStep: Double;
      fWavelength: Double;
      procedure SetStep(const AValue: Double);
    protected
      function DoGetColor: TColor; override;
    public
      constructor Create;
      property Step: Double read fStep write SetStep;
  end;

  { TCustomWebSafeColorPallete }

  TCustomWebSafeColorPallete = class(TColorPallete)
    private
      function GetAWebSafeColor(r_i, g_i, b_i: Integer): TColor;
    end;

  { TRandomWebSafeColorPallete }

  TRandomWebSafeColorPallete = class(TCustomWebSafeColorPallete)
    protected
      function DoGetColor: TColor; override;
    end;
{
procedure HSVToRGB(const H, S, V: Single; out R, G, B: Single);
  in
  H = Hue.  Range is from 0..1.  0.5 = 180 degrees, 1 = 360. or H < 0 for gray
  S = Satration.  Range is 0..1 where 0 is white and 1 is no saturation.
  V = Value.  Range is 0..255

  out
  R = 0..255
  G = 0..255
  B = 0..255

  If H < 0 then the result is a gray value R=V, G=V, B=V

  William Egge, public@eggcentric.com
  http://www.eggcentric.com
}
procedure HSVToRGB(const H, S, V: Single; out R, G, B: Single);

{
  procedure RGBToHSV(const R, G, B: Single; out H, S, V: Single);
    in
    R = 0..255
    G = 0..255
    B = 0..255

    out
    H = Hue. -1 for grey scale or range 0..1.  0..1 represents 0..360 degrees
    S = Saturation. Range = 0..1. 0 = white, 1 = no saturation.
    V = Value or intensity. Range 0..255

  William Egge, public@eggcentric.com
  http://www.eggcentric.com
}
procedure RGBToHSV(const R, G, B: Single; out H, S, V: Single);
procedure WavelengthToRGB(const Wavelength: Double; out R,G,B: Byte);

function GetRandomMixedColor(AColor: TColor): TColor;
function GetRandomGoldenRatioColor: TColor;
function GetRandomGaminePathColor: TColor;
function GetRandomGamineStarColor: TColor;
function GetRandomStandardColor: TColor;



implementation

procedure HSVToRGB(const H, S, V: Single; out R, G, B: Single);
const
  SectionSize = 60/360;
var
  Section: Single;
  SectionIndex: Integer;
  f: single;
  p, q, t: Single;
begin
  if H < 0 then
  begin
    R:= V;
    G:= R;
    B:= R;
  end
  else
  begin
    Section:= H/SectionSize;
    SectionIndex:= Floor(Section);
    f:= Section - SectionIndex;
    p:= V * ( 1 - S );
    q:= V * ( 1 - S * f );
    t:= V * ( 1 - S * ( 1 - f ) );
    case SectionIndex of
      0:
        begin
          R:= V;
          G:= t;
          B:= p;
        end;
      1:
        begin
          R:= q;
          G:= V;
          B:= p;
        end;
      2:
        begin
          R:= p;
          G:= V;
          B:= t;
        end;
      3:
        begin
          R:= p;
          G:= q;
          B:= V;
        end;
      4:
        begin
          R:= t;
          G:= p;
          B:= V;
        end;
    else
      R:= V;
      G:= p;
      B:= q;
    end;
  end;
end;

procedure RGBToHSV(const R, G, B: Single; out H, S, V: Single);
var
  RGB: array[0..2] of Single;
  MinIndex, MaxIndex: Integer;
  Range: Single;
begin
  RGB[0]:= R;
  RGB[1]:= G;
  RGB[2]:= B;

  MinIndex:= 0;
  if G < R then
    MinIndex:= 1;

  if B < RGB[MinIndex] then
    MinIndex:= 2;

  MaxIndex:= 0;
  if G > R then
    MaxIndex:= 1;

  if B > RGB[MaxIndex] then
    MaxIndex:= 2;

  Range:= RGB[MaxIndex] - RGB[MinIndex];

  // Check for a gray level
  if Range = 0 then
  begin
    H:= -1; // Can't determine on greys, so set to -1
    S:= 0; // Gray is at the center;
    V:= R; // could choose R, G, or B because they are all the same value.
  end
  else
  begin
    case MaxIndex of
      0: H:=     (G-B)/Range;
      1: H:= 2 + (B-R)/Range;
      2: H:= 4 + (R-G)/Range;
    end;
    S:= Range/RGB[MaxIndex];
    V:= RGB[MaxIndex];
    H:= H * (1/6);
    if H < 0 then
      H:= 1 + H;
  end;
end;

const
  WavelengthMinimum = 380;  // Nanometers
  WavelengthMaximum = 780;
// Adapted http://www.physics.sfasu.edu/astro/color.html
procedure WavelengthToRGB(const Wavelength: Double; out R,G,B: Byte);

    CONST
      Gamma        =   0.80;
      IntensityMax = 255;

    VAR
      Blue  :  DOUBLE;
      factor:  DOUBLE;
      Green :  DOUBLE;
      Red   :  DOUBLE;

    FUNCTION Adjust(CONST Color, Factor:  DOUBLE):  INTEGER;
    BEGIN
      IF   Color = 0.0
      THEN RESULT := 0     // Don't want 0^x = 1 for x <> 0
      ELSE RESULT := ROUND(IntensityMax * Power(Color * Factor, Gamma))
    END {Adjust};

  BEGIN

    CASE TRUNC(Wavelength) OF
      380..439:
        BEGIN
          Red   := -(Wavelength - 440) / (440 - 380);
          Green := 0.0;
          Blue  := 1.0
        END;

      440..489:
        BEGIN
          Red   := 0.0;
          Green := (Wavelength - 440) / (490 - 440);
          Blue  := 1.0
        END;

      490..509:
        BEGIN
          Red   := 0.0;
          Green := 1.0;
          Blue  := -(Wavelength - 510) / (510 - 490)
        END;

      510..579:
        BEGIN
          Red   := (Wavelength - 510) / (580 - 510);
          Green := 1.0;
          Blue  := 0.0
        END;

      580..644:
        BEGIN
          Red   := 1.0;
          Green := -(Wavelength - 645) / (645 - 580);
          Blue  := 0.0
        END;

      645..780:
        BEGIN
          Red   := 1.0;
          Green := 0.0;
          Blue  := 0.0
        END;

      ELSE
        Red   := 0.0;
        Green := 0.0;
        Blue  := 0.0
    END;

    // Let the intensity fall off near the vision limits
    CASE TRUNC(Wavelength) OF
      380..419:  factor := 0.3 + 0.7*(Wavelength - 380) / (420 - 380);
      420..700:  factor := 1.0;
      701..780:  factor := 0.3 + 0.7*(780 - Wavelength) / (780 - 700)
      ELSE       factor := 0.0
    END;

    R := Adjust(Red,   Factor);
    G := Adjust(Green, Factor);
    B := Adjust(Blue,  Factor)
  END {WavelengthToRGB};


function GetRandomMixedColor(AColor: TColor): TColor;
var
  r, g, b: Byte;
begin
  r:= Trunc((Random(256) + Red(AColor))/2);
  g:= Trunc((Random(256) + Green(AColor))/2);
  b:= Trunc((Random(256) + Blue(AColor))/2);
  Result:= RGBToColor(r, g, b);
end;

function GetRandomGoldenRatioColor: TColor;
const
  golden_ratio_conjugate = 0.618033988749895;
var
  h,s,v: Single;
  r, g, b: Single;
begin
  s:= 0.5;
  v:= 0.95;
  h:= Frac(random + golden_ratio_conjugate);
  HSVToRGB(h, s, 255*v, r,g,b);
  Result:= RGBToColor(Trunc(r), Trunc(g), Trunc(b));
end;

function GetRandomGaminePathColor: TColor;
var
  r, g, b: Byte;
begin
  r:= Random(128) + 128;
  g:= Random(128) + 128;
  b:= Random(128) + 128;
  Result:= RGBToColor(r, g, b);
end;

function GetRandomGamineStarColor: TColor;
var
  r, g, b: Byte;
begin
  r:= 10*(Random(255) div 10);
  g:= 10*(Random(255) div 10);
  b:= 10*(Random(255) div 10);
  Result:= RGBToColor(r, g, b);
end;

function GetRandomStandardColor: TColor;
const
  Colors: array[0..(StandardColorsCount-1)] of TColor = (clBlack,clMaroon,clGreen,
                                             clOlive,clNavy,clPurple,clTeal,clGray,
                                             clSilver,clRed,clLime,clYellow,clBlue,
                                             clFuchsia,clAqua,clWhite);
begin
  Result:= Colors[Random(StandardColorsCount)];
end;

{ TRandomWebSafeColorPallete }

function TRandomWebSafeColorPallete.DoGetColor: TColor;
begin
  Result:= GetAWebSafeColor(Random(6),Random(6),Random(6));
end;



{ TCustomWebSafeColorPallete }

function TCustomWebSafeColorPallete.GetAWebSafeColor(r_i, g_i, b_i: Integer
  ): TColor;
var
  r, g, b: Byte;
begin
  r:= r_i * $33;
  g:= g_i * $33;
  b:= b_i * $33;
  Result:= RGBToColor(r, g, b);
end;

{ TRainbowColorPallete }

constructor TRainbowColorPallete.Create;
begin
  inherited Create;
  fWavelength:= WavelengthMinimum;
  Step:= 5;
end;

function TRainbowColorPallete.DoGetColor: TColor;
begin
  Result:= GetARainbowColor(fWavelength);
  fWavelength:= fWavelength + Step;
  if fWavelength > WavelengthMaximum then
    fWavelength:= WavelengthMinimum;
end;

procedure TRainbowColorPallete.SetStep(const AValue: Double);
begin
  if fStep=AValue then exit;
  fStep:=AValue;
end;

{ TRandomRainbowColorPallete }

function TRandomRainbowColorPallete.DoGetColor: TColor;
begin
  Result:= GetARainbowColor(WavelengthMinimum + Random*(WavelengthMaximum - WavelengthMinimum));
end;

{ TCustomRainbowColorPallete }

function TCustomRainbowColorPallete.GetARainbowColor(AWavelength: Double): TColor;
var
  r, g, b: Byte;
begin
  WavelengthToRGB(AWavelength,r,g,b);
  Result:= RGBToColor(r, g, b);
end;

{ TColorPallete }

function TColorPallete.GetColor: TColor;
begin
  Result:= DoGetColor;
end;


end.

