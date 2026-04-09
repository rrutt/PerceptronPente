// Copyright 2025-2026 Rick Rutt

unit playerperceptrons;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  perceptron;

type
  TPlayerPerceptrons = class
  private
  public
    PlayerName: string;
    PenteWins: integer;
    CaptureWins: integer;
    PenteLosses: integer;
    CaptureLosses: integer;
    Perceptrons: TPerceptronArray;
  end;

implementation

end.

