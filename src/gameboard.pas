// Copyright 2025 Rick Rutt

unit gameboard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  constants;

type
  TGameBoard = class
  private

  public
    Cells: array[MIN_COL..MAX_COL, MIN_ROW..MAX_ROW] of CellContent;

    Constructor Create;
    procedure ClearBoard;
    procedure GetDirectionOffsets(const Direction: integer; out ColOffset: integer; out RowOffset: integer);
  end;

var
  TheGameBoard: TGameBoard;

implementation
  Constructor TGameBoard.Create;
  begin
    ClearBoard;
  end;

  procedure TGameBoard.ClearBoard;
  var
    col: integer;
    row: integer;
  begin
    for col := MIN_COL to MAX_COL do begin
      for row := MIN_ROW to MAX_ROW do begin
        Cells[col, row] := EmptyCell;
      end;
    end;
  end;

  procedure TGameBoard.GetDirectionOffsets(const Direction: integer; out ColOffset: integer; out RowOffset: integer);
  begin
    case Direction of
      1: begin
        RowOffset := 0;
        ColOffset := 1;
      end;
      2: begin
        RowOffset := 1;
        ColOffset := 1;
      end;
      3: begin
        RowOffset := 1;
        ColOffset := 0;
      end;
      4: begin
        RowOffset := 1;
        ColOffset := -1;
      end;
      5: begin
        RowOffset := 0;
        ColOffset := -1;
      end;
      6: begin
        RowOffset := -1;
        ColOffset := -1;
      end;
      7: begin
        RowOffset := -1;
        ColOffset := 0;
      end;
      8: begin
        RowOffset := -1;
        ColOffset := 1;
      end;
      else begin
        RowOffset := 0;
        ColOffset := 0;
    end;
  end;
end;

end.

