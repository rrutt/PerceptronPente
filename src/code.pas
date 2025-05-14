// Copyright 2025 Rick Rutt

unit code;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Menus, Grids, Types,
  fpjson,
  constants, gameboard, jsonfilemanager, playerperceptrons, perceptron;

//TODO: https://www.tpointtech.com/single-layer-perceptron-in-tensorflow
//TODO: Increase # perceptrons.
//TODO: Expand grids to 19x19.
//TODO: ??? Serialize game play moves to file using JSON.
//TODO: ??? Support "instant replay" from current or past serialized play move file.
//TODO: Setup dual AI auto-play learning environment.
//TODO: Add option to "partially mutate" Perceprtrons set.
//TODO: Manually create Perceptrons set file(s).

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonRandomizePerceptrons: TButton;
    ButtonWritePerceptronsToFile: TButton;
    ButtonPlayWhite: TButton;
    ButtonPlayBlack: TButton;
    ButtonNewGame: TButton;
    ButtonReadPerceptronsFromFile: TButton;
    GameBoardDrawGrid: TDrawGrid;
    HeadLabel1: TLabel;
    HeadLabel3: TLabel;
    GameBoardStringGrid: TStringGrid;
    LabelGameWinnerMessage: TLabel;
    LabelFileMessage: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;

    procedure ButtonNewGameClick(Sender: TObject);
    procedure ButtonPlayBlackClick(Sender: TObject);
    procedure ButtonPlayWhiteClick(Sender: TObject);
    procedure ButtonRandomizePerceptronsClick(Sender: TObject);
    procedure ButtonReadPerceptronsFromFileClick(Sender: TObject);
    procedure ButtonWritePerceptronsToFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GameBoardDrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure GameBoardDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; {%H-}aState: TGridDrawState);
    procedure GameBoardStringGridPrepareCanvas(Sender: TObject; {%H-}aCol,
      {%H-}aRow: Integer; {%H-}aState: TGridDrawState);

  private
    TheBoard: TGameBoard;

    CurrentPlayerIsHuman: boolean;

    CurrentPlayer: CellContent;
    OpponentPlayer: CellContent;
    WinningPlayer: CellContent;

    GameOver: boolean;

    PlayerName: array[WhitePiece..BlackPiece] of string;
    PlayerCaptureCount: array[WhitePiece..BlackPiece] of integer;
    PlayerPenteCount: array[WhitePiece..BlackPiece] of integer;
    PlayerPerceptrons: array[WhitePiece..BlackPiece] of TPlayerPerceptrons;

    JsonManager: TJsonFileManager;

    procedure ClearStringGrid;
    procedure MoveForPlayer;
    function ComputeMatchScore(const ThePerceptron: TPerceptron; const BoardCol: integer; const BoardRow: integer): single;
    procedure AnalyzeMove(const MoveCol: integer; const MoveRow: integer);
    procedure AdjustPerceptronsAfterWin;
    procedure AdjustPerceptronsAfterLoss;
    function FindLeastUsedPerceptron: TPerceptron;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  p: TPerceptron;
  i: integer;
  player: CellContent;
  perceptrons: TPerceptronArray;
begin
  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  OpenDialog1.Filter := 'JSON files (*.json)|*.json|All Files (*.*)|*.*';

  SaveDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  SaveDialog1.Filter := 'JSON files (*.json)|*.json|All Files (*.*)|*.*';

  LabelGameWinnerMessage.Caption := '';
  LabelFileMessage.Caption := '';;

  TheBoard := TGameBoard.Create;

  WinningPlayer := EmptyCell;
  GameOver := false;
  CurrentPlayerIsHuman := false;

  PlayerName[WhitePiece] := 'White';
  PlayerName[BlackPiece] := 'Black';

  PlayerCaptureCount[WhitePiece] := 0;
  PlayerCaptureCount[BlackPiece] := 0;

  PlayerPenteCount[WhitePiece] := 0;
  PlayerPenteCount[BlackPiece] := 0;

  for player := WhitePiece to BlackPiece do begin
    PlayerPerceptrons[player] := TPlayerPerceptrons.Create;
    SetLength(PlayerPerceptrons[player].Perceptrons, PERCEPTRON_COUNT);
    perceptrons := PlayerPerceptrons[player].Perceptrons;
    for i := Low(perceptrons) to High(perceptrons) do begin
      p := TPerceptron.Create;
      p.RandomizePatterns;
      p.RandomizeWeights;
      perceptrons[i] := p;
    end;
  end;

  JsonManager := TJsonFileManager.Create;
end;

procedure TForm1.ButtonNewGameClick(Sender: TObject);
var
  i: integer;
  p: TPerceptron;
  player: CellContent;
  perceptrons: TPerceptronArray;
begin
  TheBoard.ClearBoard;
  GameBoardDrawGrid.Invalidate;
  ClearStringGrid;
  LabelGameWinnerMessage.Caption := '';

  PlayerCaptureCount[WhitePiece] := 0;
  PlayerCaptureCount[BlackPiece] := 0;

  PlayerPenteCount[WhitePiece] := 0;
  PlayerPenteCount[BlackPiece] := 0;

  WinningPlayer := EmptyCell;
  GameOver := false;
  CurrentPlayerIsHuman := false;

  for player := WhitePiece to BlackPiece do begin
    perceptrons := PlayerPerceptrons[player].Perceptrons;
    for i := Low(perceptrons) to High(perceptrons) do begin
      p := perceptrons[i];
      p.UsageCount := 0;;
    end;
 end;
end;

procedure TForm1.ClearStringGrid;
var
  col: integer;
  row: integer;
begin
  for col := MIN_COL to MAX_COL do begin
    for row := MIN_ROW to MAX_COL do begin
      GameBoardStringGrid.Cells[col, row] := '';
    end;
  end;
end;

procedure TForm1.GameBoardDrawGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  col: integer;
  row: integer;
begin
  CurrentPlayerIsHuman := true;

  if (Button = mbLeft) then begin
    CurrentPlayer := WhitePiece;
    OpponentPlayer := BlackPiece;
  end else begin
    CurrentPlayer := BlackPiece;
    OpponentPlayer := WhitePiece;
  end;

  GameBoardDrawGrid.MouseToCell(X, Y, col, row);
  TheBoard.Cells[col, row] := CurrentPlayer;

  AnalyzeMove(col, row);
end;

procedure TForm1.GameBoardDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  theCanvas: TCanvas;
  cell: CellContent;
begin
  theCanvas := TDrawGrid(Sender).Canvas;

  cell := TheBoard.Cells[aCol, aRow];

  if (cell = WhitePiece) then begin
    theCanvas.Brush.Color := clWhite;
    theCanvas.Ellipse(aRect);
  end else if (cell = BlackPiece) then begin
    theCanvas.Brush.Color := clBlack;
    theCanvas.Ellipse(aRect);
  end else if (cell = CapturedCell) then begin
    theCanvas.Brush.Color := clGray;
    theCanvas.FillRect(aRect);
  end;
end;

procedure TForm1.GameBoardStringGridPrepareCanvas(Sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  ts := TStringGrid(Sender).Canvas.TextStyle;
  ts.Alignment := taCenter;
  TStringGrid(Sender).Canvas.TextStyle := ts;
end;

procedure TForm1.ButtonPlayWhiteClick(Sender: TObject);
begin
  CurrentPlayerIsHuman := false;
  CurrentPlayer := WhitePiece;
  OpponentPlayer := BlackPiece;
  MoveForPlayer;
end;

procedure TForm1.ButtonRandomizePerceptronsClick(Sender: TObject);
var
  i: integer;
  p: TPerceptron;
  player: CellContent;
  perceptrons: TPerceptronArray;
begin
  for player := WhitePiece to BlackPiece do begin
    perceptrons := PlayerPerceptrons[player].Perceptrons;
    for i := Low(perceptrons) to High(perceptrons) do begin
      p := perceptrons[i];
      p.RandomizePatterns;
      p.RandomizeWeights;
    end;
  end;

  LabelFileMessage.Caption := 'Radomized Perceptrons';
end;

procedure TForm1.ButtonReadPerceptronsFromFileClick(Sender: TObject);
var
  filename: string;
  jsonObj: TJSONObject;
  jsonPlayer: TJSONObject;
  player: CellContent;
  perceptrons: TPerceptronArray;
begin
  if (OpenDialog1.Execute) then begin
    filename := OpenDialog1.Filename;
    if (not fileExists(filename)) then begin
      LabelFileMessage.Caption := 'File not found: ' + filename;
    end else begin
      jsonObj := JsonManager.ReadJsonFromFile(filename);

      for player := WhitePiece to BlackPiece do begin
        jsonPlayer := jsonManager.ParseJsonPlayer(jsonObj, player);

        perceptrons := PlayerPerceptrons[player].Perceptrons;
        JsonManager.ParseJsonPerceptrons(jsonPlayer, perceptrons);
      end;

      LabelFileMessage.Caption := 'Perceptrons read from file ' + filename;
    end;
  end else begin
    LabelFileMessage.Caption := '(File read operation cancelled.)';
  end;
end;

procedure TForm1.ButtonWritePerceptronsToFileClick(Sender: TObject);
var
  filename: string;
  jsonText: string;
begin
  if (SaveDialog1.Execute) then begin
    filename := SaveDialog1.Filename;
    jsonText := JsonManager.GenerateJsonString(PlayerPerceptrons);
    JsonManager.WriteJsonToFile(filename, jsonText);
    LabelFileMessage.Caption := 'Perceptrons written to file ' + PERCEPTRONS_FILE_NAME;
  end else begin
    LabelFileMessage.Caption := '(File write operation cancelled.)';
  end;
end;

procedure TForm1.ButtonPlayBlackClick(Sender: TObject);
begin
  CurrentPlayerIsHuman := false;
  CurrentPlayer := BlackPiece;
  OpponentPlayer := WhitePiece;
  MoveForPlayer;
end;

procedure TForm1.MoveForPlayer;
var
  boardCol: integer;
  boardRow: integer;
  bestCol: integer;
  bestRow: integer;
  bestCellScore: single;
  bestMatchScore: single;
  matchScore: single;
  p: TPerceptron;
  perceptrons: TPerceptronArray;
  bestCellPerceptron: TPerceptron;
  bestMovePerceptron: TPerceptron;
  i: integer;
begin
  perceptrons := PlayerPerceptrons[CurrentPlayer].Perceptrons;

  bestCol := MIN_COL - 1;
  bestRow := MIN_ROW - 1;
  bestCellScore := NEGATIVE_INFINITY;
  bestMovePerceptron := nil;

  for boardCol := MIN_COL to MAX_COL do begin
    for boardRow := MIN_ROW to MAX_COL do begin
      bestCellPerceptron := nil;

      GameBoardStringGrid.Cells[boardCol, boardRow] := '.';
      if ((TheBoard.Cells[boardCol, boardRow] = EmptyCell) or (TheBoard.Cells[boardCol, boardRow] = CapturedCell)) then begin
        // Guarantee a move will be made if no Perceptron finds a positive match score.
        if (bestCol < MIN_COL) then begin
          bestCol := boardCol;
        end;
        if (bestRow < MIN_ROW) then begin
          bestRow := boardRow;
        end;

        bestMatchScore := 0.0;
        for i := Low(perceptrons) to High(perceptrons) do begin
          p := perceptrons[i];

          if (bestCellPerceptron = nil) then begin
            bestCellPerceptron := p;
          end;

          matchScore := ComputeMatchScore(p, boardCol, boardRow);
          if (matchScore > bestMatchScore) then begin
            bestMatchScore := matchScore;
            bestCellPerceptron := p;
          end;
        end; // for i

        GameBoardStringGrid.Cells[boardCol, boardRow] := FloatToStrF(bestMatchScore, ffFixed, 10, 4);

        if (bestMatchScore > bestCellScore) then begin
          bestCellScore := bestMatchScore;
          bestMovePerceptron := bestCellPerceptron;
          bestCol := boardCol;
          bestRow := boardRow;
        end; // if bestMatchScore
      end; // if EmptyCell or CapturedCell
    end; // for boardRow
  end; // for boareCol

  if ((bestCol >= MIN_COL) and (bestRow >= MIN_ROW)) then begin
    inc(bestMovePerceptron.UsageCount);
    TheBoard.Cells[bestCol, bestRow] := CurrentPlayer;
    GameBoardStringGrid.Cells[bestCol, bestRow] := '<<' + GameBoardStringGrid.Cells[bestCol, bestRow] + '>>';
    AnalyzeMove(bestCol, bestRow);
    GameBoardDrawGrid.Invalidate;
  end;
end;

function TForm1.ComputeMatchScore(const ThePerceptron: TPerceptron; const BoardCol: integer; const BoardRow: integer): single;
var
  reflectionCount: integer;

  matchScore: single;

  colReflection: integer;
  rowReflection: integer;

  patternCol: integer;
  patternRow: integer;

  colOffset: integer;
  rowOffset: integer;

  col: integer;
  row: integer;

  pattern: PatternMatchCell;
  boardCell: CellContent;
begin
  matchScore := 0.0;

  for reflectionCount := 1 to 4 do begin
    case reflectionCount of
      1: begin
        colReflection := +1;
        rowReflection := +1;
      end;
      2: begin
        colReflection := +1;
        rowReflection := -1;
      end;
      3: begin
        colReflection := -1;
        rowReflection := -1;
      end;
      else begin
        colReflection := -1;
        rowReflection := +1;
      end;
    end; // case

    for patternCol := MIN_PATTERN_INDEX to MAX_PATTERN_INDEX do begin;
      for patternRow := MIN_PATTERN_INDEX to MAX_PATTERN_INDEX do begin
        pattern := ThePerceptron.MatchCells[patternCol, patternRow];

        colOffset := patternCol - MIDDLE_PATTERN_INDEX;
        rowOffset := patternRow - MIDDLE_PATTERN_INDEX;

        col := BoardCol + (colOffset * colReflection);
        row := BoardRow + (rowOffset * rowReflection);

        if ((col >= MIN_COL) and (col <= MAX_COL) and (row >= MIN_ROW) and (row <= MAX_ROW)) then begin
          boardCell := TheBoard.Cells[col, row];
          if ((pattern = MatchEmpty) and
              ((boardCell = EmptyCell) or (boardCell = CapturedCell))) then begin
            matchScore := matchScore + ThePerceptron.MatchWeights[patternCol, patternRow];
          end else if ((pattern = MatchSelf) and (boardCell = CurrentPlayer)) then begin
            matchScore := matchScore + ThePerceptron.MatchWeights[patternCol, patternRow];
          end else if ((pattern = MatchOpponent) and (boardCell = OpponentPlayer)) then begin
            matchScore := matchScore + ThePerceptron.MatchWeights[patternCol, patternRow];
          end else if (pattern <> DoNotCare) then begin
            matchScore := matchScore - ThePerceptron.MatchWeights[patternCol, patternRow];
          end;
        end; // if within board
      end; // for patternRow
    end; // for patternCol
  end; // for reflectionCount

  //matchScore := Random;  //TODO: Remove Random match score.

  result := matchScore;
end;

//TODO: Pass best move Perceptron to AnalyzeMove procedure. (Pass nil for human player move.)
procedure TForm1.AnalyzeMove(const MoveCol: integer; const MoveRow: integer);
var
  selfPlayer: CellContent;
  otherPlayer: CellContent;
  cell: CellContent;
  direction: integer;
  oppositeDirection: integer;
  offsetCol: integer;
  offsetRow: integer;
  offsetIndex: integer;
  otherNeighborCount: integer;
  selfNeighborCount: array[MIN_DIRECTION..MAX_DIRECTION] of integer;
  whileLooping: boolean;
  i: integer;
begin
  selfPlayer := TheBoard.Cells[MoveCol, MoveRow];
  if (selfPlayer = WhitePiece) then begin
    otherPlayer := BlackPiece;
  end else if (selfPlayer = BlackPiece) then begin
    otherPlayer := WhitePiece;
  end else begin
    exit;
  end;

  for direction := MIN_DIRECTION to MAX_DIRECTION do begin
    TheBoard.GetDirectionOffsets(direction, offsetCol, offsetRow);
    offsetIndex := 1;
    otherNeighborCount := 0;
    selfNeighborCount[direction] := 0;

    whileLooping := true;
    while (whileLooping and
           ((MoveCol + (offsetCol * offsetIndex)) >= MIN_COL) and
           ((MoveCol + (offsetCol * offsetIndex)) <= MAX_COL) and
           ((MoveRow + (offsetRow * offsetIndex)) >= MIN_ROW) and
           ((MoveRow + (offsetRow * offsetIndex)) <= MAX_ROW)) do begin

      cell := TheBoard.Cells[MoveCol + (offsetCol * offsetIndex), MoveRow + (offsetRow * offsetIndex)];

      if (cell = selfPlayer) then begin
        if (otherNeighborCount = 0) then begin
          inc(selfNeighborCount[direction]);
          if ((selfNeighborCount[direction] + 1) >= PENTE_PIECE_COUNT) then begin
            //TODO: Increase weight of Perceptron that triggered Pente.
            inc(PlayerPenteCount[CurrentPlayer]);
            GameBoardStringGrid.Cells[MoveCol, MoveRow] := 'Pente ' + IntToStr(PlayerPenteCount[CurrentPlayer]);
            whileLooping := false;
          end else if (direction > HALF_DIRECTION) then begin
            oppositeDirection := direction - HALF_DIRECTION;
            if ((selfNeighborCount[direction] + selfNeighborCount[oppositeDirection] + 1) >= PENTE_PIECE_COUNT) then begin
              inc(PlayerPenteCount[CurrentPlayer]);
              GameBoardStringGrid.Cells[MoveCol, MoveRow] := 'Mid-Pente ' + IntToStr(PlayerPenteCount[CurrentPlayer]);
              whileLooping := false;
            end;
          end;
        end else if (otherNeighborCount = 1) then begin
          whileLooping := false;
        end else if (otherNeighborCount = CAPTURE_PIECE_COUNT) then begin
          //TODO: Increase weight of Perceptron that triggered Capture.
          inc(PlayerCaptureCount[CurrentPlayer]);
          GameBoardStringGrid.Cells[MoveCol, MoveRow] := 'Capture ' + IntToStr(PlayerCaptureCount[CurrentPlayer]);;
          for i := 1 to CAPTURE_PIECE_COUNT do begin;
            TheBoard.Cells[MoveCol + (offsetCol * (offsetIndex - i)), MoveRow + (offsetRow * (offsetIndex - i))] := CapturedCell;
          end;
          whileLooping := false;
        end;
      end else if (cell = otherPlayer) then begin
        if (selfNeighborCount[direction] > 0) then begin
          whileLooping := false;
        end else begin
          inc(otherNeighborCount);
          if (otherNeighborCount > CAPTURE_PIECE_COUNT) then begin
            whileLooping := false;
          end;
        end;
      end else begin // EmptyCell or CapturedCell
        whileLooping := false;
      end;

      inc(offsetIndex);
    end;
  end;

  //TODO: Show player capture counts in form labels.

  if (PlayerPenteCount[CurrentPlayer] > 0) then begin
    GameOver := true;
    WinningPlayer := CurrentPlayer;
    if (CurrentPlayerIsHuman) then begin
      LabelGameWinnerMessage.Caption := PlayerName[CurrentPlayer] + ' Human player wins with a Pente.';
      AdjustPerceptronsAfterLoss;
    end else begin
      LabelGameWinnerMessage.Caption := PlayerName[CurrentPlayer] + ' Perceptron player wins with a Pente.';
      AdjustPerceptronsAfterWin;
    end;
  end else if (PlayerCaptureCount[CurrentPlayer] >= CAPTURE_WIN_COUNT) then begin
    GameOver := true;
    WinningPlayer := CurrentPlayer;
    if (CurrentPlayerIsHuman) then begin
      LabelGameWinnerMessage.Caption := PlayerName[CurrentPlayer] + ' Human player wins by Captures.';
      AdjustPerceptronsAfterLoss;
    end else begin
      LabelGameWinnerMessage.Caption := PlayerName[CurrentPlayer] + ' Perceptron player wins by Captures.';
      AdjustPerceptronsAfterWin;
    end;
  end;
end;

procedure TForm1.AdjustPerceptronsAfterWin;
var
  i: integer;
  p: TPerceptron;
  perceptrons: TPerceptronArray;
begin
  perceptrons := PlayerPerceptrons[CurrentPlayer].Perceptrons;

  for i := Low(perceptrons) to High(perceptrons) do begin
    p := perceptrons[i];
    if (p.UsageCount > 0) then begin
      p.Weight := p.Weight * p.UsageCount;
    end;

    if (Random < PERCEPTRON_MUTATION_RATE) then begin
      p.Mutate;
    end;
  end;

  p := FindLeastUsedPerceptron;
  p.RandomizePatterns;
  p.RandomizeWeights;
end;

procedure TForm1.AdjustPerceptronsAfterLoss;
var
  i: integer;
  p: TPerceptron;
  perceptrons: TPerceptronArray;
begin
  perceptrons := PlayerPerceptrons[CurrentPlayer].Perceptrons;

  for i := Low(perceptrons) to High(perceptrons) do begin
    p := perceptrons[i];
    if (p.UsageCount > 0) then begin
      p.Weight := p.Weight / p.UsageCount;
    end;

    if (Random < PERCEPTRON_MUTATION_RATE) then begin
      p.Mutate;
    end;
  end;

  p := FindLeastUsedPerceptron;
  p.RandomizePatterns;
  p.RandomizeWeights;
end;

function TForm1.FindLeastUsedPerceptron: TPerceptron;
var
  i: integer;
  p: TPerceptron;
  leastUsedPerceptron: TPerceptron;
  perceptrons: TPerceptronArray;
begin
  perceptrons := PlayerPerceptrons[CurrentPlayer].Perceptrons;

  leastUsedPerceptron := perceptrons[Low(perceptrons)];
  for i := Succ(Low(perceptrons)) to High(perceptrons) do begin
    p := perceptrons[i];
    if (p.UsageCount < leastUsedPerceptron.UsageCount) then begin
      leastUsedPerceptron := p;
    end;
  end;

  result := leastUsedPerceptron;
end;

end.
