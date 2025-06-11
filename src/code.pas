// Copyright 2025 Rick Rutt

unit code;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Menus, Grids, Spin, Types,
  fpjson,
  constants, gameboard, jsonfilemanager, playerperceptrons, perceptron;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonAutoPlay: TButton;
    ButtonPause: TButton;
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
    LabelWhitePlayerStatistics: TLabel;
    LabelBlackPlayerStatistics: TLabel;
    LabelGameWinnerMessage: TLabel;
    LabelFileMessage: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpinEditAutoPlayCount: TSpinEdit;

    //procedure AutoPlayThreadProc;
    procedure ButtonAutoPlayClick(Sender: TObject);
    procedure ButtonPauseClick(Sender: TObject);
    procedure SetupNewGame;
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
    ContinueAutoPlay: boolean;

    PlayerName: array[WhitePiece..BlackPiece] of string;
    PlayerCaptureCount: array[WhitePiece..BlackPiece] of integer;
    PlayerPenteCount: array[WhitePiece..BlackPiece] of integer;
    PlayerPerceptrons: array[WhitePiece..BlackPiece] of TPlayerPerceptrons;

    JsonManager: TJsonFileManager;

    procedure ClearStringGrid;
    procedure MoveForPlayer;
    function ComputeMatchScore(const ThePerceptron: TPerceptron; const BoardCol: integer; const BoardRow: integer): double;
    procedure AnalyzeMove(const MoveCol: integer; const MoveRow: integer; BestMovePerceptron: TPerceptron);
    procedure AdjustPerceptronsAfterWin(Player: TPlayerPerceptrons);
    procedure AdjustPerceptronsAfterLoss(Player: TPlayerPerceptrons);
    function FindLeastUsedPerceptron: TPerceptron;
    procedure UpdatePlayerStatisticsLabels;

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
  pp: TPlayerPerceptrons;
begin
  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  OpenDialog1.Filter := 'JSON files (*.json)|*.json|All Files (*.*)|*.*';

  SaveDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  SaveDialog1.Filter := 'JSON files (*.json)|*.json|All Files (*.*)|*.*';

  LabelGameWinnerMessage.Caption := '';
  LabelWhitePlayerStatistics.Caption := '';
  LabelBlackPlayerStatistics.Caption := '';
  LabelFileMessage.Caption := '';

  ButtonPause.Left := ButtonAutoPlay.Left;
  ButtonPause.Enabled := false;
  ButtonPause.Visible := false;

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
    pp := TPlayerPerceptrons.Create;
    pp.PenteWins := 0;
    pp.CaptureWins := 0;
    pp.PenteLosses := 0;
    pp.CaptureLosses := 0;
    SetLength(pp.Perceptrons, PERCEPTRON_COUNT);
    perceptrons := pp.Perceptrons;
    for i := Low(perceptrons) to High(perceptrons) do begin
      p := TPerceptron.Create;
      p.RandomizePatterns;
      p.RandomizeWeights;
      perceptrons[i] := p;
    end;
    PlayerPerceptrons[player] := pp;
  end;

  UpdatePlayerStatisticsLabels;

  JsonManager := TJsonFileManager.Create;
end;

procedure TForm1.UpdatePlayerStatisticsLabels;
begin
  LabelWhitePlayerStatistics.Caption :=
    Format('%d Wins by Pente, %d Wins by Capture, %d Losses by Pente, %d Losses by Capture',
    [PlayerPerceptrons[WhitePiece].PenteWins, PlayerPerceptrons[WhitePiece].CaptureWins,
     PlayerPerceptrons[WhitePiece].PenteLosses, PlayerPerceptrons[WhitePiece].CaptureLosses]);
  LabelWhitePlayerStatistics.Repaint;

  LabelBlackPlayerStatistics.Caption :=
    Format('%d Wins by Pente, %d Wins by Capture, %d Losses by Pente, %d Losses by Capture',
    [PlayerPerceptrons[BlackPiece].PenteWins, PlayerPerceptrons[BlackPiece].CaptureWins,
     PlayerPerceptrons[BlackPiece].PenteLosses, PlayerPerceptrons[BlackPiece].CaptureLosses]);
  LabelBlackPlayerStatistics.Repaint;

  TThread.Yield;
end;

procedure Tform1.SetupNewGame;
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

procedure TForm1.ButtonNewGameClick(Sender: TObject);
begin
  SetupNewGame;
end;

procedure AutoPlayThreadProc;
var
  gameCount: integer;
  gameNumber: integer;
begin
  gameCount := Form1.SpinEditAutoPlayCount.Value;
  for gameNumber := gameCount downto 1 do begin
    Form1.SpinEditAutoPlayCount.Value := gameNumber;
    Form1.SpinEditAutoPlayCount.Repaint;

    Form1.SetupNewGame;
    TThread.Yield;

    if (Random < 0.5) then begin
      Form1.CurrentPlayer := WhitePiece;
      Form1.OpponentPlayer := BlackPiece;
    end else begin
      Form1.CurrentPlayer := BlackPiece;
      Form1.OpponentPlayer := WhitePiece;
    end;

    repeat
      Form1.MoveForPlayer;
      Form1.GameBoardDrawGrid.Repaint;
      Form1.GameBoardStringGrid.Repaint;

      Sleep(AUTO_PLAY_MOVE_SLEEP_MILLISECONDS);
      TThread.Yield;

      if (Form1.CurrentPlayer = BlackPiece) then begin
        Form1.CurrentPlayer := WhitePiece;
        Form1.OpponentPlayer := BlackPiece;
      end else begin
        Form1.CurrentPlayer := BlackPiece;
        Form1.OpponentPlayer := WhitePiece;
      end;
    until (Form1.GameOver or (not Form1.ContinueAutoPlay));

    Form1.LabelGameWinnerMessage.Repaint;
    Sleep(AUTO_PLAY_GAME_SLEEP_MILLISECONDS);
    TThread.Yield;

    if (not Form1.ContinueAutoPlay) then begin
      break; // Out of for loop
    end;
  end;

  Form1.ButtonPause.Enabled  := false;
  Form1.ButtonPause.Visible := false;
  Form1.ButtonAutoPlay.Enabled := True;
end;

procedure TForm1.ButtonAutoPlayClick(Sender: TObject);
var
  aProc: TProcedure;
begin
  ButtonAutoPlay.Enabled := False;
  ButtonPause.Enabled := true;
  ButtonPause.Visible := true;
  ContinueAutoPlay := true;

  aProc := @AutoPlayThreadProc;
  TThread.CreateAnonymousThread(aProc).Start;
end;

procedure TForm1.ButtonPauseClick(Sender: TObject);
begin
  ContinueAutoPlay := false;
  ButtonPause.Enabled := false;
  ButtonPause.Visible := false;
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

  AnalyzeMove(col, row, nil);
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
  pp: TPlayerPerceptrons;
  player: CellContent;
  perceptrons: TPerceptronArray;
begin
  for player := WhitePiece to BlackPiece do begin
    pp := PlayerPerceptrons[player];
    pp.PenteWins := 0;
    pp.CaptureWins := 0;
    pp.PenteLosses := 0;
    pp.CaptureLosses := 0;
    perceptrons := pp.Perceptrons;
    for i := Low(perceptrons) to High(perceptrons) do begin
      p := perceptrons[i];
      p.RandomizePatterns;
      p.RandomizeWeights;
    end;
  end;

  UpdatePlayerStatisticsLabels;
  LabelFileMessage.Caption := 'Randomized Perceptrons';
end;

procedure TForm1.ButtonReadPerceptronsFromFileClick(Sender: TObject);
var
  filename: string;
  jsonObj: TJSONObject;
  jsonPlayer: TJSONObject;
  player: CellContent;
  perceptrons: TPerceptronArray;
  pp: TPlayerPerceptrons;
begin
  if (OpenDialog1.Execute) then begin
    filename := OpenDialog1.Filename;
    if (not fileExists(filename)) then begin
      LabelFileMessage.Caption := 'File not found: ' + filename;
    end else begin
      jsonObj := JsonManager.ReadJsonFromFile(filename);

      for player := WhitePiece to BlackPiece do begin
        pp := PlayerPerceptrons[player];

        jsonPlayer := jsonManager.ParseJsonPlayer(jsonObj, player);
        JsonManager.ParsePlayerWinsAndLosses(jsonPlayer, pp);

        perceptrons := pp.Perceptrons;
        JsonManager.ParseJsonPerceptrons(jsonPlayer, perceptrons);
      end;

      UpdatePlayerStatisticsLabels;
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
  bestCellScore: double;
  bestMatchScore: double;
  matchScore: double;
  p: TPerceptron;
  perceptrons: TPerceptronArray;
  bestCellPerceptron: TPerceptron;
  bestMovePerceptron: TPerceptron;
  i: integer;
  emptyCellCount: integer;
begin
  perceptrons := PlayerPerceptrons[CurrentPlayer].Perceptrons;

  bestCol := MIN_COL - 1;
  bestRow := MIN_ROW - 1;
  bestCellScore := NEGATIVE_INFINITY;
  bestMovePerceptron := nil;
  emptyCellCount := 0;

  for boardCol := MIN_COL to MAX_COL do begin
    for boardRow := MIN_ROW to MAX_COL do begin
      bestCellPerceptron := nil;

      GameBoardStringGrid.Cells[boardCol, boardRow] := '.';
      if ((TheBoard.Cells[boardCol, boardRow] = EmptyCell) or (TheBoard.Cells[boardCol, boardRow] = CapturedCell)) then begin
        Inc(emptyCellCount);

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

  if (emptyCellCount = 0) then begin
    GameOver := true;
    LabelGameWinnerMessage.Caption := 'This game is a draw.';
  end else if ((bestCol >= MIN_COL) and (bestRow >= MIN_ROW)) then begin
    inc(bestMovePerceptron.UsageCount);
    TheBoard.Cells[bestCol, bestRow] := CurrentPlayer;
    GameBoardStringGrid.Cells[bestCol, bestRow] := '<<' + GameBoardStringGrid.Cells[bestCol, bestRow] + '>>';
    AnalyzeMove(bestCol, bestRow, bestMovePerceptron);
    GameBoardDrawGrid.Invalidate;
  end;
end;

function TForm1.ComputeMatchScore(const ThePerceptron: TPerceptron; const BoardCol: integer; const BoardRow: integer): double;
var
  reflectionCount: integer;

  matchScore: double;

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

  result := matchScore;
end;

procedure TForm1.AnalyzeMove(const MoveCol: integer; const MoveRow: integer; BestMovePerceptron: TPerceptron);
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
  pp: TPlayerPerceptrons;
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
            if (BestMovePerceptron <> nil) then begin
              BestMovePerceptron.Weight := BestMovePerceptron.Weight * PENTE_WEIGHT_FACTOR;
            end;

            Inc(PlayerPenteCount[CurrentPlayer]);

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
          if (BestMovePerceptron <> nil) then begin
            BestMovePerceptron.Weight := BestMovePerceptron.Weight * CAPTURE_WEIGHT_FACTOR;
          end;

          Inc(PlayerCaptureCount[CurrentPlayer]);

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

  if (PlayerPenteCount[CurrentPlayer] > 0) then begin
    GameOver := true;
    WinningPlayer := CurrentPlayer;
    if (CurrentPlayerIsHuman) then begin
      LabelGameWinnerMessage.Caption := PlayerName[CurrentPlayer] + ' Human player wins with a Pente.';
      pp := PlayerPerceptrons[otherPlayer];
      Inc(pp.PenteLosses);
      AdjustPerceptronsAfterLoss(pp);
    end else begin
      LabelGameWinnerMessage.Caption := PlayerName[CurrentPlayer] + ' Perceptron player wins with a Pente.';
      pp := PlayerPerceptrons[CurrentPlayer];
      Inc(pp.PenteWins);
      AdjustPerceptronsAfterWin(pp);
      pp := PlayerPerceptrons[otherPlayer];
      Inc(pp.PenteLosses);
      AdjustPerceptronsAfterLoss(pp);
    end;
  end else if (PlayerCaptureCount[CurrentPlayer] >= CAPTURE_WIN_COUNT) then begin
    GameOver := true;
    WinningPlayer := CurrentPlayer;
    if (CurrentPlayerIsHuman) then begin
      LabelGameWinnerMessage.Caption := PlayerName[CurrentPlayer] + ' Human player wins by Captures.';
      pp := PlayerPerceptrons[otherPlayer];
      Inc(pp.CaptureLosses);
      AdjustPerceptronsAfterLoss(pp);
    end else begin
      LabelGameWinnerMessage.Caption := PlayerName[CurrentPlayer] + ' Perceptron player wins by Captures.';
      pp := PlayerPerceptrons[CurrentPlayer];
      Inc(pp.CaptureWins);
      AdjustPerceptronsAfterWin(pp);
      pp := PlayerPerceptrons[otherPlayer];
      Inc(pp.CaptureLosses);
      AdjustPerceptronsAfterLoss(pp);
    end;
  end;

  if (GameOver) then begin
    UpdatePlayerStatisticsLabels;
  end;
end;

procedure TForm1.AdjustPerceptronsAfterWin(Player: TPlayerPerceptrons);
var
  i: integer;
  p: TPerceptron;
  perceptrons: TPerceptronArray;
begin
  perceptrons := Player.Perceptrons;

  for i := Low(perceptrons) to High(perceptrons) do begin
    p := perceptrons[i];
    if (p.UsageCount > 0) then begin
      p.Weight := p.Weight * WINNING_PERCEPTRON_WEIGHT_FACTOR;
    end;

    if (Random < PERCEPTRON_MUTATION_RATE) then begin
      p.Mutate;
    end;
  end;

  p := FindLeastUsedPerceptron;
  p.RandomizePatterns;
  p.RandomizeWeights;
end;

procedure TForm1.AdjustPerceptronsAfterLoss(Player: TPlayerPerceptrons);
var
  i: integer;
  p: TPerceptron;
  perceptrons: TPerceptronArray;
begin
  perceptrons := Player.Perceptrons;

  for i := Low(perceptrons) to High(perceptrons) do begin
    p := perceptrons[i];
    if (p.UsageCount > 0) then begin
      p.Weight := p.Weight * LOSING_PERCEPTRON_WEIGHT_FACTOR;
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
