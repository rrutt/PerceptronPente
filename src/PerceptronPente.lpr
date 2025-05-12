// Copyright 2025 Rick Rutt

program PerceptronPente;

{$mode objfpc}{$H+}

uses
  Forms, Interfaces, SysUtils,
  code, jsonfilemanager;

{$R *.res}

begin
  Randomize;

  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
